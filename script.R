
library(rstan)
library(data.table)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

#################################
# Mobility data prep

mobility_data <- read.csv(url('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv'))

mobility_data <- data.table(mobility_data)
mobility_data <- mobility_data[country_region_code %in% c('GB', 'US') &
                               sub_region_1 == '']
mobility_data[, date := as.Date(date)]

mobility_data <- mobility_data[, .(date, country = country_region_code,
    m = (retail_and_recreation_percent_change_from_baseline +
         transit_stations_percent_change_from_baseline +
         workplaces_percent_change_from_baseline)/300)]

mobility_data <- dcast(mobility_data, date ~ country, value.var = 'm')

setnames(mobility_data, 'GB', 'm_uk')
setnames(mobility_data, 'US', 'm_us')
setorder(mobility_data, date)

mobility_data[, m_uk := as.numeric(filter(m_uk, rep(1/7, 7)))]
mobility_data[, m_us := as.numeric(filter(m_us, rep(1/7, 7)))]
mobility_data <- mobility_data[!is.na(m_uk + m_us)]

#################################
# Covid data prep

covid_data <- jsonlite::fromJSON(url('https://pomber.github.io/covid19/timeseries.json'))
covid_data <- covid_data[c('US', 'United Kingdom')]
covid_data <- rbindlist(covid_data, idcol = T)
covid_data <- covid_data[, .(date = as.Date(date), country = .id, d = deaths)]
covid_data <- dcast(covid_data, date ~ country, value.var = 'd')

setnames(covid_data, 'United Kingdom', 'd_uk')
setnames(covid_data, 'US', 'd_us')
setorder(covid_data, date)

covid_data[, d_uk := c(0.0, diff(d_uk))]
covid_data[, d_us := c(0.0, diff(d_us))]

covid_data[, d_uk := as.numeric(filter(d_uk, rep(1/7, 7)))]
covid_data[, d_us := as.numeric(filter(d_us, rep(1/7, 7)))]

covid_data <- covid_data[!is.na(d_uk + d_us)]

#################################
# Final model data prep

data <- merge(mobility_data, covid_data, by = 'date')
saveRDS(data, 'data.rds')
data <- readRDS('data.rds')

weighted_avg_mat <- function(col) {
    mat <- toeplitz(col)
    mat[upper.tri(mat)] <- 0
    mat <- mat/rowSums(mat)
    return(mat)
}

n_days <- nrow(data)
H <- weighted_avg_mat(dgamma(1:n_days, (18.2/8.46)^2, 18.2/(8.46^2))) #infec-to-dth
W <- weighted_avg_mat(dgamma(1:n_days, (6.48/3.83)^2, 6.48/(3.83^2))) #serial-intrv

int <- as.integer
stan_data <- list(n_days = n_days, n_coun = 2, H = H, W = W,
                  Di = as.matrix(data[, .(int(round(d_uk)), int(round(d_us)))]),
                  M = as.matrix(data[, .(m_uk, m_us)]),
                  Sigm = matrix(rep(plogis(1:n_days, 45, 3), 2), ncol = 2))
stan_data$M[stan_data$M > 0] = 0

#################################
# Models (unchecked work!)

model_string <- '
data {
    int n_days;
    int n_coun;
    int Di[n_days, n_coun];   //integer-deaths
    matrix[n_days, n_coun] M; //mobility
    matrix[n_days, n_days] H;
    matrix[n_days, n_days] W;
    matrix[n_days, n_coun] Sigm;
}
transformed data {
    matrix[n_days, n_coun] D;
    real prs = 1e-10;
    D = to_matrix(Di);
}
parameters {
    real<lower = 0> d;
    real<lower = 0, upper = 2> Rp;
    vector<lower = 0, upper = 100>[n_coun] b;
    vector<lower = 0, upper = 5>[n_coun] R0;
}
transformed parameters {
    matrix[n_days, n_coun] R_d; //observed-r
    matrix[n_days, n_coun] mu;
    R_d = H * exp(M * diag_matrix(b)) * diag_matrix(R0);
    R_d += Rp * Sigm; // addition not part of report 26
    mu = prs + R_d .* (W*D);
}
model {
    d ~ exponential(1);
    for (i in 1:n_coun)
        Di[, i] ~ neg_binomial_2(mu[, i], d);
}
'

model <- stan_model(model_code = model_string)
samples <- sampling(model, iter = 3000, chains = 4, data = stan_data)
traceplot(samples, pars = c('R0', 'Rp', 'b', 'd'))

#################################
# Plots

r <- summary(samples, pars = 'R_d')$summary[, c(1, 3)]
r_mu <- matrix(r[, 1], ncol = stan_data$n_coun, byrow = T)
r_sg <- matrix(r[, 2], ncol = stan_data$n_coun, byrow = T)
r_uk <- data.table(date = data$date, mu = r_mu[, 1], sg = r_sg[, 1])
r_us <- data.table(date = data$date, mu = r_mu[, 2], sg = r_sg[, 2])

plot_uk <- ggplot(r_uk, aes(x = date))
plot_uk <- plot_uk + geom_line(aes(y = mu))
plot_uk <- plot_uk + geom_ribbon(aes(ymin = mu - 2*sg, ymax = mu + 2*sg), alpha = 0.5)
plot_uk <- plot_uk + geom_hline(yintercept = 1, linetype = 2)
plot_uk <- plot_uk + labs(title = 'uk', y = 'r_experienced')

plot_us <- ggplot(r_us, aes(x = date))
plot_us <- plot_us + geom_line(aes(y = mu))
plot_us <- plot_us + geom_ribbon(aes(ymin = mu - 2*sg, ymax = mu + 2*sg), alpha = 0.5)
plot_us <- plot_us + geom_hline(yintercept = 1, linetype = 2)
plot_us <- plot_us + labs(title = 'us', y = 'r_experienced')

gridExtra::grid.arrange(plot_uk, plot_us)
