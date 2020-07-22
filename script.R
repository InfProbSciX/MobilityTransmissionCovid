
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
