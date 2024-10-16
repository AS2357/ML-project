# ---
# title: "Example of extracting data from RBA and ABS"
# author: "AS2357 Github"
# date: "2024-08-13"
# output: html_document
# ---

# Click the variable 'all_tables', it shows the whole data set, use the series number you need to download the corresponding data.

# Load necessary libraries, download if you don't have them
# install.packages("readrba")
# install.packages("readabs")

library(readrba)
library(readabs)

# Browse RBA series and get unique series numbers
all_tables <- browse_rba_series()
head(all_tables)
series_numbers <- unique(all_tables$no)

print(series_numbers)

# Employment trend
employment_trend <- read_rba(series_id = "GLFSURTR")
employment_trend

# Unemployment rate
unemp_rate <- read_rba(series_id = "GLFSURSA")
unemp_rate_df <- as.data.frame(unemp_rate)

unemp_rate_filtered <- unemp_rate_df[, c("date", "value")]
unemp_rate_filtered <- unemp_rate_filtered[unemp_rate_filtered$date >= "2019-08-01" & unemp_rate_filtered$date <= "2024-08-01",]
head(unemp_rate_filtered)

# New cash rate target
cash_rate_target <- read_rba(series_id = "ARBAMPCNCRT")

# Browse RBA tables for inflation
browse_rba_tables("inflation")
inflation_series_no <- "CPIAUCNS"

a <- browse_rba_series("currency")

# Consumer price index
cpi <- read_rba(series_id = "GCPIAG")
cpi_df <- as.data.frame(cpi)

cpi_filtered <- cpi_df[, c("date", "value")]
cpi_df_filtered <- cpi_filtered[unemp_rate_filtered$date >= "2019-08-01" & unemp_rate_filtered$date <= "2024-08-01",]

# Cash rate target
cash_rate_target <- read_rba(series_id = "FIRMMCRT")

# Other data series
CPI <- read_rba(series_id = "GCPIAG")
FX_market <- read_rba(series_id = "ARFXORFXM")
FX_liquidity <- read_rba(series_id = "ARFXORNRFX")
USD_index <- read_rba(series_id = "FUSXRTWI")
SWIFT_payments <- read_rba(series_id = "CRTGSNIS")
AUD_CNY <- read_rba(series_id = "FXRCR")
AUD_CNY <- aggregate(AUD_CNY$value, by = list(format(as.Date(AUD_CNY$date), "%Y-%m")), mean)
AUD_USD <- read_rba(series_id = "FXRUSD")
AUD_USD <- aggregate(AUD_USD$value, by = list(format(as.Date(AUD_USD$date), "%Y-%m")), mean)
Capital_Reserve <- read_rba(series_id = "ARBALCRF")
AUS_Gov_Deposits <- read_rba(series_id = "ARBALDOGIAG")
AUS_Gov_Securities <- read_rba(series_id = "ARBAAASAG")
Gold_FX <- read_rba(series_id = "ARBAAGFX")
total_asset <- read_rba(series_id = "ARBAATA")
total_liability <- read_rba(series_id = "ARBALTL")
export_index <- read_rba(series_id = "FREREWI")
real_gdp <- read_rba(series_id = "GGDPCVRGDI")

# Unemployment rate
unemp_rate <- read_rba(series_id = "GLFSURSA")

# Put all above data in this chunk into a list
rba_data <- list(
  CPI = CPI,
  FX_market = FX_market,
  FX_liquidity = FX_liquidity,
  USD_index = USD_index,
  SWIFT_payments = SWIFT_payments,
  AUD_CNY = AUD_CNY,
  AUD_USD = AUD_USD,
  Capital_Reserve = Capital_Reserve,
  AUS_Gov_Deposits = AUS_Gov_Deposits,
  AUS_Gov_Securities = AUS_Gov_Securities,
  Gold_FX = Gold_FX,
  unemp_rate = unemp_rate,
  total_asset = total_asset,
  total_liability = total_liability
)

rba_data_filtered <- lapply(rba_data, function(df) {
  if (all(c("date", "value") %in% colnames(df))) {
    return(df[, c("date", "value")])
  } else {
    return(NULL) 
  }
})

rba_data_filtered <- Filter(Negate(is.null), rba_data_filtered)

# # Save data to Excel (uncomment and specify the path if needed)
# library(writexl)
# write_xlsx(rba_data_filtered, "path you want to save the data")
# write_xlsx(AUD_CNY, "path you want to save the data")
# write_xlsx(AUD_USD, "path you want to save the data")

cash_rate_target_lag1 <- lag(cash_rate_target$value, 1)
cash_rate_target_lag2 <- lag(cash_rate_target$value, 2)
cash_rate_target <- cbind(cash_rate_target, cash_rate_target_lag1, cash_rate_target_lag2)

# # Save cash rate target to Excel (uncomment and specify the path if needed)
# write_xlsx(cash_rate_target, "path you want to save the data")

# Inflation
inflation <- read_rba(series_id = "GCPIAGQP")

monthly_data <- data.frame(date = as.Date(character()), value = numeric())

for (i in 1:(nrow(inflation) - 1)) {
  quarterly_value <- inflation$value[i]
  last_quarter_date <- inflation$date[i]
  next_months <- seq(from = last_quarter_date + 1, by = "month", length.out = 3)
  monthly_df <- data.frame(date = next_months, value = quarterly_value)
  monthly_data <- rbind(monthly_data, monthly_df)
}

last_value <- inflation$value[nrow(inflation)]
last_date <- inflation$date[nrow(inflation)]
last_months <- seq(from = last_date + 1, by = "month", length.out = 3)
last_df <- data.frame(date = last_months, value = last_value)
monthly_data <- rbind(monthly_data, last_df)

print(monthly_data)
# # Save monthly data to Excel (uncomment and specify the path if needed)
# write_xlsx(monthly_data, "path you want to save the data")

# Export index
export_index_monthly <- data.frame(date = as.Date(character()), value = numeric())

for (i in 1:(nrow(export_index) - 1)) {
  quarterly_value <- export_index$value[i]
  last_quarter_date <- export_index$date[i]
  next_months <- seq(from = last_quarter_date + 1, by = "month", length.out = 3)
  monthly_df <- data.frame(date = next_months, value = quarterly_value)
  export_index_monthly <- rbind(export_index_monthly, monthly_df)
}

last_value <- export_index$value[nrow(export_index)]
last_date <- export_index$date[nrow(export_index)]
last_months <- seq(from = last_date + 1, by = "month", length.out = 3)
last_df <- data.frame(date = last_months, value = last_value)
export_index_monthly <- rbind(export_index_monthly, last_df)

print(export_index_monthly)
# # Save export index monthly data to Excel (uncomment and specify the path if needed)
# write_xlsx(export_index_monthly, "path you want to save the data")

# Real GDP growth rate
real_gdp_growth <- diff(real_gdp$value) / lag(real_gdp$value, 1)
real_gdp <- cbind(real_gdp, real_gdp_growth)

# # Save real GDP data to Excel (uncomment and specify the path if needed)
# write_xlsx(real_gdp, "path you want to save the data")

# Convert real GDP to monthly data
real_gdp_monthly <- data.frame(date = as.Date(character()), real_gdp_growth = numeric())

for (i in 1:(nrow(real_gdp) - 1)) {
  quarterly_value <- real_gdp$real_gdp_growth[i]
  last_quarter_date <- real_gdp$date[i]
  next_months <- seq(from = last_quarter_date + 1, by = "month", length.out = 3)
  monthly_df <- data.frame(date = next_months, real_gdp_growth = quarterly_value)
  real_gdp_monthly <- rbind(real_gdp_monthly, monthly_df)
}

last_value <- real_gdp$real_gdp_growth[nrow(real_gdp)]
last_date <- real_gdp$date[nrow(real_gdp)]
last_months <- seq(from = last_date + 1, by = "month", length.out = 3)
last_df <- data.frame(date = last_months, real_gdp_growth = last_value)
real_gdp_monthly <- rbind(real_gdp_monthly, last_df)

print(real_gdp_monthly)
# # Save real GDP monthly data to Excel (uncomment and specify the path if needed)
# write_xlsx(real_gdp_monthly, "path you want to save the data")

# Gold to foreign exchange
gold_to_foreign_exchange <- read_rba(series_id = "ARBAAGFX")
# # Save gold to foreign exchange data to Excel (uncomment and specify the path if needed)
# write_xlsx(gold_to_foreign_exchange, "path you want to save the data")

# Net foreign asset
net_foreign_asset <- read_rba(series_id = "DODCNFA")

# Cash rate target
cash_rate <- read_rba(series_id = "ARBAMPCNCRT")
print(cash_rate)
# # Save cash rate data to Excel (uncomment and specify the path if needed)
# write_xlsx(cash_rate, "path you want to save the data")
