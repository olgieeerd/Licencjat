# pobranie danych z yahoofinance
library(quantmod)   
start_date <- as.Date("2024-01-01")
end_date   <- as.Date("2024-12-31")

getSymbols("BTC-USD", src = "yahoo", from = start_date, to = end_date)
getSymbols("LTC-USD", src = "yahoo", from = start_date, to = end_date)

# bazujemy na closing price
btc_prices <- Cl(`BTC-USD`)
ltc_prices <- Cl(`LTC-USD`)

crypto_data <- merge(btc_prices, ltc_prices)
colnames(crypto_data) <- c("BTC", "LTC")

crypto_data <- na.omit(crypto_data)

head(crypto_data)
tail(crypto_data)

# test ADF
library(tseries)

adf.test(crypto_data$BTC)
adf.test(crypto_data$LTC)

diff_BTC <- na.omit(diff(crypto_data$BTC))
diff_LTC <- na.omit(diff(crypto_data$LTC))

adf.test(diff_BTC)
adf.test(diff_LTC)

# test Engle'a - Grangera
model_eg <- lm(BTC ~ LTC, data = crypto_data)
summary(model_eg)

resid_eg <- model_eg$residuals
adf_resid <- adf.test(resid_eg)
adf_resid


# model ECM
resid_eg_xts <- xts(resid_eg, order.by = index(crypto_data))

ecm_lag <- lag(resid_eg_xts, k = 1)

dBTC <- diff(crypto_data$BTC)
dLTC <- diff(crypto_data$LTC)

ecm_merged <- merge(dBTC, dLTC, ecm_lag, join = "inner")
ecm_merged <- na.omit(ecm_merged)

colnames(ecm_merged) <- c("dBTC", "dLTC", "ecm_lag")

ecm_data <- data.frame(
  date = index(ecm_merged),
  coredata(ecm_merged)
)

ecm_model <- lm(dBTC ~ dLTC + ecm_lag, data = ecm_data)
summary(ecm_model)

# SIGNALING

# St Deviation - progi wejścia (1.5) i wyjścia (0.25)
resid_mean <- mean(resid_eg)
resid_sd   <- sd(resid_eg)

upper_entry  <- resid_mean + 1.5 * resid_sd
lower_entry  <- resid_mean - 1.5 * resid_sd

exit_band_hi <- resid_mean + 0.25 * resid_sd
exit_band_lo <- resid_mean - 0.25 * resid_sd

# konwersja reszt do xts
resid_eg_xts <- xts(resid_eg, order.by = index(crypto_data))

btc_ret <- ROC(crypto_data$BTC, type = "discrete")  

# loop generowanie sygnałów (-1 short; 0 flat; +1 long)
resid_vec <- as.numeric(resid_eg_xts)
dates_vec <- index(resid_eg_xts)

position_vec <- rep(0, length(resid_vec))  
pnl_vec      <- rep(0, length(resid_vec))  # dzienna stopa zwrotu strategii

position_state <- 0  

for (i in seq_along(resid_vec)) {
  r <- resid_vec[i]  # bieżąca wartość reszty
  
  if (position_state == +1 && r > exit_band_lo) {
    
    position_state <- 0
  }
  if (position_state == -1 && r < exit_band_hi) {
   
    position_state <- 0
  }
  

  if (position_state == 0) {
    if (r > upper_entry) {
      position_state <- -1  
    } else if (r < lower_entry) {
      position_state <- +1  
    }
  }
  

  position_vec[i] <- position_state
  
  # Dzienny PnL

  if (i > 1) {
    pnl_vec[i] <- position_vec[i - 1] * as.numeric(btc_ret[i])
  } else {
    pnl_vec[i] <- 0
  }
}

# ramka z datą, resztą, pozycją i dziennym p&l
trade_data <- data.frame(
  date      = dates_vec,
  residual  = resid_vec,
  position  = position_vec,
  pnl_day   = pnl_vec
)

# Prosta krzywa kapitału, przyjmujemy start 100%
trade_data$equity <- cumprod(1 + trade_data$pnl_day)

tail(trade_data, 20)




