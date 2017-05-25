## Stationarity tests

# as.xts
tslist.nationwide = get_xts(correlate.nationwide)
tslist.local = get_xts(correlate.local)
tslist.blue = get_xts(correlate.blue)
tslist.red = get_xts(correlate.red)
tslist.purple = get_xts(correlate.purple)

# ts plots

plot_tslist(tslist.nationwide)
plot_tslist(tslist.local)
plot_tslist(tslist.blue)
plot_tslist(tslist.red)
plot_tslist(tslist.purple)

# Break data to segments (data driven)

tslist.nationwide.segments = breakData(tslist.nationwide)

for (k in length(tslist.nationwide.segments[[1]])){
        tslist.nationwide.segments[[1]][[k]] = as.xts(tslist.nationwide.segments[[1]][[k]])
}


# test each ts for stationarity (call adfU). 
# KPSS tests for trend stationarity, and included in auto.arima

adfU.nationwide = adfU(tslist.nationwide, maxlags = 7)
adfU.local = adfU(tslist.local, maxlags = 7)
adfU.blue = adfU(tslist.blue, maxlags = 7)
adfU.red = adfU(tslist.red, maxlags = 7)
adfU.purple = adfU(tslist.purple, maxlags = 7)

## Lag selection

# manual examination:
autocorr(tslist.nationwide)
autocorr(tslist.local)
autocorr(tslist.blue)
autocorr(tslist.red)
autocorr(tslist.purple)

# choose the model with least parameters min(p,q)
## estimate model
# plot original vs. fitted using auto.arima
arima.nationwide = model_plot(tslist.nationwide)

# goodness of fit (AIC, BIC may be better as it accounts for number of parameters to avoid overfitting)

## diagnostics: 

#residuals resemble white noise? (1) normaly distributed (2) variance unchanged over time (3) zero mean

# ARCH test (constant variance)
# lag = 7
arch_testU(arima.nationwide)

# Ljung-Box test: H0 ACF resembles white noise - p > 0.05
# normality of residuals - qq plot
serial_test(arima.nationwide)

# diagnostics plots are better with astsa::sarima():
diagnostics(arima.nationwide)

#mean = 0


## plot irf

# try with more lags+differencing??

# try arima with xreg... (for problem2???)

# try marima?

# transfer functions? (tsa::arimax)