####################################################################################
#    Time Series Analysis of Salience Scores (in % by groups of news sources)     #                            #
####################################################################################

interesting = c(11,25,13,23,16,22,3,19,7,14,1,21,12,17,26,28)

####################################################################################
#                       Analysis of sources by group                               #
####################################################################################

correlate.nationwide = data2corr("nationwide")
correlate.local = data2corr("local")

correlate.red = data2corr("red")
correlate.blue = data2corr("blue")
correlate.purple = data2corr("purple")

####################################################################################
#                         Perliminary Data Analysis                                #
####################################################################################

# summary statistics for each of 16 interesting categories from original data:
summary_stats(correlate.nationwide, strsplit("correlate.nationwide", "\\.")[[1]][2])
summary_stats(correlate.local, strsplit("correlate.local", "\\.")[[1]][2])
summary_stats(correlate.blue, strsplit("correlate.blue", "\\.")[[1]][2])
summary_stats(correlate.red, strsplit("correlate.red", "\\.")[[1]][2])
summary_stats(correlate.purple, strsplit("correlate.purple", "\\.")[[1]][2])

# plot histograms
histplot(correlate.nationwide)
histplot(correlate.local)
histplot(correlate.blue)
histplot(correlate.red)
histplot(correlate.purple)

# cut off topics with > 85% NA values (285/335)
# nationwide_topics = interesting[c(1:5,7:11,13:14)] # cut @ NA/Zero thresh.
# G_nationwide_topics = nationwide_topics[c(1,3,4,5,6,7,8,10,11,12)] # cut @ Granger.
# general_topicss = interesting[c(1:11,13:14)]
# local_topics = interesting[c(1:11,13:14)] # cut @ NA/Zero thresh.
# G_local_topics = local_topics[c(1,3:9,11:13)] # cut @ Granger.
# G_local_topics = local_topics[c(1,3:5,7:9,11:13)] # cut out health (for comparison with nationwide)
# red_topics = interesting[c(1,3:5,7:11,13:14)]
# blue_topics = interesting[c(1,3:5,7:11,13:14)]
# purple_topics = interesting[c(1,3,5,7:11,13:14)]

# plot all time series in group, to get the general shape:
tsplot(correlate.nationwide, G_nationwide_topics)
tsplot(correlate.local, G_local_topics)
tsplot(correlate.blue, blue_topics)
tsplot(correlate.red, red_topics)
tsplot(correlate.purple, purple_topics)

# # inspect univariate autocorrelation of each topic:
# par(mfcol = c(2,1))
# lapply(seq_along(CATEGORIES[red_topics]), FUN = function (i) {
#         acf(na.fill(correlate.nationwide[, NEW_NAMES[G_nationwide_topics[i]], with = F],0))
#         pacf(na.fill(correlate.nationwide[, NEW_NAMES[G_nationwide_topics[i]], with = F],0))
# })

# test for stationarity:
stationarity.nationwide = adf(correlate.nationwide, interesting)
stationarity.local = adf(correlate.local, interesting)
stationarity.blue = adf(correlate.blue, interesting)
stationarity.red = adf(correlate.red, interesting)
stationarity.purple = adf(correlate.purple, interesting)

# estimate number of lags for VAR:
lags.nationwide = lags(correlate.nationwide, interesting)
lags.local = lags(correlate.local, interesting)
lags.blue = lags(correlate.blue, interesting)
lags.red = lags(correlate.red, interesting)
lags.purple = lags(correlate.purple, interesting)

# estimate VAR:
# for some reason, later - irf does not work with varest that returns from function
# var.nationwide = estimate(as.data.table(correlate.nationwide), as.numeric(interesting), as.numeric(lags.nationwide))
# var.local = estimate(correlate.local, G_local_topics, lags.local)
# var.blue = estimate(correlate.blue, blue_topics, lags.blue)
# var.red = estimate(correlate.red, red_topics, lags.red)
# var.purple = estimate(correlate.purple, purple_topics, lags.purple)

var.nationwide = VAR(
        y = na.fill(correlate.nationwide[, NEW_NAMES[interesting], with = F],0),
        p = lags.nationwide,
        type = "const")

var.local = VAR(
        y = na.fill(correlate.local[, NEW_NAMES[interesting], with = F],0),
        p = lags.local,
        type = "const")

var.blue = VAR(
        y = na.fill(correlate.blue[, NEW_NAMES[interesting], with = F],0),
        p = lags.blue,
        type = "const")

var.red = VAR(
        y = na.fill(correlate.red[, NEW_NAMES[interesting], with = F],0),
        p = lags.red,
        type = "const")

var.purple = VAR(
        y = na.fill(correlate.purple[, NEW_NAMES[interesting], with = F],0),
        p = lags.purple,
        type = "const")

# regression summary and plots: 1.none 2.const 3.trend 4.both
summary(var.nationwide)
plot(var.nationwide)

summary(var.local)
plot(var.local)

summary(var.blue)
plot(var.blue)

summary(var.red)
plot(var.red)

summary(var.purple)
plot(var.purple)

# ???
plot(var.nationwide$varresult$US.muslims)

# # observe structiral breaks (continious/discrete):
# # test for stability of model over time:
# # Moving Sum is better for Cumulative sum (why?)
# msum = stability(
#         x = var.const,
#         type = "OLS-MOSUM")
# plot(msum)
# 
# csum = stability(
#         x = var.const,
#         type = "OLS-CUSUM")
# plot(csum)

#test for autocorrelation in the error term:
ser.nationwide = err_test(var.nationwide, lags.nationwide)
ser.local = err_test(var.local, lags.local)
ser.blue = err_test(var.blue, lags.blue)
ser.red = err_test(var.red, lags.red)
ser.purple = err_test(var.purple, lags.purple)


# test for normal dist. of residuals:
norm.nationwide = norm_test(var.nationwide)
norm.local = norm_test(var.local)
norm.blue = norm_test(var.blue)
norm.red = norm_test(var.red)
norm.purple = norm_test(var.purple)

# test for residual Autoregression Conditional HS:
# namely autocorr. of squares residuals
arch.nationwide = arch_test(var.nationwide, lags.nationwide)
plot(arch.nationwide)

arch.local = arch_test(var.local, lags.local)
plot(arch.local)

arch.blue = arch_test(var.blue, lags.blue)
plot(arch.blue)

arch.red = arch_test(var.red, lags.red)
plot(arch.red)

arch.purple = arch_test(var.purple, lags.purple)
plot(arch.purple)

# select comparable topics:

compareable_topics = c(11,13,16,3,19,7,1,12,17)

# Granger test for the model:

# for each variables against all the others:
causality.nationwide = causality_test(var.nationwide, interesting)
causality.local = causality_test(var.local, interesting)
causality.blue = causality_test(var.blue, interesting)
causality.red = causality_test(var.red, interesting)
causality.purple = causality_test(var.purple, interesting)
#inversed:
causality.nationwide.inv = causality_test(var.nationwide, interesting, inv = T)
causality.local.inv = causality_test(var.local, interesting, inv = T)
causality.blue.inv = causality_test(var.blue, interesting, inv = T)
causality.red.inv = causality_test(var.red, interesting, inv = T)
causality.purple.inv = causality_test(var.purple, interesting, inv = T)

# GC significant topics, for further analysis are:
GC = interesting[c(1,3,5,7,9,11,13,14)]

# calculate impulse response:
irf.nationwide = impulse_response(var.nationwide)
irf.local = impulse_response(var.local)
irf.blue = impulse_response(var.blue)
irf.red = impulse_response(var.red)
irf.purple = impulse_response(var.purple)

# plot impulse response for each topic:

par(ask=F)
lapply(1:length(irf.blue), function(i){plot(irf.blue[[i]])})

# plot self by self plot:
plot(irf(var.purple, n.ahead = 7, impulse = colnames(var.purple$y)[1], response = colnames(var.purple$y)[1]))


# plot comparative plots:
irf.nationwide.plot = plot_irf(irf.nationwide, GC)
irf.local.plot = plot_irf(irf.local, GC)
irf.blue.plot = plot_irf(irf.blue, GC)
irf.red.plot = plot_irf(irf.red, GC)
irf.purple.plot = plot_irf(irf.purple, GC)
 
# par(mfrow = c(1,1))
# 
# plot(irf.nationwide$irf[[1]][1:8], 
#      type = "l", 
#      lwd = 2,
#      xlab = "days", 
#      xlim = c(1,7), 
#      ylab = "", 
#      main = NEW_NAMES[G_nationwide_topics[1]], 
#      ylim = c(0,0.25))

subplot = subplot_irf(GC)

