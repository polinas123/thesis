breakData = function(break.any){
        r = lapply(1:length(break.any), function(i){
                breaks = breakpoints(break.any[[i]]~1)
                breaks = breaks$breakpoints
                data = as.data.table(break.any[[i]])
                if (NA %in% breaks){
                        data[,segment:=NA]
                } else {
                        n = 1
                        for (j in 1:length(breaks)){
                                data[n:breaks[j],segment := breaks[j]]
                                n = breaks[j]+1
                        }
                }
                result = split(data, by = "segment")
                return(result)
        })
return(r)
}

get_xts = function(correlate.any){
# get each variable as an xts object
        ts = lapply(2:ncol(correlate.any), function(i){
                values = correlate.any[,colnames(correlate.any)[i], with = F]
        
                xts <- xts(
                        x = as.numeric(na.fill(values, 0)),
                        order.by = DATES)
        })
        return(ts)
}

plot_tslist = function(tslist.any){
# exploratory plots of ts to examine stationarity
        par(mfrow = c(4,2))
        lapply(seq_along(tslist.any), function(i){
                plot.xts(
                        tslist.any[[i]],
                        main = NEW_NAMES[interesting[i]])
                })
        
}

adfU = function(tslist.any, maxlags = 14, criteria = "AIC"){
# test for stationarity of time series
        adf.none = lapply(seq_along(tslist.any), FUN = function (i) {
                ur.df(
                        y = coredata(tslist.any[[i]]),
                        type = "none",
                        lags = maxlags, 
                        selectlags = criteria)
        })                
        
        adf.drift = lapply(seq_along(tslist.any), FUN = function (i) {
                ur.df(
                        y = coredata(tslist.any[[i]]),
                        type = "drift",
                        lags = maxlags, 
                        selectlags = criteria)
        })                
        
        adf.trend = lapply(seq_along(tslist.any), FUN = function (i) {
                ur.df(
                        y = coredata(tslist.any[[i]]),
                        type = "trend", 
                        lags = maxlags, 
                        selectlags = criteria)
        })
        return(list(adf.none, adf.drift, adf.trend))
}

autocorr = function(tslist.any){
# plots acf and pacf for each topic in a given source group
        par(mfrow = c(2,1))
        lapply(seq_along(tslist.any), function(i){
                acf(tslist.any[[i]])
                pacf(tslist.any[[i]])
        })
}

model_select = function(tslist.any, criteria = "aic"){
        lapply(seq_along(tslist.any), function(i){
                auto.arima(
                        y = tslist.any[[i]], 
                        stationary = T, 
                        #allowdrift = T,
                        allowmean = T,
                        seasonal = F,
                        ic = criteria)
                })
}

model_plot = function(tslist.any){
        model1= model_select(tslist.any)
        #model2 = model_select(tslist.any, criteria = "bic")
        
        par(mfrow = c(1,1))
        
        lapply(seq_along(tslist.any), function(i){
                plot(coredata(tslist.any[[i]]), type = "l", main = NEW_NAMES[interesting[i]])
                lines(model1[[i]]$fitted, type = "l", col = "red")
                #lines(model2[[i]]$fitted, type = "l", col = "green")
        })
        return(model1)
}

serial_test = function(arima.any){
        lapply(seq_along(arima.any), function(i){
                LB_test = Box.test(arima.any[[i]]$residuals, type="Ljung-Box", lag = 20)
                BG_test = bgtest(lm(arima.any[[i]]$residuals~1), order = 14)
                par(mfrow = c(1,1))
                tsdisplay(arima.any[[i]]$residuals)
                par(mfrow = c(2,1))
                qqnorm(arima.any[[i]]$residuals)
                qqline(arima.any[[i]]$residuals)
                hist(arima.any[[i]]$residuals)
                lines(density(arima.any[[i]]$residuals))
                print(paste("p values for topic", i, sep=" "))
                print("LB test: ")
                print(LB_test$p.value)
                print("BG test: ")
                print(BG_test$p.value)
                })
}

arch_testU = function(arima.any){
        lapply(seq_along(arima.any), function(i){
                ArchTest(x = arima.any[[i]]$residuals, lags = 7)
        })
}

diagnostics = function(arima.any){
        lapply(seq_along(arima.any), function(i){
                order = sub("\\).*", "", sub(".*\\(", "", as.character(arima.any[[i]])))
                sarima(
                        arima.any[[i]]$x, 
                        as.numeric(strsplit(order, ",")[[1]][1]),
                        as.numeric(strsplit(order, ",")[[1]][2]),
                        as.numeric(strsplit(order, ",")[[1]][3]))
                })
        
}