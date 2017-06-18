data2corr = function(grp, diff = F) {
        
        correlate.any = data.table(publish_date = as.Date(DATES))
        
        if (grp == "nationwide") {
                score = "NationwideScore"
        } else if (grp == "local") {
                score = "AllStatesScore"
        }else if (grp == "blue") {
                score = "BlueScore"
        } else if (grp == "red") {
                score = "RedScore"
        } else if (grp == "purple") {
                score = "PurpleScore"
        }
        
        if (!diff){
                lapply(seq_along(CATEGORIES[interesting]), FUN = function (
                        i,
                        s = score,
                        data = long[[interesting[i]]]
                )
                {
                        setkey(data, "publish_date")        
                        
                        origin <- data[names==s, .(publish_date, V1)]
                        origin <- setDates(origin, DATES)
                        
                        correlate.any[, (as.character(NEW_NAMES[interesting[i]])) := origin$V1]
                })
        } else {
                lapply(seq_along(CATEGORIES[interesting]), FUN = function (
                        i,
                        s = score,
                        data = long[[interesting[i]]]
                )
                {
                        setkey(data, "publish_date")        
                        
                        origin <- data[names==s, .(publish_date, V1)]
                        origin <- setDates(origin, DATES)
                        
                        correlate.any[, (as.character(NEW_NAMES[interesting[i]])) := diff(origin$V1)]
                })
                
        }
        
        return (correlate.any)
}


summary_stats = function(correlate.any, grp){
        x = lapply(seq_along(CATEGORIES[interesting]), FUN = function (i) {
                na.fill(correlate.any[, NEW_NAMES[interesting[i]], with = F],0)
        })
        
        s = lapply(seq_along(CATEGORIES[interesting]), FUN = function (i) {
                summary(x[[i]])
        })
        
        if (grp == "nationwide") {
                score = "NationwideScore"
        } else if (grp == "local") {
                score = "LocalScore"
        }else if (grp == "blue") {
                score = "BlueScore"
        } else if (grp == "red") {
                score = "RedScore"
        } else if (grp == "purple") {
                score = "PurpleScore"
        }
        
        save(s, file = paste(grp, ".Rda", sep = ""))
        
}

histplot = function(correlate.any){
        par(mfrow = c(4,4))
        lapply(seq_along(CATEGORIES[interesting]), FUN = function (i) {
                hist(
                        x = na.fill(correlate.any[, NEW_NAMES[interesting[i]], with = F],0),
                        main = NEW_NAMES[interesting[i]], 
                        xlab = "", 
                        ylab = ""
                )       
        })
}

tsplot = function(correlate.any, topics){
        par(mfrow = c(5,2))
        lapply(seq_along(CATEGORIES[topics]), FUN = function (i) {
                plot(
                        x = DATES, 
                        y = na.fill(correlate.any[, NEW_NAMES[topics[i]], with = F],0),
                        type = "l", 
                        xlab = "",
                        ylab = "% Salience",
                        main = NEW_NAMES[topics[i]])        
        })
}
adf = function(correlate.any, topics) {
        # test for stationarity of time series vectors:

        adf.none = lapply(seq_along(CATEGORIES[topics]), FUN = function (i) {
                ur.df(
                        y = na.fill(correlate.any[, NEW_NAMES[topics[i]], with = F],0),
                        type = "none",
                        lags = 7, 
                        selectlags = "AIC")
        })                
        
        adf.drift = lapply(seq_along(CATEGORIES[topics]), FUN = function (i) {
                ur.df(
                        y = na.fill(correlate.any[, NEW_NAMES[topics[i]], with = F],0),
                        type = "drift",
                        lags = 7, 
                        selectlags = "AIC")
        })                
        
        adf.trend = lapply(seq_along(CATEGORIES[topics]), FUN = function (i) {
                ur.df(
                        y = na.fill(correlate.any[, NEW_NAMES[topics[i]], with = F],0),
                        type = "trend", 
                        lags = 7, 
                        selectlags = "AIC")
        })
        return(list(adf.none, adf.drift, adf.trend))
}

lags = function(correlate.any, topics, type = "const"){
# determine number of lags, using information criteria.
        
        if (type == "none"){
                result = VARselect(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        type = "none",
                        lag.max = 7)
        } else if (type == "const"){
                result = VARselect(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        type = "const",
                        lag.max = 7)        
        } else if (type == "trend") {
                result = VARselect(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        type = "trend",
                        lag.max = 7)        
        } else if (type == "both"){
                result = VARselect(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        type = "both",
                        lag.max = 7)        
        }
return(as.numeric(result$selection[1][[1]]))
}

estimate = function(correlate.any, topics, lags, type = "const"){
# estimate a VAR model.
        
        if (type == "none"){
                result = VAR(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        p = lags,
                        type = "none")        
        } else if (type == "const") {
                result = VAR(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        p = lags,
                        type = "const")        
        } else if (type == "trend") {
                result = VAR(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        p = lags,
                        type = "trend")        
        } else if (type == "both") {
                result = VAR(
                        y = na.fill(correlate.any[, NEW_NAMES[topics], with = F],0),
                        p = lags,
                        type = "both")        
        }
return(result)
}

err_test = function(varest, lags){
# tests for autocorrelation in a VAR model.       
        
        a = serial.test(
                x = varest, 
                lags.pt = lags,
                type = "PT.asymptotic")
        
        b = serial.test(
                x = varest, 
                lags.pt = lags,
                type = "PT.adjusted")
        
        c = serial.test(
                x = varest, 
                lags.bg = lags,
                type = "BG")
        
        d = serial.test(
                x = varest, 
                lags.bg = lags,
                type = "ES")
        
        return(list(a$serial, b$serial, c$serial, d$serial))
}

norm_test = function(varest){
# test for normal distribution of residuals
        norm = normality.test(varest)
return(norm$jb.mul)
}

arch_test = function(varest, lags) {
# test for Autoregressive Conditional Hetroscedasticity
        
        arch = arch.test(
                x = varest, 
                lags.multi = lags)
return(arch)
}

causality_test = function(varest, varlist, inv = F){
        # test for granger casuality (toda-tamamoto??)
        
        topics = gsub(" ", "\\.", NEW_NAMES[varlist])
        topics = gsub("-", "\\.", topics)
        topics = gsub("'", "\\.", topics)
        topics = gsub(",", "\\.", topics)
        topics = gsub("\\(", "\\.", topics)
        topics = gsub("\\)", "\\.", topics)
        
        g = list()
        if (inv == F){
                for (j in 1:length(varlist)) {
                        # x = causality(varest, cause = varlist[j])
                        x = causality(varest, cause = topics[j])
                        g[[j]] <- x$Granger
                }
        } else {
                for (j in 1:length(varlist)) {
                        mask = !(1:length(varlist)) == j
                        #x = causality(varest, cause = varlist[mask])
                        x = causality(varest, cause = topics[mask])
                        g[[j]] <- x$Granger
                }
        }
        return(g)
}


impulse_response = function(varest, type = "ortho"){
        # calculates the impulse responses for a given var model
        # ordering dependent...
        
        if (type == "ortho"){
                response = lapply(1:length(varest$varresult), function(i){
                        irf(x = varest, 
                            n.ahead = 7, 
                            impulse = colnames(var.nationwide$y)[i])
                })
                # to get the "response for each variable:"inverted" irf:
                # plot(irf(var.nationwide, n.ahead = 7, impulse = colnames(var.nationwide$y), response = colnames(var.nationwide$y)[i]))
                
        } else {
                response = lapply(1:length(varest$varresult), function(i){
                        irf(x = varest, 
                            n.ahead = 7, 
                            impulse = colnames(var.nationwide$y)[i],
                            cumulative = T)
                })
        }
return(response)
}


# if (grp == "nationwide") {
#         title = "Nationwide News Sources"
# } else if (grp == "local") {
#         title = "Local News Sources"
# }else if (grp == "blue") {
#         title = "Blue States"
# } else if (grp == "red") {
#         title = "Red Stats"
# } else if (grp == "purple") {
#         title = "Purple States"
# }

plot_irf = function(irf.any, topics){
         
        plotlist = list()
        
        plotlist = lapply(1:length(topics), FUN = function(k){
                
                j = c(1,3,5,7,9,11,13,14)[k]
                
                irf.any.plot = plot_ly(
                        x = c(0:7),
                        y = irf.any[[j]]$irf[[1]][(8*(j-1)+1):(8*j)],
                        type = "scatter",
                        mode = "lines",
                        showlegend = F,
                        legendgroup = as.character(paste("group",k,sep = "")),
                        name = NEW_NAMES[topics[k]]
                ) %>% layout(
                        hovermode = "closest",
                        title = NEW_NAMES[topics[k]],
                        xaxis = list( title = "days"),
                        yaxis = list(title = "shock"))
                
                for (i in 1:length(topics)){
                        if (i == k) next
                        
                        l = c(1,3,5,7,9,11,13,14)[i]
                        
                        irf.any.plot = add_trace(
                                p = irf.any.plot,
                                # y = irf.any$irf[[k]][,c(1,3,5,7,9,11,13,14)][,i],        
                                y = irf.any[[j]]$irf[[1]][(8*(l-1)+1):(8*l)],
                                type = "scatter",
                                mode = "lines",
                                visible = "legendonly",
                                showlegend = T,
                                name = NEW_NAMES[topics[i]],
                                legendgroup = as.character(paste("group",i,sep = "")))
                }
                plotlist[[k]] <- irf.any.plot
        })
        return(plotlist)
}

subplot_irf = function(topics){
        lapply(1:length(topics), function(i) {
                plotly:::subplot(
                        irf.nationwide.plot[[i]], 
                        irf.local.plot[[i]], 
                        irf.blue.plot[[i]], 
                        irf.red.plot[[i]], 
                        irf.purple.plot[[i]], 
                        shareX = T, 
                        nrows = 5)%>%layout(
                                legend = list(
                                        orientation = 'h'))
                
        })
}