# in final code file: have a single function call without any parameters
# to reproduce the relevant data flow to the final paper

setwd("/Users/polinas/R/")

require(data.table)
require(readxl)
require(plotly)
require(xts)
require(SynchWave)
require(Hmisc)
require(corrplot)
require(stats)
require(vars)
require(urca)
require(DT)
require(tseries)
require(stargazer)

# source("multiplot.R")
source("prepare_data_wDT.R")
source("var_functions.R")

###############################################################################
#                               READ RAW DATA                                 #        
###############################################################################

GRAPHS_PATH = paste(getwd(), "/Graphs/", sep = "")
WEBSITES_PATH = "/Volumes/Untitled/R/Thesis/websites.csv"
STATES_PATH = "/Volumes/Untitled/R/Thesis/states.csv"
COLORS_PATH = "/Volumes/Untitled/R/Thesis/blue-red.csv"
DATA_PATH = "/Volumes/Untitled/R/Thesis/DATA"
NAMES_PATH = "/Volumes/Untitled/R/Thesis/new_names.csv"

# create directories for plots
# dir.create(path = paste(GRAPHS_PATH, "all sources", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "nationwide sources", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "local sources", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "blue states", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "red states", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "COMPARISONS", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "COMPARISONS", "/topic groups", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "COMPARISONS", "/nationwide vs local", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "COMPARISONS", "/red vs blue", sep = ""))
# dir.create(path = paste(GRAPHS_PATH, "CORRELATIONS", sep = ""))


# sources:
#         2016 election:
#         http://www.270towin.com/2016_Election/interactive_map
#         2004-2016
#         https://en.wikipedia.org/wiki/Red_states_and_blue_states#/media/File:Red_state,_blue_state.svg

WEBSITES = readWebsites()

STATES = readStates()
COLORS = readColors()
STATES = merge(STATES, COLORS, all = T, by = "state")

DT = readData()

###############################################################################
#                           MAKE TIDY DATA                                    #        
###############################################################################

# data 90 is raw data thresholded @0.9 cayegory_proba
CATEGORIES = unique(DT$category)

# if nessecary, change names of category labels:
NEW_NAMES <- as.character(read.csv(NAMES_PATH)$subject)

# split PUBLISH_DATE to publish_date and publish_time
DT[,publish_time := sapply(strsplit(publish_date,' '), "[", 2)][,publish_date := sapply(strsplit(publish_date,' '), "[", 1)]

DT[, publish_date := as.Date(publish_date)]
DT[, category := as.factor(category)]

# add states (of origin of sources):
setkey(DT,site)
setkey(WEBSITES, site)
DT = merge(DT,WEBSITES, all = F, all.x = T)

SITES = unique(DT$site)

# set the key to publish_date
setkey(DT, publish_date)
# get full date range (in days)
DATES = getDates(DT)
# add rows for days between observations, a Date filter might be agged.
# the DT is changed by reference

# if nessecary, filter out days before some date
# DATES = filterDates(DATES, filter = "2016-01-01")
# set the full range of desired dates to DT (NA is filled in rows w/o observations)
DT = setDates(DT, DATES) 

# add metrics for display:
getArticleCountPerCategory()
getNationwideCount()
getLocalCount()

getArticleCountPerCategoryPerSite()
getArticleCountPerSite()
getNormCountPerSite()

getCountPerColor()
# getRedCount()
# getBlueCount()

getScorePerGroup()
# getRedCountPerState()
# getBlueCountPerState()

getDiff()

getDiff_1()
getDiff_2()
getDiff_3()

###############################################################################
#                     GENERATE EXPLORATORY PLOTS                              #        
###############################################################################

# select relevant columns for plotting:
cols = c("ArticlesInCategoryPerDay",
         "NationwideArticlesInCategoryPerDay",
         "LocalArticlesInCategoryPerDay",
         "RedArticlesInCategoryPerDay",
         "BlueArticlesInCategoryPerDay",
         "RedScore",
         "BlueScore",
         "PurpleScore",
         "Delta",
         "Delta_1",
         "Delta_2")

cols = c("BlueScore","Delta","RedScore", "PurpleScore", "AllStatesScore", "NationwideScore", "GeneralScore", "Delta_1", "Delta_2", "Delta_3")

setkey(DT, category, publish_date)

long = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i,
        data = DT[CATEGORIES[i], 
                  as.double(lapply(.SD, max, na.rm = T)), 
                  .SDcols = cols, 
                  by="publish_date"]) {
        
        data[, names:=cols]
        setkey(data, V1)
        data$V1[is.infinite(data$V1)] = NA
        setkey(data, "names") 
        
        return(data)
})


movement_colors = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
        {
                setkey(data, "publish_date")
                
                origin <- data[names=="Delta", .(publish_date, V1)]
                origin <- setDates(origin, DATES)
                
                a <- origin[,V1]
                
                pos <- a>0
                a[pos] <- 1
                neg <- a<0
                a[neg] <- -1
                
                b <- a[2:(length(a))]
                a <- a[1:((length(a)-1))]
                
                compare <- a==b
                
                shifts <- which(!compare)
                phases <- a[shifts]
                
        return(list(shifts, phases, a))
})

par(mfrow = c(4,4))

lapply(seq_along(CATEGORIES[interesting]), FUN = function (i){
        plot(
                # x = seq(1, length(movement[[interesting[i]]][[3]])), 
                x = DATES[1:(length(DATES)-1)],
                y = na.fill(movement_colors[[interesting[i]]][[3]], 0),
                type = "l", 
                xlab = NEW_NAMES[interesting][i],
                ylab = "salience shift")
})

movement_circulation = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="Delta_3", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        pos <- a>0
        a[pos] <- 1
        neg <- a<0
        a[neg] <- -1
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- a==b
        
        shifts <- which(!compare)
        phases <- a[shifts]
        
        return(list(shifts, phases, a))
})

par(mfrow = c(4,4))

lapply(seq_along(CATEGORIES[interesting]), FUN = function (i){
        plot(
                # x = seq(1, length(movement[[interesting[i]]][[3]])), 
                x = DATES[1:(length(DATES)-1)],
                y = na.fill(movement_circulation[[interesting[i]]][[3]], 0),
                type = "l", 
                xlab = NEW_NAMES[interesting][i],
                ylab = "salience shift")
})

movementByGroup.red = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="RedScore", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- ifelse(
                test = a-b < 0,
                yes = 1,
                no = ifelse(
                        test = a-b > 0,
                        yes = -1,
                        no = 0
                ))
                
        
        firstShift <- which(!is.na(compare))[1]
        phases <- NULL
        
        return(list(firstShift, phases, compare))
})

movementByGroup.blue = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="BlueScore", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- ifelse(
                test = a-b < 0,
                yes = 1,
                no = ifelse(
                        test = a-b > 0,
                        yes = -1,
                        no = 0
                ))
        
        
        firstShift <- which(!is.na(compare))[1]
        phases <- NULL
        
        return(list(firstShift, phases, compare))
})

movementByGroup.Purple = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="PurpleScore", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- ifelse(
                test = a-b < 0,
                yes = 1,
                no = ifelse(
                        test = a-b > 0,
                        yes = -1,
                        no = 0
                ))
        
        
        firstShift <- which(!is.na(compare))[1]
        phases <- NULL
        
        return(list(firstShift, phases, compare))
})

movementByGroup.nationwide = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="NationwideScore", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- ifelse(
                test = a-b < 0,
                yes = 1,
                no = ifelse(
                        test = a-b > 0,
                        yes = -1,
                        no = 0
                ))
        
        
        firstShift <- which(!is.na(compare))[1]
        phases <- NULL
        
        return(list(firstShift, phases, compare))
})

movementByGroup.local = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="AllStatesScore", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- ifelse(
                test = a-b < 0,
                yes = 1,
                no = ifelse(
                        test = a-b > 0,
                        yes = -1,
                        no = 0
                ))
        
        
        firstShift <- which(!is.na(compare))[1]
        phases <- NULL
        
        return(list(firstShift, phases, compare))
})

movementByGroup.all = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]])
{
        setkey(data, "publish_date")
        
        origin <- data[names=="GeneralScore", .(publish_date, V1)]
        origin <- setDates(origin, DATES)
        
        a <- origin[,V1]
        
        b <- a[2:(length(a))]
        a <- a[1:((length(a)-1))]
        
        compare <- ifelse(
                test = a-b < 0,
                yes = 1,
                no = ifelse(
                        test = a-b > 0,
                        yes = -1,
                        no = 0
                ))
        
        
        firstShift <- which(!is.na(compare))[1]
        phases <- NULL
        
        return(list(firstShift, phases, compare))
})

par(mfrow = c(4,4))

lapply(seq_along(CATEGORIES[interesting]), FUN = function (i){
        plot(
                # x = seq(1, length(movement[[interesting[i]]][[3]])), 
                x = DATES[1:(length(DATES)-1)],
                y = na.fill(movementByGroup.all[[interesting[i]]][[3]], 0),
                type = "l", 
                xlab = NEW_NAMES[interesting][i],
                ylab = "salience shift")
        abline(
                v = DATES[movementByGroup.all[[interesting[i]]][[1]] -1],
                col = "red")
})

correlate = data.table(publish_date = as.Date(DATES[1:(length(DATES)-1)]))
lapply(seq_along(CATEGORIES[interesting]), FUN = function (
        i,
        data = movementByGroup.nationwide[[i]]){
        # correlate[, (NEW_NAMES[interesting[i]]) := data[[3]]]
        correlate[, (as.character(i)) := data[[3]]]
        correlate[, {avg = lapply(.SD, mean); na.fill(as.character(i), avg)}]
})







# correlate[, (colnames(correlate)[2:17]) :=lapply(.SD, as.double), .SDcols = colnames(correlate)[2:17]]
# correlate = na.aggregate(correlate)
# correlate[, (colnames(correlate)[2:17]) := 
#                   lapply( colnames(correlate)[2:17], function(x)
#         {
#                 x <- get(x);
#                 x[is.na(x)] <- mean(x, na.rm = TRUE);
#                 x
#         })]


# var_lag = lapply(seq_along(1:9), function(i){
#         var_est = VAR(y = na.fill(correlate[,2:17],0), p = i, type = "both")
# })
# 
# lapply(seq_along(var_lag), function(i) { AIC(object = var_lag[[i]]) })
# lapply(seq_along(var_lag), function(i) { BIC(object = var_lag[[i]]) })

p = VARselect(na.fill(correlate[,2:17],0))

var_est = VAR(
        y = na.fill(correlate[,2:17],0),
        p = p$selection[[1]], 
        type = "both", 
        ic = "AIC")

roots(var_est)
    
summary(var_est)
plot(var_est)

vat_est.ser = restrict(x = var_est, method = "ser", thresh = 2)

mat = rcorr(
        x = as.matrix(correlate[,2:17])
        # y = as.matrix(correlate[,2:16])
        #method = "pearson", 
        #use = "na.or.complete"
)

corrplot.mixed(
        corr = mat$r, 
        upper =  "circle",
        lower = "number",
        p.mat = mat$P,
        sig.level = 0.05,
        addCoefasPercent = T,
        order = "hclust",
        addrect = 2
        )

ccf_result = ccf(x = as.integer(as.matrix(correlate[,2])), 
    y = as.integer(as.matrix(correlate[,3])), 
    type = "correlation", 
    plot = T, 
    na.action = na.pass)

spec_result = spec.pgram(
        na.omit(correlate[,3]), 
        # kernel("daniell", 4), 
        spans=c(13,13),
        taper = 0, 
        log = "no")

PLOTS.diff = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ p = plot_ly(
        x = data["Delta"]$publish_date,
        y = data["Delta"]$V1*100,
        name = "% Difference",
        visible = T,
        color = I(ifelse(
                test = data["Delta"]$V1 > 0, 
                yes = "Red", 
                no = "Blue")),
        type = "bar") %>% 
        add_trace(
                y = data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100,
                color = I("Red"),
                type = "scatter",
                mode = "lines",
                hoverinfo="none"
        ) %>%
        add_trace(
                y = data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
                color = I("Blue"),
                type = "scatter",
                mode = "lines",
                hoverinfo="none"
        ) %>%
        layout( 
                # title = paste(
                #         "Attention to",
                #         NEW_NAMES[i], 
                #         sep = " "), 
                showlegend = F,
                yaxis = list(
                        title = "% Difference")
                        #zeroline = FALSE,
                        # showline = FALSE,
                        # showticklabels = FALSE,
                        # showgrid = FALSE)
                #rangemode = "tozero",
                #dtick = 250),
        )
return(p)
})

PLOTS.diff_1 = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ setkey(data, "names")
        p = plot_ly(
        x = data["Delta_1"]$publish_date,
        y = data["Delta_1"]$V1*100,
        name = "% Difference",
        visible = T,
        color = I(ifelse(
                test = data["Delta_1"]$V1 > 0, 
                yes = "Red", 
                no = "Blue")),
        type = "bar") %>% 
        # add_trace(
        #         y = data[V1 > 0]["Delta_1", mean(V1, na.rm = T)]*100,
        #         color = I("Red"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        # add_trace(
        #         y = data[V1 < 0]["Delta_1", mean(V1, na.rm = T)]*100,
        #         color = I("Blue"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        layout( 
                # title = paste(
                #         "Attention to",
                #         NEW_NAMES[i], 
                #         sep = " "), 
                showlegend = F,
                yaxis = list(
                        title = "% Difference")
                #zeroline = FALSE,
                # showline = FALSE,
                # showticklabels = FALSE,
                # showgrid = FALSE)
                #rangemode = "tozero",
                #dtick = 250),
        )
return(p)
})

PLOTS.diff_2 = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ setkey(data, "names")
        p = plot_ly(
        x = data["Delta_2"]$publish_date,
        y = data["Delta_2"]$V1*100,
        name = "% Difference",
        visible = T,
        color = I(ifelse(
                test = data["Delta_2"]$V1 > 0, 
                yes = "Red", 
                no = "Blue")),
        type = "bar") %>% 
        # add_trace(
        #         y = data[V1 > 0]["Delta_2", mean(V1, na.rm = T)]*100,
        #         color = I("Red"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        # add_trace(
        #         y = data[V1 < 0]["Delta_2", mean(V1, na.rm = T)]*100,
        #         color = I("Blue"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        layout( 
                # title = paste(
                #         "Attention to",
                #         NEW_NAMES[i], 
                #         sep = " "), 
                showlegend = F,
                yaxis = list(
                        title = "% Difference")
                #zeroline = FALSE,
                # showline = FALSE,
                # showticklabels = FALSE,
                # showgrid = FALSE)
                #rangemode = "tozero",
                #dtick = 250),
        )
return(p)
})

PLOTS.diff_3 = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ p = plot_ly(
        x = data["Delta_3"]$publish_date,
        y = data["Delta_3"]$V1*100,
        name = "% Difference",
        visible = T,
        color = I(ifelse(
                test = data["Delta_3"]$V1 > 0, 
                yes = "Orange", 
                no = "Green")),
        type = "bar") %>% 
        # add_trace(
        #         y = data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100,
        #         color = I("Red"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        # add_trace(
        #         y = data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
        #         color = I("Blue"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        layout( 
                # title = paste(
                #         "Attention to",
                #         NEW_NAMES[i], 
                #         sep = " "), 
                showlegend = F,
                yaxis = list(
                        title = "% Difference")
                #zeroline = FALSE,
                # showline = FALSE,
                # showticklabels = FALSE,
                # showgrid = FALSE)
                #rangemode = "tozero",
                #dtick = 250),
        )
return(p)
})

PLOTS.actual = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ p = plot_ly(
                x = data["RedScore"]$publish_date,
                y = data["RedScore",]$V1*100,
                name = "% Salience",
                color = I("Red"),
                #alpha = 0.3,
                #hoverinfo="none",
                visible = T,
                type = "bar") %>% 
        add_trace(
                x = data["BlueScore"]$publish_date,
                y = (data["BlueScore",]$V1)*(-1)*100,
                name = "% Salience",
                color = I("Blue"),
                #alpha = 0.3,
                #hoverinfo="none",
                visible = T,
                type = "bar") %>%
        layout( 
                # title = paste(
                #         "Attention to",
                #         NEW_NAMES[i], 
                #         sep = " "), 
                showlegend = F,
                yaxis = list(
                        title = "# Articles")
                        #zeroline = FALSE,
                        # showline = FALSE,
                        # showticklabels = FALSE,
                        # showgrid = FALSE)
                #rangemode = "tozero",
                #dtick = 250),
        )
return(p)
})

PLOTS.actual_purple = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ setkey(data, "names")
        p = plot_ly(
        x = data["PurpleScore"]$publish_date,
        y = data["PurpleScore",]$V1*100,
        name = "% Salience",
        color = I("Red"),
        #alpha = 0.3,
        #hoverinfo="none",
        visible = T,
        type = "bar") %>% 
        # add_trace(
        #         x = data["BlueScore"]$publish_date,
        #         y = (data["BlueScore",]$V1)*(-1)*100,
        #         name = "% Salience",
        #         color = I("Blue"),
        #         #alpha = 0.3,
        #         #hoverinfo="none",
        #         visible = T,
        #         type = "bar") %>%
        layout( 
                # title = paste(
                #         "Attention to",
                #         NEW_NAMES[i], 
                #         sep = " "), 
                showlegend = F,
                yaxis = list(
                        title = "# Articles")
                #zeroline = FALSE,
                # showline = FALSE,
                # showticklabels = FALSE,
                # showgrid = FALSE)
                #rangemode = "tozero",
                #dtick = 250),
        )
return(p)
})

PLOTS.actual_3 = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ setkey(data, "names")
        p = plot_ly(
                x = data["AllStatesScore"]$publish_date,
                y = data["AllStatesScore",]$V1*100,
                name = "% Salience - Local",
                color = I("Orange"),
                #alpha = 0.3,
                #hoverinfo="none",
                visible = T,
                type = "bar") %>% 
                add_trace(
                        x = data["NationwideScore"]$publish_date,
                        y = (data["NationwideScore",]$V1)*(-1)*100,
                        name = "% Salience - Nationwide",
                        color = I("Green"),
                        #alpha = 0.3,
                        #hoverinfo="none",
                        visible = T,
                        type = "bar") %>%
                layout( 
                        # title = paste(
                        #         "Attention to",
                        #         NEW_NAMES[i], 
                        #         sep = " "), 
                        showlegend = F,
                        yaxis = list(
                                title = "% Salience")
                        #zeroline = FALSE,
                        # showline = FALSE,
                        # showticklabels = FALSE,
                        # showgrid = FALSE)
                        #rangemode = "tozero",
                        #dtick = 250),
                )
        return(p)
})

PLOTS.comp = lapply(seq_along(CATEGORIES[1:29]), FUN = function(i){
        p = plotly::subplot(PLOTS.diff_3[[i]], PLOTS.actual_3[[i]], 
                            nrows = 2, shareX = T) %>%
                layout(
                        # yaxis = list(
                                # zeroline = FALSE,
                                # showline = FALSE,
                                # showticklabels = FALSE,
                                # showgrid = FALSE),
                       title = "% DIFFERENCE (top) vs. % SALIENCE (bottom)")
        return(p)
})

# save plots for knitr
save(PLOTS.comp, file = "display.Rda")

### .......... end display 06_03_17 .......... ###

### ......... start display 07_03_17 ......... ###

PLOTS.ontop = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]]
){ setkey(data, "names")
        p = plot_ly(
        x = data["Delta"]$publish_date,
        y = data["Delta"]$V1*100,
        # y = ifelse(
        #         test = data["Delta"]$V1*100 > data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100 | data["Delta"]$V1*100 < data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
        #         yes = ifelse(
        #                 test = data["Delta"]$V1*100 > 0,
        #                 yes = data["Delta"]$V1*100 - data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100,
        #                 no = ifelse(
        #                         test = data["Delta"]$V1*100 < 0,
        #                         yes = data["Delta"]$V1*100 - data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
        #                         no = 0)),
        #         no = 0),
        name = "% Difference",
        visible = T,
        color = I(ifelse(
                test = data["Delta"]$V1 > 0, 
                yes = "Dark Red", 
                no = "Dark Blue")),
        # alpha = ifelse(
        #         test = data["Delta"]$V1*100 > data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100 | data["Delta"]$V1*100 < data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
        #         yes = 1,
        #         no = 0),
        type = "scatter",
        mode = "markers",
        marker = list(
                size = abs(data["Delta"]$V1)*25)
        ) %>% #animation_opts(frame = 1000) %>%
        add_trace(
                x = data["BlueScore"]$publish_date,
                y = (data["BlueScore",]$V1)*(-1)*100,
                name = "% Salience",
                color = I("Blue"),
                alpha = 0.3,
                #hoverinfo="none",
                visible = T,
                type = "bar"
        ) %>%
        add_trace(
                x = data["RedScore"]$publish_date,
                y = (data["RedScore",]$V1)*100,
                name = "% Salience",
                color = I("Red"),
                alpha = 0.3,
                #hoverinfo="none",
                visible = T,
                type = "bar"
        ) %>%
        # add_trace(
        #         y = data["Delta", mean(V1, na.rm = T), by = .(V1 >0, V1<0)],
        #         color = I("Black"),
        #         type = "scatter",
        #         mode = "lines",
        #         hoverinfo="none"
        # ) %>%
        layout(
                showlegend = F,
                hovermode = "compare",
                yaxis = list(
                        dtick = 25),
                xaxis = list(
                        rangeselector = list(
                                buttons = list(
                                        # list(
                                        #         count = 1,
                                        #         label = "1 d",
                                        #         step = "day",
                                        #         stepmode = "backward"),
                                        list(
                                                count = 7,
                                                label = "7 d",
                                                step = "day",
                                                stepmode = "backward"),
                                        list(
                                                count = 1,
                                                label = "1 m",
                                                step = "mon",
                                                stepmode = "backward"),
                                        # list(
                                        #         count = 1,
                                        #         label = "MTD",
                                        #         step = "mon",
                                        #         stepmode = "todate"),
                                        list(step = "all"))),
                        rangeslider = list(type = "date")))
        

return(p)
})

#PLOTS.ontop = PLOTS.ontop[c(1:26,28,29)]

# save plots for knitr
save(PLOTS.ontop, file = "display_ontop.Rda")

### .......... end display 07_03_17 .......... ###

### ......... start display 15_03_17 ......... ###

PLOTS.penn = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]] ){ 
        setkey(data, "publish_date")
        p = plot_ly(
                #x = c(data["Delta"]$V1*100, data["Delta"]$V1*100),
                # y = data[!"Delta"]$V1*100,
                #y = c(data["RedScore"]$V1*100, data["BlueScore"]$V1*100*(-1)),
                #size = abs(c(data["Delta"]$V1, data["Delta"]$V1))*c(data["RedScore"]$V1, data["BlueScore"]$V1),
                # y = ifelse(
                #         test = data["Delta"]$V1*100 > data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100 | data["Delta"]$V1*100 < data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
                #         yes = ifelse(
                #                 test = data["Delta"]$V1*100 > 0,
                #                 yes = data["Delta"]$V1*100 - data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100,
                #                 no = ifelse(
                #                         test = data["Delta"]$V1*100 < 0,
                #                         yes = data["Delta"]$V1*100 - data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
                #                         no = 0)),
                #         no = 0),
                # name = "% Difference",
                visible = T
        ) %>% layout(
                showlegend = F,
                hovermode = "compare",
                yaxis = list(
                        dtick = 25,
                        title = "% Salience",
                        range = c(-110,110)),
                xaxis = list(
                        #         showticklabels = FALSE
                        title = "% Difference",
                        range = c(-110,110))
        ) %>% add_markers(
                x = data[names == "Delta"]$V1*100,
                y = data[names == "RedScore"]$V1*100,
                color = I("Red"),
                # color = I(ifelse(
                #         test = data[names == "Delta"]$V1 > 0,
                #         yes = "Dark Red",
                #         no = "Dark Blue")),
                alpha = 0.2, 
                showlegend = F) %>% add_markers(
                        x = data[names == "Delta"]$V1*100,
                        y = data[names == "BlueScore"]$V1*100*(-1),
                        color = I("Blue"),
                        # color = I(ifelse(
                        #         test = data[names == "Delta"]$V1 > 0,
                        #         yes = "Dark Red",
                        #         no = "Dark Blue")),
                        alpha = 0.2, 
                        showlegend = F) %>% add_markers(
                                x = c(data[names == "Delta"]$V1*100, data[names == "Delta"]$V1*100),
                                # y = c(data[names == "RedScore"]$V1*100, data[names == "BlueScore"]$V1*100*(-1)),
                                y = 0,
                                color = I(ifelse(
                                        test = c(data[names == "Delta"]$V1,data[names == "Delta"]$V1)*100 > 0, 
                                        yes = "Dark Red", 
                                        no = "Dark Blue")), 
                                frame = as.Date(c(data[names == "Delta"]$publish_date, data[names == "Delta"]$publish_date)),
                                size = abs(c(data[names == "Delta"]$V1, data[names == "Delta"]$V1))
                        ) %>% animation_opts(
                                1
                                # easing = "elastic"
                                # ) %>% animation_button(
                                #         x = 1,
                                #         xanchor = "right",
                                #         y = 0,
                                #         yanchor = "bottom"
                        ) %>% animation_slider(
                                currentvalue = list(
                                        prefix = "DAY", 
                                        font = list(color="Black"))
                        )
        return(p)
})

# save plots for knitr
save(PLOTS.penn, file = "display_penn.Rda")

### .......... end display 15_03_17 .......... ###

PLOTS.3d = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
        i, 
        data = long[[i]] ){ 
        setkey(data, "publish_date")
        p = plot_ly(
                #x = c(data["Delta"]$V1*100, data["Delta"]$V1*100),
                # y = data[!"Delta"]$V1*100,
                #y = c(data["RedScore"]$V1*100, data["BlueScore"]$V1*100*(-1)),
                #size = abs(c(data["Delta"]$V1, data["Delta"]$V1))*c(data["RedScore"]$V1, data["BlueScore"]$V1),
                # y = ifelse(
                #         test = data["Delta"]$V1*100 > data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100 | data["Delta"]$V1*100 < data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
                #         yes = ifelse(
                #                 test = data["Delta"]$V1*100 > 0,
                #                 yes = data["Delta"]$V1*100 - data[V1 > 0]["Delta", mean(V1, na.rm = T)]*100,
                #                 no = ifelse(
                #                         test = data["Delta"]$V1*100 < 0,
                #                         yes = data["Delta"]$V1*100 - data[V1 < 0]["Delta", mean(V1, na.rm = T)]*100,
                #                         no = 0)),
                #         no = 0),
                # name = "% Difference",
                #size = abs(c(data[names == "Delta"]$V1, data[names == "Delta"]$V1))*10,
                visible = T
        ) %>% layout(
                showlegend = F,
                hovermode = "closest",
                yaxis = list(
                        dtick = 25,
                        title = "% Salience",
                        range = c(0,110)),
                xaxis = list(
                        #         showticklabels = FALSE
                        title = "% Difference",
                        range = c(-110,110)),
                zaxis = list(
                        title = "Date"
                )
                
                # ) %>% add_markers(
                #         x = data[names == "Delta"]$V1*100,
                #         y = data[names == "RedScore"]$V1*100,
                #         #z = c(unique(data$publish_date)),
                #         z = 0,
                #         color = I("Red"),
                #         # color = I(ifelse(
                #         #         test = data[names == "Delta"]$V1 > 0,
                #         #         yes = "Dark Red",
                #         #         no = "Dark Blue")),
                #         alpha = 0.5,
                #         showlegend = F
                #         ) %>% add_markers(
                #                 x = data[names == "Delta"]$V1*100,
                #                 y = data[names == "BlueScore"]$V1*100*(-1),
                #                 #z = c(unique(data$publish_date)),
                #                 z = 0, 
                #                 color = I("Blue"),
                #                 # color = I(ifelse(
                #                 #         test = data[names == "Delta"]$V1 > 0,
                #                 #         yes = "Dark Red",
                #                 #         no = "Dark Blue")),
                #                 alpha = 0.5,
                #                 showlegend = F
        ) %>% add_markers(
                x = data[names == "Delta"]$V1*100,
                #y = 0,
                y = data[names == "RedScore"]$V1*100,
                z = unique(data$publish_date),
                # color = I(ifelse(
                #         test = data[names == "Delta"]$V1 > 0,
                #         yes = "Dark Red",
                #         no = "Dark Blue")),
                color = I("Red"),
                alpha = 0.5,
                #frame = as.Date(c(data[names == "Delta"]$publish_date, data[names == "Delta"]$publish_date)),
                size = abs(data[names == "Delta"]$V1*100)
                # mode = "lines"
        )%>% add_markers(
                x = data[names == "Delta"]$V1*100,
                #y = 0,
                y = data[names == "BlueScore"]$V1*100,
                z = unique(data$publish_date),
                # color = I(ifelse(
                #         test = data[names == "Delta"]$V1 > 0,
                #         yes = "Dark Red",
                #         no = "Dark Blue")),
                color = I("Blue"),
                alpha = 0.5,
                #frame = as.Date(c(data[names == "Delta"]$publish_date, data[names == "Delta"]$publish_date)),
                size = abs(data[names == "Delta"]$V1*100)
                # mode = "lines"
        ) %>% add_markers(
                x = data[names == "Delta"]$V1*100,
                y = 0,
                #y = data[names == "BlueScore"]$V1*100,
                z = unique(data$publish_date),
                color = I(ifelse(
                        test = data[names == "Delta"]$V1 > 0,
                        yes = "Dark Red",
                        no = "Dark Blue")),
                #alpha = 0.2,
                frame = as.Date(data[names == "Delta"]$publish_date)
                #size = abs(data[names == "Delta"]$V1*100),
                #mode = "lines"
        ) %>% animation_opts(
                1
                # easing = "elastic"
                # ) %>% animation_button(
                #         x = 1,
                #         xanchor = "right",
                #         y = 0,
                #         yanchor = "bottom"
        ) %>% animation_slider(
                currentvalue = list(
                        prefix = "DAY",
                        font = list(color="Black"))
        )
        
})

#################### DESCRIPTIVE STATS ##############################

interesting = c(11,25,13,23,16,22,3,19,7,14,1,21,12,17,26,28)

stats = lapply(seq_along(CATEGORIES[interesting]), 
               FUN = function (i, data = long[[i]]){
                       setkey(data, "names")
                       descriptive = data["Delta",summary(V1)]
                       symmetry = data["Delta",skewness(V1)]
                       shape = data["Delta",kurtosis(V1)]
                       return(list(descriptive, symmetry, shape))
               })

hi = lapply(seq_along(CATEGORIES[interesting]), 
            FUN = function (i, data = long[[i]]){ 
                    setkey(data, "names")
                    h = ggplot(data["Delta"],aes(x=V1)) +
                            geom_histogram(binwidth=.1, colour="black", fill=ifelse(
                                    test = data["Delta", median(V1, na.rm = T)] > 0,
                                    yes = "red",
                                    no = ifelse( test = !data["Delta", median(V1, na.rm = T)],
                                                 yes = "white",
                                                 no = "blue"))) + ggtitle(NEW_NAMES[interesting][i]) +
                            geom_vline(aes(xintercept= data["Delta", mean(V1, na.rm = T)]),
                                       color = "green", linetype="dashed", size=1) +
                            geom_text(y = 70, x = -0.3, label = paste("Skewness:", round(data["Delta",skewness(V1)],2), sep = " ")) +
                            geom_text(y = 30, x = -0.3, label = paste("Kurtosis:", round(data["Delta",kurtosis(V1)],2), sep = " "))
                    # h = plot_ly(
                    #         x = data["Delta"]$V1,
                    #         type = "histogram",
                    #         width = .1,
                    #         name = NEW_NAMES[i]
                    # ) %>% add_trace(
                    #         x = c(data["Delta", mean(V1)],0),
                    #         y = c(0,0),
                    #         mode = "lines",
                    #         name = "mean"
                    # ) %>% layout(
                    #         xaxis = list(
                    #                 range = c(-1,1),
                    #                 dtick = .25
                    #         )
                    
                    return (h)
            }
)

# works but SLOWLY (perhaps implemented poorly)
multiplot(plotlist = hi, cols=4)

#subplot(hi, nrows = 10, shareX = T, shareY = T) %>% layout(showlegend = F)

################ END DESCRIPTIVE STATS ##############################


# plot states:

setkey(DT, state, category, publish_date)

for (s in unique(STATES$state)){
        PLOTS.states = lapply(seq_along(CATEGORIES[1:29]), FUN = function(
                i, 
                data = DT[.(s)]
        ){ p = plot_ly(
                x = data$publish_date,
                y = data$LocalArticlesInCategoryPerDay,
                hoverinfo = "text",
                text = paste(
                        "Date: ",
                        data$publish_date,
                        "<br>",
                        "Articles:",
                        data$LocalArticlesInCategoryPerDay,
                        "<br>",
                        sep = " "),
                type = "bar")%>%layout( 
                        title = paste(
                                "Attention to",
                                NEW_NAMES[i], 
                                sep = " "))
        
        
        #htmlwidgets::saveWidget(p, paste(GRAPHS_PATH, "diff/", "'", NEW_NAMES[i], "'",".html", sep = ""))
        
        return(p)
        })
        
assign(as.character(s), PLOTS.states)
}

PLOTS.states_comp[[1]] = plotly::subplot(CA, nrows = 10, shareX = T)
