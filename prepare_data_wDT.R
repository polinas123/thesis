cleanData = function(dt) {
        # CLEANING PROCEDURES:
        # remove NA values
        dt = data.table:::na.omit.data.table(dt)
        
        # cut irrelevant contexts
        setkey(dt, "context")
        dt = dt["American Politics",]
        
        # cut lower category probabilities:
        dt = dt[category_proba>0.9,]
        
        # remove duplicate "clean texts" from the same site:
        dt = data.table:::unique.data.table(
                x = dt, 
                by = c("sent_text", "site"))
        
        # cut dates before 2016:
        dt = dt[publish_date >= strptime(
                "2016-01-01", 
                format="%y%y-%m-%d"),]
        
        # remove trailing spaces from "site"
        dt[, site := gsub("\\s+$", "", as.character(site))]
        
        # cut non US websites
        dt = dt[site %in% WEBSITES$site,]
        
        # cut out "general" category if exists
        if ("general" %in% unique(dt$category)){
                dt = tmp[category!="general",]}
        
        return(dt)
}

readData = function(filesdir = DATA_PATH) {
        
        MYcolnames = c(
                "ROW_ID",
                "article_id",
                "site",
                "url",
                "publish_date",
                "sent_id",
                "title",
                "article_text",
                "context",
                "sent_text",
                "category",
                "category_proba")        
                
        MYcoltypes = c(
                "numeric",
                "numeric",
                "text",
                "text",
                "text",
                "numeric",
                "text",
                'text',
                "text",
                "text",
                "text",
                "numeric")
        
        filenames = list.files(filesdir, full.names = T)
        
        print (paste(length(filenames), "to process", sep = " "))
        
        last_ROW_ID = 0
        last_article_id = 0
        
        dtlist = lapply( seq_along(filenames), FUN = function(i) {
                
                tmp = data.table(
                        read_excel(
                        filenames[i], 
                        col_names = T, 
                        col_types = MYcoltypes))
                
                colnames(tmp)[1] = MYcolnames[1]
                
                tmp[,ROW_ID:= ROW_ID + last_ROW_ID]
                tmp[,article_id:= article_id + last_article_id]
                
                last_ROW_ID = as.numeric(tmp[, .SD[.N]]$ROW_ID)
                last_article_id = as.numeric(tmp[, .SD[.N]]$article_id)
                
                tmp = cleanData(tmp)
                
                print (paste(
                        "FINISHED proccessing file #", i, sep = ""))
                
                return(tmp)
        })
        
        dt = data.table:::rbindlist(dtlist)
}

readWebsites = function() {

        states = fread(file = WEBSITES_PATH, 
                       sep = ",", 
                       header = T, 
                       select = c(1:3),
                       col.names = c("site", "country", "state"),
                       key = "country",
                       data.table = T)
        states = states["US"]
        states[, country:=NULL]
        states = data.table:::unique.data.table(states, by = "site")
}

readStates = function() {
        
        states = fread(file = STATES_PATH, 
                       sep = ",", 
                       header = T, 
                       select = c(1:2),
                       col.names = c("state", "stateName"),
                       key = "state",
                       data.table = T)
}

readColors = function() {
        
        colors = fread(file = COLORS_PATH, 
                       sep = ",", 
                       header = T, 
                       select = c(1,10),
                       col.names = c("state", "color"),
                       key = "state",
                       data.table = T)
}

getDates = function(DT){
#calculate and retunr full dates range as char vector
# OR AS XTS index
# ALTERNATELY: ADD NA ROWS TO DT.
        startDate = as.Date(DT[,publish_date[1]])
        endDate = as.Date(DT[,publish_date[.N]])
        
        #dates = xts::as.xts(seq(startDate, endDate, by = "days"))
        dates = seq.Date(startDate, endDate, by = "days")
}

filterDates = function(DATES, filter = character()){
        dates <- as.character(DATES)
        dates <- dates[dates>=filter]
}

setDates = function(DT, DATES) {
        dates = data.table(DATES)
        colnames(dates) = "publish_date"
        setkey(dates, publish_date)
        
        DT = merge(DT, dates, all.y = T, by = "publish_date")
}

getArticleCountPerCategory = function(){
        # how many articles in a given category+publish_date
        setkey(DT, category, publish_date)
        DT[, 
           ArticlesInCategoryPerDay := length(unique(na.omit(article_id))), 
           by = "category,publish_date"]
        # alt. method
        #x = nrow(DT[.(topic,d), .N, by = article_id, nomatch = 0])
        #return(x)
}

getArticleCountPerCategoryPerSite = function(){
        # how many articles in a given category+publish_date
        setkey(DT, site, category, publish_date)
        DT[, 
           ArticlesInCategoryPerDayPerSite := length(
                   unique(na.omit(article_id))), 
           by = "site,category,publish_date"]
        # alt. method
        #x = nrow(DT[.(topic,d), .N, by = article_id, nomatch = 0])
        #return(x)
}

getArticleCountPerSite= function(){
        # how many articles in a given site+publish_date
        setkey(DT, site, publish_date)
        DT[,
           ArticlesInSitePerDay := length(unique(na.omit(article_id))),
           by = "site,publish_date"]
}

getNormCountPerSite = function(){
        # normalized score: in category by total per site
        setkey(DT, site, category, publish_date)
        DT[, ArticlesScorePerSite := double(nrow(DT))]
        DT[,
           ArticlesScorePerCategoryPerSite := round(
                   ArticlesInCategoryPerDayPerSite/ArticlesInSitePerDay,
                   2),
           by = "site,category,publish_date"]
}

# getNormCountPerState = function(){
#         # normalized score: in category by total per site
#         setkey(DT, state, category)
#         DT[,
#            ArticlesScorePerCategoryPerState := as.integer(
#                    sum(unique(ArticlesScorePerSite))),
#            by = "state,category"]
# }

getNationwideCount = function() {
        setkey(DT, state, category, publish_date)
        DT["Nationwide",
           NationwideArticlesInCategoryPerDay := length(unique(na.omit(article_id))),
           by = "category,publish_date"]
}

getLocalCount = function() {
        setkey(DT, state, category, publish_date)
        DT[STATES[!"Purple"]$state,
           LocalArticlesInCategoryPerDay := length(unique(na.omit(article_id))),
           by = "category,publish_date"]
}

getCountPerColor = function() {
        setkey(STATES, "color")
        setkey(DT, state, category, publish_date)
        DT[.(STATES["Red"]$state), 
           RedArticlesInCategoryPerDay:=length(
                   unique(na.omit(article_id))),
           by = "category,publish_date"]
        DT[.(STATES["Blue"]$state), 
           BlueArticlesInCategoryPerDay:=length(
                   unique(na.omit(article_id))),
           by = "category,publish_date"]
        DT[.(STATES["Purple"]$state), 
           PurpleArticlesInCategoryPerDay:=length(
                   unique(na.omit(article_id))),
           by = "category,publish_date"]
}

getScorePerGroup = function() {
        setkey(STATES, "color")
        setkey(DT, state, category, publish_date)
        DT[.(STATES["Red"]$state), 
           RedScore := round(mean(
                   unique(ArticlesScorePerCategoryPerSite)),2),
           by = "category,publish_date"]
        DT[.(STATES["Blue"]$state), 
           BlueScore := round(mean(
                   unique(ArticlesScorePerCategoryPerSite)),2),
           by = "category,publish_date"]
        DT[.(STATES["Purple"]$state), 
           PurpleScore := round(mean(
                   unique(ArticlesScorePerCategoryPerSite)),2),
           by = "category,publish_date"]
        DT[.(STATES$state), 
           AllStatesScore := round(mean(
                   unique(ArticlesScorePerCategoryPerSite)),2),
           by = "category,publish_date"]
        DT[.("Nationwide"), 
           NationwideScore := round(mean(
                   unique(ArticlesScorePerCategoryPerSite)),2),
           by = "category,publish_date"]
        DT[, 
           GeneralScore := round(mean(
                   unique(ArticlesScorePerCategoryPerSite)),2),
           by = "category,publish_date"]
}

# getBlueCount = function() {
#         setkey(STATES, "color")
#         setkey(DT, state, category, publish_date)
#         DT[.(STATES["Blue"]$state), 
#            BlueArticlesInCategoryPerDay:=length(
#                    unique(na.omit(article_id))),
#            by = "category,publish_date"]
# }

# getBlueCountPerState = function() {
#         setkey(STATES, "color")
#         setkey(DT, state, site, category, publish_date)
#         DT[.(STATES["Blue"]$state), 
#            BlueScore := mean(unique(ArticlesScorePerCategoryPerSite)),
#            by = "site,category,publish_date"]
# }
# 
# getPurpleCount = function() {
#         setkey(STATES, "color")
#         setkey(DT, state, category, publish_date)
#         DT[.(STATES["Purple"]$state), 
#            PurpleArticlesInCategoryPerDay:=length(
#                    unique(na.omit(article_id))),
#            by = "category,publish_date"]
# }

getDiff = function() {
# calculate difference Red - Blue
        setkey(DT, category, publish_date)
        DT[, Delta := double(nrow(DT))]
        DT[, Delta := max( unique( na.fill(
                RedScore, 0)) - max( unique(
                        na.fill(BlueScore, 0)))),
           by = "category,publish_date"]
}

getDiff_1 = function() {
        # calculate difference Red - Blue
        setkey(DT, category, publish_date)
        DT[, Delta_1 := double(nrow(DT))]
        DT[, Delta_1 := max( unique( na.fill(
                PurpleScore, 0)) - max( unique(
                        na.fill(RedScore, 0)))),
           by = "category,publish_date"]
}

getDiff_2 = function() {
        # calculate difference Red - Blue
        setkey(DT, category, publish_date)
        DT[, Delta_2 := double(nrow(DT))]
        DT[, Delta_2 := max( unique( na.fill(
                PurpleScore, 0)) - max( unique(
                        na.fill(BlueScore, 0)))),
           by = "category,publish_date"]
}

getDiff_3 = function() {
        # calculate difference Red - Blue
        setkey(DT, category, publish_date)
        DT[, Delta_3 := double(nrow(DT))]
        DT[, Delta_3 := max( unique( na.fill(
                AllStatesScore, 0)) - max( unique(
                        na.fill(NationwideScore, 0)))),
           by = "category,publish_date"]
}

# getDiffPerSources = function(DT, topic, d) {
#         
#         DT[.(topic,d), DeltaPerSources := max( unique( na.fill(
#                 RedArticlesOutOfRedSources, 0)) - max( unique(
#                         na.fill(BlueArticlesOutOfBlueSources, 0))))]
# }