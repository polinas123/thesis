getArticleCountPerCategory = function(DT){
        # how many articles in a given category+publish_date
        setkey(DT, category, publish_date)
        DT[, 
           ArticlesInCategoryPerDay := length(unique(na.omit(article_id))), 
           by = "category,publish_date"]
        # alt. method
        #x = nrow(DT[.(topic,d), .N, by = article_id, nomatch = 0])
        #return(x)
}

getArticleCountPerCategoryPerSite = function(DT){
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

getArticleCountPerSite= function(DT){
        # how many articles in a given site+publish_date
        setkey(DT, site, publish_date)
        DT[,
           ArticlesInSitePerDay := length(unique(na.omit(article_id))),
           by = "site,publish_date"]
}

getNormCountPerSite = function(DT){
        # normalized score: in category by total per site
        setkey(DT, site, category, publish_date)
        DT[, ArticlesScorePerSite := double(nrow(DT))]
        DT[,
           ArticlesScorePerCategoryPerSite := round(
                   ArticlesInCategoryPerDayPerSite/ArticlesInSitePerDay,
                   2),
           by = "site,category,publish_date"]
}

getNormCountPerState = function(DT){
        # normalized score: in category by total per site
        setkey(DT, state, category)
        DT[,
           ArticlesScorePerCategoryPerState := as.integer(
                   sum(unique(ArticlesScorePerSite))),
           by = "state,category"]
}

getNationwideCount = function(DT) {
        setkey(DT, state, category, publish_date)
        DT["Nationwide",
           NationwideArticlesInCategoryPerDay := length(unique(na.omit(article_id))),
           by = "category,publish_date"]
}

getLocalCount = function(DT) {
        setkey(DT, state, category, publish_date)
        DT[STATES[!"Purple"]$state,
           LocalArticlesInCategoryPerDay := length(unique(na.omit(article_id))),
           by = "category,publish_date"]
}

getCountPerColor = function(DT) {
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

getScorePerGroup = function(DT) {
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