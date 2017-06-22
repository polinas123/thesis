<<<<<<< HEAD
readWebsites = function() {
        
        states <- fread(file = WEBSITES_PATH, 
=======
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
        
        suffix = sample(100000, length(filenames), replace = F)
        
        dtlist = lapply( seq_along(filenames), FUN = function(i, s = suffix) {
                
                tmp = data.table(
                        read_excel(
                                filenames[i], 
                                col_names = T, 
                                col_types = MYcoltypes))
                
                colnames(tmp)[1] = MYcolnames[1]
                
                tmp[,article_id := paste(article_id, s[i], sep = "_")]
                
                tmp = cleanData(tmp)
                
                print (paste(
                        "FINISHED proccessing file #", i, sep = ""))
                
                return(tmp)
        })
        
        dt = data.table:::rbindlist(dtlist)
        dt[, ROW_ID := NULL]
}

readWebsites = function() {
        
        states = fread(file = WEBSITES_PATH, 
>>>>>>> 8f92d9f894673e6d046d0e15b4d9935dc99fe520
                       sep = ",", 
                       header = T, 
                       select = c(1:3),
                       col.names = c("site", "country", "state"),
                       key = "country",
                       data.table = T)
<<<<<<< HEAD
        states <- states["US"]
        states[, country:=NULL]
        states <- data.table:::unique.data.table(states, by = "site")
=======
        states = states["US"]
        states[, country:=NULL]
        states = data.table:::unique.data.table(states, by = "site")
>>>>>>> 8f92d9f894673e6d046d0e15b4d9935dc99fe520
}

readStates = function() {
        
<<<<<<< HEAD
        states <- fread(file = STATES_PATH, 
=======
        states = fread(file = STATES_PATH, 
>>>>>>> 8f92d9f894673e6d046d0e15b4d9935dc99fe520
                       sep = ",", 
                       header = T, 
                       select = c(1:2),
                       col.names = c("state", "stateName"),
                       key = "state",
                       data.table = T)
}

readColors = function() {
        
<<<<<<< HEAD
        colors <- fread(file = COLORS_PATH, 
=======
        colors = fread(file = COLORS_PATH, 
>>>>>>> 8f92d9f894673e6d046d0e15b4d9935dc99fe520
                       sep = ",", 
                       header = T, 
                       select = c(1,10),
                       col.names = c("state", "color"),
                       key = "state",
                       data.table = T)
}

getDates = function(DT){
<<<<<<< HEAD
        
        #calculate and retunr full dates range as char vector
        setkey(DT, "publish_date")
        startDate <- as.Date(DT[,publish_date[1]])
        endDate <- as.Date(DT[,publish_date[.N]])
        
        DATES <- seq.Date(startDate, endDate, by = "days")
        DATES <- as.data.table(DATES)
        colnames(DATES) <- "publish_date"
        
        return(DATES)
}

setDates = function(DT, DATES) {
        
        setkey(DATES, "publish_date")
        setkey(DT, "publish_date")
        
        DT <-  merge(DT, DATES, all.y = T, by = "publish_date")
        
        # DT <- na.fill(DT, 0) is very slow...
        lapply( seq(11,230,1), function(j){
                set(DT, which(is.na(DT[[j]])), j, 0)
        })
        
        return(DT)
=======
        #calculate and retunr full dates range as char vector
        # OR AS XTS index
        # ALTERNATELY: ADD NA ROWS TO DT.
        startDate = as.Date(DT[,publish_date[1]])
        endDate = as.Date(DT[,publish_date[.N]])
        
        #dates = xts::as.xts(seq(startDate, endDate, by = "days"))
        dates = seq.Date(startDate, endDate, by = "days")
        dates <- as.data.table(dates)
        colnames(dates) <- "publish_date"
        return(dates)
}

filterDates = function(DATES, filter = character()){
        dates <- as.character(DATES)
        dates <- dates[dates>=filter]
}

setDates = function(DT, DATES) {
        dates = data.table(DATES)
        colnames(dates) = "publish_date"
        setkey(dates, publish_date)
        setkey(DT, publish_date)
        
        DT = merge(DT, dates, all.y = T, by = "publish_date")
>>>>>>> 8f92d9f894673e6d046d0e15b4d9935dc99fe520
}