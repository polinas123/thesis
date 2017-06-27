readWebsites = function() {
        
        states <- fread(file = WEBSITES_PATH, 
                       sep = ",", 
                       header = T, 
                       select = c(1:3),
                       col.names = c("site", "country", "state"),
                       key = "country",
                       data.table = T)
        states <- states["US"]
        states[, country:=NULL]
        states <- data.table:::unique.data.table(states, by = "site")
}

readStates = function() {
        
        states <- fread(file = STATES_PATH, 
                       sep = ",", 
                       header = T, 
                       select = c(1:2),
                       col.names = c("state", "stateName"),
                       key = "state",
                       data.table = T)
}

readColors = function() {
        
        colors <- fread(file = COLORS_PATH, 
                       sep = ",", 
                       header = T, 
                       select = c(1,10),
                       col.names = c("state", "color"),
                       key = "state",
                       data.table = T)
}

getDates = function(DT){
        
        #calculate and retunr full dates range as char vector
        setkey(DT, "publish_date")
        startDate <- as.Date(DT[,publish_date[1]])
        endDate <- as.Date(DT[,publish_date[.N]])
        
        DATES <- seq.Date(startDate, endDate, by = "days")
        DATES <- as.data.table(DATES)
        colnames(DATES) <- "publish_date"
        
        return(DATES)
}

