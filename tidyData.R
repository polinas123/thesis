getAggregated = function(DT, selectcol = NULL, selectval = NULL, aggrfun = ".N", grpby = NULL){
        # how many articles per day
        
        if (!is.null(selectcol)){
                setkey(DT, selectcol)
                DT[selectval, .N, by = c("publish_date", grpby)]
        } else {
                if (!is.null(grpby)){
                        DT[, .N, by = c("publish_date", grpby)]
                } else {
                        DT[, .N, by = c("publish_date")]
                }
        }
}

setDates = function(DT, DATES, cols) {
        
        setkey(DATES, "publish_date")
        setkey(DT, "publish_date")
        
        DT <-  merge(DT, DATES, all.y = T, by = "publish_date")
        
        #DT <- DT[, (cols) := sapply(.SD, na.fill, fill = 0), .SDcols = cols]
        lapply(seq(8,14,1), function(j){
                set(DT, which(is.na(DT[[j]])), j, 0)
        })

        return(DT)
}