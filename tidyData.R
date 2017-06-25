getAggregated = function(DT, selectcol = NULL, selectval = NULL, aggrfun = ".N", grpby = NULL){
        # how many articles per day
        
        if (!is.null(grpby)){
                DT[!is.na(grpby), .N, by = c("publish_date", grpby)]
        }
        if (!is.null(selectcol)){
                setkey(DT, selectcol)
                DT[selectval, .N, by = c("publish_date", grpby)]
        } else {
                DT[, .N, by = c("publish_date", grpby)]
        }
        
}

setAggregated = function(name, DT, selectcol = NULL, selectval = NULL, aggrfun = ".N", grpby = NULL){
        # add columns by reference
        
}