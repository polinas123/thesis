getAggregated = function(DT, selectcol = NULL, selectval = NULL, aggrfun = ".N", grpby = NULL){
        # how many articles per day
        
        if (!is.null(selectcol)){
                setkey(DT, selectcol)
                DT[selectval, .N, by = c("publish_date", grpby)]
        } else {
                if (!is.null(grpby)){
                        setkeyv(DT, grpby)
                        DT[!NA_character_, .N, by = c("publish_date", grpby)]
                } else {
                        DT[, .N, by = c("publish_date")]
                }
        }
}

setAggregated = function(name, DT, selectcol = NULL, selectval = NULL, aggrfun = ".N", grpby = NULL){
        # add columns by reference
        
}