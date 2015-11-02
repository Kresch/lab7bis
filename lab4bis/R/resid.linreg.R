#method: resid

#' @title resid
#' @param what results of the use of linreg function
#' @description this function takes apart the residuals of the linear regression 
#' calculated by linreg function
#' @return a vector with the resiudals

resid<-function(X){
        UseMethod("resid")
}


resid.linreg<-function(X){
        return(c(X$resid))
}

