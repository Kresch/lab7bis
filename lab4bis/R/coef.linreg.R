#method: coef

#' @title coef
#' @param what results of the use of linreg function
#' @description this function only separates the estimate of the coefficients apart.
#' @return a named vector of the estimate of the coefficients

coef.linreg<-function(result){
        vect<-c(result$coefficients)
        names(vect)<-rownames(result$coefficients)
        return(vect)
}