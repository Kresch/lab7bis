#method: print

#' @title print
#' @param what results of the use of linreg function
#' @description this function prints important results calculated by linreg function
#' @return the call, ie the formula of the regression, and the estimate of the coefficients, 
#' with their names

print.linreg<-function(X){
        cat("Call:","\n")
        print(X$call)
        cat("\n")
        cat("Coefficients:","\n")
        cat(rownames(X$coefficients),"\n")
        cat(X$coefficients)
}
