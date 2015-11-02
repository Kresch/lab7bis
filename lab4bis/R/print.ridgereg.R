#method: print

#' @title print ridge
#' @name print ridge
#' @param what results of the use of ridgereg function
#' @description this function prints important results calculated by ridgereg function
#' @return the call, ie the formula of the regression, and the estimate of the coefficients, 
#' with their names

print.ridgereg<-function(X){
        cat("Call:","\n")
        print(X$call)
        cat("\n")
        cat("Coefficients:","\n")
        cat(rownames(X$coefficients),"\n")
        cat(X$coefficients)
}
