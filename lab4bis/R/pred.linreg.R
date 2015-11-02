#method: Pred

#' @title pred
#' @name pred
#' @param what results of the use of linreg function, and a numeric vector, which length is the
#' same as the number of coefficients.
#' @description this function predicts the values of the parameter we want to explain, 
#' ie it calculates y hat for the vector of values in parameter
#' @return a vector with the predicted values

pred<-function(result,...){
        UseMethod("pred")
}
#The error of building comes from that we didnt have the X argument in usemethod...
pred.linreg<-function(result,x){
        c(1,x)%*%result$coefficients
}
