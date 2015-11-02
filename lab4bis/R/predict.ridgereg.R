#method: Predict

#' @title predict ridge
#' @param what results of the use of ridgereg function, and a numeric vector, which length is the
#' same as the number of coefficients.
#' @description this function predicts the values of the parameter we want to explain, 
#' ie it calculates y hat for the vector of values in parameter
#' @return a vector with the predicted values

#the difference here is that we want it to be able to predict for new datasets.
#these will be inputed as data.frames with the coefficient names. These can be extracted by:
#jhcf

predict<-function(result,...){
        UseMethod("predict")
}

predict.ridgereg<-function(result,newdata){
        if(!is.data.frame(newdata)){stop("newdata is not a data.frame!")}
                newdata<-cbind(intercept=rep(1,nrow(newdata)),newdata)
                res<-as.matrix(newdata)%*%result$coefficients
                return(t(res))
}
