#Ridgereg

#' @name ridgereg
#' @title ridgereg
#' @author Niclas Lovsjö & Maxime Bonneau
#' @description Uses ridge regression to fit a model.
#' @param lambda - hyperparameter to be tuned. if not specified, we set it to 0.
#' @param a "formula" of the form y ~ x+... and a dataframe
#' @references ESLII: http://web.stanford.edu/~hastie/local.ftp/Springer/OLD/ESLII_print4.pdf


#say:
formula<-Sepal.Length~Sepal.Width+Petal.Width
data<-iris


ridgereg<-function(formula,data,lambda=0){
        norm_data<-function(vect){
                res<-(vect-mean(vect))/(sd(vect))
                return(res)
        }
        #data will come as a data.frame with one y-vector and covariates in X.
        #formula defines which are which.

        #we will use same as in linreg to define this:
        data1 <- deparse(substitute(data))
        formula1 <- c()
        formula1[2] <- paste(data1,"$",as.character(formula[2]), sep = "")
        vect <- unlist(strsplit(as.character(formula[3]), "[+]"))
        names_i_need<-vect
        for(i in 1:length(vect)){
                vect[i]<- paste(data1, "$", vect[i], sep = "")
        }
        formula1 <- paste(formula1[2], formula[1])
        formula1 <- paste(formula1, vect[1])
        j <- 2
        while(j <= length(vect)){
                formula1 <- paste(formula1, "+", vect[j])
                j <- j+1
        }
        formula1 <- as.formula(formula1)
        data<-as.data.frame(data)
        #error handling, might wanna add more.
        if(!(class(formula1)=="formula"))
        {stop(cat(formula, "is not an formula!"))}
        if(!is.data.frame(data)){
                stop(cat(data, "is not a data.frame!"))
        }
        
        #design matrix X
        X<-model.matrix(formula,training)
        #all.vars(formula)[2] will always be the response
        y<-as.matrix(training[all.vars(formula)[1]])
        
        #now we can use regular lin.alg. to find the needed values
        
        p<-ncol(X)
        n<-length(y)
        
        # norm_X<-apply(X[,2:ncol(X)],2,norm_data)
        #doesnt work for ncol(X)=2 i.e. 1 covariate obv.
        #change to:
        namn<-colnames(X)
        #a mess with the names here...
        if (ncol(X)==2){
                X<-norm_data(X[,2])
                X<-cbind(rep(1,p),X)
                colnames(X)<-namn
        } else {
                X<-apply(X[,c(2:ncol(X))],2,norm_data)
        }
        
        # colnames(X)<-c("(Intercept)",)
        #takes out intercept and normalizes X, then enter intercept after normalizing.
        #i.e. we dont wanna normalize the intercept.
        
#         B_ridge<-function(lambda){
#                 res<-solve(t(X)%*%X+lambda*diag(ncol(X)))%*%(t(X)%*%y)
#                 return(res)
#         }
        # solve(t(X)%*%X)%*%(t(X)%*%y)
        
        
        beta_hat<-solve(t(as.matrix(X))%*%as.matrix(X)+lambda*diag(ncol(X)))%*%(t(as.matrix(X))%*%(y))
        fitted.values<-X%*%beta_hat
        result<-list(coefficients=beta_hat,fitted.values=fitted.values,
                     call=call("ridgereg",formula))
        class(result)<-"ridgereg"
        
        return(result)
}