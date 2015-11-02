###
#' @title linreg
#' @param a "formula" of the form y ~ x+... and a dataframe
#' @description mimics the lm-function in R, i.e. makes linear regression.
#' @return an object with class "linreg", i.e. that has attributes according to linear regression.
#' @references https://en.wikipedia.org/wiki/Linear_regression
#' @author Niclas Lovsjö & Maxime Bonneau

#model to test functions with. erase later.



linreg<-function(formula,data){
        
        data1 <- deparse(substitute(data))
        formula1 <- c()
        formula1[2] <- paste(data1,"$",as.character(formula[2]), sep = "")
        vect <- unlist(strsplit(as.character(formula[3]), "[+]"))
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
        
        #error handling, might wanna add more.
        if(!(class(formula1)=="formula"))
        {stop(cat(formula, "is not an formula!"))}
        if(!is.data.frame(data)){
                stop(cat(data, "is not a data.frame!"))
        }
        
        #design matrix X
        X<-model.matrix(formula,data)
        
        #all.vars(formula)[2] will always be the response
        y<-as.matrix(data[all.vars(formula1)[2]])
        
        #now we can use regular lin.alg. to find the needed values
        p<-ncol(X)-1
        n<-length(y)
        
        beta_hat<-solve(t(X)%*%X)%*%(t(X)%*%y)
        y_hat<-X%*%beta_hat
        eps_hat<-y-y_hat
        df<-n-p
        sigma_sq<-as.numeric(t(eps_hat)%*%eps_hat)/df
        var_beta_hat<-sigma_sq*solve(t(X)%*%X)
        t_beta<-beta_hat/sqrt(diag(var_beta_hat))
        
        #hat-matrix: H=X(t(X)X)^-1 t(X)
        #variance of a particular residual is
        #var(e_i)=sigma_sq(1-H_(ii))
        #so that standardized residual is
        #e_i/(sqrt(var(e_i)))
        #we will use this as a method, ie rstandard.linreg
        
        #studentized residuals:
        H<-X%*%solve(t(X)%*%X)%*%t(X)   #i.e. hat-matrix
        #we define stud. residuals as eps_hat[i]/sqrt(sigma_sq*(1-H[i]))
        stud_eps<-function(){
                stud_eps<-c()
                for (i in 1:length(eps_hat)){
                        stud_eps<-c(stud_eps,eps_hat[i]/sqrt(sigma_sq*(1-H[i])))
                }
                return(stud_eps)
        }
        stud_eps<-stud_eps()
        
        result<-list(coefficients=beta_hat, resid=eps_hat, 
                     df.residual=df,rank=ncol(X)-1,
                     call=call("linreg",formula),
                     fitted.values=as.vector(X%*%beta_hat),
                     rstudent=stud_eps)
        
        #build class

        class(result) <- "linreg"

        
        #this is our new class "linreg"
        #which we have set attr to according to the list above
        # such as "coefficients" etc
        
        #set methods
        
        #I am not sure if we can define methods inside the
        #function like this.
        #         resid.linreg<<-function(result){
        #                 result$resid
        #         }
        #         coefficients.linreg<<-function(result){
        #                 result$coefficients
        #         }
        #         pred.linreg<<-function(result,x){
        #                 x%*%result$coefficients
        #         }
        
        #use those later 
        
        # devAskNewPage()
        #Cant get it to print "call...blabalba"
        #and perhaps tab it
        
        return(result)
}