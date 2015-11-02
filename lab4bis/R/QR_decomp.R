#### QR-decomp

#we have from reg. least squares-> min(y-XB)^2=min(y-QRB)^2=min(t(Q)y-t(Q)QRB)^2
#=min(t(Q)y-RB)^2. Taking the derivative gives t(Q)y=RB<->B_hat=R^(-1)t(Q)y
# so if we use "qr.R(qr(X))" and "qr.Q(qr(X))" we have the betas. And the point?
# if we have huge design matrices, we only need almost half of the values stored (computed)
# if we use "R" instead of X.

QR_decomp<-function(X,y){
        p<-ncol(X)-1
        n<-length(y)
        
        R<-qr.R(qr(X))
        Q<-qr.Q(qr(X))
        
        beta_hat_qr<-solve(R)%*%t(Q)%*%y
        
        y_hat<-X%*%beta_hat
        eps_hat<-y-y_hat
        df<-n-p
        sigma_sq<-as.numeric(t(eps_hat)%*%eps_hat)/df
        
        var_beta_qr<-sigma_sq*solve((t(R)%*%R))
        cat("Beta_hat is", beta_hat_qr,"\n")
        cat("and it's variance is:",var_beta_qr)
}