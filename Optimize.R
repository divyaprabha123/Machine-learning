oneVsall <- function(X,yy,num_labels,lambda)
{
m=nrow(X)
nn=dim(X)
n=nn[2]
all_theta=matrix(c(rep(0,num_labels*(n))),nc=n)
initial_theta=matrix(c(rep(0,(n))),nc=1)
Vlogistic<-Vectorize(logistic)
for(i in 1:num_labels)
{
 #Vlogistic<-Vectorize(logistic)

 dataa<-as.data.frame(yy)
 dataa$temp<-0
 dataa$temp[dataa$yy==i]<-1
 Y<-dataa$temp
 theta=optim(par=initial_theta,fn=logistic,X=X,Y=Y,method="BFGS")
 all_theta[i,]=theta$par
 
}
return(all_theta)
}
 