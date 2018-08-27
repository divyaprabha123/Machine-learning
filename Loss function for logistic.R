logistic <- function(theta,X,Y){
X=as.matrix(X)
tdim=dim(as.matrix(theta))
Xdim=dim(X)
y=as.matrix(Y)
#if(tdim[1]!=Xdim[2]){
#i=matrix(rep(1,length(y)),nc=1)
#X=cbind(i,X)
#}
m=nrow(X)
lambda=0.8
z=X%*%theta
temp=theta
sigmoid=1/(1+exp(-z))
temp=theta
temp[1]=0
predictions=(-((t(y))%*%(log(sigmoid))))-((t(1-y))%*%(log(1-sigmoid)))
o=drop(lambda/(2*m))*sum(temp^2)
J=((1/5)*sum(predictions))+o
return(J)

}