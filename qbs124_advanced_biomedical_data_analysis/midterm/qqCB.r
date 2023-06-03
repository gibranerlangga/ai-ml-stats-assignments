qqCB <-
function(Y=rnorm(100,mean=-1,sd=2),lambda=0.95)
{
# dump("qqCB","c:\\QBS124\\qqCB.r")
par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),cex.lab=1.5,cex.main=1.5)
Y=sort(Y)
n=length(Y)
ii=1:n;thq=qnorm((1:n)/n)
Z=(Y-mean(Y))/sd(Y)
qn=qnorm((1:n)/n)
plot(qn,Z,xlim=c(-3,3),ylim=c(-3,3),xlab="Theoretical normal quantile, Z",ylab="Ordered data Z-score",main="Q-Q plot for testing normal distribution")
segments(-5,-5,5,5,col=2)
upB=qnorm(qbeta(.5+lambda/2,shape1=ii,shape2=n-ii+1))
lowB=qnorm(qbeta(.5-lambda/2,shape1=ii,shape2=n-ii+1))
lines(thq,upB,type="s",col=3)
lines(thq,lowB,type="s",col=3)
}
