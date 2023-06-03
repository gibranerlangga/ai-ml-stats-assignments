qqband <-function(lambda=.95)
{
dump("qqband","c:\\QBS124\\qqband.r")
par(mfrow=c(1,3),mar=c(4,4,3,1))
n=c(10,100,1000)
for(i in 1:3)
{
	x=rnorm(n[i])
	x=x[order(x)]
	x=(x-mean(x))/sd(x)
	thq=qnorm(((1:n[i])-.5)/n[i])
	plot(thq,x,xlab="",ylab="")
	mtext(side=1,"Theoretical quantile",line=2.75,cex=1.25)
	mtext(side=2,"Empirical quantile",line=2.5,cex=1.25)
	mtext(side=3,paste("n =",n[i]),line=.75,cex=1.5,font=2)
#Normal qq
  q=seq(from=-8,to=8,length=1000)
	pthq=pnorm(q)
	Zl=qnorm((1+lambda)/2)
	qL1=pthq-Zl*sqrt(pthq*(1-pthq)/n[i])
	qL1[qL1<=0]=10^-7
	lb=qnorm(qL1)
	qU1=pthq+Zl*sqrt(pthq*(1-pthq)/n[i])
	qU1[qU1>=1]=1-10^-7
	ub=qnorm(qU1)
	lines(q,lb,col=2)
	lines(q,ub,col=2)
#Beta qq	
	ii=1:n[i]
	upB=qnorm(qbeta(.5+lambda/2,shape1=ii,shape2=n[i]-ii+1))
	lowB=qnorm(qbeta(.5-lambda/2,shape1=ii,shape2=n[i]-ii+1))
	lines(thq,(upB-mean(x))/sd(x),type="s",col=3)
	lines(thq,(lowB-mean(x))/sd(x),type="s",col=3)
	
	legend("topleft",c("Normal q-q","Beta q-q"),col=2:3,lty=1,bg="gray94",cex=1.75)
}


}

qqband()
