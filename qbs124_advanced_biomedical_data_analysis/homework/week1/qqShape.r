qqShape <- function()
{
dump("qqShape","c:\\QBS124\\qqShape.r")
par(mfrow=c(1,3),mar=c(4,4,4,1))
n=1000
qn=qnorm((1:n)/n)

for(i in 1:3)
{
	plot(qn,qn,type="l",xlab="",ylab="",xlim=c(-3,3),ylim=c(-3,3))

	if(i==1)
	{
		x=runif(n)
		x=(x-mean(x))/sd(x)
		x=x[order(x)]			
		mtext(side=3,"Short symmetric tails",cex=1.25,line=.5)
	}
	if(i==2)
	{
		s=sample(x=c(-1,1),replace=T,size=n,prob=c(.5,.5))
		x=s*rexp(n=n,rate=1) #Laplace or double-exp distribution
		x=(x-mean(x))/sd(x)
		x=x[order(x)]			
		mtext(side=3,"Long symmetric tails",line=.5,cex=1.25)
	}
	if(i==3)
	{
		x=rchisq(n,df=3)
		x=(x-mean(x))/sd(x)
		x=x[order(x)]			
		mtext(side=3,"Long right tail\nSkewed to right",cex=1.25,line=.5)
	}

	points(qn,x)
	mtext(side=1,"Theoretical N(0,1) quantile",outer=T,line=-1.5,cex=1.25)
	mtext(side=2,"Empirical quantile",outer=T,line=-1.3,cex=1.25)

}


}


qqShape()
