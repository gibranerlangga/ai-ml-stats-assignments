cdf.dyn <- function(job=1,n=100)
{
dump("cdf.dyn","c:\\QBS124\\cdf.dyn.r")
t0=Sys.time()
if(job==1) #1 cdf
{
	X=exp(rnorm(n,mean=1,sd=.1)) #lognormal distribution
	X=sort(X)
	th=seq(from=min(X),to=max(X),length=n)
	for(i in 1:n)
	{
			ch=as.character(i)
			if(i<10) ch=paste("00",ch,sep="")
			if(i>=10 & i<100) ch=paste("0",ch,sep="")       
			jpeg(paste("c:\\QBS124\\cdfdyn\\cdf",ch,".jpg",sep=""),width=800,height=600)
			par(mfrow=c(1,1),mar=c(4.5,4.5,3,1),cex.lab=1.5,cex.main=1.5)
			plot(X,X,type="n",ylim=c(0,1),xlab="Data",ylab="Proportion",main="CDF is the proportion of data less than the threshold")
			rug(X,ticksize=0.1)
			segments(th[i],-1,th[i],.05,col=2,lwd=3)
			segments(min(X)-1,0,th[i],0,col=2)
			x=seq(from=0,to=th[i],length=100)
			lines(x,pnorm(log(x),mean=1,sd=.1),col=4,lwd=2)
			Xi=c(X[X<=th[i]],th[i])
			ni=length(Xi)
			lines(Xi,(1:ni)/n,type="s",col=3,lwd=3)
			legend("topleft",c("Data","Threshold","Empirical cdf","Theoretical lognormal cdf"),col=c(1,2,3,4),lwd=c(1,3,3,2),cex=1.5,bg="gray95")
			dev.off()		
	}
	system("magick c:\\QBS124\\cdfdyn\\*.jpg c:\\QBS124\\cdfdyn_job1.gif")
	print(Sys.time()-t0)
}
if(job==2) #2 cdfs for comparison
{
	X=exp(rnorm(n,mean=1,sd=.1))
	Y=exp(rnorm(1.5*n,mean=.9,sd=.08))
	XY=sort(c(X,Y))
	X=sort(X);Y=sort(Y)
	n=length(XY)
	th=XY
	for(i in 1:n)
	{
			ch=as.character(i)
			if(i<10) ch=paste("00",ch,sep="")
			if(i>=10 & i<100) ch=paste("0",ch,sep="")       
			jpeg(paste("c:\\QBS124\\cdfdyn2\\cdf",ch,".jpg",sep=""),width=1000,height=800)
			par(mfrow=c(1,1),mar=c(4.5,4.5,3,1),cex.lab=1.75,cex.main=1.75)
			plot(XY,XY,type="n",ylim=c(0,1),xlab="Combined data",ylab="Proportion",main="Two CDFs for uniform data comparison: Y < X")
			legend("topleft",c("Data","Threshold","cdf of sample X","cdf of sample Y"),col=c(1,1,2,3),lwd=c(1,3,3,3),cex=1.5,bg="gray95")
			rug(X,ticksize=0.075,col=2)
			rug(Y,ticksize=0.05,col=3)
			segments(th[i],-1,th[i],.05,lwd=3)
			segments(min(XY)-1,0,th[i],0)
			Xi=c(X[X<=th[i]],th[i])
			ni=length(Xi)
			lines(Xi,(1:ni)/length(X),type="s",col=2,lwd=2)
			
			Yi=c(Y[Y<=th[i]],th[i])
			ni=length(Yi)
			lines(Yi,(1:ni)/length(Y),type="s",col=3,lwd=2)
			
			dev.off()		
	}
	system("magick c:\\QBS124\\cdfdyn2\\*.jpg c:\\QBS124\\cdfdyn_job2.gif")
}
if(job==3) #2 cdfs for comparison and ROC curve
{
	X=exp(rnorm(n,mean=1,sd=.1))
	Y=exp(rnorm(1.5*n,mean=.9,sd=.08))
	XY=sort(c(X,Y))
	X=sort(X);Y=sort(Y)
	nX=length(X);nY=length(Y)
	n=length(XY)
	th=XY
	niY=niX=rep(NA,n)
	for(i in 1:n)
	{
			ch=as.character(i)
			if(i<10) ch=paste("00",ch,sep="")
			if(i>=10 & i<100) ch=paste("0",ch,sep="")       
			jpeg(paste("c:\\QBS124\\cdfdyn3\\cdf",ch,".jpg",sep=""),width=1200,height=600)
			par(mfrow=c(1,2),mar=c(4.5,4.5,3,1),cex.lab=1.75,cex.main=1.75)
			plot(XY,XY,type="n",ylim=c(0,1),xlab="Combined data",ylab="Proportion",main="Two CDFs for uniform data comparison: Y < X")
			legend("topleft",c("Data","Threshold","cdf of sample X","cdf of sample Y"),col=c(1,1,2,3),lwd=c(1,3,3,3),cex=1.5,bg="gray95")
			rug(X,ticksize=0.075,col=2)
			rug(Y,ticksize=0.05,col=3)
			segments(th[i],-1,th[i],.05,lwd=3)
			segments(min(XY)-1,0,th[i],0)
			Xi=c(X[X<=th[i]],th[i])
			niX[i]=length(Xi)
			lines(Xi,(1:niX[i])/nX,type="s",col=2,lwd=2)
			
			Yi=c(Y[Y<=th[i]],th[i])
			niY[i]=length(Yi)
			lines(Yi,(1:niY[i])/nY,type="s",col=3,lwd=2)
			
			plot(niX[1:i]/nX,niY[1:i]/nY,xlim=c(0,1),ylim=c(0,1),lwd=3,type="s",xlab="1-Specificity (false positive)",ylab="Sensitivity (true positive)",main="ROC curve")			
			segments(-1,-1,2,2,col=4)
			
			dev.off()		
	}
	system("magick \QBS124\\cdfdyn3\\*.jpg c:\\QBS124\\cdfdyn_job3.gif")
}


}
