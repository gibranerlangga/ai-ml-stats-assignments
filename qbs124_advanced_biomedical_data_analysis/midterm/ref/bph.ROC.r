bph.ROC <- function() {
    #dump("bph.ROC","c:\\QBS124\\bph.ROC.r")
    d=read.csv("bp.csv")
    h=d[,1];bp=d[,2]
    bp0=bp[h==0];bp1=bp[h==1]
    n=length(h)
    par(mfrow=c(1,2),mar=c(4.5,4.5,5,1))
    bp0=sort(bp0)
    n0=length(bp0)
    m0=mean(bp0);s0=sd(bp0)
    x=seq(from=80,to=200,length=200)
    plot(bp0,(1:n0)/n0,type="s",xlim=c(80,200),col=3,lwd=3,ylab="CDF, probability",xlab="Blood pressure")
    title("Two cdfs for normal and hypertension people")
    lines(x,pnorm(x,mean=m0,sd=s0),col=3)
    bp1=sort(bp1)
    n1=length(bp1)
    m1=mean(bp1);s1=sd(bp1)
    lines(bp1,(1:n1)/n1,type="s",col=2,lwd=3)
    lines(x,pnorm(x,mean=m1,sd=s1),col=2)
	rug(bp0,ticksize=0.075,col=3)
	rug(bp1,ticksize=0.05,col=2)
    legend("topleft",c("Normal patients empiricial cdf","Normal patients normal cdf","Hypertension patients empirical cdf",
    "Hypertension patients normal cdf"),col=c(3,3,2,2),lwd=c(3,1,3,1),lty=1,cex=1,bg="gray95")
    bp=sort(bp)
    sens=fp=toter10=rep(NA,n)
    AUC=toter=0
    for(i in 1:n)
    {
		sens[i]=sum(bp0<bp[i])/n0
		fp[i]=sum(bp1<bp[i])/n1
		toter10[i]=(1/11)*(1-sens[i])+(10/11)*fp[i]
		if(i>1) AUC=AUC+(fp[i]-fp[i-1])*sens[i]
    }
    opt.thresh=unique(bp[which(toter10==min(toter10))])
	fp10=sum(bp1<opt.thresh)/n1
	plot(fp,sens,type="l",lwd=3,xlab="False positive",ylab="Sensitivity",main="ROC curve for identificiation of the normal patient")
    segments(fp10,-1,fp10,2,col=2)
	text(fp10+.01,1,"Optimal threshold",col=2,adj=0)
	segments(opt.thresh,-1,opt.thresh,2,col=2)
    AUC.th=pnorm((m1-m0)/sqrt(s0^2+s1^2))
    segments(opt.thresh,-1,opt.thresh,2,col=2)
    text(.72,.4,paste("binormal AUC = ",round(100*AUC.th),", Empirical AUC = ",round(100*AUC),"%\nOptimal threshold =",round(opt.thresh),sep=""),cex=1.25,font=4)
    lines(pnorm(x,mean=m1,sd=s1),pnorm(x,mean=m0,sd=s0))
    legend("bottomright",c("Empirical ROC curve","Binormal ROC curve","Optimal threshold"),lty=1,lwd=c(3,1,1),col=c(1,1,2),cex=1.4,bg="gray96")    
}

bph.ROC()
