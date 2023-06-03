mah <-
function(job=1,n1=500,n2=300,mu1=c(2,2),mu2=c(3,3.5),sd1=1.5,sd2=3,ro=-.4,ss=3)
{
dump("mah","c:\\QBS124\\mah.r")
set.seed(ss)
if(job==1)
{	
	par(mfrow=c(1,1),mar=c(4,4,4,1))
    Omega=matrix(c(sd1^2,sd1*sd2*ro,sd1*sd2*ro,sd2^2),2,2)
    co=chol(Omega)
    p1=cbind(rnorm(n1),rnorm(n1))
	  p1=p1%*%co+rep(1,n1)%*%t(mu1)
    p2=cbind(rnorm(n2),rnorm(n2))
    p2=p2%*%co+rep(1,n2)%*%t(mu2)
    p12=rbind(p1,p2)
    plot(p12,type="n",xlab="x",ylab="y")
    points(p1[,1],p1[,2],col=2)
    points(mu1[1],mu1[2],pch=3,cex=4)
    points(p2[,1],p2[,2],col=3)
    points(mu2[1],mu2[2],pch=3,cex=4)
readline()	
    z=seq(from=0,to=1,length=100)
    lines(mu1[1]*z+mu2[1]*(1-z),mu1[2]*z+mu2[2]*(1-z))
    a=solve(Omega)%*%(mu1-mu2)
    x=seq(from=-100,to=100,length=100)
    ma=(mu1+mu2)/2
    y=ma[2]-a[1]/a[2]*(x-ma[1])
    lines(x,y,lwd=3)
readline()	
    i1=1:n1;i2=1:n2
    ind=1:(n1+n2);memb=c(rep(1,n1),rep(2,n2))
    classR=(p12-(rep(1,n1+n2)%*%t(ma)))%*%a
    #IFer=(classR<0 & memb==1) | (classR>0 & memb==2)
	IFer=sum(classR<0 & memb==1)
    EmpErr=IFer/n1
    delta2=t(mu1-mu2)%*%solve(Omega)%*%(mu1-mu2)
    TheorErr=pnorm(-sqrt(delta2)/2)
    title(paste("Fisher LDA for classification, the total error of misclassification =",round(2*TheorErr,3),"\nEmpirical 1st cluster miscl prob =",round(EmpErr,4),", theoretical 1st cluster miscl error =",round(TheorErr,4)))
	print("Press Enter");readline()
#Logistic regression
	Y=c(rep(0,n1),rep(1,n2))
	go=glm(Y~p12,family=binomial())
	so=summary(go)
	print(so)
	co=coef(go)
	z=seq(from=-100,to=100,length=100)
	lines(z,-(co[1]+co[2]*z)/co[3],col=4,lwd=3)
	legend("bottomleft",c("Discriminant analysis","Logistic regression"),lty=1,lwd=3,col=c(1,4),cex=1.25)
	
}
if(job==2) #ROC curve
{
	par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),cex.lab=1.5,cex.main=1.5)
    Omega=matrix(c(sd1^2,sd1*sd2*ro,sd1*sd2*ro,sd2^2),2,2)
    co=chol(Omega)
    p1=cbind(rnorm(n1),rnorm(n1))
	p1=p1%*%co+rep(1,n1)%*%t(mu1)
    p2=cbind(rnorm(n2),rnorm(n2))
    p2=p2%*%co+rep(1,n2)%*%t(mu2)
    p12=rbind(p1,p2)
	memb=c(rep(1,n1),rep(2,n2))
	a=solve(Omega)%*%(mu1-mu2)
    ma=(mu1+mu2)/2
    classR=(p12-rep(1,n1+n2)%*%t(ma))%*%a
    #IFer=(classR<0 & memb==1) | (classR>0 & memb==2)
	n=n1+n2
	sens=fp=rep(0,n)
	s.classR=sort(classR)
	AUC=0
	for(i in 1:n)
	{
		sens[i]=sum(classR>s.classR[i] & memb==1)/sum(memb==1)
		fp[i]=sum(classR>s.classR[i] & memb==2)/sum(memb==2)	
		if(i>1) AUC=AUC+(fp[i-1]-fp[i])*sens[i]
	}
	delta=sqrt(sum(a*(mu1-mu2)))
	plot(fp,sens,type="s",lwd=3,xlab="False positive rate",ylab="Sensitivity",main="ROC curve for the LDA classification")
	text(.2,.4,paste("Empirical AUC (LDA) = ",round(100*AUC,1),"%",cex=""),cex=1.5,adj=0,font=3)
	text(.2,.35,paste("Binormal AUC = ",round(100*pnorm(delta/sqrt(2)),1),"%",cex=""),cex=1.5,adj=0,font=3,col=2)
	legend("bottomright",c("Empirical ROC curve","Binormal ROC curve"),col=1:2,lwd=c(3,1),cex=1.25)
	u=seq(from=delta^2/2-4*delta,to=delta^2/2+4*delta,length=1000)
	lines(pnorm((u-delta^2/2)/delta),pnorm((u+delta^2/2)/delta),col=2,lwd=3)
	
#Logistic regression
	Y=c(rep(0,n1),rep(1,n2))
	LP=glm(Y~p12,family=binomial())$linear.predictors
	LP=sort(LP)
	sens=fp=rep(0,n)
	s.classR=Y
	AUC=0
	for(i in 1:n)
	{
		sens[i]=sum(classR<LP[i] & Y==1)/sum(Y==1)
		fp[i]=sum(classR<LP[i] & Y==0)/sum(Y==0)	
		if(i>1) AUC=AUC+(fp[i]-fp[i-1])*sens[i]
	}
	lines(fp,sens,type="l",col=4,lwd=3)	
	text(.2,.3,paste("Empirical AUC (logistic regression) = ",round(100*AUC,1),"%",cex=""),cex=1.5,adj=0,font=3,col=4)
}	
}

mah()
 