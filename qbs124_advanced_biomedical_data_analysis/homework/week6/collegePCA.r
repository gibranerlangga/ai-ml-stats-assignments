collegePCA <- function(job=1) {
#dump("collegePCA","c:\\QBS124\\collegePCA.r")
XA=read.csv("CollegeAdmData.csv")
XA=as.matrix(XA)
n=nrow(XA)
namit=c("High school curriculum","SAT score","College interview","Out of school activity","Sport & research programs","Essay","Letters of recommendation")
nam=c("HSC","SAT","CI","OSA","SR","ES","LR") #short names
m=length(nam)

if(job==1) #plot pdf/pmf
{	
	m=ncol(XA)
	par(mfrow=c(2,4))
	for(i in 1:m)
	{
		xc=XA[,i]
		if(i==1 || i>4)
		{
			pr=rep(0,5)
			for(j in 1:5) pr[j]=mean(xc==j)
			plot(1:5,pr,type="h",lwd=5,ylab="Probability",xlab=nam[i],main=namit[i])		
		}
		if(i==2)
		{
			plot(density(xc),type="l",ylab="Density",xlab=nam[i],main=namit[i])
			rug(xc)				
		}
		if(i==3 || i==4)
		{
			pr=rep(0,10)
			for(j in 1:10) pr[j]=mean(xc==j)
			plot(1:10,pr,type="h",lwd=5,ylab="Probability",xlab=nam[i],main=namit[i])		
		}	
	}
}
if(job==2) #pairwise correlation
{
	par(mfrow=c(7,7),mar=c(2,2,2,2),omi=c(0,0,0.1,0.1))
	for(i1 in 1:7)
	for(i2 in 1:7)
	{
		x1=XA[,i1];x2=XA[,i2]
		plot(x1,x2,xlab="",ylab="")	
		if(i1==1) mtext(side=3,nam[i2],line=.25,font=2)
		if(i2==m) mtext(side=4,nam[i1],line=.5,font=2)
		abline(lsfit(y=x2,x=x1),col=2,lwd=3)
		if(i1 !=i2) legend("topleft",paste("r=",round(cor(x1,x2),2),sep=""),cex=1.25,bg="gray92")
	}
}
if(job==3) #1st PCA: projection onto line, wrong direction
{
	par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),cex.lab=1.5,cex.main=1.5,cex.axis=1.25)
	sc=rep(0,n)
	for(i in 1:m)
		sc=sc+rank(XA[,i])
		
	V=var(XA)
	eV=eigen(V)
	un=rep(1,n)
	pr1=(XA-un%*%t(colMeans(XA)))%*%eV$vectors[,1]
	
	plot(sc,pr1,xlab="Total ranks/score",ylab="PCA projection onto line",main="")
	abline(lsfit(y=pr1,x=sc),lwd=3,col=2)
	h=round(eV$vectors[,1],3)
	strh=paste(n,"college applicants\nStudent synthetic score = ")
	for(j in 1:m)
	{
		if(h[j]>0) sig="+" else sig="-"
		strh=paste(strh,sig,abs(h[j]),"*",nam[j])
	}
	title(strh)	
	legend("topright","Wrong direction",cex=2,lty=1,lwd=3,col=2)
}
if(job==3.1) #1st PCA: projection onto line, fixed, right firection
{
	sc=rep(0,n)
	for(i in 1:m)
		sc=sc+rank(XA[,i])			
			
	V=var(XA)
	eV=eigen(V)
	un=rep(1,n)
	pr1=-(XA-un%*%t(colMeans(XA)))%*%eV$vectors[,1]
	
	
	par(mfrow=c(1,1),mar=c(4.5,4.5,4,1),cex.lab=1.5,cex.main=1.5,cex.axis=1.25)
	plot(sc,pr1,xlab="Total rank/score",ylab="Reverse (-) PCA projection onto line",main="")
	h=round(-eV$vectors[,1],3)
	strh=paste(n,"college applicants\nStudent synthetic score = ")
	for(j in 1:m)
	{
		if(h[j]>0) sig="+" else sig="-"
		strh=paste(strh,sig,abs(h[j]),"*",nam[j])
	}
	title(strh)		
	
	sco=sc[order(-sc)]
	segments(sco[1000],-1000,sco[1000],1000,col=3)
	points(sc[sc>=sco[1000]],pr1[sc>=sco[1000]],col=3,pch=16,cex=.5)
	
	
	pr1p=pr1[order(-pr1)]
	segments(-1000,pr1p[1000],100000,pr1p[1000],col=4)
	points(sc[pr1>=pr1p[1000]]+100,pr1[pr1>=pr1p[1000]],col=4,pch=16,cex=.5)
	
	text(sc+100,pr1,as.character(1:n),adj=0,cex=.5)
	abline(lsfit(y=pr1,x=sc),col=2,lwd=3)
	r2=cor(pr1,sc)^2
	legend("bottomright",paste("R-squared =",round(r2,2)),cex=2)
	legend("topleft",c("1000 best total rank","1000 best PCA-based"),pch=16,col=3:4,cex=1.25)
}
if(job==4) #Final decision: who is in
{
	pdf("c:\\QBS124\\collegePCA_4.pdf",width=30,height=5)
	par(mfrow=c(2,1),mar=c(3,2,3,1))
	sc=rep(0,n)
	for(i in 1:m)
		sc=sc+rank(XA[,i])
		
	V=var(XA)
	eV=eigen(V)
	un=rep(1,n)
	pr1=-(XA-un%*%t(colMeans(XA)))%*%eV$vectors[,1]
	ii=1:n
	plot(density(sc),type="l",xlab="Total rank",ylab="")
	segments(sc,rep(-1,n),sc,rep(2*10^-5,n))
	text(sc,rep(2.1*10^-5,n),ii,srt=90,adj=0)
	id.sc.adm=ii[order(-sc)]
	
	plot(density(pr1),type="l",xlab="PC1",ylab="")
	segments(pr1,rep(-1,n),pr1,rep(.001,n))
	text(pr1,rep(.0012,n),ii,srt=90,adj=0)
	dev.off()
	id.pr1.adm=ii[order(-pr1)]
	
	print("1000 Students to admit (id):")
	da=data.frame(cbind(id.pr1.adm[1:1000],id.sc.adm[1:1000]))
	names(da)=c("Max eigenvector","Total score")
	print(da)
}

}

collegePCA(job=1)
