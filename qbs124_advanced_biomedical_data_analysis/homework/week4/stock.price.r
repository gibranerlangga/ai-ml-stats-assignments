stock.price <-
function(job=1,istock=c(3,45,65))
{
dump("stock.price","c:\\QBS124\\stock.price.r")
t0=Sys.time()
if(job==0)
{
	#install.packages("tidyquant")
	library(tidyquant)
	options("getSymbols.warning4.0"=FALSE)
	options("getSymbols.yahoo.warning"=FALSE)
	# Downloading Apple price using quantmod
	comp=read.csv("c:\\QBS124\\mostactive.csv",header=F)
	sym=as.character(comp[,1])
	X=matrix(ncol=500,nrow=8000)
	maxnr=0
	for(i in 1:500)
	{
		#getSymbols(sym[i], from = '2018-08-01',to = "2020-09-01",warnings = FALSE,auto.assign = TRUE)
		AAPL.df=tq_get(x=sym[i],from = "2018-08-01",to = "2021-04-20")
		#AAPL.df=as.data.frame(AAPL)
		nc=ncol(AAPL.df);nr=nrow(AAPL.df)
		X[1:nr,i]=AAPL.df$adjusted
		dat=AAPL.df$date
		if(nr>maxnr) maxnr=nr
	}
	compn=comp[!is.na(X[maxnr,]),]
	X=X[,!is.na(X[maxnr,])]
	X=as.data.frame(X[1:525,],row.names=dat[1:525])
	names(X)=as.character(compn[,1])
	write.csv(X,"c:\\QBS124\\Most active stocks prices_April_24.2021.csv")
	print(Sys.time()-t0)
}
if(job==1)
{
	X=read.csv("c:\\QBS124\\Most active stocks prices.csv")
	dat=as.character(X[,1])
	sym=names(X[,2:ncol(X)])
	X=as.matrix(X[,2:ncol(X)])
	nc=ncol(X)
	par(mfrow=c(1,1),mar=c(4.5,4.5,3,1),cex.lab=1.5,cex.main=1.5)
	matplot(1:nrow(X),X,type="l",col=1,lty=1,xlab="Time",ylab="Stock price",xaxt="n",main=paste(nc,"daily stock prices on the original scale, $ per share"))
	ind=c(seq(from=1,to=500,by=50),525)
	axis(side=1,at=ind,labels=dat[ind])
	
	text(rep(527,nc),X[525,],sym,adj=0)
	print("Press ENTER");readline()

	matplot(1:nrow(X),log(X),type="l",col=1,lty=1,xlab="Time",ylab="log stock price",xaxt="n",main="Daily stock prices on the log scale")
	axis(side=1,at=ind,labels=dat[ind])
	text(rep(526,nc),log(X[525,]),sym,adj=0)
	print("Press ENTER");readline()
	
	iss=1:(nrow(X)-1)
	RET=log(X[iss+1,])-log(X[iss,])
	nr=nrow(RET)
	matplot(1:nr,RET,type="l",ylim=c(-.25,.25),col=1,lty=1,xlab="Time",ylab="Return",xaxt="n",main="Retuns of stock prices")
	axis(side=1,at=ind[2:length(ind)],labels=dat[ind[2:length(ind)]])
	text(rep(527,nc),RET[524,],sym,adj=0)
	print("Movie creation (~ 3 min): Press ENTER/ESC");readline()
	

	br=c(-.5,-.25,0,0.25,.5,.75,1)
	cl=c("blue","deepskyblue","lightblue","yellow","brown","red")
	for(i in 1:(nr-30))
	{
		ch=as.character(i)
		if(i<10) ch=paste("00",ch,sep="")
		if(i>=10 & i<100) ch=paste("0",ch,sep="")
		jpeg(paste("c:\\QBS124\\stockR\\cor_",ch,".jpg",sep=""),height=1000,width=1000)
		par(mfrow=c(1,1),mar=c(1,1,3,1))
		R=cor(RET[1:(nr-i+1),])	
		image(1:nc,1:nc,R,breaks=br,col=cl,axes=F,xlab="",ylab="",main=dat[i])
		dev.off()
	}
	system("magick c:\\QBS124\\stockR\\*.jpg c:\\QBS124\\stockR.gif")
	print(Sys.time()-t0)
}
if(job==1.01)
{
#install.packages("pheatmap")
	library(pheatmap)
	
	X=read.csv("c:\\QBS124\\Most active stocks prices.csv")
	dat=as.character(X[,1])
	sym=names(X[,2:ncol(X)])
	X=as.matrix(X[,2:ncol(X)])
	iss=1:(nrow(X)-1)
	
	RET=log(X[iss+1,])-log(X[iss,])
	R=cor(RET)
	R=as.data.frame(R)
	names(R)=sym
	pheatmap(R)
	readline()
	R=solve(R)
	for(i in 1:ncol(R))
	for(j in 1:ncol(R))
	if(i>j)
	R[i,j]=R[j,i]=-R[i,j]/sqrt(R[i,i]*R[j,j])
	diag(R)=1
	pheatmap(R)
}
if(job==1.1) #individual stocks on the background of others
{
	ind=c(seq(from=1,to=500,by=50),525)
	comp=read.csv("c:\\QBS124\\mostactive.csv",header=F)
	X=read.csv("c:\\QBS124\\Most active stocks prices.csv")
	dat=as.character(X[,1])
	sym=names(X[,2:ncol(X)])
	X=as.matrix(X[,2:ncol(X)])
	nc=ncol(X)
	iss=1:(nrow(X)-1)
	RET=log(X[iss+1,])-log(X[iss,])
	nr=nrow(RET)
	for(i in 1:nc)
	{
		par(mfrow=c(1,1))
		matplot(1:nr,RET,type="l",ylim=c(-.25,.25),col=1,lty=1,xlab="Time",ylab="Return",xaxt="n",main=paste(comp[i,1],": ",comp[i,2],sep=""))
		lines(1:nr,RET[,i],col=2,lwd=3)
		axis(side=1,at=ind[2:length(ind)],labels=dat[ind[2:length(ind)]])
		print("Press ENTER");readline()
	}
}
if(job==2) #3d scattetplots
{
	X=read.csv("c:\\QBS124\\Most active stocks prices.csv")
	comp=read.csv("c:\\QBS124\\mostactive.csv",header=F)
	namX=names(X)
	dat=as.character(X[,1])
	sym=names(X[,2:ncol(X)])
	X=as.matrix(X[,2:ncol(X)])
	nc=ncol(X)
	iss=1:(nrow(X)-1)
	RET=log(X[iss+1,])-log(X[iss,])
	da=RET[,istock]
	namX=as.character(comp[istock,2])	
	for(theta in 1:360)
	{
		thCH=as.character(theta)
		if(theta<10) thCH=paste("00",thCH,sep="")
		if(theta>=10 & theta<100) thCH=paste("0",thCH,sep="")
		jpeg(paste("c:\\QBS124\\3stock\\3stock",thCH,".jpg",sep=""),width=1000,height=1000,quality=100)
		par(mfrow=c(1,1),mar=c(4,2,2,2),cex.main=2,cex.lab=2)
		op=persp(x=range(da[,1]),y=range(da[,2]),z=matrix(ncol=2,nrow=2),zlim=range(da[,3]),xlab=namX[1],ylab=namX[2],zlab=namX[3],theta=theta,phi=30,r=1000,ticktype="detailed",nticks=9,main=paste("3 stock returns, theta=",theta,sep=""))	
		p3=trans3d(x=da[,1],y=da[,2],z=da[,3], pmat=op)
		#lines(p3$x,p3$y,lwd=3)
		points(p3$x,p3$y,pch=16,cex=2,col=2)
		p2=trans3d(x=da[,1], y=da[,2], z=rep(min(da[,3]),nrow(da)),pmat=op)
		segments(p3$x,p3$y,p2$x,p2$y)
		points(p2$x,p2$y,pch=16,cex=1,col=3)
		dev.off()
	}
	system("magick c:\\QBS124\\3stock\\*.jpg c:\\QBS124\\3stock.gif")
	print(Sys.time()-t0)
}
}
