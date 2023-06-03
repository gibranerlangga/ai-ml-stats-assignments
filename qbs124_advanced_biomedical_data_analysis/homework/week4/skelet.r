skelet <-
function(job=1)
{
	dump("skelet","c:\\QBS124\\skelet.r")
	d=read.csv("c:\\QBS124\\Goldman.csv",stringsAsFactors=F)
	female=as.numeric(d[,3])
	d=d[,18:ncol(d)]
	nm=names(d)
	nc=ncol(d);nr=nrow(d)	
	for(i in 1:nc)
	if(sum(is.na(d[,i]))==nr) {alln=i;break}
	d=d[,-alln]
	nm=nm[-alln]
	nc=ncol(d)
	R=cor(d,use="pairwise.complete.obs")
	n=nrow(R)
	if(job==1) #correlation heatmap, show in R
	{
		par(mfrow=c(1,1),mar=c(0,1,2,1))
		cl=c("deepskyblue","lightblue","green","yellow","red")
		image(1:n,1:n,breaks=c(-.75,-.5,0,.5,.75,1),ylim=c(-5,n+1),xlim=c(-2,n+.5),col=cl,ylab="",xlab="",R,axes=F)	
		text(1:n,rep(0.5,n),nm,adj=1,cex=.75,srt=45)
		text(rep(.3,n),1:n,nm,adj=1,cex=.75)
		for(i in 1:n)
		for(j in 1:n)
		text(i,j,round(R[i,j],2),cex=.5)
		mtext(side=3,paste("Correlation heatmap of",n,"osteometric measurements taken from 1538 human skeletons"),cex=2) 
		br=c("-0.75 to -0.5","-0.5 to 0","0 to 0.5","0.5 to 0.75","0.75 to 1.0")
		legend(11,-2,br,col=cl,pch=15,horiz=T,cex=1.25)
	}
	if(job==2) #save as pdf
	{
		pdf("c:\\QBS124\\slelet_2.pdf",height=15,width=18)
		par(mfrow=c(1,1),mar=c(0,1,2,1))
		cl=c("deepskyblue","lightblue","green","yellow","red")
		image(1:n,1:n,breaks=c(-.75,-.5,0,.5,.75,1),ylim=c(-5,n+1),xlim=c(-2,n+.5),col=cl,ylab="",xlab="",R,axes=F)	
		text(1:n,rep(0.5,n),nm,adj=1,cex=.75,srt=45)
		text(rep(.3,n),1:n,nm,adj=1,cex=.75)
		for(i in 1:n)
		for(j in 1:n)
		text(i,j,round(R[i,j],2),cex=.5)
		mtext(side=3,paste("Correlation heatmap of",n,"osteometric measurements taken from 1538 human skeletons"),cex=2) 
		br=c("-0.75 to -0.5","-0.5 to 0","0 to 0.5","0.5 to 0.75","0.75 to 1.0")
		legend(11,-2,br,col=cl,pch=15,horiz=T,cex=1.25)
		dev.off()	
	}
	if(job==3) #3d PCA
	{
		d=as.matrix(d)
		C=var(d,use="pairwise.complete.obs")
		colm=colMeans(d,na.rm=T)
		for(j in 1:ncol(d))
		{
			x=d[,j]
			x[is.na(x)]=colm[j]
			d[,j]=x
		}
		covd=var(d)
		eigd=eigen(covd,sym=T)
		da=(d-colm)%*%eigd$vectors[,1:3]
		namX=c("1st PCA","2nd PCA","3d PCA")
		for(theta in 1:360) #borrow from 'stock.price'
		{
			par(mfrow=c(1,1),mar=c(4,2,2,2),cex.main=2,cex.lab=2)
			op=persp(x=range(da[,1]),y=range(da[,2]),z=matrix(ncol=2,nrow=2),zlim=range(da[,3]),xlab=namX[1],ylab=namX[2],zlab=namX[3],theta=theta,phi=30,r=1000,ticktype="detailed",nticks=9,main=paste("Goldman PCA, theta=",theta,sep=""))	
			p3=trans3d(x=da[,1],y=da[,2],z=da[,3], pmat=op)
			#lines(p3$x,p3$y,lwd=3)
			points(p3$x,p3$y,pch=16,cex=1,col=female+2)
			p2=trans3d(x=da[,1], y=da[,2], z=rep(min(da[,3]),nrow(da)),pmat=op)
			segments(p3$x,p3$y,p2$x,p2$y)
			points(p2$x,p2$y,pch=2,cex=.51,col=female+2)
			legend("bottomleft",c("Female","Male"),col=2:3,pch=16,cex=1.5)
			readline()
		}	
	}
}
