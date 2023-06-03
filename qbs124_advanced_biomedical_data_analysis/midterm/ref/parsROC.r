parsROC <- function() {
# dump("parsROC","c:\\QBS124\\parsROC.r")
#Application of the ROC-criterion for finding a parsimonious logistic regression
t0=Sys.time()
d=read.csv("Goldman.csv",stringsAsFactors=F)
nm=names(d[18:ncol(d)])
sex=as.numeric(as.vector(d[,3]))
d=as.matrix(d[,18:ncol(d)])
nc=ncol(d);nr=nrow(d)	
for(i in 1:nc)
if(sum(is.na(d[,i]))==nr) {alln=i;break}
d=d[,-c(alln,5,50,51)]
nm=nm[-c(alln,5,50,51)]
d=d[!is.na(sex),]
sex=sex[!is.na(sex)]
nr=nrow(d);nc=ncol(d)
AUC=rep(0,nc)
for(ivar in 1:nc)
{
	x=d[,ivar]
	y=sex[!is.na(x)]
	x=x[!is.na(x)]
	ni=length(x)
	n0=sum(1-y);n1=sum(y)
	o=glm(y~x,family=binomial)
	sod=sort(x)
	fp0=0
	for(i in 1:ni)
	{
		sens=sum(x<sod[i]&y==1)/n1
		fp=sum(x<sod[i]&y==0)/n0
		if(i>1) AUC[ivar]=AUC[ivar]+sens*(fp-fp0)
		fp0=fp	
	}	
}
i=1:nc
ibest=i[AUC==max(AUC)]
v1.best=d[,ibest];nm.best=nm[ibest]

y=sex[!is.na(v1.best)]
x=v1.best[!is.na(v1.best)]
n0=sum(1-y);n1=sum(y)
ni=length(x)
o=glm(y~x,family=binomial)
print(summary(o))
sod=sort(x)
AUC.best=fp0=0
sens=fp=rep(0,ni)
for(i in 1:ni)
{
	sens[i]=sum(x<sod[i]&y==1)/n1
	fp[i]=sum(x<sod[i]&y==0)/n0
	if(i>1) AUC.best=AUC.best+sens[i]*(fp[i]-fp0)
	fp0=fp[i]	
}
par(mfrow=c(1,2),mar=c(4.5,4.5,4,1),cex.lab=1.5,cex.main=1.5,cex.axis=1.25)
plot(fp,sens,type="s",lwd=3,xlab="False positive",ylab="Sensitivity",main=paste("The best ROC-criterion predictor for female:",nm.best))
text(.6,.3,paste("AUC = ",round(AUC.best*100,1),"%",sep=""),cex=2,font=2)

#finding the best second predictor
AUC=rep(0,nc)
x1=d[,ibest]
for(ivar in 1:nc)
if(ivar!=ibest)
{
	x2=d[,ivar]
	nai=!is.na(x2) & !is.na(x1)
	y=sex[nai]
	x1i=x1[nai];x2i=x2[nai]	
	ni=length(x1i)
	n0=sum(1-y);n1=sum(y)
	o=glm(y~x1i+x2i,family=binomial)
	lp=o$linear.predictors
	sod=sort(lp)
	fp0=0
	for(i in 1:ni)
	{
		sens=sum(lp>sod[i]&y==1)/n1
		fp=sum(lp>sod[i]&y==0)/n0
		if(i>1) AUC[ivar]=AUC[ivar]+sens*(fp0-fp)
		fp0=fp	
	}	
}

i=1:nc
ibest2=i[AUC==max(AUC)]
v2.best=d[,ibest2];nm.best2=nm[ibest2]

nai=!is.na(v1.best) & !is.na(v2.best)
y=sex[nai]
x1i=v1.best[nai];x2i=v2.best[nai]	
ni=length(x1i)
o=glm(y~x1i+x2i,family=binomial)
print(summary(o))
lp=o$linear.predictors
sod=sort(lp)
AUC2=0
sens=fp=rep(0,ni)
for(i in 1:ni)
{
	sens[i]=sum(lp>sod[i]&y==1)/n1
	fp[i]=sum(lp>sod[i]&y==0)/n0
	if(i>1) AUC2=AUC2+sens[i]*(fp0-fp[i])
	fp0=fp[i]	
}

plot(fp,sens,type="s",lwd=3,xlab="False positive",ylab="Sensitivity",main=paste("Two best ROC-criterion predictors for female: ",nm.best,"&",nm.best2))
text(.6,.3,paste("AUC = ",round(AUC2*100,1),"%",sep=""),cex=2,font=2)
text(.1,.15,"Bruzek (2002) reports that 95% accuracy can be achieved\nwith 5 features of the hip bone",adj=0,font=3,cex=1.4) 
print(Sys.time()-t0)
}

parsROC()
