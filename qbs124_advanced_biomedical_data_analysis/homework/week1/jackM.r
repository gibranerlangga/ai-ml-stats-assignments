jackM <-
function(N=1000)
{
dump("jackM","c:\\StatBook\\jackM.r")
par(mfrow=c(1,1),mar=c(4.5,4.5,3,1),cex.lab=1.5,cex.main=1.5)
T=c(38,45,31,29,35,40,37,36,42,41,55,59)
den=density(T,from=25,to=70)
h=den$bw
mode_T=round(den$x[den$y==max(den$y)],1)
plot(den$x,den$y,type="l",lwd=3,xlab="Commute to work, min",ylab="Density",main=paste("Density of time to commute to work, mode =",mode_T,"minutes"))
rug(T,ticksize=0.05)
segments(c(55,59),c(-1,-1),c(55,59),c(.005,.005),lwd=2)
xs=seq(from=60,to=70,length=N)
adif=rep(NA,N)
for(i in 1:N) adif[i]=mean(pnorm((T-xs[i])/h))
adif=abs(adif-0.01)
x99=xs[adif==min(adif)]
segments(x99,-1,x99,0.01,lty=2)
text(x99,0.011,paste(round(x99,1),"minutes to leave home before 9 am"),srt=90,cex=1.25,adj=0)

#Newton's iterations
t0=max(T)
for(s in 1:10)
{
	t.new=t0+(mean(pnorm((T-t0)/h))-0.01)/mean(dnorm((T-t0)/h))*h
	if(abs(t.new-t0)<0.0001) break
	t0=t.new
}
print(t.new)

}
