par(mfcol=c(2,1))
##Q1
r=2
k=500
a=100
out=matrix(0,100,10)
for(j in c(1:10)){
	out[1,j]=10*j+50
	for(i in c(2:100)){
		out[i,j]=out[i-1,j]+r*out[i-1,j]*(1-(out[i-1,j]/k))*((out[i-1,j]-a)/k)
	}
}
matplot(c(1:100),out[,1:10],type="l",xlab="time", ylab = "population",col=(1:10))
legend("topright",legend=c("init=50","init=60","init=70","init=80",
	"init=90","init=100","init=110","init=120","init=130","init=140","init=150"),
	pt.cex=1,cex=0.8,col = c(4,2), lty = 1:10) 
##Q2
##library(rootSolve)
##Allee_effect=function(Time,x){
##	dx=2*x*(1-(x/500))*((x-100)/500)
##	return(list(dx))
##}
##xint = c(x=10.0:500.0 by=10.0)
##result=uniroot.all(Allee_effect,x=xint,c(0,500))
##result
x=c(0:510)
y=2*x*(1-(x/500))*((x-100)/500)
plot(x,y,typ="l",ylab="dx")
abline(a=0,b=0)