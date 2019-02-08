K=500
r=matrix(c(1.8,2.3,2.45,2.56,2.8),1,5)
population=matrix(100,1,5)
population=rbind(population,matrix(0,99,5))
for(t in c(1:(100-1))){
	population[t+1,]=population[t,]+r*population[t,]*(1-(population[t,]/K))
}
population

standard=matrix(seq(1,800,by=0.1),7991,1)
iterator=function(x){
	x+r*x*(1-(x/K))
}
standard=cbind(standard,t(apply(standard,1,iterator)))
standard
par(mfrow=c(2,3))
for(j in c(1:5)){
	N=matrix(0,198,2)
	for(i in c(1:99)){
		N[2*i-1,1]=population[i,j]
		N[2*i-1,2]=population[i+1,j]
		N[2*i,1]=population[i+1,j]
		N[2*i,2]=population[i+1,j]
	}
	plot(c(1:700),c(1:700),xlim=c(1,1000),ylim=c(1,1000),type = "l", xlab = "N(t)", ylab = "N(t+1)",col=2)
	lines(N[,1],N[,2], col=4)
	lines(standard[,1],standard[,j+1],col="green")
} 