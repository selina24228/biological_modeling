K=500
r=matrix(c(1.8,2.3,2.45,2.56,2.8),1,5)
population=matrix(100,1,5)
population=rbind(population,matrix(0,99,5))
for(t in c(1:(100-1))){
	population[t+1,]=population[t,]+r*population[t,]*(1-(population[t,]/K))
}
population=cbind(c(1:100),population)
population

population2=matrix(101,1,5)
population2=rbind(population2,matrix(0,99,5))
for(t in c(1:(100-1))){
	population2[t+1,]=population2[t,]+r*population2[t,]*(1-(population2[t,]/K))
}
population2=cbind(c(1:100),population2)
population2

par(mfrow=c(2,3))
for(i in c(2:6)){
	plot(population[,1],population[,i], type = "l", xlab = "time", ylab = "Abundance",col=4) 
	lines(population[,1],population2[,i],col=2)
}
