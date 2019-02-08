K=500
r=matrix(seq(1.8,3,by=0.01),1,121)

population=matrix(100,1,121)
population=rbind(population,matrix(0,500,121))
for(t in c(1:500)){
	population[t+1,]=population[t,]+r*population[t,]*(1-(population[t,]/K))
}

r_dup=r
for(i in c(1:500)){
	r_dup=rbind(r_dup,r)
}

plot(r_dup[10:500,1],population[10:500,1],type="p",xlim=c(1.5,3),ylim=c(1,1000),col="blue")
for(i in c(2:121)){
	points(r_dup[10:500,i],population[10:500,i],col="blue")
}
 