##build Leslie matrix
teasel=matrix(c(0,0.966,0.013,0.007,0.008,0,
                0,0,0.01,0,0,0,
                0,0,0.125,0.125,0,0,
                0,0,0,0.238,0.245,0.023,
                0,0,0,0,0.167,0.75,
                322.38,0,3.448,30.17,0.862,0),6,6)
teasel

##population size over 20 years and growth rate
growthRate=0
ageStructure=matrix(0,6,21)
ageStructure[,1]=c(10,10,10,10,10,10)
for(i in c(1:20)){
	ageStructure[,i+1]=teasel%*%ageStructure[,i]
	growthRate=growthRate+(sum(ageStructure[,i+1])/sum(ageStructure[,i]))
}
growthRate=growthRate/20
growthRate
re_scale=function(x){
	x=(x/sum(x))
}
ageStructure=apply(ageStructure,2,re_scale)
ageStructure

##stable age structure
##by observate the result above
##we find that the stable percentage of six class is {0.64,0.26,0.01,0.07,0.01,0.005}

##relative reproductive valu
##sensitivity
sensitivity_test=matrix(c(1,2,1,3,1,4,1,5,2,3,3,3,3,4,4,4,4,5,4,6,5,5,5,6,6,1,6,3,6,4,6,5),16,2,byrow=T)
sensitivity_test=cbind(sensitivity_test,matrix(0,16,1))
colnames(sensitivity_test)=c("x","contribution","result")
for(i in c(1:16)){
	teasel_cpy=teasel
	teasel_cpy[sensitivity_test[i,0],sensitivity_test[i,1]]=teasel_cpy[sensitivity_test[i,0],sensitivity_test[i,1]]+0.01
	eigens=matrix(eigen(teasel_cpy,only.values=T),6,1)
	sort(eigens)
}
sensitivity_test
