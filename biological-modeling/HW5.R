##build a complete lfe table
lifeTable=matrix(0:9,10,1)
lx=matrix(c(1,6.2e-5,3.4e-5,2e-5,1.55e-5,1.1e-5,6.5e-6,2e-6,2e-6,0),10,1)
lifeTable=cbind(lifeTable,lx)
mx=matrix(c(0,4.6e3,8.7e3,1.16e4,1.27e4,1.27e4,1.27e4,1.27e4,1.27e4,0),10,1)
lifeTable=cbind(lifeTable,mx)
count_lxmx=matrix(lx*mx,10,1)
lifeTable=cbind(lifeTable,count_lxmx)
count_xlxmx=matrix(lifeTable[,1]*lifeTable[,4],10,1)
lifeTable=cbind(lifeTable,count_xlxmx)

##rename 
colnames(lifeTable)=c("x(Age)","lx","mx","lx*mx","x*lx*mx")
lifeTable

##count answer
R0=sum(lifeTable[,"lx*mx"])
R0
G=sum(lifeTable[,"x*lx*mx"])/R0
G
r_appr=(log(R0))/G
r_appr

##Lx and ex
lifeTable_range=matrix(0,10,2)
rownames(lifeTable_range)=c("0~1","1~2","2~3","3~4","4~5","5~6","6~7","7~8","8~9","9~10")
colnames(lifeTable_range)=c("Lx","ex")
for(i in 1:9){
	lifeTable_range[i,1]=(lifeTable[i,2]+lifeTable[i+1,2])/2
}
lifeTable_range[10,1]=lifeTable[10,2]/2
for(i in 1:10){
	lifeTable_range[i,2]=sum(lifeTable_range[i:10,1])/lifeTable[i,2]
}
lifeTable_range

##solve real r
x=matrix(lifeTable[,"lx*mx"],10,1)
x=cbind(x,x)ioooo
colnames(x)=c("lx*mx","tmp")
for(i in c(-0.1:0.1,by=1e-4)){
	sum()
}
x
