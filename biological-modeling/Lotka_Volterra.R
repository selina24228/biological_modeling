library(deSolve)
LVmod = function(Time, State, Pars) {
    with(as.list(c(State, Pars)), {
        dx=x*(1-x)-a1*x*y/(1+b1*x)
        dy=a1*x*y/(1+b1*x)-a2*y*z/(1+b2*y)-d1*y
        dz=a2*y*z/(1+b2*y)-d2*z
        return(list(c(dx,dy,dz)))
    })
 } 
pars = c(a1=5,b1=3.3,a2=0.1,b2=2,d1=0.4,d2=0.01)
yini = c(x=0.5,y=0.15,z= 5)
times = seq(0, 1000, by = 1)
out = as.data.frame(ode(func = LVmod, y = yini, parms = pars, times = times))

par(mfcol=c(2,1))
#Oscillation plot
matplot(out[,1], out[,2:4], type = "l", xlab = "time", ylab = "Abundance",
    main = "Lotka-Volterra", lwd = 2,col=c(4,2))
legend("topright", c("x", "y","z"), col = c(4,2), lty = 1:3) 


#Phase plot
library(scatterplot3d)
scatterplot3d(out[,2],out[,3],out[,4],type='l',highlight.3d=TRUE,xlab="x",ylab="y",zlab="z",col.axis="blue",col.grid="lightblue")
points(500,50,pch="*",col=4)
