# Set up the function:
tprob <- function(t=1, df=3, show.normal=TRUE, xlim=c(-4,4)){
  curve(dt(x, df), from=xlim[1], to=xlim[2], lwd=2)
  xlo = seq(xlim[1], -abs(t), length.out=100)
  xhi = seq(abs(t), xlim[2], length.out=100)
  polygon(c(xlo[1],xlo,xlo[100]), c(0,dt(xlo,df),0), col="#128b37", border=NA)
  polygon(c(xhi[1],xhi,xhi[100]), c(0,dt(xhi,df),0), col="#128b37", border=NA)  
  abline(h=0, v=c(-abs(t),abs(t)), col=c("gray","orange","orange"), lwd=c(1,3,3))
  text(xlo[1], dt(.5,df), paste("Area = ", round(pt(-abs(t), df)*2,4)), pos=4)
  if(show.normal){
    curve(dnorm(x), add=TRUE, col="gray")
  }
}

# Use the function
tprob(t=-2)
tprob(t=-8, xlim=c(-9,9))
tprob(t=-2, df=15)
