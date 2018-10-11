#Gaussian noise
plot_gauss_noise <- function(n){
  x <- 1:n
  y <- rep(0, n)
  flips <- rbernoulli(n)
  heads <- NULL
  for(i in 1:n){
    if(flips[i]){
      heads[i] <- 1
    }else{
      heads[i] <- -1
    }
  }
  
  plot(y~x, pch=16, bty='n', axes=F, xlab="", ylab="")
  lines(x=c(1,n), y=c(0,0))
  arrows(x0=x, y0=y, x1=x, y1=heads, col="blue", length=0.15, lwd=2)
  arrows(x0=c(1,n), y0=c(0,0), x1=c(1-0.5,n+0.5), y1=c(0,0), length=0.12)
}
plot_gauss_noise(20)


#Poisson noise
plot_pois_noise <- function(n){
  x <- 1:n
  y <- rep(0, n)
  flips <- rbernoulli(n, 0.12)
  heads <- NULL
  for(i in 1:n){
    if(flips[i]){
      heads[i] <- 1
    }else{
      heads[i] <- 0
    }
  }
  
  plot(y~x, pch=16, bty='n', axes=F, xlab="", ylab="")
  lines(x=c(1,n), y=c(0,0))
  arrows(x0=x, y0=y, x1=x, y1=heads, col="blue", length=0.15, lwd=2)
  arrows(x0=c(1,n), y0=c(0,0), x1=c(1-0.5,n+0.5), y1=c(0,0), length=0.12)
}
plot_pois_noise(20)
clip(x1=0, x2=21, y1=-0.5, y2=1.5)
