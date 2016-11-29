### simple example of basis function

x = seq(1,100,1)
mean = sin(x)
set.seed(1)
upper = mean + runif(100,0.3,1)
lower = mean - runif(100,0.3,1)
dat = data.frame(y1=upper,y2=lower)

### original dimension
par(mfrow=c(2,2))
plot(x,dat$y1,col="red",ylim=c(-3,3))
points(x,dat$y2,col="blue")

lines(x,mean,lty=2)

basis = function(x,method="sin"){
  if(method=="sin") phi = sin(x)
  if(method=="cos") phi = cos(x)
  return(phi)
}

### featured dimension
plot(basis(x),dat$y1,col="red",ylim=c(-3,3))
points(basis(x),dat$y2,col="blue")
lines(basis(x),mean,lty=2)

## if cos
plot(basis(x,method="cos"),dat$y1,col="red",ylim=c(-3,3))
points(basis(x,method="cos"),dat$y2,col="blue")
lines(basis(x,method="cos"),mean,lty=2)

### Gaussian Process

curve_fitting <- data.frame(
	x=c(0.000000,0.111111,0.222222,0.333333,0.444444,0.555556,0.666667,0.777778,0.888889,1.000000),
	t=c(0.349486,0.830839,1.007332,0.971507,0.133066,0.166823,-0.848307,-0.445686,-0.563567,0.261502))

plot(curve_fitting)

gp <- function(y,x,beta, sigma,kernel="gaussian",k.n=100,draw.conv=TRUE) {
  N = length(y)
  # Gaussian kernel
  if(kernel =="gaussian")	kernel <- function(x1, x2) exp(-(x1-x2)^2/(2*sigma^2))
  # Gram matrix
	K <- outer(x,x, kernel)
	# covariance of marginal
	C_N <- K + diag(N)/beta
	# Initialize Mean
	mean = matrix(0,k.n,1)
	# Initialize s2
	s2 = matrix(0,k.n,1)
	k = seq(0,1,length=k.n) ## +

	
	## caculate mean and s2 for each window
	for(i in 1:k.n){
	  x_n = k[i]
	  kvec = matrix(0,N,1)
	  for(j in 1:N){
	    kvec[j] = outer(x_n,x[j], kernel)
	  }
	  c = outer(x_n,x_n,kernel) + 1/beta
	  # Posterior Mean
	  mean[i] = t(kvec) %*% solve(C_N) %*% curve_fitting$t
	  # Posterior s2
	  s2[i] = c - t(kvec) %*% solve(C_N) %*% kvec
	}
	
　# t(95%) = 2.262
	t95 = 2.262
	upper = mean + t95*s2/sqrt(N)
	lower = mean - t95*s2/sqrt(N)
	
	plot(k,mean, xlim=c(0,1), ylim=c(-1,1), xlab="", ylab="",type="l",col="black",main= paste("Kernel Size:",k.n))
	par(new=T)
	plot(x,y, xlim=c(0,1), ylim=c(-1,1), xlab=paste("beta=",beta,", sigma=",sigma), ylab="")
  if (draw.conv==TRUE){
    par(new=T)
    plot(k,upper, xlim=c(0,1), ylim=c(-1,1), xlab="", ylab="",type="l",col="red")
    par(new=T)
    plot(k,lower, xlim=c(0,1), ylim=c(-1,1), xlab="", ylab="",type="l",col="blue")
  }
	
}

## Application


#### N = 10
#### tuning beta,sigma
par(mfrow=c(2,2)) 
gp(curve_fitting$t,curve_fitting$x, beta=100, sigma=0.1,k.n=100);
gp(curve_fitting$t,curve_fitting$x, beta=4, sigma=0.1,k.n=100);
gp(curve_fitting$t,curve_fitting$x, beta=25, sigma=0.3,k.n=100);
gp(curve_fitting$t,curve_fitting$x, beta=25, sigma=0.05,k.n=100);

#### change window size
par(mfrow=c(2,2)) 
gp(curve_fitting$t,curve_fitting$x, beta=10, sigma=0.3,k.n=2);
gp(curve_fitting$t,curve_fitting$x, beta=10, sigma=0.3,k.n=5);
gp(curve_fitting$t,curve_fitting$x, beta=10, sigma=0.3,k.n=7);
gp(curve_fitting$t,curve_fitting$x, beta=10, sigma=0.3,k.n=100);

### change to a big data set
curve_fitting <- data.frame(
	x=seq(0,1,length=500),
	t=rnorm(500,sin(seq(0,1,length=500)*10),0.4)  )

### fully calculate
par(mfrow=c(2,2))
plot(curve_fitting)
gp(curve_fitting$t,curve_fitting$x, beta=10, sigma=0.1,k.n=100);
gp(curve_fitting$t,curve_fitting$x, beta=1, sigma=0.05,k.n=100);
gp(curve_fitting$t,curve_fitting$x, beta=0.3, sigma=0.05,k.n=100);

### change window size
t =proc.time()
gp(curve_fitting$t,curve_fitting$x, beta=0.3, sigma=0.1,k.n=100)
proc.time() -t
t =proc.time()
gp(curve_fitting$t,curve_fitting$x, beta=0.3, sigma=0.1,k.n=50)
proc.time() -t
t =proc.time()
gp(curve_fitting$t,curve_fitting$x, beta=0.3, sigma=0.1,k.n=20)
proc.time() -t
t =proc.time()
gp(curve_fitting$t,curve_fitting$x, beta=0.3, sigma=0.1,k.n=10)
proc.time() -t

