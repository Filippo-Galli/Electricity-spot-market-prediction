# An Introduction to Functional Data Analysis
# Applied Statistics, 2022/2023 
# 

####  
#### From rough data to smooth functions #### 
####  

setwd("~/Documents/politecnico/corsi/2023 Applied Statistics")


# Partly based on Ramsay, Hooker, Graves, 
# "Functional Data Analysis with R and Matlab", Springer, 2009


# Upload noisy data
noisycurve <- read.table("noisycurvebis.txt",header=T)
head(noisycurve)
dim(noisycurve)

Xobs0 <- noisycurve$X0
abscissa <- noisycurve$Abscissa
NT <- length(abscissa) # number of locations of observations


plot(abscissa,Xobs0,xlab="t",ylab="observed data")
plot(abscissa,Xobs0,xlab="t",ylab="observed data", type = "l")

# Upload true data (without noise)
# X0 contains the values of the true curve
# X1 contains the values of the true first derivative
# X2 contains the values of the true second derivative
truecurve <- read.table("truecurve.txt",header=T)
head(truecurve)

points(abscissa,truecurve$X0vera,type="l", col = 2, lwd = 2)

# compute the central finite differences
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])

par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data",type="l")
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data",main = "function")
points(truecurve$Abscissa,truecurve$X0vera,type='l',col="orange",lwd=3)
legend("topleft", legend = c("noisy data","true curve"), col = c("black", "orange"), lwd = c(1,2))
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l",main = "1st derivative")
points(truecurve$Abscissa,truecurve$X1vera,type='l',col="orange",lwd=3)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l",main = "2nd derivative")
points(truecurve$Abscissa,truecurve$X2vera,type='l',col="orange",lwd=3)



#### 
#### REGRESSION SPLINES ####
####

# Load package fda
library(fda)

# Set parameters
m <- 5           # spline order 
degree <- m-1    # spline degree 

nbasis <- 9

# Create the basis
help(create.bspline.basis)
basis <- create.bspline.basis(rangeval=c(0,1), nbasis=nbasis, norder=m)
# If breaks are not provided, equally spaced knots are created
names(basis)


plot(basis)

# Evaluate the basis on the grid of abscissa
basismat <- eval.basis(abscissa, basis)
dim(basismat) # number of data x number of basis
head(basismat)

# Fit via LS
help(lsfit)

est_coef = lsfit(basismat, Xobs0, intercept=FALSE)$coef
est_coef

Xsp0 <- basismat %*% est_coef

par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
abline(v=basis$params)

# to obtain the first derivative (argument Lfdobj=1)
basismat1<- eval.basis(abscissa, basis, Lfdobj=1)
head(basismat1)
Xsp1 <- basismat1 %*% est_coef

# to obtain the second derivative (argument Lfdobj=2)
basismat2<- eval.basis(abscissa, basis, Lfdobj=2)
Xsp2 <- basismat2 %*% est_coef


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,truecurve$X0vera ,type="l",col="orange",lwd=3)
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
legend("topleft", legend = c("noisy data","true curve","estimated curve"), col = c("black", "orange","blue"), lwd = c(1,3,2))
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(truecurve$Abscissa,truecurve$X1vera,type='l',col="orange",lwd=3)
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(truecurve$Abscissa,truecurve$X2vera,type='l',col="orange",lwd=3)
points(abscissa,Xsp2 ,type="l",col="blue",lwd=2)

dev.off()

############################
# alternative code 
help(smooth.basis)
Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative
Xsp2bis <- eval.fd(abscissa, Xsp$fd, Lfd=2) # second derivative
df <- Xsp$df   #  the degrees of freedom in the smoothing curve  
df             #  for regression splines the df are the number of basis
############################


#### Oversmoothing: number of basis too low ####

nbasis <- 7

basisbis <- create.bspline.basis(c(0,1), nbasis, m)
par(mfrow=c(1,1))
plot(basisbis)

basismatbis <- eval.basis(abscissa, basisbis)
Xsp0bis <- basismatbis %*% lsfit(basismatbis, Xobs0, intercept=FALSE)$coef

basismat1bis <- eval.basis(abscissa, basisbis,Lfdobj=1)
Xsp1bis <- basismat1bis %*% lsfit(basismatbis, Xobs0, intercept=FALSE)$coef

basismat2bis <- eval.basis(abscissa, basisbis,Lfdobj=2)
Xsp2bis <- basismat2bis %*% lsfit(basismatbis, Xobs0, intercept=FALSE)$coef


par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="green",lwd=2)
abline(v=basisbis$params,lty=2)


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
legend("topleft", legend = c("noisy data","estimate df = 7","estimate df = 9"), col = c("black", "green","blue"), lwd = c(1,2,2))
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xsp2bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp2 ,type="l",col="blue",lwd=2)


#### Undersmoothing: number of basis too high ####

nbasis <- 30

basister <- create.bspline.basis(c(0,1), nbasis, m)
par(mfrow=c(1,1))
plot(basister)

basismatter <- eval.basis(abscissa, basister)
Xsp0ter <- basismatter %*% lsfit(basismatter, Xobs0, intercept=FALSE)$coef

basismat1ter <- eval.basis(abscissa, basister,Lfdobj=1)
Xsp1ter <- basismat1ter %*% lsfit(basismatter, Xobs0, intercept=FALSE)$coef

basismat2ter <- eval.basis(abscissa, basister,Lfdobj=2)
Xsp2ter <- basismat2ter %*% lsfit(basismatter, Xobs0, intercept=FALSE)$coef


par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0ter ,type="l",col="red",lwd=2)
abline(v=basister$params,lty=2)


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0ter ,type="l",col="red",lwd=2)
points(abscissa,Xsp0bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1ter ,type="l",col="red",lwd=2)
points(abscissa,Xsp1bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xsp2ter ,type="l",col="red",lwd=2)
points(abscissa,Xsp2bis ,type="l",col="green",lwd=2)
points(abscissa,Xsp2 ,type="l",col="blue",lwd=2)



# generalized cross-validation
nbasis <- 6:30
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,1), nbasis[i], m)
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v = nbasis[which.min(gcv)], col = 2)


#### Bias-Variance tradeoff ####

sigma <- 0.003 # True sigma. Estimated before as sigmahat
nbasis <- 9:15
integrationinterval <- 11:90
bias <- rep(NA,len=length(nbasis))
var <- rep(NA,len=length(nbasis))
for (j in 1:length(nbasis)){
  basis <- create.bspline.basis(c(0,1), nbasis[j], m)
  basismat <- eval.basis(abscissa, basis)
  S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat)
  bias[j] <- sum((truecurve$X0vera-S%*%truecurve$X0vera)[integrationinterval])
  var[j] <- (sigma^2)*sum(diag(S[integrationinterval,integrationinterval]))
}
mse <- var+bias^2

par(mfrow=c(1,1))
plot(nbasis,bias^2,ylim=c(0,max(mse)),type="l",ylab="",main="Bias-Variance tradeoff")
points(nbasis,var,col="red",type="l")
points(nbasis,mse,col="green",type="l",lwd=3)
legend('topright', c("Bias","Var","MSE"), col=c("black","red","green"), 
       lty=1, cex=.5)



#### Approximate pointwise confidence intervals ####
# As in linear models, we can estimate the variance of x(t) as
# sigma^2*diag[phi*(phi'phi)^{-1}(phi)']
S <- basismat%*%solve(t(basismat)%*%basismat)%*%t(basismat) #projection operator 
sum(diag(S))
sigmahat <- sqrt(sum((Xsp0-Xobs0)^2)/(NT-df)) #estimate of sigma
lb <- Xsp0-qnorm(0.975)*sigmahat*sqrt(diag(S))
ub <- Xsp0+qnorm(0.975)*sigmahat*sqrt(diag(S))


plot(abscissa,Xsp0,type="l",col="blue",lwd=2,ylab="")
points(abscissa,lb,type="l",col="blue",lty="dashed")
points(abscissa,ub,type="l",col="blue",lty="dashed")
points(abscissa,truecurve$X0vera,type="l")




####
#### SMOOTHING SPLINES ####
####

# breaks <- abscissa[((0:50)*2)+1]
breaks <- abscissa

basis <- create.bspline.basis(breaks, norder=m)
functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=1e-8)  
# functional parameter, having arguments: 
# basis, order of the derivative to be penalized, smoothing parameter.

Xss <- smooth.basis(abscissa, Xobs0, functionalPar)

Xss0 <- eval.fd(abscissa, Xss$fd, Lfd=0)
Xss1 <- eval.fd(abscissa, Xss$fd, Lfd=1)
Xss2 <- eval.fd(abscissa, Xss$fd, Lfd=2)

df <- Xss$df   #  the degrees of freedom in the smoothing curve
df
gcv <- Xss$gcv  #  the value of the gcv statistic
gcv


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xss0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xss1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xss2 ,type="l",col="blue",lwd=2)

# change lambda: 1e-5
functionalParbis <- fdPar(fdobj=basis, Lfdobj=3, lambda=1e-5)  

Xssbis <- smooth.basis(abscissa, Xobs0, functionalParbis)

Xss0bis <- eval.fd(abscissa, Xssbis$fd, Lfd=0)
Xss1bis <- eval.fd(abscissa, Xssbis$fd, Lfd=1)
Xss2bis <- eval.fd(abscissa, Xssbis$fd, Lfd=2)

dfbis <- Xssbis$df   #  the degrees of freedom in the smoothing curve
dfbis
gcvbis <- Xssbis$gcv  #  the value of the gcv statistic
gcvbis


# change lambda: 1e-12
functionalParter <- fdPar(fdobj=basis, Lfdobj=3, lambda=1e-12)  

Xsster <- smooth.basis(abscissa, Xobs0, functionalParter)

Xss0ter <- eval.fd(abscissa, Xsster$fd, Lfd=0)
Xss1ter <- eval.fd(abscissa, Xsster$fd, Lfd=1)
Xss2ter <- eval.fd(abscissa, Xsster$fd, Lfd=2)

dfter <- Xsster$df   #  the degrees of freedom in the smoothing curve
dfter
gcvter <- Xsster$gcv  #  the value of the gcv statistic
gcvter


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xss0ter ,type="l",col="red",lwd=2)
points(abscissa,Xss0bis ,type="l",col="green",lwd=2)
points(abscissa,Xss0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xss1ter ,type="l",col="red",lwd=2)
points(abscissa,Xss1bis ,type="l",col="green",lwd=2)
points(abscissa,Xss1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xss2ter ,type="l",col="red",lwd=2)
points(abscissa,Xss2bis ,type="l",col="green",lwd=2)
points(abscissa,Xss2 ,type="l",col="blue",lwd=2)

# Recommendation: when choosing the smoothing parameter, look at the
# derivatives vs central finite differences

# generalized cross-validation
lambda <- 10^seq(-12,-5,by = 0.5)
gcv <- numeric(length(lambda))
for (i in 1:length(lambda)){
  functionalPar <- fdPar(fdobj=basis, Lfdobj=3, lambda=lambda[i])  
  gcv[i] <- smooth.basis(abscissa, Xobs0, functionalPar)$gcv
}
par(mfrow=c(1,1))
plot(log10(lambda),gcv)
lambda[which.min(gcv)]


# best lambda
functionalParbest <- fdPar(fdobj=basis, Lfdobj=3, lambda=lambda[which.min(gcv)])  

Xssbest <- smooth.basis(abscissa, Xobs0, functionalParbest)

Xss0best <- eval.fd(abscissa, Xssbest$fd, Lfd=0)
Xss1best <- eval.fd(abscissa, Xssbest$fd, Lfd=1)
Xss2best <- eval.fd(abscissa, Xssbest$fd, Lfd=2)

dfbest <- Xssbest$df   #  the degrees of freedom in the smoothing curve
dfbest
gcvbest <- Xssbest$gcv  #  the value of the gcv statistic
gcvbest


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xss0ter ,type="l",col="red",lwd=1)
points(abscissa,Xss0bis ,type="l",col="green",lwd=1)
points(abscissa,Xss0best ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xss1ter ,type="l",col="red",lwd=1)
points(abscissa,Xss1bis ,type="l",col="green",lwd=1)
points(abscissa,Xss1best ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xss2ter ,type="l",col="red",lwd=1)
points(abscissa,Xss2bis ,type="l",col="green",lwd=1)
points(abscissa,Xss2best ,type="l",col="blue",lwd=2)



####
#### LOCAL POLYNOMIAL REGRESSION ####
####

library(KernSmooth)
help(locpoly)

m <- 5           # order of the polynomial
degree <- m-1    # degree of the polynomial


bw <- 0.05 # bandwidth

Xsm0 <- locpoly(abscissa, Xobs0, degree=degree,
                bandwidth=bw, gridsize=length(abscissa), 
                range.x=range(abscissa))
Xsm0 <- Xsm0$y


par(mfrow=c(1,1))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsm0 ,type="l",col="blue")

# Estimate of the derivatives

Xsm1 <- locpoly(abscissa,Xobs0,drv=1,degree=degree,bandwidth=bw,
                gridsize=length(abscissa), range.x=range(abscissa))
Xsm1 <- Xsm1$y

Xsm2 <- locpoly(abscissa,Xobs0,drv=2,degree=degree,bandwidth=bw,
                gridsize=length(abscissa), range.x=range(abscissa))
Xsm2 <- Xsm2$y

par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsm0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsm1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xsm2 ,type="l",col="blue",lwd=2)



# Changing the bandwidth: larger value 
bw <- 0.15

Xsm0bis <- locpoly(abscissa,Xobs0,drv=0,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm0bis <- Xsm0bis$y

Xsm1bis <- locpoly(abscissa,Xobs0,drv=1,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm1bis <- Xsm1bis$y

Xsm2bis <- locpoly(abscissa,Xobs0,drv=2,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm2bis <- Xsm2bis$y


par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsm0bis ,type="l",col="green",lwd=2)
points(abscissa,Xsm0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsm1bis ,type="l",col="green",lwd=2)
points(abscissa,Xsm1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xsm2bis ,type="l",col="green",lwd=2)
points(abscissa,Xsm2 ,type="l",col="blue",lwd=2)

# a too large bandwidth leads to oversmoothing


# a too small bandwidth leads to undersmoothing
bw <- 0.015 

Xsm0ter <- locpoly(abscissa,Xobs0,drv=0,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm0ter <- Xsm0ter$y

Xsm1ter <- locpoly(abscissa,Xobs0,drv=1,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm1ter <- Xsm1ter$y

Xsm2ter <- locpoly(abscissa,Xobs0,drv=2,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm2ter <- Xsm2ter$y

par(mfrow=c(1,3))
plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsm0ter ,type="l",col="red",lwd=2)
points(abscissa,Xsm0bis ,type="l",col="green",lwd=2)
points(abscissa,Xsm0 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsm1ter ,type="l",col="red",lwd=2)
points(abscissa,Xsm1bis ,type="l",col="green",lwd=2)
points(abscissa,Xsm1 ,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="t",ylab="second differences x",type="l")
points(abscissa,Xsm2ter ,type="l",col="red",lwd=2)
points(abscissa,Xsm2bis ,type="l",col="green",lwd=2)
points(abscissa,Xsm2 ,type="l",col="blue",lwd=2)

# Recommendation: when choosing the bandwidth, look at the
# derivatives vs central finite differences



#### Constrained functions ####


####
#### SMOOTHING for positive curves ####
####

# y_j = exp(w(t_j)) + e_j

# f(t) = exp(w(t)) 

# The function w(t) is unconstrained
# The function f(t) is positive

# w(t) is modeled via a basis expansion 
# numerical methods are used to compute the coefficients of the 
# basis expansion

help(smooth.pos)




####
#### SMOOTHING for monotone curves ####
####


# Example: Berkeley growth data
 
help(growth)
names(growth)


matplot(growth$age, growth$hgtf, type = "l")

# If we neglect considering that the curves must be monotone...

age <- growth$age
heightbasis12 <- create.bspline.basis(rangeval = c(1,18), nbasis = 12, norder = 6)
basismat <- eval.basis(evalarg = growth$age, basisobj = heightbasis12)
heightmat <- growth$hgtf
heightcoef <- lsfit(x = basismat, y = heightmat, intercept=FALSE)$coef

height <- basismat %*% lsfit(basismat, heightmat, intercept=FALSE)$coef

basismat1 <- eval.basis(evalarg = growth$age, basisobj = heightbasis12,
                         Lfdobj=1)
heightvelocity <- basismat1 %*% lsfit(x = basismat, y = heightmat, 
                                   intercept=FALSE)$coef

basismat2 <- eval.basis(evalarg = growth$age, basisobj = heightbasis12,Lfdobj=2)
heightacceleration <- basismat2 %*% lsfit(x=basismat, y= heightmat, intercept=FALSE)$coef


par(mfrow=c(1,3))
matplot(age,height,type="l" )
matplot(age,heightvelocity,type="l" )
abline(h=0)
matplot(age,heightacceleration,type="l")


# par(mfrow=c(1,3))
# matplot(age,height,type="l" )
# matplot(age[-c(1,2,3,31)],heightvelocity[-c(1,2,3,31),],type="l" )
# abline(h=0)
# matplot(age[-c(1,2,3,31)],heightacceleration[-c(1,2,3,31),],type="l")


# a negative velocity does not make any sense (girls height does not 
# decrease over this age interval)


# A model for monotone curves

# f(t) = int_(t_0)^t exp(w(u)) du
# y_j = b_0 + b_1 * f(t_j) + e_j

# The function w(t) is unconstrained
# The function f(t) is monotone increasing
# b_1>0 for monotone increasing functions
# b_1<0 for monotone decreasing functions
# b_0 is the value of the function at t_0

# w(t) is modeled via a basis expansion 
# numerical methods are used to compute the coefficients of the
# basis expansion, as well as b_0, b_1

nage <- length(age)
ageRng <- range(age)
nfine <- 101
agefine <- seq(ageRng[1], ageRng[2], length=nfine)

# Let's consider only the first 5 girls 

hgtf <- growth$hgtf[,1:5]
ncasef <- dim(hgtf)[2]

# We set up an order 6 spline basis with knots at ages of observations

norder <- 6
nbasis <- nage - 2 + norder 
wbasis <- create.bspline.basis(rangeval = ageRng, nbasis = nbasis, 
                              norder = norder, breaks = age)

# We construct the functional parameter with penalization of the third 
# derivative

Lfdobj <- 3          
lambda <- 10^(-0.5)  
cvecf <- matrix(0, nbasis, ncasef) # this is used as initial value 
                                   # for the numerical techniques
Wfd0 <- fd(coef = cvecf, basisobj = wbasis)
growfdPar <- fdPar(fdobj = Wfd0, Lfdobj = Lfdobj, lambda = lambda)

# We carry out a monotone smoothing 
help(smooth.monotone)

growthMon <- smooth.monotone(argvals = age, y = hgtf, WfdParobj = growfdPar)


Wfd <- growthMon$Wfd
betaf <- growthMon$beta
hgtfhatfd <- growthMon$yhatfd

velocfdUN <- deriv.fd(expr = hgtfhatfd, Lfdobj = 1)
velocmeanfdUN <- mean.fd(velocfdUN)

accelfdUN <- deriv.fd(expr = hgtfhatfd, Lfdobj = 2)
accelmeanfdUN <- mean.fd(accelfdUN)


par(mfrow=c(2,2),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)
plot(hgtfhatfd, xlim=c(1,18), lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Growth (cm)")
plot(velocfdUN, xlim=c(1,18),  lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Velocity (cm/yr)")
plot(accelfdUN, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=2,
     cex=2, xlab="Age", ylab="Acceleration (cm/yr/yr)")

plot(wbasis)

#######################################################################


######
# Extension to multidimensional curves
######


noisycurve3D <- read.table("noisycurve3D.txt",header=T)
Xobs0 <- noisycurve3D$X0
Yobs0 <- noisycurve3D$Y0
Zobs0 <- noisycurve3D$Z0
obs0 <- rbind(Xobs0,Yobs0,Zobs0)
abscissa <- noisycurve3D$Abscissa
NT <- length(abscissa)

truecurve3D <- read.table("truecurve3D.txt",header=T)
Xtrue0 <- truecurve3D$X0
Ytrue0 <- truecurve3D$Y0
Ztrue0 <- truecurve3D$Z0
true0 <- rbind(Xtrue0,Ytrue0,Ztrue0)


library(rgl)

open3d()
lines3d(t(true0[1,]),t(true0[2,]),t(true0[3,]),xlab="",ylab="",zlab="",size=3,axes=F)
points3d(t(obs0[1,]),t(obs0[2,]),t(obs0[3,]),xlab="",ylab="",zlab="",size=2,axes=F,pch=19,cex=2)
box3d()


# compute the difference quotient
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincY1 <- (Yobs0[3:NT]-Yobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
rappincZ1 <- (Zobs0[3:NT]-Zobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])

rappincX2 <- ((Xobs0[3:NT]-Xobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Xobs0[2:(NT-1)]-Xobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])
rappincY2 <- ((Yobs0[3:NT]-Yobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Yobs0[2:(NT-1)]-Yobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])
rappincZ2 <- ((Zobs0[3:NT]-Zobs0[2:(NT-1)])/(abscissa[3:NT]-abscissa[2:(NT-1)])-(Zobs0[2:(NT-1)]-Zobs0[1:(NT-2)])/(abscissa[2:(NT-1)]-abscissa[1:(NT-2)]))*2/(abscissa[3:(NT)]-abscissa[1:(NT-2)])




par(mfrow=c(3,3),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)

plot(abscissa,obs0[1,],xlab=expression(tilde(s)),ylab="observed data x",cex=0.1,asp=1)
plot(abscissa,obs0[2,],xlab=expression(tilde(s)),ylab="observed data y",cex=0.1,asp=1)
plot(abscissa,obs0[3,],xlab=expression(tilde(s)),ylab="observed data z",cex=0.1,asp=1)
plot(abscissa[2:(NT-1)],rappincX1,xlab=expression(tilde(s)),ylab="first differences x",type="l",asp=1)
plot(abscissa[2:(NT-1)],rappincY1,xlab=expression(tilde(s)),ylab="first differences y",type="l",asp=1)
plot(abscissa[2:(NT-1)],rappincZ1,xlab=expression(tilde(s)),ylab="first differences z",type="l",asp=1)
plot(abscissa[2:(NT-1)],rappincX2,xlab=expression(tilde(s)),ylab="second differences x",type="l")
plot(abscissa[2:(NT-1)],rappincY2,xlab=expression(tilde(s)),ylab="second differences y",type="l")
plot(abscissa[2:(NT-1)],rappincZ2,xlab=expression(tilde(s)),ylab="second differences z",type="l")




bw <- 0.05

Xsm0 <- locpoly(abscissa,Xobs0,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm0 <- Xsm0$y

Xsm1 <- locpoly(abscissa,Xobs0,drv=1,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm1 <- Xsm1$y

Xsm2 <- locpoly(abscissa,Xobs0,drv=2,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Xsm2 <- Xsm2$y


Ysm0 <- locpoly(abscissa,Yobs0,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Ysm0 <- Ysm0$y

Ysm1 <- locpoly(abscissa,Yobs0,drv=1,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Ysm1 <- Ysm1$y

Ysm2 <- locpoly(abscissa,Yobs0,drv=2,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Ysm2 <- Ysm2$y


Zsm0 <- locpoly(abscissa,Zobs0,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Zsm0 <- Zsm0$y

Zsm1 <- locpoly(abscissa,Zobs0,drv=1,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Zsm1 <- Zsm1$y

Zsm2 <- locpoly(abscissa,Zobs0,drv=2,degree=degree,bandwidth=bw,gridsize=length(abscissa), range.x=range(abscissa))
Zsm2 <- Zsm2$y



par(mfrow=c(3,3),mar=c(6,5,2,1),mex=0.6, mgp=c(2.2,0.7,0),pty="m", font.main=1,font.lab=1, font.axis=1,cex.lab=1.3,cex.axis=1)

plot(abscissa,obs0[1,],xlab="s",ylab="x",cex=0.1,asp=1,xlim=c(0,1))
points(abscissa,Xsm0,type="l",col="blue",lwd=2)
plot(abscissa,obs0[2,],xlab="s",ylab="y",cex=0.1,asp=1,xlim=c(0,1))
points(abscissa,Ysm0,type="l",col="blue",lwd=2)
plot(abscissa,obs0[3,],xlab="s",ylab="z",cex=0.1,asp=1,xlim=c(0,1))
points(abscissa,Zsm0,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX1,xlab="s",ylab="x'",type="l",ylim=c(-0.5,0.5),xlim=c(0,1))
points(abscissa,Xsm1,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincY1,xlab="s",ylab="y'",type="l",ylim=c(-0.5,0.5),xlim=c(0,1))
points(abscissa,Ysm1,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincZ1,xlab="s",ylab="z'",type="l",ylim=c(-0.5,0.5),xlim=c(0,1))
points(abscissa,Zsm1,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincX2,xlab="s",ylab="x''",type="l")
points(abscissa,Xsm2,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincY2,xlab="s",ylab="y''",type="l")
points(abscissa,Ysm2,type="l",col="blue",lwd=2)
plot(abscissa[2:(NT-1)],rappincZ2,xlab="s",ylab="z''",type="l")
points(abscissa,Zsm2,type="l",col="blue",lwd=2)


open3d()
lines3d(t(true0[1,]),t(true0[2,]),t(true0[3,]),xlab="",ylab="",zlab="",size=3,axes=F)
points3d(t(obs0[1,]),t(obs0[2,]),t(obs0[3,]),size=2,pch=19,cex=2)
lines3d(t(Xsm0),t(Ysm0),t(Zsm0),size=3,col="blue")
box3d()


# or we may use another among the techniques seen above


#### Problem 4 - 18/06/2021 ####
#' The file power.txt reports the measurements of the electric power consumption 
#' in one household collected every day for one year. Considering a functional data 
#' analysis approach, answer to the following questions.
#' a) Perform a smoothing of the data using a Fourier basis. Choose the number of 
#'    basis functions using a generalized cross-validation (GCV) criterion. 
#'    Report the plot of the values of the GCV statistic as a function of the number 
#'    of basis functions, the number of basis functions chosen, a plot of the basis 
#'    system used and a plot of the smoothed data.
#' b) Compute an approximation of the first derivative of the curve from the data 
#'    and the first derivative of the smoothed curve obtained at point (a). 
#'    Provide a plot to compare the two and comment on the result.
#' c) Choose a number of basis functions that you deem appropriate to show the effect 
#'    of oversmoothing. Report the number of basis functions chosen, provide a plot 
#'    of the the smoothed data and comment the result.
#' d) Choose a number of basis functions that you deem appropriate to show the effect 
#'    of overfitting. Report the number of basis functions chosen, provide a plot of 
#'    the the smoothed data and comment the result.
    
graphics.off()
rm(list=ls())

library(fda)

data <- read.table('power.txt', header=T)
dim(data)
head(data)

NT <- dim(data)[1]
abscissa <- 1:365
Xobs0 <- data$power

plot(abscissa,Xobs0, type = "l")

## point a)

# generalized cross-validation
nbasis <- 6:50
gcv <- numeric(length(nbasis))
for (i in 1:length(nbasis)){
  basis <- create.fourier.basis(range(abscissa), nbasis[i])
  gcv[i] <- smooth.basis(abscissa, Xobs0, basis)$gcv
}
par(mfrow=c(1,1))
plot(nbasis,gcv)
nbasis[which.min(gcv)]
abline(v=nbasis[which.min(gcv)],col='red')

basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis[which.min(gcv)])
plot(basis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)


## point b)

# compute the central finite differences
rappincX1 <- (Xobs0[3:NT]-Xobs0[1:(NT-2)])/(abscissa[3:NT]-abscissa[1:(NT-2)])
Xsp1bis <- eval.fd(abscissa, Xsp$fd, Lfd=1) # first derivative

plot(abscissa[2:(NT-1)],rappincX1,xlab="t",ylab="first differences x",type="l")
points(abscissa,Xsp1bis,type='l',col="orange",lwd=3)


## point c)

# oversmoothing
nbasis <- 5
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)


## point d)

# overfitting
nbasis <- 50
basis <- create.fourier.basis(rangeval=range(abscissa), nbasis=nbasis)

Xsp <- smooth.basis(argvals=abscissa, y=Xobs0, fdParobj=basis)
Xsp0bis <- eval.fd(abscissa, Xsp$fd) #  the curve smoothing the data

plot(abscissa,Xobs0,xlab="t",ylab="observed data")
points(abscissa,Xsp0bis ,type="l",col="red",lwd=2)

