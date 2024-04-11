rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(fda)

######################### Data Processing #############################

# Load the data from .csv file
df <- read.csv("csv/2023-01-01_to_2023-12-31.csv")

df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df$Data <- as.Date(df$Data)
df$ZonaMercato <- factor(df$ZonaMercato)

######################### Smoothing - Prezzo #############################

day <- c("2023-01-02")
hours <- c("1")
zona_mercato <- c("CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;")

# dataframe to modify
df_plot <- df[df$Data %in% day & df$Ora %in% hours, ]

df_plot$cum_sum_quantita <- cumsum(df_plot$Quantita)
df_plot$cum_sum_quantita <- as.numeric(df_plot$cum_sum_quantita)

########################## loess approch #############################

# Create a smooth line for the Prezzo values
fit <- loess(df_plot$Prezzo ~ df_plot$cum_sum_quantita)

t <- paste("Loess: ", day, " H:", hours)

# curve complete
plot(df_plot$cum_sum_quantita, df_plot$Prezzo, lwd = 2, col = "red", xlab = "Observation Number", ylab = "Prezzo", main = t)
lines(df_plot$cum_sum_quantita, df_plot$PrezzoZonale)
yl <- predict(fit, newdata = data.frame(x = df_plot$cum_sum_quantita))
lines(df_plot$cum_sum_quantita, yl, col = "blue", lwd = 2)

# zoom on PrezzoZonale hotspot

df_plot_hotspot <- subset(df_plot, cum_sum_quantita >= 75000 & cum_sum_quantita <= 100000)

fit <- loess(Prezzo ~ cum_sum_quantita, data = df_plot_hotspot)

# Ensure the newdata data frame matches the structure of the original data
newdata <- data.frame(cum_sum_quantita = df_plot_hotspot$cum_sum_quantita)

# Generate predictions
yl <- predict(fit, newdata = newdata)

# Now, plotting should work without errors
plot(df_plot_hotspot$cum_sum_quantita, df_plot_hotspot$Prezzo, lwd = 2, col = "red", xlab = "Observation Number", ylab = "Prezzo", main = "Your Title Here", xlim = c(75000, 100000), ylim = c(0, 500))
lines(df_plot_hotspot$cum_sum_quantita, df_plot_hotspot$PrezzoZonale)
lines(df_plot_hotspot$cum_sum_quantita, yl, col = "blue", lwd = 2)

########################## bezier Curve approch #############################

# Bezier curve - 
bezier_curve <- function(x, y, n = 10) {
  outx <- NULL
  outy <- NULL

  i <- 1
  for (t in seq(0, 1, length.out = n)) {
    b <- bez(x, y, t)
    outx[i] <- b$x
    outy[i] <- b$y
    i <- i + 1
  }

  return(list(x = outx, y = outy))
}

bez <- function(x, y, t) {
  outx <- 0
  outy <- 0
  n <- length(x) - 1
  for (i in 0:n) {
    outx <- outx + choose(n, i) * ((1 - t)^(n - i)) * t^i * x[i + 1]
    outy <- outy + choose(n, i) * ((1 - t)^(n - i)) * t^i * y[i + 1]
  }

  return(list(x = outx, y = outy))
}

t <- paste(day, " H:", hours)

# curve complete
plot(df_plot$cum_sum_quantita, df_plot$Prezzo, lwd = 2, col = "red", xlab = "Observation Number", ylab = "Prezzo", main = t)
lines(df_plot$cum_sum_quantita, df_plot$PrezzoZonale)
smoothed_curve <- bezier_curve(df_plot$cum_sum_quantita, df_plot$Prezzo, 150)
points(smoothed_curve$x, smoothed_curve$y, type = "l", col = "blue")

# zoom on prezzoZonale hotspot 
plot(df_plot$cum_sum_quantita, df_plot$Prezzo, lwd = 2, col = "red", xlab = "Observation Number", ylab = "Prezzo", main = t, xlim = c(75000, 100000), ylim = c(0, 500))
lines(df_plot$cum_sum_quantita, df_plot$PrezzoZonale)
points(smoothed_curve$x, smoothed_curve$y, type = "l", col = "blue")

########################## Spline approch #############################

knots <- c(seq(0, length(df_plot$cum_sum_quantita), 5)) #Location of knots
n_knots <- length(knots) #Number of knots
n_order <- 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis <- length(knots) + n_order - 2;
basis <- create.bspline.basis(rangeval = c(min(df_plot$cum_sum_quantita), max(df_plot$cum_sum_quantita)), n_basis)

# compute the fitted values
result <- smooth.basis(df_plot$cum_sum_quantita, df_plot$Prezzo, basis)
prezzo_hat <- eval.fd(df_plot$cum_sum_quantita, result$fd, Lfd = 0)

# plot the curve
t <- paste("Spline: ", day, " H:", hours)
plot(x, df_plot$Prezzo, lwd = 2, col = "red", xlab = "Observation Number", ylab = "Prezzo", main = t)
lines(df_plot$cum_sum_quantita, df_plot$Prezzo, lwd = 2, col = "green")
points(x, prezzo_hat, type = "l", col = "blue")
lines(df_plot$cum_sum_quantita, df_plot$PrezzoZonale)

rm(df_plot, prezzo_hat, result, basis, n_order, n_basis, n_knots, knots, t, x)

######################### Smoothing - Prezzo Zonale #################################

# Molto lento. Oltre al parallelo, altre soluzioni?
df_smoothing <- df
df_smoothing <- subset(df_smoothing, select = -c(Quantita, Prezzo))

# Keep only unique rows based on ID and Group
df_smoothing <- df_smoothing[!duplicated(df_smoothing[, c("Data", "Ora")]), ]

# Omit rows with missing values
df_smoothing <- na.omit(df_smoothing)

# Save Date and Hour in a unique column with appropriate format for algebric computations
df_smoothing$DateTime <- paste(df_smoothing$Data, df_smoothing$Ora, sep=" ")
df_smoothing <- subset(df_smoothing, select = -c(Data, Ora))
df_smoothing$DateTime <- as.numeric(as.POSIXct(df_smoothing$DateTime, format = "%Y-%m-%d %H", tz = "CET"))

# Omit rows with missing values
df_smoothing <- na.omit(df_smoothing)

# Save abscissa and target variable
x <- df_smoothing$DateTime
Y <- df_smoothing$PrezzoZonale

# Order the abscissa to avoid problems with plotting
ordered_indices <- order(x)
x <- x[ordered_indices]
Y <- Y[ordered_indices]


# Frequencies
table(x)

# Looking at the dataframe we should have equidistant time steps, so it makes sense
# to consider equispaced knots


# Compute numerical derivatives with central differences
n = length(x)
rappincX1 <- (Y[3:n] - Y[1:(n - 2)]) / (x[3:n] - x[1:(n - 2)])
rappincX2 <- ((Y[3:n] - Y[2:(n - 1)]) / (x[3:n] - x[2:(n - 1)]) - (Y[2:(n - 1)] - Y[1:(n - 2)]) / (x[2 : (n - 1)] - x[1:(n - 2)])) * 2 / (x[3 : (n)] - x[1 : (n - 2)])

##### First approach: dimensionality reduction ----

# Parameters
n_points <- n
basisOrder <- 4


# Choose the best value of numbasis by minimizing the GeneralizedCrossValidation
numbasis <- seq(from = 2000, to = n_points/2, by = 1000) # troppe basi rendono la matrice psi singolare. Se da problemi 
                                                         # diminuire estremo dx

GeneralizedCrossValidations <- numeric(length(numbasis))

for (i in 1:length(numbasis)){
  basis <- create.bspline.basis(rangeval=c(min(x),max(x)), nbasis=numbasis[i], norder=basisOrder)
  GeneralizedCrossValidations[i] <- smooth.basis(x, Y, basis)$gcv
} # farlo andare in parallelo?

bestNumBasis=numbasis[[which.min(GeneralizedCrossValidations)]]
# around 2000. If left like that it is 2010
bestBasis <- create.bspline.basis(rangeval=c(min(x),max(x)), nbasis=bestNumBasis, norder=basisOrder)
result <- smooth.basis(x, Y, bestBasis)

# RMK. I have used bspline basis and not fourier basis since periodicity 
# is not all that strong. Otherwise use fourier basis

# Compute the fitted values and the fitted first and second order derivatives
Y_hat <- eval.fd(x, result$fd, Lfd=0)
diff1Y_hat <- eval.fd(x, result$fd, Lfd=1)
diff2Y_hat <- eval.fd(x, result$fd, Lfd=2)

# Plot the curves 
x11()
par(mfrow=c(1,3))
#title(main = "Space dimension reduction smoothing curve")

plot(x, Y, col="lightgreen", type="l", xlab="Time", ylab="Prices")
points(x, Y_hat, type="l", col="blue")
lines(x, Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Obserbed data", "Fitted curve"), col=c("grey", "blue"), lty=c(1, 1), lwd=c(1, 2))

plot(x[2:(n-1)],rappincX1,col="lightgreen", type="l", xlab="Time",ylab="Prices first differences")
points(x,diff1Y_hat ,type="l",col="blue")
lines(x, diff1Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Data numerical first derivative", "Fitted curve first derivative"), col=c("red", "blue"), lty=c(1, 1), lwd=c(1, 2))

plot(x[2:(n-1)],rappincX2, col="lightgreen", type="l", xlab="Time",ylab="Prices second differences")
points(x,diff2Y_hat ,type="l",col="blue")
lines(x, diff2Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Data numerical second derivative", "Fitted curve second derivative"), col=c("red", "blue"), lty=c(1, 1), lwd=c(1, 2))


##### One at a time confidence intervals ----

# (li ho fatti ma si possono anche togliere, non aggiungono troppo)

# compute manually the quantities of interest 
Psi <- eval.basis(x, bestBasis)
c_hat = lsfit(Psi, Y, intercept=FALSE)$coeff
S=Psi%*%solve(t(Psi)%*%Psi)%*%t(Psi)
Y_hat=Psi%*%c_hat

# Compute level alpha confidence intervals for each abscissa (one at a time NOT simultaneous)
alpha=0.01
quantile=qnorm(1-alpha/2)
sigma_hat=(1/(n_points-sum(diag(S)))*t(Y-Y_hat)%*%(Y-Y_hat))[1,1]
CI= cbind(Y_hat-quantile*sqrt(diag(S)), Y_hat, Y_hat+quantile*sqrt(diag(S)))


# Plot the Confidence Intervals
x11()
par(mfrow=c(1,1))
#title(main = "One at a time confidence intervals of space dimension reduction smoothing curve")

plot(x, Y, col="lightgreen", type="l", xlab="Hours", ylab="Prices")
points(x, Y_hat, type="l", col="blue")
lines(x, Y_hat, col="blue", lwd=3)
points(x, CI[,2],type="l",col="red",lty="dashed")
points(x,CI[,3],type="l",col="red",lty="dashed")
legend("topright", legend=c("Obserbed data", "Fitted curve", "C.I."), col=c("lightgreen", "blue", "red"), lty=c(1, 1), lwd=c(1, 2))


##### Second approach: smoothing with penalization ----

# Parameters
PenalizingOrder <- 2  
basisOrder=PenalizingOrder+2

# Create a basis starting with number of knots found before
# (too computationally expensive a knot for each abscissa)

# Problema: quante basi usare? Se sono troppe il programma è davvero troppo lento.
# Ho solo settato a 100 per far runnare il codice i tempi decenti, tuttavia 
# la curva delle derivate e molto piatta a causa di ciò

basis<-create.bspline.basis(rangeval=c(min(x),max(x)),nbasis=100, norder=basisOrder)

# Choose the best value of lambda by minimizing the GeneralizedCrossValidation
lambdas <- 10^seq(-12, 5, by=0.5)
GeneralizedCrossValidations<-numeric(length(lambdas))
for (i in 1:length(lambdas)){
  fdParObj <- fdPar(fdobj=basis, Lfdobj=PenalizingOrder, lambda=lambdas[i]) 
  GeneralizedCrossValidations[i] <- smooth.basis(x, Y, fdParObj)$gcv
}

fdParObjFinal <-fdPar(fdobj=basis, Lfdobj=PenalizingOrder, lambda=lambdas[which.min(GeneralizedCrossValidations)])  
result <- smooth.basis(x, Y, fdParObjFinal)

# Compute the fitted values and the fitted first and second order derivatives
Y_hat <- eval.fd(x, result$fd, Lfd=0)
diff1Y_hat <- eval.fd(x, result$fd, Lfd=1)
diff2Y_hat <- eval.fd(x, result$fd, Lfd=2)

# Plot the curves
x11()
par(mfrow=c(1,3))

#title(main = "Smoothing curve with penalization")

plot(x, Y, col="lightgreen", type="l", xlab="Time", ylab="Prices")
points(x, Y_hat, type="l", col="blue")
lines(x, Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Obserbed data", "Fitted curve"), col=c("grey", "blue"), lty=c(1, 1), lwd=c(1, 2))

plot(x[2:(n-1)],rappincX1,col="lightgreen", type="l", xlab="Time",ylab="Prices first differences")
points(x,diff1Y_hat ,type="l",col="blue")
lines(x, diff1Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Data numerical first derivative", "Fitted curve first derivative"), col=c("red", "blue"), lty=c(1, 1), lwd=c(1, 2))

plot(x[2:(n-1)],rappincX2, col="lightgreen", type="l", xlab="Time",ylab="Prices second differences")
points(x,diff2Y_hat ,type="l",col="blue")
lines(x, diff2Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Data numerical second derivative", "Fitted curve second derivative"), col=c("red", "blue"), lty=c(1, 1), lwd=c(1, 2))

# RMK. Le seguenti osservazioni le ho notate prendendo un numero di dati moolto inferiore)
#      (circa un centinaio) 

# RMK. Looking at the graphs we see that with this second approach (i.e.penalizing
# the second derivative) we have a much lower variance and much higher bias
# compared to the first. 

# Idea: the higher the derivative order to penalize, the higher the variance, the 
# less the bias. What is the best combination?
# Choose the derivative order to penalize that minimizes the MSE

#P <- eval.penalty(basis, Lfdobj=fdParObjFinal$Lfdobj, rng=fdParObjFinal$rangeval)

penalizing_orders <- 2:15 
mse_values <- numeric(length(penalizing_orders))
lambdabest <- numeric(length(penalizing_orders))

for (i in seq_along(penalizing_orders)) {
  PenalizingOrder <- penalizing_orders[i]
  basisOrder <- PenalizingOrder + 2
  
  basis <- create.bspline.basis(rangeval=c(min(x),max(x)), norder=basisOrder)
  
  # Choose the best value of lambda by minimizing the GeneralizedCrossValidation
  lambdas <- 10^seq(-12, 5, by=0.5)
  GeneralizedCrossValidations <- numeric(length(lambdas))
  for (j in 1:length(lambdas)){
    fdParObj <- fdPar(fdobj=basis, Lfdobj=PenalizingOrder, lambda=lambdas[j]) 
    GeneralizedCrossValidations[j] <- smooth.basis(x, Y, fdParObj)$gcv
  }
  
  lambdabest[i] <- lambdas[which.min(GeneralizedCrossValidations)]
  fdParObjFinal <- fdPar(fdobj=basis, Lfdobj=PenalizingOrder, lambda=lambdabest[i])  
  result <- smooth.basis(x, Y, fdParObjFinal)
  
  # Compute the fitted values
  Y_hat <- eval.fd(x, result$fd, Lfd=0)
  
  # Compute the MSE for the current PenalizingOrder
  residuals <- Y - Y_hat
  squared_residuals <- residuals^2
  mse_values[i] <- mean(squared_residuals)
  
}

# Find the PenalizingOrder that minimizes the MSE
optimal_penalizing_order <- penalizing_orders[which.min(mse_values)]
optimal_penalizing_order
optimal_lambda=lambdabest[which.min(mse_values)]
optimal_basis_order=optimal_penalizing_order+2

optimal_basis <- create.bspline.basis(rangeval=c(min(x),max(x)), norder=optimal_basis_order)

optimal_fdParObj <- fdPar(fdobj=optimal_basis, Lfdobj=optimal_penalizing_order, lambda=optimal_lambda) 
optimal_result <- smooth.basis(x, Y, optimal_fdParObj)

Y_hat <- eval.fd(x, optimal_result$fd, Lfd=0)
diff1Y_hat <- eval.fd(x, optimal_result$fd, Lfd=1)
diff2Y_hat <- eval.fd(x, optimal_result$fd, Lfd=2)

# Plot the curves
x11()
par(mfrow=c(1,3))

#title(main = "Smoothing curve with best penalization order")

plot(x, Y, col="lightgreen", type="l", xlab="Time", ylab="Prices")
points(x, Y_hat, type="l", col="blue")
lines(x, Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Obserbed data", "Fitted curve"), col=c("grey", "blue"), lty=c(1, 1), lwd=c(1, 2))

plot(x[2:(n-1)],rappincX1,col="lightgreen", type="l", xlab="Time",ylab="Prices first differences")
points(x,diff1Y_hat ,type="l",col="blue")
lines(x, diff1Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Data numerical first derivative", "Fitted curve first derivative"), col=c("red", "blue"), lty=c(1, 1), lwd=c(1, 2))

plot(x[2:(n-1)],rappincX2, col="lightgreen", type="l", xlab="Time",ylab="Prices second differences")
points(x,diff2Y_hat ,type="l",col="blue")
lines(x, diff2Y_hat, col="blue", lwd=3)
legend("topright", legend=c("Data numerical second derivative", "Fitted curve second derivative"), col=c("red", "blue"), lty=c(1, 1), lwd=c(1, 2))

# RMK. From first approach it seems that by increasing the penalty order
# the less the MSE. Something wrong? Try with another parameter like AIC?


# NOTE. It is not necessary to build the curve imposing positive constraint.
# If needed, code below (I started wiring it but then realized it was not worth it)
# --> Per me si può togliere


numbasis=12
PenalizingOrder <- 2  
basisOrder=PenalizingOrder+2

basis<-create.bspline.basis(rangeval=c(1,24), nbasis=numbasis, norder=basisOrder)


lambdas <- 10^seq(-12, 5, by=0.5)
GeneralizedCrossValidations<-numeric(length(lambdas))
for (i in 1:length(lambdas)){
  fdParObj <- fdPar(fdobj=basis, Lfdobj=PenalizingOrder, lambda=lambdas[i]) 
  GeneralizedCrossValidations[i] <- smooth.basis(x, Y, fdParObj)$gcv
}
nobs=length(Y)

Psi <- eval.basis(x, basis)

fdObj <- fd(coef=matrix(0, basis$nbasis, nobs), basisobj=basis)

fdParObj <- fdPar(fdobj=fdObj, Lfdobj=PenalizingOrder, lambda=lambda) 

Y_hat<-smooth.pos(as.numeric(x), Y, fdParObj)

Y_hat <- eval.fd(x, result$fd, Lfd=0)
diff1Y_hat <- eval.fd(x, result$fd, Lfd=1)
diff2Y_hat <- eval.fd(x, result$fd, Lfd=2)

attributes(Y_hat)
Y_hat$y
Y_hat$posfd
Y_hat$argvals
Y_hat$Wfdobj
Y_hat$Flist

posvals = eval.posfd(as.numeric(x),Y_hat)
plot(posvals, ylim=c(-1,7))
abline(h=0,lty = 2, col=4)

rm(basis, basismat, basismat1, basismat2, bestBasis, CI, df, df_boxplot, 
   df_smoothing, diff1Y_hat, diff2Y_hat, fdObj, fdParObj, fdParObjFinal,
   MatrixBasis, means_vector_hour, means_vector_month)