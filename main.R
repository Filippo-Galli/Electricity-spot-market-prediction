rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)
library(fda)
library(tidyr)
library(stats)
library(fdasrvf)
library(forecast)
library(lubridate)
library(data.table)

######################### DataFrame Building #############################

# Load the data from .csv file
df <- read.csv("csv/MGP_2023.csv")
df <- na.omit(df) # remove rows with NA's

# set proper type to column
df$Data <- as.Date(as.character(df$Data), format='%Y%m%d')
df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df$ZonaMercato <- factor(df$ZonaMercato)
df$Tipo <- factor(df$Tipo)

# separate OFF and BID df

df.off <- df[which(df$Tipo=='OFF'),1:6]
df.bid <- df[which(df$Tipo=='BID'),1:6]

######################### ZonaMercato inspection #########################

this.df <- df.off

sort(table(this.df$ZonaMercato), decreasing=T)

zone.full <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;" # full pool zone

# some miss
zone.no.cors <- "CALA;CNOR;CSUD;NORD;SARD;SICI;SUD;AUST;COAC;FRAN;GREC;SLOV;SVIZ;MALT;COUP;MONT;"
zone.no.sici.malt.cala <- c("CALA;CNOR;CSUD;NORD;SARD;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;COUP;MONT;","CNOR;CSUD;NORD;SARD;SUD;AUST;COAC;CORS;FRAN;GREC;SLOV;SVIZ;COUP;MONT;")

# full split in half NORD-SUD
zone.nord <- "CNOR;NORD;AUST;CORS;FRAN;SLOV;SVIZ;COUP;"
zone.sud <- "CALA;CSUD;SARD;SICI;SUD;COAC;GREC;MALT;MONT;"

count.ZonaMercato <- count(this.df %>% group_by(ZonaMercato))

zone.category <- case_when(
  count.ZonaMercato$ZonaMercato %in% c(zone.nord,zone.sud) ~ "NORD+SUD",
  count.ZonaMercato$ZonaMercato %in% zone.full ~ "FULL",
  count.ZonaMercato$ZonaMercato %in% zone.no.cors ~ "NOCORS",
  count.ZonaMercato$ZonaMercato %in% zone.no.sici.malt.cala ~ "NOSICIMALTCALA",
  TRUE ~ "OTHER"
)

ggplot(count.ZonaMercato, aes(x = ZonaMercato, y = n, fill=zone.category)) +
  geom_bar(stat="identity",color="black") +
  scale_fill_manual(name="ZonaMercato", values=c("FULL"="green", "NORD+SUD"= "forestgreen", "NOCORS" = "red", "NOSICIMALTCALA"='yellow', "OTHER"="skyblue"), labels=c('Full Pool','No CORS','Split NORD-SUD','No SICI;MALT;CALA','Other')) +
  theme(axis.text.x = element_blank(), legend.position = "top") +
  labs(title = "ZonaMercato", x = "Zones", y = "Count")

zone <- c(zone.full, zone.nord, zone.sud, zone.no.cors, zone.no.sici.malt.cala)

######################### Data loss ######################################

this.df <- df.off

# group by selected zone and count null values (should be zero for all)
null.prices <- this.df %>%
  filter(ZonaMercato %in% zone) %>%
  group_by(Data, Ora, ZonaMercato) %>%
  summarize(num.nulls = sum(is.na(Prezzo)))

# count for each day&hour the number of entries in which the zone is in the selected zone
zone.day.hour <- this.df %>%
  group_by(Data, Ora) %>%
  summarize(n = sum(ZonaMercato %in% zone))

# select when no specific_zone is present
missing.zone.day.hour <- zone.day.hour %>% filter (n == 0)

# count missing hour per Date
missing.hour <- missing.zone.day.hour %>% count( Data , name = "Missing Hour")

# Convert the 'Data' column to Date type
missing.hour$Data <- as.Date(missing.hour$Data)

# Extract the month from the 'Data' column
missing.hour$Month <- format(missing.hour$Data, "%m")

# Define colors for each month
month_colors <- c(
  "01" = "red",
  "02" = "blue",
  "03" = "green",
  "04" = "orange",
  "05" = "purple",
  "06" = "cyan",
  "07" = "magenta",
  "08" = "yellow",
  "09" = "gray",
  "10" = "darkgreen",
  "11" = "darkblue",
  "12" = "darkred"
)

# Plot the barplot with legend
ggplot(missing.hour, aes(x = Data, y = `Missing Hour`, fill = Month)) +
  geom_bar(stat = "identity") +
  labs(x = " ", y = "Missing Hour", title = " Missing Hour per Day",
       fill = "Month") +
  scale_fill_manual(values = month_colors, 
                    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

mean(missing.hour$`Missing Hour`)

# from now on we just one 'zone', so can get rid of column

df <- df[which(df$ZonaMercato %in% zone),-3]
df.off <- df.off[which(df.off$ZonaMercato %in% zone),-3]
df.bid <- df.bid[which(df.bid$ZonaMercato %in% zone),-3]

############################## Boxplot Prezzo:Ora ##############################

this.df <- df.off

# Calculate median values of Prezzo for each Ora
medians <- this.df %>%
  group_by(Ora) %>%
  summarise(median_Prezzo = median(Prezzo)) %>%
  mutate(color_value = rank(median_Prezzo, ties.method = "first")) # Rank based on median

# Join the median values back to the original data frame
df_boxplot <- this.df %>%
  left_join(medians, by = "Ora")

# Create the boxplot with enhanced color differentiation
ggplot(df_boxplot, aes(x = Ora, y = Prezzo, fill = color_value)) + 
  geom_boxplot() + 
  #geom_text(data = medians, aes(x = Ora, y = median_Prezzo, label = round(median_Prezzo, 2)), 
  #         size = 3, vjust = -1.5) +
  ggtitle("Boxplot of Prezzo by Ora - Year 23") + 
  labs(x = "Ora", y = "Prezzo") +
  coord_cartesian(ylim = c(0, 500)) +
  scale_fill_gradient(low = "green", high = "red", limits = c(min(medians$color_value), max(medians$color_value)),
                      breaks = c(min(medians$color_value), max(medians$color_value)),
                      labels = c(min(medians$median_Prezzo), max(medians$median_Prezzo)),
                      guide = guide_colorbar(title = "median_Prezzo")) + # Modifica il titolo della legenda# Green to red gradient
  theme_minimal()

############################## Boxplot Prezzo:Month ##############################

this.df <- df.off
this.df$Month <- as.factor(month(this.df$Data))

# Calculate median values of Prezzo for each Ora
medians <- this.df %>%
  group_by(Month) %>%
  summarise(median_Prezzo = median(Prezzo)) %>%
  mutate(color_value = rank(median_Prezzo, ties.method = "first")) # Rank based on median

# Join the median values back to the original data frame
df_boxplot <- this.df %>%
  left_join(medians, by = "Month")

# Create the boxplot with enhanced color differentiation
ggplot(df_boxplot, aes(x = Month, y = Prezzo, fill = color_value)) + 
  geom_boxplot() + 
  #geom_text(data = medians, aes(x = Ora, y = median_Prezzo, label = round(median_Prezzo, 2)), 
  #         size = 3, vjust = -1.5) +
  ggtitle("Boxplot of Prezzo by Month - Year 23") + 
  labs(x = "Month", y = "Prezzo") +
  coord_cartesian(ylim = c(0, 500)) +
  scale_fill_gradient(low = "green", high = "red", limits = c(min(medians$color_value), max(medians$color_value)),
                      breaks = c(min(medians$color_value), max(medians$color_value)),
                      labels = c(min(medians$median_Prezzo), max(medians$median_Prezzo)),
                      guide = guide_colorbar(title = "median_Prezzo")) + # Modifica il titolo della legenda# Green to red gradient
  theme_minimal()

############################## Curves (STEP) #####################################

# Group by day and hour, then sort each group by prezzo
sorted.df.off <- df.off %>%
  group_by(Data, Ora) %>%
  arrange(Data, Ora, Prezzo)
sorted.df.bid <- df.bid %>%
  group_by(Data, Ora) %>%
  arrange(Data, Ora, desc(Prezzo))

# calculate cumulative sums
cumulative.df.off <- sorted.df.off %>%
  mutate(Quantita.sum = cumsum(Quantita))
cumulative.df.bid <- sorted.df.bid %>%
  mutate(Quantita.sum = cumsum(Quantita))

# data sequence
start.date <- as.Date("2023-01-01")
end.date <- as.Date("2023-12-31")
date.sequence <- seq.Date(start.date, end.date, by = "day")

# quantita, prezzo range to display
quantity.range <- c(10000, 50000)  
prezzo.range <- c(0, 400)

restricted.plot.off <- list()
restricted.plot.bid <- list()
for(i in 1:365){

  restricted.plot.off[[i]] <- cumulative.df.off %>%
  filter( Data == date.sequence[i] &
          Quantita.sum >= quantity.range[1] & Quantita.sum <= quantity.range[2] &
          Prezzo >= prezzo.range[1] & Prezzo <= prezzo.range[2])

  restricted.plot.bid[[i]] <- cumulative.df.bid %>%
    filter( Data == date.sequence[i] &
              Quantita.sum >= quantity.range[1] & Quantita.sum <= quantity.range[2] &
              Prezzo >= prezzo.range[1] & Prezzo <= prezzo.range[2])

  print(paste("Percentage: ", i*100/365, " %"))

}

# # Improved Version of above loop 
# filtered_off <- setDT(cumulative.df.off)[Data %in% date.sequence & 
#                                            Quantita.sum >= quantity.range[1] & 
#                                            Quantita.sum <= quantity.range[2] & 
#                                            Prezzo >= prezzo.range[1] & 
#                                            Prezzo <= prezzo.range[2]]
# 
# filtered_bid <- setDT(cumulative.df.bid)[Data %in% date.sequence & 
#                                            Quantita.sum >= quantity.range[1] & 
#                                            Quantita.sum <= quantity.range[2] & 
#                                            Prezzo >= prezzo.range[1] & 
#                                            Prezzo <= prezzo.range[2]]
# 
# # Initialize lists to store results
# restricted.plot.off <- list()
# restricted.plot.bid <- list()
# 
# # Iterate over dates
# for(i in 1:length(date.sequence)){
#   
#   # Extract data for the current date
#   off_data <- filtered_off[Data == date.sequence[i]]
#   bid_data <- filtered_bid[Data == date.sequence[i]]
#   
#   # Add to lists
#   restricted.plot.off[[i]] <- off_data
#   restricted.plot.bid[[i]] <- bid_data
#   
#   # Print progress
#   print(paste("Percentage: ", i*100/length(date.sequence), " %"))
# }

names(restricted.plot.off) <- date.sequence
names(restricted.plot.bid) <- date.sequence

day <- as.Date('2023-01-01')
nday <- which(date.sequence==day)
ggplot(restricted.plot.bid[[nday]], aes(x = Quantita.sum, y = Prezzo, color = as.factor(Ora))) +
  geom_step() +
  labs(title = paste("Cumulative Sum of Quantity vs. Prezzo for ", date.sequence[nday]),
       x = "Cumulative Quantity",
       y = "Prezzo",
       color = "Hour") +
  theme_minimal()

################### Functions ##########################

find_intersection <- function(x.synt.off,x.synt.bid,c.off, c.bid) {
  
  # temporary dataset to order data and prices - OFF
  temp.data <- data.frame(x = x.synt.off, y = c.off )
  temp.data <- temp.data[order(temp.data$x),]
  
  x.synt.off <- temp.data$x
  c.off <- temp.data$y
  
  # temporary dataset to order data and prices - BID
  temp.data <- data.frame(x = x.synt.bid, y = c.bid )
  temp.data <- temp.data[order(temp.data$x),]
  
  x.synt.bid <- temp.data$x
  c.bid <- temp.data$y
  
  
  
  intersections <- data.frame()
  
  fd.1 <- stepfun(x.synt.off, c(0,c.off))
  fd.2 <- stepfun(x.synt.bid, c(400,c.bid))
  
  all_breaks <- sort(unique(c(x.synt.off, x.synt.bid)))
  
  # Evaluate fd.off at x_values
  y_off <- fd.1(all_breaks)
  
  # Evaluate fd.bid at x_values
  y_bid <- fd.2(all_breaks)
  
  qta.inter <- all_breaks[which((y_bid-y_off)<0)][1]
  if(qta.inter %in% x.synt.off){
    prz.break <- fd.2(qta.inter)
  }
  if(qta.inter %in% x.synt.bid){
    prz.break <- fd.1(qta.inter)
  }
  return(data.frame(x=qta.inter,y=prz.break))
}

############################## Market Clearing Price #############################

day <- as.Date("2023-02-01")
nday <- which(date.sequence==day)
hour <- 1

mcp.df <- read.table("MarketCoupling.txt")
mcp.df <- mcp.df[,-5]
mcp.df <- mcp.df[which(as.Date(mcp.df$Data) %in% date.sequence),]

mcq.df <- mcp.df %>%
  group_by(Data, Ora) %>%
  summarise(delta = sum(FlussoImport - FlussoExport)) %>%
  ungroup()

delta.mcq <- mcq.df[mcq.df$Data==day & mcq.df$Ora == hour,3]

off <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)]
bid <- restricted.plot.bid[[nday]][which(restricted.plot.bid[[nday]]$Ora %in% hour),c(2,5,6)]
off.shift <- off
off.shift$Quantita.sum <- off.shift$Quantita.sum + rep(as.numeric(delta.mcq),nrow(off.shift))

off$Type <- 'Supply'
off.shift$Type <- 'SupplyS'
bid$Type <- 'Demand'

off.bid <- rbind(off,off.shift, bid)
off.bid$Type.Ora <- interaction(off.bid$Type, off.bid$Ora)

# plot of curves(off-bid) selected
plot <- ggplot(off.bid, aes(x = Quantita.sum, y = Prezzo, color = Type.Ora, linetype = Type)) +
  geom_step() +
  labs(title = paste("Cumulative Sum of Quantity vs. Prezzo for ", date.sequence[nday]),
       x = "Cumulative Quantity",
       y = "Prezzo",
       color = "Type") +
  theme_minimal() +
  scale_linetype_manual(values = c("Supply" = "solid", "Demand" = "solid", "SupplyS" = "dashed")) +
  scale_color_manual(values = c("Supply.1" = "red", "Demand.1" = "blue", "SupplyS.1" = "red")) +
  guides(linetype='none')

mcp <- find_intersection(off$Quantita.sum,bid$Quantita.sum,off$Prezzo,bid$Prezzo)
plot + geom_point(data = mcp, aes(x = x, y = y), color = "black", size = 3, inherit.aes = FALSE) +
  annotate('text',x=50000, y = 50, label = paste('Quantity:',as.character(mcp$x),'\n Prezzo:',as.character(mcp$y),sep = ' '))

################################### Smoothing ###################################################

nbasis <- list()
step.length <- 50
n <- length(seq(quantity.range[1],quantity.range[2],by = step.length))

for(nday in 1:365){
  
  day.list <- list()
  
  for(hour in 1:24){
    
    this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)]
    
    # original data
    abscissa <- this.df$Quantita.sum
    ordinate <- this.df$Prezzo
    
    if(length(abscissa) > 0 & length(ordinate) > 0){
      day.list[[as.character(hour)]] <- n # number of basis equal to number of considered points
      
    } else{
      day.list[[as.character(hour)]] <- NA
    }
    
    
  }
  
  nbasis[[nday]] <- day.list
  
}

# all hours of day

day <- as.Date("2023-02-01")
nday <- which(date.sequence==day)

plot(NA, NA, xlim = quantity.range, ylim = prezzo.range, xlab = "Quantita", ylab = "Prezzo", main = paste("Smoothed of",day,sep=" "))

df.to.use <- restricted.plot.bid[[nday]]

for(hour in 1:24){
  this.df <-df.to.use[which(df.to.use$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
  # original data
  abscissa <- this.df$Quantita.sum
  ordinate <- this.df$Prezzo
  
  if(length(abscissa) > 0 & length(ordinate) > 0){
  # step function -> syntetic point -> smoothing
  step.function <- stepfun(abscissa,c(ordinate[1],ordinate))
  x.synt <- seq(quantity.range[1],quantity.range[2],by = step.length)
  test.fPCA <- 
  y.synt <- step.function(x.synt)
  
  order <- 1
  basis <- create.bspline.basis(rangeval=quantity.range, nbasis=nbasis[[nday]][[hour]], norder=order)
  smoothed <- smooth.basis(argvals=x.synt, y=y.synt, fdParobj=basis)
  smooth.eval <- eval.fd(abscissa, smoothed$fd)
  points(abscissa,smooth.eval,type='s', col = hour)
  }
  
}

# plot all selected days of fixed hour

start.day <- as.Date("2023-01-01")
end.day<- as.Date("2023-01-31")
hour <- 1
ndays <- seq(which(date.sequence==start.day),which(date.sequence==end.day), by = 1)

matrix.smoothed.eval <- c()

plot(NA, NA, xlim = quantity.range, ylim = prezzo.range, xlab = "Quantita", ylab = "Prezzo", main = paste("Smoothed of h:",as.character(hour),sep=" "))

df.to.use <- restricted.plot.off

i <- 1
for(nday in ndays){
  this.df <- df.to.use[[nday]][which(df.to.use[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
  # original data
  abscissa <- this.df$Quantita.sum
  ordinate <- this.df$Prezzo
  
  if(length(abscissa) > 0 & length(ordinate) > 0){
    # step function -> syntetic point -> smoothing
    step.function <- stepfun(abscissa,c(ordinate[1],ordinate))
    x.synt <- seq(quantity.range[1],quantity.range[2],by = step.length)
    y.synt <- step.function(x.synt)
    
    order <- 1
    basis <- create.bspline.basis(rangeval=quantity.range, nbasis=nbasis[[nday]][[hour]], norder=order)
    smoothed <- smooth.basis(argvals=x.synt, y=y.synt, fdParobj=basis)
    smoothed.eval <- eval.fd(abscissa, smoothed$fd)
    points(abscissa,smoothed.eval,type='s', col = i)
    
    matrix.smoothed.eval <- cbind(matrix.smoothed.eval,eval.fd(x.synt, smoothed$fd))
  }
  
  i <- i+1
  
}

####################################### Remove Useless things#######################
rm(bid, count.ZonaMercato, cumulative.df.bid, cumulative.df.off, df_boxplot, df.bid, df.off)

rm(zone.full, zone.no.cors, zone.no.sici.malt.cala, zone.nord, zone.sud, zone.category, zone)

rm(matrix.smoothed.eval, zone.day.hour, this.df, sorted.df.bid, sorted.df.off)

rm(missing.zone.day.hour, missing.hour, month_colors, mcp.df, mcq.df, delta.mcq)

rm(medians, null.prices, off, off.bid, off.shift, plot)

####################################### fPCA #################################

# init of matrix of functions 
matrix.smoothed.eval.off <- c()
matrix.smoothed.eval.bid<- c()

# by days, fixed hour
start.day <- as.Date("2023-01-31")
end.day<- as.Date("2023-02-06")
hour <- 1
ndays <- seq(which(date.sequence==start.day),which(date.sequence==end.day), by = 1)

knots.bid <- unique(c(10000,seq(10000, 20000, length = 100), seq(20000, 30000, length = 600), seq(30000, 40000, length = 100), seq(40000, 50000, length = 100),50000 ))

for(nday in ndays){
  ## Offerta
  this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
  # original data
  abscissa <- this.df$Quantita.sum
  ordinate <- this.df$Prezzo
  
  if(length(abscissa) > 0 & length(ordinate) > 0){
    step.function <- stepfun(abscissa,c(ordinate[1],ordinate))
    x.synt.off <- seq(quantity.range[1],quantity.range[2],by = step.length)
    y.synt.off <- step.function(x.synt.off)
    
    order <- 1
    basis <- create.bspline.basis(rangeval=quantity.range, nbasis=nbasis[[nday]][[hour]], norder=order)
    smoothed <- smooth.basis(argvals=x.synt.off, y=y.synt.off, fdParobj=basis)
    matrix.smoothed.eval.off <- cbind(matrix.smoothed.eval.off,eval.fd(x.synt.off, smoothed$fd))
  }
  
  ## Domanda
  this.df <- restricted.plot.bid[[nday]][which(restricted.plot.bid[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  # original data
  abscissa <- this.df$Quantita.sum
  ordinate <- this.df$Prezzo
  
  if(length(abscissa) > 0 & length(ordinate) > 0){
    step.function <- stepfun(abscissa, c(ordinate[1], ordinate))
    x.synt.bid <- knots.bid
    y.synt.bid <- step.function(x.synt.bid)
    
    order <- 1
    basis <- create.bspline.basis(breaks = knots.bid, norder=2)
    smoothed <- smooth.basis(argvals=x.synt.bid, y=y.synt.bid, fdParobj=basis)
    matrix.smoothed.eval.bid <- cbind(matrix.smoothed.eval.bid, eval.fd(x.synt.bid, smoothed$fd))
  }
}

# Create a basis for the functional data object
basis.off <- create.bspline.basis(rangeval = quantity.range, nbasis = 801, norder = 2)
basis.bid <- create.bspline.basis(breaks = knots.bid, norder=2)

# Create a functional data object
fd.off <- Data2fd(argvals = x.synt.off, y = matrix.smoothed.eval.off, basis.off)
fd.bid <- Data2fd(argvals = x.synt.bid, y = matrix.smoothed.eval.bid, basis.bid)

# Perform functional principal component analysis
k <- 10
fpca.off <- pca.fd(fd.off, nharm=k, centerfns = FALSE)
fpca.bid <- pca.fd(fd.bid, nharm=k, centerfns = FALSE)

# Plot the functional principal components
plot.fd(fpca.off$harmonics, xlab = "Quantita", ylab = "Prezzo OFF", main = "Functional Principal Components")
legend('topright', legend = paste("PC", 1:k), col = 1:k, lty = 1)
plot.fd(fpca.bid$harmonics, xlab = "Quantita", ylab = "Prezzo OFF", main = "Functional Principal Components")
legend('topright', legend = paste("PC", 1:k), col = 1:k, lty = 1)


# Save of scores matrix
scores.matrix.off <- fpca.off$scores
scores.matrix.bid <- fpca.bid$scores


# Perform forecasting and save models
pc.model.prediction.off <- list()
pc.model.prediction.bid <- list()

for (i in 1:ncol(scores.matrix.off)){
  scores.off <- scores.matrix.off[,i]
  scores.bid <- scores.matrix.bid[,i]
  forecast.off <- forecast(auto.arima(scores.off))
  forecast.bid <- forecast(auto.arima(scores.bid))
  pc.model.prediction.off[[paste("PC", i)]] <- forecast.off
  pc.model.prediction.bid[[paste("PC", i)]] <- forecast.bid

}

# Extract the mean forecasted values
mean.prediction.forecast.off <- list()
for(i in 1:k){
  mean.prediction.forecast.off[[i]] <- predict(pc.model.prediction.off[[paste("PC", i)]], h = 1)$mean
}
mean.prediction.forecast.bid <- list()
for(i in 1:k){
  mean.prediction.forecast.bid[[i]] <- predict(pc.model.prediction.bid[[paste("PC", i)]], h = 1)$mean
}


# Reconstruct the forecasted curves
mean.prediction.forecast.value.off <- NULL
for(i in 1:k ){
  mean.prediction.forecast.value.off <- cbind(mean.prediction.forecast.value.off, mean.prediction.forecast.off[[i]][1])
}
forecast.reconstructed.curve.off <- fpca.off$harmonics$coefs %*% t(mean.prediction.forecast.value.off)

mean.prediction.forecast.value.bid <- NULL
for(i in 1:k ){
  mean.prediction.forecast.value.bid <- cbind(mean.prediction.forecast.value.bid, mean.prediction.forecast.bid[[i]][1])
}
forecast.reconstructed.curve.bid <- fpca.bid$harmonics$coefs %*% t(mean.prediction.forecast.value.bid)

# test accuracy of the code 
# plot(x.synt, scores.matrix.bid[1, ] %*% t(fpca.bid$harmonics$coefs) , ylab = "prezzo", xlab = "Quantita", main = "Reconstructed forecasted curves")
# lines(x.synt, matrix.smoothed.eval.bid[,1], col = 'blue', lwd = 6)

plot(NA, NA, xlim = quantity.range, ylim = prezzo.range, xlab = "Quantita", ylab = "Prezzo", main = "Predicted Curve")
# Reconstruct the forecasted curves applying our gang variable 
points(x.synt.off, forecast.reconstructed.curve.off, xlab = "Quantita", ylab = "Prezzo",lty =2, main = "Reconstructed forecasted curves", lwd=2, col = 'red', type='s')
# Reconstruct the forecasted curves applying our gang variable 
points(x.synt.bid, forecast.reconstructed.curve.bid, xlab = "Quantita", ylab = "Prezzo", main = "Reconstructed forecasted curves", lwd=2, col = 'green',type='s')
#delta_value <- delta.mcq %>% pull(delta)
#x.synt.off <- x.synt.off + delta_value
#points(x.synt.off, forecast.reconstructed.curve.off, xlab = "Quantita", ylab = "Prezzo", main = "Reconstructed forecasted curves", lwd=2, col = 'red', type='s')
#delta_value

intr <- find_intersection(x.synt.off, x.synt.bid, forecast.reconstructed.curve.off,forecast.reconstructed.curve.bid)
abline(v=intr$x,h=intr$y,lty=2,col='grey')
intr

####################################### fPCA go back hour by hour #################################

knots.bid <- unique(c(10000,seq(10000, 20000, length = 100), seq(20000, 30000, length = 600), seq(30000, 40000, length = 100), seq(40000, 50000, length = 100),50000 ))

# init of matrix of functions 
matrix.smoothed.eval.off <- c()
matrix.smoothed.eval.bid<- c()

# predict.day.seq <- as.Date(c(
#                           '2023-01-09','2023-01-24', '2023-02-09','2023-02-24', '2023-03-09','2023-03-24', '2023-04-09','2023-04-24',
#                           '2023-05-09','2023-05-24', '2023-06-09','2023-06-24', '2023-07-09','2023-07-24', '2023-08-09','2023-08-24',
#                           '2023-09-09','2023-09-24', '2023-10-09','2023-10-24', '2023-11-09','2023-11-24', '2023-12-09','2023-12-24'
#                           )) # 2 giorni al mese scegli

predict.day.seq <- as.Date(c('2023-01-11',
                             '2023-01-27',
                             '2023-02-11',
                             '2023-02-27',
                             '2023-03-11',
                             '2023-03-27',
                             '2023-04-11',
                             '2023-04-27',
                             '2023-05-11',
                             '2023-05-27',
                             '2023-06-11',
                             '2023-06-27',
                             '2023-07-11',
                             '2023-07-27',
                             '2023-08-11',
                             '2023-08-27',
                             '2023-09-11',
                             '2023-09-27',
                             '2023-10-11',
                             '2023-10-27',
                             '2023-11-11',
                             '2023-11-27',
                             '2023-12-11',
                             '2023-12-27')) # 2 giorni al mese scegli
hour.to.predict <- 20
go.back.by <- 5
info.by.day <- list()

for(day.to.predict in predict.day.seq){
  
  info <- c()
  
  n.end.day <- which(date.sequence==as.Date(day.to.predict))
  n.start.day <- n.end.day-go.back.by
  hour.seq <- 1:24
  
  for(nday in n.start.day:n.end.day){
    print(nday)
    if(nday==n.end.day){
      hour.seq <- 1:(hour.to.predict-1)
    }
    
    for(hour in hour.seq) {
      
      ## Offerta
      this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
      
      # original data
      abscissa <- this.df$Quantita.sum
      ordinate <- this.df$Prezzo
      
      if(length(abscissa) > 0 & length(ordinate) > 0){
        step.function <- stepfun(abscissa,c(ordinate[1],ordinate))
        x.synt.off <- seq(quantity.range[1],quantity.range[2],by = step.length)
        y.synt.off <- step.function(x.synt.off)
        
        order <- 1
        basis <- create.bspline.basis(rangeval=quantity.range, nbasis=nbasis[[nday]][[hour]], norder=order)
        smoothed <- smooth.basis(argvals=x.synt.off, y=y.synt.off, fdParobj=basis)
        matrix.smoothed.eval.off <- cbind(matrix.smoothed.eval.off,eval.fd(x.synt.off, smoothed$fd))
      }
      
      ## Domanda
      this.df <- restricted.plot.bid[[nday]][which(restricted.plot.bid[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
      # original data
      abscissa <- this.df$Quantita.sum
      ordinate <- this.df$Prezzo
      
      if(length(abscissa) > 0 & length(ordinate) > 0){
        step.function <- stepfun(abscissa, c(ordinate[1], ordinate))
        x.synt.bid <- knots.bid
        y.synt.bid <- step.function(x.synt.bid)
        
        order <- 1
        basis <- create.bspline.basis(breaks = knots.bid, norder=2)
        smoothed <- smooth.basis(argvals=x.synt.bid, y=y.synt.bid, fdParobj=basis)
        matrix.smoothed.eval.bid <- cbind(matrix.smoothed.eval.bid, eval.fd(x.synt.bid, smoothed$fd))
      }
    }
  }
  
  # Create a basis for the functional data object
  basis.off <- create.bspline.basis(rangeval = quantity.range, nbasis = n, norder = 2)
  basis.bid <- create.bspline.basis(breaks = knots.bid, norder=2)
  
  # Create a functional data object
  fd.off <- Data2fd(argvals = x.synt.off, y = matrix.smoothed.eval.off, basis.off)
  fd.bid <- Data2fd(argvals = x.synt.bid, y = matrix.smoothed.eval.bid, basis.bid)
  
  # Perform functional principal component analysis
  k <- 10
  fpca.off <- pca.fd(fd.off, nharm=k, centerfns = FALSE)
  fpca.bid <- pca.fd(fd.bid, nharm=k, centerfns = FALSE)
  
  # Save of scores matrix
  scores.matrix.off <- fpca.off$scores
  scores.matrix.bid <- fpca.bid$scores
  
  
  # Perform forecasting and save models
  pc.model.prediction.off <- list()
  pc.model.prediction.bid <- list()
  
  for (i in 1:ncol(scores.matrix.off)){
    scores.off <- scores.matrix.off[,i]
    scores.bid <- scores.matrix.bid[,i]
    forecast.off <- forecast(auto.arima(scores.off))
    forecast.bid <- forecast(auto.arima(scores.bid))
    pc.model.prediction.off[[paste("PC", i)]] <- forecast.off
    pc.model.prediction.bid[[paste("PC", i)]] <- forecast.bid
    
  }
  
  # Extract the mean forecasted values
  mean.prediction.forecast.off <- list()
  for(i in 1:k){
    mean.prediction.forecast.off[[i]] <- predict(pc.model.prediction.off[[paste("PC", i)]], h = 1)$mean
  }
  mean.prediction.forecast.bid <- list()
  for(i in 1:k){
    mean.prediction.forecast.bid[[i]] <- predict(pc.model.prediction.bid[[paste("PC", i)]], h = 1)$mean
  }
  
  
  # Reconstruct the forecasted curves
  mean.prediction.forecast.value.off <- NULL
  for(i in 1:k ){
    mean.prediction.forecast.value.off <- cbind(mean.prediction.forecast.value.off, mean.prediction.forecast.off[[i]][1])
  }
  forecast.reconstructed.curve.off <- fpca.off$harmonics$coefs %*% t(mean.prediction.forecast.value.off)
  
  mean.prediction.forecast.value.bid <- NULL
  for(i in 1:k ){
    mean.prediction.forecast.value.bid <- cbind(mean.prediction.forecast.value.bid, mean.prediction.forecast.bid[[i]][1])
  }
  forecast.reconstructed.curve.bid <- fpca.bid$harmonics$coefs %*% t(mean.prediction.forecast.value.bid)

  intr <- find_intersection(x.synt.off,x.synt.bid,forecast.reconstructed.curve.off,forecast.reconstructed.curve.bid)
  
  # Real intersection
  nday <- which(date.sequence==day.to.predict)
  hour <- hour.to.predict
  
  off <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)]
  bid <- restricted.plot.bid[[nday]][which(restricted.plot.bid[[nday]]$Ora %in% hour),c(2,5,6)]
  
  if(length(off$Quantita.sum) > 0 & length(bid$Quantita.sum) > 0){
    real.intr <- find_intersection(off$Quantita.sum,bid$Quantita.sum,off$Prezzo,bid$Prezzo)    
  } else{
    real.intr <- data.frame(x=1,y=1)
  }
  
  info <- rbind(info, c(qta_real = real.intr$x,
                        prz_real = real.intr$y,
                        qta_pred = intr$x,
                        prz_pred = intr$y,
                        diff_qta = intr$x-real.intr$x,
                        diff_prz = intr$y-real.intr$y,
                        diff_qta_percentage = abs(intr$x-real.intr$x)/real.intr$x,
                        diff_prz_percentage = abs(intr$y-real.intr$y)/real.intr$y))
  
  info.by.day[[as.character(as.Date(day.to.predict))]] <- info
  
  print(paste("Day:",as.Date(day.to.predict)," done! - Percentage of work: ",(which(predict.day.seq==day.to.predict)/length(predict.day.seq)*100)," %"))
}

info.by.day

mean.percentage.qta <- 0
mean.percentage.prz <- 0
it <- 0
for(day in predict.day.seq){
  if(info.by.day[[as.character(as.Date(day))]][1] != 1 & info.by.day[[as.character(as.Date(day))]][2] != 1){
    text <- paste("diff qta %: ", info.by.day[[as.character(as.Date(day))]][7]*100)
    text <- paste(text, " diff prz %: ", info.by.day[[as.character(as.Date(day))]][8]*100)
    print(text)
    
    mean.percentage.qta <- mean.percentage.qta + info.by.day[[as.character(as.Date(day))]][7]*100
    mean.percentage.prz <- mean.percentage.prz + info.by.day[[as.character(as.Date(day))]][8]*100
    
    it <- it+1
  }
}

print(paste("Mean diff qta %: ", mean.percentage.qta/it))
print(paste("Mean diff prz %: ", mean.percentage.prz/it))

save(info.by.day, file = "Pippo_test.RData")

##################################### Csv test ############################################

temp <- data.frame()

insert_data_csv <- function(info.by.day, ora){
  
  for (i in seq_along(info.by.day)) {
    # extract info
    day <- as.Date(names(info.by.day)[i])
    info <- info.by.day[[i]]
    
    # create a data frame
    temp <- rbind(temp, data.frame(day = day, info, ora = ora))
  }
  
  return(temp)
}

# Load test data obtained in the previous chapter - the names are of my team member which helped me in this project
load("Klaus_test.RData")
temp <- insert_data_csv(info.by.day, 20)
load("Ale_test.RData")
temp <- insert_data_csv(info.by.day, 13)
load("Pippo_test.RData")
temp <- insert_data_csv(info.by.day, 1)
load("Rob_test.RData")
temp <- insert_data_csv(info.by.day, 8)

# Remove data which doesn't have a real quantity which are marked with a 1
temp <- temp[-which(temp$qta_real==1), ]

# Apply create numeric column 
temp <- temp %>% mutate_all(as.numeric)

# remove data for which our function find intersection doesn't work
temp <- temp [- which(temp$diff_prz_percentage > 10), ]

# mean and variance of our result 
colMeans(temp)
var(temp)
sd(temp)

temp$ora <- as.factor(temp$ora)

# plot boxplot for hour of percentage differences of price
par(mfrow=c(2,2))
for(i in c(1, 8, 13, 20)){
  boxplot(temp[temp$ora==i, 9], main = paste("Hour:",i), xlab = "Percentage", ylab = "Diff Prezzo")
}

# some statistics
sd.per.hour <- temp %>% group_by(ora) %>% summarise(sd = sd(diff_prz))
sd.per.hour

mean.diff.prz <- temp %>% group_by(ora) %>% summarise(mean = mean(diff_prz))
mean.diff.prz

mean.diff.prz.percentage <- temp %>% group_by(ora) %>% summarise(mean = mean(diff_prz_percentage)*100)
mean.diff.prz.percentage

# table to see if our results are good
table <- cbind(mean.diff.prz, mean.diff.prz.percentage[2], sd.per.hour[2])
names(table) <- c("Ora", "Mean difference", "%", "SD")
table

# write the result in a csv file  
write.csv(temp, "test_result.csv")

