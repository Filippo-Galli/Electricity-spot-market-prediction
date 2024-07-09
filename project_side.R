rm(list = ls())

######################### Library #############################

library(dplyr)
library(ggplot2)
library(tidyverse)
library (fda)
library(tidyr)
library(stats)

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

}
names(restricted.plot.off) <- date.sequence
names(restricted.plot.bid) <- date.sequence

ggplot(restricted.plot.bid[[24]], aes(x = Quantita.sum, y = Prezzo, color = as.factor(Ora))) +
  geom_step() +
  labs(title = paste("Cumulative Sum of Quantity vs. Prezzo for ", date.sequence[24]),
       x = "Cumulative Quantity",
       y = "Prezzo",
       color = "Hour") +
  theme_minimal()

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

# Function to find intersections
find_intersections <- function(df1, df2) {
  intersections <- data.frame()
  for (i in 1:(nrow(df1) - 1)) {
    for (j in 1:(nrow(df2) - 1)) {
      # Check if lines (x1, y1)-(x2, y2) and (x3, y3)-(x4, y4) intersect
      a1 <- df1$Prezzo[i+1] - df1$Prezzo[i]
      b1 <- df1$Quantita.sum[i] - df1$Quantita.sum[i+1]
      c1 <- a1 * df1$Quantita.sum[i] + b1 * df1$Prezzo[i]
      
      a2 <- df2$Prezzo[j+1] - df2$Prezzo[j]
      b2 <- df2$Quantita.sum[j] - df2$Quantita.sum[j+1]
      c2 <- a2 * df2$Quantita.sum[j] + b2 * df2$Prezzo[j]
      
      det <- a1 * b2 - a2 * b1
      if (det != 0) {
        x <- (b2 * c1 - b1 * c2) / det
        y <- (a1 * c2 - a2 * c1) / det
        # Check if intersection point (x, y) is within line segments
        if (x >= min(df1$Quantita.sum[i], df1$Quantita.sum[i+1]) && 
            x <= max(df1$Quantita.sum[i], df1$Quantita.sum[i+1]) && 
            x >= min(df2$Quantita.sum[j], df2$Quantita.sum[j+1]) && 
            x <= max(df2$Quantita.sum[j], df2$Quantita.sum[j+1])) {
          intersections <- rbind(intersections, data.frame(x = x, y = y))
        }
      }
    }
  }
  return(intersections)
}

mcp <- find_intersections(off,bid)
plot + geom_point(data = mcp, aes(x = x, y = y), color = "black", size = 3, inherit.aes = FALSE)

################################### Smoothing ###################################################

library(fda)

# TOO HEAVY and not very informative
# nbasis.gcv <- list()
# 
# for(day in as.character(date.sequence)){
#   
#   day.list <- list()
#   
#   for(hour in 1:24){
#     
#     this.df <- restricted.plot.off[[day]][which(restricted.plot.off[[day]]$Ora %in% hour),c(2,5,6)]
#     
#     # original data
#     abscissa <- this.df$Quantita.sum
#     ordinate <- this.df$Prezzo
#     
#     if(length(abscissa) > 0 & length(ordinate) > 0){
#       # step function -> syntetic point -> smoothing
#       step.function <- stepfun(abscissa,c(ordinate[1],ordinate))
#       x.synt <- seq(quantity.range[1],quantity.range[2],by = 100)
#       y.synt <- step.function(x.synt)
#       order <- 1
#       nbasis <- seq(100, 400, by = 10)
#       gcv <- rep(NA,length(nbasis))
#       
#       for (i in 1:length(nbasis)){
#         basis <- create.bspline.basis(rangeval=quantity.range, nbasis=nbasis[i], norder=order)
#         gcv[i] <- smooth.basis(x.synt, y.synt, basis)$gcv
#       }
#       
#       day.list[[as.character(hour)]] <- nbasis[which.min(gcv)]
#       
#     } else{
#       day.list[[as.character(hour)]] <- NA
#     }
#     
#     
#   }
#   
#   nbasis.gcv[[day]] <- day.list
#   
# }

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

nday <- 150
day <- as.character(date.sequence[nday])

plot(NA, NA, xlim = quantity.range, ylim = prezzo.range, xlab = "Quantita", ylab = "Prezzo", main = paste("Smoothed of",day,sep=" "))

for(hour in 1:24){
  this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
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


i <- 1
for(nday in ndays){
  this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
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

##################################### Alignment ############################################

install.packages("fdasrvf")
library(fdasrvf)

# init of matrix of functions to align
matrix.smoothed.eval <- c()
#matrix.smoothed.eval.fd <- c()

# by days, fixed hour
start.day <- as.Date("2023-01-01")
end.day<- as.Date("2023-01-31")
hour <- 1
ndays <- seq(which(date.sequence==start.day),which(date.sequence==end.day), by = 1)

for(nday in ndays){
  
  this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
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
    matrix.smoothed.eval <- cbind(matrix.smoothed.eval,eval.fd(x.synt, smoothed$fd))
    #matrix.fd <- Data2fd(argvals=x.synt, y=matrix.smoothed.eval.fd, basis)
    #matrix.smoothed.eval.fd <- cbind(matrix.smoothed.eval.fd,eval.fd(x.synt, datafd))
  }
  
}

# by hours, fixed day

day <- as.Date("2023-01-01")
nday <- which(date.sequence==day)

for(hour in 1:24){
  this.df <- restricted.plot.off[[nday]][which(restricted.plot.off[[nday]]$Ora %in% hour),c(2,5,6)] %>% group_by(Ora)
  
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
    matrix.smoothed.eval <- cbind(matrix.smoothed.eval,eval.fd(x.synt, smoothed$fd))
    #matrix.fd <- Data2fd(argvals=x.synt, y=matrix.smoothed.eval.fd, basis)
    #matrix.smoothed.eval.fd <- cbind(matrix.smoothed.eval.fd,eval.fd(x.synt, datafd))
  }
  
}

# Perform warping using time warping
aligned.curves <- time_warping(matrix.smoothed.eval, time = x.synt)
#aligned.curves.fd <- time_warping(matrix.smoothed.eval.fd, time = x.synt)

plot(NA, NA, xlim = quantity.range, ylim = prezzo.range, xlab = "Quantita", ylab = "Prezzo", main = "Alignement plot")
legend('bottomright', legend = c('Original', 'Aligned'), col = c('blue', 'red'), lty = 1)

for(j in 1:ncol(matrix.smoothed.eval)){
  lines(x.synt, matrix.smoothed.eval[,j], col = 'blue')
  lines(x.synt, aligned.curves$fn[,j], col = 'red')
  #lines(x.synt, aligned.curves.fd$fn[,j], col = 'green')
}

####################################### fPCA #################################

# init of matrix of functions to align
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

library(forecast)

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

find_intersections_prediction <- function(c.off, c.bid) {
  intersections <- data.frame()
  for (i in 1:(nrow(c.off) - 1)) {
    for (j in 1:(nrow(c.bid) - 1)) {
      # Check if lines (x1, y1)-(x2, y2) and (x3, y3)-(x4, y4) intersect
      a1 <- c.off[i+1] - c.off[i]
      b1 <- x.synt.off[i] - x.synt.bid[i+1]
      c1 <- a1 * x.synt.bid[i] + b1 * c.off[i]
      
      a2 <- c.bid[j+1] - c.bid[j]
      b2 <- x.synt.bid[j] - x.synt.bid[j+1]
      c2 <- a2 * x.synt.bid[j] + b2 * c.bid[j]
      
      det <- a1 * b2 - a2 * b1
      if (det != 0) {
        x <- (b2 * c1 - b1 * c2) / det
        y <- (a1 * c2 - a2 * c1) / det
        # Check if intersection point (x, y) is within line segments
        if (x >= min(x.synt.off[i], x.synt.off[i+1]) && 
            x <= max(x.synt.off[i], x.synt.off[i+1]) && 
            x >= min(x.synt.bid[j], x.synt.bid[j+1]) && 
            x <= max(x.synt.bid[j], x.synt.bid[j+1])) {
          intersections <- rbind(intersections, data.frame(x = x, y = y))
        }
      }
    }
  }
  return(intersections)
}

intr <- find_intersections_prediction(forecast.reconstructed.curve.off,forecast.reconstructed.curve.bid)
abline(v=intr$x,h=intr$y,lty=2,col='grey')
intr

par(mfrow=c(1,1))





















