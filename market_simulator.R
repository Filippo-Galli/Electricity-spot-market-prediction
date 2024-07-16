library(Microsoft365R)
library(dplyr)
library(ggplot2)
library(tidyverse)
library (fda)
library(tidyr)
library(stats)
library(readxl)

od <- get_business_onedrive() #only one time

# REMEMBER: set correctly the working directory and the file name

find_step_intersections <- function(c.off, c.bid) {
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

repeat {
  
  print("Catching new data...")
  
  ## OneDrive connection
  
  # newfile <- od$get_item("Insert your offer or bid!.xlsx")
  # newfile$download("responses.xlsx", overwrite = T)
  # data <- read_excel(path="C:/Users/User/Documents/GitHub/Applied/responses.xlsx")
  
  ## Local connection
  data <- read_excel("Insert your offer or bid!.xlsx")
  random_item <- sample(1:nrow(data), 1)
  data <- data[-random_item,]
  
  names(data)[8] <- "Type"
  names(data)[11] <- "OFF quant"
  names(data)[14] <- "OFF price"
  names(data)[17] <- "BID quant"
  names(data)[20] <- "BID price"
  
  # Maybe using OneDrive connection you need do update this and add +1 to all indexes here
  data <- data[, c(1, 2, 3, 4, 5, 8, 11, 14, 17, 20)]
  
  data_off <- data[which(data$Type=="offer"), c(7,8)]
  data_bid <- data[which(data$Type=="bid"), c(9,10)]
  names(data_off) <- c("Quantita", "Prezzo")
  names(data_bid) <- c("Quantita", "Prezzo")
  data_off$Quantita <- as.numeric(data_off$Quantita)
  data_bid$Quantita <- as.numeric(data_bid$Quantita)
  data_off$Prezzo <- as.numeric(data_off$Prezzo)
  data_bid$Prezzo <- as.numeric(data_bid$Prezzo)
  
  
  # Group by day and hour, then sort each group by prezzo
  sorted.df.off <- data_off %>% arrange(Prezzo)
  sorted.df.bid <- data_bid %>% arrange(desc(Prezzo))
  
  # calculate cumulative sums
  cumulative.df.off <- sorted.df.off %>%
    mutate(Quantita.sum = cumsum(Quantita))
  cumulative.df.bid <- sorted.df.bid %>%
    mutate(Quantita.sum = cumsum(Quantita))
  quantity.range <- c(0, 5000000)  
  prezzo.range <- c(0, 600)
  
  restricted.plot.off <- list()
  restricted.plot.bid <- list()
  
  restricted.plot.off[[1]] <- cumulative.df.off %>%
    filter(Quantita.sum >= quantity.range[1] & Quantita.sum <= quantity.range[2] & 
             Prezzo >= prezzo.range[1] & Prezzo <= prezzo.range[2])
  
  restricted.plot.bid[[1]] <- cumulative.df.bid %>%
    filter( Quantita.sum >= quantity.range[1] & Quantita.sum <= quantity.range[2] & 
              Prezzo >= prezzo.range[1] & Prezzo <= prezzo.range[2])
  
  plot(restricted.plot.bid[[1]]$Quantita.sum, restricted.plot.bid[[1]]$Prezzo, ylim=c(0,max(restricted.plot.bid[[1]]$Prezzo)+100), col='blue',
       main = "Cumulative Sum of Quantity vs. Prezzo", xlab = "Cumulative Quantity", ylab = "Prezzo", type = "s")
  points(restricted.plot.off[[1]]$Quantita.sum, restricted.plot.off[[1]]$Prezzo, col='red', type = "s")
  legend("topright", legend=c("Bid", "Offer"), col=c("blue", "red"), pch=15)
  
  #### intersection ----
  x.synt.off <- restricted.plot.off[[1]]$Quantita.sum
  x.synt.bid <- restricted.plot.bid[[1]]$Quantita.sum
  mcp <- find_step_intersections(restricted.plot.off[[1]]$Prezzo,restricted.plot.bid[[1]]$Prezzo)
  print(paste("Equilibrium point: x = ", mcp[1], ", y = ", mcp[2]))
  abline(h=mcp[2], col="grey", lty=2)
  abline(v=mcp[1], col="grey", lty=2)
  
  Sys.sleep(5)
  
}


#### mail ----
mail <- data$`Posta elettronica`
mail <- unique(mail)
my_outlook <- get_business_outlook()
my_email <- my_outlook$create_email(paste("The auction is closed.\nThere have been exchanged", as.character(round(mcp[1], digits=2)),
                                      "GWh of electricity at", as.character(round(mcp[2], digits=2)), "€/GWh\nThank you for participating in the electricity market\nIt's Friday again, be safe...and vote for us!\nGroup 24: Barbieri, Fortuna, Galli, Grifalconi, Howe"), 
                                    subject = "Electricity market results", to = mail)
pdf(file="saving_plot.pdf")
plot(restricted.plot.bid[[1]]$Quantita.sum, restricted.plot.bid[[1]]$Prezzo, type='s', xlim=c(0,max(restricted.plot.bid[[1]]$Quantita.sum)+100), ylim=c(0,max(restricted.plot.bid[[1]]$Prezzo)+100), col='blue')
points(restricted.plot.off[[1]]$Quantita.sum, restricted.plot.off[[1]]$Prezzo, type='s', col='red')
dev.off()
my_email$add_attachment("saving_plot.pdf")
my_email$send()


fd.1 <- stepfun(restricted.plot.off[[1]]$Quantita.sum[-1], restricted.plot.off[[1]]$Prezzo)
plot(fd.1, verticals = TRUE, do.points = TRUE, col = "red", lwd = 2)
