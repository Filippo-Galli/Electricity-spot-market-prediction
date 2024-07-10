library(Microsoft365R)
library(dplyr)
library(ggplot2)
library(tidyverse)
library (fda)
library(tidyr)
library(stats)
library(readxl)

od <- get_business_onedrive() #only one time
#od$list_files()
#od$open_item("Insert your offer or bid!.xlsx")

#to be insert in a loop
newfile <- od$get_item("Insert your offer or bid!.xlsx")
newfile$download("responses.xlsx", overwrite = T)
data <- read_excel(path="C:/Users/User/Documents/GitHub/Applied/responses.xlsx")


data_off <- data[which(data$`Do you want to buy or sell electricity?`=="offer"), c(12,15)]
data_bid <- data[which(data$`Do you want to buy or sell electricity?`=="bid"), c(18,21)]
names(data_off) <- c("Quantita", "Prezzo")
names(data_bid) <- c("Quantita", "Prezzo")
data_off$Quantita <- data_off$Quantita*1000
data_bid$Quantita <- data_bid$Quantita*1000
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

plot(restricted.plot.bid[[1]]$Quantita.sum, restricted.plot.bid[[1]]$Prezzo, type='s',
     xlim=c(0,max(restricted.plot.bid[[1]]$Quantita.sum)+1000), ylim=c(0,max(restricted.plot.bid[[1]]$Prezzo)+100), col='blue',
     main = "Cumulative Sum of Quantity vs. Prezzo", xlab = "Cumulative Quantity", ylab = "Prezzo")
points(restricted.plot.off[[1]]$Quantita.sum, restricted.plot.off[[1]]$Prezzo, type='s', col='red')

#### intersection ----
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

mcp <- find_intersections(restricted.plot.off[[1]],restricted.plot.bid[[1]])
mcp

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
