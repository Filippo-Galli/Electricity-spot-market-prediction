library(Microsoft365R)
library(dplyr)
library(ggplot2)
library(tidyverse)
library (fda)
library(tidyr)
library(stats)

od <- get_business_onedrive()
#od$list_files()
#od$open_item("Test senza titolo 1.xlsx")
newfile <- od$get_item("Test senza titolo 1.xlsx")
newfile$download("responses.xlsx", overwrite = T)
data <- read_excel(path="C:/Users/User/OneDrive - Politecnico di Milano/responses.xlsx")

data_off <- data[which(data$`Do you want to buy or sell electricity?`=="offer"), c(12,15)]
data_bid <- data[which(data$`Do you want to buy or sell electricity?`=="bid"), c(18,21)]
names(data_off) <- c("Quantita", "Prezzo")
names(data_bid) <- c("Quantita", "Prezzo")
# Group by day and hour, then sort each group by prezzo
sorted.df.off <- data_off %>% arrange(desc(Prezzo))
sorted.df.bid <- data_bid %>% arrange(Prezzo)

# calculate cumulative sums
cumulative.df.off <- sorted.df.off %>%
  mutate(Quantita.sum = cumsum(Quantita))
cumulative.df.bid <- sorted.df.bid %>%
  mutate(Quantita.sum = cumsum(Quantita))
quantity.range <- c(0, 50000)  
prezzo.range <- c(0, 40000)

restricted.plot.off <- list()
restricted.plot.bid <- list()

restricted.plot.off[[1]] <- cumulative.df.off %>%
    filter(Quantita.sum >= quantity.range[1] & Quantita.sum <= quantity.range[2] & 
              Prezzo >= prezzo.range[1] & Prezzo <= prezzo.range[2])
  
restricted.plot.bid[[1]] <- cumulative.df.bid %>%
    filter( Quantita.sum >= quantity.range[1] & Quantita.sum <= quantity.range[2] & 
              Prezzo >= prezzo.range[1] & Prezzo <= prezzo.range[2])
combined_data <- rbind(restricted.plot.off[[1]], restricted.plot.bid[[1]]) 
ggplot(restricted.plot.off[[1]], aes(x = Quantita.sum, y = Prezzo)) +  geom_step() +
  labs(title = paste("Cumulative Sum of Quantity vs. Prezzo"),
       x = "Cumulative Quantity",
       y = "Prezzo") +
  theme_minimal()
plot(restricted.plot.bid[[1]]$Quantita.sum, restricted.plot.bid[[1]]$Prezzo, type='s', xlim=c(0,max(restricted.plot.bid[[1]]$Quantita.sum)+100), ylim=c(0,max(restricted.plot.bid[[1]]$Prezzo)+100), col='blue')
points(restricted.plot.off[[1]]$Quantita.sum, restricted.plot.off[[1]]$Prezzo, type='s', col='red')
