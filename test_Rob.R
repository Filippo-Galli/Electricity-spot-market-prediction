library(dplyr)
library(ggplot2)
library(tidyverse)
# data cleaning -----------------------------------------------------------



df <- read.csv("data2008", header=T)
# off_data$PrezzoZonale = as.numeric(off_data$PrezzoZonale)
print("Data Cleaning...")

# Clean all the rows without all zones
table(df$ZonaMercato)
correct_zone <- c('E_nw;Nord;E_ne;Tbrv;Mftv;Cnor;Cors;Sard;E_co;Coac;Csud;Sud;Fogn;Rosn;Brnn;E_sd;Calb;')
df <- df[!df$ZonaMercato != correct_zone, ]
table(df$ZonaMercato)

# Clean all the demand rows
table(df$Tipo)
correct_tipo <- c('OFF')
df <- df[!df$Tipo != correct_tipo, ]
table(df$Tipo)

# dropping column with only one value
df <- subset(df, select = -c(Tipo, ZonaMercato, Mercato, element))

# Convert the 'Prezzo' column from character to numeric
df$Prezzo <- as.numeric(df$Prezzo)

# Convert the 'Quantita' column from character to numeric
df$Quantita <- as.numeric(df$Quantita)

# Convert the 'Ora' column from character to factor 
df$Ora <- as.numeric(df$Ora)

# Convert the 'PrezzoZonale' column from character to numeric
df$PrezzoZonale <- as.numeric(df$PrezzoZonale)

df <- na.omit(df) 
write.csv(df, "data2008_cl", row.names = F)

# cleaned data analysis ---------------------------------------------------


df <-read.csv("data2008_cl")
#split the date in year month and day
df <- df %>%
  mutate(Data = as.Date(as.character(Data), format = "%Y%m%d"))
df <- df %>%
  mutate(year = year(Data))
df <- df %>%
  mutate(month = month(Data))
df <- df %>%
  mutate(day = day(Data))
df$year <- factor(df$year, levels = c("2008"))
df$month <- factor(df$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
df$Ora <- factor(df$Ora, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))
df <- na.omit(df)

ggplot(df, aes(x = Ora, y = PrezzoZonale, fill = Ora)) +
  geom_boxplot() +
  labs(x = "Ora", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by Ora")
scale_fill_discrete(name = "Ora")

dev.new()
par(mfrow=c(3,4))
for(i in 1:12) {
  qqnorm(df$PrezzoZonale[which(df$Ora==i)])
  qqline(df$PrezzoZonale[which(df$Ora==i)], col = 2, lwd=3)
}
dev.new()
par(mfrow=c(3,4))
for(i in 13:24) {
  qqnorm(df$PrezzoZonale[which(df$Ora==i)])
  qqline(df$PrezzoZonale[which(df$Ora==i)], col = 2)
}

ggplot(df, aes(x = month, y = PrezzoZonale, fill = month)) +
  geom_boxplot() +
  labs(x = "month", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by month")
  scale_fill_discrete(name = "month")

dev.new()
par(mfrow=c(3,4))
for(i in 1:12) {
  qqnorm(df$PrezzoZonale[which(df$month==i)])
  qqline(df$PrezzoZonale[which(df$month==i)], col = 2, lwd=3)
}

# df containing prezzoZonale without replicas -----------------------------


#dataframe containing PrezzoZonale without replicas(to be optimized)

df_prezzoZonale=data.frame(year=0, month=0, day=0, hour=0, prezzoZonale=0)
df_prezzoZonale$year[1] <- as.character(df$year[1])
df_prezzoZonale$month <- as.character(df$month[1])
df_prezzoZonale$day <- as.character(df$day[1])
df_prezzoZonale$hour<- as.character(df$Ora[1])
df_prezzoZonale$prezzoZonale <- df$PrezzoZonale[1]

for(n in 2:dim(df)[1]) {
  if(as.character(df$Ora[n])!=df_prezzoZonale$hour[dim(df_prezzoZonale)[1]]||as.character(df$day[n])!=df_prezzoZonale$day[dim(df_prezzoZonale)[1]]
    ||as.character(df$month[n])!=df_prezzoZonale$month[dim(df_prezzoZonale)[1]]||as.character(df$year[n])!=df_prezzoZonale$year[dim(df_prezzoZonale)[1]]) {
    new_row <- data.frame(year = as.character(df$year[n]),
                          month = as.character(df$month[n]),
                          day = as.character(df$day[n]),
                          hour = as.character(df$Ora[n]),
                          prezzoZonale = df$PrezzoZonale[n])
    df_prezzoZonale <- rbind(df_prezzoZonale, new_row)
  }
}

df_prezzoZonale$year <- factor(df_prezzoZonale$year, levels = c("2008"))
df_prezzoZonale$month <- factor(df_prezzoZonale$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
df_prezzoZonale$hour <- factor(df_prezzoZonale$hour, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24"))

#simple analysis of the dataframe containing PrezzoZonale without replicas
tot_mean = mean(df_prezzoZonale$prezzoZonale)
tot_sigma = sd(df_prezzoZonale$prezzoZonale)
means_vector_hour <- aggregate(prezzoZonale ~ hour, data = df_prezzoZonale, FUN = mean)
means_vector_month <- aggregate(prezzoZonale ~ month, data = df_prezzoZonale, FUN = mean)
sd_hour <- aggregate(prezzoZonale ~ hour, data = df_prezzoZonale, FUN = sd)
sd_month <- aggregate(prezzoZonale ~ month, data = df_prezzoZonale, FUN = sd)
par(mfrow=c(1,2))
plot(levels(df_prezzoZonale$hour), sd_hour[,2])
plot(levels(df_prezzoZonale$month), sd_month[,2])
# Create boxplots for each month
ggplot(df_prezzoZonale, aes(x = month, y = prezzoZonale, fill = month)) +
  geom_boxplot() +
  labs(x = "month", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by month") +
  scale_fill_discrete(name = "month")

# Create boxplots for each hour
ggplot(df_prezzoZonale, aes(x = hour, y = prezzoZonale, fill = hour)) +
  geom_boxplot() +
  labs(x = "hour", y = "prezzoZonale") +
  ggtitle("Boxplot of prezzoZonale by hour") +
  scale_fill_discrete(name = "hour")
  