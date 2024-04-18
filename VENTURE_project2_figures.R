# BIOSTAT 620 - Group Project II
# Vikram Bala, Ethan Werner, Neyan Deng
# Team VENTURE

library(readxl)
library(dplyr)
library(ggplot2)

# read data and create lag-1 variable for total screen time
data <- read_excel("final_full_data.xlsx")
data$lag_st <- c(NA, data$Total.ST.min[-length(data$Total.ST.min)])

# generate boxplots
boxplot(Total.ST.min~Phase, data=data, main="Screen Time by Phase")
boxplot(Total.ST.min~weekday, data=data, main="Screen Time by Weekday/Weekend Status")
boxplot(Total.ST.min~Treatment, data=data[data$Phase=="Treatment",], main="Screen Time by Treatment During Intervention")

# generate scatterplots
data_sub_phase <- select(data, "Total.ST.min", "lag_st", "Pickups")
plot(data_sub_phase)
plot(data_sub_phase, col=as.factor(data$Phase), main="Pairwise Correlation by Phase")

data_sub_treatment <- select(data[data$Phase=="Treatment",], "Total.ST.min", "lag_st", "Pickups")
plot(data_sub_treatment)
plot(data_sub_treatment, col=as.factor(data[data$Phase=="Treatment",]$Treatment),
     main="Pairwise Correlation by Treatment During Intervention")

# generate time series plots
data_grouped <- data %>% group_by(Date) %>% summarize(mean_st=mean(Total.ST.min, na.rm=T))
data_grouped <- data_grouped[1:101,]
data_grouped$Phase <- ifelse(as.numeric(row.names(data_grouped)) <= 94, 0, 1)
ggplot(aes(x=Date, y=mean_st, col=Phase), data=data_grouped) + geom_line() +
  ggtitle("Time Series of Mean Screen Time by Phase")

data_treat <- data[data$Phase == "Treatment",]
data_grouped2 <- data_treat %>% group_by(Date, Treatment) %>% summarize(mean_st=mean(Total.ST.min, na.rm=T))
data_grouped2 <- data_grouped2[1:14,]
ggplot(aes(x=Date, y=mean_st, col=Treatment), data=data_grouped2) + geom_line() +
  ggtitle("Time Series of Mean Screen Time by Treatment During Intervention")

# generate ACF plots
base <- data_grouped[data_grouped$Phase == 0,]
treat <- data_grouped[data_grouped$Phase == 1,]
acf_base <- acf(base$mean_st, plot=T, main="ACF of Mean Screen Time Across Baseline")
acf_treat <- acf(treat$mean_st, plot=T, main="ACF of Mean Screen Time Across Treatment")

int_A <- data_grouped2[data_grouped2$Treatment == "A",]
int_B <- data_grouped2[data_grouped2$Treatment == "B",]
acf_A <- acf(int_A$mean_st, plot=T, main="ACF of Mean Screen Time Across \n Treatment A During Intervention")
acf_B <- acf(int_B$mean_st, plot=T, main="ACF of Mean Screen Time Across \n Treatment B During Intervention")
