library(dplyr)
library(tidyverse)
library(ggplot2)
library(readr)

data1 = read_csv("lab_sodra.csv")

summary(data1)

data1 = filter(data1, data1$ecoActCode==949900)
print(data1)

#1

install.packages("viridis")
library("viridis")

hist(data1$avgWage, main = NULL, 
     xlab = "avgWage",
     ylab = "count",
     col = plasma(40),
     breaks = 100,
     border = "black", 
     xlim = c(0, 5000))

#2

filtered = data1 %>%
  group_by(code) %>%
  summarise(suma = sum(avgWage)) %>% 
  arrange(desc(suma)) %>% head(5)

merged = merge(filtered, data1, by = "code")
ggplot(merged, aes(x=month, y=avgWage, group = name, color = name)) +
  geom_line() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

#3


group_by(merged, name) %>%
  summarise(maxNumInsured = max(numInsured)) %>%
  ggplot(aes(x=reorder(name, -maxNumInsured), y=maxNumInsured, group = name, 
             fill=name )) +
  geom_bar(stat="identity") +
  xlab("name") +
  ylab("apdraustieji") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
