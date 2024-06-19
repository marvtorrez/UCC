library(readr)
library(ggplot2)
library(dplyr)
library(plyr)

d1<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/pract1.csv")

names(d1)
View(d1)
names(d2)

#Análisis de tabla d1, 

hist(d1$edad)


a1<-ggplot(d1, aes(edad)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density();a1

a2 <- ggplot(d1, aes(patologias)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density();a2

ggplot(d1, aes(x = edad)) +
  geom_histogram()

ggplot(d1, aes(x = edad)) +
  geom_histogram(binwidth = 0.5)

ggplot(d1, aes(x = edad)) +
  geom_histogram(binwidth = 0.5, color = "white", fill = "blue")


# medidas de tendencia central



res <- ddply(d1, c("Departamento"), summarise, sample_size=length(Departamento), mean_Pre=mean(edad), sd_Pre=sd(edad), se_Pre=sd(edad)/sqrt(length(Departamento)))
res

# analsis de tabla d2


