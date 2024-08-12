library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(rriskDistributions)


## 

alt<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/tabla_alt.csv")
names(alt)

a1<-ggplot(alt, aes(Phi)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a2<-ggplot(alt, aes(log(Phi)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a3<-ggplot(alt, aes(sqrt(Phi)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

ggarrange(a1, a2, a3, ncol = 2, nrow = 2)

m1<-lm(Phi~as.factor(Rango_atrac), data = alt)
summary(m1)

aovm1<-aov(m1)
aovm1

plot(m1, which=1)
plot(m1, which=2)

TukeyHSD(aovm1, conf.level=.95)

plot(TukeyHSD(aovm1, conf.level=.95), las = 2)

library(agricolae)

tukey.test2 <- HSD.test(aovm1, trt = 'as.factor(Rango_atrac')
tukey.test2

boxplot(Phi~Rango_atrac, data=alt,main='Simetria',xlab='Phi',ylab='Género')


#