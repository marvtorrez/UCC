setwd("G:/clases/UCC/2024_ucc_estad")

library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(rriskDistributions)

alt<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/tabla_alt.csv")

names(alt)
View(alt)

altDF<-data.frame(alt)

gen <- split(altDF, altDF$gen)

f<-gen$FEMENINO
m<-gen$MASCULINO

fit.cont(m$ombligo)

# Prueba de shapiro 

shapiro.test(alt$ombligo)

# si p es menor de 0.05 se dice que la distribución no es normal

t1 <- t.test(ombligo ~ gen, data=alt)
t1

t1i <- lm(ombligo ~ as.factor(gen), data=alt)
summary(t1i)

plot(t1i, which=1)
plot(t1i, which=2)

boxplot(ombligo ~ gen, data=alt,main='Alto',ylab='Metros', xlab = 'Género')


##patologías

pat<-read_excel("patologias.xlsx")
names(pat)

d2=pat$Patologias

fit.cont(d2)

g1<-ggplot(pat, aes(Patologias)) +
  geom_histogram(aes(y =..density..), fill = "blue") +
  geom_density()

g2<-ggplot(pat, aes(log(Patologias)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(pat, aes(sqrt(Patologias)))+
  geom_histogram(aes(y =..density..), fill = "green") +
  geom_density()

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)


t2<-t.test(Patologias ~ Rango_Edad, data=pat)
t2

t2i <- lm(Patologias ~ as.factor(Rango_Edad), data=pat)
summary(t2i)

plot(t2i, which=1)
plot(t2i, which=2)

boxplot(Patologias ~ Rango_Edad, data=pat,main='Enfermedades',ylab='# patologías', xlab = 'Años')

p_1<- ddply(pat, "Rango_Edad", summarise, Pro_pa=mean(Patologias), sd_pa=sd(Patologias), se_pa=sd(Patologias)/sqrt(length(Patologias)))
p_1


pat_pro<-ggplot(p_1, aes(x = Rango_Edad, y = Pro_pa))+ geom_bar(stat = "identity", width=0.2,fill='steelblue',col='black')+
    labs(x = 'Rango Edad', y = '# patologías', title = 'Enfermedades')
  
pat_pro

pat_pro1 <- pat_pro+geom_errorbar(aes(ymin=Pro_pa-se_pa, ymax=Pro_pa+se_pa), width=0.1)
pat_pro1
