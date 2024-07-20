setwd("G:/clases/UCC/2024_ucc_estad")

install.packages("ggpubr")

library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(rriskDistributions)

p<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/conteo_potreros.csv")

names(p)
View(p)


a1<-ggplot(p, aes(cantidad)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a2<-ggplot(p, aes(log(cantidad)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a3<-ggplot(p, aes(sqrt(cantidad)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

ggarrange(a1, a2, a3, ncol = 2, nrow = 2)

# veremos dsitribuciones totales

d1=p$cantidad
View(d1)
fit.cont(d1)

# total


n <- length(p$cantidad); n 
media<-mean(p$cantidad);media
desv<-sd(p$cantidad);desv
nivelconfianza = 0.95

error.est <- desv/sqrt(n) 
error.est
margen.error <- 1.960 * error.est 
margen.error

lim.inf <-media - margen.error;lim.inf

lim.sup <-media + margen.error;lim.sup


### para el alto por separación de tratamientos

alt<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/tabla_alt.csv")
names(alt)

boxplot(Alto~Rango_atrac, data=alt,main='Alto',ylab='Género')


alt_df<-data.frame(alt)
names
gen <- split(alt_df, alt_df$gen)

f_alt<-ggplot(gen$FEMENINO, aes(Alto)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

f_alt

m_alt<-ggplot(gen$MASCULINO, aes(Alto)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

m_alt

ggarrange(f_alt,m_alt, ncol = 2, nrow = 1)


f<-gen$FEMENINO
m<-gen$MASCULINO
View(f)

nf <- length(f$Alto); n 
mediaf<-mean(f$Alto);media
desvf<-sd(f$Alto);desv
nivelconfianza = 0.95

error.estf <- desv/sqrt(n) 
error.estf
margen.errorf <- 1.960 * error.est 
margen.errorf

lim.inf_f <-media - margen.error;lim.inf

lim.sup_f <-media + margen.error;lim.sup


nm <- length(m$Alto); nm
mediam<-mean(m$Alto);mediam
desvm<-sd(m$Alto);desvm
nivelconfianza = 0.95

error.estm <- desv/sqrt(n) 
error.estm
margen.errorm <- 1.960 * error.est 
margen.errorm

lim.inf_m <-mediam - margen.error;lim.inf_m

lim.sup_m <-mediam + margen.error;lim.sup_m

boxplot(Alto~Rango_atrac, data=f,main='Alto Feminino',ylab='Género')


#  Ombligo




# Atractivo