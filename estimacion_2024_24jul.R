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
View(alt)

boxplot(Dif~Rango_atrac, data=alt,main='Atractivo_por dif',ylab='Dif')


alt_df<-data.frame(alt)

gen <- split(alt_df, alt_df$gen)
View(gen)

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
mediaf<-mean(f$Alto);mediaf
desvf<-sd(f$Alto);desvf
nivelconfianza = 0.95

error.estf <- desvf/sqrt(nf) 
error.estf
margen.errorf <- 1.960 * error.estf 
margen.errorf

lim.inf_f <-mediaf - margen.errorf;lim.inf_f

lim.sup_f <-mediaf + margen.errorf;lim.sup_f


nm <- length(m$Alto); nm
mediam<-mean(m$Alto);mediam
desvm<-sd(m$Alto);desvm
nivelconfianza = 0.95

error.estm <- desvm/sqrt(nm) 
error.estm
margen.errorm <- 1.960 * error.estm 
margen.errorm

lim.inf_m <-mediam - margen.errorm;lim.inf_m

lim.sup_m <-mediam + margen.errorm;lim.sup_m

boxplot(Alto~Rango_atrac, data=f,main='Alto Feminino',ylab='Género')


#  Ombligo

alt_df<-data.frame(alt)

gen <- split(alt_df, alt_df$gen)

f<-gen$FEMENINO
m<-gen$MASCULINO

names(alt_df)
nf1 <- length(f$ombligo); nf1 
mediaf1<-mean(f$ombligo);mediaf1
desvf1<-sd(f$ombligo);desvf1
nivelconfianza = 0.95

error.estf1 <- desvf1/sqrt(nf1) 
error.estf1
margen.errorf1 <- 1.960 * error.estf1 
margen.errorf1

lim.inf_f1 <-mediaf1 - margen.errorf1;lim.inf_f1

lim.sup_f1 <-mediaf1 + margen.errorf1;lim.sup_f1


nm1 <- length(m$ombligo); nm1
mediam1<-mean(m$ombligo);mediam1
desvm1<-sd(m$ombligo);desvm1
nivelconfianza = 0.95

error.estm1 <- desvm1/sqrt(nm1) 
error.estm1
margen.errorm1 <- 1.960 * error.estm1 
margen.errorm1

lim.inf_m1 <-mediam1 - margen.errorm1;lim.inf_m1

lim.sup_m1 <-mediam1 + margen.errorm1;lim.sup_m1

# Atractivo



### Patologías ###

pot <- read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/conteo_potreros.csv")
names(pot)
View(pot)
