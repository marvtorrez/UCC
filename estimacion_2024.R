setwd("G:/clases/UCC/2024_ucc_estad/Unidad III")

library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(rriskDistributions)

p<-read_excel("conteo_potreros.xlsx")
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
fit.cont(d1)

# Altura total


n <- length(p$cantidad); n 
media<-mean(p$cantidad);b
desv<-sd(p$cantidad);c
nivelconfianza = 0.95

error.est <- desv/sqrt(n) 
error.est
margen.error <- 1.960 * error.est 
margen.error

lim.inf <-media - margen.error;lim.inf

lim.sup <-media + margen.error;lim.sup


### para el alto



