setwd("G:/clases/UCC/2023_ucc_estad/Semana IV")

install.packages("")
library(rriskDistributions)
library(mgcv)
library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)


pr2<-read_excel("ejerc.xlsx")
View(pr2)
names(pr2)

bx=pr2$Si

bx

fit.cont(bx)

fig1<-ggplot(pr2, aes(Si)) +
  geom_histogram(aes(y =..density..), fill = "pink") +
  geom_density()

fig1




