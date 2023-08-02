#Establecer el area de trabajo

setwd("G:/clases/UCC/2023_ucc_estad/Semana 31 agosto")
library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(glmm.hp)

ed <- read_excel("ed_gen.xlsx")
View(ed)
names(ed)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(ed)
pc<-data.frame(Femenina, Masculino)

# p > 0.05, nos indica una distribución normal

shapiro.test(Femenina) 
shapiro.test(Masculino) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(ed, aes(Masculino)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(ed, aes(log(Masculino)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(ed, aes(sqrt(Masculino)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

f=ed$Femenina
m=ed$Masculino

boxplot(f,m,
        main = "Edad",
        names = c("f", "m"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(log(Femenina), log(Masculino),alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Femenina, Masculino, paired = TRUE);bf

# un puntaje mayor de 10 se dice que la evidencia es anecdotical en favor de la Ho
# Haremos un gr?fico para ver estos dos promedios

### OTro ejemplo

ph <- read_excel("phi.xlsx")
View(ph)
names(ph)

# agregar la funciónn attach, Permite referenciar los nombres de las columnas de los data.frames

attach(ph)
ph1<-data.frame(Fem, Ma)

# p > 0.05, nos indica una distribución normal

shapiro.test(Fem) 
shapiro.test(Ma) 

# un p < 0.05 indica que se acepta la asunción que existen diferencias o sea que la variables no son homg?neas

var.test(Femenina, Masculino)

g1<-ggplot(ph1, aes(Fem)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g2<-ggplot(ph1, aes(log(Fem)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g3<-ggplot(ph1, aes(sqrt(Fem)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

g1
g2
g3

ggarrange(g1, g2, g3, ncol = 2, nrow = 2)

f1=ph1$Fem
m1=ph1$Ma

boxplot(f1,m1,
        main = "Phi",
        names = c("f1", "m1"),
        las = 1,
        col = c("orange","red"),
        border = "brown",
        Vertical = TRUE,
        notch = FALSE)

#podemos observar que los datos no cumplen normalidad


t.test(log(Fem), log(Ma),alternative="two.sided",var.equal=F) 


#Haremos una an?lisis bayesiano

install.packages("BayesFactor")
library(BayesFactor)

?BayesFactor
bf = ttestBF(Fem, Ma, paired = TRUE);bf

# un puntaje mayor de 10 se dice que la evidencia es anecdotical en favor de la Ho
# Haremos un gr?fico para ver estos dos promedios
