install.packages("ggplot2")

library(readr)
library(ggplot2)
library(plyr)
library(dplyr)


m<-2

m

m1<-6+m

m1

m2<-m1+m

m2

d1<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/pract1.csv")

names(d1)
View(d1)


#Análisis de tabla d1, 

hist(d1$patologias)


a1<-ggplot(d1, aes(edad)) +
  geom_histogram(aes(y =..density..), fill = "#4682B4") +
  geom_density()
a1

a2 <- ggplot(d1, aes(patologias)) +
  geom_histogram(aes(y =..density..), fill = "#4682B4") +
  geom_density();a2

ggplot(d1, aes(x = edad)) +
  geom_histogram()

ggplot(d1, aes(x = edad)) +
  geom_histogram(binwidth = 3)

a3 <- ggplot(d1, aes(x = edad)) +
  geom_histogram(binwidth = 0.9, color = "black", fill = "#4682B4")

a3

# medidas de tendencia central

library(plyr)

mean(d1$edad)

r_1<- ddply(d1, "Departamento", summarise, mean_edad=mean(edad), sd_edad=sd(edad), se_edad=sd(edad)/sqrt(length(edad)))
r_1

## PRACTICA:  Repetir lo anterior con las patologías

# otros gráficos, gráfico 1 graficos de barras

ggplot(d1, aes(x = Departamento, y = edad)) + geom_point()

ggplot(d1, aes(x = sexo, y = edad)) + geom_point() + facet_grid(~sexo)

a3<-ggplot(r_1, aes(x = Departamento, y = mean_edad)) + geom_bar(stat = "identity")

a3

ggplot(r_1, aes(x = Departamento, y = mean_edad)) +
  geom_bar(stat = "identity", width=0.5,fill='lightblue',col='black') +
  labs(x = 'Departamentos', y = 'Edad (Años)', title = 'Departamento Edad') +
    theme_minimal() +  
  theme(panel.grid = element_blank())



a4<-ggplot(d1,aes(Departamento,edad)) + geom_boxplot()
a4



# analsis de tabla d2

d2<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/tabla_alt.csv")

names(d2)
View(d1)

hist(d2$Atractivo)