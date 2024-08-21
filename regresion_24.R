library(dplyr)
library(plyr)
library(readxl)
library(ggplot2)
library(ggpubr)
library(rriskDistributions)

alt<-read.csv("https://raw.githubusercontent.com/marvtorrez/UCC/main/tabla_alt.csv")
names(alt)
View(alt)

alt1<-data.frame(alt)

alt2 <- select(alt1, -esT, -No, -gen, -Rango_atrac)

library(GGally)

ggpairs(alt2, lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")


m1 <- lm(Atractivo~Edad+Alto+ombligo+Dif+Phi+cat_gen, data = alt2)
summary(m1)

# El resultado del modelo tiene un R cuadrado multiple (Multiple R-squared) de 0.1802
# lo que significa que apenas explica el 18%  de ka variabidad del atractivo debido a 
# los facotres biometricos medidos

m2 <- lm(log(Atractivo)~Edad+Alto+ombligo+Dif+Phi, data = alt2)
summary(m2)

plot(m2, which=1)
plot(m2, which=2)

library(lmtest)
bptest(m2)


# La prueba de Breusch-Pagan estipula que si p<0.05 las varianzas son distintas
# lo que se dice heterocedásticas

step(object = m2, direction = "both", trace = 1)

# el modelo resultante es: lm(log(Atractivo)~Edad+Alto+ombligo+Phi

confint(lm(formula =Atractivo~Edad+Alto+ombligo+Phi, data = alt2))

library(gridExtra)

plot1 <- ggplot(data = alt2, aes(Edad, m2$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = alt2, aes(Alto, m2$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = alt2, aes(ombligo, m2$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = alt2, aes(Phi, m2$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()

grid.arrange(plot1, plot2, plot3, plot4)

# Podemos ver que la linealidad está comprometida