## Sacar tamaño muesral

#Zα/2   Nivel de confianza	
# 1.595  89%
# 1.645  90%
# 1.755  92%
# 1.885  94%
# 1.960  95%
# 2.170  97%
# 2.325  98%
# 2.575  99%

# p = es la proporción que presenta el atributo
# q = su complementario

# e2 = es el error máximo admisible, en tanto por ciento, 
# cuando se desconoce su valor, entonces el investigador fija un 
# criterio que puede variar entre el 1% (0.01) y 9% (0.09)
# N = Tamaño de la población

# n = Z2α/2Npqe2(N−1)+Z2α/2pq

# al 95%,  

Z=1.960;p=0.5;q=1-p;N=500;e=0.05
n=(Z^2*N*p*q)/(e^2*(N-1)+Z^2*p*q)
n
