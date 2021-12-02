carsalesRAW=read.table("car_sales.txt",header=T,dec = ".")
carsales=na.omit(carsalesRAW)
carsales2=carsales[,-c(1,2,5)]
carsales2=na.omit(carsales2)
names(carsales2)
attach(carsales2)
c = cor(carsales2)
c
heatmap(c, scale="none")

# APARTADO A
# Deducimos a partir del mapa de calor y la matriz de covarianzas que las variables explicativas con   
# mayor relación lineal  para la variable respuesta (reventa) son caballos y precio, por tanto 
# deducimos que a partir de ambas podremos obtener un buen modelo lineal para predecir la variable 
# reventa. (Antes debemos ver la colinealidad entre estas dos variables)

# APARTADO B
# Variables fuertemente relacionadas: Longitud-batalla (0.853) Caballos-Motor (0.86)
# Gracias a la tabla de correlaciones observamos que las parejas de variables explicativas con mayor 
# relación lineal son Longitud-Batalla y Caballos-motor. 
# Que existan variables explicativas tan correlacionadas implica que pueda existir el problema de 
# multicolinealidad. (habría que ver los autovectores y el determinante de la matriz cov)

# APARTADO C 
carsales3 = carsales[,-c(1,2)]
attach(carsales3)
tipo
r1 = lm(reventa~tipo)
plot(r1)
summary(r1)

# Planteamos un modelo reventa = b0g + b1g * tipo
# b0gorro = 0 
# b1gorro = 0 (pvalor ~ ) 
plot(tipo, reventa)