library(car)
require(car)

# Leemos el archivo ciga.txt

ciga=read.table("ciga.txt",header=T,dec = ".")
attach(ciga)
names(ciga)

# Estudio descriptivo y relaciones entre variables
summary(ciga)
var(ciga)
cor(ciga) # no funciona bien pues tenemos una columna de strings
nombre
# Eliminamos la primera columna
ciga2 = ciga[,2:5]
ciga2
var(ciga2)
cor(ciga2)
heatmap(cor(ciga2), scale="none") # Vemos que la variable más correlacionada con "monoxcar" es "alquitra"
boxplot(alquitra,nicotina,peso,monoxcar,col="blue", names=c("alq", "nic", "peso","mono"))
pairs(ciga2)


# Normalidad de monoxcar mediante S-W
hist(monoxcar)
qqnorm(monoxcar)
qqline(monoxcar)
shapiro.test(monoxcar) # p-valor grande
# Podemos suponer que la variable monoxcar sigue una distribución normal

# Normalidad de nicotina (no sigue una normal)
hist(nicotina)
qqnorm(nicotina)
qqline(nicotina)
shapiro.test(nicotina) # p-valor pequeño
# No podemos suponer que la variable nicotina sigue una distribución normal


# Normalidad de alquitra (no sigue una normal)
hist(alquitra)
qqnorm(alquitra)
qqline(alquitra)
shapiro.test(alquitra) # p-valor pequeño
# No podemos suponer que la variable alquitra sigue una distribución normal


# Normalidad de peso
hist(peso)
qqnorm(peso)
qqline(peso)
shapiro.test(peso) # p-valor suficientemente grande
# Podemos suponer que la variable peso sigue una distribución normal

# Gráficas de las relaciones entre variables
plot(nicotina,monoxcar)
plot(alquitra,monoxcar)
plot(peso,monoxcar)
plot(nicotina,alquitra)

r0=lm(nicotina~alquitra)
r0

# Regresión lineal de monoxcar en funcion de nicotina
r1=lm(monoxcar~nicotina)
r1
summary(r1)
shapiro.test(resid(r1)) # p-valor grande, aceptamos que los residuos siguen una normal
plot(r1)

#Homocedasteceidad, todavia no he descargado la libreria car, ncv.test(r1) 


# Regresión lineal de monoxcar en funcion de alquitra
r2=lm(monoxcar~alquitra)
r2
shapiro.test(resid(r2)) # p-valor grande, aceptamos que los residuos siguen una normal
plot(r2)


# Regresión lineal múltiple de monoxcar en funcion del resto de variables
rn=lm(monoxcar~alquitra+nicotina+peso)
shapiro.test(resid(rn)) # p-valor grande, aceptamos que los residuos siguen una normal
plot(rn)
anova(rn) 
summary(rn)
rn

# Regresión lineal múltiple de monoxcar en función del alquitrán y la nicotina
r3=lm(monoxcar~alquitra+nicotina)
r3

# Intervalo de confianza de los parámetros del modelo
confint(r3,level=0.95)


mean(resid(r1))
mean(resid(r2))
mean(resid(rn))
mean(resid(r3))


# Podemos concluir que la variable peso apenas tiene relevancia sobre la variable monoxcar.

# Vemos que el nivel de significación es mayor en la variable explicativa "alquitra", que ya habíamos visto
# que tiene mayor importancia sobre la variable respuesta "monoxcar"
# Además vemos que el coeficiente que obtenemos en la regresión es positivo, (por cada tipo de tabaco 
# con los mismos valores de nicotina y peso, por cada unidad que aumente el alquitran aumentará el monóxido 
# de carbono en 0.9 unidades)
# Por tanto, podemos concluir que a más alquitrán, más monóxido de carbono 
# El coeficiente de la nicotina es de -2.6, luego por cada unidad que aumente la nicotina a mismo 
# alquitran y peso, el monoxcar dismunuirá en 2.6 unidades. Debemos tener en cuenta que los valores de la 
# nicotina están entre 0 y 2, mientras que los del alquitrán son unas 10 veces mayores. Esto explica 
# que a pesar de los coeficientes el alquitrán tenga un mayor nivel de significación.

# Se da una fuerte relación lineal directa entre nicotina y alquitrán. 

# También se da una relación lineal directa entre monóxido y nicotina

# Conclusión final
# La variable que tiene mayor importancia sobre el monóxido de carbono es el alquitrán, con una relación
# directa.


