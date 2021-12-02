### Carlos Garcia Sancho ###
### Práctica 3 Car Sales ###

carsalesRAW=read.table("car_sales.txt",header=T,dec = ".")
carsales=na.omit(carsalesRAW)
carsales2=carsales[,-c(1,2,5)]
carsales2=na.omit(carsales2)
names(carsales2)
attach(carsales2)
summary(carsales2)

#####################
## APARTADOS A Y B ##
#####################

c = cor(carsales2)
c

det(c)

heatmap(c, scale="none")

################
## APARTADO C ##
################

carsales3 = carsales[,-c(1,2)]
attach(carsales3)
tipo
class(tipo)
levels(tipo)
contrasts(tipo)

r1 = lm(reventa~tipo, data = carsales)
plot(r1)
summary(r1)
shapiro.test(residuals(r1))
# No hay normalidad en los residuos (p valor ~ 0)

reventa.auto=carsales[tipo=="Automóvil","reventa"]
summary(reventa.auto)
reventa.camion=carsales[tipo=="Camión","reventa"]
summary(reventa.camion)
boxplot(reventa.auto, reventa.camion, names=c("auto", "camion"), col = "lightblue")

################
## APARTADO D ##
################

todas.car.lm = lm(reventa ~ ventas+tipo+precio+motor_s+caballos+batalla+anchura+longitud+peso_revestimiento
        +tapón_combustible+kpl, data = carsales)
todas.car.lm
summary(todas.car.lm)
plot(todas.car.lm)
shapiro.test(residuals(todas.car.lm))
# Hay normalidad en los residuos (aunque el p valor = 0.28 pequeño)


#################
## APARTADO E ##
################

# Estrategia 1: Selección de variables por p-valor

# Quitamos aquellas variables cuyos contrasten tienen un p-valor mayor.

car.pvalor.lm = lm(reventa ~ precio + motor_s + longitud + peso_revestimiento + tapón_combustible, data = carsales)
summary(car.pvalor.lm)

car.pvalor.lm = update(car.pvalor.lm,
                   lm(reventa ~ precio + longitud + peso_revestimiento + tapón_combustible, data = carsales))
summary(car.pvalor.lm)
plot(car.pvalor.lm)
shapiro.test(residuals(car.pvalor.lm))
# Toma como variables explicativas precio, longitud, peso_revestimiento, tapón_combustible.
# Hay normalidad en los residuos (aunque p valor = 0.1121)

# Estrategia 2: Regresión paso a paso
# Hacia atrás y hacia delante
step(car.lm,direction = "both")
#reventa ~ precio + motor_s + longitud + peso_revestimiento + tapón_combustible + kpl
# Toma como variables explicativas precio, motor_s, longitud, peso_revestimiento, tapón_combustible, kpl
car.stepbnf.lm = lm(reventa ~ precio + motor_s + longitud + peso_revestimiento + tapón_combustible + kpl, data = carsales)
summary(car.stepbnf.lm)
plot(car.stepbnf.lm)
shapiro.test(residuals(car.stepbnf.lm)) 
# p valor = 0,12, no podemos rechazar normalidad en los residuos

# Hacia atrás
step(car.lm,direction = "backward")
# reventa ~ precio + motor_s + longitud + peso_revestimiento + tapón_combustible + kpl
# Igual que back and forward

# Hacia delante
nulo=lm(reventa~1)
step(nulo,direction = "forward",
     scope=list(lower=nulo, upper=car.lm))
# precio + longitud + peso_revestimiento + tapón_combustible
car.stepforward.lm = lm(reventa ~ precio + longitud + peso_revestimiento + tapón_combustible, data = carsales)
summary(car.stepforward.lm)

## Estrategia 3: Mejor selección de subconjuntos
library(leaps)
car.subsets=regsubsets(reventa~ ventas+tipo+precio+motor_s+caballos+batalla+anchura+longitud+peso_revestimiento
                       +tapón_combustible+kpl ,data=carsales)
summary(car.subsets)
summary(car.subsets)$rsq


# precio,motor_s,caballos,batalla,longitud,peso_revestimiento,tapón_combustible,kpl
car.subset.rsq = lm(reventa ~ precio+motor_s+caballos+batalla+longitud+peso_revestimiento+tapón_combustible+kpl, data = carsales)

summary(car.subsets)
summary(car.subsets)$bic
# bic = -310.0438 {precio,longitud,peso_revestimiento,tapón_combustible}
car.subset.bic = lm(reventa ~ precio+longitud+peso_revestimiento+tapón_combustible,data=carsales)

## Comparamos brevemente los modelos  
# Por Bondad de ajuste
summary(car.lm)             # Multiple R-squared:  0.9472,	Adjusted R-squared:  0.9417 
summary(car.pvalor.lm)      # Multiple R-squared:  0.9441,	Adjusted R-squared:  0.9421 
summary(car.stepbnf.lm)     # Multiple R-squared:  0.946,	  Adjusted R-squared:  0.9431 
summary(car.stepforward.lm) # Multiple R-squared:  0.9441,	Adjusted R-squared:  0.9421 
summary(car.subset.rsq)     # Multiple R-squared:  0.9468,	Adjusted R-squared:  0.9429 

# Por BIC
BIC(car.lm)                 # 622.3848
BIC(car.pvalor.lm)          # 599.2804
BIC(car.stepbnf.lm)         # 601.2307
BIC(car.stepforward.lm)     # 595.8155
BIC(car.subset.rsq)         # 608.8155


################
## APARTADO F ##
################

summary(car.stepforward.lm)


############################
## APARTADO G. PREDICCIÓN ##
############################

predict(car.stepforward.lm,newdata=data.frame(precio=(mean(precio)),longitud=mean(longitud),
                                              peso_revestimiento=mean(peso_revestimiento),
                                              tapón_combustible=mean(tapón_combustible)),interval="confidence")
mean(reventa)


################
## APARTADO H ##
################

summary(car.subsets)

# precio, peso_revestimiento, tapón_combustible

tres.subset.lm = lm(reventa~precio,peso_revestimiento,tapón_combustible,data=carsales)
tres.subset.lm
summary(tres.subset.lm)
plot(tres.subset.lm)




summary(car.lm)                 
summary(car.pvalor.lm)          
summary(car.stepbnf.lm)         
summary(car.stepforward.lm)    
summary(car.subset.rsq) 

shapiro.test(residuals(car.stepforward.lm
                       ))


