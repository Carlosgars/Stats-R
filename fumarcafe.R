inf=read.table(file="fumarcafe(1).txt")
head(inf)
names(inf)
attach(inf)
dim(inf)
summary(inf)
class(infarto)
contrasts(infarto)
contrasts(cafe)
contrasts(tabaco)
tabaco = relevel(tabaco,ref="No fumador")
tablafum=table(infarto,tabaco)
tablafum
100*prop.table(tablafum)
fisher.test(tablafum)
fisher.test(tablafum,alternative="greater")
r.fum=glm(infarto~tabaco,family = "binomial")
summary(r.fum)
exp(coef(r.fum))
IC.fum = confint(r.fum)
exp(IC.fum)
tablacafe=table(infarto,cafe)
tablacafe
100*prop.table(tablacafe)
fisher.test(tablacafe)
fisher.test(tablacafe,alternative="greater")
r.cafe=glm(infarto~cafe,family = "binomial")
summary(r.cafe)
coef(r.cafe)
exp(coef(r.fum))
IC.cafe = confint(r.cafe)
exp(IC.cafe)
todas=glm(infarto~cafe+tabaco,family = "binomial")
summary(todas)
coef(todas)
exp(coef(todas))
IC.todas = confint(todas)
exp(IC.todas)

t3=table(tabaco,cafe)
t3
100*prop.table(t3)
fisher.test(t3)
fisher.test(t3,alternative="greater")

nulo=glm(infarto~1,family = "binomial")
step(nulo,direction="forward",scope=list(lower=nulo,upper=todas))


inf_si=subset(inf,tabaco=="fumador")
summary(inf_si)
