data=read.dta13("guns.dta")
rm(list=ls(all=TRUE))
library(data.table)
library(foreign)
library(plm)

context<-read.dta("guns.dta")
summary(context)
hist(context$vio)
hist(context$rob)
hist(context$incarc_rate)
hist(context$mur)


###Vio
model1 <- lm(log(vio) ~ shall, data = context)
summary(model1)

model2 <- lm(log(vio) ~ shall + log(incarc_rate) + log(density) + avginc +
               log(pop) + pb1064 + pw1064 + pm1029, data = context)
summary(model2)

model3 <- plm(log(vio) ~ shall + log(incarc_rate) + log(density) + avginc +
               log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "pooling",data = context)
summary(model3)

model4 <- plm(log(vio) ~ shall + log(incarc_rate) + log(density) + avginc + I(avginc*avginc)+
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "within",data = context)
summary(model4)
coeftest(model4, vcovHC)

model5 <- plm(log(vio) ~ shall + factor(year) + log(incarc_rate) + log(density) + avginc +
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "within",data = context)
summary(model5)
coeftest(model5, vcovHC)

pFtest(model5, model4)


############mur
model6 <- lm(log(mur) ~ shall, data = context)
summary(model6)

model7 <- lm(log(mur) ~ shall + log(incarc_rate) + log(density) + avginc +
               log(pop) + pb1064 + pw1064 + pm1029, data = context)
summary(model7)

model8 <- plm(log(mur) ~ shall + log(incarc_rate) + log(density) + avginc +
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "pooling",data = context)
summary(model8)

model9 <- plm(log(mur) ~ shall + log(incarc_rate) + log(density) + avginc + I(avginc*avginc)+
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "within",data = context)
summary(model9)
coeftest(model9, vcovHC)

model10 <- plm(log(mur) ~ shall + factor(year) + log(incarc_rate) + log(density) + avginc +
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "within",data = context)
summary(model10)
coeftest(model10, vcovHC)

pFtest(model10, model9)

####rob
model11 <- lm(log(mur) ~ shall, data = context)
summary(model11)

model12 <- lm(log(mur) ~ shall + log(incarc_rate) + log(density) + avginc +
               log(pop) + pb1064 + pw1064 + pm1029, data = context)
summary(model12)

model13 <- plm(log(mur) ~ shall + log(incarc_rate) + log(density) + avginc +
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "pooling",data = context)
summary(model13)

model14 <- plm(log(mur) ~ shall + log(incarc_rate) + log(density) + avginc + I(avginc*avginc)+
                log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "within",data = context)
summary(model4)
coeftest(model4, vcovHC)

model15 <- plm(log(mur) ~ shall + factor(year) + log(incarc_rate) + log(density) + avginc +
                 log(pop) + pb1064 + pw1064 + pm1029, index=c("stateid","year"),model = "within",data = context)
summary(model15)
coeftest(model15, vcovHC)

pFtest(model15, model14)
