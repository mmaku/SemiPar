rm(list=ls())

library(HRW)
# library(mgcv)
library(mgcv)
data(BostonMortgages)

summary(BostonMortgages)
str(BostonMortgages)

deny.glm.first <- glm(deny ~ black,
                      family = binomial,
                      data = BostonMortgages)
summary(deny.glm.first)

deny.glm <- glm(deny ~ black + dir + lvr + pbcr
                + self + single + as.factor(ccs),
                family = binomial, data = BostonMortgages)
summary(deny.glm)

deny.smth <- gam(deny ~ black + s(dir) + s(lvr)
                 + pbcr + self + single + as.factor(ccs),
                 family = binomial,
                 method = "REML", data = BostonMortgages)
summary(deny.smth)

opar <- par(mfrow = c(2, 2), bty = "l", lwd = 2)

plot(deny.smth, shade = TRUE, shade.col = "palegreen",
     trans = plogis, rug = TRUE, xlim = c(0, 1), select = 1,
     ylab = "probability of denial",
     xlab = "debt payment to income ratio")
plot(deny.smth, shade = TRUE, shade.col = "palegreen",
     select = 1, rug = TRUE, xlim = c(0, 1),
     ylab = "logit of denial",
     xlab = "debt payment to income ratio" )
plot(deny.smth, shade = TRUE, shade.col = "palegreen",
     trans = plogis, rug = TRUE, xlim = c(0, 1), select = 2,
     ylab = "probability of denial",
     xlab = "loan size to assessed value")
plot(deny.smth, shade = TRUE, shade.col = "palegreen",
     rug = TRUE, xlim = c(0, 1), select = 2,
     ylab = "logit of denial",
     xlab = "loan size to assessed value" )
par(opar)

anova(deny.glm, deny.smth, test="Chisq")


detach(package:mgcv)
# install.packages("gam")
library(gam)
# install.packages("Ecdat")
library(Ecdat)

data(Caschool)

summary(Caschool)
str(Caschool)

fitInitial <- gam::gam(mathscr ~ calwpct + mealpct
                  + compstu + expnstu + str
                  + log(avginc) + elpct,data = Caschool)

stepFit <- step.gam(fitInitial,
                    scope = list("calwpct" = ~ 1 + calwpct + s(calwpct,2),
                                 "mealpct" = ~ 1 + mealpct + s(mealpct,2),
                                 "compstu" = ~ 1 + compstu + s(compstu,2),
                                 "expnstu" = ~ 1 + expnstu + s(expnstu,2),
                                 "str" = ~ 1 + str + s(str,2),
                                 "log.avginc" = ~ 1 + log(avginc)
                                 + s(log(avginc),2),
                                 "elpct" = ~ 1 + elpct + s(elpct,2)))

print(names(stepFit$"model")[-1])

detach(package:gam)
library(mgcv)

fitStepCaschool <- gam(mathscr ~ mealpct + elpct
                       + s(calwpct,k = 27) + s(compstu,k = 27)
                       + s(log(avginc),k =27),data = Caschool)

gam.check(fitStepCaschool)


