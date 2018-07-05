# SemiPar 13.03.2017r.

rm(list=ls())


# PACKAGES
# install.packages("C:/Users/Michal/Dropbox/01 Studia/01 Szko³a - projekty/20 SemiPar/HRW_0.1.zip", repos = NULL)
library(HRW, quietly = TRUE)
# install.packages("ggplot2")
library(ggplot2, quietly = TRUE)
library(mgcv, quietly = TRUE)

# LAB
data(WarsawApts)
x = WarsawApts$construction.date
y = WarsawApts$areaPerMzloty
plot(x,y,col= "dodgerblue")

penSplFit1 = smooth.spline(x, y, spar=1.1)
lines(penSplFit1, col="magenta")

penSplFit2 = smooth.spline(x, y, spar=0.1)
lines(penSplFit2, col="red")

penSplFit3 = smooth.spline(x, y, spar=0.5)
lines(penSplFit3, col="green3", lwd = 5)

penSplFitGCV=smooth.spline(x, y)
plot(x, y, col="dodgerblue")
lines(penSplFitGCV, col="darkgreen")

penSplFitGCVmgcv = gam(y~s(x, k=50))
plot(penSplFitGCV)

shiftAmt =  mean(fitted(penSplFitGCVmgcv))
plot(penSplFitGCVmgcv, shift=shiftAmt, ylim=range(y-shiftAmt))
points(x, y, col="dodgerblue")

fitgamDflt = gam(y~s(x))

gam.check(fitgamDflt)

fitBiggerBasis = gam(y~s(x, k=30))
gam.check(fitBiggerBasis)

gam.check(fitBiggerBasis)

fitBiggerBasis = gam(y~s(x, k=30))
summary(fitBiggerBasis)$edf

par(mfrow=c(2,2))
fitLowEDF = smooth.spline(x, y, df=2.5)
plot(x, y, col="dodgerblue", main="EDF = 2.5")
lines(fitLowEDF, col="darkgreen")
fitHighEDF = smooth.spline(x, y, df=50)
plot(x, y, col="dodgerblue", main="EDF = 50")
lines(fitHighEDF, col="darkgreen")
fitGCV=smooth.spline(x, y, df=14.087)
plot(x, y, col="dodgerblue", main="EDF = 14.087")
lines(fitGCV, col="darkgreen")





