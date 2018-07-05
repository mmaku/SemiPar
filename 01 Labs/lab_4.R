# Exercise 1

rm(list=ls())

library(HRW)
library(lattice)
library(mgcv)
# install.packages("fields")
library(fields)

data(ozoneSub)

x <- ozoneSub$longitude
y <- ozoneSub$latitude
z <- ozoneSub$ozone
cloud(z ~ x * y, xlab = "longitude", ylab = "latitude", zlab = "ozone")

fit <- gam(z ~ s(x,y))
plot(fit)
points(x,y,col = "dodgerblue")

gam.check(fit)

fitHighBasis <- gam(z ~ s(x,y,k = 60))
summary(fitHighBasis)

plot(fitHighBasis)
points(x,y,col = "dodgerblue")
gam.check(fitHighBasis)

ngrid <- 201
xgrid <- seq(min(x),max(x),length = ngrid)
ygrid <- seq(min(y),max(y),length = ngrid)
xymesh <- expand.grid(xgrid,ygrid)
names(xymesh) <- c("x","y")
fitmesh <- matrix(predict(fitHighBasis,newdata = xymesh),
                  ngrid,ngrid)
filled.contour(xgrid,ygrid,fitmesh,
               xlab = "longitude",ylab = "latitude",
               key.title = title(main = "ozone"))

# map('state', xlim = c(min(xgrid), max(xgrid)), ylim = c(min(ygrid), max(ygrid)))
US()

# Exercise 2

rm(list=ls())

library(HRW) 
library(mgcv)

data(SydneyRealEstate)

x <- SydneyRealEstate$longitude
y <- SydneyRealEstate$latitude
z <- SydneyRealEstate$LogSalePrice

plot(x,y,col = "dodgerblue",cex = 0.1)

data(SydneyRealEstateBdry2013)
# print(SydneyRealEstateBdry2013)
lines(SydneyRealEstateBdry2013,col = "navy",lwd = 3)

fit <- gam(z ~ s(x,y,k = 150))
plot(fit)

ngrid <- 201
xgrid <- seq(min(x),max(x),length = ngrid)
ygrid <- seq(min(y),max(y),length = ngrid)
xymesh <- expand.grid(xgrid,ygrid)
names(xymesh) <- c("x","y")

fitmesh <- matrix(predict(fit,newdata = xymesh),ngrid,ngrid)
image(xgrid,ygrid,fitmesh,col=terrain.colors(1000),
      xlab = "longitude",ylab = "latitude")

points(x,y,col = "dodgerblue",pch = ".")

outInds <- (1:ngrid^2)[pointsInPoly(xymesh,
                                    SydneyRealEstateBdry2013)==FALSE]
fitmesh[outInds] <- NA
image(xgrid,ygrid,fitmesh,col=terrain.colors(1000),
      xlab = "longitude",ylab = "latitude",
      xlim = c(150.6,151.4),ylim = c(-34.15,-33.55))
lines(SydneyRealEstateBdry2013,col = "navy",lwd = 3)

