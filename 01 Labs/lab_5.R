# Exercise 1

rm(list=ls())

library(HRW)
library(lattice)
library(mgcv)
# install.packages("fields")
library(fields)

data(femSBMD)
SBMDblack <- femSBMD[femSBMD$black == 1,]
fit <- gam(spnbmd ~ s(age),data = SBMDblack)
SBMDblack$spnbmd_cent <- residuals(fit)

id <- unique(SBMDblack$idnum)
dat <- matrix(NA, nrow = 0, ncol = 3)
for (i in id)
{
    samp <- SBMDblack[SBMDblack$idnum == i,]
    m <- dim(samp)[1]
    for (j in 1:m)
    {
        for (k in 1:m)
        {
            dat <- rbind(dat,c(samp$age[j],samp$age[k],
                               samp$spnbmd_cent[j] * samp$spnbmd_cent[k]))
        }
    }
}


age.lim <- range(SBMDblack$age)
ng <- 10
xgrid <- seq(age.lim[1],age.lim[2],length=ng)
ygrid <- seq(age.lim[1],age.lim[2],length=ng)
index.x <- cut(dat[,1], xgrid, include.lowest=T)
index.y <- cut(dat[,2], ygrid, include.lowest=T)
dat.mean <- tapply(dat[,3], list(index.x, index.y), mean)
image.plot(xgrid, ygrid, dat.mean, col=heat.colors(32),
           xlim=age.lim, ylim=age.lim,
           xlab="age (years)",ylab="age (years)", asp=1)
fit_cov <- gam(dat[,3] ~ s(dat[,1], dat[,2]), method="REML")

par(mfrow = c(1, 1))
plot(fit_cov, scheme=2, xlim=age.lim, ylim=age.lim,
     cex=1.2, xlab="age (years)", ylab="age (years)", main="", asp=1)

# Exercise 2


rm(list=ls())

library(mgcv)
# install.packages("oro.nifti")
library(oro.nifti) # for readNIfTI()
# install.packages("refund")
library(refund)

Kirby21_fMRI <- readNIfTI("KKI2009-01-fMRI.nii.gz", reorient = TRUE)
image_dat = Kirby21_fMRI[,40,]  ##coronal slice##
ex.gr = expand.grid(1:80,1:37)
ex.gr[,1] <- ex.gr[,1]/80
ex.gr[,2] <- ex.gr[,2]/37
fit_TP <- gam( as.vector(image_dat) ~ s(ex.gr[,1],
ex.gr[,2],k = 625) )
fit_TE <- gam( as.vector(image_dat) ~ te(ex.gr[,1],
ex.gr[,2],k = c(35, 18)))

knots_SS <- list(seq(0,1,length = 50),seq(0,1,length = 35))
fit_SS <- fbps(image_dat,knots = knots_SS)

par(mfrow = c(2, 2),mai=rep(.35,4))
image(image_dat, col = heat.colors(12),
main = "unsmoothed")
image(matrix(fit_TP$fitted, nrow = 80, ncol = 37),
main = "Thin-plate, 625 knots")
image(matrix(fit_TE$fitted, nrow = 80, ncol = 37),
main = "Tensor-product, 35x18 grid of knots")
image(fit_SS$Yhat,
main = "Sandwich smoother, 50 x 30 grid of knots")

