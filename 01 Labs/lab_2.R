library(glmnet)
library(MASS)
library(SLOPE)

k=500
set.seed(8)
X=diag(x=1, nrow=1000, ncol=1000)
eps<-rnorm(1000,0,1)
beta<-c(rep(4,k),numeric(1000-k))
Xbeta<-X[,1:k]%*%rep(4,k)
Y<-Xbeta+eps

#a-wyklad beta_pred_a=Y
blad_a=rep(0,100)
for ( i in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_ #beta^=Y
  blad_a[i]=sum((Y_-beta)^2)
}
mean(blad_a)  

#b
#i
c_opt_i<-1-1000/(1000+sum((beta)^2))
bety_pred_b_i<-c_opt_i*Y

blad_bi=rep(0,100)
for ( i in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  bety_pred_b_i_<-c_opt_i*Y_
  blad_bi[i]=sum((bety_pred_b_i_-beta)^2)
}
mean(blad_bi) 

#ii
c_opt_ii<-1-1000/(sum((Y)^2))
bety_pred_b_ii<-c_opt_ii*Y

blad_bii=rep(0,100)
for ( i in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  c_opt_ii<-1-1000/(sum((Y_)^2))
  bety_pred_b_ii_<-c_opt_ii*Y_
  blad_bii[i]=sum((bety_pred_b_ii_-beta)^2)
}
mean(blad_bii) 

#c
W<-eigen(t(X)%*%X)
W1<-W$values

sure1<-function(lambda)
{
  SURE1<-0
  for (i in 1:1000) 
  {
    SURE1<-SURE1+sum((Y[i]-Y[i]/(1+lambda))^2)
  }
  SURE1<-SURE1+2000/(1+lambda)
  return(SURE1)
}
optym<-optimize(sure1, interval=c(0, 10000))
lambda_optym<-optym$minimum
### tutaj wyniki takie same:
c<- SLOPE(X,Y,lambda_method="user", lambda=rep(lambda_optym,1000), sigma=1,normalize=FALSE)

##
bety_pred_c<-rep(0,1000)
for (i in 1:1000)
{
  if (Y[i]>lambda_optym)
  bety_pred_c[i]<-(Y[i]-lambda_optym) else
  if (Y[i]<(-lambda_optym))
  bety_pred_c[i]<-(Y[i]+lambda_optym) else
  if (abs(Y[i])<=lambda_optym)
  bety_pred_c[i]=0
}

blad_c<-rep(0,100)
moc_c<-rep(0,100)
FDR_c<-rep(0,100)
for ( j in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  sure1<-function(lambda)
  {
    SURE1<-0
    for (i in 1:1000) 
    {
      SURE1<-SURE1+sum((Y_[i]-Y_[i]/(1+lambda))^2)
    }
    SURE1<-SURE1+2000/(1+lambda)
    return(SURE1)
  }
  optym<-optimize(sure1, interval=c(0, 100))
  lambda_optym<-optym$minimum
  
  bety_pred_c_<-rep(0,1000)
  for (i in 1:1000)
  {
    if (Y_[i]>lambda_optym)
      bety_pred_c_[i]<-(Y_[i]-lambda_optym) else
        if (Y_[i]<(-lambda_optym))
          bety_pred_c_[i]<-(Y_[i]+lambda_optym) else
            if (abs(Y_[i])<=lambda_optym)
              bety_pred_c_[i]=0
  }
  blad_c[j]<-sum((bety_pred_c_-beta)^2)
  moc_c[j]<-length(which(bety_pred_c_!=0))
  FDR_c[j]<-length(which(bety_pred_c_[(k+1):1000]!=0))
  
}
mean(blad_c)
mean(moc_c)
mean(FDR_c)




#d
#i zwykla wersja
bety_pred_d_i<-rep(0,1000)
l<-qnorm(1-0.05/1000)
for (i in 1:1000)
{
  if (Y[i]>l)
    bety_pred_d_i[i]<-(Y[i]-l) else
      if (Y[i]<(-l))
        bety_pred_d_i[i]<-(Y[i]+l) else
          if (abs(Y[i])<=l)
            bety_pred_d_i[i]=0
}

blad_d_i<-rep(0,100)
moc_d_i<-rep(0,100)
FDR_d_i<-rep(0,100)
for(j in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  bety_pred_d_i_<-rep(0,1000)
  for (i in 1:1000)
  {
    if (Y_[i]>l)
      bety_pred_d_i_[i]<-(Y_[i]-l) else
        if (Y_[i]<(-l))
          bety_pred_d_i_[i]<-(Y_[i]+l) else
            if (abs(Y_[i])<=l)
              bety_pred_d_i_[i]=0
  }
  blad_d_i[j]<-sum((bety_pred_d_i_-beta)^2)
  moc_d_i[j]<-length(which(bety_pred_d_i_!=0))
  FDR_d_i[j]<-length(which(bety_pred_d_i_[(k+1):1000]!=0))
  
}
mean(blad_d_i)
mean(moc_d_i)
mean(FDR_d_i)


#d ii- odciazona
s<-SLOPE(X,Y,lambda_method="user", lambda=rep(qnorm(1-0.05/1000),1000), sigma=1,normalize=FALSE)

indeksy<-rep(0,1000)
for(i in 1:1000)
{
  ifelse(i %in% s$selected,indeksy[i]<-i,indeksy[i]<-0)
}

bety_pred_d_ii<-rep(0, 1000)
for (i in 1: 1000)
{
  if(indeksy[i]==0)
    bety_pred_d_ii[i]<-0 else
    bety_pred_d_ii[i]<-Y[i] 
}

#blad,moc i FDR 
blad_d_ii<-rep(0,100)
moc_d_ii<-rep(0,100)
FDR_d_ii<-rep(0,100)
for(j in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  s_<-SLOPE(X,Y_,lambda_method="user", lambda=rep(qnorm(1-0.05/1000),1000), sigma=1,normalize=FALSE)
  indeksy_<-rep(0,1000)
  for(i in 1:1000)
  {
    ifelse(i %in% s_$selected,indeksy_[i]<-i,indeksy_[i]<-0)
  }
  
  bety_pred_d_ii_<-rep(0, 1000)
  for (i in 1: 1000)
  {
    if(indeksy_[i]==0)
      bety_pred_d_ii_[i]<-0 else
        bety_pred_d_ii_[i]<-Y_[i] 
  }
  blad_d_ii[j]<-sum((bety_pred_d_ii_-beta)^2)
  moc_d_ii[j]<-length(which(bety_pred_d_ii_!=0))
  FDR_d_ii[j]<-length(which(bety_pred_d_ii_[(k+1):1000]!=0))
}
mean(blad_d_ii)
mean(moc_d_ii)
mean(FDR_d_ii)


#e i
s_e_i<-SLOPE(X,Y,fdr=0.1,lambda = "bhq",sigma=1,normalize = FALSE)
indeksy<-rep(0,1000)
for(i in 1:1000)
{
  ifelse(i %in% s_e_i$selected,indeksy[i]<-i,indeksy[i]<-0)
}

bety_pred_e_i<-rep(0, 1000)
for (i in 1: 1000)
{
  if(indeksy[i]==0)
    bety_pred_e_i[i]<-0 else
      bety_pred_e_i[i]<-Y[i] 
}

blad_e_i<-rep(0,100)
moc_e_i<-rep(0,100)
FDR_e_i<-rep(0,100)
for(j in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  s_e_i_<-SLOPE(X,Y_,fdr=0.1,lambda = "bhq",sigma=1,normalize = FALSE)
  indeksy_<-rep(0,1000)
  for(i in 1:1000)
  {
    ifelse(i %in% s_e_i_$selected,indeksy_[i]<-i,indeksy_[i]<-0)
  }
  
  bety_pred_e_i_<-rep(0, 1000)
  for (i in 1: 1000)
  {
    if(indeksy_[i]==0)
      bety_pred_e_i_[i]<-0 else
        bety_pred_e_i_[i]<-Y_[i] 
  }
  blad_e_i[j]<-sum((bety_pred_e_i_-beta)^2)
  moc_e_i[j]<-length(which(bety_pred_e_i_!=0))
  FDR_e_i[j]<-length(which(bety_pred_e_i_[(k+1):1000]!=0))
}
mean(blad_e_i)
mean(moc_e_i)
mean(FDR_e_i)

#ii
eps_<-rnorm(1000,0,1)
Y_<-Xbeta+eps_
s_e_ii<-SLOPE(X,Y_,fdr=0.1,lambda = "bhq",sigma=1,normalize = FALSE)
s_e_ii

wart_niezerowe<-s_e_ii$beta[s_e_ii$selected]
sum((wart_niezerowe[1:496]-4)^2)+sum((wart_niezerowe[497:525])^2)+16*4

indeksy<-rep(0,1000)
for(i in 1:1000)
{
  ifelse(i %in% s_e_ii$selected,indeksy[i]<-i,indeksy[i]<-0)
}


bety_p_e_ii<-rep(0, 1000)
for (i in 1: 1000)
{
  if(indeksy[i]==0)
    bety_p_e_ii[i]<-0 else
      bety_p_e_ii[i]<- 
}

wart_niezerowe<-s_e_ii$beta[s_e_ii$selected]
P<-cbind(s_e_ii$beta[s_e_ii$selected],rep(1,length(s_e_ii$selected)))
#U<-as.matrix(P)
parametry<-solve(t(P)%*%P)%*%t(P)%*%t(t(Y[s_e_ii$selected]))
alfa<-parametry[1,1]
gamma<-parametry[2,1]

bety_pred_e_ii<-alfa*wart_niezerowe+gamma


#petla heheh
blad_e_ii<-rep(0,100)
moc_e_ii<-rep(0,100)
FDR_e_ii<-rep(0,100)
for(j in 1:100)
{
  eps_<-rnorm(1000,0,1)
  Y_<-Xbeta+eps_
  s_e_ii_<-SLOPE(X,Y_,fdr=0.1,lambda = "bhq",sigma=1,normalize = FALSE)
  indeksy_<-rep(0,1000)
  for(i in 1:1000)
  {
    ifelse(i %in% s_e_ii_$selected,indeksy_[i]<-i,indeksy_[i]<-0)
  }
  
  bety_p_e_ii_<-rep(0, 1000)
  for (i in 1: 1000)
  {
    if(indeksy_[i]==0)
      bety_p_e_ii_[i]<-0 else
        bety_p_e_ii_[i]<- 
  }
  
  wart_niezerowe_<-s_e_ii_$beta[s_e_ii_$selected]
  G<-cbind(s_e_ii_$beta[s_e_ii_$selected],rep(1,length(s_e_ii_$selected)))
  Z<-as.matrix(G)
  parametry_<-solve(t(Z)%*%Z)%*%t(Z)%*%t(t(Y[s_e_ii_$selected]))
  alfa_<-parametry_[1,1]
  gamma_<-parametry_[2,1]
  bety_pred_e_ii_<-rep(0,1000)
  bety_pred_e_ii_<-alfa_*wart_niezerowe_+gamma_
  blad_e_ii[j]<-sum((bety_pred_e_ii_-beta)^2)
  moc_e_ii[j]<-length(which(bety_pred_e_ii_!=0))
  FDR_e_ii[j]<-length(which(bety_pred_e_ii_[(k+1):1000]!=0))
}
mean(blad_e_ii)
mean(moc_e_ii)
mean(FDR_e_ii)

result <- SLOPE(X, Y, fdr=0.1)
fdp(result$selected)