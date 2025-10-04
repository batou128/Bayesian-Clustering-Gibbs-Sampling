### Projet 4A
library(LaplacesDemon)
library(ggplot2)
library(ggpubr)


TirageY <- function(Nbgrp, Nbtirage){
  pik = rdirichlet(1, rep(3,Nbgrp))
  muk = rep(0,Nbgrp)
  sigmak = rinvgamma(Nbgrp, shape = 100, scale = 100)
  for(i in 1:Nbgrp){
    muk[i] = rnorm(1, mean = 20.8, sd = 10*sqrt(sigmak[i]))
  }
  
  
  X = c()
  for (i in 1:Nbtirage) {
    loi = sample(seq(1,Nbgrp), size = 1, prob = pik) # Z choix du groupe 
    X = append(X,rnorm(1,muk[loi],sqrt(sigmak[loi])))
  }
  listefinale = list(X, pik, muk, sigmak)  
  return(listefinale)
}

Y = TirageY(3,10000)

pik = Y[[2]]
muk = Y[[3]]
sigmak = Y[[4]]

title1 <- paste("Mu =",as.character(muk[1]),"|","Sigma =",as.character(sigmak[1]), sep = " ")
title2 <- paste("Mu =",as.character(muk[2]),"|","Sigma =",as.character(sigmak[2]), sep = " ")
title3 <- paste("Mu =",as.character(muk[3]),"|","Sigma =",as.character(sigmak[3]), sep = " ")

df = as.data.frame(Y[[1]])
g1 <- ggplot(df,aes(x=Y[[1]]))+
  geom_histogram(mapping = aes(x = Y[[1]], y=..density..), fill="steelblue", colour="black")+
  labs(title = title1)+
  scale_x_continuous(limits = c(muk[1]-3*sigmak[1], muk[1]+3*sigmak[1]))+
  stat_function(fun = dnorm, args = list(mean = muk[1], sd = sqrt(sigmak[1])))+
  xlab("Y")

g2 <- ggplot(df,aes(x=Y[[1]]))+geom_histogram(mapping = aes(x = Y[[1]], y=..density..), fill="steelblue", colour="black")+labs(title = title2)+ scale_x_continuous(limits = c(muk[2]-3*sigmak[2], muk[2]+3*sigmak[2]))+
  stat_function(fun = dnorm, args = list(mean = muk[2], sd = sqrt(sigmak[2])))+
  xlab("Y")

g3 <- ggplot(df,aes(x=Y[[1]]))+geom_histogram(mapping = aes(x = Y[[1]], y=..density..), fill="steelblue", colour="black")+labs(title = title3)+ scale_x_continuous(limits = c(muk[3]-3*sigmak[3], muk[3]+3*sigmak[3]))+
  stat_function(fun = dnorm, args = list(mean = muk[3], sd = sqrt(sigmak[3])))+
  xlab("Y")


fig <- ggarrange(g1,g2,g3, nrow = 2, ncol = 2)
plot(fig)

g <- ggplot(df,aes(x=Y[[1]]))+
  geom_histogram(mapping = aes(x = Y[[1]], y=..density..), fill="steelblue", colour="black",binwidth = 1.5)+
  stat_function(fun = dnormm, args = list(p = pik, mu = muk, sigma = sqrt(sigmak)))+
  xlab("Data")
plot(g)
# Il se peut qu'il y ait un problème d'arrondi et que la densité ne s'affiche pas dans ce relancer le code

gal = MASS::galaxies
gal = gal/1000
dfgal = as.data.frame(gal)


ggal = ggplot(dfgal,aes(x = gal))+
  geom_histogram(fill="purple", colour="white")+xlab("Répartition des galaxies")
plot(ggal)
