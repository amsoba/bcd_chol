library(mclust)
modch = Mclust(centerh, modelNames = c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"))
summary(modch)
plot(modch, what = "classification")

antalklynger <- modch$G
hvor <- modch$classification
indekser <- 1:length(hvor)
means <- modch$par$mean
vars <- modch$par$variance$sigma
klyngecentre <- c()
for(i in 1:antalklynger){
  klyngei <- indekser[hvor==i]
  meani <- means[,i]
  variInvers <- solve(vars[,,i])
  disti <- Inf
  kmin <- 0
  for(k in klyngei){
    h <- matrix(centerh[k,]-meani,ncol=1)
    d <- t(h)%*%variInvers%*%h#Mahalanobis
    if(d>disti) next
    kmin <- k
    disti <- d
  }
  klyngecentre <- c(klyngecentre,kmin)
}
klyngecentre
centerh[klyngecentre,]

#maxmin <- c()
#for(i in 1:antalklynger){
#    klyngei <- indekser[hvor==i]
#    dist <- 0
#    kmax <- 0
#    for(k in klyngei){
#        disti <- Inf
#        h <- centerh[k,]
#        for(x in 1:antalklynger){
#            if(x==i) next #afstand til eget klyngecenter er irrelevant
#            h <- sum((h-means[,x])^2)
#            if(h>disti) next#vi leder efter det tætteste klyngecenter
#        disti <- h
#        }
#        if(disti<dist) next#dette billede er tættere på de andre
#                           #centre end et tidligere billede
#        kmax <- k
#        dist <- disti
#    }
#    maxmin <- c(maxmin,kmax)#billede hvis minimale afstand til andre
#                            #centre er maksimal
#}
#centerh[maxmin,]#meget mere ekstreme ift klyngecentre
#plot(centerh[maxmin,])
