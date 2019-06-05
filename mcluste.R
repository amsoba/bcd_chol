ekstra <- cbind(centerc, angle)
colnames(ekstra)[4] <- "theta"

library(mclust)
mode = Mclust(ekstra, modelNames = c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"))
summary(mode)
plot(mode, what = "classification")

antalklynger <- mode$G
hvor <- mode$classification
indekser <- 1:length(hvor)
means <- mode$par$mean
vars <- mode$par$variance$sigma
klyngecentre <- c()
for(i in 1:antalklynger){
  klyngei <- indekser[hvor==i]
  meani <- means[,i]
  variInvers <- solve(vars[,,i])
  disti <- Inf
  kmin <- 0
  for(k in klyngei){
    h <- matrix(ekstra[k,]-meani,ncol=1)
    d <- t(h)%*%variInvers%*%h
    if(d>disti) next
    kmin <- k
    disti <- d
  }
  klyngecentre <- c(klyngecentre,kmin)
}
klyngecentre
ekstra[klyngecentre,]
