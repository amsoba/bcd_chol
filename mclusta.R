library(mclust)
moda = Mclust(angle, modelNames = c("E","V"))
summary(moda)
plot(angle, col=moda$classification, ylab="Vinkel [grader]") #fejl med plot hvis dataframe

angle <- as.data.frame(angle)
antalklynger <- moda$G
hvor <- moda$classification
indekser <- 1:length(hvor)
means <- moda$par$mean
vars <- moda$par$variance$sigma
klyngecentre <- c()
for(i in 1:antalklynger){
  klyngei <- indekser[hvor==i]
  meani <- means[i]
  variInvers <- 1/vars
  disti <- Inf
  kmin <- 0
  for(k in klyngei){
    h <- matrix(angle[k,]-meani,ncol=1)
    d <- t(h)%*%variInvers%*%h
    if(d>disti) next
    kmin <- k
    disti <- d
  }
  klyngecentre <- c(klyngecentre,kmin)
}
klyngecentre
angle[klyngecentre,]