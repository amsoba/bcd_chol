eud <- function(a,b){
  (a[1]-b[1])^2+(a[2]-b[2])^2+(a[3]-b[3])^2
}
euda <- function(a,b){
  (a[1]-b[1])^2
}

mindist <- function(x,c){
  di <- 100
  for (i in 1:1001){
    d <- eud(x[i,],c[])
    if (d>di) next
    di <- d
  }
  di
}
mindista <- function(x,c){
  di <- 100
  for (i in 1:1001){
    d <- euda(x[i,],c[])
    if (d>di) next
    di <- d
  }
  di
}

kmidch <- c()
for (i in 1:10){
  kmidch <- rbind(kmidch,mindist(centerh,clustch$centers[i,]))
}
row.names(kmidch)
clustch$tot.withinss

kmidcm <- c()
for (i in 1:10){
  kmidcm <- rbind(kmidcm,mindist(centerm,clustcm$centers[i,]))
}
row.names(kmidcm)
clustcm$tot.withinss

kmidcc <- c()
for (i in 1:10){
  kmidcc <- rbind(kmidcc,mindist(centerc,clustcc$centers[i,]))
}
row.names(kmidcc)
clustcc$tot.withinss

kmidrh <- c()
for (i in 1:10){
  kmidrh <- rbind(kmidrh,mindist(rigidh,clustrh$centers[i,]))
}
row.names(kmidrh)
clustrh$tot.withinss

kmidrm <- c()
for (i in 1:10){
  kmidrm <- rbind(kmidrm,mindist(rigidm,clustrm$centers[i,]))
}
row.names(kmidrm)
clustrm$tot.withinss

kmidrc <- c()
for (i in 1:10){
  kmidrc <- rbind(kmidrc,mindist(rigidc,clustrc$centers[i,]))
}
row.names(kmidrc)
clustrc$tot.withinss

kmido <- c()
for (i in 1:10){
  kmido <- rbind(kmido,mindist(oxygen,clusto$centers[i,]))
}
row.names(kmido)
clusto$tot.withinss

kmida <- c()
for (i in 1:10){
  kmida <- rbind(kmida,mindista(as.data.frame(angle),clusta$centers[i,]))
}
kmida
clusta$tot.withinss

dis <- matrix(ncol=10,nrow=1001)
for (i in 1:10){
  for (j in 1:1001){
    dis[j,i] <- euda(angle[j],clusta$centers[i,])
  }
}

rows <- c()
for (i in 1:10){
  rows <- rbind(rows, which(dis[,i]==kmida[i,]))
}
rows
