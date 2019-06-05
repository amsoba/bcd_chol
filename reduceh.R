# udregning af klyngernes centre
hcenter <- function(data){
hcentre <- c()
for (i in 1:max(data)){
  k <- c()
  for (j in 1:1001){
    if (data[j,4]==i){
      ny <- data[j,1:3]
      k <- rbind(k,ny)
      k <- apply(k,2,mean)
    }
  }
  hcentre <- rbind(hcentre, k)
}
hcentre
}
hcentera <- function(data){
  hcentre <- c()
  for (i in 1:max(data)){
    k <- c()
    for (j in 1:1001){
      if (data[j,2]==i){
        ny <- data[j,1]
        k <- rbind(k,ny)
        k <- apply(k,2,mean)
      }
    }
    hcentre <- rbind(hcentre, k)
  }
hcentre
}

hch <- cbind(centerh, as.data.frame(cutch))
hierch <- c()
for (i in 1:max(cutch)){
  hierch <- rbind(hierch,mindist(centerh,hcenter(hch)[i,]))
}
row.names(hierch)

hcm <- cbind(centerm, as.data.frame(cutcm))
hiercm <- c()
for (i in 1:max(cutcm)){
  hiercm <- rbind(hiercm,mindist(centerm,hcenter(hcm)[i,]))
}
row.names(hiercm)

hcc <- cbind(centerc, as.data.frame(cutcc))
hiercc <- c()
for (i in 1:max(cutcc)){
  hiercc <- rbind(hiercc,mindist(centerc,hcenter(hcc)[i,]))
}
row.names(hiercc)

hrh <- cbind(rigidh, as.data.frame(cutrh))
hierrh <- c()
for (i in 1:max(cutrh)){
  hierrh <- rbind(hierrh,mindist(rigidh,hcenter(hrh)[i,]))
}
row.names(hierrh)

hrm <- cbind(rigidm, as.data.frame(cutrm))
hierrm <- c()
for (i in 1:max(cutrm)){
  hierrm <- rbind(hierrm,mindist(rigidm,hcenter(hrm)[i,]))
}
row.names(hierrm)

hrc <- cbind(rigidc, as.data.frame(cutrc))
hierrc <- c()
for (i in 1:max(cutrc)){
  hierrc <- rbind(hierrc,mindist(rigidc,hcenter(hrc)[i,]))
}
row.names(hierrc)

ho <- cbind(oxygen, as.data.frame(cuto))
hiero <- c()
for (i in 1:max(cuto)){
  hiero <- rbind(hiero,mindist(oxygen,hcenter(ho)[i,]))
}
row.names(hiero)

ha <- cbind(angle, as.data.frame(cuta))
hiera <- c()
for (i in 1:max(cuta)){
  hiera <- rbind(hiera,mindista(angle,hcentera(ha)[i,]))
}

hcenta <- hcentera(ha)
dis <- matrix(ncol=11,nrow=1001)
for (i in 1:11){
  for (j in 1:1001){
    dis[j,i] <- euda(angle[j],hcenta[i,])
  }
}

rows <- c()
for (i in 1:11){
  rows <- rbind(rows, which(dis[,i]==hiera[i,]))
}
rows
