# hierarchical clustering
hclustch = hclust(dist(centerh), method = "complete")
plot(hclustch,xlab='', ylab='Afstand [m*10^-10]')
cutch = cutree(hclustch,8)
centerh <- as.data.frame(centerh)
plot(centerh, col=cutch)

hclustcm = hclust(dist(centerm), method = "complete")
plot(hclustcm,xlab='', ylab='Afstand [m*10^-10]')
cutcm = cutree(hclustcm,9)
# farvevalg
for (i in 1:1001){
  if (cutcm[i]=="9"){
    cutcm[i] <- "#FF9933"
  }
}
centerm <- as.data.frame(centerm)
plot(centerm, col=cutcm)

hclustcc = hclust(dist(centerc), method = "complete")
plot(hclustcc,xlab='', ylab='Afstand [m*10^-10]')
cutcc = cutree(hclustcc,7)
centerc <- as.data.frame(centerc)
plot(centerc, col=cutcc)

hclustrh = hclust(dist(rigidh), method = "complete")
plot(hclustrh,xlab='', ylab='Afstand [m*10^-10]')
cutrh = cutree(hclustrh,8)
rigidh <- as.data.frame(rigidh)
plot(rigidh, col=cutrh)

hclustrm = hclust(dist(rigidm), method = "complete")
plot(hclustrm,xlab='', ylab='Afstand [m*10^-10]')
cutrm = cutree(hclustrm,9)
# farvevalg
for (i in 1:1001){
  if (cutrm[i]=="9"){
    cutrm[i] <- "#FF9933"
  }
}
rigidm <- as.data.frame(rigidm)
plot(rigidm, col=cutrm)

hclustrc = hclust(dist(rigidc), method = "complete")
plot(hclustrc,xlab='', ylab='Afstand [m*10^-10]')
cutrc = cutree(hclustrc,10)
# farvevalg
for (i in 1:1001){
  if (cutrc[i]=="9"){
    cutrc[i] <- "#FF9933"
  }
  if (cutrc[i]=="10"){
    cutrc[i] <- "#CCFF00"
  }
  
}
rigidc <- as.data.frame(rigidc)
plot(rigidc, col=cutrc)

hclusto = hclust(dist(oxygen), method = "complete")
plot(hclusto,xlab='', ylab='Afstand [m*10^-10]')
cuto = cutree(hclusto,12)
# farvevalg
for (i in 1:1001){
  if (cuto[i]=="9"){
    cuto[i] <- "#FF9933"
  }
  if (cuto[i]=="10"){
    cuto[i] <- "#CCFF00"
  }
  if (cuto[i]=="11"){
    cuto[i] <- "#999999"
  }
  if (cuto[i]=="12"){
    cuto[i] <- "#CC00CC"
  }
  
}
oxygen <- as.data.frame(oxygen)
plot(oxygen, col=cuto)

hclusta = hclust(dist(angle), method = "complete")
plot(hclusta,xlab='', ylab='Afstand [m*10^-10]')
cuta = cutree(hclusta,11)
# farvevalg
for (i in 1:1001){
  if (cuta[i]=="9"){
    cuta[i] <- "#FF9933"
  }
  if (cuta[i]=="10"){
    cuta[i] <- "#CCFF00"
  }
  if (cuta[i]=="11"){
    cuta[i] <- "#999999"
  }
}
plot(angle, col=cuta, ylab='Vinkel [grader]')
