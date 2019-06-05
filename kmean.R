# kmeans clustering af morePictures
# center for kolesterol
clustch = kmeans(centerh,10, nstart=100)
centerh = as.data.frame(centerh)
#farvevalg
for (i in 1:1001){
  if (clustch$cluster[i]=="1"){
    clustch$cluster[i] <- "#000000"
  }
  if (clustch$cluster[i]=="2"){
    clustch$cluster[i] <- "#999999"
  }
  if (clustch$cluster[i]=="3"){
    clustch$cluster[i] <- "#FF0000"
  }
  if (clustch$cluster[i]=="4"){
    clustch$cluster[i] <- "#CC00CC"
  }
  if (clustch$cluster[i]=="5"){
    clustch$cluster[i] <- "#0000CC"
  }
  if (clustch$cluster[i]=="6"){
    clustch$cluster[i] <- "#00FFFF"
  }
  if (clustch$cluster[i]=="7"){
    clustch$cluster[i] <- "#669900"
  }
  if (clustch$cluster[i]=="8"){
    clustch$cluster[i] <- "#CCFF00"
  }
  if (clustch$cluster[i]=="9"){
    clustch$cluster[i] <- "#FFFF00"
  }
  if (clustch$cluster[i]=="10"){
    clustch$cluster[i] <- "#FF9933"
  }
}
plot(centerh, col=(clustch$cluster))

clustcm = kmeans(centerm,10, nstart=100)
centerm = as.data.frame(centerm)
#farvevalg
for (i in 1:1001){
  if (clustcm$cluster[i]=="1"){
    clustcm$cluster[i] <- "#000000"
  }
  if (clustcm$cluster[i]=="2"){
    clustcm$cluster[i] <- "#999999"
  }
  if (clustcm$cluster[i]=="3"){
    clustcm$cluster[i] <- "#FF0000"
  }
  if (clustcm$cluster[i]=="4"){
    clustcm$cluster[i] <- "#CC00CC"
  }
  if (clustcm$cluster[i]=="5"){
    clustcm$cluster[i] <- "#0000CC"
  }
  if (clustcm$cluster[i]=="6"){
    clustcm$cluster[i] <- "#00FFFF"
  }
  if (clustcm$cluster[i]=="7"){
    clustcm$cluster[i] <- "#669900"
  }
  if (clustcm$cluster[i]=="8"){
    clustcm$cluster[i] <- "#CCFF00"
  }
  if (clustcm$cluster[i]=="9"){
    clustcm$cluster[i] <- "#FFFF00"
  }
  if (clustcm$cluster[i]=="10"){
    clustcm$cluster[i] <- "#FF9933"
  }
}
plot(centerm, col=(clustcm$cluster))

clustcc = kmeans(centerc,10, nstart=100)
centerc = as.data.frame(centerc)
#farvevalg
for (i in 1:1001){
  if (clustcc$cluster[i]=="1"){
    clustcc$cluster[i] <- "#000000"
  }
  if (clustcc$cluster[i]=="2"){
    clustcc$cluster[i] <- "#999999"
  }
  if (clustcc$cluster[i]=="3"){
    clustcc$cluster[i] <- "#FF0000"
  }
  if (clustcc$cluster[i]=="4"){
    clustcc$cluster[i] <- "#CC00CC"
  }
  if (clustcc$cluster[i]=="5"){
    clustcc$cluster[i] <- "#0000CC"
  }
  if (clustcc$cluster[i]=="6"){
    clustcc$cluster[i] <- "#00FFFF"
  }
  if (clustcc$cluster[i]=="7"){
    clustcc$cluster[i] <- "#669900"
  }
  if (clustcc$cluster[i]=="8"){
    clustcc$cluster[i] <- "#CCFF00"
  }
  if (clustcc$cluster[i]=="9"){
    clustcc$cluster[i] <- "#FFFF00"
  }
  if (clustcc$cluster[i]=="10"){
    clustcc$cluster[i] <- "#FF9933"
  }
}
plot(centerc, col=(clustcc$cluster))

#oxygen
clusto = kmeans(oxygen,10, nstart=100)
oxygen = as.data.frame(oxygen)
#farvevalg
for (i in 1:1001){
  if (clusto$cluster[i]=="1"){
    clusto$cluster[i] <- "#000000"
  }
  if (clusto$cluster[i]=="2"){
    clusto$cluster[i] <- "#999999"
  }
  if (clusto$cluster[i]=="3"){
    clusto$cluster[i] <- "#FF0000"
  }
  if (clusto$cluster[i]=="4"){
    clusto$cluster[i] <- "#CC00CC"
  }
  if (clusto$cluster[i]=="5"){
    clusto$cluster[i] <- "#0000CC"
  }
  if (clusto$cluster[i]=="6"){
    clusto$cluster[i] <- "#00FFFF"
  }
  if (clusto$cluster[i]=="7"){
    clusto$cluster[i] <- "#669900"
  }
  if (clusto$cluster[i]=="8"){
    clusto$cluster[i] <- "#CCFF00"
  }
  if (clusto$cluster[i]=="9"){
    clusto$cluster[i] <- "#FFFF00"
  }
  if (clusto$cluster[i]=="10"){
    clusto$cluster[i] <- "#FF9933"
  }
}
plot(oxygen, col=(clusto$cluster))

#center rigid del 
clustrh = kmeans(rigidh,10, nstart=100)
rigidh = as.data.frame(rigidh)
#farvevalg
for (i in 1:1001){
  if (clustrh$cluster[i]=="1"){
    clustrh$cluster[i] <- "#000000"
  }
  if (clustrh$cluster[i]=="2"){
    clustrh$cluster[i] <- "#999999"
  }
  if (clustrh$cluster[i]=="3"){
    clustrh$cluster[i] <- "#FF0000"
  }
  if (clustrh$cluster[i]=="4"){
    clustrh$cluster[i] <- "#CC00CC"
  }
  if (clustrh$cluster[i]=="5"){
    clustrh$cluster[i] <- "#0000CC"
  }
  if (clustrh$cluster[i]=="6"){
    clustrh$cluster[i] <- "#00FFFF"
  }
  if (clustrh$cluster[i]=="7"){
    clustrh$cluster[i] <- "#669900"
  }
  if (clustrh$cluster[i]=="8"){
    clustrh$cluster[i] <- "#CCFF00"
  }
  if (clustrh$cluster[i]=="9"){
    clustrh$cluster[i] <- "#FFFF00"
  }
  if (clustrh$cluster[i]=="10"){
    clustrh$cluster[i] <- "#FF9933"
  }
}
plot(rigidh, col=(clustrh$cluster))

clustrm = kmeans(rigidm,10, nstart=100)
rigidm = as.data.frame(rigidm)
#farvevalg
for (i in 1:1001){
  if (clustrm$cluster[i]=="1"){
    clustrm$cluster[i] <- "#000000"
  }
  if (clustrm$cluster[i]=="2"){
    clustrm$cluster[i] <- "#999999"
  }
  if (clustrm$cluster[i]=="3"){
    clustrm$cluster[i] <- "#FF0000"
  }
  if (clustrm$cluster[i]=="4"){
    clustrm$cluster[i] <- "#CC00CC"
  }
  if (clustrm$cluster[i]=="5"){
    clustrm$cluster[i] <- "#0000CC"
  }
  if (clustrm$cluster[i]=="6"){
    clustrm$cluster[i] <- "#00FFFF"
  }
  if (clustrm$cluster[i]=="7"){
    clustrm$cluster[i] <- "#669900"
  }
  if (clustrm$cluster[i]=="8"){
    clustrm$cluster[i] <- "#CCFF00"
  }
  if (clustrm$cluster[i]=="9"){
    clustrm$cluster[i] <- "#FFFF00"
  }
  if (clustrm$cluster[i]=="10"){
    clustrm$cluster[i] <- "#FF9933"
  }
}
plot(rigidm, col=(clustrm$cluster))

clustrc = kmeans(rigidc, 10, nstart=100)
rigidc = as.data.frame(rigidc)
#farvevalg
for (i in 1:1001){
  if (clustrc$cluster[i]=="1"){
    clustrc$cluster[i] <- "#000000"
  }
  if (clustrc$cluster[i]=="2"){
    clustrc$cluster[i] <- "#999999"
  }
  if (clustrc$cluster[i]=="3"){
    clustrc$cluster[i] <- "#FF0000"
  }
  if (clustrc$cluster[i]=="4"){
    clustrc$cluster[i] <- "#CC00CC"
  }
  if (clustrc$cluster[i]=="5"){
    clustrc$cluster[i] <- "#0000CC"
  }
  if (clustrc$cluster[i]=="6"){
    clustrc$cluster[i] <- "#00FFFF"
  }
  if (clustrc$cluster[i]=="7"){
    clustrc$cluster[i] <- "#669900"
  }
  if (clustrc$cluster[i]=="8"){
    clustrc$cluster[i] <- "#CCFF00"
  }
  if (clustrc$cluster[i]=="9"){
    clustrc$cluster[i] <- "#FFFF00"
  }
  if (clustrc$cluster[i]=="10"){
    clustrc$cluster[i] <- "#FF9933"
  }
}
plot(rigidc, col=(clustrc$cluster))

#vinkel mellem kolesterol og z-aksen
clusta = kmeans(angle,10, nstart=100)
#farvevalg
for (i in 1:1001){
  if (clusta$cluster[i]=="1"){
    clusta$cluster[i] <- "#000000"
  }
  if (clusta$cluster[i]=="2"){
    clusta$cluster[i] <- "#999999"
  }
  if (clusta$cluster[i]=="3"){
    clusta$cluster[i] <- "#FF0000"
  }
  if (clusta$cluster[i]=="4"){
    clusta$cluster[i] <- "#CC00CC"
  }
  if (clusta$cluster[i]=="5"){
    clusta$cluster[i] <- "#0000CC"
  }
  if (clusta$cluster[i]=="6"){
    clusta$cluster[i] <- "#00FFFF"
  }
  if (clusta$cluster[i]=="7"){
    clusta$cluster[i] <- "#669900"
  }
  if (clusta$cluster[i]=="8"){
    clusta$cluster[i] <- "#CCFF00"
  }
  if (clusta$cluster[i]=="9"){
    clusta$cluster[i] <- "#FFFF00"
  }
  if (clusta$cluster[i]=="10"){
    clusta$cluster[i] <- "#FF9933"
  }
}
plot(angle, col=(clusta$cluster), xlab='Indeks',ylab='Vinkel [grader]') 

