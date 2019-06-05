setwd("~/AAU/Kandidat/10. semester/Data/alle_strukturer")

onePicture <- function(file,rotxy=TRUE){
    require("bio3d")
    ch <- read.pdb2(file)$atom
    ch <- ch[,c("eleno","elety","resid","resno","x","y","z","elesy")]
    for(i in c(2,3,8)) ch[,i] <- factor(ch[,i])
    GAxyz <- ch[ch$resid=="4GA",c("x","y","z")]
    p1 <- princomp(GAxyz)
                                        #du skal prøve biplot(p1)
    rotation <- p1$loadings[,]
    center <- apply(GAxyz,2,mean)
    ch[,c("x","y","z")] <- (as.matrix(ch[,c("x","y","z")])-center)%*%rotation
                                        #sukkerringen er nu i
                                        #xy-planet og centreret i (0,0,0)
    if(ch[42,7]>0) ch[,7] <- -ch[,7] #billedet vendes, så iltmolekylet i
                                        #cholesterol er i bunden
    if(rotxy){#vi roterer så molekyle 126 har x koordinat lig 0 og
              #positiv y-koordinat.
        ref <- as.numeric(ch[126,5:6])
        ref <- ref/sqrt(sum(ref^2))
        rot <- matrix(c(ref[2],-ref[1],ref[1],ref[2]),2)
        ch[,5:6] <- cbind(ch[,5],ch[,6])%*%rot
        if(ch[76,5]>0) ch[,5] <- -ch[,5]#vi vender x-aksen, så
                                        #molekyle 76 ligger i starten 
    }
    ch
}
