files <- list.files()#filnavne i aktiv mappe
OK <- c()#vi skal bruge filer med efternavn pdb
for(i in 1:length(files)){
    x <- files[i]
    n <- nchar(x)
    if(substr(files[i],n-3,n)==".pdb") OK <- c(OK,i)
}
files <- files[OK]

fileno <- function(x){#filnavnene skal ordnes så de kommer i
                      #nummerrækkefølge 
    n <- nchar(x)
    substr(x,10,n-4)
}
no <- as.numeric(sapply(files,fileno))
files <- files[order(no)]

pictures <- list()#roterede og centrede data - ift sukker - for hvert billede
for(x in files) pictures <- c(pictures,list(onePicture(x)))
