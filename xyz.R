# HELE KOLESTEROL
# center
centerh <- c()
for(x in pictures){
  xyz <- x[x$resno==900,5:7]
  centerh <- rbind(centerh,apply(xyz,2,mean))
}
ts.plot(centerh,col=1:3,xlab='Indeks',ylab='x,y,z [m*10^-10]')

#massevægtet
centerm <- c()
for(x in pictures){
  c <- x[x$resno==900&x$elesy=="C",5:7]
  o <- (8/6)*x[x$resno==900&x$elesy=="O",5:7]
  h <- (1/12)*x[x$resno==900&x$elesy=="H",5:7]
  vaegt <- dim(c)[1]+8/6+dim(h)[1]/12
  centerm <- rbind(centerm,apply(rbind(c,h,o),2,sum)/vaegt)
}
ts.plot(centerm,col=1:3,xlab='Indeks',ylab='x,y,z [m*10^-10]')

#kun carbon
centerc <- c()
for(x in pictures){
  xyz <- x[x$resno==900&x$elesy=="C",5:7]
  centerc <- rbind(centerc,apply(xyz,2,mean))
}
ts.plot(centerc,col=1:3,xlab='Indeks',ylab='x,y,z [m*10^-10]')

# RIGID DEL AF KOLESTEROL
# center
rigidh <- c()
for (x in pictures){
  xyz <- x[c(156:172,174:175,193:214,216:221), 5:7]
  rigidh <- rbind(rigidh, apply(xyz,2,mean))
}
ts.plot(rigidh, col=1:3, xlab='Indeks',ylab='x,y,z [m*10^-10]')

#massevægtet
rigidm <- c()
for(x in pictures){
  c <- x[c(156:172,174:175),5:7]
  h <- (1/12)*x[c(193:214,216:221),5:7]
  vaegt <- dim(c)[1]+dim(h)[1]/12
  rigidm <- rbind(rigidm,apply(rbind(c,h),2,sum)/vaegt)
}
ts.plot(rigidm, col=1:3, xlab='Indeks',ylab='x,y,z [m*10^-10]')

#kun carbon
rigidc <- c()
for (x in pictures){
  xyz <- x[c(156:172,174:175),5:7]
  rigidc <- rbind(rigidc, apply(xyz,2,mean))
}
ts.plot(rigidc, col=1:3, xlab='Indeks',ylab='x,y,z [m*10^-10]')

# OXYGEN
oxygen <- c()
for(x in pictures){
  xyz <- x[173,5:7]
  oxygen <- rbind(oxygen,apply(xyz,2,mean))
}
ts.plot(oxygen, col=1:3, xlab='Indeks',ylab='x,y,z [m*10^-10]')
