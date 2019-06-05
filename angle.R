# vinklen mellem vektor i rigid del af kolesterol og akserne
angle <- c()
for(x in pictures){
  a <- x[x$eleno==171,5:7]
  b <- x[x$eleno==156,5:7]
  vec <- c(b[,1]-a[,1],b[,2]-a[,2],b[,3]-a[,3])
  z <- c(0,0,1)
  theta <- (180/pi)*acos((vec%*%z)/(sqrt(vec[1]^2+vec[2]^2+vec[3]^2)*sqrt(z[1]^2+z[2]^2+z[3]^2)))
  angle <- rbind(angle,theta)
}
ts.plot(angle,xlab='Indeks',ylab='Vinkel [grader]',col='green')
