library(lattice)
load('gss1.Rdata')
data <- result[1:100,1:120]
for (i in 1:100){
  for (j in 1:120){
    data[i,j] <- data[i,j] - sqrt(2 * (log(300) + log(360) + i * log(300 / i) + j * log(360 / j)))
  }
}
levelplot(data^4)
load('gss2.Rdata')
data2 <- result[1:100,1:50]
for (i in 1:100){
  for (j in 1:50){
    data2[i,j] <- data2[i,j] - sqrt(2 * (log(500) + log(50) + i * log(500 / i) + j * log(50 / j)))
  }
}
newdata2 <- data2^4
levelplot(newdata2)
