source('adaptiveShabalin.R')
source('randomMatrix.R')
source('gss.R')
#--------Initialization---------------
M <- 4000
N <- 500
m <- 70
n <- 250
B <- 30
crit <- sqrt(max(log(M)/n, log(N)/m, (log(M) + log(N))/(m + n)))
mu <- (1 + (1 : 30) * 0.1) * crit

#-------Simulation main--------------------
errada <- matrix(nrow = length(mu), ncol = B)
errgss <- matrix(nrow = length(mu), ncol = B)
for (i in 1 : length(mu)){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, mu[i])
    y <- largestAdaptiveShabalin(data,5,5)
    z <- goldenSecSearch(data, 500, 400)
    errada[i,j] <- length(setdiff(1:m, y$Rows)) + length(setdiff(y$Rows, 1:m)) + length(setdiff(1:n, y$Cols)) + length(setdiff(y$Cols, 1:n)) 
    errgss[i,j] <- length(setdiff(1:m, z$Rows)) + length(setdiff(z$Rows, 1:m)) + length(setdiff(1:n, z$Cols)) + length(setdiff(z$Cols, 1:n)) 
  }
}

save(errada, errgss, file = 'result2.Rdata')


