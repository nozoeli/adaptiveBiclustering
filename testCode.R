source('adaptiveShabalin.R')
source('randomMatrix.R')
source('gss.R')
#--------Initialization---------------
M <- 1000
N <- 1200
m <- 170
n <- 140
B <- 30
crit <- sqrt(max(log(M)/n, log(N)/m, (log(M) + log(N))/(m + n)))
mu <- (2 + (1:30) * 0.1) * crit

#-------compare within the two algorithms--------------------
errada <- matrix(nrow = length(mu), ncol = B)
errgss <- matrix(nrow = length(mu), ncol = B)
for (i in 1 : length(mu)){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, mu[i])
    y <- largestAdaptiveShabalin(data, 5, 5)
    z <- goldenSecSearch(data, 500, 500)
    errada[i,j] <- length(setdiff(1:m, y$Rows)) + length(setdiff(y$Rows, 1:m)) + length(setdiff(1:n, y$Cols)) + length(setdiff(y$Cols, 1:n)) 
    errgss[i,j] <- length(setdiff(1:m, z$Rows)) + length(setdiff(z$Rows, 1:m)) + length(setdiff(1:n, z$Cols)) + length(setdiff(z$Cols, 1:n)) 
  }
}

save(errada, errgss, file = 'result.Rdata')


