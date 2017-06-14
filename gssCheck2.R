source('randomMatrix.R')
source('shabalin.R')

#------------------------------------

M <- 500
N <- 50
m <- 10
n <- 25
crit <- 2 * sqrt(max(log(M)/n, log(N)/m, (log(M) + log(N))/(m + n)))

data <- normalMatrix(M, N, m, n, crit)
result <- matrix(0, M/5, N)
for (i in 1 : M/5){
  for (j in 1 : N){
    result[i, j] <- largestSbmxShabalin(data, i, j) / sqrt(i * j)
  }
}

save(result, file = 'gss2.Rdata')