source('randomMatrix.R')
source('shabalin.R')

#------------------------------------

M <- 300
N <- 360
m <- 40
n <- 60
crit <- 2 * sqrt(max(log(M)/n, log(N)/m, (log(M) + log(N))/(m + n)))

data <- normalMatrix(M, N, m, n, crit)
result <- matrix(0, M/3, N/3)
for (i in 1 : M/3){
  for (j in 1 : N/3){
    result[i, j] <- largestSbmxShabalin(data, i, j) / sqrt(i * j)
  }
}

save(result, file = 'gss1.Rdata')

#-----------------------------------

