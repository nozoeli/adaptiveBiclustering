source('adaptiveShabalin.R')
source('randomMatrix.R')
source('gss.R')

#---------------------------Evaluation Running Time--------------
B <- 100
ptm1 <- matrix(0, B, 3)
ptm2 <- matrix(0, B, 3)
ptm3 <- matrix(0, B, 3)
ptm4 <- matrix(0, B, 3)

for (i in 1 : B){
  x = normalMatrix(1000, 1000, 100, 100, 2.5)
  ptm <- proc.time()
  a <- largestAdaptiveShabalin(x, 5, 5)
  ptm1[i, ] <- as.vector(proc.time() - ptm)[1:3]
  ptm <- proc.time()
  b <- svdBicluster(x)
  ptm2[i, ] <- as.vector(proc.time() - ptm)[1:3]
  ptm <- proc.time()
  c <- goldenSecSearch(x, 500, 500)
  ptm3[i, ] <- as.vector(proc.time() - ptm)[1:3]
  ptm <- proc.time()
  d <- lmgBicluster(x)
  ptm4[i, ] <- as.vector(proc.time() - ptm)[1:3]
}

save(ptm1, ptm2, ptm3, ptm4, file = 'timeEval.Rdata')

load('timeEval.Rdata')
user <- c(ptm1[, 1], ptm3[, 1], ptm4[, 1], ptm2[, 1])
system <- c(ptm1[, 2], ptm3[, 2], ptm4[, 2], ptm2[, 2])
elapsed <- c(ptm1[, 3], ptm3[, 3], ptm4[, 3], ptm2[, 3])
timecate <- rep(c('user', 'system', 'elapsed'), each = length(user))
method <- perm_method <- factor(rep(c('Adaptive Hill-climbing', 'Golden Section Search', 'Greatest Marginal Gap', 'Spectral Method'), each = B), c('Adaptive Hill-climbing', 'Golden Section Search', 'Greatest Marginal Gap', 'Spectral Method'))
data <- data.frame('time' = c(user, system, elapsed), 'method' = as.factor(method), 'category' = as.factor(timecate))
library('ggplot2')
p <- ggplot(data, aes(x = category, y = time, color = method))
p + geom_boxplot(outlier.shape = NA) + scale_colour_manual(values = c("#FF0010", "#990000", "#2BCE48", "#0075DC"))
