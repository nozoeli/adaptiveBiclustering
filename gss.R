source('shabalin.R')

shabalinSum <- function(X, m, n, cInd = NULL){  # function that returns the normalized mean by LAS algorithm
  result <- shabalin(X, m, n, cInd)
  x <- sum(X[result$Rows, result$Cols]) / sqrt(length(result$Rows) * length(result$Cols))
  return(list('nmean' = x, 'Rows' = result$Rows, 'Cols' = result$Cols))
}

goldenSecSearch <- function(X, msup, nsup){
  phi <- 0.618
  mmin <- 1
  nmin <- 1
  mmax <- msup
  nmax <- nsup
  m <- numeric(2)
  n <- numeric(2)
  fmx <- matrix(NA, 2, 2)
  repeat{
    m[1] <- ceiling(mmax + (mmin - mmax) * phi)
    m[2] <- floor(mmin + (mmax - mmin) * phi)
    n[1] <- ceiling(nmax + (nmin - nmax) * phi)
    n[2] <- floor(nmin + (nmax - nmin) * phi)
    for (i in 1:2){
      for (j in 1:2){
        fmx[i, j] <- shabalinSum(X, m[i], n[j])$nmean
      }
    }
    indi <- which(fmx == max(fmx))[1]
    mmax <- m[2] + (mmax - m[2]) * (1 - indi %% 2)
    mmin <- mmin + (m[1] - mmin) * (1 - indi %% 2)
    nmax <- n[2] + (nmax - n[2]) * (indi > 2)
    nmin <- nmin + (n[1] - nmin) * (indi > 2)
    if (mmax - mmin < 4 & nmax - nmin <4) break
  }
  sollist <- vector("list", (mmax - mmin) * (nmax - nmin))
  llh <- numeric((mmax - mmin) * (nmax - nmin))
  for (i in mmin : mmax){
    for (j in nmin : nmax){
      sollist[(i - mmin) * (nmax - nmin) + j - nmin + 1] <- list(shabalinSum(X, i, j))
      llh[(i - mmin) * (nmax - nmin) + j - nmin + 1] <- sollist[[(i - mmin) * (nmax - nmin) + j - nmin + 1]]$nmean
    }
  }
  final = sollist[[which(llh == max(llh))]]
  return(list('Rows' = final$Rows, 'Cols' = final$Cols))
}