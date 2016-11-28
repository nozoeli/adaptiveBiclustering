source('shabalin.R')

shabalinSum <- function(X, m, n, cInd = NULL){  # function that returns the normalized mean by LAS algorithm
  result <- shabalin(X, m, n, cInd)
  x <- sum(X[result$Rows, result$Cols]) / sqrt(length(result$Rows) * length(result$Cols))
  return(list('nmean' = x, 'Rows' = result$Rows, 'Cols' = result$Cols))
}

goldenSecSearch <- function(X, msup, nsup){  # main golden section search algorithm
  phi <- 0.618
  mmin <- 1
  nmin <- 1
  mmax <- msup
  nmax <- nsup
  m <- numeric(2)
  n <- numeric(2)
  fmx <- matrix(NA, 2, 2)
  repeat{
    m[1] <- ceiling(mmax + (mmin - mmax) * phi)  # candidate dimension indexes
    m[2] <- floor(mmin + (mmax - mmin) * phi)
    n[1] <- ceiling(nmax + (nmin - nmax) * phi)
    n[2] <- floor(nmin + (nmax - nmin) * phi)
    for (i in 1:2){
      for (j in 1:2){
        fmx[i, j] <- shabalinSum(X, m[i], n[j])$nmean  # calculate all the combinations and its corresponding function value
      }
    }
    indi <- which(fmx == max(fmx))[1]  # [1] is for the case when there are multiple maximals, especially when one dimension is degenerated to 3
    mmax <- m[2] + (mmax - m[2]) * (1 - indi %% 2)  # choose the search frame of the next step
    mmin <- mmin + (m[1] - mmin) * (1 - indi %% 2)
    nmax <- n[2] + (nmax - n[2]) * (indi > 2)
    nmin <- nmin + (n[1] - nmin) * (indi > 2)
    if (mmax - mmin < 4 & nmax - nmin <4) break  # when both sides are degenerated to 3, break out
  }
  sollist <- vector("list", (mmax - mmin) * (nmax - nmin))
  llh <- numeric((mmax - mmin) * (nmax - nmin))
  for (i in mmin : mmax){  # search all the possible combinations for the remaining candidate dimensions
    for (j in nmin : nmax){
      sollist[(i - mmin) * (nmax - nmin) + j - nmin + 1] <- list(shabalinSum(X, i, j))
      llh[(i - mmin) * (nmax - nmin) + j - nmin + 1] <- sollist[[(i - mmin) * (nmax - nmin) + j - nmin + 1]]$nmean
    }
  }
  final = sollist[[which(llh == max(llh))]]  # take out the one which produces the largest normalzed mean
  return(list('Rows' = final$Rows, 'Cols' = final$Cols))
}