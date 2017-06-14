source('shabalin.R')
lambda <- function(m, n, M, N){
  return(sqrt(2 * (log(M * N) + m * log(M / m) + n * log(N / n))))
}

adaptiveShabalin <- function(X, m, n, cInd = NULL){
  # This is an adaptive form of Shabalin's method of finding the anomaly
  # The method is inspired by hill-climbing the likelihood funcition of a joint normal density
  # m and n are the initial guess of the anomaly size
  # cInd indicates the initialization of the first iteration.
  M <- nrow(X)
  N <- ncol(X)
  iniInd <- shabalin(X, m, n, cInd)  # initialize the indices by Shabalin's hill-climbing method
  cInd <- iniInd$Cols
  rInd <- iniInd$Rows
  cNum <- n
  rNum <- m
  repeat {
    rSum <- sort(X[, cInd] %*% rep(1, cNum), d = T, ind = T)  # sum the entries in the anomaly colunms
    rCom <- cumsum(rSum$x) / sqrt(1 : M) - lambda(1 : M, length(cInd), M, N) # calculate the normalized mean for each possible column number
    #drCom <- c(sign(diff(rCom)), -1)  # calculate the trend of the normalized mean when the column number increasing 
    #r0 <- min(which(drCom[rNum : M] == -1)) + rNum - 1
    r0 <- which(rCom == max(rCom))
    # The smallest rSum such that is less than the comparison, so that all sums larger than that is included
    rIndnew <- rSum$ix[1 : r0] 
    rNum <- length(rIndnew)  # renew the row indices
    cSum <- sort(rep(1, rNum) %*% X[rIndnew ,], d = T, ind = T)
    cCom <- cumsum(cSum$x) / sqrt(1 : N) - lambda(length(rIndnew), 1 : N, M, N)
    #dcCom <- c(sign(diff(cCom)), -1)
    #c0 <- min(which(dcCom[cNum : N] == -1)) + cNum - 1
    c0 <- which(cCom == max(cCom))
    cIndnew <- cSum$ix[1 : c0]
    #rNum <- length(rIndnew)
    cNum <- length(cIndnew)
    #newInd <- shabalin(X, rNum, cNum, cInd = cIndnew)
    #cIndnew <- newInd$Cols  # clean the procedure by local hill-climbing under the new anomaly size
    #rIndnew <- newInd$Rows
    if (identical(cInd, cIndnew) & identical(rInd, rIndnew)) break
    cInd <- cIndnew
    rInd <- rIndnew
  }
  return(list('Rows' = rInd, 'Cols' = cInd))
}

largestAdaptiveShabalin <- function(X, m, n, iter = 50){
  M <- nrow(X)
  N <- ncol(X)
  base <- 0
  sollist <- vector("list", iter) 
  llh <- numeric(iter)
  for (i in 1 : iter){
    result <- adaptiveShabalin(X, m, n, cInd = sample(1:N, size = n))
    sollist[i] <- list(result)
    theta <- sum(X[result$Rows, result$Cols]) 
    llh[i] <- theta^2 / (length(result$Rows) * length(result$Cols))
    base <- base + (llh[i] - base) * (llh[i] > base)
  }
  final <- sollist[[min(which(llh == base))]] # in case there are ties in the iteration
  return(list('Rows' = final$Rows, 'Cols' = final$Cols))
}

#---------------Comparison to svd method-------------------
svdBicluster <- function(X){
  s <- svd(X)
  rInd <- kmeans(s$u[, 1], 2)$cluster - 1
  cInd <- kmeans(s$v[, 1], 2)$cluster - 1
  return(list('Rows' = rInd, 'Cols' = cInd))
}

#--------------Comparison to largest marginal gap method-------
lmgBicluster <- function(X){
  rSum <- apply(X, 1, sum)
  cSum <- apply(X, 2, sum)
  rInd <- sort(rSum, d=TRUE, ind=TRUE)
  cInd <- sort(cSum, d=TRUE, ind=TRUE)
  rBreak <- which(diff(rInd$x) == min(diff(rInd$x)))
  cBreak <- which(diff(cInd$x) == min(diff(cInd$x)))
  return(list('Rows' = rInd$ix[1 : rBreak], 'Cols' = cInd$ix[1 : cBreak]))
}