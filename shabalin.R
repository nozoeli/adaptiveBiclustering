shabalin = function(X, m, n, cInd=NULL) {
	# Local hill-climbing algorithm for finding a submatrix with largest sum
  # From Shabalin et al (2009)
  # X : real-valued matrix
	# m, n : dimensions of the submatrix
	# cInd : starting column indices (random by default)
  	M = nrow(X)
  	N = ncol(X)
  	if (is.null(cInd)) {
  		cSum = colSums(X)
     	cInd = sort(cSum, d=TRUE, ind=TRUE)$ix[1:n] 		
	  	}
  	repeat {
    	rSum = X[,cInd] %*% rep(1,n)
    	rInd = sort(rSum, d=TRUE, ind=TRUE)$ix[1:m]
    	cSum = rep(1,m) %*% X[rInd,]
    	cIndNew = sort(cSum, d=TRUE, ind=TRUE)$ix[1:n]
    	if (identical(cIndNew, cInd)) break 
    	cInd = cIndNew
  		}
  	return(list('Rows' = rInd,'Cols' = cInd))
	}

largestSbmxShabalin <- function(X, m, n, iter = 100){
  # The function finds a m*n submatrix with largest entry sum
  # The method is Shabalin(2009) with random initial start to prevent local maximal
  # The function returns the sum of the submatrix
  M <- nrow(X)
  N <- ncol(X)
  base <- 0
  for (i in 1 : iter){
    r_temp <- shabalin(X, m, n, cInd = sample(x = 1 : N, n))  # random initial column index, preventing local maximal
    sum_temp <- sum(X[r_temp$Rows, r_temp$Cols])
    base <- base + (sum_temp - base) * (sum_temp > base)  # find the m*n matrix with largest entry sum
  }
  return(base)
}

