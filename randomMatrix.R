normalMatrix = function(M, N, m, n, theta){
	# M, N : dimensions of the matrix
	# m, n : dimensions of the submatrix
	# theta : positive elevated mean
	# Creates a random matrix where the entries are independent normal with unit variance and mean 0, except for the top-left submatrix (of given size) where the mean is theta
	A = matrix(rnorm(M*N), M, N)
	A[1:m, 1:n] = A[1:m, 1:n] + theta
	return(A)
	}


poissonMatrix <- function(M, N, m, n, theta){
  # Generalize centered poisson distribution according to the standard exponential model
  # M, N : dimension of the matrix
  # m, n : dimension of the submatrix
  # theta stands for the parameter of the elevated matrix.
  # The base distribution is Pois(1) - 1
  # The distribution with parameter theta is Pois(e^theta) - 1
  mu <- exp(theta)
  mx1 <- matrix(1, M, N)
  if (m * n != 0){
    mx1[1:m, 1:n] <- matrix(mu, m, n)}
  mx <- matrix(rpois(M * N, as.vector(mx1)), M, N) - 1
  return(mx)
}

radeMatrix <- function(M, N, m, n, theta){
  # Generalize Rademacher distribution according to the standard exponential model
  # M, N : dimension of the matrix
  # m, n : dimension of the submatrix
  # theta stands for the parameter of the elevated matrix. 
  # The base distribution is Rade(0.5)
  # The distribution with parameter theta is Rade(e^theta / e^theta + e^-theta)
  mu <- exp(theta) / (exp(theta) + exp(-theta))
  mx1 <- matrix(0.5, M, N)
  if (m * n != 0){
    mx1[1:m, 1:n] <- matrix(mu, m, n)}
  mx <- 2 * matrix(rbinom(M * N, 1, as.vector(mx1)), M, N) - 1
  return(mx)
}
