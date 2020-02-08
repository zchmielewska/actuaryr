#' Random numbers from the normal distribution
#'
#' The function returns random numbers from the normally distributed population.
#'
#' @param sample_size the size of the sample to be returned
#' @param mean the mean of the normal distribution
#' @param variance the variance of the normal distribution
#'
#' @return Random numbers from the normally distributed population.
#' @export
#'
#' @examples
#' rn_norm(sample_size = 5, mean = 2, variance = 3)
rn_norm <- function(sample_size, mean, variance) {
  result <- stats::rnorm(n = sample_size, mean = mean, sd = base::sqrt(variance))
  return(result)
}

#' Probability distribution function of the normal distribution
#'
#' The function returns the value on the probability distribution function
#' (or density function) of the normal distribution.
#'
#' @param k the argument of the probability distribution function
#' @param mean the mean of the normal distribution
#' @param variancethe variance of the normal distribution
#'
#' @return The value of the probability distribution function.
#' @export
#'
#' @examples
#' pdf_norm(k = 3, mean = -1, variance = 5)
pdf_norm <- function(k, mean, variance) {
  result <- stats::dnorm(x = k, mean = mean, sd = base::sqrt(variance))
  return(result)
}

#' Cumulative distribution function of the normal distribution
#'
#' The function returns the value on the cumulative distribution function
#' of the normal distribution.
#'
#' The result expresses the probability that the random variable
#' from this distribution is lower or equal to the given value k,
#' i.e. P(X <= k).
#' It is also equal to the area under the probability distribution function
#' on the range [-Inf, k].
#'
#' @param k the argument of the cumulative distribution function
#' @param mean the mean of the normal distribution
#' @param variance the variance of the normal distribution
#'
#' @return The value of the cumulative distribution function.
#' @export
#'
#' @examples
#' cdf_norm(k = 3, mean = -1, variance = 5)
cdf_norm <- function(k, mean, variance) {
  result <- stats::pnorm(q = k, mean = mean, sd = base::sqrt(variance))
  return(result)
}

#' Random numbers from the continuous uniform distribution
#'
#' The function returns random numbers from the population with the continuous
#' uniform distribution.
#'
#' @param sample_size the size of the sample to be returned
#' @param a the beginning of the range
#' @param b the end of the range
#'
#' @return Random numbers from the population with continuous uniform distribution.
#' @export
#'
#' @examples
#' rn_cunif(sample_size = 5, a = 1, b = 3)
rn_cunif <- function(sample_size, a, b) {
  result <- stats::runif(n = sample_size, min = a, max = b)
  return(result)
}

#' Probability distribution function of the continuous uniform distribution
#'
#' The function returns the value on the probability distribution function
#' (or density function) of the continuous uniform distribution.
#'
#' @param k the argument of the probability distribution function
#' @param a the beginning of the range
#' @param b the end of the range
#'
#' @return The value on the probability distribution function.
#' @export
#'
#' @examples
#' pdf_cunif(k = 3, a = 1, b = 5)
pdf_cunif <- function(k, a, b) {
  result <- stats::dunif(x = k, min = a, max = b)
  return(result)
}

#' Cumulative distribution function of the continuous uniform distribution
#'
#' The function returns the value on the cumulative distribution function
#' of the continuous uniform distribution.
#'
#' The result expresses the probability that the random variable
#' from this distribution is lower or equal to the given value k,
#' i.e. P(X <= k).
#' It is also equal to the area under the probability distribution function
#' on the range [-Inf, k].
#'
#' @param k the argument of the cumulative distribution function
#' @param a the beginning of the range
#' @param b the end of the range
#'
#' @return The value on the cumulative distribution function.
#' @export
#'
#' @examples
#' pdf_cunif(k = 3, a = 1, b = 5)
cdf_cunif <- function(k, a, b) {
  result <- stats::punif(q = k, min = a, max = b)
  return(result)
}


#' Random numbers from the discrete uniform distribution
#'
#' The function returns random numbers from the population with the discrete
#' uniform distribution.
#'
#' @param sample_size the size of the sample to be returned
#' @param a the beginning of the range
#' @param b the end of the range
#'
#' @return Random numbers from the population with the discrete uniform
#' distribution.
#' @export
#'
#' @examples
rn_dunif <- function(sample_size, a, b) {
  result <- base::sample(x = base::seq(from = a, to = b), size = sample_size, replace = TRUE)
}

#' Probability mass function of the discrete uniform distribution
#'
#' The function returns the value on the probability mass function.
#'
#' The result expresses the probability that the random variable from this
#' distribution amounts to the given value k, i.e. P(X=k).
#'
#' @param k the argument of the probability mass function
#' @param a the beginning of the range
#' @param b the end of the range
#'
#' @return The value on the probability mass function.
#' @export
#'
#' @examples
#' pmf_dunif(k = 2, a = 1, b = 3)
pmf_dunif <- function(k, a, b) {
  if(b < a) stop("b should be higher or equal to a")

  if(a <= k & k <= b) {
    n <- b - a + 1
    result <- 1/n
  } else {
    result <- 0
  }
  return(result)
}

#' Cumulative distribution function of the discrete uniform distribution
#'
#' The function returns the value on the cumulative distribution function
#' of the discrete uniform distribution.
#'
#' The result expresses the probability that the random variable
#' from this distribution is lower or equal to the given value k,
#' i.e. P(X <= k).
#' It is also equal to the area under the probability distribution function
#' on the range [-Inf, k].
#'
#' @param k the argument of the cumulative distribution function
#' @param a the beginning of the range
#' @param b the end of the range
#'
#' @return
#' @export
#'
#' @examples
cdf_dunif <- function(k, a, b) {
  if(b < a) stop("b should be higher or equal to a")

  if(k < a) {
    result <- 0
  } else if (a <= k & k <= b) {
    n <- b - a + 1
    result <- (base::floor(k) - a + 1)/n
  } else {
    result <- 1
  }
  return(result)
}
