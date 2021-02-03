# suma a doua variabile aleatoare continue independente folosind formula de convolutie
convolution_sum <- function(fx,fy) {
  res <-function(z) {
    integrate(function(y) {
      fx(z-y) * fy(y)
      },-Inf,Inf) $ value
  }
    return(res)
}

# diferenta a doua variabile aleatoare continue independente folosind formula de convolutie
convolution_diff <- function(fx,fy) {
  res <-function(z) {
    integrate(function(y) {
      g(y-z)*f(y)
    },-Inf,Inf) $ value
  }
  return(res)
}
