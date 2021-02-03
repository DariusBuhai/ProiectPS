# suma a doua variabile aleatoare continue independente folosind formula de convolutie

sumaConvolutie <- function(fx,fy) {
  res <-function(z) {
    integrate(function(y) {
      fx(z-y) * fy(y)
      },-Inf,Inf) $ value
  }
    return(res)
}
# diferenta a doua variabile aleatoare continue independente folosind formula de convolutie

difConvolutie <- function(fx,fy) {
  res <-function(z) {
    integrate(function(y) {
      g(y-z)*f(y)
    },-Inf,Inf) $ value
  }
  return(res)
}


