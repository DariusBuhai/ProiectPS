# Example
# f1 <- function (x, y) {
#   return (3/2 * (x^2+y^2))
# }
#
# covariance_and_corelation(f1, c(0,1), c(0, 1))

# Extracts x out of common density of x and y
extractXMarginal <- function(f, dx){
  Vectorize(function(y){
    integrate(function(x){f(x, y)}, dx[1], dx[2]) $ value
  })
}

# Extracts y out of common density of x and y
extractYMarginal <- function(f, dy){
  Vectorize(function(x){
    integrate(function(y){f(x, y)}, dy[1], dy[2]) $ value
  })
}

# Double integrates x and y
integrateXY <- function(f, dx, dy){
  integrate(
    Vectorize(function(y){
      integrate(function(x){f(x, y)}, dx[1], dx[2]) $ value
    })
  , dy[1], dy[2]) $ value
}

# Calculates median
median <- function(f, d){
  integrate(function(x){ x * f(x)}, d[1], d[2]) $ value
}

# Calculates median
covariance_and_corelation <- function(pdf, dx, dy){

  fx = extractXMarginal(pdf, dx)
  fy = extractYMarginal(pdf, dy)

  mx = median(fx, dx)
  my = median(fy, dy)

  cov = integrateXY(function(x,y){x*y*pdf(x,y)}, dx, dy) - (mx*my)

  cor = cov / (mx*my)

  list(cov = cov, cor = cor)
}
