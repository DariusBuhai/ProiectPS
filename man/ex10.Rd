\name{ex10}
\alias{covariance_and_correlation}
\title{Covariance and Corelation coefficient}
\usage{
  covariance_and_correlation(pdf, dx, dy)
}
\description{
  Calculul covariantei si coeficientului de corelatie pentru doua variabile aleatoare continue
}
\examples{
  f <- function (x, y) {
     return (3/2 * (x^2+y^2))
  }
  covariance_and_correlation(f, c(0,1), c(0,3))
}
