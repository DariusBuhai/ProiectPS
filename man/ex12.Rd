\name{ex11}
\alias{convolution_sum and convolution_diff}
\title{Sum and difference of 2 pdfs}
\usage{
  convolution_sum(fx, fy)
  convolution_diff(fx, fy)
}
\description{
  Construirea sumei și diferenței a două variabile aleatoare continue independente folosind formula de convoluție
}
\examples{

  fx <- function(t) dnorm(t, 0.5, 0.9)
  fy <- function(t) dlnorm(t, 1.2, 0.99)

  convolution_sum(fx, fy)
  convolution_diff(fx, fy)
}
