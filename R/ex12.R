# suma a doua variabile aleatoare continue independente folosind formula de convolutie

convolution_sum <- function(fx,fy) {
  res <-function(z) {
    integrate(function(y) {
      fx(z-y) * fy(y)
      },-Inf,Inf)
  )
}

# diferenta a doua variabile aleatoare continue independente folosind formula de convolutie
convolution_diff <- function(fx,fy) {
  res <-function(z) {
    integrate(function(y) {
      g(y-z)*f(y)
    },-Inf,Inf) $ value

  }
}


f_1 <- function(x)(pnorm(x,mean=1))
f_2 <- function(x) (pnorm(x,mean=2))
f_3 <- Vectorize(convolution_sum(f_1, f_2))
f_4 <- Vectorize(convolution_diff(f_1,f_2))
frez <- Vectorize(f_3)

t= seq(1,10)
plot(f_1,from=-10,to=10,type="l")
plot(f_2,from=-10,to=10,type="l")
plot(f_3(t),from=-10,to=10,type="l")
plot(f_4(t),from=-10,to=10,type="l")

#f_1 <- function(x)(pnorm(x,mean=1))
#f_2 <- function(x) (pnorm(x,mean=2))

