### Cerinta 6: Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are o repartiție continuă cunoscută
## iar g este o funcție continuă precizată de utilizator.

medium <- function(f, d = c(-Inf, Inf)){
  integrate(function(x){ x * f(x)}, d[1], d[2]) $ value
}

ex6 <- function(g, fx) {
  # y = g(X) e o noua variabila aleatoare, ii calculez media
  # folosesc formula pt media functiilor de x
  gx <- g(fx)
  e_y <- integrate(function(x)(h(x) *fx(x)))
  # pt dispersie, folosesc compunerea celor 2 functii si media calculata inainte
  new_f<-(x-e_y)^2
  dispersie <-medium(new_f)
}
