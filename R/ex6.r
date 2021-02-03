### Cerinta 6: Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are o repartiție continuă cunoscută
## iar g este o funcție continuă precizată de utilizator.

source("integrate.R")
source("dispersion.R")
ex6 <- function(g, fx) {
  # y = g(X) e o noua variabila aleatoare, ii calculez media
  # folosesc formula pt media functiilor de x
  e_y <- safe_integrate(function(x)(h(x) *fx(x)))
}
