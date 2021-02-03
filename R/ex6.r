### Cerinta 6: Calculul mediei și dispersiei unei variabile aleatoare g(X), unde X are o repartiție continuă cunoscută
## iar g este o funcție continuă precizată de utilizator.


ex6 <- function(g, fx) {
  # y = g(X) e o noua variabila aleatoare, ii calculez media
  # folosesc formula pt media functiilor de x
  e_y <- integrate(function(x){g(x) *fx(x)},-100,100) $ value
  # pt dispersie, folosesc compunerea celor 2 functii si media calculata inainte
  new_f<-function(x)((x-e_y)^2)
  dispersie <-medium(new_f)
  print(e_y)
  print(dispersie)
}

# metoda alternativa de rezolvare
ex6_2 <- function(g,fx) {
  h<- function(x)(g(fx(x)))
  e_y <- integrate (function(x)(x * h(x)) ,-100,100) $ value
  new_f <- function(x)((x-e_y)^2)
  dispersie<-medium(new_f)
  print(e_y)
  print(dispersie)
}
                    
f1 <- function(x)(x+3)
f2 <- function(x) (1 * exp(1)^(-1 * x))
plot(seq(-10,10,0.1),f2(seq(-10,10,0.1)))
ex6(f1,f2)
ex6_2(f1,f2)
