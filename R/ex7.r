####
# Cerinta: Crearea unei funcții P care permite calculul diferitelor tipuri de
# probabilități asociate unei variabile aleatoare continue(similar funcției P din pachetul discreteRV)
#
# Header functie: myP(f, p)
#    - unde f este o functie densitate de probabilitate (pdf)
#    - iar p este un string ce reprezinta probabilitatea (conditionata sau independenta).
#
# Obligatoriu, var se va afla in stanga operatorului
# Exemple: todo
####

 g <- function (x) {
      fun <- 0.1*(3*(x^2) + 1)
      fun[x<0] = 0
      fun[x>2]=0
      return ( fun )
 }

library(sjmisc)
myP <- function(f, p) {
  operatii_posibile=c("<=",">=","=","<",">")


  ############## FUNCTII HELPER #############

  parseaza_expresie <- function(expresie) {

    # scot whitespace
    expresie <- gsub(" ", "", expresie)

    for(op in operatii_posibile) {

      # am dat split corect => in stanga am variabila, in dreapta am bound-ul
      split <- unlist(strsplit(expresie, op, fixed = TRUE))
      splitSize <- length(split)

      if (splitSize == 2) {
        # returnez (v.a.c, operatie, bound)
        return (c(split[1],op,split[2]))
      }
    }
    return(c(-1))


  }

## Calculeaza probabilitatea ##
evalueaza <- function(operator, bound) {

  bound = switch(bound,
                 "-Inf" = -Inf,
                 "+Inf" = +Inf,
                 as.double(bound))

  integrala <- integrate(f, -Inf, bound) $ value
  print(operator)
  print(bound)
  return (integrala)
  ans <- switch(
    operator,
    "=" = 0,
    "<=" = integrala,
    "<" = integrala,
    ">=" = 1 - integrala,
    ">" = 1 - integrala)

  return(ans)

  }


prob_independenta <- function(expresie) {

  parametri <- parseaza_expresie(expresie)

  if(length(parametri) != 3)
    return("Eroare la parsarea probabilitatii")

  # aici presupun ca expresiile mele sunt mereu de forma x operator bound
  # artrebui o verificare, poate, a ordinii

  operator <- parametri[2]
  bound <- parametri[3]

  print(evalueaza(operator, bound))

}
  ############ END functii helper ###########

 ############ functie main ##################

  # parsez parametri


  parti = unlist(strsplit(p, "|", fixed = TRUE))
  len = paste(length(parti))
  switch(len,
         "0" = return("Eroare"),
         "1" = return(prob_independenta(p)),
         "2" = return("2 argumete"),
         )
  return ("eroare");

  ########### END functie main ##############
}
