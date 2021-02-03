
# folosind definitia
get_CDF_from_PDF <- function (f, x) {
  tryCatch ({
      integral(Vectorize(f), 0, x)
    }, error = function (e) {
      # print(e)
    }
  )
}

plot_density_or_repartition <- function(f, a, b) {
  # verific prima conditie
  for (x in seq(a, b, 0.01)) {
    if (f(x) < 0) {
      print(paste("pdf nu poate sa aiba valori negative",
                  "dar am obtinut", f(x), "in", x, sep=" "))
      return()
    }
  }

  # verific a doua conditie
  total <- integral(Vectorize(f), max(-Inf, a), min(b, +Inf))
  if (total != 1) {
    print(paste("probabilitatea cumulata totala trebuie sa fie egala cu 1",
                "dar am obtinut", total, sep=" "))
    return()
  }

  # pdf a fost validata
  # afisez graficul pdf

  xs <- seq(a, b, 0.01)
  ys <- c()
  for (x in xs) {
    ys = append(ys, f(x))
  }
  plot(xs, ys, type="l", main="PDF", col="red", xlab="x", ylab="y")

  # calculez si afisez graficul CDF

  ys <- c()
  for (x in xs) {
    ys = append(ys, get_CDF_from_PDF(f, x))
  }
  plot(xs, ys, col="red", type="l", main="CDF", xlab="x", ylab="y")
}

parse_known_repartition <- function(name, ...) {
  params <- list(...)
  if (name == "uniform") {
    if (!is.null(params$a) && !is.null(params$b)) {
      a <- params$a
      b <- params$b
      if (a >= b) {
        return("parametri invalizi")
      }

      f <- function(x) 1 / (b - a)
      plot_density_or_repartition(f, a, b)
    }
    else {
      return("parametrii necesari nu au fost pasati")
    }
  }
  else if (name == "exp") {
    if (!is.null(params$lambda)) {
      lambda <- params$labmda
      if (labmda <= 0) {
        return("parametru invalizi")
      }

      f <- function(x) (lambda * exp(1)^(-lambda * x))
      plot_density_or_repartition(f, 0, Inf)
    }
    else {
      return("parametrii necesari nu au fost pasati")
    }
  }
}

parse_known_repartition("uniform", a=0, b=10)
parse_known_repartition("exp", lambda=2)
