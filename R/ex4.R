
# integreaza pdf ca sa obtin valoarea cdf
get_CDF_from_PDF <- function (f, x) {
  tryCatch ({
      integrate (f, 0, x)$value
    },
    error = function (e) {
      print(e)
    }
  )
}

# verific daca functia de densitate este valida
# trebuie sa fie pozitiva
# probabilitatea totala trebuie sa fie 1 -> sigur se intampla ceva
check_density <- function(f, a, b) {
  # verific prima conditie
  for (x in seq(a, b, 0.1)) {
    if (f(x) < 0) {
      print(paste("pdf nu poate sa aiba valori negative",
                  "dar am obtinut", f(x), "in", x, sep=" "))
      return(FALSE)
    }
  }

  # verific a doua conditie
  total <- integral(Vectorize(f), max(-Inf, a), min(b, +Inf))
  if (abs(total - 1) > 0.1) {
    print(paste("probabilitatea cumulata totala trebuie sa fie egala cu 1",
                "dar am obtinut", total, sep=" "))
    return(FALSE)
  }

  return(TRUE)
}

# afisaza graficul densitatii
plot_density <- function(f, a, b) {
  # validez pdf
  if (!check_density(f, a, b)) {
    return()
  }

  # pdf a fost validata
  # afisez graficul pdf

  xs <- seq(a, b, 0.1)
  ys <- c()
  for (x in xs) {
    ys = append(ys, f(x))
  }
  plot(xs, ys, type="l", main="PDF", col="red", xlab="x", ylab="y")
}

# o folosesc pentru repartitiile standard carora le stim
# functia de repartitie
# afisaza graficul repartitiei
plot_repartition <- function(F, a, b) {
  # calculez si afisez graficul CDF

  xs <- seq(a, b, 0.01)
  ys <- c()
  for (x in xs) {
    ys = append(ys, F(x))
  }
  plot(xs, ys, col="red", type="l", main="CDF", xlab="x", ylab="y")
}

# o folosesc pentru repartitiile carora
# nu le stim functia de repartitie
# ne folosim de pdf
# afisaza graficul repartitiei
plot_generic_repartition <- function(f, a, b) {
  # validez pdf
  if (!check_density(f, a, b)) {
    return()
  }

  xs <- seq(a, b, 0.01)
  ys <- c()
  for (x in xs) {
    ys = append(ys, get_CDF_from_PDF(f, x))
  }
  plot(xs, ys, col="red", type="l", main="CDF", xlab="x", ylab="y")
}

parse_known_repartition <- function(name, CDF=FALSE, ...) {
  params <- list(...)
  if (name == "uniform") {
    if (!is.null(params$a) && !is.null(params$b)) {
      a <- params$a
      b <- params$b
      if (a >= b) {
        return("parametri nevalizi")
      }

      f <- function(x) 1 / (b - a)
      F <- function(x)(x - a) / (b - a)
      if (CDF) {
        plot_repartition(F, a, b)
      }
      else {
        plot_density(f, a, b)
      }
    }
    else {
      return("parametrii necesari nu au fost pasati")
    }
  }
  else if (name == "exp") {
    if (!is.null(params$lambda)) {
      lambda <- params$lambda
      if (lambda <= 0) {
        return("parametru nevalizi")
      }

      f <- function(x) (lambda * exp(1)^(-lambda * x))
      F <- function(x) (1 - exp(1)^(-lambda * x))
      if (CDF) {
        plot_repartition(F, 0, 50)
      }
      else {
        plot_density(f, 0, 50)
      }
    }
    else {
      return("parametrii necesari nu au fost pasati")
    }
  }
  else if (name == "normal") {
    if (!is.null(params$mu && !is.null(params$sigma))) {
      mu <- params$mu
      sigma <- params$sigma
      if (sigma <= 0) {
        return("parametru nevalid")
      }

      f <- function(x) ((1 / (sigma * sqrt(pi * 2)))*(exp(1)^((-(x - mu)^2)/(2  * sigma ^ 2))))
      F <- function(x) (pnorm(x, mu, sigma))
      if (CDF) {
        plot_repartition(F, -25, 25)
      }
      else {
        plot_density(f, -25, 25)
      }
    }
    else {
      return("parametrii necesari nu au fost pasati")
    }
  }
  else if (name == "pareto") {
    if (!is.null(params$m) && !is.null(params$alpha)) {
      m <- params$m
      alpha <-params$alpha
      if (alpha <= 0 || m <= 0) {
        return("parametrii nevalizi")
      }

      f <- function(x) (alpha * m^alpha) / (x ^ (alpha + 1))
      F <- function(x) (1 - (m ^ alpha) / (x ^ alpha))
      if (CDF) {
        plot_repartition(F, m, m + 50)
      }
      else {
        plot_density(f, m, m + 50)
      }
    }
  }
  else {
    print("repartitie necunoscuta")
  }
}

# Example:
# parse_known_repartition("uniform", FALSE, a=0, b=10)
# parse_known_repartition("uniform", TRUE, a=0, b=10)
# parse_known_repartition("exp", FALSE, lambda=2)
# parse_known_repartition("exp", TRUE, lambda=2)
# parse_known_repartition("normal", FALSE, mu=0, sigma=1)
# parse_known_repartition("normal", TRUE, mu=0, sigma=1)
# parse_known_repartition("pareto", FALSE, m=3, alpha=1)
# parse_known_repartition("pareto", TRUE, m=3, alpha=1)
# plot_generic_repartition(function(x) x / 2, 0, 2)
