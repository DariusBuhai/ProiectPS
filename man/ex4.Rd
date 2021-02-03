\name{ex4}
\alias{plot_repartitions}
\title{Plotting for different repartitions}
\usage{
  parse_known_repartition(type, is_CDF, *params)
  plot_density(f, a, b)
  plot_generic_repartition(f, a, b)
}
\description{
  Reprezentarea grafică a densității și a funcției de repartiție pentru diferite tipuri de repartitie.
  Tipurile de repartitie pot fi standard / cunoscute, sau oarecare.
}
\examples{
  parse_known_repartition("uniform", FALSE, a=0, b=10)
  parse_known_repartition("uniform", TRUE, a=0, b=10)
  parse_known_repartition("exp", FALSE, lambda=2)
  parse_known_repartition("exp", TRUE, lambda=2)
  parse_known_repartition("normal", FALSE, mu=0, sigma=1)
  parse_known_repartition("normal", TRUE, mu=0, sigma=1)
  parse_known_repartition("pareto", FALSE, m=3, alpha=1)
  parse_known_repartition("pareto", TRUE, m=3, alpha=1)
  plot_generic_repartition(function(x) x / 2, 0, 2)
}
