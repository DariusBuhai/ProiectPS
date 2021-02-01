
is_positive <- function(f, lo = -10000000, hi=10000000){
  vals <- seq(lo, hi, len=100000)
  all(f(vals)>=0)
}

safe_integrate <- function(f){
  tryCatch(integrate(Vectorize(f), -Inf, Inf),
           error = function(e){
             FALSE
           })
}

check_pdf <- function(f){
  # Check if the function is positive
  if(!is_positive(f))
    return(FALSE)
  # Safe integrate, handle errors
  i = safe_integrate(f)
  # Cannot integrate
  if(typeof(i)=='logical' && i==FALSE)
    return(FALSE)
  v = i $ value
  round(v) == 1
}
