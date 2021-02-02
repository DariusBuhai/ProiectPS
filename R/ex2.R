# Example
# f <- function(x){
#   if (x > 0 && x < 2){
#      3/8 * (4*x-2*x^2)
#   }else{
#      0
#   }
# }
#
# check_pdf(f)

# Check if a function is positive in a given interval
is_positive <- function(f, lo = -10000000, hi=10000000){
  vals <- seq(lo, hi, len=100000)
  all(f(vals)>=0)
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
