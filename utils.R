
mean_fn <- function(var){
  round(mean(var,na.rm=T),digits=3)
}

stdev_prop <- function(p,n) {
  sqrt((p*(1-p))/n)
}

ci_lower_fn <- function(est) {
  est - (t_val * as.name(paste(est,"_sd")))
}

ci_upper_fn <- function(est) {
  est + (t_val * as.name(paste(est,"_sd")))
}
