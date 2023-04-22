
mean_fn <- function(var){
  round(mean(var,na.rm=T),digits=3)
}

stdev_prop <- function(p,n) {
  sqrt((p*(1-p))/n)
}