generate_func<-function(x,y){
  return(stepfun(x, y, f = as.numeric(0), ties = "ordered",right = FALSE))
}
