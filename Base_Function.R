Base2Dec <- function(x, B) {
  result = 0
  
  for (i in 1:(nchar(as.character(x)))){
    
  result = result + round((B^(i-1))*10*(x/(10^i) - floor(x/(10^i))),0)
  x = x - round((10^(i-1))*10*(x/(10^i) - floor(x/(10^i))),0)
  }
  
  return(result)
  
}


Dec2Base <- function(x, B) {
  result = 0
  x=x+1
  while (x>1) {
    
  i  = 1
  
  while (x>B^i) {
    i  = i + 1
  }
  if (x==B^(i - 1)) {
    result = result + 10^i
    x = x - B^i
  } else {
    result = result + 10^(i - 1)
    x = x - B^(i - 1)
    }
  }
  
  return(result)
  
}

Base2Base <- function(x,F,T) {
  return(Dec2Base(Base2Dec(x,F),T))
}