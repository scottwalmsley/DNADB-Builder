
ppmErr =  function(m,m0){
  (m-m0)/m0 * 1e6
  
}

tol = function(m,ppm){
  dM = ppm*m/1e6
  c(m-dM,m+dM)
}

getMassTolRange <- function(m,ppm){
  
  dM <- ppm*m/1e6
  
  return(c(m-dM,m+dM))
  
}
