#* @get /matrix
matrixfun <- function(){
  
  mission <- c("Taxi","Medical","Commuter",	"Cargo",	"Recreation")
  
  
  power <- c("FossilFuel",	"Electric",	"Hybrid")
  
  mat <- expand.grid(mission,power)
}

