
##Estimates single sex death vector

death <- function(population,deathrates){
  return(round(population*deathrates,0))
}