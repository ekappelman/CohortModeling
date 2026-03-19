
##This returns a single sex vector of migration from populations


migration <- function(population,mig,rates=TRUE){
  if(length(population)!=length(mig.rates)){
    return("Population vector and migration rates vectors are different lengths")
  }
  if(rates){
    return(round(population*mig,0))
  }else{
    return(mig)
  }
}