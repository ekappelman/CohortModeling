
##Estimates total births by sex when supplied female populations, birth rates, and sex ratio

births <- function(females,birthrates,femalepercentage=0.49){
  if(length(females)!=length(birthrates)){
    return("Birth rates and population vectors are different lengths")
  }
  females <- round(sum(females * birthrates)*femalepercentage,0)
  males <- round(sum(females * birthrates) - females,0)
  return(data.frame(Females=females,Males=males))
}