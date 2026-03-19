
##Promotion function takes the a single sex vector and promotes and applies deaths
##births and migration that is calculated elsewhere

promotion <- function(births,deaths,migration,existing){
  promotion <- c(births,existing[1:16],sum(existing[17:18]))
  newGen <- promotion-deaths+migration
  return(newGen)
}

