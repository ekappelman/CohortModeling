library(yaml)
source("CohortFunctions.R")
config <- read_yaml("config.yaml")
state <- config$settings$state
county <- config$settings$county
years <- c(config$settings$year1,config$settings$year2,config$settings$year3)
deathDir <- config$settings$deathDir
birthDir <- config$settings$birthDir

drates <- deathRates(state,county,years,deathDir)
brates <- birthRates(state,county,years,birthDir)
mrates <- migrationRates(state,county,years,birthDir,deathDir)

birth(population = getAgeSex(state,county,year=2019),birthrates = brates)

promotion()