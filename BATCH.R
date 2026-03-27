library(yaml)
source("CohortFunctions.R")
config <- read_yaml("config.yaml")
state <- config$settings$state
county <- config$settings$county
years <- c(config$settings$year1,config$settings$year2,config$settings$year3)
deathDir <- config$settings$deathDir
birthDir <- config$settings$birthDir

ACSData <- getACSYears(state,county,years)

drates <- deathRates(ACSData,deathDir)
brates <- birthRates(ACSData,birthDir)
mrates <- migrationRates(ACSData,birthDir,deathDir)

base_pop = getAgeSex(state,county,year=2022)

births <- birth(population = base_pop,birthrates=brates)
deaths <- death(population=base_pop,deathrates=drates)
migrations <- migration(population = base_pop,mig = mrates)
promotion(births=births,deaths=deaths,migration=migrations,existing=base_pop)
