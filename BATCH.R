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

base_pop = getAgeSex(state,county,year=config$settings$ACSbaseyear)

results <- cohort_model(
  name=config$title,
  brates = brates,
  drates = drates,
  mrates = mrates,
  BASE = base_pop,
  START_YEAR = config$settings$startYear,
  END_YEAR = config$settings$endYear
)
