library(tidycensus)
library(tidyverse)
load("ACSVariableNames.RData")


getAgeSex <- function(state,county,year){
  if(year>2016){
  ACS <- get_acs(geography = "county",table="S0101",state=state,
     county=county,year=year,survey = "acs5",cache_table = TRUE)
  ACS <- merge(ACS,NewNames,by.x="variable",by.y="Var")
  ACS <- ACS[,c("estimate","Cohort","Sex")]
  ACS$Year=year
  return(ACS)
  }else{
    ACS <- get_acs(geography = "county",table="S0101",state=state,
     county=county,year=year,survey = "acs5",cache_table = TRUE)
    maleTotal <- ACS$estimate[ACS$variable=="S0101_C02_001"]
    femaleTotal <- ACS$estimate[ACS$variable=="S0101_C03_001"]
    ACS <- merge(ACS,OldNames,by.x="variable",by.y="Var")
    ACS <- ACS[,c("estimate","Cohort","Sex")]
    ACS$Year=year
    ACS$tempest <- round(ACS$estimate/100*c(rep(maleTotal,18),rep(femaleTotal,18)),0)
    ACS$estimate <- ACS$tempest
    ACS <- ACS[,-5]
        return(ACS)
  }
}
  
getACSYears <- function(state,county,years){
  AgeSexData <- getAgeSex(state,county,years[1]) %>%
    rbind(getAgeSex(state,county,years[2])) %>%
    rbind(getAgeSex(state,county,years[3])) %>%
    return()
}






