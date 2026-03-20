
##Finds periods deaths
death <- function(population,deathrates){
  return(round(population/1000*deathrates,0))
}

##Finds period migration
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

##Single sex promortion function
promotion <- function(births,deaths,migration,existing){
  promotion <- c(births,existing[1:16],sum(existing[17:18]))
  newGen <- promotion-deaths+migration
  return(newGen)
}

##Download and organize ACS age-sex cohort data
getAgeSex <- function(state,county,year){
  load("ACSVariableNames.RData")
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

##Download and organize three years of ACS data
getACSYears <- function(state,county,years){
  AgeSexData <- getAgeSex(state,county,years[1]) %>%
    rbind(getAgeSex(state,county,years[2])) %>%
    rbind(getAgeSex(state,county,years[3])) %>%
    return()
}

##Find age-sex specific death rates based on three years of ACS data and deaths
deathRates <- function(state,county,years){
  ACSData <- getACSYears(state,county,years)
  Deaths <- read.csv("DeathData\\Deaths.csv")
  ACSDeaths <- merge(ACSData,Deaths)
  ACSDeaths$Rate <- ACSDeaths$Deaths/(ACSDeaths$estimate/1000)
  Averages <- aggregate(ACSDeaths$Rate,by=list(ACSDeaths$Sex,ACSDeaths$Cohort),FUN=mean)
  colnames(Averages) <- c("Sex","Cohort","DeathRatePer1000")
  cohorts <- c("Under 5","5-9 years","10-14 years","15-19 years","20-24 years",
               "25-29 years","30-34 years","35-39 years","40-44 years",
               "45-49 years","50-54 years","55-59 years","60-64 years",
               "65-69 years","70-74 years","75-79 years","80-84 years","Over 85")
  Averages$Cohort <- factor(Averages$Cohort,levels=cohorts)
  Averages$Sex <- factor(Averages$Sex)
  return(Averages)
}