
##Finds periods deaths
death <- function(population,deathrates){
  df <- merge(population,deathrates)
  df$Deaths <- round((df$estimate/1000)*df$DeathRatePer1000,0)
  df <- df[c("Sex","Cohort","Deaths")]
  return(df)
}

##Finds periods deaths
birth <- function(population,birthrates){
  temp <- merge(population[population$Sex=="Female",],birthrates,by.x="Cohort",by.y="Maternal Cohort")
  temp$births <- (temp$estimate/1000)*temp$Rate
  births <- aggregate(temp$births,by=list(temp$`Infant Sex`),FUN=sum)
  colnames(births)<- c("Sex","Total Birth")
  births$`Total Birth` <- round(births$`Total Birth`,0)
  return(births)
}

##Finds period migration
migration <- function(population,mig,rates=TRUE){
  if(rates){
    df <- merge(population,mig)
    df$Migrations <- round((df$estimate/1000)*df$Migration,0)
    df <- df[,c("Cohort","Sex","Migrations")]
    return(df)
  }else{
    df <- merge(population,mig)
    df$Migrations <- round(df$Migration,0)
    df <- df[,c("Cohort","Sex","Migrations")]
    return(df)
  }
}
##Single sex promortion function
promotion <- function(births,deaths,migration,existing){
  dat <- merge(merge(deaths,migration),existing)
  dat <- dat[,c("Sex","Cohort","Deaths","Migrations","estimate")]
  dat$Cohort <- factor(dat$Cohort,levels=c("Under 5","5-9 years","10-14 years",
                "15-19 years","20-24 years","25-29 years","30-34 years","35-39 years",
                "40-44 years","45-49 years","50-54 years","55-59 years","60-64 years",
                "65-69 years","70-74 years","75-79 years","80-84 years","Over 85"))
  
  males <- dat[dat$Sex=="Male",]
  females <- dat[dat$Sex=="Female",]
  
  males <- males[order(males$Cohort),]
  females <- females[order(females$Cohort),]
  
  males$promotion <- c(births$`Total Birth`[births$Sex=="Male"],males$estimate[1:16],sum(males$estimate[17:18]))
  males$newGen <- males$promotion-males$Deaths+males$Migrations
  
  females$promotion <- c(births$`Total Birth`[births$Sex=="Female"],females$estimate[1:16],sum(females$estimate[17:18]))
  females$newGen <- females$promotion-females$Deaths+females$Migration
  
  newGen <- rbind(males,females)

  return(newGen)
}

##Single sex promortion function
findMigration <- function(births,deaths,start,end){
  promotion <- c(births,start[1:16],sum(start[17:18]))
  migration <- end-(promotion-deaths)
  return(migration/(start/1000))
}

##Download and organize ACS age-sex cohort data
getAgeSex <- function(state,county,year){
  library(tidycensus,quietly = TRUE)
  load("ACSVariableNames.RData")
  if(year>2016){
    ACS <- suppressMessages(get_acs(geography = "county",table="S0101",state=state,
                   county=county,year=year,survey = "acs5",cache_table = TRUE))
    ACS <- merge(ACS,NewNames,by.x="variable",by.y="Var")
    ACS <- ACS[,c("estimate","Cohort","Sex")]
    ACS$Year=year
    print(paste0("ACS ",year," download complete"))
    return(ACS)
  }else{
    ACS <- suppressMessages(get_acs(geography = "county",table="S0101",state=state,
                   county=county,year=year,survey = "acs5",cache_table = TRUE))
    maleTotal <- ACS$estimate[ACS$variable=="S0101_C02_001"]
    femaleTotal <- ACS$estimate[ACS$variable=="S0101_C03_001"]
    ACS <- merge(ACS,OldNames,by.x="variable",by.y="Var")
    ACS <- ACS[,c("estimate","Cohort","Sex")]
    ACS$Year=year
    ACS$tempest <- round(ACS$estimate/100*c(rep(maleTotal,18),rep(femaleTotal,18)),0)
    ACS$estimate <- ACS$tempest
    ACS <- ACS[,-5]
    rm(maleTotal,femaleTotal)
    print(paste0("ACS ",year," download complete"))
    return(ACS)
  }
}

##Download and organize three years of ACS data
getACSYears <- function(state,county,years){
  print("Downloading and combining ACS data sets")
  library('tidyverse',quietly = TRUE)
  AgeSexData <- getAgeSex(state,county,years[1]) %>%
    rbind(getAgeSex(state,county,years[2])) %>%
    rbind(getAgeSex(state,county,years[3])) %>%
    return()
}

##Find age-sex specific death rates based on three years of ACS data and deaths
deathRates <- function(ACSData,deathPath){
  Deaths <- read.csv(deathPath)
  print("Calculating death rates")
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
  rm(ACSData,Deaths,ACSDeaths)
  Averages <- Averages[order(Averages$Cohort),]
  Averages <- Averages[order(Averages$Sex),]
  return(Averages)
}

birthRates <- function(ACSData,birthPath){
  births <- read.csv(birthPath)
  dat <- merge(births,ACSData[ACSData$Sex=="Female",],by=c("Cohort","Year"))
  dat$PeriodRate <- dat$Births/(dat$estimate/1000)
  rates <- aggregate(dat$PeriodRate,by=list(dat$Cohort,dat$Sex.x),FUN=mean)
  colnames(rates)<- c("Maternal Cohort","Infant Sex","Rate")
  return(rates)
}

migrationRates <- function(ACSData,birthDir,deathDir){
  births <- read.csv(birthDir)
  totals <- aggregate(births$Births,by=list(births$Sex,births$Year),FUN=sum)
  colnames(totals) <- c("Sex","Year","Births")
  Deaths <- read.csv(deathDir)
  migMale1 <- findMigration(births=totals[totals$Sex=="Male"&totals$Year==years[2],"Births"],
                deaths=Deaths[Deaths$Sex=="Male"&Deaths$Year==years[2],"Deaths"],
                start=ACSData[ACSData$Sex=="Male"&ACSData$Year==years[1],"estimate"],
                end=ACSData[ACSData$Sex=="Male"&ACSData$Year==years[2],"estimate"])
  migMale2 <- findMigration(births=totals[totals$Sex=="Male"&totals$Year==years[3],"Births"],
                            deaths=Deaths[Deaths$Sex=="Male"&Deaths$Year==years[3],"Deaths"],
                            start=ACSData[ACSData$Sex=="Male"&ACSData$Year==years[2],"estimate"],
                            end=ACSData[ACSData$Sex=="Male"&ACSData$Year==years[3],"estimate"])
  maleMig <- data.frame(Migration=rowMeans(cbind(migMale1,migMale2)),Sex="Male",
                          Cohort=(ACSData$Cohort[1:18]))
  migFemale1 <- findMigration(births=totals[totals$Sex=="Female"&totals$Year==years[2],"Births"],
                            deaths=Deaths[Deaths$Sex=="Female"&Deaths$Year==years[2],"Deaths"],
                            start=ACSData[ACSData$Sex=="Female"&ACSData$Year==years[1],"estimate"],
                            end=ACSData[ACSData$Sex=="Female"&ACSData$Year==years[2],"estimate"])
  migFemale2 <- findMigration(births=totals[totals$Sex=="Female"&totals$Year==years[3],"Births"],
                            deaths=Deaths[Deaths$Sex=="Female"&Deaths$Year==years[3],"Deaths"],
                            start=ACSData[ACSData$Sex=="Female"&ACSData$Year==years[2],"estimate"],
                            end=ACSData[ACSData$Sex=="Female"&ACSData$Year==years[3],"estimate"])
  femaleMig <- data.frame(Migration=rowMeans(cbind(migFemale1,migFemale2)),Sex="Female",
                          Cohort=(ACSData$Cohort[1:18]))
  migration <- rbind(maleMig,femaleMig)
  return(migration)  
}



