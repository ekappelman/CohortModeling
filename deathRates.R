source('getPopulation.r')



ACSData <- getACSYears("Montana","Missoula",c(2014,2019,2024))
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