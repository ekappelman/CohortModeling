AgeSexCohort <- read.csv("StartData.csv")

AClevels <- c("Under 5","5 to 9","10 to 14","15 to 19","20 to 24","25 to 29",
              "30 to 34","35 to 39","40 to 44","45 to 49","50 to 54","55 to 59",
              "60 to 64","65 to 69","70 to 74","75 to 79","80 to 84","85 plus")

AgeSexCohort$Age <- factor(AgeSexCohort$Age, levels=AClevels)

Sex.Split <- split(AgeSexCohort,AgeSexCohort$Sex)

FemaleNext <- c(FemaleBirths,)

