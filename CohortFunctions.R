# ============================================================
# Cohort Component Model Helper Functions
# Cleaned, organized, and commented without changing functionality
# ============================================================
library(openxlsx)
library(ggplot2)

# ------------------------------------------------------------
# 1. Calculate deaths for each sex/cohort
# ------------------------------------------------------------
death <- function(population, deathrates) {
  # Join population to death rates using shared fields
  df <- merge(population, deathrates)
  
  # Apply death rate per 1,000 population
  df$Deaths <- round((df$estimate / 1000) * df$DeathRatePer1000, 0)
  
  # Keep only needed output fields
  df <- df[c("Sex", "Cohort", "Deaths")]
  
  return(df)
}

# ------------------------------------------------------------
# 2. Calculate births by infant sex
# ------------------------------------------------------------
birth <- function(population, birthrates) {
  # Keep only female population and join to maternal birth rates
  temp <- merge(
    population[population$Sex == "Female", ],
    birthrates,
    by.x = "Cohort",
    by.y = "Maternal Cohort"
  )
  
  # Apply birth rates per 1,000 women
  temp$births <- (temp$estimate / 1000) * temp$Rate
  
  # Sum births by infant sex
  births <- aggregate(temp$births, by = list(temp$`Infant Sex`), FUN = sum)
  colnames(births) <- c("Sex", "Total Birth")
  
  # Round total births
  births$`Total Birth` <- round(births$`Total Birth`, 0)
  
  return(births)
}

# ------------------------------------------------------------
# 3. Calculate migration by sex/cohort
#    rates = TRUE  -> migration values are rates per 1,000
#    rates = FALSE -> migration values are already counts
# ------------------------------------------------------------
migration <- function(population, mig, rates = TRUE) {
  df <- merge(population, mig)
  
  if (rates) {
    df$Migrations <- round((df$estimate / 1000) * df$Migration, 0)
  } else {
    df$Migrations <- round(df$Migration, 0)
  }
  
  df <- df[, c("Cohort", "Sex", "Migrations")]
  
  return(df)
}

# ------------------------------------------------------------
# 4. Promote one cohort forward one period and apply
#    births, deaths, and migration
# ------------------------------------------------------------
promotion <- function(births, deaths, migrations, existing) {
  # Combine all model components
  dat <- merge(merge(deaths, migrations), existing)
  dat <- dat[, c("Sex", "Cohort", "Deaths", "Migrations", "estimate")]
  
  # Set cohort order explicitly
  cohort_levels <- c(
    "Under 5", "5-9 years", "10-14 years", "15-19 years",
    "20-24 years", "25-29 years", "30-34 years", "35-39 years",
    "40-44 years", "45-49 years", "50-54 years", "55-59 years",
    "60-64 years", "65-69 years", "70-74 years", "75-79 years",
    "80-84 years", "Over 85"
  )
  
  dat$Cohort <- factor(dat$Cohort, levels = cohort_levels)
  
  # Split by sex and sort by cohort
  males   <- dat[dat$Sex == "Male", ]
  females <- dat[dat$Sex == "Female", ]
  
  males   <- males[order(males$Cohort), ]
  females <- females[order(females$Cohort), ]
  
  # Promote population forward:
  # - new "Under 5" comes from births
  # - all other cohorts come from previous age group
  # - final cohort absorbs top two oldest groups
  males$promotion <- c(
    births$`Total Birth`[births$Sex == "Male"],
    males$estimate[1:16],
    sum(males$estimate[17:18])
  )
  males$newGen <- males$promotion - males$Deaths + males$Migrations
  
  females$promotion <- c(
    births$`Total Birth`[births$Sex == "Female"],
    females$estimate[1:16],
    sum(females$estimate[17:18])
  )
  
  females$newGen <- females$promotion - females$Deaths + females$Migrations
  
  newGen <- rbind(males, females)
  
  return(newGen)
}


# ------------------------------------------------------------
# 5. Download and organize ACS age/sex cohort data
# ------------------------------------------------------------
getAgeSex <- function(state, county, year) {
  library(tidycensus, quietly = TRUE)
  load("Data\\ACSVariableNames.RData")
  
  ACS <- suppressMessages(
    get_acs(
      geography = "county",
      table = "S0101",
      state = state,
      county = county,
      year = year,
      survey = "acs5",
      cache_table = TRUE
    )
  )
  
  if (year > 2016) {
    # Newer ACS years: direct estimates
    ACS <- merge(ACS, NewNames, by.x = "variable", by.y = "Var")
    ACS <- ACS[, c("estimate", "Cohort", "Sex")]
    ACS$Year <- year
    
    print(paste0("ACS ", year, " download complete"))
    return(ACS)
    
  } else {
    # Older ACS years: use percentages and reconstruct counts
    maleTotal   <- ACS$estimate[ACS$variable == "S0101_C02_001"]
    femaleTotal <- ACS$estimate[ACS$variable == "S0101_C03_001"]
    
    ACS <- merge(ACS, OldNames, by.x = "variable", by.y = "Var")
    ACS <- ACS[, c("estimate", "Cohort", "Sex")]
    ACS$Year <- year
    
    ACS$tempest <- round(
      ACS$estimate / 100 * c(rep(maleTotal, 18), rep(femaleTotal, 18)),
      0
    )
    
    ACS$estimate <- ACS$tempest
    ACS <- ACS[, -5]
    
    rm(maleTotal, femaleTotal)
    
    print(paste0("ACS ", year, " download complete"))
    return(ACS)
  }
}


# ------------------------------------------------------------
# 6. Download and combine three ACS years
# ------------------------------------------------------------
getACSYears <- function(state, county, years) {
  print("Downloading and combining ACS data sets")
  library("tidyverse", quietly = TRUE)
  
  AgeSexData <- getAgeSex(state, county, years[1]) %>%
    rbind(getAgeSex(state, county, years[2])) %>%
    rbind(getAgeSex(state, county, years[3])) %>%
    return()
}


# ------------------------------------------------------------
# 7. Calculate average death rates by sex/cohort
# ------------------------------------------------------------
deathRates <- function(ACSData, deathPath) {
  Deaths <- read.csv(deathPath)
  
  print("Calculating death rates")
  
  ACSDeaths <- merge(ACSData, Deaths)
  ACSDeaths$Rate <- ACSDeaths$Deaths / (ACSDeaths$estimate / 1000)
  
  Averages <- aggregate(
    ACSDeaths$Rate,
    by = list(ACSDeaths$Sex, ACSDeaths$Cohort),
    FUN = mean
  )
  
  colnames(Averages) <- c("Sex", "Cohort", "DeathRatePer1000")
  
  cohorts <- c(
    "Under 5", "5-9 years", "10-14 years", "15-19 years",
    "20-24 years", "25-29 years", "30-34 years", "35-39 years",
    "40-44 years", "45-49 years", "50-54 years", "55-59 years",
    "60-64 years", "65-69 years", "70-74 years", "75-79 years",
    "80-84 years", "Over 85"
  )
  
  Averages$Cohort <- factor(Averages$Cohort, levels = cohorts)
  Averages$Sex <- factor(Averages$Sex)
  
  rm(ACSData, Deaths, ACSDeaths)
  
  Averages <- Averages[order(Averages$Cohort), ]
  Averages <- Averages[order(Averages$Sex), ]
  
  return(Averages)
}

# ------------------------------------------------------------
# 8. Calculate average birth rates by maternal cohort and infant sex
# ------------------------------------------------------------
birthRates <- function(ACSData, birthPath) {
  births <- read.csv(birthPath)
  
  dat <- merge(
    births,
    ACSData[ACSData$Sex == "Female", ],
    by = c("Cohort", "Year")
  )
  
  dat$PeriodRate <- dat$Births / (dat$estimate / 1000)
  
  rates <- aggregate(
    dat$PeriodRate,
    by = list(dat$Cohort, dat$Sex.x),
    FUN = mean
  )
  
  colnames(rates) <- c("Maternal Cohort", "Infant Sex", "Rate")
  
  return(rates)
}


# ------------------------------------------------------------
# 9. Calculate average migration rates from three ACS years
# ------------------------------------------------------------
migrationRates <- function(ACSData, birthDir, deathDir) {
  births <- read.csv(birthDir)
  years = as.integer(names(table(ACSData$Year)))
  # Sum births by sex and year
  totals <- aggregate(births$Births, by = list(births$Sex, births$Year), FUN = sum)
  colnames(totals) <- c("Sex", "Year", "Births")
  
  Deaths <- read.csv(deathDir)
  
  # Male migration rates between period 1->2 and 2->3
  migMale1 <- findMigration(
    births = totals[totals$Sex == "Male" & totals$Year == years[2], "Births"],
    deaths = Deaths[Deaths$Sex == "Male" & Deaths$Year == years[2], "Deaths"],
    start  = ACSData[ACSData$Sex == "Male" & ACSData$Year == years[1], "estimate"],
    end    = ACSData[ACSData$Sex == "Male" & ACSData$Year == years[2], "estimate"]
  )
  
  migMale2 <- findMigration(
    births = totals[totals$Sex == "Male" & totals$Year == years[3], "Births"],
    deaths = Deaths[Deaths$Sex == "Male" & Deaths$Year == years[3], "Deaths"],
    start  = ACSData[ACSData$Sex == "Male" & ACSData$Year == years[2], "estimate"],
    end    = ACSData[ACSData$Sex == "Male" & ACSData$Year == years[3], "estimate"]
  )
  
  maleMig <- data.frame(
    Migration = rowMeans(cbind(migMale1, migMale2)),
    Sex = "Male",
    Cohort = ACSData$Cohort[1:18]
  )
  
  # Female migration rates between period 1->2 and 2->3
  migFemale1 <- findMigration(
    births = totals[totals$Sex == "Female" & totals$Year == years[2], "Births"],
    deaths = Deaths[Deaths$Sex == "Female" & Deaths$Year == years[2], "Deaths"],
    start  = ACSData[ACSData$Sex == "Female" & ACSData$Year == years[1], "estimate"],
    end    = ACSData[ACSData$Sex == "Female" & ACSData$Year == years[2], "estimate"]
  )
  
  migFemale2 <- findMigration(
    births = totals[totals$Sex == "Female" & totals$Year == years[3], "Births"],
    deaths = Deaths[Deaths$Sex == "Female" & Deaths$Year == years[3], "Deaths"],
    start  = ACSData[ACSData$Sex == "Female" & ACSData$Year == years[2], "estimate"],
    end    = ACSData[ACSData$Sex == "Female" & ACSData$Year == years[3], "estimate"]
  )
  
  femaleMig <- data.frame(
    Migration = rowMeans(cbind(migFemale1, migFemale2)),
    Sex = "Female",
    Cohort = ACSData$Cohort[1:18]
  )
  
  migration <- rbind(maleMig, femaleMig)
  
  return(migration)
}

# ------------------------------------------------------------
# 10. Back-calculate migration rate from two observed periods
# ------------------------------------------------------------
findMigration <- function(births, deaths, start, end) {
  # Promote beginning population forward one step
  promotion <- c(births, start[1:16], sum(start[17:18]))
  
  # Residual migration = observed end - expected end without migration
  migration <- end - (promotion - deaths)
  
  # Return migration rate per 1,000 starting population
  return(migration / (start / 1000))
}



cohort_model <- function(name,brates, drates, mrates, BASE, START_YEAR, END_YEAR,
                         output_path = paste0("Output\\",name," Output.xlsx")) {
  
  current_pop <- BASE[, c("estimate", "Cohort", "Sex")]
  
  cohort_levels <- c(
    "Under 5", "5-9 years", "10-14 years", "15-19 years",
    "20-24 years", "25-29 years", "30-34 years", "35-39 years",
    "40-44 years", "45-49 years", "50-54 years", "55-59 years",
    "60-64 years", "65-69 years", "70-74 years", "75-79 years",
    "80-84 years", "Over 85"
  )
  
  # -------------------------
  # Population output (wide)
  # -------------------------
  population_out <- current_pop
  colnames(population_out) <- c(paste0("Year ", START_YEAR), "Cohort", "Sex")
  
  # -------------------------
  # Tracking objects
  # -------------------------
  births_track <- data.frame()
  
  deaths_out <- current_pop[, c("Cohort", "Sex")]
  migrations_out <- current_pop[, c("Cohort", "Sex")]
  
  n <- (END_YEAR - START_YEAR) / 5
  
  for (i in 1:n) {
    
    year <- START_YEAR + 5 * i
    
    # Recalculate each component from updated population
    births <- birth(population = current_pop, birthrates = brates)
    deaths <- death(population = current_pop, deathrates = drates)
    migrations <- migration(population = current_pop, mig = mrates)
    
    # -------------------------
    # Track births (long)
    # -------------------------
    births_tmp <- births
    births_tmp$Year <- year
    colnames(births_tmp) <- c("Sex", "Births", "Year")
    births_tmp <- births_tmp[, c("Year", "Sex", "Births")]
    births_track <- rbind(births_track, births_tmp)
    
    # -------------------------
    # Track deaths (wide)
    # -------------------------
    deaths_tmp <- deaths[, c("Cohort", "Sex", "Deaths")]
    colnames(deaths_tmp) <- c("Cohort", "Sex", paste0("Year ", year))
    deaths_out <- merge(deaths_out, deaths_tmp, by = c("Cohort", "Sex"), all = TRUE)
    
    # -------------------------
    # Track migrations (wide)
    # -------------------------
    migrations_tmp <- migrations[, c("Cohort", "Sex", "Migrations")]
    colnames(migrations_tmp) <- c("Cohort", "Sex", paste0("Year ", year))
    migrations_out <- merge(migrations_out, migrations_tmp, by = c("Cohort", "Sex"), all = TRUE)
    
    # -------------------------
    # Project forward
    # -------------------------
    g1 <- promotion(
      births = births,
      deaths = deaths,
      migration = migrations,
      existing = current_pop
    )
    
    g1_out <- g1[, c("newGen", "Cohort", "Sex")]
    colnames(g1_out) <- c(paste0("Year ", year), "Cohort", "Sex")
    
    population_out <- merge(population_out, g1_out, by = c("Cohort", "Sex"), all = TRUE)
    
    # Reset for next loop
    current_pop <- g1[, c("newGen", "Cohort", "Sex")]
    colnames(current_pop) <- c("estimate", "Cohort", "Sex")
  }
  
  # -------------------------
  # Sort outputs
  # -------------------------
  population_out$Cohort <- factor(population_out$Cohort, levels = cohort_levels)
  population_out$Sex <- factor(population_out$Sex, levels = c("Male", "Female"))
  population_out <- population_out[order(population_out$Sex,population_out$Cohort), ]
  
  deaths_out$Cohort <- factor(deaths_out$Cohort, levels = cohort_levels)
  deaths_out$Sex <- factor(deaths_out$Sex, levels = c("Male", "Female"))
  deaths_out <- deaths_out[order(deaths_out$Sex,deaths_out$Cohort), ]
  
  migrations_out$Cohort <- factor(migrations_out$Cohort, levels = cohort_levels)
  migrations_out$Sex <- factor(migrations_out$Sex, levels = c("Male", "Female"))
  migrations_out <- migrations_out[order( migrations_out$Sex,migrations_out$Cohort), ]
  
  births_track$Sex <- factor(births_track$Sex, levels = c("Male", "Female"))
  births_track <- births_track[order(births_track$Sex,births_track$Year), ]
  
  # Convert factors back to character for cleaner Excel output
  population_out$Cohort <- as.character(population_out$Cohort)
  population_out$Sex <- as.character(population_out$Sex)
  deaths_out$Cohort <- as.character(deaths_out$Cohort)
  deaths_out$Sex <- as.character(deaths_out$Sex)
  migrations_out$Cohort <- as.character(migrations_out$Cohort)
  migrations_out$Sex <- as.character(migrations_out$Sex)
  births_track$Sex <- as.character(births_track$Sex)
  
  # -------------------------
  # Write Excel workbook
  # -------------------------
  wb <- createWorkbook()
  
  addWorksheet(wb, "Population")
  writeData(wb, "Population", population_out)
  
  addWorksheet(wb, "Births")
  writeData(wb, "Births", births_track)
  
  addWorksheet(wb, "Deaths")
  writeData(wb, "Deaths", deaths_out)
  
  addWorksheet(wb, "Migrations")
  writeData(wb, "Migrations", migrations_out)
  
  saveWorkbook(wb, output_path, overwrite = TRUE)
  # -------------------------
  # Create total population time series
  # -------------------------
  
  # Identify year columns
  year_cols <- grep("^Year ", names(population_out), value = TRUE)
  
  # Sum total population by year
  total_pop <- colSums(population_out[, year_cols], na.rm = TRUE)
  
  # Convert to data frame
  total_pop_df <- data.frame(
    Year = as.numeric(gsub("Year ", "", year_cols)),
    Population = as.numeric(total_pop)
  )
  
  # -------------------------
  # Create and save plot
  # -------------------------

  p <- ggplot(total_pop_df, aes(x = Year, y = Population)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(limits = c(0, NA)) +  # force y-axis to start at 0
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.08))) +  
    labs(
      title = "Total Population Projection",
      x = "Year",
      y = "Total Population"
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(t = 10, r = 20, b = 10, l = 10)  # extra right margin
    )
  
  ggsave(
    filename = "Output//total_population_projection.png",
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
  return(list(
    population = population_out,
    births = births_track,
    deaths = deaths_out,
    migrations = migrations_out
  ))
}