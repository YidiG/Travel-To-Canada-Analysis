# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data Analysis - Canadian Travel Data
# Course: Data Analysis & Business Intelligence
# Author: Merlin Gutter
# Data Source: https://www.kaggle.com/datasets/russellpecar/visitors-to-canada
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# =========================== General Settings =================================

# --- Cleaning ---

rm(list=ls())

# --- Load Packages ---

install.packages("tidyverse")  #designed for data since operations
library(tidyverse)

# ==============================================================================

# =========================== Functions ========================================
{
# --------------------------- Clean Data Set - Country Names -------------------

clean <- function(dataset) {

  # rename header
  dataset <- rename(dataset,`country` = `Country of residence`)
            
  # --- rename country's ---
            
  # define which country entries to change
  Fin = c("United States of America residents entering Canada",
          "Residents of countries other than the United States of America entering Canada",
          "Americas, countries other than the United States of America",
          "North America, countries other than the United States of America")
            
  # define replacement
  Repl = c("United States of America",
           "Other than USA",
           "America excl. USA",
           "North America excl. USA")
            
  #loop to replace
  for (i in seq(1, length(Fin), by = 1)) {
    dataset$country[dataset$country == Fin[i]] <- Repl[i] #replace country name which is in Fin with Repl element
    }
            
  #create America continent data row
  dataset <- rbind(dataset, c(NA,colSums(dataset %>%
                                                  filter(country == "America excl. USA" | country == "United States of America") %>%
                                                  select(-country),
                                                na.rm = T))) # set NA as 0
  
  #name American continent row
  dataset[nrow(dataset), 1] <- "America"
  
  # delete unnecessary rows (don't have information that are interesting)
  dataset <- dataset %>%
    filter(!country %in% c("Non-resident visitors entering Canada",
                           "Other than USA",
                           "America excl. USA"))
  
  #--- Return ---
  return(dataset)
}

# --------------------------- Separate Data set into various smaller -----------

Seperate <- function(dataset) {
  
# --- vector creation ---
  
  # create vector with continents
  Continents <- c("America",
                  "Europe",
                  "Africa",
                  "Asia",
                  "Oceania",
                  "Antarctica and Adjacent Islands")
  
  # create vector for each continent
  America <- c("North America excl. USA",
               "Central America",
               "Caribbean",
               "South America",
               "Americas, n.o.s.",
               "United States of America")
  
  Europe <- c("Western Europe",
              "Central/Eastern Europe",
              "Northern Europe",
              "Southern Europe",
              "East Mediterranean Europe",
              "Europe, n.o.s.")
  
  Africa <- c("Western Africa",
              "Eastern Africa",
              "Northern Africa",
              "Central Africa",
              "Southern Africa",
              "Africa, n.o.s.")
  
  Asia <- c("Middle East",
            "Northeast Asia",
            "Southeast Asia",
            "Southern Asia",
            "Asia, n.o.s.")
  
  Oceania <- c("Australasia",
               "Melanesia",
               "Micronesia",
               "Polynesia",
               "Oceania, n.o.s.")
  
  Antarctica <- c("Antarctica and Adjacent Islands Region",
                  "Antarctica and Adjacent Islands, n.o.s.")
  
  # create list with Continents and corresponding Regions  
  Region <- list(America, Europe, Africa, Asia, Oceania, Antarctica)
  
  # name the list
  names(Region) <- Continents 

  
  # --- Create Hierarchy I - Continents ---
  
  # Create Data Frame with only continents
  data_continent <- dataset %>%
    filter(country %in% Continents)
  
  
  # ---- Create Hierarchy II - Regions ---
  
  List_Region <- list()
  
  for (i in Continents) {
    
    #create Data Frame with only the Regions of one Continent
    data_one_Region <- dataset %>%
      filter(country %in%  Region[[i]])
    
    #add data frame to list
    List_Region[[length(List_Region)+1]] <- data_one_Region
  }
  
  names(List_Region) <- Continents      
  
  
  # --- Create Hierarchy III - Countries ---
  
  #exclude every data that is not one country
  data_country <- dataset %>%
    filter(!country %in% c(Continents, Africa, America[1:5], Antarctica, Asia, Europe, Oceania[2:5])) #USA & Australia is in Continents and countries
  
  
  # --- Create Output ---
  
  #aggregate all Hierarchy levels to one list
  Canada_split <- list(Continents = data_continent,
                       Regions = List_Region,
                       Countries = data_country)
  
  return(Canada_split)
}

# --------------------------- Transform Dataset into wide format ---------------

wide <- function(dataset) {
  
  #create long dataset
  data_long <- reshape(data =  dataset,
                       direction = "long",
                       varying = as.character(seq(as.numeric(names(dataset)[2]), as.numeric(names(dataset)[ncol(dataset)]), by = 1)),
                       idvar = c("country"),
                       v.name = c("Tourists"),
                       times = (seq(as.numeric(names(dataset)[2]), as.numeric(names(dataset)[ncol(dataset)]), by = 1)),
                       new.row.names = 1:(nrow(dataset)*(ncol(dataset)-1))
  )
  
  return(data_long)
}

# --------------------------- Analysis Function Detailed -----------------------

Analyze <- function(time_start, time_end, dataset) {
  
  # create column with selected countries
  country <- c(dataset$country)
  
  # selected time frame
  time <- c(match(time_start, names(dataset)), match(time_end, names(dataset)))
  time_frame <- seq(time[1], time[2], by = 1)
  
  #total sum over selected countries and years
  SUM <- c(rowSums(dataset %>%
                       select(time_frame),
                     na.rm = T)) # set NA as 0       
  
  # average of selected countries and years
  AVG <- c(rowMeans(dataset %>%
                        select(time_frame),
                      na.rm = T)) # set NA as 0 
  
  # mean over selected countries and years
  # TBD
  
  # insert data frame over time to control
  df <- dataset %>%
    select(time_frame)
  
  # combine name, SUM, AVG, MEAN  for return
  ins <- data_frame(country, SUM, AVG)
  
  # combine name and data
  data <- data_frame(country, df)
  
  # combine data sets into list to return
  out <- list(Insights = ins,
              Dataset = data)
  
  return(out) 
}

# --------------------------- Analysis Function Yearly -------------------------

Analyze_years <- function(time_start, time_end, dataset) {
          
  # create vector with all years in time period
  years <- seq(time_start,time_end,by = 1)
          
  #create data set with only country names
  df <- data_frame(country = dataset$country)
          
  # loop to calculate total numbers per year per country 
  for (year in years) {

    # find columns with corresponding years
    year_filter <- grep(year ,names(dataset)) 
              
    #total sum over selected countries and years
    SUM_year <- c(rowSums(dataset %>%
                            select(year_filter),
                            na.rm = T)) # set NA as 0
              
    # add SUM column to data frame
    df[ , ncol(df) + 1] <- SUM_year 
              
    # name new column according to year
    colnames(df)[ncol(df)] <- paste0(year)
    }
          
  #total sum over selected countries and years
  SUM <- c(rowSums(df %>%
                          select(c(seq(2,ncol(df), by = 1))),
                        na.rm = T)) # set NA as 0
          
  # average of selected countries and years
  AVG <- c(rowMeans(df %>%
                           select(c(seq(2,ncol(df), by = 1))),
                         na.rm = T)) # set NA as 0 
  
  # combine name, SUM, AVG, MEAN  for return
  ins <- data_frame(country = c(dataset$country), SUM, AVG)
  
  # combine data sets into list to return
  out <- list(Insights = ins,
              Dataset = df)
            
  return(out)
}

# --------------------------- Analysis Function Seasons ------------------------

Analyze_seasons <- function(time_start, time_end, dataset) {
  
  # create vector with all years in time period
  years <- seq(time_start,time_end,by = 1)
  
  #create vector with seasons
  winter <- c("*-10", "*-11", "*-12", "*-01", "*-02", "*-03")
  summer <- c("*-04", "*-05", "*-06", "*-07", "*-08", "*-09")
  
  #create data set with only country names
  df_winter <- data_frame(country = dataset$country)
  df_summer <- data_frame(country = dataset$country)
  
  # loop to calculate total numbers per year per country 
  for (year in years) {
    
    # find columns with corresponding years
    year_filter <- grep(year ,names(dataset))
    
    df_year <- dataset %>%
      select(year_filter)
    
    # initializing SUM_year_season
    SUM_year_season <- 0
    
    #create winter sum per year
    for (season in winter) {
      
      #find colums that are in the sinter season
      season_filter <- grep(season, names(df_year))
      
      #total sum over selected countries and years
      SUM_year_season <- SUM_year_season + c(rowSums(df_year %>%
                                     select(season_filter),
                                   na.rm = T)) # set NA as 0
    }
    # add SUM column to data frame
    df_winter[ , ncol(df_winter) + 1] <- SUM_year_season
    
    # name new column according to year
    colnames(df_winter)[ncol(df_winter)] <- paste0(year," Winter")
    
    # initializing SUM_year_season
    SUM_year_season <- 0
    
    #create winter sum per year
    for (season in summer) {
      
      #find colums that are in the sinter season
      season_filter <- grep(season, names(df_year))
      
      #total sum over selected countries and years
      SUM_year_season <- SUM_year_season + c(rowSums(df_year %>%
                                     select(season_filter),
                                   na.rm = T)) # set NA as 0
    }
    # add SUM column to data frame
    df_summer[ , ncol(df_summer) + 1] <- SUM_year_season
    
    # name new column according to year
    colnames(df_summer)[ncol(df_summer)] <- paste0(year," Summer")
    
  }
  
  #create list for output
  df_out <- list(Summer = df_summer,
                 Winter = df_winter)
  
  
  
  #total sum over selected countries and years - summer
  SUM_summer <- c(rowSums(df_summer %>%
                            select(c(seq(2,ncol(df_summer), by = 1))),
                          na.rm = T)) # set NA as 0
  
  # average of selected countries and years - summer
  AVG_summer <- c(rowMeans(df_summer %>%
                             select(c(seq(2,ncol(df_summer), by = 1))),
                           na.rm = T)) # set NA as 0 
  
  #total sum over selected countries and years - winter
  SUM_winter <- c(rowSums(df_winter %>%
                            select(c(seq(2,ncol(df_winter), by = 1))),
                          na.rm = T)) # set NA as 0
  
  # average of selected countries and years - winter
  AVG_winter <- c(rowMeans(df_winter %>%
                             select(c(seq(2,ncol(df_winter), by = 1))),
                           na.rm = T)) # set NA as 0 
  
  
  # combine name, SUM, AVG, MEAN  for return
  ins <- data_frame(country = c(dataset$country), SUM_summer, AVG_summer, SUM_winter, AVG_winter)
  
  # combine data sets into list to return
  out <- list(Insights = ins,
              Dataset = df_out)
  
  return(out)
}

# --------------------------- Export Function ----------------------------------

export <- function(dataset, name) {
  
  write.table(dataset, file= name, sep = ";", row.names = F, col.names = T)
  
}

# --------------------------- Function Plot Bar Chart --------------------------

plot_bar <- function(dataset) {
  
  #only insert the Insight Datasets into this plotting function
  
  #create data object
  plot_export <- ggplot(dataset, aes(x = country, y= SUM)) +
    geom_col() +
    ggtitle("Total Travelors") +
    ylab("Travelors") +
    theme(axis.text.x = element_text(angle=90))
  
  #plot the graphic
  return(plot(plot_export))
}

# --------------------------- Function Plot Line Chart -------------------------

plot_over_time <- function(dataset) {
 
  #only insert the Dataset Datasets into this plotting function
   
  plot_export <- ggplot(wide(dataset), aes(y = Tourists , x = time), group = country) +
    geom_line(aes(color = country)) +
    xlab("Years") +
    ggtitle("Travelors over Time") +
    geom_smooth(method=loess, se= FALSE, color="black")
  
  #plot the graphic
  return(plot(plot_export))
}

}
# ==============================================================================

# =========================== Import Data Data Set =============================
{
# --- Working Directory ---

setwd("~/Library/CloudStorage/OneDrive-Personal/HHL/01_Study/04_Fall_2022_exchange/K723_Data Mining and Business Intelligence/04_Capstone/01_Dataset")

# --- Inport Datasets ---

canada_raw <- read.csv("canada_visitors.csv", header = T, sep = ",", check.names = F)
ontario_raw <- read.csv("ontario_visitors.csv", header = T, sep = ",", check.names = F)
alberta_raw <- read.csv("alberta_visitors.csv", header = T, sep = ",", check.names = F)
BC_raw <- read.csv("British_Columbia_visitors.csv", header = T, sep = ",", check.names = F)
Manitoba_raw <- read.csv("Manitoba_visitors.csv", header = T, sep = ",", check.names = F)
NB_raw <- read.csv("New_Brunswick_visitors.csv", header = T, sep = ",", check.names = F)
NL_raw <- read.csv("Newfoundland_and_Labrador_visitors.csv", header = T, sep = ",", check.names = F)
NWT_raw <- read.csv("Northwest_Territories_visitors.csv", header = T, sep = ",", check.names = F)
NS_raw <- read.csv("Nova_Scotia_visitors.csv", header = T, sep = ",", check.names = F)
Nunavut_raw <- read.csv("Nunavut_visitors.csv", header = T, sep = ",", check.names = F)
PEI_raw <- read.csv("Prince_Edward_Island_visitors.csv", header = T, sep = ",", check.names = F)
Quebec_raw <- read.csv("Quebec_visitors.csv", header = T, sep = ",", check.names = F)
Saskatchewan_raw <- read.csv("Saskatchewan_visitors.csv", header = T, sep = ",", check.names = F)
Yukon_raw <- read.csv("Yukon_visitors.csv", header = T, sep = ",", check.names = F)
}
# ==============================================================================

# =========================== Clear & Separate Data ============================

{
canada <- Seperate(clean(canada_raw))
ontario <- Seperate(clean(ontario_raw))
alberta <- Seperate(clean(alberta_raw))
BC <- Seperate(clean(BC_raw))
Manitoba <- Seperate(clean(Manitoba_raw))
NB <- Seperate(clean(NB_raw))
NL <- Seperate(clean(NL_raw))
NWT <- Seperate(clean(NWT_raw))
NS <- Seperate(clean(NS_raw))
Nunavut <- Seperate(clean(Nunavut_raw))
PEI <- Seperate(clean(PEI_raw))
Quebec <- Seperate(clean(Quebec_raw))
Saskatchewan <- Seperate(clean(Saskatchewan_raw))
Yukon <- Seperate(clean(Yukon_raw))
}

# ==============================================================================

# =========================== Analyze Data =====================================

# --- Filter data set - if needed ----------------------------------------------

#define filter vector
filt <- c("")

#filter countries according given vector
dataset <- dataset_raw %>%
  filter(country %in% criteria)
# ------------------------------------------------------------------------------

# --- Detailed Analysis over specific time frame ---
df_analyse_detailed <- Analyze("1972-01","1973-03", canada$Continents)


# --- Analysis over specific time frame by year ---
df_analyse_year <- Analyze_years(2000, 2021, ontario$Continents)

# --- Analysis over specific time frame by year and season ---
df_analyse_season <- Analyze_seasons(2000, 2021, canada$Regions$Oceania)


# --------------------------- Export Analyzed Tables  --------------------------

{
setwd("~/Library/CloudStorage/OneDrive-Personal/HHL/01_Study/04_Fall_2022_exchange/K723_Data Mining and Business Intelligence/04_Capstone/02_Results")

dataset <- df_analyse_year$Insights

name <- c("continents_yearly_Insights_since_2000.csv")

export(dataset, name)
}

# --------------------------- Plot Analyzed Tables  ----------------------------

# --- Create line plot over time with smoothed trend ---
plot_over_time(df_analyse_year$Dataset)

# --- Create bar plot for categories ---
plot_bar(df_analyse_year$Insights)

