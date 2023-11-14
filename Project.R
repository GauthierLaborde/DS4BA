################## CONCATENATE ALL THE CSV FILES INTO ONE DATASET ###############################

install.packages("dplyr")
library(dplyr)

# Set the working directory to the folder containing your CSV files
setwd("C:/Users/Gauthier/Downloads/europe-champions-league-master/europe-champions-league-master")

# List all CSV files in the directory
csv_files <- list.files(pattern = "\\.csv$")

# Initialize an empty data frame to store the concatenated data
concatenated_data <- data.frame()

# Loop through each CSV file and concatenate to the data frame
for (file in csv_files) {
  file_path <- file.path("C:/Users/Gauthier/Downloads/europe-champions-league-master/europe-champions-league-master", file)
  df <- read.csv(file_path, header = TRUE)  # Adjust options as needed
  concatenated_data <- dplyr::bind_rows(concatenated_data, df)
}

# Save the concatenated data to a new CSV file
write.csv(concatenated_data, file = "C:/Users/Gauthier/Downloads/europe-champions-league-master/europe-champions-league-master/concatenated_data.csv", row.names = FALSE)

# Print a message indicating successful completion
cat("CSV files successfully concatenated and saved to C:/Users/Gauthier/Downloads/europe-champions-league-master/europe-champions-league-master/concatenated_data.csv\n")

######################################################################

rm(list=ls())
lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)

install.packages("tidyverse") 
install.packages("readr")
install.packages("tidyr")
library(tidyverse)
library(readr)
library(tidyr)

# Read the dataset
dat <- read_csv2("https://www.dropbox.com/scl/fi/1hfit00bbnachuek7y250/concatenated_data.csv?rlkey=6m4n3wzbnlygof9cu4v9qb031&dl=1")
str(dat)
dat

# Create a column for each variable
dat <- separate(dat, col = c("Stage,Round,Group,Date,Team.1,FT,HT,Team.2,X.FT,ET,P,Comments"), into = c("Stage","Round","Group","Date","Team.1","FT","HT","Team.2","X.FT","ET","P","Comments"), sep = ",")
print(dat)

# Delete the useless columns
dat <- subset(dat, select=-Comments)
dat <- subset(dat, select=-P)
dat <- subset(dat, select=-Group)
dat <- subset(dat, select=-X.FT)
dat <- subset(dat, select=-Stage)

# Rename the column ET for a better understanding
dat = rename(dat, "Penalty"="ET")

#supprimer les cases avec «  dans la colonne Penalty
dat$Penalty <- gsub('"', '', dat$Penalty)

#transformer draw en variable binaire, 0 quand pas match nul et 1 quand match nul
dat$Penalty <- ifelse(dat$Penalty == "", 0, 1)

#supprimer les guillemets dans X.FT et remplacer par vide
dat$X.FT <- gsub('"', '', dat$X.FT)

# Extracting the year of the competition
dat$Date <- as.character(dat$Date)  # Convert Date to character to handle the format
dat$Year <- sapply(strsplit(dat$Date, " "), function(x) {
  month <- match(x[3], month.abb)  # Get the month as a numeric value
  year <- as.numeric(x[length(x) - 1])  # Get the year
  
  if (month >= 7) {
    return(as.character(year + 1))
  } else {
    return(as.character(year))
  }
})

dat <- subset(dat, select=-Date)

# Extracting the round
dat <- dat %>%
  mutate(Leg = ifelse(grepl("Leg 1", dat$Round), 1, 
                             ifelse(grepl("Leg 2", dat$Round), 2, 0)))

dat <- dat %>%
  mutate(Round = case_when(
    grepl("Qual", Round) | grepl("Matchday", Round) ~ "0",
    TRUE ~ "1"
  ))

dat$CountryT1 <- str_extract(dat$Team.1, "[A-Z]{3}")
dat$CountryT2 <- str_extract(dat$Team.2, "[A-Z]{3}")

dat$NameTeam.1 <- sub("›.+", "›", dat$Team.1)
dat$NameTeam.1 <- substr(dat$NameTeam.1, 1, nchar(dat$NameTeam.1) - 2)
dat$NameTeam.2 <- sub("›.+", "›", dat$Team.2)
dat$NameTeam.2 <- substr(dat$NameTeam.2, 1, nchar(dat$NameTeam.2) - 2)
dat <- subset(dat, select=-Team.1)
dat <- subset(dat, select=-Team.2)

dat <- dat %>%
  mutate(Results = case_when(
    strsplit(dat$FT, "-") %>%
      sapply(function(x) as.numeric(x[1])) > strsplit(dat$FT, "-") %>%
      sapply(function(x) as.numeric(x[2])) ~ 1,
    strsplit(dat$FT, "-") %>%
      sapply(function(x) as.numeric(x[1])) < strsplit(dat$FT, "-") %>%
      sapply(function(x) as.numeric(x[2])) ~ 2,
    TRUE ~ 0
  ))

dat$FT <- substr(dat$FT, 1, 3)
dat <- dat %>%
  mutate(GoalsScored = sapply(strsplit(dat$FT, "-"), function(x) sum(as.numeric(x))))

# Print the updated data frame
print(dat)
