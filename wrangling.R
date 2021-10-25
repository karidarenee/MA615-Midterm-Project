# Wrangling file

# GOAL: have one dataframe with the following columns: 
# chemical, chemical type (insecticide, herbicide, fungicide, fertilizer), 
# toxicity-human/bee, toxicity-level, state, year, and measurement(s) 

pesticides <- read.csv("Pesticides.csv", header = TRUE)
strawberries <- read.csv("Strawberries.csv", header = TRUE)