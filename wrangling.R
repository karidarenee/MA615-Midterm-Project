# Wrangling file

# GOAL: have one dataframe with the following columns: 
# chemical, chemical type (insecticide, herbicide, fungicide, fertilizer), 
# toxicity-human/bee, toxicity-level, state, year, and measurement(s) 


pesticide <- read.csv("Pesticides.csv")
straw <- read.csv("Strawberries.csv")
