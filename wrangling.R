# Wrangling file

# GOAL: have one dataframe with the following columns: 
# chemical, chemical type (insecticide, herbicide, fungicide, fertilizer), 
# toxicity-human/bee, toxicity-level, state, year, and measurement(s) 

pesticides <- read.csv("Pesticides.csv", header = TRUE)
strawb <- read.csv("Strawberries.csv", header = TRUE)

#--From Haviland's code--------------------------------------------------------#
# Separate Domain into dname and type
strawb <- strawb %>%  separate(col=Domain,
                               into = c("dname", "type" ), 
                               sep = ",", 
                               fill = "right")
# make a copy of Domain.Categoy
strawb %<>% 
  mutate(Chemicals = Domain.Category) %>% 
  relocate(Chemicals, .after = Domain.Category) 


# create list of chemicals
strawb %<>% separate(col = Chemicals,
                     into = c("title", "details"),
                     sep = ":",
                     fill = "right")

strawb %<>% mutate(details = str_extract(str_trim(details) ,"[^(].*[^)]") )
#at start (^) allowable characters are between square brackets

strawb %<>% mutate(type = str_trim(type)) #recreate column

#------------------------------------------------------------------------------#