# Wrangling file

# GOAL: have one dataframe with the following columns: 
# chemical, chemical type (insecticide, herbicide, fungicide, fertilizer), 
# toxicity-human/bee, toxicity-level, state, year, and measurement(s) 

#Load packages
pacman::p_load(tidyverse, magrittr, stringr, dplyr)

# Import Data
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


#select only chemical columns
strawb_chem <- strawb %>% filter((type=="FUNGICIDE")|
                                   (type=="HERBICIDE")|
                                   (type=="INSECTICIDE")|
                                   (type== "FERTILIZER")|
                                   (type== "OTHER"))


#drop no info columns
drop_no_info_cols <- function(df){
  cnames = colnames(strawb)
  T = NULL
  for(i in 1:ncol(df)){T <- c(T, nrow(unique(df[i])))}
  drop_cols <- cnames[which(T == 1)]
  return(select(df, !all_of(drop_cols)))
}

strawb_chem <- drop_no_info_cols(strawb_chem)
#------------------------------------------------------------------------------#

#separate the name from the =Number
chemname <- str_split(strawb_chem$details, " =", simplify = TRUE)
strawb_chem$name <- chemname[,1]

#format chemical names in the pesticide file to be all caps to match the 
#strawb_chem file chem_split columns
pesticides <- rename(pesticides, name = ï..Pesticide)
pesticides <- mutate_all(pesticides, .funs=toupper)

#remove empty rows
pesticides <- pesticides[!apply(pesticides == "", 1, all), ] 

#delete white space in names
#pesticides$name <- str_trim(pesticides$name) 

join <- inner_join(strawb_chem, pesticides)
join <- select(join, -c("Domain.Category", "title", "details"))
