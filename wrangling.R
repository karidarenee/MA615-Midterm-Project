# Wrangling file

# GOAL: have one dataframe with the following columns: 
# chemical, chemical type (insecticide, herbicide, fungicide, fertilizer), 
# toxicity-human/bee, toxicity-level, state, year, and measurement(s) 

#Load packages
pacman::p_load(tidyverse, magrittr, stringr, dplyr)

# Import Data
pesticides <- read.csv("Pesticides.csv", header = TRUE, fileEncoding = "UTF-8-BOM")
strawb <- read.csv("Strawberries.csv", header = TRUE, fileEncoding = "latin1")

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

# set Value column to numeric
strawb$Value <- as.numeric(strawb$Value)

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
strawb_chem$chemical <- chemname[,1]

#format chemical names in the pesticide file to be all caps to match the 
#strawb_chem file chem_split columns
pesticides <- rename(pesticides, chemical = Pesticide)
pesticides <- mutate_all(pesticides, .funs=toupper)

#remove empty rows
pesticides <- pesticides[!apply(pesticides == "", 1, all), ] 

#delete white space in names
#pesticides$name <- str_trim(pesticides$name) 

strawberry <- inner_join(strawb_chem, pesticides)
strawberry <- select(strawberry, -c("Domain.Category", "title", "details"))

# 10.27
# break Data.item into new columns
strawberry %<>% separate(col = Data.Item, 
                   into = c("Strawberries", "items", "discription", "units"),
                   sep = ",",
                   fill = "right")
# explore the new columns
distinct(strawberry, Strawberries)
distinct(strawberry, items)
distinct(strawberry, discription)
distinct(strawberry, units)
# <<<<<<< HEAD
strawberry
strawberry %>%drop_na(Value)
# =======

# use numerical values to represent the bee toxin level
# 1 = SLIGHT, 2 = MODERATE, 3 = HIGH
#a <- sub("SLIGHT","1",strawberry$Bee.Toxins)
#b <- sub("MODERATE","2",a)
#c <- sub("HIGH","3",b)
# strawberry$Bee.Toxins <- c
strawberry$Bee.Toxins <- plyr::mapvalues(strawberry$Bee.Toxins, 
                                           from = c("SLIGHT","MODERATE","HIGH"), to = c(1,2,3))
strawberry$Bee.Toxins <- as.numeric(strawberry$Bee.Toxins)

# use numerical values to represent the Carcinogen level
# 1 = POSSIBLE, 2 = PROBABLE, 3 = KNOWN
strawberry$Carcinogen <- plyr::mapvalues(strawberry$Carcinogen, 
                                         from = c("POSSIBLE","PROBABLE","KNOWN"), to = c(1,2,3))
strawberry$Carcinogen <- as.numeric(strawberry$Carcinogen)
# >>>>>>> fc5afcadacad5040681a31f076c52d2455463e2d

# change names of variable "type","Carcinogen","Bee.Toxins
colnames(strawberry)[colnames(strawberry) %in% 
                       c("type","Carcinogen","Bee.Toxins")] <- 
  c("chemical type","toxicity-level","toxicity-bee")
