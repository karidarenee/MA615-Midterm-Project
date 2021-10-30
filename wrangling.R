# Wrangling file

# GOAL: have one dataframe with the following columns: 
# chemical, chemical type (insecticide, herbicide, fungicide, fertilizer), 
# toxicity-human/bee, toxicity-level, state, year, and measurement(s) 

oldw <- getOption("warn")
options(warn = -1)

#Load packages
pacman::p_load(tidyverse, magrittr, stringr, dplyr,janitor)

# Import Data
pesticides <- read.csv("Pesticides.csv", header = TRUE, fileEncoding = "latin1")
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
pesticides<- clean_names(pesticides)
pesticides<- rename(pesticides, chemical = i_pesticide)
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
strawberry %<>%drop_na(Value)
# =======

# change names of variable "type","Carcinogen","Bee.Toxins
colnames(strawberry)[colnames(strawberry) %in% 
                       c("type")] <- c("chemical_type")
colnames(strawberry)[colnames(strawberry) %in% 
                       c("discription")] <- c("Measurement(s)")
colnames(strawberry)[colnames(strawberry) %in% 
                       c("bee_toxins")] <- c("toxicity_bee")


# use numerical values to represent the bee toxin level
# 1 = SLIGHT, 2 = MODERATE, 3 = HIGH
#a <- sub("SLIGHT","1",strawberry$Bee.Toxins)
#b <- sub("MODERATE","2",a)
#c <- sub("HIGH","3",b)
# strawberry$Bee.Toxins <- c
strawberry$toxicity_bee <- plyr::mapvalues(strawberry$toxicity_bee, 
                                         from = c("SLIGHT","MODERATE","HIGH"), to = c(1,2,3))
strawberry$toxicity_bee <- as.numeric(strawberry$toxicity_bee)

# use numerical values to represent the Carcinogen level
# 1 = POSSIBLE, 2 = PROBABLE, 3 = KNOWN
strawberry$carcinogen <- plyr::mapvalues(strawberry$carcinogen, 
                                         from = c("POSSIBLE","PROBABLE","KNOWN"), to = c(1,2,3))
strawberry$carcinogen <- as.numeric(strawberry$carcinogen)
# >>>>>>> fc5afcadacad5040681a31f076c52d2455463e2d

table(strawberry$Year)

# remove missing values in carcinogen.
strawberry_car <- subset(strawberry, carcinogen != "")

table(strawberry_car$Year)

table(strawberry_car$chemical)
table(strawberry_car$chemical_type)
# remove missing values in toxicity_bee
strawberry_car_insec <- subset(strawberry_car, toxicity_bee != "")

table(strawberry_car_insec$chemical_type)



#Subsets by different measurement
table(strawberry$`Measurement(s)`)
strawberry$`Measurement(s)` = str_trim(strawberry$`Measurement(s)`)
df.lb = subset(strawberry, `Measurement(s)` == "MEASURED IN LB")
df.app = subset(strawberry, `Measurement(s)` == "MEASURED IN LB / ACRE / APPLICATION")
df.year = subset(strawberry, `Measurement(s)` == "MEASURED IN LB / ACRE / YEAR")
df.num = subset(strawberry, `Measurement(s)` == "MEASURED IN NUMBER")
df.PCT = subset(strawberry, `Measurement(s)` == "MEASURED IN PCT OF AREA BEARING")

#Calculate proportions of strawberries by 'Measurements=Number'
library(dplyr)
df.num<-tibble::as_tibble(df.num)
#Count frequency/proportion for each state
df.num %>%
  group_by(State) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
#I think we can show the proportions of each states on our map using mouse over.

#Calculate proportions of values(numbers) grouped by 'toxicity_bee'
df.value<-tibble::as_tibble(df.num$Value)
df.toxicity_bee<-tibble::as_tibble(df.num$toxicity_bee)
df.toxicity<-cbind(df.value,df.toxicity_bee)
#Rename column 2 into 'toxicity_bee'
names(df.toxicity)[2]<- 'toxicity_bee'

df.toxicity %>%
  group_by(toxicity_bee) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

options(warn = oldw)
