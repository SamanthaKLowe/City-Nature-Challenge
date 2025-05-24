#CNC Wrapped Analysis
#Sam Lowe
#May 5, 2025
library(tidyverse)
setwd("~/Documents/GitHub/City-Nature-Challenge/CNC_data")

#load in Alachua data
alachua <- read_csv("alachua_cnc.csv")|>
  filter(user_login == "jb1634"|user_login == "sam_k_lowe"|
         user_login == "levihoskins"|user_login == "lxgallivan"|
           user_login == "thomaslilkendey")
#add name column:
alachua <- alachua|>
  mutate(name = case_when(
    user_login == "jb1634" ~ "Jackson",
    user_login == "sam_k_lowe" ~ "Sam",
    user_login == "levihoskins" ~ "Levi",
    user_login == "lxgallivan" ~ "Lee"
  ))
#select name and scientific name only:
alachua <- alachua|>
  select(name, scientific_name)
#load in SOFL individual datasets
brittany <- read_csv("brittany_cnc.csv")|>
  mutate(name = "Brittany")
corey <- read_csv("corey_cnc.csv")|>
  mutate(name= "Corey")
  
analise <- read_csv("analise_cnc.csv")|>
  mutate(name= "Analise")
mario <- read_csv("mario_cnc.csv")|>
  mutate(name= "Mario")
anna <- read_csv("anna_cnc.csv")|>
  mutate(name= "Anna")
marina <- read_csv("marina_cnc.csv")|>
  mutate(name= "Marina")

#combine all the SOFL people into one df and select name and species:
sofl <- bind_rows(analise,anna, brittany, corey, marina, mario)|>
  select(name, scientific_name)

#Make one df:
cnc_gerg <- bind_rows(sofl, alachua)
#total observations: 2,509 and observers:
length(unique(cnc_gerg$name))
#number of unique species:
length(unique(cnc_gerg$scientific_name)) # 1065

#Number of unique species but only those at species level
cnc_gerg <- cnc_gerg |>
  mutate(species_only = if_else(str_count(scientific_name, "\\S+") >= 2, 
                                         scientific_name, 
                                         NA_character_))
length(unique(cnc_gerg$species_only)) # 878

#Group members of lab: most obs and most species: 
member_obs <- cnc_gerg|>
  group_by(name)|>
  summarise(count = n())

member_species <- cnc_gerg|>
  group_by(name)|>
  summarise(count= length(unique(scientific_name)))

member_species_level_only <- cnc_gerg|>
  group_by(name)|>
  summarise(count= length(unique(species_only)))

#What species did we see most?
species_report <- cnc_gerg |>
  group_by(scientific_name)|>
  summarise(count = n())
            