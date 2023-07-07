#This script is for scoring strategy use data. The subjects saw a description of each strategy type (1-7) 
# and then typed in a box which ones they used. The script takes their response and creates 7 new variables
# that hold a 1 if they reported using that response. This data can then be used for further analysis
# on rates of strategy usage and success rates.

library(tidyverse)
library(here)
library(rio)
library(janitor)
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(tcltk)
library(haven)

#Pick where your directory is. 
#I have a file in mine called "data" where the data is pulled 
#from in the next step
dir <- tclvalue(tkchooseDirectory())

# Import data set
poli <- import(here("data","TextStrat.sav"), setclass="tbl_df" ) %>%
  clean_names()

# Assign a word (whatever you want to find in the data; in this case the number 1-7) to a variable name
present1 <- '1'
present2 <- '2'
present3 <- '3'
present4 <- '4'
present5 <- '5'
present6 <- '6'
present7 <- '7'

# Detects whether that word is present in the sentence, and if it is it returns a 1 in the variable name you point to (s1-s7)
# in this case s1-s7 stands for strategy 1 and so on.
poli$s1 <- as.integer(str_detect(poli$strat, present1))
poli$s2 <- as.integer(str_detect(poli$strat, present2))
poli$s3 <- as.integer(str_detect(poli$strat, present3))
poli$s4 <- as.integer(str_detect(poli$strat, present4))
poli$s5 <- as.integer(str_detect(poli$strat, present5))
poli$s6 <- as.integer(str_detect(poli$strat, present6))
poli$s7 <- as.integer(str_detect(poli$strat, present7))

# Makes sure it's set as a data frame
poli <- as.data.frame(poli)

# Exports file to .sav or .csv
write_sav(poli, path = paste(dir, "/", "poliStratScored.sav", sep = ""))
export(poli, file = paste(dir, "/", "poliTextStrat.csv", sep = ""))
