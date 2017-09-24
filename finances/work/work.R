library(plyr)
library(dplyr)
library(lubridate)
library(tools)
library(readr)
library(magrittr)
library(stringr)
library(stringdist)
library(finances)

skrooge <- '~/Dropbox/Budget/budget.skg'
after_date <- '2017-08-31'


if (FALSE) {
  new <- "work/8001/stmt.csv"
  output_path <- '~/Diana, Chk 8001.csv'
  classified <- bofa_credit(new, skrooge, output_path, after_date)
}

if (FALSE) {
  new <- "work/Lukasz Checking.csv"
  output_path <- '~/Lukasz Checking.csv'
  classified <- fcu_any(new, skrooge, output_path, after_date)
}
