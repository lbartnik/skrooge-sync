library(plyr)
library(dplyr)
library(lubridate)
library(tools)
library(readr)
library(magrittr)
library(finances)


if (FALSE) {
  new <- "work/8001/stmt.csv"
  skrooge <- '~/Dropbox/Budget/budget.skg'
  output_path <- '~/Diana, Sav 8001.csv'
  after_date <- '2017-05-01'
  classified <- bofa_credit(new, skrooge, output_path, after_date)
}

if (FALSE) {
  new <- "work/January2017_5431.csv"
  skrooge <- '~/Dropbox/Budget/budget.skg'
  output_path <- '~/Diana, BoA 5431.csv'
  after_date <- '2016-10-09'
  classified <- bofa_credit(new, skrooge, output_path, after_date)
}

