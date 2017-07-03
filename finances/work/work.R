library(plyr)
library(dplyr)
library(lubridate)
library(tools)
library(readr)
library(magrittr)
library(finances)


if (FALSE) {
  bofa_file <- "work/bofa_current.csv"
  skrooge_file <- '~/Documents/budget.skg'
  classified <- bofa(bofa_file, skrooge_file, after_date = '2017-06-10')
}

if (FALSE) {
  new <- "work/February2017_7333.csv"
  skrooge <- '~/Dropbox/Budget/budget.skg'
  output_path <- 'work/Diana, BoA 7333.csv'
  after_date <- '2017-01-01'
  classified <- bofa_credit(new, skrooge, output_path, after_date)
}

