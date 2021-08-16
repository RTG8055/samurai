#!/usr/bin/env Rscript

source("slash.r")
library("quantmod")
library(dplyr)
library(stringr)

args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0){
  stop("Need start and end date to download data", call.=FALSE)
} else if(length(args) == 1){
  # no end date provided and data_dir provided
  args[2] = Sys.Date()
  args[3] = "data/"
} else if(length(args) == 2){
  # no data_dir provided
  args[3] = "data/"
}

run_num=9 ## flag to reset/re-download all data files

# data_dir = "data/"

## download individual files if they dont exist already
download_daily_data(as.Date(args[1]), as.Date(args[2]),run_num, data_dir = args[3])


