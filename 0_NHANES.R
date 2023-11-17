#library(survey)
#library(foreign) # read.xport
#library(dplyr)
#library(magrittr)
#library(readxl) # read_excel

# TODO: Task Remove unnecessary stuff
source("function/get_NHANES_data.R")
NHANEScodes <- "data/NHANEScodes_file.xlsx"
cohort <- "15-16"
save_directory <- "saves"

get_NHANES_data(codes_file = NHANEScodes, cohort = cohort,
                save_directory = save_directory)

wtvars <- as.data.frame(read_excel(NHANEScodes, sheet = 2))
demofiles <- wtvars$demofile
datafiles <- wtvars$file
bwtfiles <- wtvars$BWfile
creatfiles <- wtvars$creatfile
wtvar <- wtvars$wtvariable

names(demofiles) <- names(datafiles) <- 
  names(bwtfiles) <- names(creatfiles) <- 
  names(wtvar) <- wtvars$sample
