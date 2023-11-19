library(foreign) # read.xport
#library(survey)
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

locat <- paste0(save_directory, "/rawData/2015-2016")
datafiles <- "ssneon_i.xpt"
data_locat <- paste0(locat, "/", datafiles)
demo_locat <- paste0(locat, "/",unique(demofiles[which(names(demofiles) == cohort)]))
bwt_locat <- paste0(locat, "/", tolower(unique(bwtfiles[which(names(bwtfiles) == cohort)])))
creat_locat <- paste0(locat, "/",unique(creatfiles[which(names(creatfiles) == cohort)]))

demo <- read.xport(demo_locat)
cdta <- read.xport(data_locat)
bwt <- read.xport(bwt_locat)
creat <- read.xport(creat_locat)
chem2yrwt <- "WTSB2YR"

#met <- c("URX4FP", "URXOPM", "URXTCC", "URXCB3", "URXCCC")
#if (all(met %in% names(cdta) == TRUE)) {
#  chemvars <- c("URX4FP", "URXCB3", "URXCCC", "URXOPM", "URXTCC")
#  LODnames <- c("URD4FPLC", "URDCB3LC", "URDCCCLC", "URDOPMLC", "URDTCCLC")
#} else if (all(met[1:4] %in% names(cdta) == TRUE)){
#  chemvars <- c("URX4FP", "URXCB3", "URXOPM", "URXTCC")
#  LODnames <- c("URD4FPLC", "URDCB3LC", "URDOPMLC", "URDTCCLC")
#} else {
#  chemvars <- c("URX4FP", "URXOPM", "URXTCC") 
#  LODnames <- c("URD4FPLC", "URDOPMLC", "URDTCCLC")
#}
#

seq <- "SEQN"
PSU <- "SDMVPSU" # the variable in the demographic data file giving the sampling unit
STRA <- "SDMVSTRA" # variable in the demographic data file giving the stratum for each observation
demoageyr <- "RIDAGEYR"
demogendr <- "RIAGENDR"
demoeth <- "RIDRETH1"
MECwt <- "WTMEC2YR"
bodywtcomment <- "BMIWT"

## -----------------------------------------------------------------
##   Demographics

## Interesting demographic variables are:
## demoageyr: age in years
## demogendr: gender: male=1, female=2
## demoeth: Race/Ethnicity:
##        Mexican American=1
##        Other Hispanic=2
##        Non-Hispanic White=3
##        Non-Hispanic Black=4
##        Other Race-Including Multi-Racial=5

## Select out the variables we'll need going forward

demo <- demo[,c(seq,PSU,STRA,demoageyr,demogendr,demoeth,MECwt)]
## Set up gender, age, and ethnicity as factors using the same levels as the NHANES reports
#demo[,demogendr] <- factor(demo[,demogendr], labels=c("Male","Female"))
#demo$AgeGroup <- cut(demo[,demoageyr], breaks=c(-1,5.5,11.5,19.5,65.5, 100.5),
#                     labels=c("0 - 5","6 - 11 years","12 - 19 years", "20 - 65 years", "66 years and older"))
#demo$RaceEthn <- factor(demo[,demoeth],
#                        labels=c("Mexican American","Other Hispanic","Non-Hispanic White",
#                                 "Non-Hispanic Black","Other"))
#demo$ChildBearingAgeFemale <- factor(demo[,demogendr] == "Female" & (demo[,demoageyr] >= 16 & demo[,demoageyr] <= 49),
#                                     labels=c("NotReproAgeFemale","ReproAgeFemale"))
