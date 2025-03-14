# List of packages
packages <- c("readxl",  # read excel file
              "foreign", # read .XPT file
              "survey",  # svydesign
              "dplyr",   # Data manipulation
              "skimr")   # Summary statistics

# Install missing packages and load them
invisible(lapply(packages, function(x) {
    if (!require(x, character.only = TRUE)) install.packages(x)
    library(x, character.only = TRUE)
}))

# Load NHANES codes from NHANES Excel file
codes_file <- "data/NHANEScodes_file.xlsx"
if (!file.exists(codes_file)) stop("Error: 'codes_file' not found at ", codes_file)
wtvars <- read_excel(codes_file, sheet = 2) |> as.data.frame()
demofiles <- setNames(wtvars$demofile, wtvars$sample)
datafiles <- setNames(wtvars$file, wtvars$sample)
bwtfiles <- setNames(wtvars$BWfile, wtvars$sample)
creatfiles <- setNames(wtvars$creatfile, wtvars$sample)

# Define cohort and year
year <- c("2015")
cohort <- c("15-16")
datafiles <- c("ssneon_i.xpt")
chem2yrwt  <- c("WTSSBI2Y")

# Load pre-trained models for creatinine calculation
for (file in c("Wtns.rda", "Agens.rda", "modparms.rda")) {
  if (!file.exists(file.path("data", file))) stop("Error: Missing ", file)
}
load("data/Wtns.rda")
load("data/Agens.rda")
load("data/modparms.rda")

# Define creatinine calculation function
# Define creatinine calculation function
CreatFun <- \(newdata) {
  X <- cbind(model.matrix(~0 + RIAGENDR + RaceEthn, data = newdata),
             predict(Wtns, newdata$BMXWT),
             predict(Agens, newdata$RIDAGEYR))
  10^(X %*% modparms[-length(modparms)])
}

# Define required chemical variables
chemvars <- c("SSIMID", "SSACET", "SSCLOT", "SSTHIA", "SSOHIM", "SSAND")
LODnames <- c("SSIMIDLC", "SSACETLC", "SSCLOTLC", "SSTHIALC", "SSOHIMLC", "SSANDLC")

# Define required demographic variables
seq <- "SEQN"
PSU <- "SDMVPSU"          # Sampling unit
STRA <- "SDMVSTRA"        # Stratum for each observation
demoageyr <- "RIDAGEYR"   # Age in years
demogendr <- "RIAGENDR"   # Gender
demoeth <- "RIDRETH1"     # Ethnicity
MECwt <- "WTMEC2YR"       # MEC weight
bodywtcomment <- "BMIWT"  #
creatinine <- "URXUCR"    # Urinary creatinine
bodywt <- "BMXWT"         #
bodymassindex <- "BMXBMI" # 

# Process each cohort (single iteration for "15-16")
for (j in 1) {
  
  # Read NHANES data
  select_cohort <- cohort
  locat <- file.path("data/nhanes", year)
  demo_locat <- file.path(locat, unique(demofiles[names(demofiles) == cohort]))
  data_locat <- file.path(locat, datafiles)
  bwt_locat <- file.path(locat, tolower(unique(bwtfiles[names(bwtfiles) == cohort])))
  creat_locat <- file.path(locat, unique(creatfiles[names(creatfiles) == cohort]))

  # Read data with error checking
  for (file in c(demo_locat, data_locat, bwt_locat, creat_locat)) {
    if (!file.exists(file)) stop("Error: File not found: ", file)
  }
  demo <- read.xport(demo_locat)
  cdta <- read.xport(data_locat)
  bwt <- read.xport(bwt_locat)
  creat <- read.xport(creat_locat)

  # Process demographic data
  demo <- demo |> 
    select(SEQN, SDMVPSU, SDMVSTRA, RIDAGEYR, RIAGENDR, RIDRETH1, WTMEC2YR) |>
    mutate(
      RIAGENDR = factor(RIAGENDR, labels = c("Male", "Female")),
      AgeGroup = cut(RIDAGEYR, breaks = c(-1, 5.5, 11.5, 19.5, 65.5, 100.5), 
                     labels = c("0-5", "6-11", "12-19", "20-65", "Over 65")),
      RaceEthn = factor(RIDRETH1, labels = c("Mexican American", "Other Hispanic", 
                                             "Non-Hispanic White", "Non-Hispanic Black", "Other")),
      ChildBearingAgeFemale = factor(RIAGENDR == "Female" & RIDAGEYR >= 16 & RIDAGEYR <= 49,
                                     labels = c("NotReproAgeFemale", "ReproAgeFemale"))
    )
  # Merge datasets
  alldata <- merge(demo, cdta[,c(seq, chem2yrwt, chemvars, LODnames)],
    by.x=seq, by.y=seq, all.y=TRUE)
  alldata <- merge(alldata, creat[,c(seq, creatinine)],
    by.x=seq, by.y=seq, all.x=TRUE) # 2125
   bwt[!is.na(bwt[,bodywtcomment]),bodywt] <- NA
  
  ## Create obesity factor from bodymassindex
  bwt$Obesity <- cut(bwt[,bodymassindex], breaks=c(-0.5,30,500),
    labels=c("BMI <= 30", "BMI > 30"))
  alldata <- merge(alldata, bwt[,c(seq, bodywt, bodymassindex, "Obesity")],
    by.x=seq, by.y=seq, all.x=TRUE)

  # Impute missing body weights
  if (any(is.na(alldata[[bodywt]]))) {
    dta <- merge(demo, bwt[, c(seq, bodywt)], by.x = seq, by.y = seq, all.x = TRUE, all.y = TRUE)
    dsg <- na.omit(svydesign(ids = as.formula(paste0("~", PSU)), 
                             strata = as.formula(paste0("~", STRA)),
                             weights = as.formula(paste0("~", MECwt)), 
                             nest = TRUE, data = dta))
    selectmales <- dsg$variables[[demogendr]] == "Male"
    selectfemales <- dsg$variables[[demogendr]] == "Female"
    males <- svyby(as.formula(paste0("~", bodywt)), as.formula(paste0("~", demoageyr)),
                   subset(dsg, selectmales), svymean)
    females <- svyby(as.formula(paste0("~", bodywt)), as.formula(paste0("~", demoageyr)),
                     subset(dsg, selectfemales), svymean)
    imp <- data.matrix(cbind(males[[bodywt]], females[[bodywt]]))
    isnabw <- is.na(alldata[[bodywt]])
    alldata[isnabw, bodywt] <- imp[cbind(alldata[isnabw, demoageyr], 
                                         as.integer(alldata[isnabw, demogendr]))]
  }

  ## Fixup factors so there are no missing levels
  for (nm in names(alldata)) {
    if (is.factor(alldata[,nm])) {
      alldata[,nm] <- factor(alldata[,nm])}
  }
    
  # Calculate daily creatinine
  alldata$DailyCreatinine <- CreatFun(alldata)
  
 # Clean and add cohort (updated for dplyr 1.1.0+)
  alldata <- alldata |> 
    filter_at(vars(URXUCR, DailyCreatinine, BMXWT), all_vars(!is.na(.))) |> 
    mutate(cohort = cohort)

  if (j == 1) all_data <- alldata else all_data <- rbind(all_data, alldata)
}

# Display first few rows
message("\nFirst Few Rows of Processed Data:")
head(all_data)

# Descriptive Statistics
message("Unweighted Descriptive Statistics:")
skim(all_data)

# Compare with Ospina et al. (2019)
