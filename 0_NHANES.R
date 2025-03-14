# Purpose: Download NHANES data for specified cohorts with progress percentage
# Notes: 
# - Modified from the bayesmarker package
# - Updated for NHANES data site changes (02/2025)
# - Assumes NHANEScodes_file.xlsx contains codes (sheet 1) and weights (sheet 2)


# List of packages
pkgs <- c("readxl") # For reading Excel files


# Install missing packages and load them
invisible(lapply(pkgs, function(x) {
    if (!require(x, character.only = TRUE)) install.packages(x)
    library(x, character.only = TRUE)
}))

#' Download NHANES Data Files
#'
#' Downloads specified NHANES data files based on a codes file and cohort selection,
#' displaying download progress as a percentage.
#'
#' @param codes_file Path to Excel file with NHANES codes and weights.
#' @param cohort Vector of cohort codes (e.g., "15-16") to filter data; NULL for all.
#' @param save_directory Directory to save downloaded files; NULL for default "nhanes".
#' @return Invisible NULL; downloads files to disk.
#' @examples
#' get_NHANES_data("data/NHANEScodes_file.xlsx", "15-16", "data")
get_NHANES_data <- function(codes_file = NULL, cohort = NULL, save_directory = NULL) {
  # Input validation
  if (is.null(codes_file)) {
    stop("Error: 'codes_file' must be provided.")
  }
  if (!file.exists(codes_file)) {
    stop("Error: 'codes_file' not found at specified path.")
  }
  
  # Read codes and weights
  codes <- read_excel(codes_file, sheet = 1) |> as.data.frame()
  weights <- read_excel(codes_file, sheet = 2) |> as.data.frame()
  
  # Filter samples by cohort
  samps <- unique(codes$recent_sample)
  if (!is.null(cohort)) {
    samps <- intersect(samps, cohort)
    if (length(samps) == 0) {
      stop("Error: No matching cohorts found in 'recent_sample'.")
    }
  }
  
  # Map laboratory files to samples
  labFiles <- lapply(samps, \(x) unique(codes$NHANESfile[codes$recent_sample == x]))
  names(labFiles) <- samps
  
  # NHANES cycle reference table
  ref <- data.frame(
    Full = c("1999", "2001", "2003", "2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019"),
    Short = c("99-00", "01-02", "03-04", "05-06", "07-08", "09-10", "11-12", "13-14", "15-16", "17-18", "19-20"),
    Ext = c("", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"),
    stringsAsFactors = FALSE
  )
  
  # Construct file list (lab, demo, body weight, creatinine, urine flow)
  result <- lapply(seq_along(samps), \(i) {
    ind <- match(samps[i], weights$sample)
    c(
      paste0(labFiles[[i]], ".XPT"),
      paste0(ifelse(samps[i] == "99-00", "DEMO", "DEMO_"), ref$Ext[ref$Short == samps[i]], ".XPT"),
      paste0(ifelse(samps[i] == "99-00", "BMX", "BMX_"), ref$Ext[ref$Short == samps[i]], ".XPT"),
      toupper(weights$creatfile[ind]),
      toupper(weights$urineflow[ind])
    ) |> na.omit()
  })
  names(result) <- samps
  
  # Calculate total number of files to download
  total_files <- sum(lengths(result))
  if (total_files == 0) {
    message("No files to download.")
    return(invisible(NULL))
  }
  
  # Download files with progress percentage
  message("Starting downloads...")
  oldw <- getOption("warn")
  options(warn = -1)  # Suppress warnings temporarily
  file_count <- 0  # Track completed downloads
  
  for (i in seq_along(samps)) {
    cycle <- ref$Full[match(samps[i], ref$Short)]
    base_dir <- file.path(ifelse(is.null(save_directory), "nhanes", save_directory), "nhanes", cycle)
    dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    
    for (file in result[[i]]) {
      if (nchar(file) == 0) next
      url <- paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/", cycle, "/DataFiles/", file)
      dest <- file.path(base_dir, tolower(file))
      
      # Skip if file already exists
      if (file.exists(dest)) {
        file_count <- file_count + 1
        percent <- round((file_count / total_files) * 100, 1)
        message(sprintf("Progress: %.1f%% (Skipped: %s already exists)", percent, file))
        next
      }
      
      # Download and update progress
      message("Downloading: ", url)
      tryCatch(
        {
          download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
          file_count <- file_count + 1
          percent <- round((file_count / total_files) * 100, 1)
          message(sprintf("Progress: %.1f%% (Completed: %s)", percent, file))
        },
        error = \(e) {
          file_count <- file_count + 1  # Count failed attempts toward progress
          percent <- round((file_count / total_files) * 100, 1)
          message(sprintf("Progress: %.1f%% (Failed: %s - %s)", percent, file, e$message))
        }
      )
    }
  }
  options(warn = oldw)
  message(sprintf("Download complete: %d/%d files processed.", file_count, total_files))
  invisible(NULL)
}

#' Wrapper to Fetch Data for a Specific Cohort
#'
#' @param cohort NHANES cohort code (e.g., "15-16").
#' @examples
#' get_data("15-16")
get_data <- function(cohort = "15-16") {
  get_NHANES_data(
    codes_file = "data/NHANEScodes_file.xlsx",
    cohort = cohort,
    save_directory = "data"
  )
}

# Define cohorts and years
years <- c("1999", "2001", "2007", "2009", "2011", "2013", "2015")
cohorts <- c("99-00", "01-02", "07-08", "09-10", "11-12", "13-14", "15-16")

# Download data for a specific cohort (e.g., 15-16)
i <- 7  # Index for "15-16"
check_dir <- file.path("data/nhanes", years[i])
if (!dir.exists(check_dir) || length(list.files(check_dir)) == 0) {
  get_data(cohort = cohorts[i])
}
