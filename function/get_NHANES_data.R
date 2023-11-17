library(readxl) # read_excel
library(doParallel) # registerDoParallel

get_NHANES_data <- function(codes_file = NULL, cohort = NULL, save_directory = NULL) {

  if (is.null(codes_file)){
    print("Error: please provide a file name")
    stop()
  }

  codes <- as.data.frame(read_excel(codes_file, sheet = 1))
  weights <- as.data.frame(read_excel(codes_file, sheet = 2))

  # Generate list of needed laboratory files 
  # (has metabolite concentration measurements)
  samps <- unique(codes$recent_sample)

  # Limit by cohort
  if (!is.null(cohort)){
    samps <- samps[samps %in% cohort]
  }

  labFiles <- list()
  labFiles <- lapply(samps, function(x) 
    unique(codes$NHANESfile[codes$recent_sample == x]))
  names(labFiles) <- samps

  # Reference table for phases and file conventions
  ref <- data.frame("Full" = c("1999-2000", "2001-2002", "2003-2004", 
                               "2005-2006", "2007-2008", "2009-2010",
                               "2011-2012", "2013-2014", "2015-2016", 
                               "2017-2018", "2019-2020"),
                    "Short" = c("99-00", "01-02", "03-04", "05-06", "07-08", 
                                "09-10", "11-12", "13-14", "15-16", "17-18", 
                                "19-20"),
                    "Ext" = c("", "B", "C", "D", "E", "F", "G", "H",
                              "I", "J", "K"),
                    stringsAsFactors = FALSE)


  ### Add demographic, bodyweight, and creatinine files
  result <- list()
  for (i in 1:length(samps)) {
    ind <- match(samps[i], weights$sample)
    result[[i]] <- c(paste(labFiles[[i]], ".XPT", sep = ""),
                     paste(ifelse(samps[i] != "99-00", "DEMO_", "DEMO"), 
                           ref$Ext[ref$Short == samps[i]], ".XPT", sep = ""),
                     paste(ifelse(samps[i] != "99-00", "BMX_", "BMX"), 
                           ref$Ext[ref$Short == samps[i]], ".XPT", sep = ""),
                     toupper(weights$creatfile[ind]), 
                     toupper(weights$urineflow[ind]))
  }

  names(result) <- samps
  
  ### ??? debug to solve the issue for 99-00, 01-02, 03-04 data
  for (i in 1:length(result)) 
    result[[i]] <- result[[i]][complete.cases(result[[i]])]
  ### ???
  

  ### Download the needed files from the NHANES website and save in the correct directory
  print("Starting Downlodas")
  oldw <- getOption("warn")
  options(warn = -1)
  for (i in 1:length(samps)) {
    indP <- match(samps[i], ref$Short)

    # Create rawData directory
    if (is.null(save_directory)){
      if (!dir.exists("rawData")) {
        dir.create("./rawData")
      }
    } else {
      if (!dir.exists(save_directory)){
        dir.create(save_directory)
      }
      if (!dir.exists(file.path(save_directory, "rawData/"))) {
        dir.create(file.path(save_directory, "rawData/"))
      }
    }

    # Download the files
    # ???
    #registerDoParallel(cores = 3)
    #system.time(
    #  foreach(j = 1:length(result[[i]])) %dopar% {
    #    if (result[[i]][j] == ""){
    #      next
    #    }
    #    if (is.null(save_directory)){
    #      if (!dir.exists(paste("rawData/", ref$Full[indP], sep = ""))) {
    #        dir.create(paste("./rawData", ref$Full[indP], sep = ""))
    #      }
    #      print(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""))
    #      tryCatch(download.file(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""),
    #                             destfile = paste("./rawData/", ref$Full[indP], "/", tolower(result[[i]][j]), sep = ""),
    #                             mode = "wb"), error = function(e) print(paste(result[[i]][j], 'was not found', sep = " ")))
    #    } else {
    #      if (!dir.exists(file.path(save_directory, paste("rawData/", ref$Full[indP], sep = "")))) {
    #        dir.create(file.path(save_directory, paste("rawData/", ref$Full[indP], sep = "")))
    #      }
    #      print(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""))
    #      tryCatch(download.file(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""),
    #                             destfile = file.path(save_directory,
    #                                                  paste("rawData/", ref$Full[indP], "/", tolower(result[[i]][j]), sep = "")),
    #                             mode = "wb"), error = function(e) print(paste(result[[i]][j], 'was not found', sep = " ")))
    #    }
    #  }
    #)

    
    for (j in 1:length(result[[i]])) {
        if (result[[i]][j] == ""){
          next
        }
        if (is.null(save_directory)){
          if (!dir.exists(paste("rawData/", ref$Full[indP], sep = ""))) {
            dir.create(paste("./rawData", ref$Full[indP], sep = ""))
          }
          print(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""))
          tryCatch(download.file(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""),
                                 destfile = paste("./rawData/", ref$Full[indP], "/", tolower(result[[i]][j]), sep = ""),
                                 mode = "wb"), error = function(e) print(paste(result[[i]][j], 'was not found', sep = " ")))
        } else {
          if (!dir.exists(file.path(save_directory, paste("rawData/", ref$Full[indP], sep = "")))) {
           dir.create(file.path(save_directory, paste("rawData/", ref$Full[indP], sep = "")))
          }
          print(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""))
          tryCatch(download.file(paste("https://wwwn.cdc.gov/nchs/nhanes/", ref$Full[indP], "/", result[[i]][j], sep = ""),
                                 destfile = file.path(save_directory,
                                                    paste("rawData/", ref$Full[indP], "/", tolower(result[[i]][j]), sep = "")),
                                 mode = "wb"), error = function(e) print(paste(result[[i]][j], 'was not found', sep = " ")))
        }
      }
  

  }
  options(warn = oldw)

}
