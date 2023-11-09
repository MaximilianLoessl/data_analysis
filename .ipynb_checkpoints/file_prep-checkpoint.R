# this file prepares the dataframes. It reads them, and prepares them for analysis. For Windows, you would need to change the make option for the out dir from /files to \files

# Check if there are numbers in the subject name--> put them as age.
# check if bids structure --> special analysis --> could be done with reading the arguments, and if one argument is "bids" --> launch special analysis
# differentiation btw Hc's and Patient's via for example first argument in the cmd is Hc's, second is patients

# CONTROL:
# control if age is correct 

# danger points:
# - read the roi file, bc if not dataframe structure could read it wrong and then afterwards would break. as well, at the moment the file is with the "_thickness" ending. this we need to change
# - the first column is expected to contain the names
# - the age point is a large danger point. 1. if someone uses another name for the age column than AGE, age, Age, it wont be recognized and be computed again. needs to be commented out manually.
# 2. the age computing happens with grep from the subject names. so if the age is not in the subject names, it wont work--> so I need to add that if it fails, it continues with the warning that
# age has not been added and either continue without age or add it manually

# this would be the first file to use --> could set up a github repo with the codes. setup would be
# outdir
# frames
# src
# 
# packages: need to better this loading up, eventually with installing directly
library(tidyverse)
library(optparse)
library(dplyr)
library(stringr)

# function to get the individual path, where this file is saved
getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}


#install.packages("optparse")
# working directory, is only the same like file dir if cd there
cur_dir <- getwd()
# file dir, where file is saved
file_dir <- getCurrentFileLocation()

sprintf("this is the file directory: %s", file_dir)

# set the flags of the cmd line input
option_list = list(
  make_option(c("-y", "--healthy"), type = "character", default = NULL, help = "Give the path tho the healthy controls. \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "character"),
  make_option(c("-p", "--patient"), type = "character", default = NULL, help = "Give the path tho the patients. \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "character"),
  make_option(c("-o", "--out"), type = "character", default = paste0(file_dir, "/files"), help = "output file name [default \"%default\"]", metavar = "character"),
  make_option(c("-g", "--group"), type = "character", default = NULL, help = "add a grouping variable for both dataframes. In case you want to import multiple couples of HC/Pat", metavar = "character"),
  make_option(c("-j", "--joke"), type = "character", default = NULL, help = "build-in jokes", metavar = "character")
)
# get the arguments from the list of arguments
opt_parser <- OptionParser(option_list = option_list)
opt = parse_args(opt_parser)
print(opt)
# hc and pats need to be defined
if (is.null(opt$healthy) || is.null(opt$patient)){
  print_help(opt_parser)
  stop("Paths of the Health Controls and Patients must be supplied", call.=FALSE)
}

if (!is.null(opt$joke)){
  print("jokemode on =)")
  Sys.sleep(5)
  message("Error: no files found")
  Sys.sleep(10)
  print("joke")
}

# directories and labeling
HC_DIR <- opt$healthy
PT_DIR <- opt$patient
OUT_DIR <- opt$out



# need to import the roi, but change it so that the format is like the roi, that I have
region <- read.csv(file = paste0(file_dir, "/region.csv")) 
region <- region[, 1][grepl("lh_|rh_", region[, 1])]
core <- region %>% append("Subject", after = 0) # this is the core structure of the df
# lookbehind for match f_ or m_, but doesn't include it in the final match , the d for one or more digits.
# !!Dagnger!!
age_pattern <- "(?<=f_|m_)\\d+"

healthy_label <- "hc"
patient_label <- "pt"
# creates dir if it does not exist, comment it for efficiency
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

# rm(filelist) # dont know if needed
# if group var has been added
# if(exists(opt$group)) group <- opt$group

# if multiple files in dir--> get the ones with thickness
filegrab <- function(DIR){
  filelist <- list.files(DIR, full.names=TRUE)
  if (length(filelist) == 0){
    DIR <- DIR
    # stop("No files found in the directory")
  } else if (length(filelist) == 1){
    DIR <- filelist[1]
  } else {
    DIR <- filelist[grep(pattern = "thickness\\b", filelist, fixed = FALSE)]
  }
 return(DIR)
}

filegrab(PT_DIR)

# this works fine: --> perhaps need to add header and strings
# healthy_frame <- read.csv(file = filegrab(HC_DIR), header = TRUE, stringsAsFactors=FALSE)




# PT_DIR <- '/home/user/Enigma_PD_Data/PD_FS600_stats2table/lh.aparc_stats_volume.txt'
# PT_DIR <- '/home/user/patients/'
# HC_DIR <- '/home/user/health_controls/'
# PT_DIR



# this fun reader is written in CC (=criminal code) but is needed bc of different reading options
file_reader <- function(DIR){
  if (length(DIR[grep(pattern = "\\.csv", DIR, fixed = FALSE)]) >= 1){
    dataframes <- lapply(DIR, function(i){read_csv(i)})
    names(dataframes) <- sub("\\.csv", "", basename(DIR))
  } else if (length(DIR[grep(pattern = "\\.txt", DIR, fixed = FALSE)]) >= 1){
    dataframes <- lapply(DIR, function(i){read_delim(i, delim = "\t", escape_double = FALSE, trim_ws = TRUE)})
    names(dataframes) <- sub("\\.txt ", "", basename(DIR))
  } else {
    stop("At the moment, only .txt or .csv files are accepted")
  }
  return(dataframes)
}
# print(test)


healthy_list <- file_reader(filegrab(HC_DIR))
patient_list <- file_reader(filegrab(PT_DIR))


# this function prepares the dataframe, taking into account that the dir consists either one or two frames.
# because of that, the previous function creates a list, consisting either one or two frames. 
# If it is one frame, it should be the whole dataframe. if not, it doesn't matter, --> later hemi will be either full, lh or rh, based on unique(grep(names))
# if the list consits two dataframes (both thickness) --> they will be merged by column, bc mostly it is lh rh 
# add that every roi should be numeric
get_age <- function(df){
  if (any(c("age", "AGE", "Age") %in% names(df))){
    return(df)
  } else {
    df$age <- as.numeric(str_extract(df$Subject, age_pattern))
    return(df)
  }
}

subjecting <- function(df){
  if (!typeof(df[, 1]) == "numeric") { # like this first col only gets Subject if not numeric.
    names(df)[1] <- "Subject"
    # change the names of the df to only the region
    names(df) <- str_replace(names(df), paste0(".*(", paste(region, collapse = "|"), ").*"), "\\1") # this was painful to get, but works no matter the size of the df, string or whatever
    return(get_age(df[names(df) %in% core]))
  } else {
    message(sprintf("The first col is expected to be subjects, but is %s", names(df)[1]))
    return(df)
  }
}
# subjecting(as.data.frame(patient_list[1]))
# core
# select()
# test <- as.data.frame(patient_list[1])
# names(test) <- str_replace(names(test), paste0(".*(", paste(region, collapse = "|"), ").*"), "\\1")
# names(test)
# test %>% select(names(test), matches(core))
# test[names(test) %in% core]
# frame_prep <- function(list){
#   if (length(list) == 1){
#     frame <- as.data.frame(list[1])
#     if (!typeof(frame[, 1]) == "numeric") { # like this first col only gets Subject if not numeric.
#       names(frame)[1] <- "Subject"
#       # change the names of the df to only the region
#       names(frame) <- str_replace(names(frame), paste0(".*(", paste(region, collapse = "|"), ").*"), "\\1") # this was painful to get, but works no matter the size of the df, string or whatever
#       return(get_age(frame))
#     } else {
#       message(sprintf("The first col is expected to be subjects, but is %s", names(frame)[1]))
#       return(frame)
#       }
#   } else if (length(list) == 2){
#     print("2 elements")
#   } else {
#     print("more")
#   }
# }

frame_prep <- function(list){
  if (length(list) == 1){
    frame <- subjecting(as.data.frame(list[1]))
    return(frame)
  } else if (length(list) == 2){
    frames <- lapply(list, function(x) subjecting(as.data.frame(list)))
    if (all.equal(frames[[1]], frames[[2]])){
      return(frames[[1]])
    } else {
      return(frames)
    }
  } else {
    print("more")
  }
}




healthy_frame <- frame_prep(file_reader(filegrab(HC_DIR)))
patient_frame <- frame_prep(file_reader(filegrab(PT_DIR)))


healthy_frame$group <- healthy_label
patient_frame$group <- patient_label

write.csv(x = healthy_frame, file = paste0(OUT_DIR, "/healthy_frame.csv"), row.names = FALSE)
write.csv(x = patient_frame, file = paste0(OUT_DIR, "/patient_frame.csv"), row.names = FALSE)

          

# names(test) <- apply(X = names(test) , FUN = function(x) regexpr(pattern = region, "", x))

# nächste Woche hier weiter machen, strings wurden replaced, jetzt dataframe auf region und Subject kürzen, dann age versuchen zu bekommen




