# this file prepares the dataframes. It reads them, and prepares them for analysis. For Windows, you would need to change the make option for the out dir from /files to \files
# you perhaps need to install the packages beforehand


# to dos:
# reader not only for dataframes, instead for multiple single subjects as well --> put them into dataframe 
# check if bids structure --> special analysis --> could be done with reading the arguments, and if one argument is "bids" --> launch special analysis
# add flag with option to run all of the analysis files --> source("/path/to/file") in the end
# the question is, if I source multiple files from this file, do they run in parallel?
# option to put multiple frames for HC / PT that get bind by row?
# write in description that possibility to decide which files should run by zb changing code 
# write in desc that possible to change things down in the code to for example include other var in different analysis. zb bei comparisons oder scn nicht nur Unterscheidung HC PT sondern noch
# Möglichkeit group Unterscheidung zu machen --> wegen Möglichkeit keeper in file prep
# write desc dass entweder age providen oder names df sex_age sein sollten 
# write in desc dass eig nichts hardcoded sein sollte, wenn mit file prep durch laufen, aber wenn einzelne Files, kann es zu schwierigkeiten kommen, zb mit durchlaufen lassen comparisons --> Subject Variable hardcoded
# in desc erklären wie mit keep funktioniert für die comparisons
# bonferroni corr for the comparisons?


# CONTROL:
# control if age is there and correct 

# danger points:
# - read the roi file, bc if not dataframe structure could read it wrong and then afterwards would break. as well, at the moment the file is with the "_thickness" ending. this we need to change
# - the first column is expected to contain the names. Control for this is only with if the column is not numeric, it is seen as Subjects
# - the age point is a large danger point. 1. if someone uses another name for the age column than AGE, age, Age, it wont be recognized and be computed again. needs to be commented out manually.
# 2. the age computing happens with grep from the subject names. so if the age is not in the subject names (f_45_), it wont work--> so I need to add that if it fails, it continues with the warning that
# age has not been added and either continue without age or add it manually
# file reader: if multiple files are in a folder, you would need to specify the 
# keep rows --> needs to be written right


### Packages: #################################################################################

# library(tidyverse)
# library(optparse)
# library(dplyr)
# library(stringr)


load_packages <- function(packages){
  lapply(packages, library, character.only = TRUE)
}

#install packages if needed and load them, list needs to be controlled
list.of.packages <- c("tidyverse", "optparse") # dplyr and stringer already in tidyverse
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
load_packages(list.of.packages)


### Paths & Flags ######################################################################################

# scroll down to add DIR's manually, ca 140

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
w_dir <- getwd()
# file dir, where file is saved
file_dir <- getCurrentFileLocation()

sprintf("this is the file directory: %s", file_dir)

# set the flags of the cmd line input
option_list = list(
  make_option(c("-y", "--healthy"), type = "character", default = NULL, help = "Give the path tho the healthy controls. \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "character"),
  make_option(c("-p", "--patient"), type = "character", default = NULL, help = "Give the path tho the patients. \n     Should be a dataframe containing all the subjects.  [example: /path/to/PT]", metavar = "character"),
  make_option(c("-o", "--out"), type = "character", default = paste0(file_dir, "/files"), help = "output file name [default \"%default\"]", metavar = "character"),
  make_option(c("-g", "--group"), type = "character", default = NULL, help = "add a grouping variable for both dataframes. In case you want to import multiple couples of HC/Pat. \n    NOT POSSIBLE YET!!", metavar = "character"),
  make_option(c("-f", "--full"), action = "store_true", default = FALSE, help = "Full analysis"),
  make_option(c("-k", "--keep"),type = "character", default = NULL, help = "Columnname you want to keep"),
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
keeper <- opt$keep


# import roi
region <- read.csv(file = paste0(file_dir, "/region.csv"))
region <- region[, 1][grepl("lh_|rh_", region[, 1])]
core <- region %>% append("Subject", after = 0) # this is the core structure of the df
core <- core %>% append(keeper) # Idea to just append the colname we want to keep
# print("core: ")
# print(core)
# grep for age is a lookbehind for match f_ or m_, but doesn't include it in the final match , the d for one or more digits.
# can be changed, If another grepling is needed
age_pattern <- "(?<=f_|m_)\\d+"

healthy_label <- "hc"
patient_label <- "pt"
# creates dir if it does not exist, comment it for efficiency
if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR)

# rm(filelist) # dont know if needed, does not work yet
# if group var has been added
# if(exists(opt$group)) group <- opt$group 



# if multiple files in dir--> get the ones with thickness
# the idea is the possibility to have a single file, a single file in a directory or multiple files in a directory as input. 
filegrab <- function(DIR){
  filelist <- list.files(DIR, full.names=TRUE)
  if (length(filelist) == 0){
    DIR <- DIR
    # stop("No files found in the directory") # was idea before, but like this you can't add single file
  } else if (length(filelist) == 1){
    DIR <- filelist[1]
  } else {
    DIR <- filelist[grep(pattern = "thickness\\b", filelist, fixed = FALSE)] # for longer file list
  }
 return(DIR)
}

# filegrab(PT_DIR)



# Add Dirs manually for direct computation

# PT_DIR <- '/home/user/Enigma_PD_Data/PD_FS600_stats2table/lh.aparc_stats_volume.txt'
# PT_DIR <- '/home/user/patients/'
# HC_DIR <- '/home/user/health_controls/'
# PT_DIR



# this fun reader is written in CC (=criminal code) but is needed bc of different reading options
# a .txt or a .csv file needs a different reading. the criminal part is, that it doesn't look directly if is csv or txt, instead for the length of the files. if the length of csv is one or more --> read_csv
# little workaround, but works
file_reader <- function(DIR){
  if (length(DIR[grep(pattern = "\\.csv", DIR, fixed = FALSE)]) >= 1){
    dataframes <- lapply(DIR, function(i){read_csv(i)}) #lapply bc apply read function to dir list
    names(dataframes) <- sub("\\.csv", "", basename(DIR)) # to get normal df names, not really important
  } else if (length(DIR[grep(pattern = "\\.txt", DIR, fixed = FALSE)]) >= 1){
    dataframes <- lapply(DIR, function(i){read_delim(i, delim = "\t", escape_double = FALSE, trim_ws = TRUE)})
    names(dataframes) <- sub("\\.txt ", "", basename(DIR))
  } else {
    stop("At the moment, only .txt or .csv files are accepted")
  }
  return(dataframes)
}


# for debugging:
# healthy_list <- file_reader(filegrab(HC_DIR))
# patient_list <- file_reader(filegrab(PT_DIR))



get_age <- function(df){
  if (any(c("age", "AGE", "Age") %in% names(df))){ # check if age col exists
    if ("AGE" %in% names(df)){
      names(df)[names(df)=="AGE"] <- "age"
    } else if ("Age" %in% names(df)){
      names(df)[names(df)=="Age"] <- "age"
    } else {
      print("age already in the dataframe")
      return(df)
    }
    
  } else {
    df$age <- as.numeric(str_extract(df$Subject, age_pattern)) # if age does not exist, grep the age in the subjects with defined pattern
    return(df)
  }
}


subjecting <- function(df){
  if (!typeof(df[, 1]) == "numeric") { # like this first col only gets Subject if not numeric.
    names(df)[1] <- "Subject"
    
    # change the names of the df to only the region
    names(df) <- str_replace(names(df), paste0(".*(", paste(core, collapse = "|"), ").*"), "\\1") # this was painful to get, but works no matter the size of the df, string or whatever, here changed region to core
    
    return(get_age(df[names(df) %in% core])) # subset df to core and get age
  } else {
    message(sprintf("The first col is expected to be subjects, but is %s", names(df)[1]))
    return(df)
  }
}

# final function, apply the subjecting with the subjecting
frame_prep <- function(list){
  if (length(list) == 1){
    frame <- subjecting(as.data.frame(list[1]))
    return(frame)
  } else if (length(list) == 2){
    frames <- lapply(list, function(x) subjecting(as.data.frame(list)))
    if (all.equal(frames[[1]], frames[[2]])){ # bc previos fun creates the same frame twice with lapply
      return(frames[[1]])
    } else {
      return(frames)
    }
  } else {
    print("more")
  }
}


system(paste0("ls ", file_dir))

healthy_frame <- frame_prep(file_reader(filegrab(HC_DIR)))
patient_frame <- frame_prep(file_reader(filegrab(PT_DIR)))

# apply labels for further analysis
healthy_frame$group <- healthy_label
patient_frame$group <- patient_label

write.csv(x = healthy_frame, file = paste0(OUT_DIR, "/healthy_frame.csv"), row.names = FALSE)
write.csv(x = patient_frame, file = paste0(OUT_DIR, "/patient_frame.csv"), row.names = FALSE)

if(opt$full == TRUE){
  print("full analysis")
  # struct cov networks file gets launched with default parameters
  system(command = paste0("Rscript --vanilla ", file_dir, "/comparisons.R"), wait = TRUE)
  system(command = paste0("Rscript --vanilla ", file_dir, "/scn.R"), wait = TRUE)
} else {
  print("only read files")
}



# test <- HCDL_labeled
# colnames(test)[colnames(test)=="Subject"] <- "subidub"
# 
# if ("Subject" %in% names(test)){
#   print("exists")
# } else {
#   print("does not exist")
# }
# 
# View(roi)
# difcol <- colnames(test)[!colnames(test) %in% roi]
# 

# 
# a <- c("a", "b", "c", "d")
# b <- c("a", "b", "c")
# a[!a %in% b]
