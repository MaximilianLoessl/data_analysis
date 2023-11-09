load_packages <- function(packages){
  lapply(packages, library, character.only = TRUE)
}

#install packages if needed and load them, list needs to be controlled
list.of.packages <- c("ggplot2", "plyr", "readr", "tidyverse", "ggpubr", "dplyr", "ggseg", "magrittr", "igraph", "tidyr", "datawizard", "effectsize")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
load_packages(list.of.packages)

# takes the arguments from the command line
args <- commandArgs(trailingOnly = TRUE) # if false returns all the arguments from the command line
print(args)
if(length(args) > 0) {
  BASE_DIR <- args[2]
  OUT_DIR <- args[2]
  FILES_DIR <- unlist(strsplit(args[1], ',')) # shouldn't make a difference to just args[1]
} else {
  OUT_DIR <- '/home/user/analysis/OUT'
}
# creates dir if it does not exist, commented bc efficiency
# if(!dir.exists(OUT_DIR)) dir.create(OUT_DIR)


# check if you like
# print(paste0("this is out: ", OUT_DIR))
# print(paste0("this is base: ", BASE_DIR))
# print(paste0("this is files: ", FILES_DIR))

# list files in file dir
filelist <- list.files(FILES_DIR, pattern="*.csv", full.names=TRUE)
# read the files in the list
dataframes <- lapply(filelist, function(i){read_csv(i)})
names(dataframes) <- sub("\\.csv", "", basename(filelist))
cat(filelist)
### prep functions #############################################################################
# labeling of a df
labeling <- function(df){
  workingframe <- df
  if (endsWith(deparse(substitute(df)), "lh")) {
    workingframe$hemi <- "lh"
  } else if (endsWith(deparse(substitute(df)), "rh")) {
    workingframe$hemi <- "rh"
  } else {
    workingframe$hemi <- "full"
  }
  if (grepl("HC", deparse(substitute(df)))){
    workingframe$group <- "hc"
  } else {
    workingframe$group <- "pd"
  }
  if (grepl("DL", deparse(substitute(df)))){
    workingframe$software <- "dl"
  } else {
    workingframe$software <- "fs"
  }
  return(workingframe)
}
#adding age if not there already
add_age <- function(df){
  # this works only for specific dataframes, bc of the specific gsub --> change it if needed
  if(!'age' %in% names(df)) result <- df %>% add_column(age = gsub("(\\w+)_(\\w+)_(\\w+)_(\\w+)", "\\3", df$Subject))
  else result <- df
  return(result)
}
# if easier to work with 1 big frame than with 2


View(labeling(dataframes$HCDL_new))
View(dataframes$PDDL_labeled)
preparation <- labeling(dataframes$HCDL_new)
preparation$age <- gsub("(\\w+)_(\\w+)_(\\w+)_(\\w+)", "\\3", preparation$Subject)
preparation <- subset(preparation, select = -c(MODEL))
DL_Frame <- rbind(preparation, dataframes$PDDL_labeled)
DL_Frame$age <- as.numeric(DL_Frame$age)

# define variables
roi <- names(dataframes$HCDL_new)[grepl("lh_|rh_", names(dataframes$HCDL_new))] %>% sub(patter = "_thickness", replacement= "", x = .)
region <- names(dataframes$HCDL_new)[grepl("lh_|rh_", names(dataframes$HCDL_new))]

calc_dvalues <- function(df, prep_ggseg=FALSE){
  # fun get the residuals --> would have been better with apply than hardcode in loop
  #

  # get_resid <- function(metric, df) {
  # return(resid(lm(as.formula(paste0(metric, ' ~ age')), df)))
  # }
  # fun compute a model
  # get_model <- function(metric, df) {
  #   return(lm(as.formula(paste0(metric, ' ~ age')), df))
  # }


  df.pd <- df %>% filter(group == "pd")
  df.hc <- df %>% filter(group == "hc")
  #hc_resids_frame <- df.hc$Subject
  results <- as.data.frame(matrix(ncol = 2, dimnames = list(c(), c("region", "d_value")))) # creates NA's, need to drop later
  for (reg in region){
    # compute lm everytime new is not efficient but life is not perfect
    model <- lm(as.formula(paste0(reg, ' ~ age')), df.hc)
    # if sapply would work better, it would be here. But so it calculates resids anyway
    hc_resids <- resid(lm(as.formula(paste0(reg, ' ~ age')), df.hc))
    #hc_resids_frame <- cbind(as.data.frame(hc_resids_frame), hc_resids)
    # predict the pd values based on hc model
    pd_pred <- predict(model, newdata = df.pd)
    # need to unlist bc can't convert list to numeric
    pd_resids <- unlist(df.pd[reg]) - pd_pred
    d_value <- effectsize::cohens_d(x = hc_resids, y = pd_resids)
    # create matrix with current region and dvalue to bind together
    prep <- matrix(data = c(reg, d_value$Cohens_d), ncol = 2, dimnames = list(c(), c("region", "d_value")))
    results <- rbind(results, prep)
  }
  if (prep_ggseg){
    return(data.frame(label = roi, p = as.numeric(drop_na(results)$d_value)))
  } else {
    return(drop_na(results))
  }
}

test <- calc_dvalues(DL_Frame, prep_ggseg = TRUE)
print(paste0("this is a test ", test))

plot_ggseg <- function(df){
  seg <- ggseg(df, atlas = dk,
               colour = "white",
               size = .1,
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", unique(df$software), ", PD-HC")) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits= c(-1, 1))
  # ggsave(filename = paste0(OUT_DIR, ".pdf", plot = seg))
  return(seg)
}
jpeg(filename = paste0(OUT_DIR, "/plot.jpeg"))
plot <- plot_ggseg(calc_dvalues(DL_Frame, prep_ggseg = TRUE))
plot
dev.off()