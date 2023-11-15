# this file is for group comparisons. 

# should contain:
# thickness boxplots, but not only thickness --> adjustable?
# thickness brain comparisons with ggseg
# simple mean comparisons
library(optparse)
library(tidyverse)
library(ggseg)

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

# working directory, is only the same like file dir if cd there
w_dir <- getwd()
# file dir, where file is saved
file_dir <- getCurrentFileLocation()
# frames_dir <- paste0(file_dir, "/files/")
# sprintf("this is the file directory: %s", frames_dir)


# set the flags of the cmd line input
option_list = list(
  make_option(c("-y", "--healthy"), type = "character", default = paste0(file_dir, "/files/healthy_frame.csv"), help = "Give the path tho the healthy controls. \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "HEALTHY"),
  make_option(c("-p", "--patient"), type = "character", default = paste0(file_dir, "/files/patient_frame.csv"), help = "Give the path tho the patients_frame \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "PATIENT"),
  make_option(c("-o", "--out"), type = "character", default = paste0(file_dir, "/out/"), help = "output file name [default \"%default\"]", metavar = "OUT_DIR"),
  make_option(c("-l", "--plot"), type = "character", default = paste0(file_dir, "/plots/"), help = "plot file name [default \"%default\"]", metavar = "PLOT_DIR")
)
# get the arguments from the list of arguments
opt_parser <- OptionParser(option_list = option_list)
opt = parse_args(opt_parser)
print(opt)

library(matrixStats)
library(ggpubr)
library(ggcorrplot)
library(igraph)

print("healthy frame dir:")
print(opt$healthy)

# directories and labeling
healthy_frame <- read.csv(opt$healthy)
patient_frame <- read.csv(opt$patient)
OUT_DIR <- opt$out
PLOT_DIR <- opt$plot

healthy_frame <- read.csv("/home/user/PD_analysis/files/healthy_frame.csv")
patient_frame <- read.csv("/home/user/PD_analysis/files/patient_frame.csv")
OUT_DIR <- paste0(file_dir, "/out/")
PLOT_DIR <- paste0(file_dir, "/plots/")

set.seed(42)

N_SAMPLE_NUM <- 1000

thresholds <- seq(0, 1, 0.01)

region <- read.csv(file = paste0(file_dir, "/region.csv"))
region <- region[, 1][grepl("lh_|rh_", region[, 1])]
roi <- region
core <- region %>% append("Subject", after = 0) # this is the core structure of the df
core <- core %>% append(c("age", "group")) # to check if there is another column as grouping variable

if (!all.equal(names(healthy_frame), names(patient_frame))){
  stop("Provide dataframes with matching columns")
} else {
  print("Checking if columnnames match - match")
}

identical(names(healthy_frame), core)


if (identical(names(healthy_frame), core)) {
    calc_dvalues <- function(healthy_frame, patient_frame, prep_ggseg=FALSE){ # prep ggseg only if the prep for ggseg is needed, otherways just return age corrected value
    # fun get the residuals --> would have been better with apply than hardcode in loop
    #
    
    # get_resid <- function(metric, df) {
    # return(resid(lm(as.formula(paste0(metric, ' ~ age')), df)))
    # }
    # fun compute a model
    # get_model <- function(metric, df) {
    #   return(lm(as.formula(paste0(metric, ' ~ age')), df))
    # }
    
    
    df.pt <- patient_frame
    df.hc <- healthy_frame
    #hc_resids_frame <- df.hc$Subject
    results <- as.data.frame(matrix(ncol = 2, dimnames = list(c(), c("region", "d_value")))) # creates NA's, need to drop later
    for (reg in region){
      # compute lm everytime new is not efficient but life is not perfect
      model <- lm(as.formula(paste0(reg, ' ~ age')), df.hc)
      # if sapply would work better, it would be here. But so it calculates resids anyway
      hc_resids <- resid(lm(as.formula(paste0(reg, ' ~ age')), df.hc))
      #hc_resids_frame <- cbind(as.data.frame(hc_resids_frame), hc_resids)
      # predict the pd values based on hc model
      pd_pred <- predict(model, newdata = df.pt)
      # need to unlist bc can't convert list to numeric
      pd_resids <- unlist(df.pt[reg]) - pd_pred
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
  
  
  plot_ggseg <- function(df){
    seg <- ggseg(df, atlas = dk,
                 colour = "white",
                 size = .1,
                 position = "stacked",
                 mapping = aes(fill = p)) +
      theme_void() +
      ggtitle(paste0("Thickness-Segmentation for ", unique(df$group), ", PD-HC")) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits= c(-1, 1))
    # ggsave(filename = paste0(OUT_DIR, ".pdf", plot = seg))
    return(seg)
  }
} else {
  diff_cols <- colnames(healthy_frame)[!colnames(healthy_frame) %in% core]
  if (length(diff_cols) == 0) {
    stop("Is the Frame missing of the columns: Subject, age or group?")
  } else if (length(diff_cols) == 1){
    if (length(unique(diff_cols == 1))) {
      print(paste0(diff_cols, " as grouping variable"))
      dataframe <- rbind(healthy_frame, patient_frame)
      vars <- unique(dataframe[diff_cols])
      print(names(dataframe[diff_cols]))
      print(vars)
    } else {
      print("to many levels in a grouping var")
    }
  } else {
    print(paste0("there are ", length(diff_cols), "extra columns"))
    print("to many variables included")
  }
}





# plot_ggseg(calc_dvalues(healthy_frame, patient_frame, prep_ggseg=TRUE))


jpeg(filename = paste0(PLOT_DIR, "/plot.jpeg"))
plot <- plot_ggseg(calc_dvalues(healthy_frame, patient_frame, prep_ggseg=TRUE))
plot
dev.off()






# if other grouping variable --> check levels, if grösser 3 print auf jedenfall warning dass als grouping var verwendet,
