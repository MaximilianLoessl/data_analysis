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

# healthy_frame <- read.csv("/home/user/PD_analysis/files/healthy_frame.csv")
# patient_frame <- read.csv("/home/user/PD_analysis/files/patient_frame.csv")
# OUT_DIR <- paste0(file_dir, "/out/")
# PLOT_DIR <- paste0(file_dir, "/plots/")



set.seed(42)

N_SAMPLE_NUM <- 1000

thresholds <- seq(0, 1, 0.01)

region <- read.csv(file = paste0(file_dir, "/region.csv"))
region <- region[, 1][grepl("lh_|rh_", region[, 1])]
roi <- region
core <- region %>% append("Subject", after = 0) # this is the core structure of the df
core <- core %>% append(c("age", "group")) # to check if there is another column as grouping variable

# if (any(region %in% dk$data$label)) {
#   print("Desikan-Killiany atlas")
# } else if (any(region %in% aseg$data$label)) {
#   print("Subcortical atlas aseg")
# } else {
#   print("Atlas unknown, segmentation not possible")
# }

# this is possibly more sensitive bc it greps for the region. The question is if we want it to be sensitive bc
# we would want an exact match. 
if (any(sapply(dk$data$region, function(x) grepl(x, region)))){
  print("Desikan-Killiany atlas")
  atlas <- dk
} else if (any(sapply(aseg$data$region, function(x) grepl(x, region)))){
  print("Subcortical atlas aseg")
  atlas <- aseg
} else {
  print("Atlas unknown, segmentation not possible")
}




# patient_frame$MODEL <- "DL"
# healthy_frame$MODEL <- "test2"
# print(names(healthy_frame))
# print(names(patient_frame))
# 
# identical(names(healthy_frame %>% select(sort(names(.)))), names(patient_frame %>% select(sort(names(.)))))

sprintf("length names healthy frame: %s", length(names(healthy_frame)))
sprintf("length names patients frame: %s", length(names(patient_frame)))
# identical(names(healthy_frame), core)

# setdiff(names(healthy_frame), names(patient_frame))
# check if hc and pt columnnames match
if (!identical(names(healthy_frame %>% select(sort(names(.)))), names(patient_frame %>% select(sort(names(.)))))){
  stop("Provide dataframes with matching columns")
} else {
  print("Checking if columnnames match - match")
}



# all.equal(names(healthy_frame), core)
# healthy_frame <- healthy_frame[core]
# patient_frame <- patient_frame[core]

# now we got the problem that we allowed to bring in different grouping variables in the file_prep. if we bring in only one grouping var, it should be no problem, normal comparison.
# but if there comes in a grouping variable, we need to adapt the plotting to it, taking differing number of unique values into account. as we defined the region in a file, we add
# Subject, age and group and together they build the core. If age was already in the dataframe as AGE or Age, the name changed to age.
# now we check if the Column names match the core. If not, extra column got computed.

if (identical(names(healthy_frame), core)) {
print("frames match the core structure")
    calc_dvalues <- function(healthy_frame, patient_frame, prep_ggseg=FALSE){ # prep ggseg only if the prep for ggseg is needed, otherways just return age corrected value
    # fun get the residuals --> would have been better with apply than  for loop
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
      # d_value <- effectsize::cohens_d(x = hc_resids, y = pd_resids)
d_value <- effectsize::cohens_d(x = pd_resids, y = hc_resids)
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
    seg <- ggseg(df, atlas = atlas,
                 colour = "white",
                 size = .1,
                 position = "stacked",
                 mapping = aes(fill = p)) +
      theme_void() +
      ggtitle(paste0("Thickness-Segmentation for d-Values for the Residuals of Healthy Controls vs Patients")) +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-1, 1))
    # ggsave(filename = paste0(OUT_DIR, ".pdf", plot = seg))
    return(seg)
  }
  filename = paste0(PLOT_DIR, "/plot.jpeg")
  print(filename)
} else {
  # else for the case that the frames are NOT identical with the core --> if --keep in file prep
print("frames don't match the core structure")
  print("columns you wanted to keep: ")
  diff_cols <- colnames(healthy_frame)[!colnames(healthy_frame) %in% core]
  print(unique(healthy_frame[diff_cols]))
  
  if (length(diff_cols) == 0) {
    # putting a stop for the case this file gets used without proper prep
    stop("Is the Frame missing of the columns: Subject, age or group? You should start with the file 'file_prep.R'")
  } else if (length(diff_cols) == 1){
    # if there is only 1 different column
    if (identical(unique(healthy_frame[diff_cols]), unique(patient_frame[diff_cols]))) {
      # check if the values of the different column are the same --> use it as title
      print(paste0("we use ", diff_cols, " as title."))
      calc_dvalues <- function(healthy_frame, patient_frame, prep_ggseg=FALSE){ # prep ggseg only if the prep for ggseg is needed, otherways just return age corrected value
      # fun get the residuals --> would have been better with apply than  for loop
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
        # d_value <- effectsize::cohens_d(x = hc_resids, y = pd_resids)
d_value <- effectsize::cohens_d(x = pd_resids, y = hc_resids)
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
      seg <- ggseg(df, atlas = atlas,
                   colour = "white",
                   size = .1,
                   position = "stacked",
                   mapping = aes(fill = p)) +
        theme_void() +
        ggtitle(paste0("Thickness-Segmentation for ", unique(healthy_frame[diff_cols]), " - d-Values for the Residuals of Healthy Controls vs Patients")) +
        scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits= c(-1, 1))
      # ggsave(filename = paste0(OUT_DIR, ".pdf", plot = seg))
      return(seg)
    }
    filename = paste0(PLOT_DIR, "/", unique(healthy_frame[diff_cols]),"_plot.jpeg")
    print(filename)
    } else {
      print(paste0("Too many levels in a grouping var. There is the Group-Variable for HC/PT and the ", unique(diff_cols), "-Variable with ", unique(healthy_frame[diff_cols]), "/", unique(patient_frame[diff_cols])))
    }
  } else {
    print(paste0("there are ", length(diff_cols), "extra columns"))
    print("to many variables included")
  }
}

# hier montag weiter machen, unterer Teil erst nur rein kopiert der funktion die unterscheidet zwischen mit oder ohne zusatzcols --> müsste einfacheren weg geben als for loop für die untersch variablen
# plot <- plot_ggseg(df = calc_dvalues(healthy_frame = healthy_frame, patient_frame = patient_frame, prep_ggseg=TRUE))
# plot
# 
# calc_dvalues(healthy_frame = healthy_frame, patient_frame = patient_frame, prep_ggseg=TRUE)

# plot_ggseg(calc_dvalues(healthy_frame, patient_frame, prep_ggseg=TRUE))

# jpeg(filename = paste0(PLOT_DIR, "/", unique(healthy_frame[diff_cols]),"plot.jpeg"))
#     plot <- plot_ggseg(df = calc_dvalues(healthy_frame = healthy_frame, patient_frame = patient_frame, prep_ggseg=TRUE))
#     plot
#     dev.off()
    
jpeg(filename = filename)
plot <- plot_ggseg(df = calc_dvalues(healthy_frame = healthy_frame, patient_frame = patient_frame, prep_ggseg=TRUE))
plot
dev.off()



