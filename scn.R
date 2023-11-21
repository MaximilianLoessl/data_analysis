### SCN ####

# The goal is, to read the files in the files dir, in the format of the output from the file prep--> subject, brain areas eihter full or one hemi, age and group
# then compute the SCN --> compute the plots like before
# flags for different scn measurements

# to tos:
# comment exactly and option to run directly without terminal
# add the possibility, that there is another variable than group inside. if that is the case, the plots should look different. 

# Description
# this file runs a healthy and a patients frame and compares them regarding one measure of the graph theroy. (default is global efficiency)
# 1000x sampling with 80% of the smaller group. (leads to more variance in the bigger frame if difference is big)
# the values get age corrected, can be turned off, For example if age is missing

### Packages: #################################################################################




# use this when used packages are fixed
# load_packages <- function(packages){
#   lapply(packages, library, character.only = TRUE)
# }
# 
# #install packages if needed and load them, list needs to be controlled
# list.of.packages <- c("tidyverse", "optparse", "dplyr", "stringr")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# load_packages(list.of.packages)
library(optparse)
library(tidyverse)

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

# working directory, is only the same like file dir if cd there
w_dir <- getwd()
# file dir, where file is saved
file_dir <- getCurrentFileLocation()

sprintf("this is the file directory: %s", file_dir)

# set the flags of the cmd line input
option_list = list(
  make_option(c("-y", "--healthy"), type = "character", default = paste0(file_dir, "/files/healthy_frame.csv"), help = "Give the path tho the healthy controls. \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "HEALTHY"),
  make_option(c("-p", "--patient"), type = "character", default = paste0(file_dir, "/files/patient_frame.csv"), help = "Give the path tho the patients_frame \n     Should be a dataframe containing all the subjects.  [example: /path/to/HC]", metavar = "PATIENT"),
  make_option(c("-o", "--out"), type = "character", default = paste0(file_dir, "/out/"), help = "output file name [default \"%default\"]", metavar = "OUT_DIR"),
  make_option(c("-l", "--plot"), type = "character", default = paste0(file_dir, "/plots/"), help = "plot file name [default \"%default\"]", metavar = "PLOT_DIR"),
  make_option(c("-m", "--measure"), type = "character", default = "glob_efficiency", help = "Options: glob_efficiency, number_edges, mean_pathlength,  glob_transitivity [default \"%default\"]", metavar = "MEASURE")
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
measure <- opt$measure



# healthy_frame <- read.csv("/home/user/PD_analysis/files/healthy_frame.csv")
# patient_frame <- read.csv("/home/user/PD_analysis/files/patient_frame.csv")
# OUT_DIR <- "/home/user/PD_analysis/out/"
# PLOT_DIR <- "/home/user/PD_analysis/plots/"
# measure <- pick one from down 

# this is to get the name of the current measure in quotes for titles
current_measure <- dQuote(measure)

set.seed(42)

N_SAMPLE_NUM <- 10

thresholds <- seq(0, 1, 0.01)

region <- read.csv(file = paste0(file_dir, "/region.csv"))
region <- region[, 1][grepl("lh_|rh_", region[, 1])]
roi <- region
core <- region %>% append("Subject", after = 0) # this is the core structure of the df


if (!all.equal(names(healthy_frame), names(patient_frame))){
  stop("Provide dataframes with matching columns")
} else {
  print("Checking if columnnames match")
}


# my fun for global eff
glob_efficiency <- function(threshold, cor_matrix) {
  # binary
  cor_matrix[abs(cor_matrix) <= threshold] <- 0
  cor_matrix[abs(cor_matrix) > threshold] <- 1
  # graph from cormatrix
  gdiro <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected", diag = FALSE)
  # calculate global efficiency
  g.ef <- global_efficiency(gdiro ,weights = NULL, directed = FALSE)
  return(g.ef)
}

# fun to calculate the number of edges
number_edges <- function(threshold, cor_matrix) {
  # binary
  cor_matrix[abs(cor_matrix) <= threshold] <- 0
  cor_matrix[abs(cor_matrix) > threshold] <- 1
  # graph from cormatrix
  gdiro <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected", diag = FALSE)
  # calculate mean pathlength
  nr.edges <- length(E(gdiro)) #with the length is the same as gsize() 
  return(nr.edges)
}

# my fun for mean pathlength

mean_pathlength <- function(threshold, cor_matrix) {
  # binary
  cor_matrix[abs(cor_matrix) <= threshold] <- 0
  cor_matrix[abs(cor_matrix) > threshold] <- 1
  # graph from cormatrix
  gdiro <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected", diag = FALSE)
  # calculate mean pathlength
  g.apl <- mean_distance(gdiro, directed = FALSE, unconnected = TRUE)
  return(g.apl)
}
# fun global transitivity
glob_transitivity <- function(threshold, cor_matrix) {
  # binary
  cor_matrix[abs(cor_matrix) <= threshold] <- 0
  cor_matrix[abs(cor_matrix) > threshold] <- 1
  # graph from cormatrix
  gdiro <- graph_from_adjacency_matrix(cor_matrix, mode = "undirected", diag = FALSE)
  # calculate mean pathlength
  g.tr <- transitivity(gdiro, type = "global")
  return(g.tr)
}

#control for age -> ONLY IF DEMOGRAPHICS CORRECT, so needs on and off with true false
lm_corrected <- function(metric, df) {
  return(resid(lm(as.formula(paste0(metric, " ~ age")), df)))
}

sample_data <- function(df, N=num_samples_fraction) { #N not length models anymore
  # get a random 80% from the df
  s <- sample_n(df, N)
  # apply the thresholds to the fun for efficiency and the fun to the corr data of the sample
  sapply(thresholds, measure, abs(cor(s)))
}



plot_scn <- function(healthy_frame, patient_frame, correct_values = TRUE, scn_fun=measure){
  #create the empty frames
  df_all = data.frame()
  plots_cor = list()
  
  dataframe <- rbind(healthy_frame, patient_frame)
  groups <- unique(dataframe$group)
  num_samples <- min(table(dataframe$group))
  num_samples_fraction <- floor(num_samples*0.8)
  print(sprintf('Smallest group: %i, using %i for sampling', num_samples, num_samples_fraction))
  for(i in groups){
    df <- dataframe[dataframe$group == i, ]
    if(correct_values){
      # calculate corrected values (using lm with age+etiv)
      print("corrected usage")
      corr_data <- sapply(roi, lm_corrected, df)
    } else {
      # use uncorrected values
      print("uncorrectet usage")
      corr_data <- df %>% select(roi) # select roi to cut out the Subjects and stuff, only roi's
    }
    sampled <- replicate(n=N_SAMPLE_NUM, sample_data(data.frame(corr_data), N = num_samples_fraction))
    sampled_mean <- sampled %>% rowMeans()
    sampled_sd <- sampled %>% rowSds()
    # put resultframe together with mean sd and the CI
    df_result <- data.frame(thresholds=thresholds, result=sampled_mean, result_SD=sampled_sd, result_lo=sampled_mean-sampled_sd, result_hi=sampled_mean+sampled_sd, group=i)
    # bind the results to the dataframe
    df_all <- bind_rows(df_all, df_result)
    
    # plot the cormatrix for DL and FS (not binary)
    cor_plot <- ggcorrplot(cor(corr_data), hc.order = FALSE, type = 'full', lab = FALSE, show.legend = FALSE)+
      theme_void()+labs(title=paste0("Corrplot for: ", i))+theme(legend.position = 'none')
    plots_cor <- c(plots_cor, list(cor_plot))
  }
  p <- ggplot(df_all, aes(x=thresholds, y=result, color=group, ymin=result_lo, ymax=result_hi))+
    geom_line()+
    # errorbars if sampling is true
    geom_errorbar(alpha=0.5)+
    theme(aspect.ratio=1)+
    labs(title=paste0("plot for", current_measure))+theme(plot.title = element_text(hjust=0.5, face='bold'))
  
  plots <- ggarrange(p, ggarrange(plotlist=plots_cor, ncol=2, nrow=2), ncol=1, nrow=2)
  
  return(list(data=df_all, plots=plots))
}
  



results <- plot_scn(healthy_frame, patient_frame)

# display the plots
# results$plots


df_scn_all <- results$data

df_scn_all$DIAG <- factor(df_scn_all$group, levels = c(unique(healthy_frame$group), unique(patient_frame$group)))



# calculate the limits at .5 threshold, lowest value *0.98 and the highest *1.02 to have some rand übrig 
lims <- c(min(df_scn_all[df_scn_all$thresholds == 0.50, ]$result_lo)*0.98, max(df_scn_all[df_scn_all$thresholds == 0.50, ]$result_hi)*1.02)

# fist plot for the fun global efficiency, Threshold
p1 <- ggplot(df_scn_all, aes(x=thresholds, y=result, ymin=result_lo, ymax=result_hi, group=DIAG))+ #could take interaction out, bc model = diag
  geom_line(aes(color= group, linetype = group), linewidth=0.5)+ # linetype not really important
  geom_errorbar(aes(color=group, linetype=DIAG), alpha=0.5, linewidth=0.5)+
  # dashed line at .5
  geom_vline(xintercept = 0.5, linetype='dashed', linewidth=0.3, alpha=0.6)+
  # get the little bar to see where the second plot is from
  annotate('rect', fill = 'lightblue', alpha = 0.5, xmin=0.49, xmax=0.51, ymin=lims[1], ymax=lims[2])+  # annotate great for putting in stuff
  theme(aspect.ratio=1, legend.position = c(0.15, 0.25))+scale_alpha(guide = 'none')+
  ylab(current_measure)+xlab('Threshold')+labs(linetype='Group', color='Method')
p1

# values at 0.5
df_scn_all[df_scn_all$thresholds == 0.50, ]

# plot for the view at .5, the results at threshold .5 with the sd errorbars
p2 <- ggplot(df_scn_all[df_scn_all$thresholds == 0.50, ], aes(y=result, ymin=result_lo, ymax=result_hi, x=group, color=group))+
  geom_point()+
  geom_errorbar(aes(linetype=DIAG))+
  facet_wrap(~DIAG, scales='free_x')+
  ylim(lims)+
  ylab(paste0(current_measure, "at 0.5 Threshold"))+xlab('')+theme(legend.position = 'none', panel.background = element_rect(fill = 'lightblue'))
p2
# put plots together
ggarrange(plotlist=list(p1, p2), widths = c(2, 1), nrow=1, ncol=2)
# # save them in the outdir
ggsave(filename = paste(OUT_DIR, "/", current_measure, '.pdf', sep=''), width=10, height = 6)





