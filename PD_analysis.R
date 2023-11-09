# the goal is to write a script, that is callable from the command line, that computes some basics of the comparison between PD and HC, FS and DL, plots the brain differences and
# similarities, adventages and disadventages from DL. Then into the SCN.
# showing, that the results are komisch
# Afterwards the Motion Dataset, and the comparision between FreeSurfer and DL+Direct, adventages, disadventages
# finishing up with bringing together the Movement Dataset and the PD Dataset, from the classification to the comparison in the metrics

# all of this should come into a Bericht, commented, so that you can re-read it. So in Markdown?

# pack nicht zu viel in 1 fun, lieber verschachteln!!

### Libraries ###########################################################
library(readr)
library(tidyverse)
library(ggplot2)
library(ggpubr) 
library(dplyr)
library(ggseg)
library(magrittr)
library(igraph)
library(ggseg)
library(tidyr)
library(datawizard)
library(effectsize)
load_basic_packages()
library(plyr)


# library(ggsignif)
# library(afex) 
# # library(hrbrthemes) # does not work, dependencies missing
# library(datawizard)
# library(tibble)
# library(reshape2)
# library(grid)
# library(scales)
# library(ggrepel)
# library(stringr)
# # packages for the regression and anova
# library(lmerTest)
# library(broom)
# # library(flex)
# library(devtools) # to get flexplot from github
# # packages from igraph file:
# library(corrr)
# library(Matrix)
# library(lme4)
# library(tidyselect)
# library(gridExtra)
# library(corrplot)
# library(threejs)

### read files ##############################################

#PATH's
BASE_DIR <- "/home/user/PD_analysis/"
FILES_DIR <- "/home/user/PD_analysis/files/"
OUTPUT_DIR <- "/home/user/PD_analysis/out/"

setwd(BASE_DIR)

if(!dir.exists(BASE_DIR)) dir.create(BASE_DIR)
if(!dir.exists(FILES_DIR)) dir.create(FILES_DIR)
if(!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)

# list files in file dir
filelist <- list.files("files", pattern="*.csv", full.names=TRUE)
# read the files in the list
dataframes <- lapply(filelist, function(i){read_csv(i)})
names(dataframes) <- sub("\\.csv", "", basename(filelist))

# gives a list containing all dataframes. These have been preprocessed, so you would need to change the df reading
# structure should be Subjects as rows, first col "Subject", followed by required roi's


# in case of rather having single dataframes:

# for (i in filelist){
#   assign(paste0("df_", sub("\\.csv", "", basename(i))), read_csv(i))
# }


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


          
write.csv(as.data.frame(roi), "/home/user/PD_analysis/region.csv", row.names = FALSE)
write_file(region, "/home/user/PD_analysis/region.txt")
write_delim(x = region, file = "/home/user/PD_analysis/region.txt")
write.table(as.data.frame(region), file = "/home/user/PD_analysis/region.txt", sep = "")
### Variables ######################################################

roi <- names(dataframes$HCDL_new)[grepl("lh_|rh_", names(dataframes$HCDL_new))] %>% sub(patter = "_thickness", replacement= "", x = .)
region <- names(dataframes$HCDL_new)[grepl("lh_|rh_", names(dataframes$HCDL_new))]



N_SAMPLE_NUM <- 10
### basic comparisons ############################################################

# Differences in regionwise thickness, PD normed at HC

# important for norming
remove_all_labels <- function(df){
  df_rem <- df[, -which(names(df) %in% c("age", "software", "group", "Subject", "lh_MeanThickness_thickness", "rh_MeanThickness_thickness"))] #removes all the selected columns
  return(df_rem)
}

compare_thickness <- function(df.hc, df.pd, software){
  df_normed <- datawizard::standardize(remove_all_labels(df.pd), reference = remove_all_labels(df.hc))
  print(df_normed)
  df_map <- data.frame(label = roi, p = colMeans(df_normed[region]))
  seg <- ggseg(df_map, atlas = dk, 
        colour = "white",
        size = .1, 
        position = "stacked",
        mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", software, ", PD normed at HC")) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-1.3, 1.3))
  return(seg)
}

compare_thickness(df.hc = dataframes$HCDL_new, df.pd = dataframes$PDDL_labeled, software = "DL")

# macht so noch keinen sinn, die df's haben unterschiedliche grössen, Normierung so nicht ganz korrekt --> hier nochmal genau kontrollieren,
# warum eig genormt wurde, und warum so genormt. 1. möglichkeit pd an gesunden normen -->  Problem : untersch Grösse der VPN --> Lösung 1000x zufällige Stichprobe ziehen und Mittelwert berechnen aller normierungen
# 2. möglichkeit beide einzeln normen und voneinander abziehen --> Verlust an Info, Problem der untersch grösse
# 3. möglichkeit nicht normen und einfach Mittelwerte PD von HC's abziehen

compare_thickness_simple <- function(df.hc, df.pd, software){
  # simple bc just subtraction hc - pd of the means
  df_sub <- apply(X = df.pd[region], 2, FUN = mean) - apply(X = df.hc[region], 2, FUN = mean) 
  #df_sub <- remove_all_labels(df.hc) - remove_all_labels(df.pd)
  print(df_sub)
}

compare_thickness_simple(df.hc = dataframes$HCDL_new, df.pd = dataframes$PDDL_labeled, software = "DL")
 # funktioniert, aber PD jetzt in allen Regionen viel weniger Thickness, nicht normal so



sampled_mean <- function(df){
  # pull sample --> mean
  # sample size based on the smallest group, in my case Parkinson Patients vs HC's
  num_samples <- min((df %>% group_by(group) %>%summarise(n = n(), .groups = 'drop'))$n)
  # get only the HC's to grab
  df <- df %>% filter(group == "hc")
  # grab sample from the df
  x <- replicate(N_SAMPLE_NUM, sample_n(df[region], num_samples), simplify = FALSE)
  # add the samples and calculate the mean
  y <- Reduce("+", x)/N_SAMPLE_NUM
  return(y)
}

sampled_mean(DL_Frame)


substract_means <- function(df){
  pd <- df %>% filter(group == "pd")
  df_sub <- apply(X = pd[region], 2, FUN = mean) - apply(X = sampled_mean(df), 2, FUN = mean)
  print(df_sub)
}
substract_means(DL_Frame)

compare_thickness <- function(df){
  df.pd <- df %>% filter(group == "pd")
  df_normed <- datawizard::standardize(df.pd, reference = sampled_mean(df))
  #print(df_normed)
  df_map <- data.frame(label = roi, p = colMeans(df_normed[region]))
  print(df_map)
  seg <- ggseg(df_map, atlas = dk, 
               colour = "white",
               size = .1, 
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", unique(df$software), ", PD normed at HC")) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-8, 8))
  return(seg)
}
normed <- compare_thickness(DL_Frame)

compare_thickness <- function(df){
  df.pd <- df %>% filter(group == "pd")
  df_sub <- df.pd[region] - sampled_mean(df)
  print(df_sub)
  df_map <- data.frame(label = roi, p = colMeans(df_sub[region]))
  print(df_map)
  seg <- ggseg(df_map, atlas = dk, 
               colour = "white",
               size = .1, 
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", unique(df$software), ", PD-HC")) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-1, 1))
  return(seg)
}
substracted <- compare_thickness(DL_Frame)

normed
substracted
dim(sampled_mean(DL_Frame))
test <- datawizard::standardize(k[region], reference = sampled_mean(DL_Frame))
apply(X = test, MARGIN = 2, mean)


k <- DL_Frame %>% filter(group == "pd")
dim(k[region])
View(k[region])

View(sampled_mean(DL_Frame))

# immernoch keine Ahnung, warum die Werte so off sind, wenn ich die PD an den HC norme

# Hier standardize mit durchschnitt von Sampling der HC's
compare_thickness <- function(df){
  df.pd <- df %>% filter(group == "pd")
  df.pd <- as.data.frame(df.pd[region])
  df.hc <- sampled_mean(df[df$age > 45,])
  df_normed <- datawizard::standardize(df.pd, reference = df.hc)
  # hc_stand <- standardize(sampled_mean(df))
  # print("pd standardized \n")
  # print(pd_stand)
  # print("hc standardized \n")
  # print(hc_stand)
  # df_sub <- pd_stand - hc_stand
  # print(df_sub)
  df_map <- data.frame(label = roi, p = colMeans(df_normed[region]))
  seg <- ggseg(df_map, atlas = dk, 
               colour = "white",
               size = .1, 
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", unique(df$software), ", PD-HC")) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-1.3, 1.3))
  return(seg)
}

compare_thickness(DL_Frame)



# hier standardize mit gesamten Hc Frame
compare_thickness <- function(df.hc, df.pd, software){
  df.hc <- as.data.frame(df.hc[region])
  df.pd <- as.data.frame(df.pd[region])
  print(paste0("This is the HC Frame: "))
  print(df.hc)
  df_normed <- datawizard::standardize(df.pd, reference = df.hc)
  # print(df_normed)
  df_map <- data.frame(label = roi, p = colMeans(df_normed[region]))
  seg <- ggseg(df_map, atlas = dk, 
               colour = "white",
               size = .1, 
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", software, ", PD normed at HC")) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-1.3, 1.3))
  return(seg)
}

compare_thickness(df.hc = dataframes$HCDL_new[dataframes$HCDL_new$age > 45, ], df.pd = dataframes$PDDL_labeled, software = "DL")

dataframes$HCDL_new$age <- hcs$age
# hier standardisieren mit gematchten HC's
compare_thickness <- function(df.hc, df.pd, software){
  df.hc <- as.data.frame(df.hc[region])
  df.pd <- as.data.frame(df.pd[region])
  print(paste0("This is the HC Frame: "))
  print(df.hc)
  df_normed <- datawizard::standardize(df.pd, reference = df.hc)
  # print(df_normed)
  df_map <- data.frame(label = roi, p = colMeans(df_normed[region]))
  seg <- ggseg(df_map, atlas = dk, 
               colour = "white",
               size = .1, 
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", software, ", PD normed at HC")) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", limits= c(-1.3, 1.3))
  return(seg)
}

compare_thickness(df.hc = HCDL_w_age, df.pd = PDDL_w_age, software = "DL")

# das Problem ist das standardisieren an einem anderen Df. je nach dem welcher Df als Referenz genommen wird gibt es radikal unterschiedliche Resultate.

summary(dataframes$HCDL_new[2:4])
summary(sampled_mean(DL_Frame)[1:3])
summary(HCDL_w_age[, 2:4])

apply(dataframes$HCDL_new[2:4], 2, sd)
apply(sampled_mean(DL_Frame)[1:3], 2, sd)
apply(HCDL_w_age[, 2:4], 2, sd)

# checking the linear models
lm_Pd <- function(metric, df) {
  return(lm(as.formula(paste0(metric, ' ~ group + age')), df))
}
View(DL_Frame)
lin_mods <- list()
lin_mods <- sapply(region, lm_Pd, DL_Frame)

test <- lm(as.formula(paste0("lh_bankssts_thickness", ' ~ group + age')), DL_Frame)
lm(formula = DL_Frame$lh_bankssts_thickness, ' ~ age + group', data = DL_Frame)
lm(data = DL_Frame, formula = lh_bankssts_thickness ~ group + age)
cohens_d(standardize(test))
cohens_d(lh_bankssts_thickness ~ group, data = DL_Frame)

summary(test)
coef(test)[2]



lm_Pd_coef <- function(metric, df) {
  return(lm(as.formula(paste0(metric, ' ~ age')), df))
}
View(DL_Frame)
lin_mods <- list()
lin_mods <- sapply(region, lm_Pd_coef, DL_Frame)
lin_mods
DL_Frame %>% rstatix::cohens_d(lh_cuneus_thickness ~ group, var.equal = TRUE)
effectsize::cohens_d(data = DL_Frame, DL_Frame$lh_bankssts_thickness~DL_Frame$group, pooled_sd = TRUE)



sapply(region, lm_corrected, DL_Frame)
group1residuals <- resid(lm(as.formula(paste0("lh_bankssts_thickness", ' ~ age')), DL_Frame[DL_Frame$group == "hc", ]))
test <- lm(as.formula(paste0("lh_bankssts_thickness", ' ~ age')), DL_Frame[DL_Frame$group == "hc", ])
group2_predictions <- predict(test, newdata = DL_Frame[DL_Frame$group == "pd", ])
group2residuals <- DL_Frame$lh_bankssts_thickness[DL_Frame$group == "pd"]- group2_predictions
mean(resid(test))
mean(group2residuals)

(mean(group1residuals) - mean(group2residuals))/sqrt((var(group1residuals)+var(group2residuals))/2)

n1 <- length(group1residuals)
n2 <- length(group2residuals)
var1 <- var(group1residuals)
var2 <- var(group2residuals)

pooled <- ((n1-1)*var1 + (n2-1)*var2) / (n1+n2-2)

(mean(group1residuals) - mean(group2residuals))/pooled


k <- cohens_d(group1residuals, group2residuals)

DL_Frame$age <- as.numeric(DL_Frame$age)

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
  print(c(df.pd))
  
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
    print(paste0("pd_pred", pd_pred))
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

plot_ggseg <- function(df){
  seg <- ggseg(df, atlas = dk, 
               colour = "white",
               size = .1, 
               position = "stacked",
               mapping = aes(fill = p)) +
    theme_void() +
    ggtitle(paste0("Thickness-Segmentation for ", unique(df$software), ", PD-HC")) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", limits= c(-1, 1))
  return(seg)
}

plot_ggseg(calc_dvalues(DL_Frame, prep_ggseg = TRUE))

# resulate sehen anders aus, als die von laansma und auch als die vorherigen, das ist jetzt mit alterskorrektur, sieht so aus, als würde es bestehende effekte verstärken... 
# alles hier nochmal gut überprüfen und dann einfügen in pipeline mit df extraktion von ordner --> HC PD ersetzen durch Healthies und Sickkies, damit übertragbar auf verschiedene Datasets!
