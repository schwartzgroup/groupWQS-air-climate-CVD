###############################################################################
# Project: SID mixtures                                                       #
# Code: Analysis using group WQS, 3 groups                                    #
###############################################################################

############################# 0. Setup ##############################
rm(list=ls())
gc()

library(dplyr)
library(stringr)
library(groupWQS)
library(ggplot2)
library(fastDummies)

dir_data <- '/n/home07/yaw624/WQS/data/'
dir_data_save <- '/n/home07/yaw624/WQS/data/'
dir_results <- '/n/home07/yaw624/WQS/results/'

group_list <- list(c("ec","nh4","no3","oc","so4","o3","no2"),
                   c("wmean_temporal","smean_temporal"),
                   c("wsd_temporal","ssd_temporal"))

set.seed(1234)



############################# 1. IHD ##############################
dat_ihd_agg_zcta <- readRDS(paste0(dir_data_save,'dat_ihd_agg_zcta.rds'))
dat_ihd_agg_zcta <- as.data.frame(dat_ihd_agg_zcta)
dat_ihd_agg_zcta <- dat_ihd_agg_zcta[sample(1:nrow(dat_ihd_agg_zcta)),]
dat_ihd_agg_zcta$ZCTA <- NULL
dat_ihd_agg_zcta <- dat_ihd_agg_zcta[!is.infinite(rowSums(dat_ihd_agg_zcta)),]
dat_ihd_agg_zcta <- dat_ihd_agg_zcta[complete.cases(dat_ihd_agg_zcta),]
dat_ihd_agg_zcta$wmean_temporal <- -dat_ihd_agg_zcta$wmean_temporal

# standardize all
dat_ihd_agg_zcta$ec <- scale(dat_ihd_agg_zcta$ec)
dat_ihd_agg_zcta$nh4 <- scale(dat_ihd_agg_zcta$nh4)
dat_ihd_agg_zcta$no3 <- scale(dat_ihd_agg_zcta$no3)
dat_ihd_agg_zcta$oc <- scale(dat_ihd_agg_zcta$oc)
dat_ihd_agg_zcta$so4 <- scale(dat_ihd_agg_zcta$so4)
dat_ihd_agg_zcta$o3 <- scale(dat_ihd_agg_zcta$o3)
dat_ihd_agg_zcta$no2 <- scale(dat_ihd_agg_zcta$no2)
dat_ihd_agg_zcta$wmean_temporal <- scale(dat_ihd_agg_zcta$wmean_temporal)
dat_ihd_agg_zcta$wsd_temporal <- scale(dat_ihd_agg_zcta$wsd_temporal)
dat_ihd_agg_zcta$smean_temporal <- scale(dat_ihd_agg_zcta$smean_temporal)
dat_ihd_agg_zcta$ssd_temporal <- scale(dat_ihd_agg_zcta$ssd_temporal)
dat_ihd_agg_zcta$wmean_spatial <- scale(dat_ihd_agg_zcta$wmean_spatial)
dat_ihd_agg_zcta$wsd_spatial <- scale(dat_ihd_agg_zcta$wsd_spatial)
dat_ihd_agg_zcta$smean_spatial <- scale(dat_ihd_agg_zcta$smean_spatial)
dat_ihd_agg_zcta$ssd_spatial <- scale(dat_ihd_agg_zcta$ssd_spatial)
dat_ihd_agg_zcta$r_age_1 <- scale(dat_ihd_agg_zcta$r_age_1)
dat_ihd_agg_zcta$r_age_2 <- scale(dat_ihd_agg_zcta$r_age_2)
dat_ihd_agg_zcta$r_age_3 <- scale(dat_ihd_agg_zcta$r_age_3)
dat_ihd_agg_zcta$r_age_4 <- scale(dat_ihd_agg_zcta$r_age_4)
dat_ihd_agg_zcta$r_race_1 <- scale(dat_ihd_agg_zcta$r_race_1)
dat_ihd_agg_zcta$r_male <- scale(dat_ihd_agg_zcta$r_male)
dat_ihd_agg_zcta$pct_non_hispanic_white <- scale(dat_ihd_agg_zcta$pct_non_hispanic_white)
dat_ihd_agg_zcta$pct_black <- scale(dat_ihd_agg_zcta$pct_black)
dat_ihd_agg_zcta$pct_edu_some_college <- scale(dat_ihd_agg_zcta$pct_edu_some_college)
dat_ihd_agg_zcta$pct_poverty <- scale(dat_ihd_agg_zcta$pct_poverty)
dat_ihd_agg_zcta$pct_renting <- scale(dat_ihd_agg_zcta$pct_renting)
dat_ihd_agg_zcta$year <- scale(dat_ihd_agg_zcta$year)

X <- make.X(dat_ihd_agg_zcta, num.groups = 3, groups = group_list)
x.s <- make.x.s(dat_ihd_agg_zcta, num.groups = 3, groups = group_list)
Y <- dat_ihd_agg_zcta$r_tot
Z <- dat_ihd_agg_zcta[,c('r_age_1','r_age_2','r_age_3','r_age_4','r_race_1','r_male','pct_non_hispanic_white','pct_black',
                         'pct_edu_some_college','pct_poverty','pct_renting','year',
                         'wmean_spatial','wsd_spatial','smean_spatial','ssd_spatial')]
Z <- as.matrix(Z)

mod_groupWQS_ihd <- gwqs.fit(y = Y, x = X, z = Z, x.s = x.s, B=100, n.quantiles = 10, func = "continuous")

saveRDS(mod_groupWQS_ihd,file=paste0(dir_results,'mod_groupWQS_3groups_ihd.rds'))



############################# 2. CD ##############################
dat_cd_agg_zcta <- readRDS(paste0(dir_data_save,'dat_cd_agg_zcta.rds'))
dat_cd_agg_zcta <- as.data.frame(dat_cd_agg_zcta)
dat_cd_agg_zcta <- dat_cd_agg_zcta[sample(1:nrow(dat_cd_agg_zcta)),]
dat_cd_agg_zcta$ZCTA <- NULL
dat_cd_agg_zcta <- dat_cd_agg_zcta[!is.infinite(rowSums(dat_cd_agg_zcta)),]
dat_cd_agg_zcta <- dat_cd_agg_zcta[complete.cases(dat_cd_agg_zcta),]
dat_cd_agg_zcta$wmean_temporal <- -dat_cd_agg_zcta$wmean_temporal

# standardize all
dat_cd_agg_zcta$ec <- scale(dat_cd_agg_zcta$ec)
dat_cd_agg_zcta$nh4 <- scale(dat_cd_agg_zcta$nh4)
dat_cd_agg_zcta$no3 <- scale(dat_cd_agg_zcta$no3)
dat_cd_agg_zcta$oc <- scale(dat_cd_agg_zcta$oc)
dat_cd_agg_zcta$so4 <- scale(dat_cd_agg_zcta$so4)
dat_cd_agg_zcta$o3 <- scale(dat_cd_agg_zcta$o3)
dat_cd_agg_zcta$no2 <- scale(dat_cd_agg_zcta$no2)
dat_cd_agg_zcta$wmean_temporal <- scale(dat_cd_agg_zcta$wmean_temporal)
dat_cd_agg_zcta$wsd_temporal <- scale(dat_cd_agg_zcta$wsd_temporal)
dat_cd_agg_zcta$smean_temporal <- scale(dat_cd_agg_zcta$smean_temporal)
dat_cd_agg_zcta$ssd_temporal <- scale(dat_cd_agg_zcta$ssd_temporal)
dat_cd_agg_zcta$wmean_spatial <- scale(dat_cd_agg_zcta$wmean_spatial)
dat_cd_agg_zcta$wsd_spatial <- scale(dat_cd_agg_zcta$wsd_spatial)
dat_cd_agg_zcta$smean_spatial <- scale(dat_cd_agg_zcta$smean_spatial)
dat_cd_agg_zcta$ssd_spatial <- scale(dat_cd_agg_zcta$ssd_spatial)
dat_cd_agg_zcta$r_age_1 <- scale(dat_cd_agg_zcta$r_age_1)
dat_cd_agg_zcta$r_age_2 <- scale(dat_cd_agg_zcta$r_age_2)
dat_cd_agg_zcta$r_age_3 <- scale(dat_cd_agg_zcta$r_age_3)
dat_cd_agg_zcta$r_age_4 <- scale(dat_cd_agg_zcta$r_age_4)
dat_cd_agg_zcta$r_race_1 <- scale(dat_cd_agg_zcta$r_race_1)
dat_cd_agg_zcta$r_male <- scale(dat_cd_agg_zcta$r_male)
dat_cd_agg_zcta$pct_non_hispanic_white <- scale(dat_cd_agg_zcta$pct_non_hispanic_white)
dat_cd_agg_zcta$pct_black <- scale(dat_cd_agg_zcta$pct_black)
dat_cd_agg_zcta$pct_edu_some_college <- scale(dat_cd_agg_zcta$pct_edu_some_college)
dat_cd_agg_zcta$pct_poverty <- scale(dat_cd_agg_zcta$pct_poverty)
dat_cd_agg_zcta$pct_renting <- scale(dat_cd_agg_zcta$pct_renting)
dat_cd_agg_zcta$year <- scale(dat_cd_agg_zcta$year)

X <- make.X(dat_cd_agg_zcta, num.groups = 3, groups = group_list)
x.s <- make.x.s(dat_cd_agg_zcta, num.groups = 3, groups = group_list)
Y <- dat_cd_agg_zcta$r_tot
Z <- dat_cd_agg_zcta[,c('r_age_1','r_age_2','r_age_3','r_age_4','r_race_1','r_male','pct_non_hispanic_white','pct_black',
                        'pct_edu_some_college','pct_poverty','pct_renting','year',
                        'wmean_spatial','wsd_spatial','smean_spatial','ssd_spatial')]
Z <- as.matrix(Z)

mod_groupWQS_cd <- gwqs.fit(y = Y, x = X, z = Z, x.s = x.s, B=100, n.quantiles = 10, func = "continuous")

saveRDS(mod_groupWQS_cd,file=paste0(dir_results,'mod_groupWQS_3groups_cd.rds'))



############################# 3. HF ##############################
dat_hf_agg_zcta <- readRDS(paste0(dir_data_save,'dat_hf_agg_zcta.rds'))
dat_hf_agg_zcta <- as.data.frame(dat_hf_agg_zcta)
dat_hf_agg_zcta <- dat_hf_agg_zcta[sample(1:nrow(dat_hf_agg_zcta)),]
dat_hf_agg_zcta$ZCTA <- NULL
dat_hf_agg_zcta <- dat_hf_agg_zcta[!is.infinite(rowSums(dat_hf_agg_zcta)),]
dat_hf_agg_zcta <- dat_hf_agg_zcta[complete.cases(dat_hf_agg_zcta),]
dat_hf_agg_zcta$wmean_temporal <- -dat_hf_agg_zcta$wmean_temporal

# standardize all
dat_hf_agg_zcta$ec <- scale(dat_hf_agg_zcta$ec)
dat_hf_agg_zcta$nh4 <- scale(dat_hf_agg_zcta$nh4)
dat_hf_agg_zcta$no3 <- scale(dat_hf_agg_zcta$no3)
dat_hf_agg_zcta$oc <- scale(dat_hf_agg_zcta$oc)
dat_hf_agg_zcta$so4 <- scale(dat_hf_agg_zcta$so4)
dat_hf_agg_zcta$o3 <- scale(dat_hf_agg_zcta$o3)
dat_hf_agg_zcta$no2 <- scale(dat_hf_agg_zcta$no2)
dat_hf_agg_zcta$wmean_temporal <- scale(dat_hf_agg_zcta$wmean_temporal)
dat_hf_agg_zcta$wsd_temporal <- scale(dat_hf_agg_zcta$wsd_temporal)
dat_hf_agg_zcta$smean_temporal <- scale(dat_hf_agg_zcta$smean_temporal)
dat_hf_agg_zcta$ssd_temporal <- scale(dat_hf_agg_zcta$ssd_temporal)
dat_hf_agg_zcta$wmean_spatial <- scale(dat_hf_agg_zcta$wmean_spatial)
dat_hf_agg_zcta$wsd_spatial <- scale(dat_hf_agg_zcta$wsd_spatial)
dat_hf_agg_zcta$smean_spatial <- scale(dat_hf_agg_zcta$smean_spatial)
dat_hf_agg_zcta$ssd_spatial <- scale(dat_hf_agg_zcta$ssd_spatial)
dat_hf_agg_zcta$r_age_1 <- scale(dat_hf_agg_zcta$r_age_1)
dat_hf_agg_zcta$r_age_2 <- scale(dat_hf_agg_zcta$r_age_2)
dat_hf_agg_zcta$r_age_3 <- scale(dat_hf_agg_zcta$r_age_3)
dat_hf_agg_zcta$r_age_4 <- scale(dat_hf_agg_zcta$r_age_4)
dat_hf_agg_zcta$r_race_1 <- scale(dat_hf_agg_zcta$r_race_1)
dat_hf_agg_zcta$r_male <- scale(dat_hf_agg_zcta$r_male)
dat_hf_agg_zcta$pct_non_hispanic_white <- scale(dat_hf_agg_zcta$pct_non_hispanic_white)
dat_hf_agg_zcta$pct_black <- scale(dat_hf_agg_zcta$pct_black)
dat_hf_agg_zcta$pct_edu_some_college <- scale(dat_hf_agg_zcta$pct_edu_some_college)
dat_hf_agg_zcta$pct_poverty <- scale(dat_hf_agg_zcta$pct_poverty)
dat_hf_agg_zcta$pct_renting <- scale(dat_hf_agg_zcta$pct_renting)
dat_hf_agg_zcta$year <- scale(dat_hf_agg_zcta$year)

X <- make.X(dat_hf_agg_zcta, num.groups = 3, groups = group_list)
x.s <- make.x.s(dat_hf_agg_zcta, num.groups = 3, groups = group_list)
Y <- dat_hf_agg_zcta$r_tot
Z <- dat_hf_agg_zcta[,c('r_age_1','r_age_2','r_age_3','r_age_4','r_race_1','r_male','pct_non_hispanic_white','pct_black',
                        'pct_edu_some_college','pct_poverty','pct_renting','year',
                        'wmean_spatial','wsd_spatial','smean_spatial','ssd_spatial')]
Z <- as.matrix(Z)

mod_groupWQS_hf <- gwqs.fit(y = Y, x = X, z = Z, x.s = x.s, B=100, n.quantiles = 10, func = "continuous")

saveRDS(mod_groupWQS_hf,file=paste0(dir_results,'mod_groupWQS_3groups_hf.rds'))



############################# 4. ARR ##############################
dat_arr_agg_zcta <- readRDS(paste0(dir_data_save,'dat_arr_agg_zcta.rds'))
dat_arr_agg_zcta <- as.data.frame(dat_arr_agg_zcta)
dat_arr_agg_zcta <- dat_arr_agg_zcta[sample(1:nrow(dat_arr_agg_zcta)),]
dat_arr_agg_zcta$ZCTA <- NULL
dat_arr_agg_zcta <- dat_arr_agg_zcta[!is.infinite(rowSums(dat_arr_agg_zcta)),]
dat_arr_agg_zcta <- dat_arr_agg_zcta[complete.cases(dat_arr_agg_zcta),]
dat_arr_agg_zcta$wmean_temporal <- -dat_arr_agg_zcta$wmean_temporal

# standardize all
dat_arr_agg_zcta$ec <- scale(dat_arr_agg_zcta$ec)
dat_arr_agg_zcta$nh4 <- scale(dat_arr_agg_zcta$nh4)
dat_arr_agg_zcta$no3 <- scale(dat_arr_agg_zcta$no3)
dat_arr_agg_zcta$oc <- scale(dat_arr_agg_zcta$oc)
dat_arr_agg_zcta$so4 <- scale(dat_arr_agg_zcta$so4)
dat_arr_agg_zcta$o3 <- scale(dat_arr_agg_zcta$o3)
dat_arr_agg_zcta$no2 <- scale(dat_arr_agg_zcta$no2)
dat_arr_agg_zcta$wmean_temporal <- scale(dat_arr_agg_zcta$wmean_temporal)
dat_arr_agg_zcta$wsd_temporal <- scale(dat_arr_agg_zcta$wsd_temporal)
dat_arr_agg_zcta$smean_temporal <- scale(dat_arr_agg_zcta$smean_temporal)
dat_arr_agg_zcta$ssd_temporal <- scale(dat_arr_agg_zcta$ssd_temporal)
dat_arr_agg_zcta$wmean_spatial <- scale(dat_arr_agg_zcta$wmean_spatial)
dat_arr_agg_zcta$wsd_spatial <- scale(dat_arr_agg_zcta$wsd_spatial)
dat_arr_agg_zcta$smean_spatial <- scale(dat_arr_agg_zcta$smean_spatial)
dat_arr_agg_zcta$ssd_spatial <- scale(dat_arr_agg_zcta$ssd_spatial)
dat_arr_agg_zcta$r_age_1 <- scale(dat_arr_agg_zcta$r_age_1)
dat_arr_agg_zcta$r_age_2 <- scale(dat_arr_agg_zcta$r_age_2)
dat_arr_agg_zcta$r_age_3 <- scale(dat_arr_agg_zcta$r_age_3)
dat_arr_agg_zcta$r_age_4 <- scale(dat_arr_agg_zcta$r_age_4)
dat_arr_agg_zcta$r_race_1 <- scale(dat_arr_agg_zcta$r_race_1)
dat_arr_agg_zcta$r_male <- scale(dat_arr_agg_zcta$r_male)
dat_arr_agg_zcta$pct_non_hispanic_white <- scale(dat_arr_agg_zcta$pct_non_hispanic_white)
dat_arr_agg_zcta$pct_black <- scale(dat_arr_agg_zcta$pct_black)
dat_arr_agg_zcta$pct_edu_some_college <- scale(dat_arr_agg_zcta$pct_edu_some_college)
dat_arr_agg_zcta$pct_poverty <- scale(dat_arr_agg_zcta$pct_poverty)
dat_arr_agg_zcta$pct_renting <- scale(dat_arr_agg_zcta$pct_renting)
dat_arr_agg_zcta$year <- scale(dat_arr_agg_zcta$year)

X <- make.X(dat_arr_agg_zcta, num.groups = 3, groups = group_list)
x.s <- make.x.s(dat_arr_agg_zcta, num.groups = 3, groups = group_list)
Y <- dat_arr_agg_zcta$r_tot
Z <- dat_arr_agg_zcta[,c('r_age_1','r_age_2','r_age_3','r_age_4','r_race_1','r_male','pct_non_hispanic_white','pct_black',
                         'pct_edu_some_college','pct_poverty','pct_renting','year',
                         'wmean_spatial','wsd_spatial','smean_spatial','ssd_spatial')]
Z <- as.matrix(Z)

mod_groupWQS_arr <- gwqs.fit(y = Y, x = X, z = Z, x.s = x.s, B=100, n.quantiles = 10, func = "continuous")

saveRDS(mod_groupWQS_arr,file=paste0(dir_results,'mod_groupWQS_3groups_arr.rds'))



############################# 5. Combined ##############################
dat_combined_agg_zcta <- readRDS(paste0(dir_data_save,'dat_combined_agg_zcta.rds'))
dat_combined_agg_zcta <- as.data.frame(dat_combined_agg_zcta)
dat_combined_agg_zcta <- dat_combined_agg_zcta[sample(1:nrow(dat_combined_agg_zcta)),]
dat_combined_agg_zcta$ZCTA <- NULL
dat_combined_agg_zcta <- dat_combined_agg_zcta[!is.infinite(rowSums(dat_combined_agg_zcta)),]
dat_combined_agg_zcta <- dat_combined_agg_zcta[complete.cases(dat_combined_agg_zcta),]
dat_combined_agg_zcta$wmean_temporal <- -dat_combined_agg_zcta$wmean_temporal

# standardize all
dat_combined_agg_zcta$ec <- scale(dat_combined_agg_zcta$ec)
dat_combined_agg_zcta$nh4 <- scale(dat_combined_agg_zcta$nh4)
dat_combined_agg_zcta$no3 <- scale(dat_combined_agg_zcta$no3)
dat_combined_agg_zcta$oc <- scale(dat_combined_agg_zcta$oc)
dat_combined_agg_zcta$so4 <- scale(dat_combined_agg_zcta$so4)
dat_combined_agg_zcta$o3 <- scale(dat_combined_agg_zcta$o3)
dat_combined_agg_zcta$no2 <- scale(dat_combined_agg_zcta$no2)
dat_combined_agg_zcta$wmean_temporal <- scale(dat_combined_agg_zcta$wmean_temporal)
dat_combined_agg_zcta$wsd_temporal <- scale(dat_combined_agg_zcta$wsd_temporal)
dat_combined_agg_zcta$smean_temporal <- scale(dat_combined_agg_zcta$smean_temporal)
dat_combined_agg_zcta$ssd_temporal <- scale(dat_combined_agg_zcta$ssd_temporal)
dat_combined_agg_zcta$wmean_spatial <- scale(dat_combined_agg_zcta$wmean_spatial)
dat_combined_agg_zcta$wsd_spatial <- scale(dat_combined_agg_zcta$wsd_spatial)
dat_combined_agg_zcta$smean_spatial <- scale(dat_combined_agg_zcta$smean_spatial)
dat_combined_agg_zcta$ssd_spatial <- scale(dat_combined_agg_zcta$ssd_spatial)
dat_combined_agg_zcta$r_age_1 <- scale(dat_combined_agg_zcta$r_age_1)
dat_combined_agg_zcta$r_age_2 <- scale(dat_combined_agg_zcta$r_age_2)
dat_combined_agg_zcta$r_age_3 <- scale(dat_combined_agg_zcta$r_age_3)
dat_combined_agg_zcta$r_age_4 <- scale(dat_combined_agg_zcta$r_age_4)
dat_combined_agg_zcta$r_race_1 <- scale(dat_combined_agg_zcta$r_race_1)
dat_combined_agg_zcta$r_male <- scale(dat_combined_agg_zcta$r_male)
dat_combined_agg_zcta$pct_non_hispanic_white <- scale(dat_combined_agg_zcta$pct_non_hispanic_white)
dat_combined_agg_zcta$pct_black <- scale(dat_combined_agg_zcta$pct_black)
dat_combined_agg_zcta$pct_edu_some_college <- scale(dat_combined_agg_zcta$pct_edu_some_college)
dat_combined_agg_zcta$pct_poverty <- scale(dat_combined_agg_zcta$pct_poverty)
dat_combined_agg_zcta$pct_renting <- scale(dat_combined_agg_zcta$pct_renting)
dat_combined_agg_zcta$year <- scale(dat_combined_agg_zcta$year)

X <- make.X(dat_combined_agg_zcta, num.groups = 3, groups = group_list)
x.s <- make.x.s(dat_combined_agg_zcta, num.groups = 3, groups = group_list)
Y <- dat_combined_agg_zcta$r_tot
Z <- dat_combined_agg_zcta[,c('r_age_1','r_age_2','r_age_3','r_age_4','r_race_1','r_male','pct_non_hispanic_white','pct_black',
                              'pct_edu_some_college','pct_poverty','pct_renting','year',
                              'wmean_spatial','wsd_spatial','smean_spatial','ssd_spatial')]
Z <- as.matrix(Z)

mod_groupWQS_combined <- gwqs.fit(y = Y, x = X, z = Z, x.s = x.s, B=100, n.quantiles = 10, func = "continuous")

saveRDS(mod_groupWQS_combined,file=paste0(dir_results,'mod_groupWQS_3groups_combined.rds'))

