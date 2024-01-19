###############################################################################
# Project: SID mixtures                                                       #
# Code: Compile results: 3 groups                                             #
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



############################# 1. Fig 1: Cumulative effects for 3 groups ##############################
mod_ihd <- readRDS(paste0(dir_results,'mod_groupWQS_3groups_ihd.rds'))
results_ihd <- data.frame(summary(mod_ihd$fit)$coefficients[2:4,1:2])
names(results_ihd) <- c('est','se')
results_ihd$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')
results_ihd$subtype <- 'Ischemic heart disease'
rm(mod_ihd)
gc()

mod_cd <- readRDS(paste0(dir_results,'mod_groupWQS_3groups_cd.rds'))
results_cd <- data.frame(summary(mod_cd$fit)$coefficients[2:4,1:2])
names(results_cd) <- c('est','se')
results_cd$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')
results_cd$subtype <- 'Cerebrovascular disease'
rm(mod_cd)
gc()

mod_hf <- readRDS(paste0(dir_results,'mod_groupWQS_3groups_hf.rds'))
results_hf <- data.frame(summary(mod_hf$fit)$coefficients[2:4,1:2])
names(results_hf) <- c('est','se')
results_hf$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')
results_hf$subtype <- 'Heart failure'
rm(mod_hf)
gc()

mod_arr <- readRDS(paste0(dir_results,'mod_groupWQS_3groups_arr.rds'))
results_arr <- data.frame(summary(mod_arr$fit)$coefficients[2:4,1:2])
names(results_arr) <- c('est','se')
results_arr$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')
results_arr$subtype <- 'Arrhythmia'
rm(mod_arr)
gc()

results <- rbind(results_ihd,results_cd,results_hf,results_arr)
results$est_exp <- (exp(results$est)-1)*100
results$est_exp_upper <- (exp(results$est+qnorm(1-0.25/4)*results$se)-1)*100
results$est_exp_lower <- (exp(results$est-qnorm(1-0.25/4)*results$se)-1)*100

results$group <- factor(results$group,levels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies'))

p_ihd <- results %>% filter(subtype=='Ischemic heart disease') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')) +
  ggtitle(expression(paste('A. Ischemic heart disease')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

p_cd <- results %>% filter(subtype=='Cerebrovascular disease') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')) +
  ggtitle(expression(paste('B. Cerebrovascular disease')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

p_hf <- results %>% filter(subtype=='Heart failure') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')) +
  ggtitle(expression(paste('C. Heart failure')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

p_arr <- results %>% filter(subtype=='Arrhythmia') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies')) +
  ggtitle(expression(paste('D. Arrhythmia')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

fig_1 <- cowplot::plot_grid(p_ihd,p_cd,p_hf,p_arr,nrow=1)
ggsave(file=paste0(dir_results,'fig_1_cumulative_3groups.pdf'),fig_1,width=12,height=4,units="in",dpi=300,device='pdf')
ggsave(file=paste0(dir_results,'fig_1_cumulative_3groups.jpg'),fig_1,width=12,height=4,units="in",dpi=300,device='jpg')
dev.off()



############################# 2. Fig 2: Cumulative effects for 4 groups ##############################
mod_ihd <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_ihd.rds'))
results_ihd <- data.frame(summary(mod_ihd$fit)$coefficients[2:5,1:2])
names(results_ihd) <- c('est','se')
results_ihd$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies','Interactions')
results_ihd$subtype <- 'Ischemic heart disease'
rm(mod_ihd)
gc()

mod_cd <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_cd.rds'))
results_cd <- data.frame(summary(mod_cd$fit)$coefficients[2:5,1:2])
names(results_cd) <- c('est','se')
results_cd$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies','Interactions')
results_cd$subtype <- 'Cerebrovascular disease'
rm(mod_cd)
gc()

mod_hf <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_hf.rds'))
results_hf <- data.frame(summary(mod_hf$fit)$coefficients[2:5,1:2])
names(results_hf) <- c('est','se')
results_hf$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies','Interactions')
results_hf$subtype <- 'Heart failure'
rm(mod_hf)
gc()

mod_arr <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_arr.rds'))
results_arr <- data.frame(summary(mod_arr$fit)$coefficients[2:5,1:2])
names(results_arr) <- c('est','se')
results_arr$group <- c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies','Interactions')
results_arr$subtype <- 'Arrhythmia'
rm(mod_arr)
gc()

results <- rbind(results_ihd,results_cd,results_hf,results_arr)
results$est_exp <- (exp(results$est)-1)*100
results$est_exp_upper <- (exp(results$est+qnorm(1-0.25/4)*results$se)-1)*100
results$est_exp_lower <- (exp(results$est-qnorm(1-0.25/4)*results$se)-1)*100

results$group <- factor(results$group,levels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies','Interactions'))

p_ihd <- results %>% filter(subtype=='Ischemic heart disease') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                              'Interactions between air pollutants\nand temperature mean anomalies')) +
  ggtitle(expression(paste('A. Ischemic heart disease')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

p_cd <- results %>% filter(subtype=='Cerebrovascular disease') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                              'Interactions between air pollutants\nand temperature mean anomalies')) +
  ggtitle(expression(paste('B. Cerebrovascular disease')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

p_hf <- results %>% filter(subtype=='Heart failure') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                              'Interactions between air pollutants\nand temperature mean anomalies')) +
  ggtitle(expression(paste('C. Heart failure')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

p_arr <- results %>% filter(subtype=='Arrhythmia') %>%
  ggplot(aes(group,est_exp))+
  geom_point(aes(colour = group),size=2)+
  geom_errorbar(aes(x = group,ymin = est_exp_lower, ymax = est_exp_upper,colour = group),width=0.2)+
  geom_hline(aes(yintercept = 0), linetype="dashed")+
  xlab('') +
  ylab("Percent change (%)")+
  ylim(-0.008,0.013)+
  scale_x_discrete(labels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                              'Interactions between air pollutants\nand temperature mean anomalies')) +
  ggtitle(expression(paste('D. Arrhythmia')))+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA),
        legend.title=element_blank(),
        strip.background = element_rect(fill="white"),
        axis.text.x = element_text(angle = 45, hjust=1))+
  guides(colour = FALSE, fill = FALSE)

fig_2 <- cowplot::plot_grid(p_ihd,p_cd,p_hf,p_arr,nrow=1)
ggsave(file=paste0(dir_results,'fig_2_cumulative_4groups.pdf'),fig_2,width=12,height=4,units="in",dpi=300,device='pdf')
ggsave(file=paste0(dir_results,'fig_2_cumulative_4groups.jpg'),fig_2,width=12,height=4,units="in",dpi=300,device='jpg')
dev.off()



############################# 3. Fig 3: Weights for IHD ##############################
mod_ihd <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_ihd.rds'))

weight_pollution <- data.frame(sort(mod_ihd[[3]][[1]]))
names(weight_pollution) <- 'weight'
weight_pollution$exp <- row.names(weight_pollution)
weight_pollution$group <- 'Air pollutants'

weight_temp_mean <- data.frame(sort(mod_ihd[[3]][[2]]))
names(weight_temp_mean) <- 'weight'
weight_temp_mean$exp <- row.names(weight_temp_mean)
weight_temp_mean$group <- 'Temperature mean anomalies'

weight_temp_sd <- data.frame(sort(mod_ihd[[3]][[3]]))
names(weight_temp_sd) <- 'weight'
weight_temp_sd$exp <- row.names(weight_temp_sd)
weight_temp_sd$group <- 'Temperature SD anomalies'

weight_interaction <- data.frame(sort(mod_ihd[[3]][[4]]))
names(weight_interaction) <- 'weight'
weight_interaction$exp <- row.names(weight_interaction)
weight_interaction$group <- 'Interactions between air pollutants and temperature mean anomalies'

weight_all <- rbind(weight_pollution,weight_temp_mean,weight_temp_sd,weight_interaction)
weight_all$group <- factor(weight_all$group,levels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                                                       'Interactions between air pollutants and temperature mean anomalies'))

weight_all$label[weight_all$exp=='o3'] <- "O[3]"
weight_all$label[weight_all$exp=='oc'] <- "OC"
weight_all$label[weight_all$exp=='no3'] <- "NO[3]^{'-'}"
weight_all$label[weight_all$exp=='ec'] <- "EC"
weight_all$label[weight_all$exp=='no2'] <- "NO[2]"
weight_all$label[weight_all$exp=='nh4'] <- "NH[4]^{'+'}"
weight_all$label[weight_all$exp=='so4'] <- "SO[4]^{'2-'}"
weight_all$label[weight_all$exp=='smean_temporal'] <- "Summer ~ mean"
weight_all$label[weight_all$exp=='wmean_temporal'] <- "Minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ssd_temporal'] <- "Summer ~ SD"
weight_all$label[weight_all$exp=='wsd_temporal'] <- "Winter ~ SD"
weight_all$label[weight_all$exp=='o3_wmean_temporal'] <- "O[3]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_wmean_temporal'] <- "OC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ec_wmean_temporal'] <- "EC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_smean_temporal'] <- "NO[2]%*%summer ~ mean"
weight_all$label[weight_all$exp=='no3_smean_temporal'] <- "NO[3]^{'-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='ec_smean_temporal'] <- "EC%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_wmean_temporal'] <- "SO[4]^{'2-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='nh4_smean_temporal'] <- "NH[4]^{'+'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_smean_temporal'] <- "SO[4]^{'2-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='nh4_wmean_temporal'] <- "NH[4]^{'+'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_wmean_temporal'] <- "NO[2]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no3_wmean_temporal'] <- "NO[3]^{'-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_smean_temporal'] <- "OC%*%summer ~ mean"
weight_all$label[weight_all$exp=='o3_smean_temporal'] <- "O[3]%*%summer ~ mean"

weight_all <- weight_all[order(weight_all$weight),]
weight_all$exp <- factor(weight_all$exp,levels = weight_all$exp)

fig_3_weight_ihd <- ggplot(weight_all, aes(weight,exp)) +
  geom_point() +
  facet_grid(group ~ ., scales = "free", space = "free",labeller = label_wrap_gen(multi_line = TRUE)) +
  xlab('Weight') +
  ggtitle('Ischemic heart disease')+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.title=element_blank()) + 
  ylab('')

for (f in unique(weight_all$label)) {
  facet_labels <- subset(weight_all, group == f)
  fig_3_weight_ihd <- fig_3_weight_ihd + scale_y_discrete(breaks = weight_all$exp, labels = parse(text = weight_all$label))
}

fig_3_weight_ihd

ggsave(file=paste0(dir_results,'fig_3_weight_ihd.pdf'),fig_3_weight_ihd,width=6,height=6,units="in",dpi=300,device='pdf')
dev.off()



############################# 4. Fig 4: Weights for CD ##############################
mod_cd <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_cd.rds'))

weight_pollution <- data.frame(sort(mod_cd[[3]][[1]]))
names(weight_pollution) <- 'weight'
weight_pollution$exp <- row.names(weight_pollution)
weight_pollution$group <- 'Air pollutants'

weight_temp_mean <- data.frame(sort(mod_cd[[3]][[2]]))
names(weight_temp_mean) <- 'weight'
weight_temp_mean$exp <- row.names(weight_temp_mean)
weight_temp_mean$group <- 'Temperature mean anomalies'

weight_temp_sd <- data.frame(sort(mod_cd[[3]][[3]]))
names(weight_temp_sd) <- 'weight'
weight_temp_sd$exp <- row.names(weight_temp_sd)
weight_temp_sd$group <- 'Temperature SD anomalies'

weight_interaction <- data.frame(sort(mod_cd[[3]][[4]]))
names(weight_interaction) <- 'weight'
weight_interaction$exp <- row.names(weight_interaction)
weight_interaction$group <- 'Interactions between air pollutants and temperature mean anomalies'

weight_all <- rbind(weight_pollution,weight_temp_mean,weight_temp_sd,weight_interaction)
weight_all$group <- factor(weight_all$group,levels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                                                       'Interactions between air pollutants and temperature mean anomalies'))

weight_all$label[weight_all$exp=='o3'] <- "O[3]"
weight_all$label[weight_all$exp=='oc'] <- "OC"
weight_all$label[weight_all$exp=='no3'] <- "NO[3]^{'-'}"
weight_all$label[weight_all$exp=='ec'] <- "EC"
weight_all$label[weight_all$exp=='no2'] <- "NO[2]"
weight_all$label[weight_all$exp=='nh4'] <- "NH[4]^{'+'}"
weight_all$label[weight_all$exp=='so4'] <- "SO[4]^{'2-'}"
weight_all$label[weight_all$exp=='smean_temporal'] <- "Summer ~ mean"
weight_all$label[weight_all$exp=='wmean_temporal'] <- "Minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ssd_temporal'] <- "Summer ~ SD"
weight_all$label[weight_all$exp=='wsd_temporal'] <- "Winter ~ SD"
weight_all$label[weight_all$exp=='o3_wmean_temporal'] <- "O[3]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_wmean_temporal'] <- "OC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ec_wmean_temporal'] <- "EC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_smean_temporal'] <- "NO[2]%*%summer ~ mean"
weight_all$label[weight_all$exp=='no3_smean_temporal'] <- "NO[3]^{'-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='ec_smean_temporal'] <- "EC%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_wmean_temporal'] <- "SO[4]^{'2-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='nh4_smean_temporal'] <- "NH[4]^{'+'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_smean_temporal'] <- "SO[4]^{'2-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='nh4_wmean_temporal'] <- "NH[4]^{'+'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_wmean_temporal'] <- "NO[2]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no3_wmean_temporal'] <- "NO[3]^{'-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_smean_temporal'] <- "OC%*%summer ~ mean"
weight_all$label[weight_all$exp=='o3_smean_temporal'] <- "O[3]%*%summer ~ mean"

weight_all <- weight_all[order(weight_all$weight),]
weight_all$exp <- factor(weight_all$exp,levels = weight_all$exp)

fig_4_weight_cd <- ggplot(weight_all, aes(weight,exp)) +
  geom_point() +
  facet_grid(group ~ ., scales = "free", space = "free",labeller = label_wrap_gen(multi_line = TRUE)) +
  xlab('Weight') +
  ggtitle('Cerebrovascular disease')+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.title=element_blank()) + 
  ylab('')

for (f in unique(weight_all$label)) {
  facet_labels <- subset(weight_all, group == f)
  fig_4_weight_cd <- fig_4_weight_cd + scale_y_discrete(breaks = weight_all$exp, labels = parse(text = weight_all$label))
}

fig_4_weight_cd

ggsave(file=paste0(dir_results,'fig_4_weight_cd.pdf'),fig_4_weight_cd,width=6,height=6,units="in",dpi=300,device='pdf')
dev.off()



############################# 5. Fig 5: Weights for HF ##############################
mod_hf <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_hf.rds'))

weight_pollution <- data.frame(sort(mod_hf[[3]][[1]]))
names(weight_pollution) <- 'weight'
weight_pollution$exp <- row.names(weight_pollution)
weight_pollution$group <- 'Air pollutants'

weight_temp_mean <- data.frame(sort(mod_hf[[3]][[2]]))
names(weight_temp_mean) <- 'weight'
weight_temp_mean$exp <- row.names(weight_temp_mean)
weight_temp_mean$group <- 'Temperature mean anomalies'

weight_temp_sd <- data.frame(sort(mod_hf[[3]][[3]]))
names(weight_temp_sd) <- 'weight'
weight_temp_sd$exp <- row.names(weight_temp_sd)
weight_temp_sd$group <- 'Temperature SD anomalies'

weight_interaction <- data.frame(sort(mod_hf[[3]][[4]]))
names(weight_interaction) <- 'weight'
weight_interaction$exp <- row.names(weight_interaction)
weight_interaction$group <- 'Interactions between air pollutants and temperature mean anomalies'

weight_all <- rbind(weight_pollution,weight_temp_mean,weight_temp_sd,weight_interaction)
weight_all$group <- factor(weight_all$group,levels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                                                       'Interactions between air pollutants and temperature mean anomalies'))

weight_all$label[weight_all$exp=='o3'] <- "O[3]"
weight_all$label[weight_all$exp=='oc'] <- "OC"
weight_all$label[weight_all$exp=='no3'] <- "NO[3]^{'-'}"
weight_all$label[weight_all$exp=='ec'] <- "EC"
weight_all$label[weight_all$exp=='no2'] <- "NO[2]"
weight_all$label[weight_all$exp=='nh4'] <- "NH[4]^{'+'}"
weight_all$label[weight_all$exp=='so4'] <- "SO[4]^{'2-'}"
weight_all$label[weight_all$exp=='smean_temporal'] <- "Summer ~ mean"
weight_all$label[weight_all$exp=='wmean_temporal'] <- "Minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ssd_temporal'] <- "Summer ~ SD"
weight_all$label[weight_all$exp=='wsd_temporal'] <- "Winter ~ SD"
weight_all$label[weight_all$exp=='o3_wmean_temporal'] <- "O[3]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_wmean_temporal'] <- "OC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ec_wmean_temporal'] <- "EC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_smean_temporal'] <- "NO[2]%*%summer ~ mean"
weight_all$label[weight_all$exp=='no3_smean_temporal'] <- "NO[3]^{'-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='ec_smean_temporal'] <- "EC%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_wmean_temporal'] <- "SO[4]^{'2-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='nh4_smean_temporal'] <- "NH[4]^{'+'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_smean_temporal'] <- "SO[4]^{'2-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='nh4_wmean_temporal'] <- "NH[4]^{'+'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_wmean_temporal'] <- "NO[2]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no3_wmean_temporal'] <- "NO[3]^{'-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_smean_temporal'] <- "OC%*%summer ~ mean"
weight_all$label[weight_all$exp=='o3_smean_temporal'] <- "O[3]%*%summer ~ mean"

weight_all <- weight_all[order(weight_all$weight),]
weight_all$exp <- factor(weight_all$exp,levels = weight_all$exp)

fig_5_weight_hf <- ggplot(weight_all, aes(weight,exp)) +
  geom_point() +
  facet_grid(group ~ ., scales = "free", space = "free",labeller = label_wrap_gen(multi_line = TRUE)) +
  xlab('Weight') +
  ggtitle('Heart failure')+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.title=element_blank()) + 
  ylab('')

for (f in unique(weight_all$label)) {
  facet_labels <- subset(weight_all, group == f)
  fig_5_weight_hf <- fig_5_weight_hf + scale_y_discrete(breaks = weight_all$exp, labels = parse(text = weight_all$label))
}

fig_5_weight_hf

ggsave(file=paste0(dir_results,'fig_5_weight_hf.pdf'),fig_5_weight_hf,width=6,height=6,units="in",dpi=300,device='pdf')
dev.off()



############################# 6. Fig 6: Weights for ARR ##############################
mod_arr <- readRDS(paste0(dir_results,'mod_groupWQS_4groups_arr.rds'))

weight_pollution <- data.frame(sort(mod_arr[[3]][[1]]))
names(weight_pollution) <- 'weight'
weight_pollution$exp <- row.names(weight_pollution)
weight_pollution$group <- 'Air pollutants'

weight_temp_mean <- data.frame(sort(mod_arr[[3]][[2]]))
names(weight_temp_mean) <- 'weight'
weight_temp_mean$exp <- row.names(weight_temp_mean)
weight_temp_mean$group <- 'Temperature mean anomalies'

weight_temp_sd <- data.frame(sort(mod_arr[[3]][[3]]))
names(weight_temp_sd) <- 'weight'
weight_temp_sd$exp <- row.names(weight_temp_sd)
weight_temp_sd$group <- 'Temperature SD anomalies'

weight_interaction <- data.frame(sort(mod_arr[[3]][[4]]))
names(weight_interaction) <- 'weight'
weight_interaction$exp <- row.names(weight_interaction)
weight_interaction$group <- 'Interactions between air pollutants and temperature mean anomalies'

weight_all <- rbind(weight_pollution,weight_temp_mean,weight_temp_sd,weight_interaction)
weight_all$group <- factor(weight_all$group,levels = c('Air pollutants','Temperature mean anomalies','Temperature SD anomalies',
                                                       'Interactions between air pollutants and temperature mean anomalies'))

weight_all$label[weight_all$exp=='o3'] <- "O[3]"
weight_all$label[weight_all$exp=='oc'] <- "OC"
weight_all$label[weight_all$exp=='no3'] <- "NO[3]^{'-'}"
weight_all$label[weight_all$exp=='ec'] <- "EC"
weight_all$label[weight_all$exp=='no2'] <- "NO[2]"
weight_all$label[weight_all$exp=='nh4'] <- "NH[4]^{'+'}"
weight_all$label[weight_all$exp=='so4'] <- "SO[4]^{'2-'}"
weight_all$label[weight_all$exp=='smean_temporal'] <- "Summer ~ mean"
weight_all$label[weight_all$exp=='wmean_temporal'] <- "Minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ssd_temporal'] <- "Summer ~ SD"
weight_all$label[weight_all$exp=='wsd_temporal'] <- "Winter ~ SD"
weight_all$label[weight_all$exp=='o3_wmean_temporal'] <- "O[3]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_wmean_temporal'] <- "OC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='ec_wmean_temporal'] <- "EC%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_smean_temporal'] <- "NO[2]%*%summer ~ mean"
weight_all$label[weight_all$exp=='no3_smean_temporal'] <- "NO[3]^{'-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='ec_smean_temporal'] <- "EC%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_wmean_temporal'] <- "SO[4]^{'2-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='nh4_smean_temporal'] <- "NH[4]^{'+'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='so4_smean_temporal'] <- "SO[4]^{'2-'}%*%summer ~ mean"
weight_all$label[weight_all$exp=='nh4_wmean_temporal'] <- "NH[4]^{'+'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no2_wmean_temporal'] <- "NO[2]%*%minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='no3_wmean_temporal'] <- "NO[3]^{'-'}%*% minus ~ winter ~ mean"
weight_all$label[weight_all$exp=='oc_smean_temporal'] <- "OC%*%summer ~ mean"
weight_all$label[weight_all$exp=='o3_smean_temporal'] <- "O[3]%*%summer ~ mean"

weight_all <- weight_all[order(weight_all$weight),]
weight_all$exp <- factor(weight_all$exp,levels = weight_all$exp)

fig_6_weight_arr <- ggplot(weight_all, aes(weight,exp)) +
  geom_point() +
  facet_grid(group ~ ., scales = "free", space = "free",labeller = label_wrap_gen(multi_line = TRUE)) +
  xlab('Weight') +
  ggtitle('Arrhythmia')+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0),
        panel.grid.minor = element_blank(),
        legend.title=element_blank()) + 
  ylab('')

for (f in unique(weight_all$label)) {
  facet_labels <- subset(weight_all, group == f)
  fig_6_weight_arr <- fig_6_weight_arr + scale_y_discrete(breaks = weight_all$exp, labels = parse(text = weight_all$label))
}

fig_6_weight_arr

ggsave(file=paste0(dir_results,'fig_6_weight_arr.pdf'),fig_6_weight_arr,width=6,height=6,units="in",dpi=300,device='pdf')
dev.off()



############################# 7. Compute number of increased HF cases ##############################
dat_hf_agg_zcta <- readRDS(paste0(dir_data_save,'dat_hf_agg_zcta.rds'))
dat_hf_agg_zcta <- as.data.frame(dat_hf_agg_zcta)
dat_hf_agg_zcta <- dat_hf_agg_zcta[complete.cases(dat_hf_agg_zcta),]

mod_hf <- readRDS(paste0(dir_results,'mod_groupWQS_3groups_hf.rds'))
results_hf <- data.frame(summary(mod_hf$fit)$coefficients[2:4,1:2])
names(results_hf) <- c('est','se')
results_hf$est_exp <- (exp(results_hf$est)-1)*100

mean(dat_hf_agg_zcta$tot)*length(unique(dat_hf_agg_zcta$ZCTA))*results_hf$est_exp[1]/100
# [1] 12632.51