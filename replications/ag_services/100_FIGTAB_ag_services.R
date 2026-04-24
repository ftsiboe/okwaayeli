rm(list = ls(all = TRUE)); gc()  
library('magrittr');library(ggplot2);library(gridExtra)
library(dplyr);library(gtable);library(stringr);library(cowplot)
devtools::document()  

project_name = "ag_services"
study_environment <- readRDS(
  file.path(paste0("replications/", project_name, "/output"),
            paste0(project_name,"_study_environment.rds")))

mspecs_optimal <- study_environment$match_specification_optimal

source("data-raw/scripts/figures_and_tables.R")

Keep.List<-c("Keep.List",ls())

# Main Specification   
rm(list= ls()[!(ls() %in% c(Keep.List))])
res_list <- list.files(study_environment$wd$estimations, pattern = "_hnormal_optimal.rds", full.names = TRUE)
res_list <- res_list[grepl(paste0("CropID_Pooled_services0_TL_hnormal_optimal.rds|",
                                 "CropID_Pooled_services1_TL_hnormal_optimal.rds|",
                                 "CropID_Pooled_services2_TL_hnormal_optimal.rds|",
                                 "CropID_Pooled_services3_TL_hnormal_optimal.rds"),res_list)]
res <- tab_main_specification(res_list=res_list,study_environment=study_environment)

wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results-msf.xlsx")))
openxlsx::writeData(wb, sheet = "msf",rbind(
  res[res$Survey %in% "GLSS0",]
) , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results-msf.xlsx")),overwrite = T)

# Fig - TREND 
rm(list= ls()[!(ls() %in% c(Keep.List))])
ef_mean <- readRDS(file.path(study_environment$wd$output,"estimations/CropID_Pooled_services0_TL_hnormal_optimal.rds"))$ef_mean
ef_mean <- ef_mean[ef_mean$stat %in% "wmean", ]
ef_mean <- ef_mean[ef_mean$estType %in% "teBC", ]
ef_mean$estm_type <- "ef_mean"
ef_mean$level_type <- gsub("efficiency", "", ef_mean$CoefName)
ef_mean$level_type <- ifelse(ef_mean$level_type %in% "", "level", ef_mean$level_type)
ef_mean$CoefName <- ef_mean$type
ef_mean <- ef_mean[c("technology_variable", "fxnforms", "distforms", "estm_type", "level_type", "sample", "Survey", "restrict", "Tech", "CoefName", "Estimate", "Estimate.sd", "jack_pv")]
ef_mean <- ef_mean[ef_mean$restrict %in% "Restricted", ]
ef_mean <- ef_mean[ef_mean$sample %in% ifelse(mspecs_optimal$link %in% NA,mspecs_optimal$distance,mspecs_optimal$link),]
ef_mean <- ef_mean[ef_mean$level_type %in% "Gap_lvl", ]
ef_mean <- ef_mean[!ef_mean$CoefName %in% "TE0", ]
ef_mean <- ef_mean[!ef_mean$Survey %in% "GLSS0", ]

ef_mean$type <- as.numeric(as.character(factor(ef_mean$CoefName, levels = c("TGR", "TE","MTE"), labels = 1:3)))
ef_mean$type <- factor(ef_mean$type, levels = 1:3,
                       labels = c("Technology gap ratio", "Technical efficiency", "Meta-technical-efficiency"))

ef_mean$Survey <- factor(ef_mean$Survey, levels = c("GLSS3","GLSS4","GLSS5","GLSS6","GLSS7"),
                         labels = c("1991/1992\n(GLSS 3)","1998/1999\n(GLSS4)","2005/2006\n(GLSS5)",
                                    "2012/2013\n(GLSS6)","2016/2017\n(GLSS7)"))

fig <- ggplot(
  data = ef_mean,
  aes(x = Survey, y = Estimate*100, group = type, fill = type, color = type, shape = type)) +
  geom_hline(yintercept = 0, size = 0.5, color = "black") +  # Add a horizontal line at y = 0
  geom_point(size=2) +
  geom_errorbar(aes(ymax = (Estimate + Estimate.sd)*100, ymin = (Estimate - Estimate.sd)*100), width = 0.10) +
  geom_line() +
  scale_fill_manual("", values = c("orange", "darkgreen", "blue")) +
  scale_color_manual("", values = c("orange", "darkgreen", "blue")) +
  scale_shape_manual("", values = c(21, 25, 24, 22, 23, 3, 4, 8, 11)) +
  scale_y_continuous(breaks = seq(-100, 100, by = 5)) +
  labs(title = "", x = "", y = "Level difference [Service provided less not provided]\n", caption = "") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "bottom") +
  theme(legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.text.x = element_text(size = 8, colour = "black"),
        axis.text.y = element_text(size = 6, colour = "black"),
        plot.caption = element_text(size = 11, hjust = 0, vjust = 0, face = "italic"),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "white", colour = "black", size = 1))
ggsave(file.path(study_environment$wd$output,"figure/score_trend.png"), fig,dpi = 600,width = 6, height = 6)


# Agricultural Service  
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <- readRDS(file.path(study_environment$wd$estimations,"CropID_Pooled_services0_TL_hnormal_optimal.rds"))$disagscors
res$disasg <- as.character(res$disagscors_var)
res$level  <- as.character(res$disagscors_level)
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
res <- res[c("disasg","level","fxnforms","distforms","Survey","input","technology_variable","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]

res <- res[grepl("services_",res$disasg),]
res <- res[(res$level %in% "1"),]
res <- res[order(res$disasg),]

res$level <- ifelse(res$disasg %in% "services_planting", "Planting", res$level)
res$level <- ifelse(res$disasg %in% "services_agchemicals", "Use of agro\nchemicals", res$level)
res$level <- ifelse(res$disasg %in% "services_mechanization", "Mechanization", res$level)
res$level <- ifelse(res$disasg %in% "services_post_harvest", "Post harvest\ncand marketing", res$level)

res$input <- factor(res$input, levels = c("TGR","TE","MTE"), labels = c("Technology gap ratio", "Technical efficiency", "Meta-technical-efficiency"))

dodge <- position_dodge(width = 0.75)

# Create ordered factor once
res$level_ord <- with(res, reorder(level, Estimate, FUN = mean))

# Alternating shaded bands
band_df <- data.frame(
  xmin = seq_along(levels(res$level_ord)) - 0.5,
  xmax = seq_along(levels(res$level_ord)) + 0.5
)

band_df <- band_df[seq(1, nrow(band_df), by = 2), ]

fig <- ggplot(
  data = res,
  aes(
    x = level_ord,
    y = Estimate,
    group = input,
    fill = input,
    colour = input
  )
) +
  geom_rect(
    data = band_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    inherit.aes = FALSE,
    fill = "#9DD9F7",
    alpha = 0.6,
    colour = NA
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, colour = "black") +
  geom_col(
    position = dodge,
    width = 0.65,
    alpha = 0.8
  ) +
  geom_errorbar(
    position = dodge,
    aes(
      ymax = Estimate + Estimate.sd,
      ymin = Estimate - Estimate.sd
    ),
    width = 0.25
  ) +
  geom_text(
    aes(
      label = sprintf("%.2f", Estimate),
      hjust = ifelse(Estimate >= 0, -0.50, 1.50)
    ),
    position = dodge,
    size = 3
  ) +
  labs(
    title = "",
    x = "Agricultural Service\n",
    y = "\nLevel difference [Service provided less not provided]",
    caption = ""
  ) +
  scale_fill_manual(name = "", values = c("#BE5E27", "#FFC425", "#00583D")) +
  scale_color_manual(name = "", values = c("#BE5E27", "#FFC425", "#00583D")) +
  ers_theme() +
  scale_y_continuous(
    expand = expansion(mult = c(0.15, 0.15))
  ) +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.title.x = element_text(size = 10, color = "black"),
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.background = element_rect(fill = "white", colour = "black", linewidth = 1)
  ) +
  coord_flip()

ggsave(file.path(study_environment$wd$output,"figure/score_by_services.png"), fig,dpi = 600,width = 7, height = 5)

# Fig - Heterogeneity          
# rm(list= ls()[!(ls() %in% c(Keep.List))])
# res <- readRDS(file.path(study_environment$wd$estimations,"CropID_Pooled_services0_TL_hnormal_optimal.rds"))$disagscors
# res$disasg <- as.character(res$disagscors_var)
# res$level  <- as.character(res$disagscors_level)
# res <- res[res$estType %in% "teBC",]
# res <- res[res$Survey %in% "GLSS0",]
# res <- res[res$restrict %in% "Restricted",]
# res <- res[res$stat %in% "mean",]
# res <- res[!res$sample %in% "unmatched",]
# res <- res[res$CoefName %in% "disag_efficiencyGap_lvl",]
# res <- res[c("disasg","level","fxnforms","distforms","Survey","input","technology_variable","Tech","CoefName","Estimate","Estimate.sd","jack_pv")]
# 
# fig <- fig_heterogeneity00(res=res,y_title="Level difference [Any services less No services]\n",study_environment=study_environment)
# fig[["genderAge"]] <- fig[["genderAge"]] + theme(axis.text.x = element_text(size = 5.5))
# ggsave(file.path(study_environment$wd$output,"figure","heterogeneity_crop_region.png"), fig[["crop_region"]],dpi = 600,width = 8, height = 5)
# ggsave(file.path(study_environment$wd$output,"figure","heterogeneity_genderAge.png"), fig[["genderAge"]],dpi = 600,width = 8, height = 5)


# Fig - Robustness         
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_robustness(y_title="\nLevel difference [Any services less No services]",
               res_list = c(file.path(study_environment$wd$output,"estimations","CropID_Pooled_services0_TL_hnormal_optimal.rds"),
                            list.files(file.path(study_environment$wd$output,"estimations"),
                                       pattern = "CropID_Pooled_services0_TL_",full.names = T)),
               study_environment=study_environment)

# Fig - Matching TE      
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_input_te(
  y_title="\nGap associated with agricultural services (%)",
  tech_lable=c("Full\nsample", "Any services\nsample", "No services\nsample"),
  study_environment=study_environment)

# Fig - Covariate balance 
rm(list= ls()[!(ls() %in% c(Keep.List))])
fig_covariate_balance(study_environment=study_environment)

bal_tab <- study_environment$balance_table
ranking <- study_environment$match_specification_ranking
mspecs  <- study_environment$match_specification_optimal
CovBalDATA <- rbind(bal_tab[(bal_tab$sample %in% "Un" & bal_tab$ARRAY %in% 5), ],
                    bal_tab[bal_tab$sample %in% "Adj", ])
CovBalDATA$sample <- ifelse(CovBalDATA$sample %in% "Un", CovBalDATA$sample,
                            ifelse(CovBalDATA$link %in% NA, CovBalDATA$distance, CovBalDATA$link))
CovBalDATA <- CovBalDATA[!CovBalDATA$value %in% NA, ]
CovBalDATA <- CovBalDATA[!CovBalDATA$Coef %in% NA, ]
CovBalDATA <- CovBalDATA[c("sample","stat","Coef","value")]

wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results-msf.xlsx")))
openxlsx::writeData(wb, sheet = "CovBalDATA",CovBalDATA , colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results-msf.xlsx")),overwrite = T)

wb <- openxlsx::loadWorkbook(file.path(study_environment$wd$output,paste0(project_name,"_results-msf.xlsx")))
openxlsx::writeData(wb, sheet = "ranking",ranking[c("ID","name","Diff.mean",	"V_Ratio.mean",	"KS.mean","rate.mean")] , 
                    colNames = T, startCol = "A", startRow = 1)
openxlsx::saveWorkbook(wb,file.path(study_environment$wd$output,paste0(project_name,"_results-msf.xlsx")),overwrite = T)


# Fig - Distribution 
rm(list= ls()[!(ls() %in% c(Keep.List))])
dataFrq <- readRDS(file.path(study_environment$wd$output,"estimations/CropID_Pooled_services0_TL_hnormal_fullset.rds"))
dataFrq <- dataFrq$ef_dist
dataFrq <- dataFrq[dataFrq$estType %in% "teBC",]
dataFrq <- dataFrq[dataFrq$Survey %in% "GLSS0",]
dataFrq <- dataFrq[dataFrq$stat %in% "estimate_weight",]
dataFrq <- dataFrq[dataFrq$restrict %in% "Restricted",]
dataFrq$Tech <- factor(as.numeric(as.character(dataFrq$TCHLvel)),levels = 0:1,labels = c("No services","Some services"))
fig_dsistribution(dataFrq,study_environment=study_environment)









# Fig - Region and crop ranking text
rm(list= ls()[!(ls() %in% c(Keep.List))])
res <-readRDS(file.path(study_environment$wd$output,"estimations/CropID_Pooled_extraction_any_TL_hnormal_optimal.rds"))$disagscors
res$disasg <- res$disagscors_var
res$level <- res$disagscors_level
res <- res[res$estType %in% "teBC",]
res <- res[res$Survey %in% "GLSS0",]
res <- res[res$restrict %in% "Restricted",]
res <- res[res$stat %in% "mean",]
res <- res[!res$sample %in% "unmatched",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$CoefName %in% "disag_efficiencyGap_pct",]
res <- res[res$input %in% "MTE",]

reg <- res[res$disagscors_var %in% "Region",]
reg <- reg[order(reg$Estimate),]
paste0(paste0(reg$level," (",round(reg$Estimate,2),"%)"),collapse = ", ")

CROP <- res[res$disagscors_var %in% "CROP",]
CROP <- CROP[order(CROP$Estimate),]
paste0(paste0(CROP$level," (",round(CROP$Estimate,2),"%)"),collapse = ", ")




