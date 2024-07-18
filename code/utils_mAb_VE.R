
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(cowplot)

###############################
# some processing of dataset
###############################

process_df <- function(df){
  
  # add/recode some variables 
  
  # month when tested and age when tested 
  df <- df %>% 
    mutate(month_when_tested = as.character(month(collection_date, label = T))) %>% 
    mutate(month_when_tested = ifelse(month_when_tested == "Sep", "Oct", month_when_tested)) %>%
    # age when tested (6m interval)
    mutate(age_at_test_in_months_cat = case_when(
      age_at_test_in_months < 6 ~ "under 6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 12 ~ "6-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    # age when tested (3m interval)
    mutate(age_at_test_in_months_cat_2 = case_when(
      age_at_test_in_months < 3 ~ "under 3m",
      age_at_test_in_months >= 3 & age_at_test_in_months < 6 ~ "3-6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 9 ~ "6-9m",
      age_at_test_in_months >= 9 & age_at_test_in_months < 12 ~ "9-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    mutate(risk_factor_atleastone = case_when(risk_factor_atleastone == 1 ~ "yes",
                                              risk_factor_atleastone == 0 ~ "no")) %>% 
    mutate(across(c(risk_factor_anemia, 
                    risk_factor_pulmonary,
                    risk_factor_cardiac,
                    risk_factor_immunodeficiency,
                    risk_factor_down,
                    #risk_factor_small_for_gestage # this has values of 1 0 NA
    ), ~ ifelse(is.na(.), "no", "yes"))) %>%
    mutate(age_at_test_in_months_cat = factor(age_at_test_in_months_cat,
                                              levels = c("under 6m",
                                                         "6-12m",
                                                         "above 1 yo"))) %>%
    mutate(age_at_test_in_months_cat_2 = factor(age_at_test_in_months_cat_2,
                                                levels = c("under 3m",
                                                           "3-6m",
                                                           "6-9m",
                                                           "9-12m",
                                                           "above 1 yo"))) %>% 
    mutate(race_ethnicity = factor(race_ethnicity,
                                   levels = c("Hispanic",
                                              "White non-Hispanic",
                                              "Black non-Hispanic",
                                              "Other non-Hispanic",
                                              "unknown")))
  
  # birth weight in grams (originally ounce)
  df <- df %>%  mutate(birth_weight = Birth.Wgt * 28.3)
  
  # classify time since mab to testing 
  df <- df %>% 
    # 2-month interval
    mutate(days_btw_mab_collection_cat = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -60 ~ "0-2 months", 
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -120 ~ "2-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat = factor(days_btw_mab_collection_cat,
                                                levels = c("no mAb",
                                                           "0-2 months",
                                                           "2-4 months",
                                                           "4 months +"))) %>%
    # 3-months interval
    mutate(days_btw_mab_collection_cat_2 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -90 ~ "0-3 months", 
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -190 ~ "3-6 months",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_2 = factor(days_btw_mab_collection_cat_2,
                                                  levels = c("no mAb",
                                                             "0-3 months",
                                                             "3-6 months"))) %>%
    # 1 months interval
    mutate(days_btw_mab_collection_cat_3 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -30 ~ "0-1 months", 
                       days_btw_mab_collection <= -30 & days_btw_mab_collection > -60 ~ "1-2 months",
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -90 ~ "2-3 months",
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -120 ~ "3-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_3 = factor(days_btw_mab_collection_cat_3,
                                                levels = c("no mAb",
                                                           "0-1 months",
                                                           "1-2 months",
                                                           "2-3 months",
                                                           "3-4 months",
                                                           "4 months +"))) %>%
    mutate(weeks_btw_mab_collection_cat = 
             case_when(
               days_btw_mab_collection < 0 & days_btw_mab_collection > -14 ~ "(0, 2)",
               days_btw_mab_collection <= -14 & days_btw_mab_collection > -14*2 ~ "[2, 4)",
               days_btw_mab_collection <= -14*2 & days_btw_mab_collection > -14*3 ~ "[4, 6)",
               days_btw_mab_collection <= -14*3 & days_btw_mab_collection > -14*4 ~ "[6, 8)",
               days_btw_mab_collection <= -14*4 & days_btw_mab_collection > -14*5 ~ "[8, 10)",
               days_btw_mab_collection <= -14*5 & days_btw_mab_collection > -14*6 ~ "[10, 12)",
               days_btw_mab_collection <= -14*6 & days_btw_mab_collection > -14*7 ~ "[12, 14)",
               days_btw_mab_collection <= -14*7 & days_btw_mab_collection > -14*8 ~ "[14, 16)",
               days_btw_mab_collection <= -14*8 ~ "[16, )",
               is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb"
             )) %>% 
    mutate(weeks_btw_mab_collection_cat = factor(weeks_btw_mab_collection_cat,
                                                 levels = c("no mAb",
                                                            "(0, 2)", "[2, 4)", "[4, 6)", "[6, 8)",
                                                            "[8, 10)", "[10, 12)", "[12, 14)", "[14, 16)",
                                                            "[16, )"
                                                            )))
    
    
  
  
  # group month_when_tested into fewer groups
  df <- df %>% 
    mutate(month_when_tested_cat = case_when(
      collection_date >= as.Date("2023-9-30") & collection_date <= as.Date("2023-11-30") ~ "Oct-Nov",
      collection_date >= as.Date("2023-12-1") & collection_date <= as.Date("2024-1-31") ~ "Dec-Jan",
      collection_date >= as.Date("2024-2-1") & collection_date <= as.Date("2024-3-31") ~ "Feb-Mar",
      collection_date >= as.Date("2024-4-1")  ~ "April and after"
    )) %>% 
    mutate(month_when_tested_cat = factor(month_when_tested_cat,
                                          levels = c("Oct-Nov", "Dec-Jan", "Feb-Mar","April and after")))
  
  
  # dosage 
  df <- df %>% 
    mutate(rsv_mab_dose = case_when(rsv_mab_detailed == "100mg before collection" ~ "100mg",
                                    rsv_mab_detailed == "50mg before collection" ~ "50mg",
                                    TRUE ~ "no mAb"))
  
  
  # oxygen support (assume those without oxygen information are those without oxygen support)
  df <- df %>% 
    mutate(highflow_oxygen = ifelse(is.na(highflow_oxygen), 0, highflow_oxygen))
  
  return(df)
  
}


# additional processing for sensitivity analysis 
process_df_ssa <- function(df){
  
  # reclassify time since vaccination
  # classify time since mab to testing 
  df <- df %>% 
    # 2-month interval
    mutate(days_btw_mab_collection_cat = 
             case_when(days_btw_mab_collection <= -7 & days_btw_mab_collection > -60 ~ "0-2 months", 
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -120 ~ "2-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat = factor(days_btw_mab_collection_cat,
                                                levels = c("no mAb",
                                                           "0-2 months",
                                                           "2-4 months",
                                                           "4 months +"))) %>%
    # 3-months interval
    mutate(days_btw_mab_collection_cat_2 = 
             case_when(days_btw_mab_collection <= -7 & days_btw_mab_collection > -90 ~ "0-3 months", 
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -190 ~ "3-6 months",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_2 = factor(days_btw_mab_collection_cat_2,
                                                  levels = c("no mAb",
                                                             "0-3 months",
                                                             "3-6 months"))) %>%
    # 1 months interval
    mutate(days_btw_mab_collection_cat_3 = 
             case_when(days_btw_mab_collection <= -7 & days_btw_mab_collection > -30 ~ "0-1 months", 
                       days_btw_mab_collection <= -30 & days_btw_mab_collection > -60 ~ "1-2 months",
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -90 ~ "2-3 months",
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -120 ~ "3-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_3 = factor(days_btw_mab_collection_cat_3,
                                                  levels = c("no mAb",
                                                             "0-1 months",
                                                             "1-2 months",
                                                             "2-3 months",
                                                             "3-4 months",
                                                             "4 months +")))
  
  return(df)
  
  
}


###############################
# Functions for VE estimation (regression models)
###############################

## unmatched  model (unstratified)
regress_unmatch <- function(df.ve,
                            confounders){
  
  # get n of cases and controls for unadjusted analysis
  n.cases.mab <- df.ve %>% filter(case_control == 1 & rsv_mab == 1) %>% nrow()
  n.cases.nomab <- df.ve %>% filter(case_control == 1 & rsv_mab == 0) %>% nrow()
  n.controls.mab <- df.ve %>% filter(case_control == 0 & rsv_mab == 1) %>% nrow()
  n.controls.nomab <- df.ve %>% filter(case_control == 0 & rsv_mab == 0) %>% nrow()

  # get n of cases and controls for adjusted analysis
  n.cases.mab.adj <-  df.ve[, c("case_control", "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & rsv_mab == 1) %>% nrow()
  n.cases.nomab.adj <-  df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & rsv_mab == 0) %>% nrow()
  n.controls.mab.adj <- df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & rsv_mab == 1) %>% nrow()
  n.controls.nomab.adj <- df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & rsv_mab == 0) %>% nrow()
  
  
  # unadjusted
  formula <- as.formula("case_control ~ rsv_mab") 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.unadj <- 1 - exp(model$coefficients[2])
  ve.ub.unadj <- 1 - exp(confint(model)[2,][1])
  ve.lb.unadj <- 1 - exp(confint(model)[2,][2])

  # adjusted
  formula <- as.formula(paste(c("case_control ~ rsv_mab", confounders), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.adj <- 1 - exp(model$coefficients[2])
  ve.ub.adj <- 1 - exp(confint(model)[2,][1])
  ve.lb.adj <- 1 - exp(confint(model)[2,][2])
  
  # make a table
  ve.results <- data.frame(
    n_cases_mab = paste0(n.cases.mab, " (", n.cases.mab.adj, ")"),
    n_cases_nomab = paste0(n.cases.nomab, " (", n.cases.nomab.adj, ")"),
    n_controls_mab = paste0(n.controls.mab, " (", n.controls.mab.adj, ")"),
    n_controls_nomab = paste0(n.controls.nomab, " (", n.controls.nomab.adj, ")"),
    ve_unadj = paste0(round(ve.median.unadj*100,1), " (", round(ve.lb.unadj*100,1), "-", round(ve.ub.unadj*100,1), ")"),
    ve_adj = paste0(round(ve.median.adj*100,1), " (", round(ve.lb.adj*100,1), "-", round(ve.ub.adj*100,1), ")")
  ) 
  
  # row.names(ve.results) <- NULL
  
  return(ve.results)
}


########################################################
### Function to convert VE to  forest plot 
########################################################
ve_forest <- function(df.plot = ve){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$ve_adj, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  # if there is lb < 0
  #a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
  #a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  
  a <- a %>% dplyr::rename(ve_adj_median = V1)
  
  df.plot <- cbind(df.plot, a %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median, ve_adj_lb, ve_adj_ub), ~ as.numeric(.)))
  
  # add some vars for plotting
  df.plot <- df.plot %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  # create labels as the first row
  df.plt <- 
    rbind(
    data.frame(against = "Outcome prevented\n",
               n_cases_mab = "Cases \n(Nirsevimab-recepient)\n",
               n_cases_nomab = "Cases \n(Non-recepient)\n",
               n_controls_mab = "Controls \n(Nirsevimab-recepient)\n",
               n_controls_nomab = "Controls \n(Non-recepient)\n",
               ve_unadj = "Unadjusted VE (95% CI)\n",
               ve_adj = "Adjusted VE (95% CI)\n",
               ve_adj_median = NA,
               ve_adj_lb = NA, 
               ve_adj_ub = NA,
               add_arrow = 0
              ),
    df.plot
  )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = against), hjust = 0) +
    # geom_text(aes(x = 2.4, label = n_cases_mab), hjust = 0) +
    # geom_text(aes(x = 2.8, label = n_controls_mab), hjust = 0) +
    # geom_text(aes(x = 3.5, label = n_cases_nomab), hjust = 0) +
    # geom_text(aes(x = 4.2, label = n_controls_nomab), hjust = 0) +
    geom_text(aes(x = 1.3, label = ve_unadj), hjust = 0) +
    geom_text(aes(x = 2.4, label = ve_adj), hjust = 0) +
    theme_void() +
    coord_cartesian(xlim = c(0, 3))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
    
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb, 
                  y = ve_index, yend = ve_index), data = df.plt.arrow,  
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
    
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 6.3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 7.5, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)

  return(plt)

}

ve_forest_withn <- function(df.plot = ve){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$ve_adj, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  # when there is lb < 0 
  #a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
  #a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  
  a <- a %>% dplyr::rename(ve_adj_median = V1)
  
  df.plot <- cbind(df.plot, a %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median, ve_adj_lb, ve_adj_ub), ~ as.numeric(.)))
  
  # add some vars for plotting
  df.plot <- df.plot %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(against = "Outcome prevented",
                 n_cases_mab = "RSV+\n(Vaxed)",
                 n_cases_nomab = "RSV+\n(Unvaxed)",
                 n_controls_mab = "RSV-\n(Vaxed)",
                 n_controls_nomab = "RSV-\n(Unvaxed)",
                 ve_unadj = "Unadjusted VE\n(95% CI)",
                 ve_adj = "Adjusted VE \n(95% CI)",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = 0
      ),
      df.plot
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = against), hjust = 0) +
    geom_text(aes(x = 2, label = n_cases_mab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 2.5, label = n_controls_mab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 3.1, label = n_cases_nomab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 3.6, label = n_controls_nomab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 4.3, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 5.3, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 5.8))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb, 
                     y = ve_index, yend = ve_index), data = df.plt.arrow,  
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
}


# Function making the plot with 2 rows for each outcome 
ve_forest_2rows <- function(df.plot = ve){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$ve_adj, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  # when there is lb < 0 
  #a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
  #a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  
  a <- a %>% dplyr::rename(ve_adj_median = V1)
  
  df.plot <- cbind(df.plot, a %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median, ve_adj_lb, ve_adj_ub), ~ as.numeric(.)))
  
  # convert to several rows per outcome
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA),
        vax = c(NA, "Unvaccinated", "Vaccinated"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = "Outcome prevented\n",
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "Unadjusted VE (95% CI)\n",
                 ve_adj = "Adjusted VE (95% CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = outcome), hjust = 0, fontface= c('bold')) +
    geom_text(aes(x = 0.05, label = vax), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.3, label = case), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.45, label = control), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.6, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.83, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 1))
  
  # df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    # geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb, 
    #                  y = ve_index, yend = ve_index), data = df.plt.arrow,  
    #              arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
}


## function to compare dose-specific VE
ve_forest_dose <- function(df.plot = ve.dose){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  
  # first, process 100mg VE
  a.100mg <- matrix(unlist(strsplit(df.plot$ve_adj_100mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.100mg$ve_adj_lb_100mg <- NA
  a.100mg$ve_adj_ub_100mg <- NA
  
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_lb_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_ub_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.100mg$n_dash){
    # when there is lb < 0 
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_lb_100mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_ub_100mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }

  a.100mg <- a.100mg %>% dplyr::rename(ve_adj_median_100mg = V1)
  
  df.plot <- cbind(df.plot, a.100mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_100mg, ve_adj_lb_100mg, ve_adj_ub_100mg), ~ as.numeric(.))) 
    
  
  # then, process 50mg VE
  a.50mg <- matrix(unlist(strsplit(df.plot$ve_adj_50mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.50mg$ve_adj_lb_50mg <- NA
  a.50mg$ve_adj_ub_50mg <- NA
  
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_lb_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_ub_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.50mg$n_dash){
    # when there is lb < 0 
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_lb_50mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_ub_50mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.50mg <- a.50mg %>% dplyr::rename(ve_adj_median_50mg = V1)
  
  df.plot <- cbind(df.plot, a.50mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_50mg, ve_adj_lb_50mg, ve_adj_ub_50mg), ~ as.numeric(.))) 
  
  
  # rearrange the data frame
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA, NA),
        vax = c(NA, "Unvaccinated", "Vaccinated (50mg)", "Vaccinated (100mg)"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab_50mg, df.plot.temp$n_cases_mab_100mg),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab_50mg, df.plot.temp$n_controls_mab_100mg),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj_50mg, df.plot.temp$ve_unadj_100mg), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj_50mg, df.plot.temp$ve_adj_100mg),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median_50mg, df.plot.temp$ve_adj_median_100mg),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb_50mg, df.plot.temp$ve_adj_lb_100mg),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub_50mg, df.plot.temp$ve_adj_ub_100mg)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  
  # add some vars for plotting
  df.plot.rearrange <- df.plot.rearrange %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = "Outcome prevented\n",
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "Unadjusted VE (95% CI)\n",
                 ve_adj = "Adjusted VE (95% CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = outcome), hjust = 0, fontface= c('bold')) +
    geom_text(aes(x = 0.05, label = vax), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.3, label = case), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.45, label = control), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.6, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.83, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 0.97))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
   
  
}

# function to compare dose-specific VE, adding any dose (100mg or 50mg) category
ve_forest_dose_addanydose <- function(df.plot = ve.dose.addanydose){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  
  # first, process 100mg VE
  a.100mg <- matrix(unlist(strsplit(df.plot$ve_adj_100mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.100mg$ve_adj_lb_100mg <- NA
  a.100mg$ve_adj_ub_100mg <- NA
  
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_lb_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_ub_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.100mg$n_dash){
    # when there is lb < 0 
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_lb_100mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_ub_100mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.100mg <- a.100mg %>% dplyr::rename(ve_adj_median_100mg = V1)
  
  df.plot <- cbind(df.plot, a.100mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_100mg, ve_adj_lb_100mg, ve_adj_ub_100mg), ~ as.numeric(.))) 
  
  
  # then, process 50mg VE
  a.50mg <- matrix(unlist(strsplit(df.plot$ve_adj_50mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.50mg$ve_adj_lb_50mg <- NA
  a.50mg$ve_adj_ub_50mg <- NA
  
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_lb_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_ub_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.50mg$n_dash){
    # when there is lb < 0 
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_lb_50mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_ub_50mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.50mg <- a.50mg %>% dplyr::rename(ve_adj_median_50mg = V1)
  
  df.plot <- cbind(df.plot, a.50mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_50mg, ve_adj_lb_50mg, ve_adj_ub_50mg), ~ as.numeric(.))) 
  
  
  # then process anydose cat
  a.anydose <- matrix(unlist(strsplit(df.plot$ve_adj_anydose, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.anydose$ve_adj_lb_anydose <- NA
  a.anydose$ve_adj_ub_anydose <- NA
  
  a.anydose[which(a.anydose$n_dash == 1),]$ve_adj_lb_anydose <- as.numeric(matrix(unlist(strsplit((a.anydose[which(a.anydose$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.anydose[which(a.anydose$n_dash == 1),]$ve_adj_ub_anydose <- as.numeric(matrix(unlist(strsplit((a.anydose[which(a.anydose$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.anydose$n_dash){
    # when there is lb < 0 
    a.anydose[which(a.anydose$n_dash == 2),]$ve_adj_lb_anydose <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.anydose[which(a.anydose$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.anydose[which(a.anydose$n_dash == 2),]$ve_adj_ub_anydose <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.anydose[which(a.anydose$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.anydose <- a.anydose %>% dplyr::rename(ve_adj_median_anydose = V1)
  
  df.plot <- cbind(df.plot, a.anydose %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_anydose, ve_adj_lb_anydose, ve_adj_ub_anydose), ~ as.numeric(.))) 
  
  
  # rearrange the data frame
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA, NA, NA),
        vax = c(NA, "Unvaccinated", "Vaccinated (50mg)", "Vaccinated (100mg)", "Vaccinated (any dosage)"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab_50mg, df.plot.temp$n_cases_mab_100mg, df.plot.temp$n_cases_mab_anydose),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab_50mg, df.plot.temp$n_controls_mab_100mg, df.plot.temp$n_controls_mab_anydose),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj_50mg, df.plot.temp$ve_unadj_100mg, df.plot.temp$ve_unadj_anydose), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj_50mg, df.plot.temp$ve_adj_100mg, df.plot.temp$ve_adj_anydose),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median_50mg, df.plot.temp$ve_adj_median_100mg, df.plot.temp$ve_adj_median_anydose),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb_50mg, df.plot.temp$ve_adj_lb_100mg, df.plot.temp$ve_adj_lb_anydose),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub_50mg, df.plot.temp$ve_adj_ub_100mg, df.plot.temp$ve_adj_ub_anydose)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  
  # add some vars for plotting
  df.plot.rearrange <- df.plot.rearrange %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = "Outcome prevented\n",
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "Unadjusted VE (95% CI)\n",
                 ve_adj = "Adjusted VE (95% CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = outcome), hjust = 0, fontface= c('bold')) +
    geom_text(aes(x = 0.05, label = vax), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.28, label = case), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.40, label = control), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.52, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.7, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 0.82))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}


### function to breakdown VE median CI string into median lb and ub columns
break_ve <-function(df.plot = ve.wane.2m){
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$VE.adjusted, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% c(a$n_dash)){
    # when there is lb < 0 
    a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a <- a %>% dplyr::rename(ve_adj_median = V1) %>% dplyr::select(-V2, -n_dash)
  
  df.plot.broken <- cbind(df.plot, a) %>% mutate(across(starts_with("ve_adj_"), ~ as.numeric(.)))
  
  return(df.plot.broken)
}








########################################################
# unused functions 
########################################################


# filter out subgroup and calculate VE of each subgroup
regress_unmatch_stratified <- function(df.ve,
                                       stratify_by,
                                       confounders){
  
  df.glm <- df.ve %>% filter(!is.na(get(stratify_by)))
  confounders <- confounders[confounders %in% stratify_by == F]
  
  # remove subgroups with too few records
  if(stratify_by == "risk_factor_gestage"){
    df.glm <- df.glm %>% filter(risk_factor_gestage != "post term")
  }
  
  groups <- unique(df.glm[ , stratify_by])
  # subset each subgroup
  for(i in 1:length(groups)){
    
    # filter out this subgroup
    df.glm <- df.ve %>% filter(get(stratify_by) == groups[i])
    
    
    # run unadjusted glm
    model.unadj <- glm(case_control ~ rsv_mab, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.unadj <- 1 - exp(model.unadj$coefficients[2])
    ve.ub.unadj <- 1 - exp(confint(model.unadj)[2,][1])
    ve.lb.unadj <- 1 - exp(confint(model.unadj)[2,][2])
    
    # run adjusted glm
    if(groups[i] == "preterm"){confounders <- confounders[confounders %in% "risk_factor_atleastone" == F]}
    formula <- as.formula(paste(c("case_control ~ rsv_mab", 
                                  confounders), collapse = " + "))
    model.adj <- glm(formula, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.adj <- 1 - exp(model.adj$coefficients[2])
    ve.ub.adj <- 1 - exp(confint(model.adj)[,1][2])
    ve.lb.adj <- 1 - exp(confint(model.adj)[,2][2])
    
    # get the number of case and controls
    n.cases <- df.glm %>% filter(case_control ==1 ) %>% nrow()
    n.controls <- df.glm %>% filter(case_control == 0) %>% nrow 
    n.cases.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 1) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    n.controls.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 0) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    
    # combine results 
    df.results.temp <- data.frame(
      n_overall = n.cases + n.controls,
      n_overall_eff = n.cases.eff + n.controls.eff,
      n_cases = n.cases,
      n_cases_eff = n.cases.eff,
      n_controls = n.controls, 
      n_controls_eff = n.controls.eff,
      ve_median_unadj = ve.median.unadj,
      ve_lb_unadj = ve.lb.unadj,
      ve_ub_unadj = ve.ub.unadj,
      ve_median_adj = ve.median.adj,
      ve_lb_adj = ve.lb.adj,
      ve_ub_adj = ve.ub.adj
    ) %>%
      mutate(stratify_by = stratify_by,
             strata = groups[i])
    
    if(i == 1){df.results <- df.results.temp}
    if(i != 1){df.results <- rbind(df.results, df.results.temp)}
    
  }
  
  row.names(df.results) <- NULL
  return(df.results)
  
}



regress_match_stratified <- function(df.ve, 
                                     stratify_by,
                                     confounders){
  
  df.glm <- df.ve %>% filter(!is.na(get(stratify_by)))
  confounders <- confounders[confounders %in% stratify_by == F]
  
  # remove subgroups with too few records
  if(stratify_by == "risk_factor_gestage"){
    df.glm <- df.glm %>% filter(risk_factor_gestage != "post term")
  }
  
  groups <- unique(df.glm[ , stratify_by])
  # subset each subgroup
  for(i in 1:length(groups)){
    
    # filter out this subgroup
    df.glm <- df.ve %>% filter(get(stratify_by) == groups[i])
    
    
    # run unadjusted glm
    model.unadj <- glm(case_control ~ rsv_mab + strata(id_case), data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.unadj <- 1 - exp(model.unadj$coefficients[2])
    ve.ub.unadj <- 1 - exp(confint(model.unadj)[2,][1])
    ve.lb.unadj <- 1 - exp(confint(model.unadj)[2,][2])
    
    
    
    # run adjusted glm
    if(groups[i] == "preterm"){confounders <- confounders[confounders %in% "risk_factor_atleastone" == F]}
    formula <- as.formula(paste(c("case_control ~ rsv_mab + strata(id_case)", 
                                  confounders), collapse = " + "))
    model.adj <- glm(formula, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.adj <- 1 - exp(model.adj$coefficients[2])
    ve.ub.adj <- 1 - exp(confint(model.adj)[,1][2])
    ve.lb.adj <- 1 - exp(confint(model.adj)[,2][2])
    
    # get the number of case and controls
    n.cases <- df.glm %>% filter(case_control ==1 ) %>% nrow()
    n.controls <- df.glm %>% filter(case_control == 0) %>% nrow 
    n.cases.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 1) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    n.controls.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 0) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    
    # combine results 
    df.results.temp <- data.frame(
      n_overall = n.cases + n.controls,
      n_overall_eff = n.cases.eff + n.controls.eff,
      n_cases = n.cases,
      n_cases_eff = n.cases.eff,
      n_controls = n.controls, 
      n_controls_eff = n.controls.eff,
      ve_median_unadj = ve.median.unadj,
      ve_lb_unadj = ve.lb.unadj,
      ve_ub_unadj = ve.ub.unadj,
      ve_median_adj = ve.median.adj,
      ve_lb_adj = ve.lb.adj,
      ve_ub_adj = ve.ub.adj
    ) %>%
      mutate(stratify_by = stratify_by,
             strata = groups[i])
    
    if(i == 1){df.results <- df.results.temp}
    if(i != 1){df.results <- rbind(df.results, df.results.temp)}
    
  }
  
  row.names(df.results) <- NULL
  return(df.results)
  
}













