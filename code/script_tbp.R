# import database from excel
d <- read_excel("~/OneDrive/research data/JIKEI/ICI/211106_PEM_DB merge.xlsx")

# data processing
d1 <- d %>% filter(Hospital != "Osaka") # remove patients from Osaka

# time-to-event data
# baseline --- event
fu <- as.Date(d1$Day.Start) %--% as.Date(d1$Day.LastVisit) # follow-up period
ttp <- as.Date(d1$Day.Start) %--% as.Date(d1$Day.Progression) # time-to-progression
ittp <- as.Date(d1$Day.Start) %--% as.Date(d1$Day.Progression.iRECIST) # time-to-progression per iRECIST
ttcp <- as.Date(d1$Day.Start) %--% as.Date(d1$Day.ClinProg) # time-to-clinical progression 
ttd <- as.Date(d1$Day.Start) %--% as.Date(d1$Day.LastAdministration) # time-to-treatment discontinuation 
ttae <- as.Date(d1$Day.Start) %--% as.Date(d1$Day.Adverse.Event) # time-to-irAE

d1$ttp_na <- time_length(ttp, unit = "month") # no progression = NA
d1$ittp_na <- time_length(ittp, unit = "month") # no progression = NA
d1$ttcp_na <- time_length(ttcp, unit = "month") # no progression = NA
d1$fu <- time_length(fu, unit = "month") 
d1$ttae_na <- time_length(ttae, unit = "month") # no irAE = NA
d1$ttd_na <- time_length(ttd, unit = "month") # patient continuing treatment = NA

# progression --- event
ppttd <- as.Date(d1$Day.Progression) %--% as.Date(d1$Day.LastAdministration)
ippttd <- as.Date(d1$Day.Progression.iRECIST) %--% as.Date(d1$Day.LastAdministration)
ppfu <- as.Date(d1$Day.Progression) %--% as.Date(d1$Day.LastVisit)
ippfu <- as.Date(d1$Day.Progression.iRECIST) %--% as.Date(d1$Day.LastVisit)

d1$ppttd_na <- time_length(ppttd, unit = "month")
d1$ippttd_na <- time_length(ippttd, unit = "month")
d1$ppfu <- time_length(ppfu, unit = "month")
d1$ippfu <- time_length(ippfu, unit = "month")

d1$fu <- as.numeric(d1$fu)
d1$ppfu <- as.numeric(d1$ppfu)
d1$ippfu <- as.numeric(d1$ippfu)

d1 = d1 %>% mutate(
  ttp = as.numeric(case_when(is.na(ttp_na) ~ fu, TRUE ~ ttp_na)), # NA -> follow-up period
  ittp = as.numeric(case_when(is.na(ittp_na) ~ fu, TRUE ~ ittp_na)), # NA -> follow-up period
  ttcp = as.numeric(case_when(is.na(ttcp_na) ~ fu, TRUE ~ ttcp_na)), # NA -> follow-up period
  ttae = as.numeric(case_when(is.na(ttae_na) ~ fu, TRUE ~ ttae_na)), # NA -> follow-up period
  ttd = as.numeric(case_when(is.na(ttd_na) ~ fu, TRUE ~ ttd_na)),
  ppttd = as.numeric(case_when(is.na(ppttd_na) ~ ppfu, ppttd_na < 0 ~ 0, TRUE ~ ppttd_na)),
  ippttd = as.numeric(case_when(is.na(ippttd_na) ~ ippfu, ippttd_na < 0 ~ 0, TRUE ~ ippttd_na)),
  
  # other variable
  
  or = case_when((Best.Response == "CR" | Best.Response == "PR") ~ "Yes", TRUE ~ "No"), # calculate OR = CR + PR
  ior = case_when((Best.Response.iRECIST == "iCPD" | 
                     Best.Response.iRECIST == "iUPD" | 
                     Best.Response.iRECIST == "iSD") ~ "No", TRUE ~ "Yes"),
  irae_2cycle = case_when((irAE == "Yes" & ttae <= 2) ~ "Yes", TRUE ~ "No"),
  age_bin = case_when(Age >= median(Age) ~ ">median", TRUE ~ "<median"),
  bmi = Body.Weight / Body.Height / Body.Height * 10000,
  bmi_bin = case_when(bmi >= median(bmi) ~ ">median", TRUE ~ "<median"),
  ECOG.Score = case_when(ECOG.PS == 0 ~ 0, TRUE ~ 1),
  Hb.Score = case_when(Hb.Baseline <= 10 ~ 1, TRUE ~ 0),
  hb_bin = case_when(Hb.Baseline <= median(Hb.Baseline) ~ "<median", TRUE ~ ">median"),
  ttp_bin = case_when(ttp <= median(ttp) ~ "<median", TRUE ~ ">median"),
  ittp_bin = case_when(ittp <= median(ittp) ~ "<median", TRUE ~ ">median"),
  Liver.Score = case_when(Liver == "Yes" ~ 1, TRUE ~ 0),
  NLR.Baseline = Neu.Baseline / Lym.Baseline,
  nlr_cut = case_when(NLR.Baseline >= 3 ~ "High", TRUE ~ "Low"),
  double = case_when(Day.Progression.iRECIST == Day.ClinProg ~ "Yes", TRUE ~ "No"),
  poor = case_when((Best.Response.iRECIST == "iCPD" | Best.Response.iRECIST == "iUPD") ~ "yes", TRUE ~ "no"),
  
  # variable regarding treatment duration
  
  ppt = case_when(ppttd >= 2 ~ "yes", TRUE ~ "no"),
  ippt = case_when(ippttd >= 2 ~ "yes", TRUE ~ "no"),
  
  # y/n -> 1/0
  
  prog_bin = case_when(Progression == "Yes" ~ 1, Progression == "No" ~ 0),
  iprog_bin = case_when(Progression.iRECIST == "Yes" ~ 1, Progression.iRECIST == "No" ~ 0),
  cp_bin = case_when(Clinical.Progression == "Yes" ~ 1, Clinical.Progression == "No" ~ 0),
  or_bin = case_when(or == "Yes" ~ 1, TRUE ~ 0),
  ior_bin = case_when(ior == "Yes" ~ 1, TRUE ~ 0),
  deceased_bin = case_when(Deceased == "Yes" ~ 1, Deceased == "No" ~ 0),
  ppt_bin = case_when(ppt == "yes" ~ 1, TRUE ~ 0),
  ippt_bin = case_when(ippt == "yes" ~ 1, TRUE ~ 0)
)

# reorder factors
d1$Metastasis.Category <- gdata::reorder.factor(d1$Metastasis.Category, 
                                                new.order = c("LN metastasis", "Visceral metastasis", "LN and visceral metastasis")) # change order of factor
d1$Best.Response <- gdata::reorder.factor(d1$Best.Response, 
                                          new.order = c("PD", "SD", "PR", "CR")) # change order of factor
d1$Best.Response.iRECIST <- gdata::reorder.factor(d1$Best.Response.iRECIST, 
                                                  new.order = c("iCPD", "iUPD", "iSD", "iPR", "iCR")) # change order of factor

# numeric -> factor
d1$Number.Prior.Chemotherapy <- as.factor(d1$Number.Prior.Chemotherapy)
d1$ECOG.Score <- as.factor(d1$ECOG.Score)

d2 <- d1 %>% filter(Histology == "Urothelial carcinoma") 
d3 <- d2 %>% filter(Platinum == "Cisplatin" | Platinum == "Carboplatin" | Platinum == "Nedaplatin") # select patients received platinum-based chemotherapy
d4 <- d3 %>% filter(!(Deceased == "Yes" & Day.LastVisit == Day.Progression))
df <- d4 %>% filter(Progression == "Yes") # select patients developing disease progression per iRECIST

# inverse probability weights calculation
cbw <- weightit(ppt_bin ~ 
                  Age + Gender + ECOG.Score + Smoking.Status + Primary.Location + Number.Prior.Chemotherapy + Platinum + 
                  Metastasis.Category + hb_bin + nlr_cut + or + ttp_bin + double, 
                data = df, method = "cbps", estimand = "ATE")
wgt <- cbw[["weights"]]
df <- tibble(df, wgt)

# logistic regression model
log <- glm(ippt_bin ~ 
             Age + Gender + ECOG.Score + Smoking.Status + Primary.Location + Number.Prior.Chemotherapy + Platinum + 
             Metastasis.Category + hb_bin + nlr_cut + ior + ittp_bin + double, # factors for calculating propensity scores
           data = df, family = binomial) %>%  # calculate PS using logistic regression model
  tbl_regression(exponentiate = TRUE, 
                 label = list(Age ~ "Age at baseline, year",
                              Gender ~ "Sex",
                              ECOG.Score ~ "ECOG performance status",
                              Smoking.Status ~ "Smoking status",
                              Primary.Location ~ "Primary location of tumor",
                              Metastasis.Category ~ "Location of metastasis",
                              Number.Prior.Chemotherapy ~ "Number of previous chemotherapies",
                              Platinum ~ "Platinum",
                              hb_bin ~ "Baseline hemoglobin, g/dL (<median or >median)",
                              nlr_cut ~ "Baseline NLR (<3 or >3)",
                              ior ~ "Objective response",
                              ittp_bin ~ "Time to immune progression, month (<median or >median)",
                              double ~ "Clinicoradiographic progression"))

#tableone
theme_gtsummary_journal(journal = "jama")
# unweighted patient characteristics
tableone_uw <- df %>% 
  select(Age, Gender, ECOG.Score, Smoking.Status, Primary.Location, 
         Metastasis.Category, Number.Prior.Chemotherapy, Platinum, hb_bin, nlr_cut, ior, ittp_bin, double, ippt) %>% 
  tbl_summary(
    by = ippt, 
    label = list(
      Age ~ "Age at baseline, year",
      Gender ~ "Sex",
      ECOG.Score ~ "ECOG performance status",
      Smoking.Status ~ "Smoking status",
      Primary.Location ~ "Primary location of tumor",
      Metastasis.Category ~ "Location of metastasis",
      Number.Prior.Chemotherapy ~ "Number of previous chemotherapies",
      Platinum ~ "Platinum",
      hb_bin ~ "Baseline hemoglobin, g/dL (<median or >median)",
      nlr_cut ~ "Baseline NLR (<3 or >3)",
      ior ~ "Objective response",
      ittp_bin ~ "Time to immune progression, month (<median or >median)",
      double ~ "Clinicoradiographic progression"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p})"),
    digits = list(all_continuous() ~ c(1, 1),
                  all_categorical() ~ c(1, 1))
  ) %>% 
  add_difference(test = list(everything() ~ "smd")) %>% 
  modify_column_hide(columns = ci)

# weighted patient characteristics
d_wgt <- survey::svydesign(ids = ~ Identifier, weights = ~ wgt, data = df)
tableone_w <- d_wgt %>% 
  tbl_svysummary(
    by = ippt, 
    include = c(Age, Gender, ECOG.Score, Smoking.Status, Primary.Location, 
                Metastasis.Category, Number.Prior.Chemotherapy, Platinum, hb_bin, nlr_cut, ior, ittp_bin, double),
    label = list(
      Age ~ "Age at baseline, year",
      Gender ~ "Sex",
      ECOG.Score ~ "ECOG performance status",
      Smoking.Status ~ "Smoking status",
      Primary.Location ~ "Primary location of tumor",
      Metastasis.Category ~ "Location of metastasis",
      Number.Prior.Chemotherapy ~ "Number of previous chemotherapies",
      Platinum ~ "Platinum",
      hb_bin ~ "Baseline hemoglobin, g/dL (<median or >median)",
      nlr_cut ~ "Baseline NLR (<3 or >3)",
      ior ~ "Objective response",
      ittp_bin ~ "Time to immune progression, month (<median or >median)",
      double ~ "Clinicoradiographic progression"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}"),
    digits = list(all_continuous() ~ c(1, 1),
                  all_categorical() ~ c(1, 1))
  ) %>% 
  add_difference(test = list(everything() ~ "smd")) %>% 
  modify_column_hide(columns = ci)

tableone_merge <- tbl_merge(tbls = list(tableone_uw, tableone_w), 
                            tab_spanner = c("**Unweighted population**", "**Weighted population**"))

write.csv(tableone_merge, "~/Desktop/result.csv") # export table

# the distribution of treatment duration
time <- df %>% filter(ippt == "yes") %>% 
  ggplot(aes(x = ippttd, fill = ippt)) +
  geom_density(alpha = 0.47) + 
  labs(x = "Treatment duration after progression, month",
       y = "Kernel density") + 
  theme_minimal() + theme(
    axis.ticks.y = element_blank(),
    text = element_text(size = 14),
    legend.position = "none") + 
  ggsci::scale_fill_aaas() + 
  ggsci::scale_color_aaas() + 
  ggtitle("Distribution of treatment duration after progression")

# Kernel density plot of estimated propensity scores
logit.ps <- glm(ippt_bin ~ 
                  Age + Gender + ECOG.Score + Smoking.Status + Primary.Location + Number.Prior.Chemotherapy + Platinum + 
                  Metastasis.Category + hb_bin + nlr_cut + ior + ittp_bin + double, # factors for calculating propensity scores
                data = df, family = binomial) # calculate PS using logistic regression model
pscores <- fitted(logit.ps) # pscores = propensity score
df <- df %>% mutate(ps = pscores)

g <- df %>% 
  ggplot(aes(x = ps, fill = ippt)) +
  geom_density(alpha = 0.47) + 
  labs(x = "Estimated probability of treatment continuation",
       y = "Kernel density",
       title = "Before weighting") + 
  theme_minimal() + theme(
    axis.ticks.y = element_blank(),
    text = element_text(size = 14),
    legend.position = "top") + 
  ggsci::scale_fill_aaas() + 
  ggsci::scale_color_aaas() + 
  guides(fill = guide_legend(title = "Treatment continuation"))

wg <- df %>% 
  ggplot(aes(x = ps, fill = ippt, weight = wgt)) +
  geom_density(alpha = 0.47) + 
  labs(x = "Estimated probability of treatment continuation",
       y = "Kernel density",
       title = "After weighting") + 
  theme_minimal() + theme(
    axis.ticks.y = element_blank(),
    text = element_text(size = 14),
    legend.position = "top") + 
  ggsci::scale_fill_aaas() + 
  ggsci::scale_color_aaas() + 
  guides(fill = guide_legend(title = "Treatment continuation"))

merge <- egg::ggarrange(g, wg, ncol = 2)

follow <- df %>% 
  survfit(Surv(fu, deceased_bin) ~ 1, data = .) 
median.fu <- surv_median(follow)[1, 2] %>% round(digits = 1) %>% as.numeric() 

# survival analysis
cpfs_fit <- survival::survfit(data = df, Surv(ttcp, cp_bin) ~ ppt, weight = wgt)
cpfs <- survminer::ggsurvplot(cpfs_fit, 
                              risk.table = FALSE, font.tickslab = 12, font.x = 12, font.y = 12,
                              title = "Clinical progression-free survival", 
                              legend = "top", legend.labs = c("no", "yes"), 
                              legend.title = "Treatment continuation", censor = FALSE,
                              xlab = "Time from treatment initiation, month", 
                              ylab = "Clinical progression-free survival probability",
                              palette = "aaas", size = 0.6, 
                              ggtheme = theme_classic()) 

os_fit <- survival::survfit(data = df, Surv(fu, deceased_bin) ~ ppt, weight = wgt)
os <- survminer::ggsurvplot(os_fit, 
                            risk.table = FALSE, font.tickslab = 12, font.x = 12, font.y = 12,
                            title = "Overall survival", 
                            legend = "top", legend.labs = c("no", "yes"), 
                            legend.title = "Treatment continuation", censor = FALSE,
                            xlab = "Time from treatment initiation, month", 
                            ylab = "Overall survival probability",
                            palette = "aaas", size = 0.6, 
                            ggtheme = theme_classic()) 

ppos_fit <- survival::survfit(data = df, Surv(ppfu, deceased_bin) ~ ppt, weight = wgt)
ppos <- survminer::ggsurvplot(ppos_fit, 
                              risk.table = FALSE, font.tickslab = 12, font.x = 12, font.y = 12,
                              title = "Post-progression overall survival", 
                              legend = "top", legend.labs = c("no", "yes"), 
                              legend.title = "Treatment continuation", censor = FALSE,
                              xlab = "Time from treatment initiation, month", 
                              ylab = "Post-progression overall survival probability",
                              palette = "aaas", size = 0.6, 
                              ggtheme = theme_classic()) 

merge.km <- 
  survminer::arrange_ggsurvplots(list(cpfs, os, ppos), print = FALSE, 
                                 nrow = 1, ncol = 3)

# weighted log rank test
pval_cpfs <- logrank.test(time = df$ttcp, event = df$cp_bin, 
                          group = df$ppt, weights = wgt)
pval_os <- logrank.test(time = df$fu, event = df$deceased_bin, 
                        group = df$ppt, weights = wgt)
pval_ppos <- logrank.test(time = df$ppfu, event = df$deceased_bin, 
                          group = df$ppt, weights = wgt)
pval_cpfs
pval_os
pval_ppos

# landmark analysis
df_lm <- df %>% filter(ppfu >= 1.2) # filter patients developing clinical progression within 2 cycles of pembrolizumab after radiographic progression
wgt2 <- weightit(ppt_bin ~ 
                   Age + Gender + ECOG.Score + Smoking.Status + Primary.Location + Number.Prior.Chemotherapy + Platinum + 
                   Metastasis.Category + hb_bin + nlr_cut + or + ttp_bin + double, 
                 data = df_lm, method = "cbps", estimand = "ATE")
wgt_lm <- wgt2[["weights"]]
df_lm <- tibble(df_lm, wgt_lm)

# weighted log rank test
pval_cpfs <- logrank.test(time = df_lm$ttcp, event = df_lm$cp_bin, 
                          group = df_lm$ppt, weights = df_lm$wgt_lm)
pval_os <- logrank.test(time = df_lm$fu, event = df_lm$deceased_bin, 
                        group = df_lm$ppt, weights = df_lm$wgt_lm)
pval_ppos <- logrank.test(time = df_lm$ppfu, event = df_lm$deceased_bin, 
                          group = df_lm$ppt, weights = df_lm$wgt_lm)
pval_cpfs["test"]
pval_os["test"]
pval_ppos["test"]

cpfs_fit <- survival::survfit(data = df_lm, Surv(ttcp, cp_bin) ~ ppt, weight = wgt_lm)
cpfs <- survminer::ggsurvplot(cpfs_fit, 
                              risk.table = FALSE, font.tickslab = 12, font.x = 12, font.y = 12,
                              title = "Clinical progression-free survival", 
                              legend = "top", legend.labs = c("no", "yes"), 
                              legend.title = "Treatment continuation", censor = FALSE,
                              xlab = "Time from treatment initiation, month", 
                              ylab = "Clinical progression-free survival probability",
                              palette = "aaas", size = 0.6, 
                              ggtheme = theme_classic()) 

os_fit <- survival::survfit(data = df_lm, Surv(fu, deceased_bin) ~ ppt, weight = wgt_lm)
os <- survminer::ggsurvplot(os_fit, 
                            risk.table = FALSE, font.tickslab = 12, font.x = 12, font.y = 12,
                            title = "Overall survival", 
                            legend = "top", legend.labs = c("no", "yes"), 
                            legend.title = "Treatment continuation", censor = FALSE,
                            xlab = "Time from treatment initiation, month", 
                            ylab = "Overall survival probability",
                            palette = "aaas", size = 0.6, 
                            ggtheme = theme_classic()) 

ppos_fit <- survival::survfit(data = df_lm, Surv(ppfu, deceased_bin) ~ ppt, weight = wgt_lm)
ppos <- survminer::ggsurvplot(ppos_fit, 
                              risk.table = FALSE, font.tickslab = 12, font.x = 12, font.y = 12,
                              title = "Post-progression overall survival", 
                              legend = "top", legend.labs = c("no", "yes"), 
                              legend.title = "Treatment continuation", censor = FALSE,
                              xlab = "Time from treatment initiation, month", 
                              ylab = "Post-progression overall survival probability",
                              palette = "aaas", size = 0.6, 
                              ggtheme = theme_classic()) 

merge.km <- 
  survminer::arrange_ggsurvplots(list(cpfs, os, ppos), print = FALSE, 
                                 nrow = 1, ncol = 3)

# testing proportional hazards asusmption
fit <- df %>% coxph(Surv(fu, deceased_bin) ~ ppt, weights = wgt, data = .)
ggcoxzph(cox.zph(fit))

# univariable Cox regression analysis with time-varying covariates
data_iTime <- df %>% select(Identifier, wgt)
data_dTime <- df %>% select(Identifier, ppttd, fu, deceased_bin)
#1. create start/stop time and time-dependent covariate
spl_dataset <- survival::tmerge(data1 = data_iTime, data2 = data_dTime, 
                                id = Identifier, event = event(fu, deceased_bin), cont_treatment = tdc(ppttd)) # tdc = time-dependent covariate
spl_dataset <- spl_dataset %>% mutate(discont_treatment = case_when(cont_treatment == 0 ~ 1, 
                                                                    cont_treatment == 1 ~ 0)) # exchange 0 and 1 in spl_dataset
#2. execute multivariable Cox regression analysis
tdc <- survival::coxph(Surv(tstart, tstop, event) ~ # define survival data (start and stop data)
                         discont_treatment,
                       cluster = Identifier, data = spl_dataset, weights = wgt) %>% 
  tbl_regression(exponentiate = TRUE,
                 label = discont_treatment ~ "Treatment duration beyond progression, month")

# RMST calculation and comparison
d_rmst <- df %>% 
  select(fu, ppfu, deceased_bin, ttcp, cp_bin, ppt, wgt) %>% 
  filter(!is.na(ppt)) %>% filter(!is.na(fu)) %>% 
  arrange(ppt) %>% 
  as.data.frame() 

d_rmst$ppt <- as.factor(d_rmst$ppt)

armst <- function(tau) {
  j = length(unique(d_rmst$ppt))
  
  rmst <- rep(999, length(1:j))
  groupval <- rep(999, length(1:j))
  rmst_var <- rep(999, length(1:j))
  rmst_se <- rep(999, length(1:j))
  
  for (i in 1:j){
    groupval[i] <- (levels(d_rmst$ppt)[i])
    dat_group <- d_rmst[which(d_rmst$ppt == (groupval[i])),]
    
    #--- AKM ---
    # Based on 'adjusted.KM' function from {IPWsurvival} package
    # Author: F. Le Borgne and Y. Foucher
    tj <- c(0, sort(unique(dat_group$fu[dat_group$cp_bin == 1])))
    dj <- sapply(tj, function(x) {sum(dat_group$wgt[dat_group$ttcp == x & dat_group$cp_bin == 1])})
    yj <- sapply(tj, function(x) {sum(dat_group$wgt[dat_group$ttcp >= x])})
    st <- cumprod(1 - (dj/yj))
    m <- sapply(tj, function(x) {sum((dat_group$wgt[dat_group$ttcp >= x])^2)})
    mj <- ((yj^2)/m)
    #ft <- data.frame(time = tj, n_risk = yj, n_event = dj, survival = st, variable = i, m = mj)
    ft <- data.frame(tj, yj, dj, st, i, mj)
    
    #--- RMST ---
    # Based on 'rmst1 function' from {survRM2} package
    # Author: Hajime Uno, Lu Tian, Angel Cronin, Chakib Battioui, Miki Horiguchi
    rtime <- ft$tj <= tau
    tj_r <- sort(c(ft$tj[rtime], tau))
    st_r <- ft$st[rtime]
    yj_r <- ft$yj[rtime]
    dj_r <- ft$dj[rtime]
    time_diff <- diff(c(0, tj_r))
    areas <- time_diff * c(1, st_r)
    rmst[i] <- sum(areas)
    
    mj_r <- ft$mj[rtime]
    var_r <- ifelse((yj_r-dj_r)　==　0, 0, dj_r /　(mj_r *　(yj_r - dj_r)))
    #var_r <- ifelse((yj_r-dj_r)==0, 0, dj_r /(yj_r *(yj_r - dj_r)))
    var_r <- c(var_r,　0)
    rmst_var[i] <- sum(cumsum(rev(areas[-1])) ^ 2 * rev(var_r)[-1])
    rmst_se[i] <- sqrt(rmst_var[i])
  }
  
  # --- Compare RMST between groups and compile output---
  
  output <- tibble(rmst = 0, 
                   lcl = 0, 
                   ucl = 0)
  
  output$rmst <- rmst[2] - rmst[1] # no = 1, yes = 2
  output$lcl <- rmst[2] - rmst[1] - qnorm(1 - 0.2 / 2) * sqrt(rmst_var[2] + rmst_var[1]) # no = 1, yes = 2
  output$ucl <- rmst[2] - rmst[1] + qnorm(1 - 0.2 / 2) * sqrt(rmst_var[2] + rmst_var[1]) # no = 1, yes = 2
  
  print(output)
}
data_rmst <- data.frame()
for(i in seq(from = 0, to = 24, by = 0.01)) {
  data_rmst = rbind(data_rmst, armst(i))
}

tau_vec <- as.data.frame(seq(from = 0, to = 24, by = 0.01))

plot_rmst <- cbind(tau_vec, data_rmst)
colnames(plot_rmst)[1] <- "tau"
colnames(plot_rmst)[2] <- "rmst"
colnames(plot_rmst)[3] <- "lcl"
colnames(plot_rmst)[4] <- "ucl"

# plot RMST difference for IPW-adjusted model
rmst_plot <- plot_rmst %>% 
  ggplot(data = ., aes(x = tau)) +
  geom_smooth(aes(y = rmst), size = 0.8) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.6, fill = "#3C5488FF") +
  scale_y_continuous(limits = c(-1, 5), breaks = c(-1, 0, 1, 2, 3, 4, 5)) +
  scale_x_continuous(limits = c(0, 16), breaks = seq(from = 0, to = 16, by = 2)) +
  geom_hline(aes(yintercept = 0), lty = "dashed") +
  labs(x = "Time from treatment initiation, month", 
       y = "Difference in RMST (95% CI), month") +
  theme_classic() +
  ggtitle("Clinical progression-free survival") +
  theme(text = element_text(size = 14),
        axis.title.x =element_text(hjust = 0.5, size = 14),
        plot.title = element_text(size = 14)) + 
  guides(fill = guide_legend(nrow = 2)) 

# CONSORT diagram using ggconsort
study_cohorts <- 
  d1 %>%
  cohort_start("The Jikei University UC DataBase<br>
  Patients with bladder/ureter/pelvic cancer<br>
               treated with pembrolizumab") %>%
  
  # Define cohorts using named expressions --------------------
# Notice that you can use previously defined cohorts in subsequent steps
cohort_define(
  d2 = .full %>% filter(Histology == "Urothelial carcinoma"),
  d3 = d2 %>% filter(Platinum == "Cisplatin" | Platinum == "Carboplatin" | Platinum == "Nedaplatin"),
  d4 = d3 %>% filter(!is.na(Best.Response)),
  d5 = d4 %>% filter(!(Deceased == "Yes" & Day.LastVisit == Day.Progression)),
  df = d5 %>% filter(Progression == "Yes"),
  d_tbp = df %>% filter(ippt == "yes"),
  d_ntbp = df %>% filter(ippt == "no"),
  
  # anti_join is useful for counting exclusions -------------
  excluded_histology = anti_join(.full, d2, by = "Identifier"),
  excluded_platinum = anti_join(d2, d3, by = "Identifier"),
  excluded_response = anti_join(d3, d4, by = "Identifier"),
  excluded_deceased = anti_join(d4, d5, by = "Identifier"),
  excluded_progression = anti_join(d5, df, by = "Identifier")
) %>%
  # Provide text labels for cohorts ---------------------------
cohort_label(
  d2 = "Patients with urothelial carcinoma",
  d3 = "Patients with previous history of platinum-based chemotherapy",
  d4 = "Patients with available treatment response data",
  d5 = "Patients who did not expired before progression",
  df = "Final set",
  d_tbp = "Continuation group",
  d_ntbp = "Discontinuation group",
  
  excluded_histology = "Non-urothelial carcinoma",
  excluded_platinum = "No prior history of platinum-based chemotherapy",
  excluded_response = "No available information regarding treatment response",
  excluded_deceased = "Expired before progression",
  excluded_progression = "Alived without progression"
)

study_consort <- study_cohorts %>%
  consort_box_add(
    "full", x = 0, y = 25, cohort_count_adorn(., .full)
  ) %>%
  consort_box_add(
    "exclusions1", x = 10, y = 20, glue::glue(
      "{cohort_count_adorn(study_cohorts, excluded_histology)}<br>
      {cohort_count_adorn(study_cohorts, excluded_platinum)}<br>
      {cohort_count_adorn(study_cohorts, excluded_response)}
      ")
  ) %>%
  consort_box_add(
    "exclusions2", x = 10, y = 15, glue::glue(
      "{cohort_count_adorn(study_cohorts, excluded_deceased)}<br>
      {cohort_count_adorn(study_cohorts, excluded_progression)}
      ")
  ) %>%
  consort_box_add(
    "df", 0, 10, cohort_count_adorn(., df)
  ) %>%
  consort_box_add(
    "d_tbp", -30, 0, cohort_count_adorn(study_cohorts, d_tbp)
  ) %>%
  consort_box_add(
    "d_ntbp", 30, 0, cohort_count_adorn(study_cohorts, d_ntbp)
  ) %>%
  
  consort_arrow_add(
    end = "exclusions1", end_side = "left", start_x = 0, start_y = 20
  ) %>%
  consort_arrow_add(
    end = "exclusions2", end_side = "left", start_x = 0, start_y = 15
  ) %>% 
  consort_arrow_add(
    start = "full", start_side = "bottom", end = "df", end_side = "top",
  ) %>% 
  consort_line_add(
    start_x = 0, start_y = 10, end_x = 0, end_y = 5,
  ) %>% 
  consort_line_add(
    start_x = -30, start_y = 5, end_x = 30, end_y = 5,
  ) %>% 
  consort_arrow_add(
    end = "d_tbp", end_side = "top", start_x = -30, start_y = 5
  ) %>%
  consort_arrow_add(
    end = "d_ntbp", end_side = "top", start_x = 30, start_y = 5
  )

study_consort %>%
  ggplot() + 
  geom_consort() +
  theme_consort(margin_h = 15, margin_v = 2.5) + 
  
  ggtext::geom_richtext(
    aes(x = 0, y = 6.75, label = "Duration of treatment beyond progression (TBP) >1.5 months (2 cycles)"),
    fill = "#9bc0fc"
  )
