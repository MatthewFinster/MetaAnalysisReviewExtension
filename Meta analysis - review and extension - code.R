# Packages --------------------------------------------------------------------
library(metafor)
library(meta)

# ------------------------------------------------------------------------------
# REPLICATING FINDINGS ---------------------------------------------------------
# ------------------------------------------------------------------------------

# Sample sizes of studies ------------------------------------------------------
shin_n = 16
noorimofrad_n = 30
ribeiro_n = 87
werner_n = 124
sachezgonzalez_n = 74
eigendorf_n = 291
nickels_n = 22
mason_n = 439-237
friedenreich_n = 212

n_allstudies = c(shin_n,noorimofrad_n,ribeiro_n,werner_n,eigendorf_n,
                 nickels_n,mason_n,friedenreich_n,sachezgonzalez_n)
sum(n_allstudies)

# Calculating a random effects model using MD ----------------------------------

# Raw data from the meta analysis
data <- data.frame(
  Study = c("Shin et al, 2008", "Nickels et al, 2022", "Friedenreich et al, 2018", 
            "Eigendorf et al, 2019", "Ribeiro et al, 2021A", "Mason et al, 2013", 
            "Ribeiro et al, 2021B", "Noorimofrad et al, 2018", "Werner et al, 2019B", 
            "Werner et al, 2019C", "Werner et al, 2019A", "Sanchez-Gonzalez et al, 2021"),
  n1 = c(8, 11, 99, 146, 28, 116, 29, 15, 29, 34, 26, 41),
  m1 = c(7.67, 0.70, 0.87, 1.12, 1.45, 1.02, 1.54, 1.60, 5.50, 5.50, 5.70, 4.81),
  sd1 = c(1.74, 0.17, 0.22, 0.25, 0.46, 0.19, 0.51, 0.20, 1.20, 0.80, 1.40, 3.16),
  n2 = c(8, 11, 113, 145, 15, 86, 15, 15, 12, 13, 10, 33),
  m2 = c(7.99, 0.81, 0.92, 1.12, 1.45, 1.01, 1.45, 1.45, 5.20, 5.20, 5.20, 1.81),
  sd2 = c(2.60, 0.14, 0.28, 0.23, 0.46, 0.17, 0.46, 0.15, 0.92, 0.92, 0.92, 2.34)
)

# ?metafor
# ?meta

# Meta analysis ---------------------------------------------------------------
# Opted for metacont (meta package) as the forest plot function is more 
# user-friendly than in rma (metafor package) 

# Meta analysis
meta_analysis <- metacont(
  n.e = data$n1, mean.e = data$m1, sd.e = data$sd1, 
  n.c = data$n2, mean.c = data$m2, sd.c = data$sd2, 
  studlab = data$Study, 
  data = data, 
  fixed = FALSE,
  method.random.ci = "HK",
  method.predict =  "HK",
  sm = "MD",
  method.tau = "REML",
  prediction = TRUE
)

# Forest plot
forest(meta_analysis,
       leftlabs = c("Study", "Experimental Mean (SD)", "Control Mean (SD)"),
       rightlabs = c("MD", "95% CI", "Weight (%)"),
       lab.e = "Experimental",
       lab.c = "Control",
       text.random = "Random-effects model",
       text.predict = "Prediction interval",
       comb.fixed = FALSE,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE,
       digits = 2,
       col.square = "grey", 
       col.diamond = "grey",
       col.predict = "darkred"
)

# Sensitivity analysis / detecting outliers -----------------------------------
# As the meta package was used, the metainf function (meta) is used for the
# leave one out analysis rather than the leave1out function (metafor)

leave_one_out <- metainf(meta_analysis)

# summary(leave_one_out)

# Plotting the leave-one-out analysis
forest(leave_one_out,
       leftlabs = c("Study removed", "Experimental Mean (SD)", "Control Mean (SD)"),
       rightlabs = c("MD", "95% CI", "Weight (%)"),
       lab.e = "Experimental", # Experimental group label
       lab.c = "Control", # Control group label
       text.random = "Random-effects model",
       comb.fixed = FALSE,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE,
       digits = 2,
       col.square = "grey",
       col.diamond = "grey",
       col.predict = "darkred"
)

# Creating a plot to complete graphical analysis of influential cases using the
# formulas described in the paper by Baujat, et al. where the X axis represents
# the contribution of the trial to the Cochran Q-test for heterogeneity
# x_hat_i = (theta_hat_i - theta_hat)^2 / sigma_hat_i^2
# where theta_hat_i refers to the effect size of study i, 
# theta_hat refers to the overall pooled
# effect size and sigma_hat_i refers to the variance of study i
# AND
# the Y axis represents the influence of the trial on the overall treatment effect
# y_hat_i = (theta_hat_minus_i - theta_hat)^2 / sigma_hat_minus_i^2

baujat(meta_analysis)

# Creating a new meta-analysis with the omitted studies -----------------------

data_after_omissions <- data.frame(
  Study = c("Shin et al, 2008", "Nickels et al, 2022", 
            "Eigendorf et al, 2019", "Ribeiro et al, 2021A", "Mason et al, 2013", 
            "Ribeiro et al, 2021B", "Noorimofrad et al, 2018", "Werner et al, 2019B", 
            "Werner et al, 2019C", "Werner et al, 2019A"),
  n1 = c(8, 11, 146, 28, 116, 29, 15, 29, 34, 26),
  m1 = c(7.67, 0.70, 1.12, 1.45, 1.02, 1.54, 1.60, 5.50, 5.50, 5.70),
  sd1 = c(1.74, 0.17, 0.25, 0.46, 0.19, 0.51, 0.20, 1.20, 0.80, 1.40),
  n2 = c(8, 11, 145, 15, 86, 15, 15, 12, 13, 10),
  m2 = c(7.99, 0.81, 1.12, 1.45, 1.01, 1.45, 1.45, 5.20, 5.20, 5.20),
  sd2 = c(2.60, 0.14, 0.23, 0.46, 0.17, 0.46, 0.15, 0.92, 0.92, 0.92),
  Subgroup = c("Resistance training", "Resistance training", "Resistance training",
               "Aerobic exercise", "Aerobic exercise", "High-intensity interval training",
               "High-intensity interval training", "High-intensity interval training",
               "Resistance training", "Aerobic exercise")
)

# Performing meta-analysis on subgroups
sub_group_analysis <- metacont(
  n.e = n1, mean.e = m1, sd.e = sd1, 
  n.c = n2, mean.c = m2, sd.c = sd2, 
  studlab = Study, 
  data = data_after_omissions, 
  fixed = FALSE, 
  method.tau = "REML",
  byvar = Subgroup
)
#?forest.meta

# Creating the forest plot with subgroups
forest(sub_group_analysis, 
       xlab = "Mean Difference",
       col.by = "black",
       col.square = "green",
       col.diamond = "black",
       col.predict = "black",
       digits.pval = 2,
       digits.sd = 2,
       xlim = c(-0.5, 0.5),
       at = seq(-0.5, 0.5, by = 0.25),
       test.overall.random = TRUE,
       test.effect.subgroup.random = TRUE
       )

# Detecting publication bias with funnel plot ----------------------------------
res <- rma(m1i = m1, sd1i = sd1, n1i = n1,
           m2i = m2, sd2i = sd2, n2i = n2,
           measure = "MD", method = "REML",
           test = "knha",
           data = data_after_omissions)

funnel(res, level=c(95, 99), refline=0, legend=TRUE,
       shade = c("white", "gray90", "gray70"))

# ------------------------------------------------------------------------------
# EXTENSIONS  ------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Getting the CHANGES between pre- and post-intervention and storing in variables

# SHIN-----------------------------------------
r <- 0.5
shin_M_Pre_E <- 0.9962
shin_M_Post_E <- 0.9959
shin_SD_Pre_E <- 0.0159
shin_SD_Post_E <- 0.0146
shin_M_Pre_C <- 1.0135
shin_M_Post_C <- 0.9991
shin_SD_Pre_C <- 0.1277
shin_SD_Post_C <- 0.0075

shin_MC_E <- shin_M_Post_E - shin_M_Pre_E
shin_MC_C <- shin_M_Post_C - shin_M_Pre_C

shin_SDC_E <- sqrt(shin_SD_Pre_E^2 + shin_SD_Post_E^2 - 2 * r * shin_SD_Pre_E * shin_SD_Post_E)
shin_SDC_C <- sqrt(shin_SD_Pre_C^2 + shin_SD_Post_C^2 - 2 * r * shin_SD_Pre_C * shin_SD_Post_C)

print(shin_MC_E)
print(shin_SDC_E)
print(shin_MC_C)
print(shin_SDC_C)

# NICKELS --------------------------------------
nickels_M_Pre_E <- 0.712
nickels_M_Post_E <- 0.699
nickels_SD_Pre_E <- 0.816
nickels_SD_Post_E <- 0.169
nickels_M_Pre_C <- 0.792
nickels_M_Post_C <- 0.810
nickels_SD_Pre_C <- 0.132
nickels_SD_Post_C <- 0.138

nickels_MC_E <- nickels_M_Post_E - nickels_M_Pre_E
nickels_MC_C <- nickels_M_Post_C - nickels_M_Pre_C

nickels_SDC_E <- sqrt(nickels_SD_Pre_E^2 + nickels_SD_Post_E^2 - 2 * r * nickels_SD_Pre_E * nickels_SD_Post_E)
nickels_SDC_C <- sqrt(nickels_SD_Pre_C^2 + nickels_SD_Post_C^2 - 2 * r * nickels_SD_Pre_C * nickels_SD_Post_C)

print(nickels_MC_E)
print(nickels_SDC_E)
print(nickels_MC_C)
print(nickels_SDC_C)

# FRIEDENREICH --------------------------------------
friedenreich_pre_E_CI_upper <- 1.34
friedenreich_pre_E_CI_lower <- 0.88
friedenreich_post_E_CI_upper <- 1.17
friedenreich_post_E_CI_lower <- 0.77
friedenreich_pre_C_CI_upper <- 1.36
friedenreich_pre_C_CI_lower <- 0.88
friedenreich_post_C_CI_upper <- 1.28
friedenreich_post_C_CI_lower <- 0.78

friedenreich_SE_pre_C <- 
  (friedenreich_pre_C_CI_upper - friedenreich_pre_C_CI_lower) / (2*1.96)
friedenreich_SE_pre_E <- 
  (friedenreich_pre_E_CI_upper - friedenreich_pre_E_CI_lower) / (2*1.96)
friedenreich_SE_post_C <- 
  (friedenreich_post_C_CI_upper - friedenreich_post_C_CI_lower) / (2*1.96)
friedenreich_SE_post_E <- 
  (friedenreich_post_E_CI_upper - friedenreich_post_E_CI_lower) / (2*1.96)

friedenreich_M_Pre_E <- 1.08
friedenreich_M_Post_E <- 0.95 
friedenreich_SD_Pre_E <- friedenreich_SE_pre_E/sqrt(friedenreich_n)
friedenreich_SD_Post_E <- friedenreich_SE_post_E/sqrt(friedenreich_n)
friedenreich_M_Pre_C <- 1.10
friedenreich_M_Post_C <- 1.00
friedenreich_SD_Pre_C <- friedenreich_SE_pre_C/sqrt(friedenreich_n)
friedenreich_SD_Post_C <- friedenreich_SE_post_C/sqrt(friedenreich_n)
  
friedenreich_MC_E <- friedenreich_M_Post_E - friedenreich_M_Pre_E
friedenreich_MC_C <- friedenreich_M_Post_C - friedenreich_M_Pre_C

friedenreich_SDC_E <- sqrt(friedenreich_SD_Pre_E^2 + friedenreich_SD_Post_E^2 - 2 * r * friedenreich_SD_Pre_E * friedenreich_SD_Post_E)
friedenreich_SDC_C <- sqrt(friedenreich_SD_Pre_C^2 + friedenreich_SD_Post_C^2 - 2 * r * friedenreich_SD_Pre_C * friedenreich_SD_Post_C)

print(friedenreich_MC_E)
print(friedenreich_SDC_E)
print(friedenreich_MC_C)
print(friedenreich_SDC_C)


# EIGENDORF --------------------------------------
eigendorf_MC_E <- 0.058
eigendorf_MC_C <- 0.02

eigendorf_SDC_E <- 0.044
eigendorf_SDC_C <- 0.06


# RIBEIRO --------------------------------

# pooled means and SDs of IAT and CAT groups (both are aerobic exercise)
ribeiro_M_Pre_E <- ((28 * 1.43) + (29 * 1.53)) / (28+29)
ribeiro_M_Post_E <- ((28 * 1.45) + (29 * 1.54)) / (28+29)
ribeiro_SD_Pre_E <- sqrt(((28-1)*0.39^2 + (29-1)*0.46^2)/(28+29-2))
ribeiro_SD_Post_E <- sqrt(((28-1)*0.46^2 + (29-1)*0.51^2)/(28+29-2))
ribeiro_M_Pre_C <- 1.40
ribeiro_M_Post_C <- 1.45
ribeiro_SD_Pre_C <- 0.50
ribeiro_SD_Post_C <- 0.46

ribeiro_MC_E <- ribeiro_M_Post_E - ribeiro_M_Pre_E
ribeiro_MC_C <- ribeiro_M_Post_C - ribeiro_M_Pre_C

ribeiro_SDC_E <- sqrt(ribeiro_SD_Pre_E^2 + ribeiro_SD_Post_E^2 - 2 * r * ribeiro_SD_Pre_E * ribeiro_SD_Post_E)
ribeiro_SDC_C <- sqrt(ribeiro_SD_Pre_C^2 + ribeiro_SD_Post_C^2 - 2 * r * ribeiro_SD_Pre_C * ribeiro_SD_Post_C)

print(ribeiro_MC_E)
print(ribeiro_SDC_E)
print(ribeiro_MC_C)
print(ribeiro_SDC_C)

# MASON -----------------------------------
mason_M_Pre_E <- 1.027
mason_M_Post_E <- 1.025
mason_SD_Pre_E <- 0.213
mason_SD_Post_E <- 0.192
mason_M_Pre_C <- 1.042
mason_M_Post_C <- 1.015
mason_SD_Pre_C <- 0.197
mason_SD_Post_C <- 0.171
  
mason_MC_E <- mason_M_Post_E - mason_M_Pre_E
mason_MC_C <- mason_M_Post_C - mason_M_Pre_C

mason_SDC_E <- sqrt(mason_SD_Pre_E^2 + mason_SD_Post_E^2 - 2 * r * mason_SD_Pre_E * mason_SD_Post_E)
mason_SDC_C <- sqrt(mason_SD_Pre_C^2 + mason_SD_Post_C^2 - 2 * r * mason_SD_Pre_C * mason_SD_Post_C)

print(mason_MC_E)
print(mason_SDC_E)
print(mason_MC_C)
print(mason_SDC_C)

# NOORIMOFARD -----------------------------------
noorimofrad_M_Pre_E <- 1.43
noorimofrad_M_Post_E <- 1.60
noorimofrad_SD_Pre_E <- 0.20
noorimofrad_SD_Post_E <- 0.13
noorimofrad_M_Pre_C <- 1.47
noorimofrad_M_Post_C <- 1.45
noorimofrad_SD_Pre_C <- 0.2
noorimofrad_SD_Post_C <- 0.15
  
noorimofrad_MC_E <- noorimofrad_M_Post_E - noorimofrad_M_Pre_E
noorimofrad_MC_C <- noorimofrad_M_Post_C - noorimofrad_M_Pre_C

noorimofrad_SDC_E <- sqrt(noorimofrad_SD_Pre_E^2 + noorimofrad_SD_Post_E^2 - 2 * r * noorimofrad_SD_Pre_E * noorimofrad_SD_Post_E)
noorimofrad_SDC_C <- sqrt(noorimofrad_SD_Pre_C^2 + noorimofrad_SD_Post_C^2 - 2 * r * noorimofrad_SD_Pre_C * noorimofrad_SD_Post_C)

print(noorimofrad_MC_E)
print(noorimofrad_SDC_E)
print(noorimofrad_MC_C)
print(noorimofrad_SDC_C)

# WERNER A --------------------------------------
wernerA_M_Pre_E <- 0.48
wernerA_M_Post_E <- 0.65
wernerA_SD_Pre_E <- 0.32
wernerA_SD_Post_E <- 0.5

wernerA_MC_E <- wernerA_M_Post_E - wernerA_M_Pre_E

wernerA_SDC_E <- sqrt(wernerA_SD_Pre_E^2 + wernerA_SD_Post_E^2 - 2 * r * wernerA_SD_Pre_E * wernerA_SD_Post_E)

print(wernerA_MC_E)
print(wernerA_SDC_E)


# WERNER B --------------------------------------
wernerB_M_Pre_E <- 0.46
wernerB_M_Post_E <- 0.65
wernerB_SD_Pre_E <- 0.33
wernerB_SD_Post_E <- 0.48

wernerB_MC_E <- wernerB_M_Post_E - wernerB_M_Pre_E

wernerB_SDC_E <- sqrt(wernerB_SD_Pre_E^2 + wernerB_SD_Post_E^2 - 2 * r * wernerB_SD_Pre_E * wernerB_SD_Post_E)

print(wernerB_MC_E)
print(wernerB_SDC_E)


# WERNER C --------------------------------------
wernerC_M_Pre_E <- 0.54
wernerC_M_Post_E <- 0.55
wernerC_SD_Pre_E <- 0.6
wernerC_SD_Post_E <- 0.6

wernerC_MC_E <- wernerC_M_Post_E - wernerC_M_Pre_E

wernerC_SDC_E <- sqrt(wernerC_SD_Pre_E^2 + wernerC_SD_Post_E^2 - 2 * r * wernerC_SD_Pre_E * wernerC_SD_Post_E)

print(wernerC_MC_E)
print(wernerC_SDC_E)


# WERNER ALL THREE ------------------------------
werner_M_Pre_C <- 0.49
werner_M_Post_C <- 0.5
werner_SD_Pre_C <- 0.55
werner_SD_Post_C <- 0.5


werner_MC_C <- werner_M_Post_C - werner_M_Pre_C

werner_SDC_C <- sqrt(werner_SD_Pre_C^2 + werner_SD_Post_C^2 - 2 * r * werner_SD_Pre_C * werner_SD_Post_C)

print(werner_MC_C)
print(werner_SDC_C)

pooled_M_Pre_E <- 
  ((26 * wernerA_M_Pre_E) + (29 * wernerB_M_Pre_E) + (34 * wernerC_M_Pre_E)) / (26 + 29 + 34)
pooled_M_Post_E <-
  ((26 * wernerA_M_Post_E) + (29 * wernerB_M_Post_E) + (34 * wernerC_M_Post_E)) / (26 + 29 + 34)
pooled_SD_Pre_E <-
  sqrt(((26 - 1) * wernerA_SD_Pre_E^2 + (29 - 1) * wernerB_SD_Pre_E^2 + (34 - 1) * wernerC_SD_Pre_E^2) / (26 + 29 + 34 - 3))
pooled_SD_Post_E <-
  sqrt(((26 - 1) * wernerA_SD_Post_E^2 + (29 - 1) * wernerB_SD_Post_E^2 + (34 - 1) * wernerC_SD_Post_E^2) / (26 + 29 + 34 - 3))

werner_pooled_MC_E <- pooled_M_Post_E - pooled_M_Pre_E
werner_pooled_SD_E <- sqrt(pooled_SD_Pre_E^2 + pooled_SD_Post_E^2 - 2 * r * pooled_SD_Pre_E * pooled_SD_Post_E)

print(werner_pooled_MC_E)
print(werner_pooled_SD_E)

# SANCHEZ - GONZALEZ ---------------------------
sanchez_M_Pre_E <- 2.30
sanchez_M_Post_E <- 4.81
sanchez_SD_Pre_E <- 2.413
sanchez_SD_Post_E <- 3.16
sanchez_M_Pre_C <- 3.88
sanchez_M_Post_C <- 1.81
sanchez_SD_Pre_C <- 6.030
sanchez_SD_Post_C <- 2.34
  
sanchez_MC_E <- sanchez_M_Post_E - sanchez_M_Pre_E
sanchez_MC_C <- sanchez_M_Post_C - sanchez_M_Pre_C

sanchez_SDC_E <- sqrt(sanchez_SD_Pre_E^2 + sanchez_SD_Post_E^2 - 2 * r * sanchez_SD_Pre_E * sanchez_SD_Post_E)
sanchez_SDC_C <- sqrt(sanchez_SD_Pre_C^2 + sanchez_SD_Post_C^2 - 2 * r * sanchez_SD_Pre_C * sanchez_SD_Post_C)

print(sanchez_MC_E)
print(sanchez_SDC_E)
print(sanchez_MC_C)
print(sanchez_SDC_C)

# New meta analysis (using pre- to post-intervention changes) ------------------
# Data frame
MCdata <- data.frame(
  Study = c("Shin et al, 2008", "Nickels et al, 2022", "Friedenreich et al, 2018", 
            "Eigendorf et al, 2019", "Ribeiro et al, 2021", "Mason et al, 2013",
            "Noorimofrad et al, 2018", "Werner et al, 2019"),
  n1 = c(8, 11, 99, 146, 57, 106, 15, 89),
  m1 = c(shin_MC_E, nickels_MC_E, friedenreich_MC_E, eigendorf_MC_E,
         ribeiro_MC_E, mason_MC_E, noorimofrad_MC_E, werner_pooled_MC_E),
  sd1 = c(shin_SDC_E, nickels_SDC_E, friedenreich_SDC_E, eigendorf_SDC_E,
          ribeiro_SDC_E, mason_SDC_E, noorimofrad_SDC_E, werner_pooled_SD_E),
  n2 = c(8, 11, 113, 145, 30, 79, 15, 35),
  m2 = c(shin_MC_C, nickels_MC_C, friedenreich_MC_C, eigendorf_MC_C,
         ribeiro_MC_C, mason_MC_C, noorimofrad_MC_C, werner_MC_C),
  sd2 = c(shin_SDC_C, nickels_SDC_C, friedenreich_SDC_C, eigendorf_SDC_C,
          ribeiro_SDC_C, mason_SDC_C, noorimofrad_SDC_C, werner_SDC_C)
)

# Meta analysis
MCmeta_analysis <- metacont(
  n.e = MCdata$n1, mean.e = MCdata$m1, sd.e = MCdata$sd1, 
  n.c = MCdata$n2, mean.c = MCdata$m2, sd.c = MCdata$sd2, 
  studlab = MCdata$Study, 
  data = MCdata, 
  fixed = FALSE,
  sm = "MD",
  method.tau = "REML",
  prediction = TRUE
)

# Forest plot
forest(MCmeta_analysis,
       leftlabs = c("Study", "Experimental Mean Change (SD)", "Control Mean Change (SD)"),
       rightlabs = c("MD", "95% CI", "Weight (%)"),
       lab.e = "Experimental",
       lab.c = "Control",
       text.random = "Random-effects model",
       text.predict = "Prediction interval",
       comb.fixed = FALSE,
       print.tau2 = TRUE,
       print.I2 = TRUE,
       print.pval.Q = TRUE,
       digits = 2,
       col.square = "grey", 
       col.diamond = "grey",
       col.predict = "darkred"
)

# New sub group analysis ----------------------------------------------------
MCdataSub <- data.frame(
  Study = c("Shin et al, 2008", "Nickels et al, 2022", "Friedenreich et al, 2018", 
            "Eigendorf et al, 2019", "Ribeiro et al, 2021", "Mason et al, 2013",
            "Noorimofrad et al, 2018", "Werner et al, 2019A", "Werner et al, 2019B",
            "Werner et al, 2019C"),
  n1 = c(8, 11, 99, 146, 57, 106, 15, 26, 29, 34),
  m1 = c(shin_MC_E, nickels_MC_E, friedenreich_MC_E, eigendorf_MC_E,
         ribeiro_MC_E, mason_MC_E, noorimofrad_MC_E, wernerA_MC_E,
         wernerB_MC_E, wernerC_MC_E),
  sd1 = c(shin_SDC_E, nickels_SDC_E, friedenreich_SDC_E, eigendorf_SDC_E,
          ribeiro_SDC_E, mason_SDC_E, noorimofrad_SDC_E, wernerA_SDC_E,
          wernerB_SDC_E, wernerC_SDC_E),
  n2 = c(8, 11, 113, 145, 30, 79, 15, 12, 11, 12),
  m2 = c(shin_MC_C, nickels_MC_C, friedenreich_MC_C, eigendorf_MC_C,
         ribeiro_MC_C, mason_MC_C, noorimofrad_MC_C, werner_MC_C, werner_MC_C, werner_MC_C),
  sd2 = c(shin_SDC_C, nickels_SDC_C, friedenreich_SDC_C, eigendorf_SDC_C,
          ribeiro_SDC_C, mason_SDC_C, noorimofrad_SDC_C, werner_SDC_C, werner_SDC_C, werner_SDC_C),
  Subgroup = c("Resistance training", "Resistance training","Aerobic exercise",
               "Resistance training", "Aerobic exercise", "Aerobic exercise",
               "High-intensity interval training", "Aerobic exercise", 
               "High-intensity interval training", "Resistance training")
  )


# Performing meta-analysis on subgroups
MCsub_group_analysis <- metacont(
  n.e = n1, mean.e = m1, sd.e = sd1, 
  n.c = n2, mean.c = m2, sd.c = sd2, 
  studlab = Study, 
  data = MCdataSub, 
  fixed = FALSE, 
  method.tau = "REML",
  byvar = Subgroup,
  prediction = TRUE
)

# Creating the forest plot with subgroups
forest(MCsub_group_analysis, 
       xlab = "Mean Difference",
       col.by = "black",
       col.square = "green",
       col.diamond = "black",
       col.predict = "black",
       digits.pval = 2,
       digits.sd = 2,
       xlim = c(-0.5, 0.5),
       at = seq(-0.5, 0.5, by = 0.25),
       test.overall.random = TRUE,
       test.effect.subgroup.random = TRUE,
       print.predict = TRUE
)

# Funnel plot ---------------------------------------------------------------
# Detecting publication bias with funnel plot---------------
HIITdata <- data.frame(
  Study = c("Noorimofrad et al, 2018", "Werner et al, 2019B"),
  n1 = c(15, 29),
  m1 = c(noorimofrad_MC_E, wernerB_MC_E),
  sd1 = c(noorimofrad_SDC_E, wernerB_SDC_E),
  n2 = c(15, 11),
  m2 = c(noorimofrad_MC_C, werner_MC_C),
  sd2 = c(noorimofrad_SDC_C, werner_SDC_C)
)

res <- rma(m1i = m1, sd1i = sd1, n1i = n1,
           m2i = m2, sd2i = sd2, n2i = n2,
           measure = "MD", method = "REML",
           data = HIITdata)

funnel(res, level=c(95, 99), refline=0, legend=TRUE,
       shade = c("white", "gray90", "gray70"))

# Meta regression using study durations, ages, subgroups -------------------

# Subdivided data
MCregression_subbed <- data.frame(
  Study = c("Shin et al, 2008", "Nickels et al, 2022", "Friedenreich et al, 2018", 
            "Eigendorf et al, 2019", "Ribeiro et al, 2021", "Mason et al, 2013",
            "Noorimofrad et al, 2018", "Werner et al, 2019A", "Werner et al, 2019B",
            "Werner et al, 2019C"),
  n1 = c(8, 11, 99, 146, 57, 106, 15, 26, 29, 34),
  m1 = c(shin_MC_E, nickels_MC_E, friedenreich_MC_E, eigendorf_MC_E,
         ribeiro_MC_E, mason_MC_E, noorimofrad_MC_E, wernerA_MC_E,
         wernerB_MC_E, wernerC_MC_E),
  sd1 = c(shin_SDC_E, nickels_SDC_E, friedenreich_SDC_E, eigendorf_SDC_E,
          ribeiro_SDC_E, mason_SDC_E, noorimofrad_SDC_E, wernerA_SDC_E,
          wernerB_SDC_E, wernerC_SDC_E),
  n2 = c(8, 11, 113, 145, 30, 79, 15, 12, 11, 12),
  m2 = c(shin_MC_C, nickels_MC_C, friedenreich_MC_C, eigendorf_MC_C,
         ribeiro_MC_C, mason_MC_C, noorimofrad_MC_C, werner_MC_C, werner_MC_C, werner_MC_C),
  sd2 = c(shin_SDC_C, nickels_SDC_C, friedenreich_SDC_C, eigendorf_SDC_C,
          ribeiro_SDC_C, mason_SDC_C, noorimofrad_SDC_C, werner_SDC_C, werner_SDC_C, werner_SDC_C),
  Subgroup = c("Resistance training", "Resistance training","Aerobic exercise",
               "Resistance training", "Aerobic exercise", "Aerobic exercise",
               "High-intensity interval training", "Aerobic exercise", 
               "High-intensity interval training", "Resistance training"),
  age = c(46.81, 50.5, 60.2, 52.9, 29, 58, 20.13, 49.05, 49.05, 49.05),
  BMI = c(28.14, 26.15, 28.6, 25.8, 28.7, 30.9, 22.75, 24.325, 49.05, 49.05)
)

# Non subdivided data
  MCregression_whole <- data.frame(
    Study = c("Shin et al, 2008", "Nickels et al, 2022", "Friedenreich et al, 2018", 
              "Eigendorf et al, 2019", "Ribeiro et al, 2021", "Mason et al, 2013",
              "Noorimofrad et al, 2018", "Werner et al, 2019"),
    n1 = c(8, 11, 99, 146, 57, 106, 15, 89),
    m1 = c(shin_MC_E, nickels_MC_E, friedenreich_MC_E, eigendorf_MC_E,
           ribeiro_MC_E, mason_MC_E, noorimofrad_MC_E, werner_pooled_MC_E),
    sd1 = c(shin_SDC_E, nickels_SDC_E, friedenreich_SDC_E, eigendorf_SDC_E,
            ribeiro_SDC_E, mason_SDC_E, noorimofrad_SDC_E, werner_pooled_SD_E),
    n2 = c(8, 11, 113, 145, 30, 79, 15, 35),
    m2 = c(shin_MC_C, nickels_MC_C, friedenreich_MC_C, eigendorf_MC_C,
           ribeiro_MC_C, mason_MC_C, noorimofrad_MC_C, werner_MC_C),
    sd2 = c(shin_SDC_C, nickels_SDC_C, friedenreich_SDC_C, eigendorf_SDC_C,
            ribeiro_SDC_C, mason_SDC_C, noorimofrad_SDC_C, werner_SDC_C),
  Durations = c(26, 52, 52, 26, 16, 52, 8, 26),  
  age = c(46.81, 50.5, 60.2, 52.9, 29, 58, 20.13, 49.05),
  BMI = c(28.14, 26.15, 28.6, 25.8, 28.7, 30.9, 22.75, 24.325)
)
  
# Meta-regression with subgroup as moderator
sub_meta_reg <- rma(m1i = m1, sd1i = sd1, n1i = n1,
                    m2i = m2, sd2i = sd2, n2i = n2,
                    measure = "MD", 
                    mods = ~ Subgroup,
                    method = "REML",
                    data = MCregression_subbed)

summary(sub_meta_reg)

# Meta-regression with age or intervention duration as moderators
whole_meta_reg <- rma(m1i = m1, sd1i = sd1, n1i = n1,
                    m2i = m2, sd2i = sd2, n2i = n2,
                    measure = "MD", 
                    mods = ~ age,
                    method = "REML",
                    data = MCregression_whole)

summary(whole_meta_reg)