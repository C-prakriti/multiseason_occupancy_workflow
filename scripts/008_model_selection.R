#---------------------------------------------------------------------------------------------
#Project: Multi-season occupancy analysis
#Purpose: To build candidate models and select the best fitted model for occupancy analysis
#------------------------------------------------------------------------------------------------
# Load necessary packages
library(unmarked)
library(AICcmodavg)
library(ggplot2)

# Load necessary files
umf <- readRDS("D:/R_projects/multiseason_occupancy/output/umf_object.rds")
dyn_cov <- read.csv("D:/R_projects/multiseason_occupancy/output/dynamic_covariates.csv")

#-----------------------------------------------------------------------------------------------------------------
# 0. Pre-modeling checks
#-------------------------------------------------------------------------------------------------------------------------
# Check for mising values
sum(is.na(siteCovs(umf)))
sum(is.na(yearlySiteCovs(umf)))

# Create convergence check helper function
check_model <- function(model, model_name){
  if(model@opt$convergence != 0){
    warning(paste(model_name, "Failed to converge"))
  }
}

#----------------------------------------------------------------------------------------------------------------------
# 1. Fit null model
#----------------------------------------------------------------------------------------------------------------------
m0 <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~1,
  data = umf
)

m0
summary(umf)

#-----------------------------------------------------------------------------------------------------------------
# 2. Detection Model Selection
#--------------------------------------------------------------------------------------------------------------------
# 2.1 Building models
# 2.1.1 Effort only model
det_eff <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~effort,
  data = umf
)
check_model(det_eff, "det_eff")

# 2.1.2 Year only model
det_year <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)
check_model(det_year, "det_year")

# 2.1.3 Effort + Year
det_eff_year <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~effort + year,
  data = umf
)
check_model(det_eff_year, "det_eff_year")

# 2.2 Select the best model
# 2.2.1 Compare the models
det_fits <- fitList(
  null = m0,
  effort = det_eff,
  year = det_year,
  effort_year = det_eff_year
)

# 2.2.2 Select the supported model
det_aic <- modSel(det_fits)
det_aic
det_table <- det_aic@Full

# Considering Year is our best model for detection
best_det_mod <- det_year

#-------------------------------------------------------------------------------------------------------------
# 3. Occupancy Model Selection
#-------------------------------------------------------------------------------------------------------------
# 3.1 Build Occupancy Models
# 3.1.1 Null Model
occ_null <- colext(psiformula = ~1, gammaformula = ~1, epsilonformula = ~1,pformula = ~year, data = umf)
occ_tri <- colext(psiformula = ~tri_z,gammaformula = ~1,epsilonformula = ~1,pformula = ~year, data = umf)
check_model(occ_tri, "occ_tri")

# 3.2 Select the best model
# 3.2.1 Fit the model
occ_fits <- fitList(null = occ_null,TRI = occ_tri)

# 3.2.2 Select the best model
occ_aic <- modSel(occ_fits)
occ_table <- occ_aic@Full

# Assuming that occ_tri is our best occupancy model
best_occ_mod <- occ_tri

#-------------------------------------------------------------------------------------------------------------
# 4. Colonization Models Selection
#---------------------------------------------------------------------------------------------------------------
# 4.1 Build Models

col_null <- colext(psiformula = ~tri_z, gammaformula = ~1, epsilonformula = ~1, pformula = ~year,data = umf)
col_cover <- colext(psiformula = ~tri_z, gammaformula = ~fcover, epsilonformula = ~1, pformula = ~year, data = umf)
col_loss <- colext(psiformula = ~tri_z, gammaformula = ~floss, epsilonformula = ~1, pformula = ~year, data = umf)
col_ndvi <- colext(psiformula = ~tri_z, gammaformula = ~ndvi, epsilonformula = ~1, pformula = ~year, data = umf)
col_set <- colext(psiformula = ~tri_z, gammaformula = ~dist_set, epsilonformula = ~1, pformula = ~year, data = umf)

col_set_cover <- colext(psiformula = ~tri_z, gammaformula = ~dist_set + fcover, epsilonformula = ~1, pformula = ~year,data = umf)
col_cover_loss <- colext(psiformula = ~tri_z, gammaformula = ~fcover + floss, epsilonformula = ~1, pformula = ~year, data = umf)
col_loss_ndvi <- colext(psiformula = ~tri_z, gammaformula = ~floss + ndvi, epsilonformula = ~1, pformula = ~year, data = umf)
col_ndvi_set <- colext(psiformula = ~tri_z, gammaformula = ~dist_set + ndvi, epsilonformula = ~1, pformula = ~year, data = umf)
col_set_loss <- colext(psiformula = ~tri_z, gammaformula = ~dist_set + floss, epsilonformula = ~1, pformula = ~year, data = umf)

col_set_cover_loss <- colext(psiformula = ~tri_z, gammaformula = ~dist_set + fcover + floss, epsilonformula = ~1, pformula = ~year,data = umf)
col_set_loss_ndvi <- colext(psiformula = ~tri_z,gammaformula = ~dist_set + floss + ndvi,epsilonformula = ~1,pformula = ~year, data = umf)
# Since our forest cover and ndvi were highly correlated they were not kept in the same model.

# Check for model fit
for (nm in c("col_null", "col_cover", "col_loss", "col_ndvi", "col_set", "col_set_cover", "col_cover_loss", "col_loss_ndvi",
             "col_ndvi_set", "col_set_loss", "col_set_cover_loss", "col_set_loss_ndvi"))
  check_model(get(nm), nm)

# 4.2 Select the best model
# 4.2.1 Fit the model
col_fits <- fitList(
  null = col_null,
  settlement = col_set,
  cover = col_cover,
  loss = col_loss,
  ndvi = col_ndvi,
  settlement_cover = col_set_cover,
  settlement_loss = col_set_loss,
  settlement_ndvi = col_ndvi_set,
  cover_loss = col_cover_loss,
  loss_ndvi = col_loss_ndvi,
  settlement_cover_loss = col_set_cover_loss,
  settlement_loss_ndvi = col_set_loss_ndvi
)

# 4.2.2 Select the best model
col_aic <- modSel(col_fits)
col_table <- col_aic@Full

# Assuming that col_loss_ndvi is the best supported models among them
#-----------------------------------------------------------------------------------------------------------------
# 5. Extinction Model Selection
#------------------------------------------------------------------------------------------------------------------------
# 5.1 Build Models
# 5.1.1 Null model
ext_null <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~1,pformula = ~year,data = umf)

ext_cover <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~fcover,pformula = ~year,data = umf)
ext_loss <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~floss,pformula = ~year, data = umf)
ext_ndvi <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~ndvi,pformula = ~year,data = umf)
ext_set <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~dist_set,pformula = ~year,data = umf)
ext_set_cover <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~dist_set + fcover,pformula = ~year,data = umf)
ext_cover_loss <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~fcover + floss,pformula = ~year,data = umf)
ext_loss_ndvi <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~floss + ndvi,pformula = ~year,data = umf)
ext_ndvi_set <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~ndvi + dist_set,pformula = ~year,data = umf)
ext_set_loss <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~dist_set + floss,pformula = ~year,data = umf)

ext_set_cover_loss <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~dist_set + fcover + floss,pformula = ~year,data = umf)
ext_set_loss_ndvi <- colext(psiformula = ~tri_z,gammaformula = ~floss + ndvi,epsilonformula = ~dist_set + floss + ndvi,pformula = ~year,data = umf)

# Since our forest cover and ndvi were highly correlated they were not kept in the same model.

# 4.2 Select the best model
# 4.2.1 Fit the model
ext_fits <- fitList(
  null = ext_null,
  settlement = ext_set,
  cover = ext_cover,
  loss = ext_loss,
  ndvi = ext_ndvi,
  settlement_cover = ext_set_cover,
  settlement_loss = ext_set_loss,
  settlement_ndvi = ext_ndvi_set,
  cover_loss = ext_cover_loss,
  loss_ndvi = ext_loss_ndvi,
  settlement_cover_loss = ext_set_cover_loss,
  settlement_loss_ndvi = ext_set_loss_ndvi
)

# 4.2.2 Select the best model
ext_aic <- modSel(ext_fits)
ext_table <- ext_aic@Full

# Assuming that ext_set is the best candidate model 

#--------------------------------------------------------------------------------------------------------------
# 5. Create global model for goodness of fit test
#--------------------------------------------------------------------------------------------------------------
gof_mod <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set,
  pformula = ~year,
  data = umf
)

gof_mod

#------------------------------------------------------------------------------------------------------------------
# 6. Goodness of Fit test / Parametric Bootstrap
#-----------------------------------------------------------------------------------------------------------------
# 6.1 Calculate Goodness of Fit
# Define fit statistics
fitstats <- function(fm) {
  observed <- getY(fm@data)
  expected <- fitted(fm)
  resids   <- residuals(fm)
  
  SSE   <- sum(resids^2, na.rm = TRUE)
  chisq <- sum((observed - expected)^2 / (expected + 1e-6), na.rm = TRUE)
  c(SSE = SSE, Chisq = chisq)
}

set.seed(123)   # reproducibility
pb_gof <- parboot(gof_mod, fitstats, nsim = 1000, report = TRUE)

print(pb_gof)

# c.hat from chi-square ratio
c_hat_pb  <- as.numeric(pb_gof@t0["Chisq"] / mean(pb_gof@t.star[, "Chisq"]))
pb_pvalue <- mean(pb_gof@t.star[, "Chisq"] >= pb_gof@t0["Chisq"])

cat("\n--- Parametric Bootstrap GOF ---\n")
cat("Observed chi-square :", round(pb_gof@t0["Chisq"], 3), "\n")
cat("Mean simulated chisq:", round(mean(pb_gof@t.star[,"Chisq"]), 3), "\n")
cat("p-value             :", round(pb_pvalue, 4), "\n")
cat("c-hat               :", round(c_hat_pb,  4), "\n")

# Retain mb.gof.test as a secondary reference (acknowledge limitation in paper)
set.seed(123)
gof_mb <- mb.gof.test(gof_mod, nsim = 20)   #Run at least 1000 replications
cat("\n--- MacKenzie-Bailey GOF (secondary, single-season approximation) ---\n")
print(gof_mb)

# Use parametric bootstrap c.hat going forward
chat <- c_hat_pb
if (chat < 1) {
  message("c.hat < 1: no evidence of overdispersion; set to 1 for QAICc.")
  chat <- 1
}
cat("Final c.hat used for QAICc:", round(chat, 4), "\n")

# Save GOF results
gof_results <- data.frame(
  Method     = c("Parametric Bootstrap", "MacKenzie-Bailey (secondary)"),
  ChiSquare  = c(round(pb_gof@t0["Chisq"], 3), round(mean(gof_mb$chi.square), 3)),
  P_value    = c(round(pb_pvalue, 4),           round(mean(gof_mb$p.value), 4)),
  C_hat      = c(round(c_hat_pb, 4),            round(mean(gof_mb$c.hat.est), 4))
)
gof_results

write.csv(gof_results,"D:/R_projects/multiseason_occupancy/output/gof_results",row.names = FALSE)

# In our demo data, bootstrap goodness-of-fit test indicated weak model fit (p = 0.397) and only weak overdispersion (c-hat = 1.0112), so we will be proceeding using QAICc.

# Since Colonization and Extinction models have multiple supported models, we will perform model averaged prediction on the colonization parameter

#---------------------------------------------------------------------------------------------------------------------------------------------------
# 7. QAICc model selection by applying to all candidate sets using chat from the gloabl model
#--------------------------------------------------------------------------------------------------------------------------------
# --- 7.1 Detection -----------------------------------------------------------
det_mod_list  <- list(m0, det_eff, det_year, det_eff_year)
det_mod_names <- c("Null", "Effort", "Year", "Effort + Year")

det_qaicc <- aictab(cand.set = det_mod_list, modnames = det_mod_names, c.hat = chat)
print(det_qaicc)

# --- 7.2 Initial occupancy ---------------------------------------------------
occ_mod_list  <- list(occ_null, occ_tri)
occ_mod_names <- c("Null","TRI")

occ_qaicc <- aictab(cand.set = occ_mod_list, modnames = occ_mod_names, c.hat = chat)
print(occ_qaicc)

# --- 7.3 Colonization --------------------------------------------------------
col_mod_list <- list(col_null, col_loss, col_cover, col_ndvi, col_set,
                     col_set_loss, col_set_cover, col_ndvi_set,
                     col_cover_loss, col_loss_ndvi, col_set_cover_loss, col_set_loss_ndvi)
col_mod_names <- c("Null","Loss","Cover","NDVI","Settlement",
                   "Settlement + Loss","Settlement + Cover","Settlement + NDVI",
                   "Cover + Loss","Loss + NDVI","Loss + Cover + Settlement","Loss + NDVI + Settlement")

col_qaicc <- aictab(cand.set = col_mod_list, modnames = col_mod_names, c.hat = chat)
print(col_qaicc)

# --- 7.4 Extinction ----------------------------------------------------------
ext_mod_list <- list(ext_null, ext_cover, ext_loss, ext_set, ext_ndvi,
                     ext_cover_loss, ext_set_cover, ext_set_loss, ext_loss_ndvi,
                     ext_ndvi_set, ext_set_cover_loss, ext_set_loss_ndvi)
ext_mod_names <- c("Null","Cover","Loss","Settlement","NDVI",
                   "Cover + Loss","Cover + Settlement","Loss + Settlement",
                   "Loss + NDVI","Settlement + NDVI",
                   "Cover + Loss + Settlement","Loss + Settlement + NDVI")

ext_qaicc <- aictab(cand.set = ext_mod_list, modnames = ext_mod_names, c.hat = chat)
print(ext_qaicc)

# --- 7.5 Supported models (delta QAICc <= 2) -------------------------------------
occ_qaicc_df <- as.data.frame(occ_qaicc)
col_qaicc_df <- as.data.frame(col_qaicc)
ext_qaicc_df <- as.data.frame(ext_qaicc)
det_qaicc_df <- as.data.frame(det_qaicc)

supported_occ <- subset(occ_qaicc_df, Delta_QAICc <= 2)
supported_col <- subset(col_qaicc_df, Delta_QAICc <= 2)
supported_ext <- subset(ext_qaicc_df, Delta_QAICc <= 2)
supported_det <- subset(det_qaicc_df, Delta_QAICc <= 2)

supported_occ
supported_col

cat("\nSupported occupancy models    :", nrow(supported_occ), "\n")
cat("Supported colonization models :", nrow(supported_col), "\n")
cat("Supported extinction models   :",
    nrow(supported_ext), "\n")
cat("Supported detection models    :", nrow(supported_det), "\n")

if (all(c(nrow(supported_occ), nrow(supported_col),
          nrow(supported_ext), nrow(supported_det)) == 1)) {
  message("Single best model adequate — model averaging not required.")
} else {
  message("Multiple supported models — model averaging will be applied.")
}

#-------------------------------------------------------------------------------
# 8. Model Averaged Predictions
#---------------------------------------------------------------------------------
# 8.1 Create Burnham & Anderson unconditional variance helper funtion
ba_var <- function(pred_mat, se_mat, weights) {
  avg_pred <- as.vector(pred_mat %*% weights)
  n        <- nrow(pred_mat)
  variance <- numeric(n)
  for (i in seq_len(n)) {
    variance[i] <- sum(
      weights * (se_mat[i, ]^2 + (pred_mat[i, ] - avg_pred[i])^2)
    )
  }
  list(avg = avg_pred, se = sqrt(variance))
}

# 8.2 Extract Colonization weights ----------------------------------------------------
col_avg_mods  <- col_mod_list
col_avg_names <- col_mod_names

col_avg_aicc <- aictab(
  cand.set = col_avg_mods,
  modnames = col_avg_names,
  c.hat    = chat
)

col_avg_wt <- col_avg_aicc$QAICcWt[
  match(col_avg_names, col_avg_aicc$Modnames)
]

cat("\nColonization model weights (sum =", round(sum(col_avg_wt), 4), "):\n")
print(data.frame(Model  = col_avg_names,
                 Weight = round(col_avg_wt, 4)))

# 8.3 Calculate Extinction weights -------------------------------------------------------
ext_avg_mods  <- ext_mod_list
ext_avg_names <- ext_mod_names

ext_avg_aicc <- aictab(
  cand.set = ext_avg_mods,
  modnames = ext_avg_names,
  c.hat    = chat
)

ext_avg_wt <- ext_avg_aicc$QAICcWt[
  match(ext_avg_names, ext_avg_aicc$Modnames)
]

cat("\nExtinction model weights (sum =", round(sum(ext_avg_wt), 4), "):\n")
print(data.frame(Model  = ext_avg_names,
                 Weight = round(ext_avg_wt, 4)))


# 8.3 Create yearly_means object where all parameters are constant at their means

yearly_means <- data.frame(
  dist_set = mean(yearlySiteCovs(umf)$dist_set, na.rm = TRUE),
  floss    = mean(yearlySiteCovs(umf)$floss,    na.rm = TRUE),
  fcover   = mean(yearlySiteCovs(umf)$fcover,   na.rm = TRUE),
  ndvi     = mean(yearlySiteCovs(umf)$ndvi,     na.rm = TRUE)
)

cat("\nYearly covariate means used as baseline for predictions:\n")
print(round(yearly_means, 4))


# 8.4 Colonization model averaged prediction for distance to settlement

# Vary dist_set across its full observed range
# Hold floss, fcover, ndvi at their mean values

col_dist_seq <- seq(
  min(yearlySiteCovs(umf)$dist_set, na.rm = TRUE),
  max(yearlySiteCovs(umf)$dist_set, na.rm = TRUE),
  length.out = 100
)

# Build newdata: 100 rows, each with all covariates at mean EXCEPT dist_set
col_set_newdat <- do.call(rbind, lapply(col_dist_seq, function(x) {
  d          <- yearly_means   # start from complete mean template
  d$dist_set <- x              # vary only dist_set
  d
}))

# Predict colonization from every model using complete newdata
col_set_pred_list <- lapply(col_avg_mods, function(m)
  predict(m, type = "col", newdata = col_set_newdat))

# Extract predicted values and SEs into matrices (100 rows x 12 models)
col_set_pred_mat <- sapply(col_set_pred_list, function(x) x$Predicted)
col_set_se_mat   <- sapply(col_set_pred_list, function(x) x$SE)

# Apply Burnham & Anderson model averaging
col_set_ba <- ba_var(col_set_pred_mat, col_set_se_mat, col_avg_wt)

# Back-transform dist_set from standardised to original scale
all_dist      <- c(dyn_cov$dist_set_2017, dyn_cov$dist_set_2018,
                   dyn_cov$dist_set_2021, dyn_cov$dist_set_2022)
dist_mean     <- mean(all_dist, na.rm = TRUE)
dist_sd       <- sd(all_dist,   na.rm = TRUE)
col_dist_orig <- col_dist_seq * dist_sd + dist_mean

col_set_df <- data.frame(
  distance     = col_dist_orig,
  colonization = col_set_ba$avg,
  lower        = pmax(col_set_ba$avg - 1.96 * col_set_ba$se, 0),
  upper        = pmin(col_set_ba$avg + 1.96 * col_set_ba$se, 1)
)

cat("\nColonization vs Settlement distance — preview:\n")
print(head(col_set_df))

# 8.5 Extinction model averaged prediction for distance to settlement

ext_dist_seq <- seq(
  min(yearlySiteCovs(umf)$dist_set, na.rm = TRUE),
  max(yearlySiteCovs(umf)$dist_set, na.rm = TRUE),
  length.out = 100
)

ext_set_newdat <- do.call(rbind, lapply(ext_dist_seq, function(x) {
  d          <- yearly_means   # start from complete mean template
  d$dist_set <- x              # vary only dist_set
  d
}))

ext_set_pred_list <- lapply(ext_avg_mods, function(m)
  predict(m, type = "ext", newdata = ext_set_newdat))

ext_set_pred_mat <- sapply(ext_set_pred_list, function(x) x$Predicted)
ext_set_se_mat   <- sapply(ext_set_pred_list, function(x) x$SE)

ext_set_ba <- ba_var(ext_set_pred_mat, ext_set_se_mat, ext_avg_wt)

# Back-transform using same dist parameters computed above
ext_dist_orig <- ext_dist_seq * dist_sd + dist_mean

ext_set_df <- data.frame(
  distance   = ext_dist_orig,
  extinction = ext_set_ba$avg,
  lower      = pmax(ext_set_ba$avg - 1.96 * ext_set_ba$se, 0),
  upper      = pmin(ext_set_ba$avg + 1.96 * ext_set_ba$se, 1)
)

cat("\nExtinction vs Settlement distance — preview:\n")
print(head(ext_set_df))


# 9. Sanity checks before plotting

cat("\n--- Sanity checks ---\n")

# Check for NAs in predictions
cat("NAs in col_set_df      :", sum(is.na(col_set_df)),  "\n")
cat("NAs in ext_set_df      :", sum(is.na(ext_set_df)),  "\n")

# Check prediction ranges are within [0,1]
cat("\nColonization vs settlement range:",
    round(range(col_set_df$colonization), 4), "\n")
cat("Extinction vs settlement range  :",
    round(range(ext_set_df$extinction), 4), "\n")

# Check CI widths — very wide CIs (upper-lower > 0.8) signal high uncertainty
cat("\nMean CI width — colonization vs settlement:",
    round(mean(col_set_df$upper - col_set_df$lower), 4), "\n")
cat("Mean CI width — extinction vs settlement  :",
    round(mean(ext_set_df$upper - ext_set_df$lower), 4), "\n")

#------------------------------------------------------------------------------------
# 10. Plot the prediction plot figures
#------------------------------------------------------------------------------------
# Fig 1 Colonization vs distance to settlement
fig_col_set <- ggplot(col_set_df,
                      aes(x = distance, y = colonization)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "orange", alpha = 0.25) +
  geom_line(linewidth = 1, colour = "darkorange3") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Model-Averaged Colonization vs Distance to Settlement",
    x     = "Distance to Settlement (m)",
    y     = "Colonization Probability"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(fig_col_set)
ggsave(
  "D:/R_projects/multiseason_occupancy/output/figure/fig_col_set.png",
  fig_col_set, width = 7, height = 5, dpi = 300
)

# Fig 2: Extinction vs distance to settlement
fig_ext_set <- ggplot(ext_set_df,
                      aes(x = distance, y = extinction)) +
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "plum", alpha = 0.25) +
  geom_line(linewidth = 1, colour = "purple4") +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "Model-Averaged Extinction vs Distance to Settlement",
    x     = "Distance to Settlement (m)",
    y     = "Extinction Probability"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(fig_ext_set)
ggsave(
  "D:/R_projects/multiseason_occupancy/output/figure/fig_ext_set.png",
  fig_ext_set, width = 7, height = 5, dpi = 300
)

#----------------------------------------------------------------------------------
# 11. Present all results into state parameter table
#-------------------------------------------------------------------------------------
# Model-averaged site-level predictions
occ_pred_raw <- predict(best_occ_mod, type = "occ")
psi_hat <- mean(occ_pred_raw$Predicted, na.rm = TRUE)
psi_se <- mean(occ_pred_raw$SE, na.rm = TRUE)

col_site_mat <- do.call(cbind, lapply(col_avg_mods,
                                      function(m) predict(m, type = "col")$Predicted))
gamma_site <- as.vector(col_site_mat %*% col_avg_wt)
gamma_hat  <- mean(gamma_site, na.rm = TRUE)
gamma_se   <- sd(gamma_site,   na.rm = TRUE)

ext_site_mat <- do.call(cbind, lapply(ext_avg_mods,
                                      function(m) predict(m, type = "ext")$Predicted))
epsilon_site <- as.vector(ext_site_mat %*% ext_avg_wt)
epsilon_hat  <- mean(epsilon_site, na.rm = TRUE)
epsilon_se   <- sd(epsilon_site,   na.rm = TRUE)

det_pred_raw <- predict(best_det_mod, type = "det")
p_hat <- mean(det_pred_raw$Predicted, na.rm = TRUE)
p_se  <- mean(det_pred_raw$SE,        na.rm = TRUE)

state_table <- data.frame(
  Parameter = c("Occupancy (ψ)", "Colonization (γ)",
                "Extinction (ε)", "Detection (p)"),
  Estimate  = round(c(psi_hat, gamma_hat, epsilon_hat, p_hat), 4),
  SE        = round(c(psi_se,  gamma_se,  epsilon_se,  p_se),  4)
)
print(state_table)

write.csv(state_table,
          "D:/R_projects/multiseason_occupancy/output/state_table.csv",
          row.names = FALSE)



