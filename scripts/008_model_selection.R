#---------------------------------------------------------------------------------------------
#Project: Multi-season occupancy analysis
#Purpose: To build candidate models and select the best fitted model for occupancy analysis
#------------------------------------------------------------------------------------------------

install.packages("MuMIn")
# Load necessary packages
library(unmarked)
library(AICcmodavg)
library(MuMIn)
library(ggplot2)

# Load necessary files
umf <- readRDS("D:/R_projects/multiseason_occupancy/output/umf_object.rds")
dyn_cov <- read.csv("D:/R_projects/multiseason_occupancy/output/dynamic_covariates.csv")

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

# 2.1.2 Year only model
det_year <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 2.1.3 Effort + Year
det_eff_year <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~effort + year,
  data = umf
)

# 2.2 Select the best model
# 2.2.1 Compare the models
det_mod <- fitList(
  null = m0,
  effort = det_eff,
  year = det_year,
  effort_year = det_eff_year
)

# 2.2.2 Select the model
modSel(det_mod)

# Considering Year is our best model for detection
best_det_mod <- det_year
#-------------------------------------------------------------------------------------------------------------
# 3. Occupancy Model Selection
#-------------------------------------------------------------------------------------------------------------
# 3.1 Build Occupancy Models
# 3.1.1 Null Model
occ_null <- colext(
  psiformula = ~1,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 3.1.2 Tri covariate 
occ_tri <- colext(
  psiformula = ~tri_z,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 3.2 Select the best model
# 3.2.1 Fit the model
occ_mod <- fitList(
  null = occ_null,
  TRI = occ_tri
)

# 3.2.2 Select the best model
modSel(occ_mod)

# Assuming that occ_tri is our best occupancy model
best_occ_mod <- occ_tri

#-------------------------------------------------------------------------------------------------------------
# 4. Colonization Models Selection
#---------------------------------------------------------------------------------------------------------------
# 4.1 Build Models
# 4.1.1 Null model
col_null <- colext(
  psiformula = ~tri_z,
  gammaformula = ~1,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 4.1.2 Single covariate models
col_cover <- colext(
  psiformula = ~tri_z,
  gammaformula = ~fcover,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_ndvi <- colext(
  psiformula = ~tri_z,
  gammaformula = ~ndvi,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_set <- colext(
  psiformula = ~tri_z,
  gammaformula = ~dist_set,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 4.1.3 Double additive models
col_set_cover <- colext(
  psiformula = ~tri_z,
  gammaformula = ~dist_set + fcover,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_cover_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~fcover + floss,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_loss_ndvi <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_ndvi_set <- colext(
  psiformula = ~tri_z,
  gammaformula = ~dist_set + ndvi,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_set_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~dist_set + floss,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 4.1.4 Multiple additive models
col_set_cover_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~dist_set + fcover + floss,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

col_set_loss_ndvi <- colext(
  psiformula = ~tri_z,
  gammaformula = ~dist_set + floss + ndvi,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# Since our forest cover and ndvi were highly correlated they were not kept in the same model.

# 4.2 Select the best model
# 4.2.1 Fit the model
col_mod <- fitList(
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
modSel(col_mod)

# Assuming that col_loss_ndvi is the best supported models among them
#-----------------------------------------------------------------------------------------------------------------
# 5. Extinction Model Selection
#------------------------------------------------------------------------------------------------------------------------
# 5.1 Build Models
# 5.1.1 Null model
ext_null <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~1,
  pformula = ~year,
  data = umf
)

# 4.1.2 Single covariate models
ext_cover <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~fcover,
  pformula = ~year,
  data = umf
)


ext_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~floss,
  pformula = ~year,
  data = umf
)


ext_ndvi <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~ndvi,
  pformula = ~year,
  data = umf
)


ext_set <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set,
  pformula = ~year,
  data = umf
)


# 4.1.3 Double additive models
ext_set_cover <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set + fcover,
  pformula = ~year,
  data = umf
)


ext_cover_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~fcover + floss,
  pformula = ~year,
  data = umf
)


ext_loss_ndvi <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~floss + ndvi,
  pformula = ~year,
  data = umf
)

ext_ndvi_set <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~ndvi + dist_set,
  pformula = ~year,
  data = umf
)

ext_set_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set + floss,
  pformula = ~year,
  data = umf
)

# 4.1.4 Multiple additive models
ext_set_cover_loss <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set + fcover + floss,
  pformula = ~year,
  data = umf
)

ext_set_loss_ndvi <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set + floss + ndvi,
  pformula = ~year,
  data = umf
)

# Since our forest cover and ndvi were highly correlated they were not kept in the same model.

# 4.2 Select the best model
# 4.2.1 Fit the model
ext_mod <- fitList(
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
modSel(ext_mod)

# Assuming that ext_set is the best candidate model 

#--------------------------------------------------------------------------------------------------------------
# 5. Fit the best final model
#--------------------------------------------------------------------------------------------------------------
final_mod <- colext(
  psiformula = ~tri_z,
  gammaformula = ~floss + ndvi,
  epsilonformula = ~dist_set,
  pformula = ~year,
  data = umf
)

final_mod
summary(final_mod)

#------------------------------------------------------------------------------------------------------------------
# 6. Goodness of Fit test / Mackenzie-Bailey Fit test
#-----------------------------------------------------------------------------------------------------------------
# 6.1 Calculate Goodness of Fit
gof <- mb.gof.test(
  final_mod,
  nsim = 1000
)

gof

# In our demo data, bootstrap goodness-of-fit test indicated acceptable model fit (p = 0.18) and only weak overdispersion (c-hat = 1.24), so we will be proceeding using AICc.

# 6.2 Check the models for model selection uncertainty
det_modsel <- modSel(det_mod)
det_modsel

occ_modsel <- modSel(occ_mod)
occ_modsel

col_modsel <- modSel(col_mod)
col_modsel

ext_modsel <- modSel(ext_mod)
ext_modsel

# Since Colonization and Extinction models have multiple supported models, we will perform model averaged prediction on the colonization parameter

#---------------------------------------------------------------------------------------------------------------------------------------------------
# 7. Model averaged Predictions
#--------------------------------------------------------------------------------------------------------------------------------
# 7.1 For Colonization Model
# 7.1.1 Define supported model set i.e. delta AIC < 2
colmodel <- list(
col_loss_ndvi,
col_null,
col_ndvi_set,
col_cover
)

colnames <- c(
 "loss_ndvi",
 "Null",
 "ndvi_set",
 "cover"
)

sel_aic <- aictab(
  cand.set = colmodel,
  modnames = colnames
)
sel_aic

# Extract AIC weights for each supported models

weight_col <- grep("AICcWt", names(sel_aic), value = TRUE)

weights <- sel_aic[[weight_col]][
    match(colnames, sel_aic$Modnames)
  ]

weights

# 7.1.2 Build Prediction gradient with the most supported covariate in the models - ndvi 
range(yearlySiteCovs(umf)$ndvi)

ndvi_seq <- seq(
  min(yearlySiteCovs(umf)$ndvi, na.rm = TRUE),
  max(yearlySiteCovs(umf)$ndvi, na.rm = TRUE),
  length.out = 100
)

# 7.1.3 Hold other covariates constant
mean_loss <- mean(yearlySiteCovs(umf)$floss, na.rm = TRUE)
mean_cover <- mean(yearlySiteCovs(umf)$fcover, na.rm = TRUE)
mean_set <- mean(yearlySiteCovs(umf)$dist_set, na.rm = TRUE)

# 7.1.4 Create prediction data
newdat <- data.frame(
  ndvi = ndvi_seq,
  floss = mean_loss,
  fcover = mean_cover,
  dist_set = mean_set
)

# 7.1.5 Generate predictions
pred_list <- lapply(
  colmodel,
  function(m)
    predict(
      m,
      type="col",
      newdata = newdat
    )
)

# 7.1.6 Extract the predictions
pred_mat <- sapply(
  pred_list,
  function(x) x$Predicted
)

dim(pred_mat)

# 7.1.7 Compute model averaged prediction
col_avg_pred <- as.vector(pred_mat%*%weights)
avg_pred

# 7.1.8 Compute unconditional variance
se_mat <- sapply(
  pred_list,
  function(x) x$SE
)

# 7.1.9 Burnham & Anderson's unconditional variance formula
avg_var <- numeric(length(ndvi_seq))
for(i in 1:length(ndvi_seq)){
  avg_var[i] <- sum(
    weights * (
      se_mat[i, ]^2 +
        (pred_mat[i, ] - col_avg_pred[i])^2
    )
  )
}

# 7.1.10 Confidence intervals
col_avg_se <- sqrt(avg_var)

col_lower <- col_avg_pred - 1.96 * col_avg_se
col_upper <- col_avg_pred + 1.96 * col_avg_se

col_lower[col_lower < 0] <- 0
col_upper[col_upper > 1] <- 1

# 7.1.11 Create original ndvi from dyn_cov
dyn_cov

all_ndvi <- c(
  dyn_cov$ndvi_2017,
  dyn_cov$ndvi_2018,
  dyn_cov$ndvi_2021
)

summary(all_ndvi)

# 7.1.12 Calculate mean and sd from them
ndvi_mean <- mean(all_ndvi, na.rm = TRUE)
ndvi_sd <- sd(all_ndvi, na.rm = TRUE)

ndvi_original <- ndvi_seq * ndvi_sd + ndvi_mean
summary(ndvi_original)

# 7.1.13 Create a dataframe
col_df <- data.frame(
  ndvi = ndvi_original,
  colonization = col_avg_pred,
  lower = col_lower,
  upper = col_upper
)

col_df

# 7.1.14 dynCreate plot
ggplot(
  col_df,
  aes(x = ndvi,
      y = colonization)
) + 
  geom_ribbon(
    aes(
      ymin = lower,
      ymax = upper
    ), alpha = 0.2
  ) + 
  geom_line(
    linewidth = 1
  ) + 
  coord_cartesian(ylim = c(0,1)) +
  labs(
    title = "Colonization probability vs NDVI",
    x = "NDVI",
    y = "Colonization Probability"
  ) + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

# 7.2 For extinction model:
#If Extinction model also shows multiple supported models and one covariate consistently appears among them, model averaged predictions can be produced using the code above
# If the null model is the top-ranked model and covariate effects are weak, model averaged coefficients can be examined instead of generating prediction curves
# 7.2.1 List the best supported models
ext_modsel

ext_models <- list(
  ext_null,
  ext_set,
  ext_cover
)

# 7.2.2 Calculate the average of the model
ext_avg <- model.avg(ext_models)


# Model averaging was attempted but supported models exhibited singular Hessian matrices, preventing reliable calculation of model-averaged coefficients. 
#Therefore, extinction inference was based on model-selection results i.e. null model.
best_ext_mod <- ext_null

#---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 8. Prediction Probability
#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 9.1 Occupancy prediction from best occupancy model
pred_psi <- predict(best_occ_mod, type = "psi")

# 9.2 Detection predictions from the best detection model
pred_det <- predict(best_det_mod, type = "det")

# 9.3 Extinction predictions from the best extinction model
pred_ext <- predict(best_ext_mod, type = "ext")

# 9.4 Extract occupancy and detection predicted probabilities
psi_vals <- pred_psi$Predicted
det_vals <- pred_det$Predicted
ext_vals <- pred_ext$Predicted

# 9.5 Overall means
mean_psi <- mean(psi_vals, na.rm = TRUE)
mean_col <- mean(col_avg_pred, na.rm = TRUE)
mean_ext <- mean(ext_vals, na.rm = TRUE)
mean_det <- mean(det_vals, na.rm = TRUE)

# 9.6 Confidence intervals
psi_lower <- mean(pred_psi$lower, na.rm = TRUE)
psi_upper <- mean(pred_psi$upper, na.rm = TRUE)

col_ci_lower <- mean(col_lower, na.rm = TRUE)
col_ci_upper <- mean(col_upper, na.rm = TRUE)

ext_lower <- mean(pred_ext$lower, na.rm = TRUE)
ext_upper <- mean(pred_ext$upper, na.rm = TRUE)

det_lower <- mean(pred_det$lower, na.rm = TRUE)
det_upper <- mean(pred_det$upper, na.rm = TRUE)

# 9.7 Final probability table
result <- data.frame(
  parameter = c("Occupancy", "Colonization", "Extinction", "Detection"),
  mean = c( mean_psi, mean_col, mean_ext, mean_det ),
  lower_CI = c(psi_lower, col_ci_lower, ext_lower, det_lower),
  upper_CI = c(psi_upper, col_ci_upper, ext_upper, det_upper)
)

result

# 9.8 Export the result table
write.csv(result, "D:/R_projects/multiseason_occupancy/output/probability_table.csv")

# 9.9 Combine all predictions
col_df
pred_col_df <- data.frame(
  parameter = "col",
  Covariate = col_df$ndvi,
  Predicted = col_df$colonization,
  lower = col_df$lower,
  upper = col_df$upper
)

pred_ext_df <- data.frame(
  parameter = "ext",
  Covariate = NA,
  Predicted = pred_ext$Predicted,
  lower = pred_ext$lower,
  upper = pred_ext$upper
)

pred_psi_df <- data.frame(
  parameter = "psi",
  Covariate = NA,
  Predicted = pred_psi$Predicted,
  lower = pred_psi$lower,
  upper = pred_psi$upper
)

pred_det_df <- data.frame(
  parameter = "det",
  Covariate = NA,
  Predicted = pred_det$Predicted,
  lower = pred_det$lower,
  upper = pred_det$upper
)

all_predictions <- rbind(
  pred_psi_df,
  pred_col_df,
  pred_ext_df,
  pred_det_df
)
all_predictions

write.csv(all_predictions, "D:/R_projects/multiseason_occupancy/output/all_predictions.csv", row.names = FALSE)














