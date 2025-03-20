# clean ws
rm(list=ls())

# load pkg
library(gmwmx2)
library(tidyr)
library(dplyr)

# load data
load("data/mat_result_simulation_2024-12-28_20-53-40.rda")

sum(df_all_results$time_gmwmx) / 60 / 60
colnames(df_all_results)

# check distribution
boxplot(df_all_results$beta_hat_trend)
boxplot(df_all_results$std_beta_hat_trend)
boxplot(df_all_results$std_beta_hat_trend, outline = F)

# id to remove
quantile(df_all_results$std_beta_hat_trend)
id_to_remove = which(df_all_results$std_beta_hat_trend>1)
length(id_to_remove)
df_all_results_sub = df_all_results[-id_to_remove, ]
boxplot(df_all_results_sub$std_beta_hat_trend)
df_all_results2 = df_all_results_sub %>% dplyr::select(station_name,component, beta_hat_trend, std_beta_hat_trend )

# pivot wider
df_all_result2_wide = pivot_wider(df_all_results2, names_from = component, values_from = c(beta_hat_trend, std_beta_hat_trend))
colnames(df_all_result2_wide)

# rescale per year and in mm (original scale is in meter/day)
df_all_result2_wide_scaled = df_all_result2_wide %>% mutate(
  beta_hat_trend_N_scaled =   beta_hat_trend_N* 365.25 * 1000,
  beta_hat_trend_E_scaled = beta_hat_trend_E*365.25* 1000,
  beta_hat_trend_V_scaled = beta_hat_trend_V*365.25* 1000,
  std_beta_hat_trend_N_scaled = std_beta_hat_trend_N * 365.25* 1000,
  std_beta_hat_trend_E_scaled = std_beta_hat_trend_E * 365.25* 1000,
  std_beta_hat_trend_V_scaled =  std_beta_hat_trend_V*365.25* 1000) %>% 
  dplyr::select(station_name,
                beta_hat_trend_N_scaled,
                beta_hat_trend_E_scaled,
                beta_hat_trend_V_scaled,
                std_beta_hat_trend_N_scaled,
                std_beta_hat_trend_E_scaled,
                std_beta_hat_trend_V_scaled)


# compare withj solution from midas
df_midas  <- gmwmx2::download_estimated_velocities_ngl()
colnames(df_midas)

df_merge = dplyr::left_join(df_all_result2_wide_scaled,
                            df_midas, by="station_name") %>%
  dplyr::select(-c(midas_version_label, time_series_duration_year)) %>% 
  dplyr::mutate(east_velocity_mm_yr = east_velocity_m_yr * 1000,
                north_velocity_mm_yr = north_velocity_m_yr * 1000,
                up_velocity_mm_yr = up_velocity_m_yr * 1000,
                east_velocity_unc_mm_yr = east_velocity_unc_m_yr*1000,
                north_velocity_unc_mm_yr = north_velocity_unc_m_yr*1000,
                up_velocity_unc_mm_yr = up_velocity_unc_m_yr * 1000
                
                
                )

colnames(df_merge)

# df_merge = df_all_result2_wide_scaled
colnames(df_merge)
range(df_merge$longitude)
df_merge$longitude <- ifelse(df_merge$longitude < -180,
                             df_merge$longitude + 360,
                             df_merge$longitude)

range(df_merge$longitude)




#rename
df_merge2 = df_merge%>% dplyr::rename(
  trend_gmwmx_dN_scaled = beta_hat_trend_N_scaled,
  trend_gmwmx_dE_scaled = beta_hat_trend_E_scaled,
  trend_gmwmx_dU_scaled = beta_hat_trend_V_scaled,
  std_trend_gmwmx_dN_scaled = std_beta_hat_trend_N_scaled,
  std_trend_gmwmx_dE_scaled = std_beta_hat_trend_E_scaled,
  std_trend_gmwmx_dU_scaled = std_beta_hat_trend_V_scaled
  
  
)


df_merge2 = na.omit(df_merge2)


colnames(df_merge2)
boxplot(df_merge2$trend_gmwmx_dN_scaled)
boxplot(df_merge2$trend_gmwmx_dE_scaled)
boxplot(df_merge2$trend_gmwmx_dU_scaled)

boxplot(df_merge2$std_trend_gmwmx_dN_scaled)
boxplot(df_merge2$std_trend_gmwmx_dE_scaled)
boxplot(df_merge2$std_trend_gmwmx_dU_scaled)





save(df_merge2, file ="merge_df.rda")
