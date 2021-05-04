Sys.setenv(TZ = "UTC")

# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("tadhg-moore/gotmtools", ref = "yaml")

# Load libraries
library(gotmtools)
library(LakeEnsemblR)
library(ggplot2)

# Set working directory
setwd("FCR_2013_2019_LER/") # Change working directory to example folder

# Set config file & models
config_file <- 'LakeEnsemblR.yaml'
model <- c("FLake")
ncdf <- "output/ensemble_output.nc"

# LHC - Calibration ----
yaml <- read_yaml(config_file)
yaml$time$start <- "2013-05-16 00:00:00"
yaml$time$stop <- "2015-01-01 00:00:00"
yaml$output$time_step <- 1
write_yaml(yaml, config_file)
num <- 500
spin_up <- 190
out_f <- "calibration_results_v3"
cmethod <- "LHC"
model <- c("FLake")
folder <- "."
dir.create(out_f, showWarnings = FALSE)

# Run LER and inspect default output
export_config(config_file, model)
run_ensemble(config_file = config_file, model = model)

# plot heatmap
plot_heatmap(ncdf, model = model) + 
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()
fit <- calc_fit(ncdf, model = model)
fit

cali_ensemble(config_file, num = num, cmethod = cmethod, parallel = FALSE, model = model, folder = ".", 
              spin_up = spin_up, job_name = model, out_f = out_f)

cal_files <- list.files(out_f, full.names = TRUE)

res <- load_LHC_results(config_file = config_file, model = model, res_files = cal_files)
dim(res[[model]])

df <- plyr::ldply(res, function(x) {
  df <- x[, -c(3:7)]
  reshape2::melt(df, id.vars = c("par_id", "rmse"))
}, .id = "model")
df$id_no <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$par_id)) 

bst_par <- df$id_no[which.min(df$rmse)]
sub <- df[df$id_no == bst_par, ]

p1 <- ggplot(df) +
  geom_point(aes(value, rmse)) +
  facet_wrap(model~variable, scales = "free_x") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  ylab("RMSE (\u00B0C)") +
  geom_vline(data = sub, aes(xintercept = value)) +
  geom_hline(yintercept = 3.5, color = "red", linetype = "dashed") +
  # coord_cartesian(ylim = c(1, 4)) +
  # scale_x_log10() +
  theme_classic(base_size = 16)
p1
# ggsave(file.path(out_f, "calib_results.png"), p1,  dpi = 300,width = 384, height = 280, units = 'mm')

# Create priors ----
par_df <- plyr::ddply(df, "variable", function(x) {
  sub <- x[x$rmse < 3.5, ]
  std <- sd(x$value)
  mn <- mean(x$value)
  data.frame(mean = mn, min = ifelse((mn - std) < 0, 0, mn - std), max = mn + std)
})
par_df

# MCMC - Calibration ----
yaml <- read_yaml(config_file)
yaml$time$start <- "2013-05-16 00:00:00"
yaml$time$stop <- "2015-01-01 00:00:00"
yaml$output$time_step <- 1

# Update limits for MCMC calibration
for(i in par_df$variable) {
  if(i %in% c("wind_speed", "swr", "lwr")) {
    yaml$calibration$met[[i]]$lower <- signif(par_df[which(par_df$variable == i), "min"], 3)
    yaml$calibration$met[[i]]$upper <- signif(par_df[which(par_df$variable == i), "max"], 3)
    yaml$calibration$met[[i]]$initial <- signif(par_df[which(par_df$variable == i), "mean"], 3)
  } else {
    yaml$calibration[[model]][[i]]$lower <- signif(par_df[which(par_df$variable == i), "min"], 3)
    yaml$calibration[[model]][[i]]$upper <- signif(par_df[which(par_df$variable == i), "max"], 3)
    yaml$calibration[[model]][[i]]$initial <- signif(par_df[which(par_df$variable == i), "mean"], 3)
  }
}

write_yaml(yaml, config_file)
num <- 10000
spin_up <- 190
cmethod <- "MCMC"
model <- c("FLake")
out_f <- paste0("mcmc_", model)
folder <- "."
dir.create(out_f, showWarnings = FALSE)


cali_ensemble(config_file, num = num, cmethod = cmethod, parallel = FALSE, model = model, folder = ".", 
              spin_up = spin_up, job_name = model, out_f = out_f)

cal_file <- list.files(out_f, full.names = TRUE)

# MCMC Calibration results -----
res <- read.csv(cal_file)
dim(res)
res$run <- 1:nrow(res)
mlt <- reshape2::melt(res, id.vars = c("qual", "run"))

# Change in the parameters over time
ggplot(mlt) +
  # geom_point(aes(run, value)) +
  geom_line(aes(run, value)) +
  geom_smooth(aes(run, value)) +
  facet_wrap(~variable, scales = "free_y") +
  scale_y_log10()

# Look for convergence
ggplot(mlt) +
  geom_point(aes(value, qual)) +
  # geom_line(aes(run, value)) +
  geom_smooth(aes(value, qual)) +
  facet_wrap(~variable, scales = "free_x") +
  # scale_x_log10() +
  theme_classic()

best_par <- res[which.min(res$qual), ]
best_par

# Input calibrated values ----
yaml <- read_yaml(config_file)
yaml$scaling_factors[[model]][["wind_speed"]] <- best_par$wind_speed
yaml$scaling_factors[[model]][["swr"]] <- best_par$swr
yaml$model_parameters[[model]][["LAKE_PARAMS/c_relax_C"]] <- best_par$LAKE_PARAMS.c_relax_C
write_yaml(yaml, config_file)

export_config(config_file, model)
run_ensemble(config_file = config_file, model = model)

plot_heatmap(ncdf, model = model) + 
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()
fit <- calc_fit(ncdf, model = model)
fit

pres <- plot_resid(ncdf)

ggpubr::ggarrange(plotlist = pres)


