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
model <- c("GLM", "GOTM", "FLake", "Simstrat", "MyLake")
model <- c("Simstrat")
ncdf <- "output/ensemble_output.nc"

# Example run
# 1. Export settings - creates directories with all model setups and exports settings from the LER configuration file
export_config(config_file = config_file, model = model)

# 2. Run ensemble lake models
run_ensemble(config_file = config_file, model = model, parallel = F)

plot_heatmap(ncdf, model = model) + 
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()

plot_ensemble(ncdf, model, depth = 1)
plot_ensemble(ncdf, model, depth = 3)
plot_ensemble(ncdf, model, depth = 6)
plot_ensemble(ncdf, model, depth = 9) + lims(y = c(0,14))
plot_ensemble(ncdf, model, "ice_height")

out <- load_var(ncdf, "temp")

res <- lapply(out, function(x) {
  
})

mod_fit <- calc_fit(ncdf, model = model)

p <- lapply(plot_resid(ncdf, model = model), function(x) x + theme_classic())
ggpubr::ggarrange(plotlist = p)


# Calibration ----
yaml <- read_yaml(config_file)
yaml$time$start <- "2013-05-16 00:00:00"
yaml$time$stop <- "2015-01-01 00:00:00"
yaml$output$time_step <- 1
write_yaml(yaml, config_file)
num <- 500
spin_up <- 190
out_f <- "calibration_results_v2"
cmethod <- "LHC"
model <- c("GLM", "GOTM", "FLake", "Simstrat")
folder <- "."


export_config(config_file, model)
run_ensemble(config_file = config_file, model = model, parallel = T)

plot_heatmap(ncdf, model = model) + 
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()
fit <- calc_fit(ncdf, model = model)
fit

plot_resid(ncdf = ncdf, model = model)
temp <- load_var(ncdf, "temp")
rLakeAnalyzer::wtr.lineseries(temp$Simstrat[, 1:40])

cali_ensemble(config_file, num = num, cmethod = cmethod, parallel = T, model = model, folder = ".", 
              spin_up = spin_up, job_name = "job1", out_f = out_f)

cal_files <- list.files(out_f, pattern ="202104161448", full.names = TRUE)

res <- load_LHC_results(config_file = config_file, model = model, res_files = cal_files)
lapply(res, head)


df <- plyr::ldply(res, function(x) {
  df <- x[, -c(3:7)]
  reshape2::melt(df, id.vars = c("par_id", "rmse"))
}, .id = "model")
df$id_no <- as.numeric(gsub(".*?([0-9]+).*", "\\1", df$par_id)) 

p1 <- ggplot(df) +
  geom_point(aes(value, rmse)) +
  facet_wrap(model~variable, scales = "free_x") +
  geom_hline(yintercept = 2, linetype = "dashed") +
  ylab("RMSE (\u00B0C)") +
  coord_cartesian(ylim = c(1, 4)) +
  # scale_x_log10() +
  theme_classic(base_size = 16)
ggsave(file.path(out_f, "calib_results.png"), p1,  dpi = 300,width = 384, height = 280, units = 'mm')

df_wid <- tidyr::pivot_wider(df, id_cols = c(par_id, rmse, model), values_from = value, names_from = variable)

ggplot(df_wid) +
  geom_point(aes(wind_speed, swr, color = rmse)) +
  facet_wrap(~model, scales = "free") +
  scale_colour_gradientn(limits = c(0, 32),
                         colours = rev(RColorBrewer::brewer.pal(11, "Spectral"))) + theme_classic()

idx <- which(df$variable == "wind_speed")

ggplot(df[idx, ], aes(rmse, value)) +
  geom_point()

mlt <- reshape2::melt(res, id.vars = c("par_id", "rmse"))
ggplot(mlt) +
  geom_density(aes(rmse, fill = L1), alpha = 0.2) +
  geom_vline(xintercept = 2)



plot_LHC(config_file = config_file, model = model, res_files = cal_files,
         qual_met = "nse", best = "high")

