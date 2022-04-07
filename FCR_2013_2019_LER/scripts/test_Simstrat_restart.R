setwd("FCR_2013_2019_LER/")

Sys.setenv(TZ = "UTC")

# remotes::install_github("tadhg-moore/LakeEnsemblR", ref = "flare")
# remotes::install_github("tadhg-moore/gotmtools", ref = "yaml")
# remotes::install_github("tadhg-moore/SimstratR")

library(SimstratR)
library(LakeEnsemblR)
library(gotmtools)
library(ggplot2)
library(magrittr)

scale_colour_discrete <- ggthemes::scale_colour_colorblind 
scale_fill_discrete <- ggthemes::scale_fill_colorblind

# Run through
config_file <- "LakeEnsemblR.yaml"
model <- "Simstrat"
vars <- "temp"

# config_file <- "simstrat.par"

yaml <- read_yaml(config_file)
start <- "2013-05-16 00:00:00"
stop <- "2013-06-15 00:00:00"
yaml$time$start <- start
yaml$time$stop <- stop
yaml$output$time_unit <- "hour"
yaml$output$time_step <- 1
yaml$output$depths <- 0.5
yaml$model_parameters$Simstrat$`Simulation/Continue from last snapshot` <- NULL
yaml$restart$use <- FALSE
write_yaml(yaml, config_file)

out_time <- seq.POSIXt(as.POSIXct(start), as.POSIXct(stop), by = "1 hour", tz = "UTC")

unlink("Simstrat/simstrat.par")
export_config(config_file, model)

unlink(list.files("Simstrat/output/", full.names = TRUE), recursive = TRUE, force = TRUE)
# run_simstrat(sim_folder = "Simstrat", "simstrat.par", verbose = F)
run_ensemble(config_file, model)

out_cont <- get_output(yaml, model, vars, out_time = out_time, run_success = T)$temp
out_cont_mlt <- out_cont %>% 
  reshape::melt(., id.vars = 1) %>% 
  dplyr::mutate(depth = as.numeric(gsub("wtr_", "", variable)))

# Inspecting process variables ----
# U <- read.delim(file.path(model, "output", "U_out.dat"), sep = ",")
# colnames(U) <- c("datetime", paste0("U_", deps2))
# U <- reshape::melt(U, id.vars = 1)
# U[, 2] <- as.numeric(gsub("U_", "", U[, 2]))
# U$var <- "U"
# V <- read.delim(file.path(model, "output", "V_out.dat"), sep = ",")
# colnames(V) <- c("datetime", paste0("V_", deps2))
# V <- reshape::melt(V, id.vars = 1)
# V[, 2] <- as.numeric(gsub("V_", "", V[, 2]))
# V$var <- "V"
# k <- read.delim(file.path(model, "output", "k_out.dat"), sep = ",")
# colnames(k) <- c("datetime", paste0("k_", deps2))
# k <- reshape::melt(k, id.vars = 1)
# k[, 2] <- as.numeric(gsub("k_", "", k[, 2]))
# k$var <- "k"
# eps <- read.delim(file.path(model, "output", "eps_out.dat"), sep = ",")
# colnames(eps) <- c("datetime", paste0("eps_", deps2))
# eps <- reshape::melt(eps, id.vars = 1)
# eps[, 2] <- as.numeric(gsub("eps_", "", eps[, 2]))
# eps$var <- "eps"
# dat <- rbind.data.frame(U, V, k, eps)
# 
# ggplot(dat) +
#   geom_line(aes(datetime, value, color = factor(variable))) +
#   facet_wrap(~var, scales = "free_y") +
#   scale_y_log10()
# 
# temp <- read.delim(file.path(model, "output", "T_out.dat"), sep = ",")
# colnames(temp) <- c("datetime", paste0("temp_", deps2))
# temp <- reshape::melt(temp, id.vars = 1)
# temp[, 2] <- as.numeric(gsub("temp_", "", temp[, 2]))
# temp$var <- "temp"
# 
# colnames(temp)[3] <- "temp"
# df <- merge(dat, temp, by = c(1,2))
# ggplot(df) +
#   geom_point(aes(value, temp, color = factor(variable))) +
#   facet_wrap(~var.x, scales = "free") +
#   scale_x_log10()
# 
# 
# wtr.lineseries(out_cont)
# wtr.lineseries(U)

# Run with manual restart ----
init <- read.delim(file.path("Simstrat", "init_cond.dat"))
z_out <- abs(init[, 1])

start <- "2013-05-16 06:00:00"
stop <- "2013-06-15 06:00:00"
dates <- seq.POSIXt(as.POSIXct(start), as.POSIXct(stop), by = "1 day", tz = "UTC")
out_l <- list()
restart_list <- list(U_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     V_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     temp_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     k_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     eps_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))))


for(i in 1:(length(dates) - 1)) {
  out_time <- seq.POSIXt(dates[i], dates[i + 1], by = "1 hour")
  start <- format(dates[i], format = "%Y-%m-%d %H:%M:%S")
  stop <- format(dates[i + 1], format = "%Y-%m-%d %H:%M:%S")
  yaml$time$start <- start
  yaml$time$stop <- stop
  write_yaml(yaml, config_file)
  dir.create("output", showWarnings = FALSE)
  if(i == 1) {
    # yaml$time$start <- format((dates[i] - lubridate::days(7)), format = "%Y-%m-%d %H:%M:%S")
    yaml$time$start <- format((dates[i]), format = "%Y-%m-%d %H:%M:%S")
    yaml$time$stop <- stop
    write_yaml(yaml, config_file)
    export_config(config_file, model, dirs = F, location = F, output_settings = F, meteo = F, 
                  extinction = F, init_cond = TRUE, inflow = F, time = TRUE, model_parameters = F)
  } else {
    export_config(config_file, model, dirs = F, location = F, output_settings = F, meteo = F, 
                  extinction = F, init_cond = FALSE, inflow = F, time = TRUE, model_parameters = F)
    simstrat_init <- read.delim(file.path(model, "init_cond.dat"))
    simstrat_init[, 2] <- signif(restart_list$U_restart[, i-1], 5)
    simstrat_init[, 3] <- signif(restart_list$V_restart[, i-1], 5)
    simstrat_init[, 4] <- signif(restart_list$temp_restart[, i-1], 5)
    simstrat_init[, 5] <- signif(restart_list$k_restart[, i-1], 5)
    simstrat_init[, 6] <- signif(restart_list$eps_restart[, i-1], 5)
    colnames(simstrat_init) <- c("Depth [m]",	"U [m/s]",	"V [m/s]",	"T [deg C]",	"k [J/kg]",	"eps [W/kg]")
    vroom::vroom_write(simstrat_init, file.path("Simstrat", "init_cond.dat"), delim = "\t",
                       col_names = TRUE, quote = "none")
  }
  run_simstrat(sim_folder = "Simstrat", "simstrat.par")
  
  # Extract variables for restarting initial conditions
  U <- read.delim(file.path(model, "output", "U_out.dat"), sep = ",")
  final_time_step <- nrow(U)
  U <- U[final_time_step, -1]
  deps2 <- colnames(U) %>%
    regmatches(., gregexpr("[[:digit:]]+\\.*[[:digit:]]*", .)) %>%
    unlist() %>%
    as.numeric()
  restart_list$U_restart[, i] <- approx(deps2, U, z_out, rule = 2)$y
  restart_list$V_restart[, i] <- read.delim(file.path(model, "output", "V_out.dat"), sep = ",")[final_time_step, -1] %>%
    approx(deps2, ., z_out, rule = 2) %>%
    .[[2]]
  restart_list$k_restart[, i] <- read.delim(file.path(model, "output", "k_out.dat"), sep = ",")[final_time_step, -1] %>%
    approx(deps2, ., z_out, rule = 2) %>%
    .[[2]]
  restart_list$eps_restart[, i] <- read.delim(file.path(model, "output", "eps_out.dat"), sep = ",")[final_time_step, -1] %>%
    approx(deps2, ., z_out, rule = 2) %>%
    .[[2]]
  restart_list$temp_restart[, i] <- read.delim(file.path(model, "output", "T_out.dat"), sep = ",")[final_time_step, -1] %>%
    approx(deps2, ., z_out, rule = 2) %>%
    .[[2]]
  
  
  tmp <- get_output(yaml, model, vars, out_time = out_time)$temp
  unlink(file.path(model, "output"), recursive = TRUE)
  out_l[[length(out_l) + 1]] <- tmp
}

wtemp_restart <- do.call(rbind, out_l)
wtemp_restart <- wtemp_restart[-which(duplicated(wtemp_restart$datetime)), ]
wtemp_restart_mlt <- wtemp_restart %>% 
  reshape::melt(., id.vars = 1) %>% 
  dplyr::mutate(depth = as.numeric(gsub("wtr_", "", variable)))

cols <- rev(viridis::plasma(ncol(wtemp_restart)))

p1 <- ggplot(out_cont_mlt) +
  geom_line(aes(datetime, value, color = factor(depth))) +
  scale_color_manual(values = cols) +
  ggtitle("No Restart") +
  ylab("Water temperature (\u00B0C)") +
  coord_cartesian(ylim = c(6, 28)) +
  theme_classic(base_size = 18) +
  guides(colour = guide_legend(title = "Depth (m)", override.aes = list(size = 3)))


p2 <- ggplot(wtemp_restart_mlt) +
  geom_line(aes(datetime, value, color = factor(depth))) +
  scale_color_manual(values = cols) +
  ggtitle("With Daily Restart") +
  ylab("Water temperature (\u00B0C)") +
  coord_cartesian(ylim = c(6, 28)) +
  theme_classic(base_size = 18) +
  guides(colour = guide_legend(title = "Depth (m)", override.aes = list(size = 3)))

g1 <- ggpubr::ggarrange(p1, p2, ncol = 1, common.legend = TRUE, legend = "right")
g1

ggsave("output/simstrat_restart_plot.png", g1, dpi = 300,width = 310,height = 220, units = 'mm')


out_cont_mlt$type <- "No_restart"
wtemp_restart_mlt$type <- "Restart"

dat <- rbind(out_cont_mlt, wtemp_restart_mlt)
sub <- dat[dat$depth == 0, ]

p3 <- ggplot(dat[dat$depth == 0, ]) +
  geom_vline(xintercept = dates, linetype = "dashed", color = "grey") +
  geom_line(aes(datetime, value, color = type)) +
  ylab("Water temperature (\u00B0C)") +
  # facet_wrap(~type) +
  theme_classic(base_size = 18)
p3
ggsave("output/simstrat_restart_plot_surfaceT.png", p3, dpi = 300,width = 310,height = 220, units = 'mm')


ggplot(dat) +
  geom_line(aes(datetime, value, color = type)) +
  # scale_color_manual(values = cols) +
  # ggtitle("With Daily Restart") +
  ylab("Water temperature (\u00B0C)") +
  facet_wrap(~depth) +
  coord_cartesian(ylim = c(6, 28)) +
  theme_classic(base_size = 18) +
  guides(colour = guide_legend(title = "Depth (m)", override.aes = list(size = 3)))


#####

# Run with Simstrat restart ----
init <- read.delim(file.path("Simstrat", "init_cond.dat"))
z_out <- abs(init[, 1])
z_out[1] <- 0.5
start <- "2013-05-16 00:00:00"
stop <- "2013-06-15 00:00:00"
restart_list <- list()
dates <- seq.POSIXt(as.POSIXct(start), as.POSIXct(stop), by = "1 day", tz = "UTC")
out_l <- list()
restart_list <- list(U_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     V_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     temp_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     k_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))),
                     eps_restart = array(NA, dim = c(length(z_out), (length(dates) - 1))))


for(i in 1:(length(dates) - 1)) {
  start <- format(dates[i], format = "%Y-%m-%d %H:%M:%S")
  stop <- format(dates[i + 1], format = "%Y-%m-%d %H:%M:%S")
  yaml$time$stop <- stop
  write_yaml(yaml, config_file)
  dir.create("output", showWarnings = FALSE)
  if(i == 1) {
    unlink("Simstrat/output/simulation-snapshot.dat")
    yaml$time$start <- start
    yaml$model_parameters$Simstrat$`Simulation/Continue from last snapshot` <- FALSE
    write_yaml(yaml, config_file)
    export_config(config_file, model, dirs = F, location = F, output_settings = F, meteo = F, 
                  extinction = F, init_cond = TRUE, inflow = F, time = TRUE, model_parameters = TRUE)
  } else {
    yaml$model_parameters$Simstrat$`Simulation/Continue from last snapshot` <- TRUE
    write_yaml(yaml, config_file)
    export_config(config_file, model, dirs = F, location = F, output_settings = F, meteo = F, 
                  extinction = F, init_cond = FALSE, inflow = F, time = TRUE, model_parameters = TRUE)
    # simstrat_init <- read.delim(file.path(model, "init_cond.dat"))
    # simstrat_init[, 1] <- -z_out
    # simstrat_init[, 2] <- signif(restart_list$U_restart[, i-1], 5)
    # simstrat_init[, 3] <- signif(restart_list$V_restart[, i-1], 5)
    # simstrat_init[, 4] <- signif(restart_list$temp_restart[, i-1], 5)
    # simstrat_init[, 5] <- signif(restart_list$k_restart[, i-1], 5)
    # simstrat_init[, 6] <- signif(restart_list$eps_restart[, i-1], 5)
    # colnames(simstrat_init) <- c("Depth [m]",	"U [m/s]",	"V [m/s]",	"T [deg C]",	"k [J/kg]",	"eps [W/kg]")
    # vroom::vroom_write(simstrat_init, file.path("Simstrat", "init_cond.dat"), delim = "\t",
    #                    col_names = TRUE, quote = "none")
  }
  run_simstrat(sim_folder = "Simstrat", "simstrat.par", verbose = TRUE)
  
  # Extract variables for restarting initial conditions
  # U <- read.delim(file.path(model, "output", "U_out.dat"), sep = ",")
  # final_time_step <- nrow(U)
  # U <- U[final_time_step, -1]
  # deps2 <- colnames(U) %>%
  #   regmatches(., gregexpr("[[:digit:]]+\\.*[[:digit:]]*", .)) %>%
  #   unlist() %>%
  #   as.numeric()
  # restart_list$U_restart[, i] <- approx(deps2, U, z_out, rule = 2)$y
  # restart_list$V_restart[, i] <- read.delim(file.path(model, "output", "V_out.dat"), sep = ",")[final_time_step, -1] %>%
  #   approx(deps2, ., z_out, rule = 2) %>%
  #   .[[2]]
  # restart_list$k_restart[, i] <- read.delim(file.path(model, "output", "k_out.dat"), sep = ",")[final_time_step, -1] %>%
  #   approx(deps2, ., z_out, rule = 2) %>%
  #   .[[2]]
  # restart_list$eps_restart[, i] <- read.delim(file.path(model, "output", "eps_out.dat"), sep = ",")[final_time_step, -1] %>%
  #   approx(deps2, ., z_out, rule = 2) %>%
  #   .[[2]]
  # restart_list$temp_restart[, i] <- read.delim(file.path(model, "output", "T_out.dat"), sep = ",")[final_time_step, -1] %>%
  #   approx(deps2, ., z_out, rule = 2) %>%
  #   .[[2]]
  
  tmp <- get_output(yaml, model, vars, obs_depths = z_out, out_time = out_time, run_success = TRUE)$temp
  # unlink(file.path(model, "output"), recursive = TRUE)
  out_l[[length(out_l) + 1]] <- tmp
}

lapply(out_l, ncol)
wtemp_sim_restart <- do.call(rbind, out_l)
head(wtemp_sim_restart[, 1:5], 28)
wtemp_sim_restart <- wtemp_sim_restart[-which(duplicated(wtemp_sim_restart$datetime)), ]

wtmp <- get_output(yaml, model, vars, obs_depths = z_out, out_time = out_time, run_success = TRUE)$temp

windows()
par(mfrow = c(2,1))
wtr.lineseries(out_cont)
# wtr.lineseries(wtemp_restart)
wtr.lineseries(wtmp)

idx <- which(out_cont$wtr_0 < out_cont$wtr_0.5)
out_cont[idx, 1:5]
(out_cont[idx, 2] - out_cont[idx, 3])

