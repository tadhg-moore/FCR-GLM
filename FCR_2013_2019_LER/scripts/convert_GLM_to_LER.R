
library(glmtools)
library(LakeEnsemblR)
library(gotmtools)
library(readr)

ler_dir <- "FCR_2013_2019_LER"
glm_dir <- "FCR_2013_2019GLMHistoricalRun_GLMv3beta"


nml_file <- file.path(glm_dir, "glm3.nml")
nml <- read_nml(nml_file)

# Hypsograph data
bath <- get_hypsography(nml)
colnames(bath) <- c("Depth_meter", "Area_meterSquared")
write.csv(bath, "FCR_2013_2019_LER/hypsograph.csv", row.names = FALSE, quote = FALSE)

# Temperature Profile
obs <- read.csv("FCR_2013_2019GLMHistoricalRun_GLMv3beta/field_data/CleanedObsTemp.csv")
obs[, 1] <- format(as.POSIXct(obs[, 1], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
colnames(obs) <- c("datetime", "Depth_meter", "Water_Temperature_celsius")
write.csv(obs, "FCR_2013_2019_LER/wtemp_observations_2013_2019.csv", row.names = FALSE, quote = FALSE)

# Met file
met <- read.csv("FCR_2013_2019GLMHistoricalRun_GLMv3beta/inputs/FCR_GLM_NLDAS_010113_123119_GMTadjusted.csv")
met[, 1] <- format(as.POSIXct(met[, 1], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
met$Rain <- (met$Rain * 1000) / (60)
colnames(met) <- c("datetime", "Shortwave_Radiation_Downwelling_wattPerMeterSquared", "Longwave_Radiation_Downwelling_wattPerMeterSquared", "Air_Temperature_celsius", "Relative_Humidity_percent", "Ten_Meter_Elevation_Wind_Speed_meterPerSecond", "Precipitation_millimeterPerHour")
write.csv(met, "FCR_2013_2019_LER/met_1hr_2013_2019.csv", row.names = FALSE, quote = FALSE)

# Inflow files
infl_fils <- strsplit(get_nml_value(nml, "inflow_fl"), ",")[[1]]
inf_list <- list()
for(i in infl_fils) {
  inf <- read.csv(file.path(glm_dir, i))
  inf[, 1] <- format(as.POSIXct(inf[, 1], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  inf <- inf[, c("time", "FLOW", "TEMP", "SALT")]
  colnames(inf) <- c("datetime", "Flow_metersCubedPerSecond", "Water_Temperature_celsius", "Salinity_practicalSalinityUnits")
  # print(summary(inf))
  inf_list[[i]] <- inf
}
inf <- inf_list[[1]]
inf$Flow_metersCubedPerSecond <- inf$Flow_metersCubedPerSecond + inf_list[[2]]$Flow_metersCubedPerSecond + inf_list[[2]]$Flow_metersCubedPerSecond
write.csv(inf, file.path(ler_dir, "/inflows_daily_2013_2019.csv"), row.names = FALSE, quote = FALSE)

# LakeEnsemblR
get_template("LakeEnsemblR_config", ler_dir)
config_file <- file.path(ler_dir, "LakeEnsemblR.yaml")

input_yaml(config_file, "location", "name", "FallingCreekReservoir")
input_yaml(config_file, "location", "latitude", get_nml_value(nml, "latitude"))
input_yaml(config_file, "location", "longitude", get_nml_value(nml, "longitude"))
input_yaml(config_file, "location", "elevation", max(get_nml_value(nml, "H")))
input_yaml(config_file, "location", "depth", max(bath$Depth_meter))
input_yaml(config_file, "location", "hypsograph", "hypsograph.csv")
input_yaml(config_file, "location", "init_depth", max(bath$Depth_meter))

input_yaml(config_file, "time", "start", obs[1, 1])
input_yaml(config_file, "time", "stop", obs[nrow(obs), 1])


input_yaml_multiple(config_file, key1 = "observations", key2 = "temperature", key3 = "file", "wtemp_observations_2013_2019.csv")
input_yaml_multiple(config_file, key1 = "input", key2 = "meteo", key3 = "file", "met_1hr_2013_2019.csv")
input_yaml_multiple(config_file, key1 = "input", key2 = "light", key3 = "Kw", get_nml_value(nml, "Kw"))

input_yaml(config_file, "inflows", "file", "inflows_daily_2013_2019.csv")

input_yaml_multiple(config_file, key1 = "model_parameters", key2 = "FLake", key3 = "fetch_lk", get_nml_value(nml, "bsn_len"))
input_yaml_multiple(config_file, key1 = "model_parameters", key2 = "GLM", key3 = "bsn_len", get_nml_value(nml, "bsn_len"))
input_yaml_multiple(config_file, key1 = "model_parameters", key2 = "GLM", key3 = "bsn_wid", get_nml_value(nml, "bsn_wid"))
