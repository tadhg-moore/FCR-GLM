folder = "."
file <- file.path(folder, "Simstrat", "output", "k_out.dat")

read_simstrat_out <- function(file, par_file = "Simstrat/simstrat.par", obs_depths = NULL, folder = ".") {
  
  cnam <- strsplit(basename(file), "_")[[1]][1]
  
  timestep <- get_json_value(file.path(folder, par_file), "Simulation", "Timestep s")
  reference_year <- get_json_value(file.path(folder, par_file), "Simulation", "Start year")
  
  temp <- read.table(file, header = TRUE,
                     sep = ",", check.names = FALSE)
  temp[, 1] <- as.POSIXct(temp[, 1] * 3600 * 24, origin = paste0(reference_year, "-01-01"))
  # In case sub-hourly time steps are used, rounding might be necessary
  temp[, 1] <- lubridate::round_date(temp[, 1], unit = lubridate::seconds_to_period(timestep))
  
  # First column datetime, then depth from shallow to deep
  temp <- temp[, c(1, ncol(temp):2)]
  
  # Remove columns without any value
  temp <- temp[, colSums(is.na(temp)) < nrow(temp)]
  
  # Add in obs depths which are not in depths and less than mean depth
  mod_depths <- as.numeric(colnames(temp)[-1])
  if(is.null(obs_depths)){
    obs_dep_neg <- NULL
  }else{
    obs_dep_neg <- -obs_depths
  }
  add_deps <- obs_dep_neg[!(obs_dep_neg %in% mod_depths)]
  depths <- c(add_deps, mod_depths)
  depths <- depths[order(-depths)]
  
  if(length(depths) != (ncol(temp) - 1)){
    message("Interpolating Simstrat temp to include obs depths... ",
            paste0("[", Sys.time(), "]"))
    
    
    # Create empty matrix and interpolate to new depths
    wat_mat <- matrix(NA, nrow = nrow(temp), ncol = length(depths))
    for(i in seq_len(nrow(temp))) {
      y <- as.vector(unlist(temp[i, -1]))
      wat_mat[i, ] <- approx(mod_depths, y, depths, rule = 2)$y
    }
    message("Finished interpolating! ",
            paste0("[", Sys.time(), "]"))
    df <- data.frame(wat_mat)
    df$datetime <- temp[, 1]
    df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
    colnames(df) <- c("datetime", paste0("wtr_", abs(depths)))
    temp <- df
  }else{
    # Set column headers
    str_depths <- abs(as.numeric(colnames(temp)[2:ncol(temp)]))
    colnames(temp) <- c("datetime", paste0(cnam, "_", str_depths))
  }
  return(temp)
}

