# data entry check for the observer data - wales

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "pointblank", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
# set a working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
# set a file name
filename <- "Onboard Observer - GPS included.csv"
observer_data_unprocessed <- readr::read_csv(paste0("data/wales/", filename), 
                                             col_types = readr::cols(ICES_sub_rect = readr::col_character())) |>  
  dplyr::select(-...39) |> dplyr::glimpse()

# check & clean colnames
observer_data_unprocessed |> names()
colnames(observer_data_unprocessed) <- observer_data_unprocessed |> janitor::clean_names() |> names() 
colnames(observer_data_unprocessed) <- c("day", "month", "year", "timeframe", "fisher", "vessel_len", "observer", "loc", "lat", "lon", "end_lat", "end_lon", 
                                         "ices_sub_rect", "string", "pot", "pots_per_string", "escape_gaps", "species", "sex", "carapace_wdth", "abdomen_wdth",           
                                         "claw_len", "claw_ht", "claw_dpth", "mass", "berried", "shell_cond", "damage_cond", "damage_detail", "moult_stage", 
                                         "black_spot", "v_notch", "landed", "pleopod_tube", "notes", "tag_num", "tag_col", "gps_release", "soak_time", 
                                         "pot_info", "more_notes", "lowest_tide", "temp", "temp_interpol_dist", "dist_shore", "roughness", 
                                         "rough_interpol_dist", "sediment", "sed_interpol_dist") 
# quick look at summary
observer_data_unprocessed |> summary() 
#observer_data_unprocessed |> pointblank::scan_data()

# check for dat entry errors
al <- pointblank::action_levels(warn_at = 0.2,
                    stop_at = 0.8,
                    notify_at = 0.5)
(observer_data_checked <- observer_data_unprocessed |>
  pointblank::create_agent(
    #tbl = observer_data_unprocessed,
    tbl_name = "observer_data_unprocessed_tbl",
    label = "initial data entry check",
    actions = al
  ) |>
    pointblank::col_is_numeric(day) |>
    pointblank::col_is_numeric(month) |>
    pointblank::col_is_numeric(year) |>
    pointblank::col_is_numeric(timeframe) |>
    pointblank::col_is_character(fisher) |>
    pointblank::col_is_numeric(vessel_len) |>
    pointblank::col_is_character(observer) |>
    pointblank::col_is_character(loc) |>
    pointblank::col_vals_between(lat, left = 50, right = 55, na_pass = TRUE) |>
    pointblank::col_vals_between(lon, left = -5.5, right = -3.0, na_pass = TRUE) |>
    pointblank::col_vals_between(end_lat, left = 50, right = 55, na_pass = TRUE) |>
    pointblank::col_vals_between(end_lon, left = -5.5, right = -3.0, na_pass = TRUE) |>
    pointblank::col_vals_in_set(ices_sub_rect, set = c("32E44", "32E47", "34E51", "34E54", "34E52", 
                                                       "36E66", "35E64", "35E61", "35E55", "35E57", 
                                                       "35E58", "34E55", "35E56", "35E62", "33E57", 
                                                       "36E63", "32E59", "36E69", "32E63", "32E56")) |>
    pointblank::col_is_numeric(string) |>
    pointblank::col_is_numeric(pot) |>
    pointblank::col_is_numeric(pots_per_string) |>
    pointblank::col_vals_in_set(escape_gaps, set = c("N", "Y")) |>
    pointblank::col_vals_in_set(species, set = c("H.gammarus", "C.pagurus")) |>
    pointblank::col_vals_in_set(sex, set = c("M", "F")) |>
    pointblank::col_is_numeric(carapace_wdth) |>
    pointblank::col_is_numeric(abdomen_wdth) |>
    pointblank::col_is_numeric(claw_len) |>
    pointblank::col_is_numeric(claw_ht) |>
    pointblank::col_is_numeric(claw_dpth) |>
    pointblank::col_is_numeric(mass) |>
    pointblank::col_vals_in_set(berried, set = c("N", "Y")) |>
    pointblank::col_is_factor(shell_cond) |>
    pointblank::col_is_factor(moult_stage) |>
    pointblank::col_vals_in_set(v_notch, set = c("N", "Y")) |>
    pointblank::col_vals_in_set(landed, set = c("N", "Y")) |>
    pointblank::col_is_numeric(soak_time) |>
    pointblank::col_is_numeric(lowest_tide) |>
    pointblank::col_is_numeric(temp) |>
    pointblank::col_is_numeric(temp_interpol_dist) |>
    pointblank::col_is_numeric(dist_shore) |>
    pointblank::col_is_numeric(roughness) |>
    pointblank::col_is_numeric(rough_interpol_dist) |>
    pointblank::col_is_numeric(sediment) |>
    pointblank::col_is_numeric(sed_interpol_dist) |>
    pointblank::interrogate())
