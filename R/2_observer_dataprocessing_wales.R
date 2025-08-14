# script for computing nominal total catch, effort, landings, CPUE, and LPUE for the observer dataset. - wales

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
# set a working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
# set a filename
filename <- "Welsh_Observer_Data_2019-2024.csv"
observer_data <- readr::read_csv(paste0("data/wales/",filename),
                                 col_types = readr::cols(Longitude = readr::col_number(),
                                                         Weight = readr::col_number(),
                                                         ICES_sub_rect = readr::col_character()))

# delete empty columns if any
observer_data <- observer_data |> 
  dplyr::select(-...41, -...56) |> 
  dplyr::glimpse() |> 
  janitor::clean_names() 
dplyr::glimpse(observer_data)
colnames(observer_data)
colnames(observer_data) <- c("day", "month", "year", "timeframe",  "fisher",  "vessel_len", "observer", "depart_port",
                             "return_port", "loc", "lat", "lon", "end_lat", "end_lon", "ices_sub_rect", "string",  
                             "pot", "pots_per_string", "escape_gaps", "species", "sex", "carapace_wdth", 
                             "abdomen_width",  "claw_len", "claw_ht", "claw_dpth", "mass", "berried", "crusty_cond", 
                             "damage_cond", "damage",  "moult_stage", "black_spot", "v_notch",  "landed", "notes", 
                             "pleopod_tube", "tag_no", "tag_col", "gps_release", "soak_time", "start_time", "end_time", 
                             "pot_info", "lowest_tide", "temp", "temp_interpol_dist", "dist_shore", "rough", 
                             "rough_interpol_dist", "sediment",  "sed_interpol_dist", "sed16", "sed7")

# data cleaning
# convert ices rectangles to coordinates
observer_data <- observer_data |> dplyr::mutate(lon_ices = mapplots::ices.rect(ices_sub_rect)$lon, 
                                                lat_ices = mapplots::ices.rect(ices_sub_rect)$lat) 

# replace records w/o coords w/ices-rect coords 
for (i in 1:nrow(observer_data)) {
  if (is.na(observer_data$lat[i])) {
    observer_data$lat[i] <- observer_data$lat_ices[i]
    observer_data$lon[i] <- observer_data$lon_ices[i]
  }
}

# clean up data
observer_data <- observer_data |> 
  dplyr::mutate(species = dplyr::recode(species, C.Pagurus = 'C.pagurus', H.Gammarus = 'H.gammarus')) |> 
  dplyr::mutate(month = dplyr::recode(month, April=4, August=8, December=12, February=2, January=1, 
                                      July=7, June=6, March=3, May=5, November=11, October=10, September=9)) |>
  dplyr::mutate(berried = dplyr::recode(berried, Y=1, N=0, y=1, n=0, "Y - CAST" = 1, CAST=1, Cast=1, Shedding=0)) |>
  dplyr::mutate(landed = dplyr::recode(landed, Y=1, N=0, y=1, Bait=2)) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0)) |>
  tidyr::unite(date, c(day, month, year), sep = "-", remove = FALSE) |> 
  dplyr::mutate(date = as.Date(date, format = "%d-%m-%Y")) |>  
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  dplyr::mutate(qtr = lubridate::quarter(date, with_year = FALSE)) |> 
  tidyr::unite(qrt.yr, c(year, qtr), sep = "-", remove = FALSE) |> 
  tidyr::unite(trip, c(fisher, date), sep = "|", remove = FALSE) |> 
  tidyr::unite(lat_lon, c(lat, lon), sep = "|", remove = FALSE) |> 
  tidyr::unite(lat_lon_trip, c(lat, lon, trip), sep = "|", remove = FALSE) |> 
  dplyr::mutate(mass = as.numeric(mass), 
                lon = as.numeric(lon), 
                soak_time = as.numeric(soak_time))  

# subset by species
observer_data_lobster <- observer_data |> 
  dplyr::filter(species == "H.gammarus") |> 
  dplyr::glimpse()

# export dataset
readr::write_csv(observer_data_lobster, file = "processed_data/wales/observer_data_lobster_clean.csv") 
