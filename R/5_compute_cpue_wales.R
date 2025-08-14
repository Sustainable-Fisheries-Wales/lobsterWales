# script for computing nominal total catch, effort, landings, CPUE, and LPUE for the observer dataset - wales

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", 
              "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source( file = "1a_observer_dataprocessing_wales.R" )

# read in data
observer_data_lobster <- readr::read_csv("processed_data/wales/observer_data_lobster_clean.csv", 
                                         col_types = readr::cols(ices_sub_rect = readr::col_character(),
                                                                 lat = readr::col_double())) |> 
  dplyr::glimpse()

# function to compute nominal catch, landings, cpue, and lpue per fishing trip 
compute_cpue.lpue <- function(data) {

  # per fishing trip
  # compute effort (number of pots lifted per trip)
  observer_data_effort <- data |> 
    dplyr::group_by(string, trip) |> 
    dplyr::summarise(pots_per_string = max(pots_per_string)) |>
    dplyr::group_by(trip) |> 
    dplyr::summarise(nominal.effort = sum(pots_per_string, na.rm = FALSE)) 
  
  # compute total catch (kg) per trip
  observer_data_catch <- data |> 
    dplyr::group_by(trip) |> 
    dplyr::summarise(nominal.catch = sum(mass, na.rm = TRUE)) 
  
  # compute total landings (kg)
  observer_data_landing <- data |> 
    dplyr::group_by(trip) |> 
    dplyr::filter(landed==1) |> 
    dplyr::summarise(nominal.landing = sum(mass, na.rm = TRUE)) 
  
  # merge datasets and add vessel info
  observer_data_trip <- observer_data_effort |> 
    list(observer_data_catch, observer_data_landing) |>
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(nominal.cpue = nominal.catch/nominal.effort, 
                  nominal.lpue = nominal.landing/nominal.effort) #|>
  observer_data_select_trip <- data |> 
    dplyr::group_by(trip) |> 
    dplyr::reframe(vessel_len = unique(vessel_len), 
                   loc = unique(loc), 
                   species = unique(species)) # trip-level info
  observer_data_trip <- observer_data_trip |> 
    dplyr::left_join(observer_data_select_trip, by = c("trip")) |>
    tidyr::separate_wider_delim(cols = trip, delim = "|", names = c("fisher", "date")) |>
    dplyr::mutate(date = as.Date(date), 
                  month = lubridate::month(date), 
                  quarter = lubridate::quarter(date), 
                  year = lubridate::year(date))
  
  # per pot set (w/ unique gps coordinates)
  # compute total number of pots set in each location in each trip
  observer_data_potset <- data |> 
    dplyr::group_by(string, lat_lon, trip) |> 
    dplyr::summarise(pots_per_string = max(pots_per_string)) |>
    dplyr::group_by(trip, lat_lon) |> 
    dplyr::summarise(potset = sum(pots_per_string, na.rm = FALSE)) 
  
  # compute total catch (kg) per each location in each trip
  observer_data_catch_potset <- data |> 
    dplyr::group_by(lat_lon, trip) |> 
    dplyr::summarise(nominal.catch_potset = sum(mass, na.rm = TRUE)) 
  
  # compute total landings (kg) per each location in each trip
  observer_data_landing_potset <- data |> 
    dplyr::group_by(lat_lon, trip) |> 
    dplyr::filter(landed==1) |> 
    dplyr::summarise(nominal.landing_potset = sum(mass, na.rm = TRUE)) 
  
  # merge all datasets
  observer_data_potset <- observer_data_potset |> 
    list(observer_data_catch_potset, observer_data_landing_potset) |> 
    purrr::reduce(dplyr::left_join) |>
    dplyr::mutate(nominal.cpue_potset = nominal.catch_potset/potset, 
                  nominal.lpue_potset = nominal.landing_potset/potset) |>
    tidyr::separate_wider_delim(cols = lat_lon, delim = "|", names = c("lat", "lon")) |>
    tidyr::separate_wider_delim(cols = trip, delim = "|", names = c("fisher", "date")) |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::mutate(lat = as.numeric(lat), 
                  lon = as.numeric(lon), 
                  month = lubridate::month(date), 
                  quarter = lubridate::quarter(date), 
                  year = lubridate::year(date))
  
  # add covariate info
  observer_data_select_potset <- data |> 
    dplyr::group_by(fisher, date, lat, lon) |> 
    dplyr::reframe(vessel_len = unique(vessel_len), 
                   loc = unique(loc), 
                   ices_sub_rect = unique(ices_sub_rect), 
                   species = unique(species), 
                   lowest_tide = unique(lowest_tide), 
                   temp = unique(temp), 
                   temp_interpol_dist = unique(temp_interpol_dist), 
                   dist_shore = unique(dist_shore), 
                   roughness = unique(rough), 
                   rough_interpol_dist = unique(rough_interpol_dist), 
                   sediment = unique(sed), 
                   sed_interpol_dist = unique(sed_interpol_dist)) # potset-level info
  observer_data_potset <- observer_data_potset |> 
    dplyr::left_join(observer_data_select_potset, by = c("fisher", "date", "lat", "lon")) 
  return(list(observer_data_trip, observer_data_potset))
}

# apply the function to each stock
observer_data_lobster_out <- compute_cpue.lpue(observer_data_lobster) # lobster
observer_data_crab_out <- compute_cpue.lpue(observer_data_crab) # crab

# apply the function to each stock for under 10m
observer_data_lobster_u10 <- observer_data_lobster |> dplyr::filter(vessel_len <= 10)
observer_data_crab_u10 <- observer_data_crab |> dplyr::filter(vessel_len <= 10)
observer_data_lobster_out_u10 <- compute_cpue.lpue(observer_data_lobster_u10) # lobster
observer_data_crab_out_u10 <- compute_cpue.lpue(observer_data_crab_u10) # crab

# export output as rds (as a list)
readr::write_rds(observer_data_lobster_out, file = "processed_data/wales/observer_data_lobster_nominal.cpue.rds") # lobster

# export output as csv
readr::write_csv(observer_data_lobster_out[[1]], file = "processed_data/wales/observer_data_lobster_nominal.cpue_trip.csv") 
readr::write_csv(observer_data_lobster_out[[2]], file = "processed_data/wales/observer_data_lobster_nominal.cpue_potset.csv") 

# plot output
# temporal variation
# select a dataset and a parameter
data <- observer_data_lobster_out[[1]]
response <- data$nominal.cpue
response.name <- "nominal catch rate (kg per number of pots hauled)"

mycolors <- c(RColorBrewer::brewer.pal(name = "Paired", n = 12), 
              RColorBrewer::brewer.pal(name = "Set3", n = 7))
(plot1 <- data |> ggplot2::ggplot(ggplot2::aes(lubridate::quarter(date, with_year = TRUE), response, 
                                     group = lubridate::quarter(date, with_year = TRUE))) +
  ggplot2::scale_color_manual(values = mycolors) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(size = 2., ggplot2::aes(lubridate::quarter(date, with_year = TRUE), response,
                                               group = lubridate::quarter(date, with_year = TRUE),
                                               color = as.factor(lubridate::month(date))), alpha = 0.4) +
  ggplot2::xlab("year") +
  ggplot2::ylab(response.name) +
  ggplot2::theme_classic() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 2))))

# export plot
ggplot2::ggsave(file=paste0("plots/wales/", "observer_trends_wales_", response.name, "_", unique(data$species), ".svg"), plot=plot1, width=12, height=8)


# spatial distribution
# select a dataset and a parameter
data <- observer_data_lobster_out[[2]]
response <- data$nominal.cpue_potset
response.name <- "nominal cpue (kg per number of pots hauled)"
xlim <- c(min(data$lon, na.rm = TRUE)*1.05,
          max(data$lon, na.rm = TRUE)*0.85)
ylim <- c(min(data$lat, na.rm = TRUE)*.995,
          max(data$lat, na.rm = TRUE)*1.01)

# wales shape file
wales <- sf::read_sf(dsn = "data/wales/wnmp_areas/wnmp_areas.shp", stringsAsFactors = FALSE)

# icea rectangles
shp_ices.rec <- sf::read_sf(dsn = "data/ICES_Rect/ICES_Statistical_Rectangles_Eco.shp", stringsAsFactors = FALSE)

# read in ICES rectangles for wales
ices_rec <- readr::read_delim(file = "data/wales/ices_rectangles_wales.csv")
ices_rec_wales <- ices_rec |> 
  dplyr::filter(proportion != 0)
ices_rec_wales <- ices_rec_wales |> 
  janitor::clean_names()

# subset wales
shp_ices.rec_wales <- shp_ices.rec |> 
  dplyr::right_join(ices_rec_wales, by = c("ICESNAME"="ices_rectangle")) |>
  dplyr::mutate(PERCENTAGE = PERCENTAGE*proportion)

xlim <- c(min(shp_ices.rec_wales$WEST, na.rm = TRUE),
          max(shp_ices.rec_wales$EAST, na.rm = TRUE))
ylim <- c(min(shp_ices.rec_wales$SOUTH, na.rm = TRUE),
          max(shp_ices.rec_wales$NORTH, na.rm = TRUE))

data <- data |> dplyr::filter((lon >= min(shp_ices.rec_wales$WEST)) & (lon <= max(shp_ices.rec_wales$EAST)) & 
                                (lat >= min(shp_ices.rec_wales$SOUTH)) & (lat <= max(shp_ices.rec_wales$NORTH)))
(plot2 <- ggplot2::ggplot() +  
  ggplot2::scale_color_manual(values = mycolors) +
    ggplot2::geom_sf(data = wales, ggplot2::aes(fill = name), alpha = 0.2,  colour = "black") +
    ggplot2::geom_sf(data = shp_ices.rec_wales, fill = NA, colour = "darkblue") +
    ggplot2::scale_fill_manual(values = mycolors) +
    #ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  ggplot2::geom_point(data=data, 
                      ggplot2::aes(x=lon, y=lat, size = nominal.lpue_potset, color=as.factor(month)), 
                      alpha=I(0.3)) + 
  ggplot2::theme_bw() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "top",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
  ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
  ggplot2::facet_wrap(~ year, 
                      strip.position = "top", ncol = 3))

# export plot
ggplot2::ggsave(file=paste0("plots/wales/", response.name, "_observer_space_wales_filter2_",  unique(data$species), ".svg"), plot=plot2, width=12, height=8)

# size structure
# select a dataset
observer_data_lobster_no.na <- observer_data_lobster |> 
  dplyr::filter(!is.na(sex))
observer_data_crab_no.na <- observer_data_crab |> 
  dplyr::filter(!is.na(sex))
sex.label <- c("male", "female")
names(sex.label) <- c(0, 1)

(plot3 <- ggplot2::ggplot(observer_data_lobster_no.na, 
                          ggplot2::aes(x = carapace_wdth, 
                                       y = as.factor(year))) +
  ggridges::geom_density_ridges(scale = 2.5, 
                                alpha = 0.3, 
                                quantile_lines = TRUE, 
                                quantiles = 0.5, 
                                ggplot2::aes(fill = as.factor(sex))) +
  ggplot2::xlab("carapace width (mm)") +
  ggplot2::ylab("year") +
  ggplot2::scale_fill_manual(labels = c("male", "female"), 
                             values = c("darkblue", "darkred")) +
  ggplot2::theme_classic() + 
  ggplot2::coord_flip() +
  ggplot2::theme( 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    axis.title.x = ggplot2::element_text(size=10),
    axis.title.y = ggplot2::element_text(size=10),	
    axis.text.x = ggplot2::element_text(size=8), 
    axis.text.y = ggplot2::element_text(size=8),
    legend.background = ggplot2::element_blank(),
    legend.position = "none",
    legend.title = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(colour="black", size = 8),
    plot.title = ggplot2::element_text(hjust = 0.4, size = 4),
    legend.key = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(), 
    strip.placement = "outside",
    strip.text.x = ggplot2::element_text(size = 10, colour = "darkblue")) +  
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))  +
  ggplot2::facet_wrap(~ sex, 
                      labeller = ggplot2::labeller(sex = sex.label), 
                      strip.position = "top", ncol = 3))

# export plot
ggplot2::ggsave(file=paste0("plots/wales/size.comp_observer_wales_", unique(data$species), ".svg"), plot=plot3, width=12, height=8)
