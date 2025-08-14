# script for extracting monthly landings data from the MMO IFISH dataset - wales

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# read in data
# effort data from shapefiles
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")
# ***detailed data only available from 2016-2020 for >15m vessels (for bycatch only)
effort2016 <- as.data.frame(sf::read_sf("analyses/data/total_fishing_effort/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2016_all_gears/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2016_all_gearsPolygon.shp", stringsAsFactors = FALSE))
effort2017 <- as.data.frame(sf::read_sf("analyses/data/total_fishing_effort/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2017_all_gears/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2017_all_gearsPolygon.shp", stringsAsFactors = FALSE))
effort2018 <- as.data.frame(sf::read_sf("analyses/data/total_fishing_effort/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2018_all_gears/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2018_all_gearsPolygon.shp", stringsAsFactors = FALSE)) 
effort2019 <- as.data.frame(sf::read_sf("analyses/data/total_fishing_effort/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2019_all_gears/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2019_all_gearsPolygon.shp", stringsAsFactors = FALSE))
effort2020 <- as.data.frame(sf::read_sf("analyses/data/total_fishing_effort/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2020_all_gears/Total_Hours_Fished_of_gr_than_15m_UK_Vessel_Landings_2020_all_gearsPolygon.shp", stringsAsFactors = FALSE)) 
effort2016$year <- 2016
effort2017$year <- 2017
effort2018$year <- 2018
effort2019$year <- 2019
effort2020$year <- 2020
effort <- dplyr::bind_rows(effort2016, effort2017, effort2018, effort2019, effort2020) |> dplyr::glimpse()

# effort in welsh waters
# read in ICES rectangles for wales
ices_rec <- readr::read_delim(file = "data/wales/ices_rectangles_wales.csv")
ices_rec_wales <- ices_rec |> 
  dplyr::filter(proportion != 0)
effort_wales <- effort |>
  dplyr::filter(ices_id %in% ices_rec_wales$`ICES Rectangle`) |>
  dplyr::left_join(ices_rec_wales, by = c("ices_id"="ICES Rectangle")) 

# all gear types 
effort_wales_select <- effort_wales |> dplyr::select("year", "objectid", "ices_id", "sub_rec", "id", 
                                                     "gndkwh", "genkwh", "gnkwh", "gnckwh", "gnskwh", "gtrkwh",
                                                     "tbnkwh", "tmkwh", "tmskwh", "txkwh", "otkwh", "otbkwh", "tbkwh", "tbbkwh",      
                                                     "otmkwh", "ottkwh", "ptkwh", "ptbkwh",  "ptmkwh", 
                                                     "fixkwh", "fpokwh")
effort_wales_select_long <- effort_wales_select |> 
  tidyr::gather(gear, effort, gndkwh:fpokwh, factor_key = TRUE) |> 
  dplyr::mutate(gear = dplyr::case_when(gear %in% c("gndkwh", "genkwh", "gnkwh", "gnckwh", "gnskwh", 
                                                    "gtrkwh") ~ "Gill nets and entangling nets",
                                        gear %in% c("fixkwh", "fpokwh") ~ "Traps",
                                        gear %in% c("tbnkwh", "tmkwh", "tmskwh", "txkwh", "tbkwh", "tbbkwh",
                                                    "otkwh", "otbkwh", "otmkwh", "ottkwh",   
                                                    "ptkwh", "ptbkwh", "ptmkwh") ~ "Trawls")) 
effort_wales_select_ices.rec <- effort_wales_select_long |>
  dplyr::group_by(gear, year, ices_id) |>
  dplyr::reframe(effort = sum(effort, na.rm = TRUE))

# vessel-level landing data
landings_vessel <- readr::read_csv("processed_data/wales/ifish.landings_wales_vessel_all.csv",
                                   col_types = readr::cols(ices_rectangle = readr::col_character())) 
landings_vessel_over15 <- landings_vessel |> 
  dplyr::filter(year >= 2016) |>
  dplyr::filter(length_group %in% c("15-18m", "24-40m", "18-24m", "Over40m")) #|>
#dplyr::filter(gear_cat != "Traps")
landings_vessel_over15_gear <- landings_vessel_over15 |> 
  dplyr::group_by(species_name, gear_cat, ices_rectangle, year) |>
  dplyr::reframe(landings = sum(live_weight_t))

# subset by species
landings_vessel_over15_gear_effort_lobster <- landings_vessel_over15_gear |>
  dplyr::filter(year <= 2020) |>
  dplyr::left_join(effort_wales_select_ices.rec, by = c("gear_cat"="gear", "ices_rectangle"="ices_id", "year")) |>
  dplyr::filter(species_name == "Lobsters") |>
  dplyr::mutate(nominal.cpue = landings/effort)

# export cpue estimates
readr::write_csv(landings_vessel_over15_gear_effort_lobster, file = "processed_data/wales/ifish_cpue_over15_wales_lobster.csv") 

# plot
mycolors = c(RColorBrewer::brewer.pal(name = "Paired", n = 12), 
             RColorBrewer::brewer.pal(name = "Set3", n = 7) )
landings_vessel_over15_gear_effort_crab |> 
  dplyr::filter(nominal.cpue < 0.015) |>
  ggplot2::ggplot(ggplot2::aes(year, nominal.cpue, group = year)) +
  ggplot2::scale_color_manual(values = mycolors) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::geom_jitter(ggplot2::aes(color=gear_cat, group = year)) +
  ggplot2::xlab("year") +
  #ggplot2::ylab(response.name) +
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
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3)))


# ifish landings: 2008-2022 (files in each folder have different data structures)
files1 <- list.files(path="data/mmo_landings_published.data/2008_2012", pattern=".csv$")
files2 <- list.files(path="data/mmo_landings_published.data/2013_2017", pattern=".csv$")
files3 <- list.files(path="data/mmo_landings_published.data/2018_2022", pattern=".csv$")
allmyData1 <- allmyData2 <- allmyData3 <- NULL
for (i in c(files1)) {
  myData1 <- readr::read_csv(file = paste0("data/mmo_landings_published.data/2008_2012/", i))
  allmyData1 <- data.table::rbindlist(list(allmyData1, myData1)) 
}
allmyData1$fao_area <- NA
colnames(allmyData1) <- allmyData1 |> 
  janitor::clean_names() |> colnames()
allmyData1 <- allmyData1 |> dplyr::select(year, month, vessel_nationality, fao_area, ices_division, 
                                          rectangle, length_group, gear_category, species, species_group,
                                          live_weight_tonnes, landed_weight_tonnes, value_pounds)
for (i in c(files2)) {
  myData2 <- readr::read_csv(file = paste0("data/mmo_landings_published.data/2013_2017/", i))
  allmyData2 <- data.table::rbindlist(list(allmyData2, myData2)) 
}
colnames(allmyData2) <- allmyData2 |> 
  janitor::clean_names()|> colnames()
allmyData2 <- allmyData2 |> dplyr::select( -"species_code", -"species_as_shown_in_publication")
colnames(allmyData2)[9] <- "species"
for (i in c(files3)) {
  myData3 <- readr::read_csv(file = paste0("data/mmo_landings_published.data/2018_2022/", i))
  allmyData3 <- data.table::rbindlist(list(allmyData3, myData3))
}
allmyData3$ices_division <- NA
colnames(allmyData3) <- allmyData3 |> 
  janitor::clean_names()|> colnames()
allmyData3 <- allmyData3 |> 
  dplyr::select(-reported_zone, -estimated_eez_of_capture, -estimated_region_of_capture, -port_of_landing, -port_nationality, -species_code, -landing_type, -tac_code)
allmyData3 <- allmyData3 |> 
  dplyr::select(year, month,  vessel_nationality, fao_division, ices_division, rectangle, length_group, gear_category, species_name, species_group, live_weight_tonnes, landed_weight_tonnes, value_pounds)
colnames(allmyData3)[4] <- "fao_area"
colnames(allmyData3)[9] <- "species"

# merge all landings datasets
ifish_landings <- data.table::rbindlist(list(allmyData1, allmyData2, allmyData3))|> dplyr::glimpse()

# subset welsh landings
ifish_landings_wales <- ifish_landings |> 
  dplyr::filter(rectangle %in% ices_rec_wales$`ICES Rectangle`) |>
  dplyr::left_join(ices_rec_wales, by = c("rectangle"="ICES Rectangle")) |>
  dplyr::mutate(month.chr = dplyr::case_when(month < 10 ~ paste0(0, month), 
                                             month >= 10 ~ as.character(month))) |>
  tidyr::unite(mon_year, c(year, month.chr), sep = "-", remove = FALSE) |> 
  dplyr::mutate(date = as.Date(paste(mon_year, "-01", sep=""))) |>
  dplyr::mutate(qtr = lubridate::quarter(date, with_year = FALSE)) |> 
  tidyr::unite(qrt.yr, c(year, qtr), sep = "-", remove = FALSE) |> 
  dplyr::mutate(live_wt_t = round(live_weight_tonnes*proportion, 4), 
                landed_wt_t = round(landed_weight_tonnes*proportion, 4)) 

# convert ices rectangles to coordinates
ifish_landings_wales <- ifish_landings_wales |> 
  dplyr::mutate(lon = mapplots::ices.rect(rectangle)$lon, lat = mapplots::ices.rect(rectangle)$lat) |> 
  dplyr::glimpse()
  
# subset landings by species
ifish_landings_wales_lobster <- ifish_landings_wales |> 
  dplyr::filter(species %in% c("Lobsters" )) |> 
  dplyr::filter((gear_category == "Pots and traps")) 

# bycatch (crabs and lobsters not caught by pots and traps)
ifish_landings_wales_lobster_bycatch <- ifish_landings_wales |> 
  dplyr::filter(species %in% c("Lobsters" )) |> 
  dplyr::filter(!(gear_category == "Pots and traps")) 

# export output as csv
readr::write_csv(ifish_landings_wales_lobster, file = "processed_data/wales/ifish_landings_wales_lobster_clean.csv") 
readr::write_csv(ifish_landings_wales_lobster_bycatch, file = "processed_data/wales/ifish_landings_wales_lobster_bycatch_clean.csv") 

# aggregate by year
ifish_landings_wales_lobster_annual <- ifish_landings_wales_lobster |> 
  dplyr::group_by(species, year, lon, lat) |> 
  dplyr::summarise(landings = sum(live_weight_tonnes), 
                   econ.value = sum(value_pounds)/1000000)

ifish_landings_wales_lobster_bycatch_annual <- ifish_landings_wales_lobster_bycatch |> 
  dplyr::group_by(species, year, lon, lat) |> 
  dplyr::summarise(landings = sum(live_weight_tonnes), 
                   econ.value = sum(value_pounds)/1000000)

# export output as csv
readr::write_csv(ifish_landings_wales_lobster_annual, file = "processed_data/wales/ifish_landings_wales_lobster_annual_clean.csv") 
readr::write_csv(ifish_landings_wales_lobster_bycatch_annual, file = "processed_data/wales/ifish_landings_wales_lobster_bycatch_annual_clean.csv") 

# exploratory plotting
# temporal trends
data <- ifish_landings_wales_lobster
response <- data$landed_weight_tonnes
response.name <- "nominal landings (t)"

mycolors = c(RColorBrewer::brewer.pal(name = "Paired", n = 12), 
             RColorBrewer::brewer.pal(name = "Set3", n = 7) )
(plot1 <- data |>
    ggplot2::ggplot(ggplot2::aes(fill = length_group, y = response, x = mon_year)) + 
    #ggplot2::scale_x_discrete(name="year", limits=c(min(lubridate::year(data$date), max(lubridate::year(data$date))))) +
    ggsci::scale_fill_jco() +
    ggplot2::geom_bar(position="stack", stat="identity") +
    ggplot2::xlab("year") +
    ggplot2::ylab(response.name) +
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
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))))

# export plot
ggplot2::ggsave(file=paste0("plots/wales/", response.name, "_ifish_trends_wales_", unique(data$species), ".svg"), plot=plot1, width=12, height=8)

# spatial variation
data <- ifish_landings_wales_lobster_annual
response <- data$landings
response.name <- "nominal landings (t)"

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

(plot2 <- ggplot2::ggplot() +  
  ggplot2::scale_color_manual(values = mycolors) +
  #ggsci::scale_color_jco() +
    ggplot2::geom_sf(data = wales, ggplot2::aes(fill = name), alpha = 0.2,  colour = "black") +
    ggplot2::geom_sf(data = shp_ices.rec_wales, fill = NA, colour = "darkgray") +
    ggplot2::scale_fill_manual(values = mycolors) +
  #ggplot2::coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  ggplot2::geom_point(data=data, 
                      ggplot2::aes(x=lon, y=lat, size = landings), 
                      alpha=I(0.3), color = "darkred") + 
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
  ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 3))) +
  ggplot2::guides(fill=ggplot2::guide_legend(title="Month")) +
  ggplot2::facet_wrap(~ year, 
                      strip.position = "top", ncol = 5))

# export plot
ggplot2::ggsave(file=paste0("plots/wales/", response.name, "_ifish_ices.rec_wales_", unique(data$species), ".svg"), plot=plot2, width=12, height=8)
