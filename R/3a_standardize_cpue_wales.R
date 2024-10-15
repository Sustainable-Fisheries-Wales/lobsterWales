# script for standardizing cpue using VAST - wales

# check if required packages are installed
required <- c("TMB", "readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)
install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
devtools::install_github("james-thorson/FishStatsUtils@main", INSTALL_opts="--no-staged-install")
devtools::install_github("james-thorson/VAST@main", INSTALL_opts="--no-staged-install")
library(VAST)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source(file = "2a_compute_cpue_wales.R" )

# read in data
observer_data_lobster <- readr::read_csv("processed_data/wales/observer_data_lobster_nominal.cpue_potset.csv",
                                         col_types = readr::cols(ices_sub_rect = readr::col_character())) |> 
  dplyr::glimpse()

# reformat datasets
observer_data_lobster$ices_rect <- mapplots::ices.rect2(observer_data_lobster$lon, observer_data_lobster$lat)
cpue_data_lobster <- observer_data_lobster |> 
  dplyr::filter(!is.na(potset)) |> dplyr::filter(ices_rect!="NANANA") |>
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, qtr, lat, lon, fisher, ices_rect, nominal.cpue_potset, areaSwept_km2) |>
  dplyr::rename(Lat = lat,
                Lon = lon,
                Year = year,
                season = qtr,
                vessel = fisher,
                icesRect = ices_rect,
                vessel = fisher,
                cpue_kg_pot = nominal.cpue_potset,
                areaSwept_km2 = areaSwept_km2,
                tide = lowest_tide,
                temperature = temp,
                roughness = roughness) 

# density covaraites
covariate_data <- observer_data_lobster |>
  dplyr::filter(!is.na(potset)) |> dplyr::filter(ices_rect!="NANANA") |>
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, lat, lon, month, quarter, fisher, nominal.cpue_potset, areaSwept_km2, lowest_tide, 
                vessel_len, dist_shore, sediment, temp, roughness) |>
  dplyr::rename(Lat = lat,
                Lon = lon,
                month = month,
                season = quarter,
                Year = year,
                vessel = fisher,
                vesselSize = vessel_len,
                cpue_kg_pot = nominal.cpue_potset,
                areaSwept_km2 = areaSwept_km2,
                tide = lowest_tide,
                temperature = temp,
                roughness = roughness,
                shoreDistance = dist_shore)

# catchability covariates
catchability_data <- observer_data_lobster |>
  dplyr::filter(!is.na(potset)) |> dplyr::filter(ices_rect!="NANANA") |>
  dplyr::mutate(areaSwept_km2 = 1) |>
  dplyr::arrange(year, month) |>
  dplyr::select(year, lat, lon, fisher, month, quarter, ices_rect, nominal.cpue_potset, areaSwept_km2, vessel_len, dist_shore, lowest_tide) |>
  dplyr::rename(Lat = lat,
                Lon = lon,
                Year = year,
                month = month,
                season = quarter,
                vesselSize = vessel_len,
                vessel = fisher,
                cpue_kg_pot = nominal.cpue_potset,
                areaSwept_km2 = areaSwept_km2,
                icesRect = ices_rect,
                tide = lowest_tide,
                shoreDistance = dist_shore) 
covariate_data_sub <- covariate_data |> 
  dplyr::filter(Year > 2020) |> # no data from 2020
  dplyr::select(Year, month, Lat, Lon, vessel, cpue_kg_pot, temperature, areaSwept_km2) |>
  na.omit()
catchability_data_sub <- catchability_data |>
  dplyr::filter(Year > 2020) |>
  dplyr::select(Year, month, Lat, Lon, vessel, cpue_kg_pot, vesselSize, shoreDistance, areaSwept_km2) |>
  na.omit()

# Creating an extrapolation grid
shp <- rgdal::readOGR(dsn = "data/wales/wnmp_areas/wnmp_areas.shp", stringsAsFactors = FALSE)
sps <- spTransform(shp, CRS("+proj=longlat +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs +ellps=WGS84 +towgs84=0,0,0 "))
lon <- sum(bbox(sps)[1,])/2
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL
LL <- readRDS('processed_data/wales/user_region.rds')
region_extent <- data.frame(long=LL$Lon+2, lat=LL$Lat)
str(region_extent)
crs_UTM <- CRS(paste0("+proj=utm +zone=", utmzone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

# Construct the extroplation grid 
cell_size <- 2000
region_grid <- sf::st_make_grid(region_polygon, cellsize = cell_size, what = "centers")

# Convert region_grid to Spatial Points 
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")

# combine shapefile data with Spatial Points
region_grid_sp@data <- over(region_grid, region_polygon)

# Convert back to coordinates
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, 
                             fid,
                             Area_km2=((cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))
# use only the inshore area
region_df <- region_df |> 
  dplyr::mutate(fid = dplyr::case_when(fid == 2 ~ NA,
                                       fid == 1 ~ 1, 
                                       fid == NA ~ NA))
region <- subset(region_df, !is.na(fid))
str(region)

# export the region file
saveRDS(region, file = "processed_data/wales/user_region.rds")

# plot the grid
png('processed_data/wales/user_region.png', width=7, height=7, units='in', res=200)
par(mfrow=c(2,2))
with(region_extent, plot(long, lat, main='Extent in points in LL'))
plot(region_polygon, main='Polygon in UTM', axes=TRUE)
plot(region_grid, col=ifelse(is.na(region_df$fid), 'red', 'black'),
     axes=TRUE, main='Extrapolation area UTM')
with(region, plot(Lon, Lat, main='Extrapolation region in LL', pch='.'))
dev.off()

# add density & catchability covariates
# define formula
# encounter rate
X1_formula = ~ poly(log(temperature), degree=2) + roughness 
Q1_formula = ~ log(vesselSize) + shoreDistance
X1config_cp = array(3, dim=c(1, 3))
Q1config_k = c(3, 3)

# positive catch rate
X2_formula = ~ poly(log(temperature), degree=2) 
Q2_formula = ~ log(vesselSize) + shoreDistance 
X2config_cp = array(3,dim=c(1, 3))
Q2config_k = c(3, 3) 

# rescale covariates for numerical stability
covariate_data_sub[,'temperature'] = covariate_data_sub[,'temperature'] / 100
covariate_data_sub[,'roughness'] = covariate_data_sub[,'roughness'] / 100
catchability_data_sub[,'vesselSize'] = catchability_data_sub[,'vesselSize'] / 100
catchability_data_sub[,'shoreDistance'] = catchability_data_sub[,'shoreDistance'] / 100

# make settings
dat <- catchability_data_sub
settings <- FishStatsUtils::make_settings(n_x = 1000, 
                                          FieldConfig = c("Omega1" = 1, "Epsilon1" = 1, "Omega2" = 1, "Epsilon2" = 1),  
                                          RhoConfig = c("Beta1" = 2, "Epsilon1" = 0, "Beta2" = 0, "Epsilon2" = 0),
                                          ObsModel = c(4, 0), 
                                          OverdispersionConfig = c("Eta1" = 0, "Eta2" = 0), 
                                          Options = c('treat_nonencounter_as_zero' = FALSE,
                                                      "report_additional_variables" = TRUE), 
                                          Region = 'User',
                                          purpose = "index2", 
                                          use_anisotropy = TRUE,
                                          bias.correct = FALSE,
                                          fine_scale = TRUE#, 
                                          #knot_method='grid'
                                          )
user_region <- readRDS('processed_data/wales/user_region.rds')
run_dir <- paste0(getwd(),"processed_data/wales/vast/lobster")
dir.create(run_dir,recursive=TRUE)

# run model
fit <- FishStatsUtils::fit_model(settings = settings,
                                     Lat_i = dat$Lat, 
                                     Lon_i = dat$Lon,
                                     t_i = dat$Year, 
                                     b_i = dat$cpue_kg_pot,
                                     a_i = dat$areaSwept_km2,
                                     v_i = dat$vessel,
                                     covariate_data = as.data.frame(covariate_data_sub),
                                     X1_formula = X1_formula,
                                     X2_formula = X2_formula,
                                     X1config_cp = X1config_cp,
                                     X2config_cp = X2config_cp,
                                     catchability_data = catchability_data_sub,
                                     Q1_formula = Q1_formula,
                                     Q2_formula = Q2_formula,
                                     Q1config_k = Q1config_k,
                                     Q2config_k = Q2config_k,
                                     #Aniso=FALSE,
                                     input_grid = user_region,
                                     working_dir = run_dir)
plot(fit,
     TmbData = fit$data_list,
     settings = settings,
     #check_residuals = FALSE,
     n_samples = 0)
check_fit(fit$parameter_estimates, check_gradients = TRUE, quiet = FALSE)

# reformat cpue data for ss
# yr month fleet obs stderr
abundance_index <- readr::read_csv("processed_data/wales/vast/lobster/index.csv") 
abundance_index <- abundance_index |>
  dplyr::select(Time, Estimate, "Std. Error for Estimate") |>
  dplyr::mutate(year = Time,
                month = 12,
                fleet = 1,
                obs = Estimate,
                stdrr = "Std. Error for Estimate") 

# export the file
readr::write_csv(abundance_index, file = "processed_data/wales/observer_data_lobster_wales_abundance.index_ss.csv") 
