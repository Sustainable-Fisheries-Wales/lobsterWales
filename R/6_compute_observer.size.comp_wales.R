# script to reformat size composition data as SS model input for the observer data - wales

# check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "RColorBrewer", "rgdal", "sp", 
              "rnaturalearth", "ggplot2", "ggridges")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# run input data processing script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source( file = "1a_observer_dataprocessing_wales.R" )

# read in datasets
observer_data_lobster <- readr::read_csv("processed_data/wales/observer_data_lobster_clean.csv", 
                                         col_types = readr::cols(ices_sub_rect = readr::col_character(),
                                                                 lat = readr::col_double())) |> 
  dplyr::glimpse()
observer_cpue_lobster <- readr::read_csv("processed_data/wales/observer_data_lobster_nominal.cpue_potset.csv", 
                                         col_types = readr::cols(ices_sub_rect = readr::col_character(),
                                                                 lat = readr::col_double())) |> 
  dplyr::glimpse()

# merge datasets
observer_cpue_lobster <- observer_cpue_lobster |> 
  dplyr::group_by(year, month, ices_sub_rect) |>
  dplyr::mutate(nominal.cpue_potset = sum(nominal.cpue_potset, na.rm = TRUE)) |>
  dplyr::mutate(fisher = readr::parse_number(fisher))
observer_data_lobster <- observer_data_lobster |> 
  tidyr::unite(month.yr.rect, c(year, month, ices_sub_rect), sep = "-", remove = FALSE) |> 
  dplyr::left_join(observer_cpue_lobster)

# reformat length composition input data (for SS)
#_yr month fleet sex part Nsamp datavector(female-male) ***separate males and females*** 
data <- observer_data_lobster
colnames(data)[29] <- "length" # recorded in mm -> convert to cm for ss
data <- data |> dplyr::filter(!is.na(sex))
data$length <- data$length/10
(size.min <- round(min(data$length, na.rm = TRUE)))
(size.max <- round(max(data$length, na.rm = TRUE)))
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, 
                                      seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr.rect))) {
  print(i)
  #print(data[data$month.yr==i,])
  subdata <- data[data$month.yr.rect==i,]
  subdata_m <- subdata[subdata$sex==0,]
  subdata_f <- subdata[subdata$sex==1,]
  if (nrow(subdata_m) > 0) {
    size.dist_m[1] <- unique(subdata_m$year)
    size.dist_m[2] <- unique(subdata_m$month)
    size.dist_m[3] <- 1 # fleet
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 #part
    size.dist_m[6] <- nrow(subdata_m) * sum(unique(subdata_m$nominal.cpue_potset/subdata_m$nominal.catch_potset), na.rm = TRUE)
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_m$nominal.cpue_potset/subdata_m$nominal.catch_potset), na.rm = TRUE)
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$month.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f) * sum(unique(subdata_f$nominal.cpue_potset/subdata_f$nominal.catch_potset), na.rm = TRUE)
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), size.max)))  * 
      sum(unique(subdata_f$nominal.cpue_potset/subdata_f$nominal.catch_potset), na.rm = TRUE)
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$month.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}

# aggregate by month
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "month.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_lobster2 <- NULL 
for (i in c(unique(data$month.yr))) {
  subdata <- size.dist_lobster[size.dist_lobster$month.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- unique(subdata_m$month)
    size.dist_m2[3] <- unique(subdata_m$fleet)
    size.dist_m2[4] <- unique(subdata_m$sex)
    size.dist_m2[5] <- unique(subdata_m$part)
    size.dist_m2[6] <- round(sum(as.numeric(subdata_m$nsample)))
    if (nrow(subdata_m) > 1) {
      size.dist_m2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_m[7:ncol(subdata_m)], 2, as.numeric))), digits=1)
    } else size.dist_m2[7:(n.size+6)] <- round(as.numeric(subdata_m[7:ncol(subdata_m)]), digits = 1)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f2[1] <- unique(subdata_f$year)
    size.dist_f2[2] <- unique(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample)))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric))), digits=1)
    } else size.dist_f2[7:(n.size+6)] <- round(as.numeric(subdata_f[7:ncol(subdata_f)]), digits = 1)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m2), as.data.frame(size.dist_f2))
  size.dist_lobster2 <- dplyr::bind_rows(as.data.frame(size.dist_lobster2), as.data.frame(size.dist))
}

# export data
readr::write_csv(size.dist_lobster, file = "processed_data/wales/observer.size.comp.data_lobster_wales_ss.csv") 
