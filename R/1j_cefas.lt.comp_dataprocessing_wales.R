# script for data cleaning for length composition dataset (from CEFAS port sampling) - wales

# ***size comp sampling was not done every month (landings data different from ifish)***

# Check if required packages are installed
required <- c("readr", "dplyr", "lubridate", "tidyr", "janitor")
installed <- rownames(installed.packages())
(not_installed <- required[!required %in% installed])
install.packages(not_installed, dependencies=TRUE)

# subset data
# read in ICES rectangles for wales
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
ices_rec <- readr::read_delim(file = "data/wales/ices_rectangles_wales.csv")
ices_rec_wales <- ices_rec |> 
  dplyr::filter(proportion != 0)

# read in data
port.sampling_lobster <- readr::read_csv("data/wales/port.sampling_lobster_wales_ices_rec_landings.csv", 
                                         col_types = readr::cols(Rectangle = readr::col_character())) |>
  dplyr::rename(SampleNo = ...1) |> dplyr::glimpse()
colnames(port.sampling_lobster) <- c("sample_no", "date_of_landing", "species", "sex", "length", "total_landed_wgt", "wgt_measured", 
                                     "rectangle", "port_of_landing", "total_at_length")

#port.sampling_lobster <- readr::read_csv("data/wales/port.sampling_lobster_wales.csv")  |> dplyr::glimpse()
port.sampling_lobster <- port.sampling_lobster |> janitor::clean_names() 

port.sampling_crab <- readr::read_csv("data/wales/port.sampling_crab_wales_ices_rec_landings.csv", 
                                         col_types = readr::cols(Rectangle = readr::col_character())) |>
  dplyr::rename(SampleNo = ...1) |> dplyr::glimpse()
colnames(port.sampling_crab) <- c("sample_no", "date_of_landing", "port_of_landing","species", "sex", "length", "rectangle",
                                  "total_landed_wgt", "wgt_measured", "total_at_length")
#port.sampling_crab <- readr::read_csv("data/wales/port.sampling_crab_wales.csv") |> dplyr::glimpse() #|>
  #dplyr::mutate(DateLanding = as.POSIXct(DateLanding, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
port.sampling_crab <- port.sampling_crab |> janitor::clean_names() 

# data cleaning
size.data_lobster_wales <- port.sampling_lobster |> 
  dplyr::filter(!is.na(date_of_landing)) |>
  dplyr::mutate(year = lubridate::year(date_of_landing), 
                month = lubridate::month(date_of_landing), 
                quarter = lubridate::quarter(date_of_landing)) |>   
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |> 
  tidyr::unite(qrt.yr, c(year, quarter), sep = "-", remove = FALSE) |>
  tidyr::unite(month.yr.rect, c(year, month, rectangle), sep = "-", remove = FALSE) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, B=1)) 

size.data_lobster_wales_nsample <- size.data_lobster_wales |>
  dplyr::group_by(year, month, sex) |>
  dplyr::reframe(nsample = length(length))

size.data_crab_wales <- port.sampling_crab |> 
  dplyr::filter(!is.na(date_of_landing)) |>
  dplyr::mutate(year = lubridate::year(date_of_landing), 
                month = lubridate::month(date_of_landing), 
                quarter = lubridate::quarter(date_of_landing)) |> 
  tidyr::unite(month.yr, c(year, month), sep = "-", remove = FALSE) |>
  tidyr::unite(qrt.yr, c(year, quarter), sep = "-", remove = FALSE) |>
  tidyr::unite(month.yr.rect, c(year, month, rectangle), sep = "-", remove = FALSE) |> 
  dplyr::mutate(sex = dplyr::recode(sex, F=1, M=0, B=1)) 

size.data_crab_wales_nsample <- size.data_crab_wales |>
  dplyr::group_by(year, month, sex) |>
  dplyr::reframe(nsample = length(length))

# # subset port sampling
# port.sampling_welsh.lobster_port <- port.sampling_lobster2 |> 
#   dplyr::filter(data_source == "Port")
# port.sampling_welsh.crab_port <- port.sampling_crab |> 
#   dplyr::filter(data_source == "Port")
# 
# # subset boat sampling
# port.sampling_welsh.lobster_boat <- port.sampling_lobster2 |> 
#   dplyr::filter(data_source == "Boat")
# port.sampling_welsh.crab_boat <- port.sampling_crab |> 
#   dplyr::filter(data_source == "Boat")

# export datasets
readr::write_csv(size.data_lobster_wales, file = "processed_data/wales/size.data_lobster_wales_clean.csv") 
readr::write_csv(size.data_crab_wales, file = "processed_data/wales/size.data_crab_wales_clean.csv") 
# readr::write_csv(port.sampling_welsh.lobster_port, file = "processed_data/wales/lt.comp_lobster_wales_port_clean.csv") 
# readr::write_csv(port.sampling_welsh.crab_port, file = "processed_data/wales/lt.comp_crab_wales_port_clean.csv")
# readr::write_csv(port.sampling_welsh.lobster_boat, file = "processed_data/wales/lt.comp_lobster_wales_boat_clean.csv") 
# readr::write_csv(port.sampling_welsh.crab_boat, file = "processed_data/wales/lt.comp_crab_wales_boat_clean.csv")


# reformat length composition input data (for SS)
# set up population length bin structure (note - irrelevant if not using size data and using empirical wtatage
#_N_LengthBins; then enter lower edge of each length bin

#_yr month fleet sex part Nsamp datavector(female-male) ***separate males and females*** 

# size.data_lobster_wales
data <- size.data_lobster_wales
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
size.min <- 1#round(min(data$length, na.rm = TRUE)))
size.max <- 20#round(max(data$length, na.rm = TRUE)))
width <- 0.2 
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), size.max))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
size.dist_lobster <- NULL 
for (i in c(unique(data$month.yr.rect))) {
  subdata <- data[data$month.yr.rect==i,]
  subdata_m <- subdata[subdata$sex==0,]
  subdata_f <- subdata[subdata$sex==1,]
  if (nrow(subdata_m) > 0) {
    size.dist_m[1] <- unique(subdata_m$year)
    size.dist_m[2] <- unique(subdata_m$month)
    size.dist_m[3] <- 1 # fleet
    size.dist_m[4] <- unique(subdata_m$sex)
    size.dist_m[5] <- 0 #part
    size.dist_m[6] <- nrow(subdata_m) * sum(unique(subdata_m$total_landed_wgt/subdata_m$wgt_measured))
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_m$total_landed_wgt/subdata_m$wgt_measured))
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$qrt.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part
    size.dist_f[6] <- nrow(subdata_f) * sum(unique(subdata_f$total_landed_wgt/subdata_f$wgt_measured))
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, seq(size.min+width, size.max-width, by = width), size.max))) * 
      sum(unique(subdata_f$total_landed_wgt/subdata_f$wgt_measured))
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$qrt.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_lobster <- dplyr::bind_rows(as.data.frame(size.dist_lobster), as.data.frame(size.dist))
}

# aggregate by quarter
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "qrt.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_lobster2 <- NULL 
for (i in c(unique(data$qrt.yr))) {
  subdata <- size.dist_lobster[size.dist_lobster$qrt.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
  size.dist_m2[1] <- unique(subdata_m$year)
  size.dist_m2[2] <- max(subdata_m$month)
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
  size.dist_f2[2] <- max(subdata_f$month)
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

# reformat for ss
colnames(size.dist_lobster2) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_lobster_m <- size.dist_lobster2 |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_lobster_f <- size.dist_lobster2 |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_lobster <- size.dist_lobster_f |> 
  dplyr::bind_cols(size.dist_lobster_m) |>
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_lobster) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                                 paste0("f", 1:n.size), paste0("m", 1:n.size))

# crab
data <- size.data_crab_wales
data$length <- data$length/10 # recorded in mm -> convert to cm for ss
size.min <- 3#round(min(data$length, na.rm = TRUE)))
size.max <- 24#round(max(data$length, na.rm = TRUE)))
width <- 0.2
n.size <- length(table(cut(data$length, 
                           breaks = c(size.min, seq(size.min+width, size.max+width*2-width, by = width), size.max+width*2))))
size.dist_m <- matrix(NA, 1, n.size+7)
size.dist_f <- matrix(NA, 1, n.size+7)
size.dist_crab <- NULL 
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
    size.dist_m[6] <- nrow(subdata_m) * sum(unique(subdata_m$total_landed_wgt/subdata_m$wgt_measured))
    size.dist_m[7:(n.size+6)] <- table(cut(subdata_m$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max+width*2-width, by = width), 
                                                      size.max+width*2))) * sum(unique(subdata_m$total_landed_wgt/subdata_m$wgt_measured))
    size.dist_m[(n.size+6)+1] <- unique(subdata_m$qrt.yr)
  }
  if (nrow(subdata_f) > 0) {
    size.dist_f[1] <- unique(subdata_f$year)
    size.dist_f[2] <- unique(subdata_f$month)
    size.dist_f[3] <- 1 # fleet
    size.dist_f[4] <- unique(subdata_f$sex)
    size.dist_f[5] <- 0 #part    
    size.dist_f[6] <- nrow(subdata_f) * sum(unique(subdata_f$total_landed_wgt/subdata_f$wgt_measured))
    size.dist_f[7:(n.size+6)] <- table(cut(subdata_f$length, 
                                           breaks = c(size.min, 
                                                      seq(size.min+width, size.max+width*2-width, by = width), 
                                                      size.max+width*2))) * sum(unique(subdata_f$total_landed_wgt/subdata_f$wgt_measured))
    size.dist_f[(n.size+6)+1] <- unique(subdata_f$qrt.yr)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m), as.data.frame(size.dist_f))
  size.dist_crab <- dplyr::bind_rows(as.data.frame(size.dist_crab), as.data.frame(size.dist))
}

# aggregate by quarter
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size), "qrt.yr")
size.dist_m2 <- matrix(0, 1, n.size+6)
size.dist_f2 <- matrix(0, 1, n.size+6)
size.dist_crab2 <- NULL 
for (i in c(unique(data$qrt.yr))) {
  subdata <- size.dist_crab[size.dist_crab$qrt.yr==i,]
  subdata_m <- subdata[subdata$sex==0,1:ncol(subdata)-1]
  subdata_f <- subdata[subdata$sex==1,1:ncol(subdata)-1]
  if (nrow(subdata_m) > 0) {
    size.dist_m2[1] <- unique(subdata_m$year)
    size.dist_m2[2] <- max(subdata_m$month)
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
    size.dist_f2[2] <- max(subdata_f$month)
    size.dist_f2[3] <- unique(subdata_f$fleet)
    size.dist_f2[4] <- unique(subdata_f$sex)
    size.dist_f2[5] <- unique(subdata_f$part)
    size.dist_f2[6] <- round(sum(as.numeric(subdata_f$nsample)))
    if (nrow(subdata_f) > 1) {
      size.dist_f2[7:(n.size+6)] <- round(colSums(as.data.frame(apply(subdata_f[7:ncol(subdata_f)], 2, as.numeric))), digits=1)
    } else size.dist_f2[7:(n.size+6)] <- round(as.numeric(subdata_f[7:ncol(subdata_f)]), digits = 1)
  }
  size.dist <- dplyr::bind_rows(as.data.frame(size.dist_m2), as.data.frame(size.dist_f2))
  size.dist_crab2 <- dplyr::bind_rows(as.data.frame(size.dist_crab2), as.data.frame(size.dist))
}

# reformat for ss
colnames(size.dist_crab2) <- c("year", "month", "fleet", "sex", "part", "nsample", paste0("s", 1:n.size))
size.dist_crab_m <- size.dist_crab2 |> 
  dplyr::filter(sex == 0) |> 
  dplyr::mutate(sex = 3) |>
  dplyr::rename(nsample.m = nsample) |>
  dplyr::select(-year, -month, -fleet, -sex, -part)
size.dist_crab_f <- size.dist_crab2 |> 
  dplyr::filter(sex == 1) |> 
  dplyr::mutate(sex = 3) 
size.dist_crab <- size.dist_crab_f |> 
  dplyr::bind_cols(size.dist_crab_m) |> 
  dplyr::mutate(nsample = as.numeric(nsample)+as.numeric(nsample.m)) |>
  dplyr::select(-nsample.m)
colnames(size.dist_crab) <- c("year", "month", "fleet", "sex", "part", "nsample", 
                              paste0("f", 1:n.size), paste0("m", 1:n.size))

# export dataset
readr::write_csv(size.dist_lobster, file = "processed_data/wales/size.comp.data_lobster_wales_ss.csv") 
readr::write_csv(size.dist_crab, file = "processed_data/wales/size.comp.data_crab_wales_ss.csv") 


# data exploration for landings data
# load landings data from ifish datasets
ifish_landings_wales_lobster <- readr::read_csv(file = "processed_data/wales/ifish_landings_wales_lobster_clean.csv",
                                                col_types = readr::cols(rectangle = readr::col_character())) 
ifish_landings_wales_crab <- readr::read_csv(file = "processed_data/wales/ifish_landings_wales_crab_clean.csv",
                                             col_types = readr::cols(rectangle = readr::col_character())) 

ifish_landings_wales_lobster_month <- ifish_landings_wales_lobster |> 
  dplyr::group_by(year, qtr, month, rectangle) |>
  dplyr::reframe(landing_ifish = sum(live_weight_tonnes),
                        value_ifish = sum(value_pounds))

size.data_lobster_wales_landings <- size.data_lobster_wales |> 
  dplyr::group_by(year, quarter, month, rectangle) |>
  dplyr::reframe(landing_cefas = mean(total_landed_wgt)) 

data_lobster <- size.data_lobster_wales_landings |> 
  dplyr::left_join(ifish_landings_wales_lobster_month, by = c("year", "quarter"="qtr", "month", "rectangle"))


# data exploration
# lobster
data1 <- size.data_crab_wales[size.data_crab_wales$sex==0 & size.data_crab_wales$month==5, ]
 
(plot1 <- data1 |> ggplot2::ggplot(ggplot2::aes(x = length, y = as.factor(year))) +
  ggridges::geom_density_ridges(scale = 2.5, 
                                alpha = 0.3, 
                                quantile_lines = TRUE, 
                                quantiles = 0.5, 
                                fill = "darkred") +
  ggplot2::xlab("carapace width (mm)") +
  ggplot2::ylab("year") +
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
ggplot2::ggsave(file="plots/wales/size.comp_lobster_wales.svg", plot=plot1, width=12, height=8)

# crab
data2 <- size.data_crab_wales

(plot2 <- data2 |> ggplot2::ggplot(ggplot2::aes(x = length, y = as.factor(year))) +
    ggridges::geom_density_ridges(scale = 2.5, 
                                  alpha = 0.3, 
                                  quantile_lines = TRUE, 
                                  quantiles = 0.5, 
                                  fill = "darkred") +
    ggplot2::xlab("carapace width (mm)") +
    ggplot2::ylab("year") +
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
ggplot2::ggsave(file="plots/wales/size.comp_crab_wales.svg", plot=plot2, width=12, height=8)
