# load libraries
library(jsonlite)
library(tidyverse)

library(optparse)

# GEOSAS API guide to access SAFRAN data here : https://geosas.fr/web/?page_id=6345 
# Description : this script extract and format daily SAFRAN data for french LTEs, using an API developed by GEOSAS

# Step 0: set optparse parameters
option_list<-list(
  make_option(c("-m","--metadata"),
              type="character"),
  make_option(c("-s","--site_name"),
              type="character",
              default=NULL),
  make_option(c("-d","--start_date_"),
              type="integer",
              default=NULL),
  make_option(c("-e","--end_date_"),
              type="integer",
              default=NULL))

opt <- parse_args(OptionParser(option_list=option_list))

#### Step 1 : set parameters for SAFRAN data extraction ####
## climate parameters to extract
parameters <- c(
  "PRENEI_Q", # solid precipitations (mm)
  "PRELIQ_Q", # liquid precipitations (mm)
  "T_Q", # daily temperature (°C)
  "ETP_Q" # reference ET0 (Penman-Monteith method) (mm)
  )

# create directory to store outputs
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

### set coordinates of french sites and temporal coverage
## set locations and temporal coverage between first and last soil C stock measurement (for forward simulations)
locations <- read_csv(opt$metadata) %>%
  filter(country=="France") %>%
  mutate(start_date=ymd(start_date, truncated = 2L)) %>% # set first date for climate data extraction : 1st January of the first year of sampling
  mutate(end_date=ymd(last_soc_date +1 , truncated = 2L)-1) %>% # set last date for climate data extraction : 31th december of the last year of sampling
  mutate_at(c('longitude', 'latitude'), ~ str_replace(., "−", "-")) %>%
  mutate_at(c('longitude', 'latitude'), as.numeric) %>%
  # coords in WKT format (EPSG 4326)
  mutate(coord_wkt = paste0("POINT(", longitude, " ", latitude, ")")) %>%
  dplyr::select(site_name, start_date, end_date, coord_wkt)

#Selection of the site(s) to extract data for (default is all French sites)
if (!is.null(opt$site_name) && nchar(trimws(opt$site_name)) > 0) {
  site_names_selected <- trimws(unlist(strsplit(opt$site_name, ",")))
  locations <- locations %>% filter(site_name %in% site_names_selected)
}

#selection of the period to extract data for (default is "all")
if (!is.null(opt$start_date_) && nchar(trimws(opt$start_date_)) > 0 &
    !is.null(opt$end_date_)   && nchar(trimws(opt$end_date_))   > 0) {
  start_date_ <- as.Date(paste0(opt$start_date_, "-01-01"))
  end_date_ <- as.Date(paste0(opt$end_date_, "-12-31"))
  locations <- locations %>%
    mutate(start_date=start_date_ , end_date=end_date_)
}

## set locations for spin-up (model initialization), ideally by extract data  15 to 30 years before the first soil C stock measurement
# get the theoretical date for spin-up for each site
su_info <- locations %>%
  # theorical start date for spin-up
  mutate(start_spinup_theo = start_date - years(30)) %>%
  # effective start date for spin-up
  mutate(start_spinup = case_when(start_spinup_theo < ymd("1959-01-01") ~ ymd("1959-01-01"),
                                  TRUE ~ start_spinup_theo)) %>% 
  mutate(spinup_duration_yr = year(start_date) - year(start_spinup)) %>%
  mutate(end_spinup= start_spinup  %m+% years(spinup_duration_yr)) %>%
  dplyr::select(site_name, start_date, end_date,
                start_spinup, end_spinup, spinup_duration_yr)


# filter sites where SAFRAN data can be extracted between 15 and 30 years before the first soil C stock measurement,
sites_su <- su_info %>%
  filter(spinup_duration_yr >15) 
# set the locations df with start and end dates before the experiment period for these sites
locations_su <- locations %>%
  inner_join(sites_su) %>%
  select(site_name, start_date=start_spinup, end_date=end_spinup, coord_wkt)

# filter sites where SAFRAN data cannot be extracted between 15 and 30 years before the first soil C stock measurement,
# for these sites we keep the first 30 years of the experiment
sites_su_alt <- su_info %>%
  filter(spinup_duration_yr < 15) %>%
  mutate(end_spinup= start_spinup  %m+% years(30))
# set the locations df with start and end dates before the experiment period for these sites
locations_su_alt <- locations %>%
  inner_join(sites_su_alt) %>%
  select(site_name, start_date=start_spinup, end_date=end_spinup, coord_wkt) 


# list of the locations dfs
locations_list <- list(
  locations = locations,
  locations_su = locations_su,
  locations_su_alt = locations_su_alt
)

# API parameters
url_service <- "https://api.geosas.fr/edr/collections/safran-isba/"
projection <- "EPSG:4326"
formatage <- "CoverageJSON"


#### Step 2 : configurate a function to extract one climate parameter for one site, and for a given time period ####
extract_data <- function(site_name, coord_wkt, start_date, end_date, param_name) {
  coord_encoded <- gsub(" ", "%20", coord_wkt)
  date_range <- paste0(start_date, "/", end_date)
  
  request <- sprintf('%sposition?coords=%s&crs=%s&parameter-name=%s&f=%s&datetime=%s',
                     url_service, coord_encoded, projection, param_name, formatage, date_range)
  message("→ ", site_name, " | ", param_name, " | from ", start_date, " to ", end_date)
  
  content <- tryCatch(readLines(request, warn = FALSE), error = function(e) return(NULL))
  if (is.null(content) || !any(grepl("\\{", content))) {
    warning("Request failed or invalid response for ", site_name, " / ", param_name)
    return(NULL)
  }
  
  data <- tryCatch(fromJSON(paste(content, collapse = "\n")), error = function(e) {
    warning("Invalid JSON for ", site_name, " / ", param_name)
    return(NULL)
  })
  
  if (!is.null(data)) {
    date_value <- data$domain$axes$t$values
    values <- data$ranges[[param_name]]$values
    df <- data.frame(date = date_value, value = values)
    # df$date <- as.POSIXct(df$date, format = '%Y-%m-%dT%H-%M-%SZ')
    df$date <- as.Date(df$date, format = '%Y-%m-%dT%H-%M-%SZ')
    colnames(df)[2] <- param_name
    return(df)
  } else {
    return(NULL)
  }
}


#### Step  3 : Apply the extraction function for the list of locations df and process the ouptuts ####
# apply the extraction function
all_results <- map(locations_list, function(locations_input) {
  pmap(locations_input, function(site_name, start_date, end_date, coord_wkt) {
    data_by_param <- map(parameters, function(param) {
      extract_data(site_name, coord_wkt, start_date, end_date, param)
    })
    
    site_df <- reduce(data_by_param, full_join, by = "date")
    site_df$site_name <- site_name
    return(site_df)
  })
})
# names(all_results)

# process the outputs 
final_results <- map(all_results, function(sublist) {
  sublist_df <- bind_rows(sublist)

  if (nrow(sublist_df) == 0 || !("PRENEI_Q" %in% names(sublist_df))) {
    return(tibble(site_name=character(), date=as.Date(character()),
                  precip=numeric(), temp=numeric(), et0=numeric()))
  }

  sublist_df %>%
    mutate(precip = PRENEI_Q + PRELIQ_Q) %>% # totale precipitations (liquid + solid)
    dplyr::select(site_name, date, precip, temp = T_Q, et0 = ETP_Q)
})



##### Step 4 : process and save the ouptuts in .rds ####

# SAFRAN data for forward simulations 
saveRDS(final_results$locations, "outputs/safran_french_sites.rds")

#### read the output to fill missing dates at versailles_ltbf (before SAFRAN availability, from 1928 to 1958)
safran_french_sites <- readRDS("outputs/safran_french_sites.rds")

##calculation of normal daily data at versailles_ltbf
versailles_ltbf_avg <- safran_french_sites %>% filter(site_name=="versailles_ltbf") %>%
  filter(date >= "1959-01-01" & date < "1989-01-01") %>%
  mutate(yday=yday(date)) %>% # get the day of year for each date
  group_by(site_name, yday) %>%
  summarise_at(vars(precip, temp, et0), ~ mean(., na.rm=TRUE)) %>%
  ungroup() %>%
  mutate_at(c('precip', 'temp', 'et0'), round, 1) 

# period to fill in versailles_ltbf : 1928-01-01 to 1958-07-31
versailles_ltbf_1928_1958 <- 
  tibble(date = seq.Date(as.Date("1928-01-01"), as.Date("1958-07-31"), by = "day")) %>%
  mutate(yday=yday(date)) %>% 
  left_join(versailles_ltbf_avg) %>%
  select(-yday) 

#merge safran data and estimated data in versailles_ltbf
safran_french_sites_filled <- safran_french_sites %>%
  bind_rows(versailles_ltbf_1928_1958) %>%
  arrange(site_name, date)

# save filled SAFRAN data for forward simulations (between first and last soil C stock measurement)
saveRDS(safran_french_sites_filled, "outputs/safran_french_sites_filled.rds")


# safran_french_sites_filled %>%
#   filter(site_name=="feucherolles_qualiagro")
#   mutate()

#  SAFRAN data for model initialization (spinup)
bind_rows(final_results$locations_su, final_results$locations_su_alt) %>%
  saveRDS("outputs/safran_french_sites_su.rds")



# metadata for climate data extraction for model initialization (spinup)
safran_spinup_metadata <- su_info %>%
  mutate(method_spinup =
           case_when(spinup_duration_yr >= 15 ~ paste("Normal data calculated from", year(start_spinup), "to", year(end_spinup)),
                     TRUE ~ "Normal data calculated from 1959 to 1989")) %>%
  mutate(time_span_spinup_yr= case_when(spinup_duration_yr >= 15 ~ year(end_spinup)-year(start_spinup),
                                                TRUE~30)) %>%
  dplyr::select(site_name, start_date, end_date, start_spinup, end_spinup, method_spinup, time_span_spinup_yr) 
safran_spinup_metadata %>% write_csv("outputs/safran_spinup_metadata.csv")