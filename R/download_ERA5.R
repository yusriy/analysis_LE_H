#install.packages("ecmwfr")
#install.packages("ncdf4")
library(ecmwfr)
library(ncdf4)

wf_set_key(user = "yusriy@usm.my", key = "40ded1e4158e091d8b13baf1deb8f532", 
           service = "cds")


Sys.setenv(ECMWF_KEY = "40ded1e4158e091d8b13baf1deb8f532")
Sys.setenv(ECMWF_EMAIL = "yusriy@usm.my")


# Define the request as before
request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type = "reanalysis",
  variable = c(
    "skin_temperature", 
    "sea_surface_temperature", 
    "2m_temperature", 
    "significant_height_of_combined_wind_waves_and_swell", 
    "surface_downwelling_longwave_radiation",
    "surface_downwelling_shortwave_radiation",
    "surface_net_downward_longwave_radiation",
    "surface_net_downward_shortwave_radiation",
    "surface_latent_heat_flux",
    "surface_sensible_heat_flux"
  ),
  year = c("2022"),
  month = c("03", "04", "05", "06", "07", "08", "09"),
  day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
  time = sprintf("%02d:00", 0:23),
  area = c(5.75, 100, 5.5, 100.25),  # North, West, South, East
  format = "netcdf",
  grid = c(0.25, 0.25),
  target = "era5_data.nc"
)

# Download the data
wf_request(user = Sys.getenv("ECMWF_EMAIL"), request = request, transfer = TRUE, path = ".")


ibrary(ecmwfr)

cds.key <- "40ded1e4158e091d8b13baf1deb8f532"
wf_set_key(user = "yusriy@usm.my", key = cds.key, service = "cds")

request <- list(
  dataset_short_name = "reanalysis-era5-single-levels",
  product_type   = "reanalysis",
  format = "netcdf",
  variable = "2m_temperature",
  year = "2016",
  month = "08",
  day = "16",
  time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
  # area is specified as N, W, S, E
  area = c(50, -20, 30, 20),
  target = "download_e5_single.nc"
)

file <- wf_request(user = "Insert_your_CDS_UID_here",
                   request = request,
                   transfer = TRUE,
                   path = "~",
                   verbose = TRUE)