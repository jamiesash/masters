
# loadsat <- function(var = "sla",
#                     sdate = as.Date("2018-05-01"), 
#                     edate = as.Date("2018-11-01"), 
#                     lons = c(190-360, 230-360), 
#                     lats = c(25, 32),
#                     origin = "1990-01-01",
#                     lat_varid = "lat",
#                     lon_varid = "lon",
#                     url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l3-chl_4km_daily-rep-v02?"){
#   
#   #if(var == "sla") url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/c3s_obs-sl_glo_phy-ssh_my_twosat-l4-duacs-0.25deg_P1D?"
#   #if(var == "CHL") url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l3-chl_4km_daily-rep-v02?"
#   #https://my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l4-chl_4km_monthly-rep-v02
#   
#   data <- nc_open(url, verbose = FALSE, write = FALSE)
#   lat  <- ncvar_get(data, varid = lat_varid)
#   lon  <- ncvar_get(data, varid = lon_varid)
#   time <- ncvar_get(data, varid = "time")
#   # time is in a crazy format. Maybe seperat loadchl function? Geting harry
#   # seconds since 1970-01-01 00:00:00
#   
#   if(var == "CHL") {
#     #time = as.POSIXct(time, origin = origin)
#     time = as.Date(time, origin = origin)
#     #time = anydate(time)
#     }
#   #else             time <- as.Date(time)
#   
#   idx_lat <- which(lat > lats[1] & lat < lats[2])
#   idx_lon <- which(lon > lons[1] & lon < lons[2])
#   idx_time <- which(time >= sdate & time <= edate)
#   
#   idx_sla <- paste(var,
#                    paste("[", range(idx_time)[1], ":1:", range(idx_time)[2], "]", sep = ""), 
#                    paste("[", range(idx_lat)[1],  ":1:", range(idx_lat)[2],  "]", sep = ""),
#                    paste("[", range(idx_lon)[1],  ":1:", range(idx_lon)[2],  "]", sep = ""),
#                    sep = "")
#   
#   idx_time <- paste("time",      paste("[", range(idx_time)[1], ":1:",range(idx_time)[2], "]", sep = ""), sep = "")
#   idx_lat  <- paste(lat_varid,  paste("[", range(idx_lat)[1],  ":1:",range(idx_lat)[2],  "]", sep = ""), sep = "")
#   idx_lon  <- paste(lon_varid, paste("[", range(idx_lon)[1],  ":1:",range(idx_lon)[2],  "]", sep = ""), sep = "")
#   idx <- paste(idx_lat, idx_lon, idx_time, idx_sla, sep = ",")
#   
#   url <- paste(url, idx, sep = "")
#   
#   data = nc_open(url, verbose = FALSE, write = FALSE)
#   ras  = ncvar_get(data)
#   lats = ncvar_get(data, varid = lat_varid)
#   lons = ncvar_get(data, varid = lon_varid)
#   time = ncvar_get(data, varid = "time")
#   attr = ncatt_get(data, varid = 0)
#   nc_close(data)
#   rm(data)
#   
#   ras = brick(ras) 
#   ras = setZ(ras, z = as.Date(time, origin = origin), name = "time")
#   #ras = setZ(ras, z = as.Date(getZ(ras), origin = "1900-01-01"), name = "time")
#   extent(ras) = extent(min(lons), max(lons), min(lats), max(lats))
#   #crs(ras) <- idk
#   ras
# }
#

url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l3-chl_4km_daily-rep-v02?"

loadsat <- function(var = "sla",
                    sdate = as.Date("2018-05-01"), 
                    edate = as.Date("2018-11-01"), 
                    lons = c(190-360, 230-360), 
                    lats = c(25, 32),
                    org = "1900-01-01",
                    lat_varid = "lat",
                    lon_varid = "lon",
                    by = 365, 
                    url = "https://my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l3-chl_4km_daily-rep-v02?") {}
  
  # pwd = "jash:5.Pellegrino@"
  # if(var == "sla") url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/c3s_obs-sl_glo_phy-ssh_my_twosat-l4-duacs-0.25deg_P1D?"
  # if(var == "CHL") url = "https://jash:5.Pellegrino@my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l3-chl_4km_daily-rep-v02?"
  # https://my.cmems-du.eu/thredds/dodsC/dataset-oc-glo-chl-multi_cci-l4-chl_4km_monthly-rep-v02
  


















