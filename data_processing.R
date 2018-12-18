rproj_dir <- rprojroot::find_rstudio_root_file()

source(file.path(rproj_dir,"utils","getLibrary.R"))
source(file.path(rproj_dir,"utils","paths.R"))

getLibrary("data.table")

data_processing <- function() {
  travel_times_dt <- fread(file.path(data_20181217,"travel_times_17.12.2018.csv"))
  routes_dt <- fread(file.path(data_20181217,"routes_17.12.2018.csv"))
  
  travel_times_dt$updatetime <- as.POSIXct(travel_times_dt$updatetime)
  travel_times_dt <- travel_times_dt[which(travel_times_dt$updatetime <= as.POSIXct("2018-11-30") &
                                             travel_times_dt$updatetime >= as.POSIXct("2018-11-01")),]
  travel_times_dt$demora_km <- 1000 * travel_times_dt$time / merge(travel_times_dt[,c('name')],routes_dt[,c('name','length')],by="name")$length
}



