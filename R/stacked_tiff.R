library(raster)

dir <- "~/Documents/MEDS/eds222_stats/project"

project_data <- list.files(path=dir, recursive = TRUE)

i = 0
tiff_list <- vector(mode='list', length=6)
for (year_folder in project_data){
  for (day_folder in year_folder){
    for (tiff_file in day_folder){
      tiff_list[[tiff_file]] <- append(tiff_list, tiff_file)
    }
  }
  
  }



