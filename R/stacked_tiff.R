library(raster)

dir <- "~/Documents/MEDS/eds222_stats/project"

project_data <- list.files(path=dir, recursive = TRUE)

tiff_list <- lapply(project_data, FUN = list)





tiff_date <- vector(mode='list', length=6)
for (i in tiff_list){
  for (j in tiff_list){
    tiff_date[i] <- append(tiff_date, j)
  }
}

# for (year_folder in project_data){
#   for (day_folder in year_folder){
#     for (tiff_file in day_folder){
#       tiff_list[[tiff_file]] <- append(tiff_list, tiff_file)
#     }
#   }
  
 # }



