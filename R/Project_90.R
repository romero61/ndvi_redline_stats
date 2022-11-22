

# PC working Directory
#setwd("G:/My Drive/UCSB/115C_Remote_Sensing/Project")

# Mac working directory
# setwd(
#   "/Users/guillermoromero/Library/CloudStorage/GoogleDrive-romero61@ucsb.edu/My Drive/UCSB/115C_Remote_Sensing/Section/Project"
#   )
getwd()


#Install and load required libraries

# install.packages("rgdal")
# install.packages("raster")
library(raster)




# PRE-PROCESSING


# Create a RasterStack from the six Landsat bands in the USGS data set for
# 06-02-90, 07-04-90, and 10-24-90

landsat_19900602 <-
  stack(
    "./year1990/June0290/LT05_L2SP_014032_19900602_20200916_02_T1_SR_B1.tiff",
    "./year1990/June0290/LT05_L2SP_014032_19900602_20200916_02_T1_SR_B2.tiff",
    "./year1990/June0290/LT05_L2SP_014032_19900602_20200916_02_T1_SR_B3.tiff",
    "./year1990/June0290/LT05_L2SP_014032_19900602_20200916_02_T1_SR_B4.tiff",
    "./year1990/June0290/LT05_L2SP_014032_19900602_20200916_02_T1_SR_B5.tiff",
    "./year1990/June0290/LT05_L2SP_014032_19900602_20200916_02_T1_SR_B7.TIF"
  )
crs(landsat_19900602) #check CRS CRS arguments:
# +proj=utm +zone=18 +datum=WGS84 +units=m +no_defs

landsat_19900704 <-
  stack(
    "./year1990/July0490/LT05_L2SP_014032_19900704_20200916_02_T1_SR_B1.TIF",
    "./year1990/July0490/LT05_L2SP_014032_19900704_20200916_02_T1_SR_B2.TIF",
    "./year1990/July0490/LT05_L2SP_014032_19900704_20200916_02_T1_SR_B3.TIF",
    "./year1990/July0490/LT05_L2SP_014032_19900704_20200916_02_T1_SR_B4.TIF",
    "./year1990/July0490/LT05_L2SP_014032_19900704_20200916_02_T1_SR_B5.TIF",
    "./year1990/July0490/LT05_L2SP_014032_19900704_20200916_02_T1_SR_B7.TIF"
  )
crs(landsat_19900704)

landsat_19901024 <-
  stack(
    "./year1990/Oct2490/LT05_L2SP_014032_19901024_20200915_02_T1_SR_B1.TIF",
    "./year1990/Oct2490/LT05_L2SP_014032_19901024_20200915_02_T1_SR_B2.TIF",
    "./year1990/Oct2490/LT05_L2SP_014032_19901024_20200915_02_T1_SR_B3.TIF",
    "./year1990/Oct2490/LT05_L2SP_014032_19901024_20200915_02_T1_SR_B4.TIF",
    "./year1990/Oct2490/LT05_L2SP_014032_19901024_20200915_02_T1_SR_B5.TIF",
    "./year1990/Oct2490/LT05_L2SP_014032_19901024_20200915_02_T1_SR_B7.TIF"
  )
crs(landsat_19901024)




# Import Philadelphia, Pennsylvania block group shapefile
#Transform to coordinate system of Landsat raster stack
#CRS arguments:
#+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs

Pennsylvania_block <-
  shapefile("~/Documents/MEDS/eds222_stats/project/Pennsylvania_blockPennsylvania.shp")

crs(Pennsylvania_block)

Pennsylvania_block <-
  spTransform(Pennsylvania_block, crs(landsat_19900602))
plot(Pennsylvania_block) #check





#FUNCTION
#Masks and crops Landsat raster stack using a polygon.
# The function  accepts two arguments: a Raster* object and a Spatial* object

mask_crop_func <- function(raster_obj, spatial_obj) {
  #change the extent of the masked image
  # so it matches the extent of the vector polygon
  cropped_object <- crop(raster_obj, extent(spatial_obj))
  # Mask removes all of the raster values outside the region of interest
  masked_object <- mask(cropped_object, spatial_obj)
}







# Mask and Crop each Raster stack to Philadelphia,Pennsylvania block group Shapefile
landsat_19900602_masked <-
  mask_crop_func(landsat_19900602, Pennsylvania_block)
landsat_19900704_masked <-
  mask_crop_func(landsat_19900704, Pennsylvania_block)
landsat_19901024_masked <-
  mask_crop_func(landsat_19901024, Pennsylvania_block)

#clear memory
rm(landsat_19900602,
   landsat_19900704,
   landsat_19901024,
   Pennsylvania_block)





#FUNCTION
#Overwrite erroneous pixel values that do not fall within
# the valid range for the Landsat surface reflectance product.
#The valid range for all bands 1-7 is 7273-43636.
#Changes scale factor to 100

value_reclassify_function <- function(masked_landsat) {
  # replace all values between negative infinity (-Inf) and 7,273
  masked_landsat_reclassified <-
    reclassify(masked_landsat, c(-Inf, 7273, NA))
  
  # replace all values between 43,636 and infinity (Inf)
  masked_landsat_reclassified <-
    reclassify(masked_landsat_reclassified, c(43636, Inf, NA))
  
  # h.	Change the scale factor of the image to 100.
  masked_landsat_reclassified <-
    (masked_landsat_reclassified * 0.0000275 - 0.2) * 100
  
}




#Overwrite erroneous pixels and changes scale factor for each image
landsat_19900602_reclass <-
  value_reclassify_function(landsat_19900602_masked)
landsat_19900704_reclass <-
  value_reclassify_function(landsat_19900704_masked)
landsat_19901024_reclass <-
  value_reclassify_function(landsat_19901024_masked)

#clear memory
rm(landsat_19900602_masked,
   landsat_19900704_masked,
   landsat_19901024_masked)


# Plot to check
plotRGB(
  landsat_19900602_reclass,
  r = 4,
  g = 3,
  b = 2,
  stretch = 'lin'
)
plotRGB(
  landsat_19900704_reclass,
  r = 4,
  g = 3,
  b = 2,
  stretch = 'lin'
)
plotRGB(
  landsat_19901024_reclass,
  r = 4,
  g = 3,
  b = 1,
  stretch = 'lin'
)



#Give the bands more intuitive names that correspond with their wavelengths.
names(landsat_19900602_reclass) <-
  c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_19900704_reclass) <-
  c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_19901024_reclass) <-
  c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")

#check
landsat_19900602_reclass
landsat_19900704_reclass
landsat_19901024_reclass





# Calculate NDVI



# FUNCTION
#Calculates NDVI which is sensitive to vegetation cover

NDVI <- function(red, NIR) {
  value <- (NIR - red) / (NIR + red)
  
  return(value)
  
}



# Calculate NDVI for each individual image using red and NIR bands
ndvi_19900602 <-
  NDVI(landsat_19900602_reclass$red,
       landsat_19900602_reclass$NIR)

ndvi_19900704 <-
  NDVI(landsat_19900704_reclass$red,
       landsat_19900704_reclass$NIR)

ndvi_19901024 <-
  NDVI(landsat_19901024_reclass$red,
       landsat_19901024_reclass$NIR)

#clear memory
rm(landsat_19900602_reclass,
   landsat_19900704_reclass,
   landsat_19901024_reclass)

#Export images of NDVI

writeRaster(ndvi_19900602,
            "ndvi_19900602",
            format = "GTiff",
            overwrite = TRUE)

writeRaster(ndvi_19900704,
            "ndvi_19900704",
            format = "GTiff",
            overwrite = TRUE)

writeRaster(ndvi_19901024,
            "ndvi_19901024",
            format = "GTiff",
            overwrite = TRUE)
