library(raster)
library(gdalUtils)

#RESAMPLE SET ORIGIN RASTERS
resample_ps<-function(l){
  library(raster)
  ##################
  #
  #INPUTS
  ###################
  ps_ndvis<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/5_fifth_stage_indices")
  naips<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected")
  naip_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis"
  ps_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  # #set origin of NAIP since it's off a little
  #######################PATTERN MATCHING
  naip<-grep(pattern = paste0("Site_",l,"_32615.tif"), naips, value = T, perl = T)
  print(naip)
  high_res<-stack(file.path(naip_dir, "projected",naip))
  origin(high_res)<-c(0,0) #set origin to 0,0
  #writeRaster(high_res, file.path(naip_dir, "reset_origin", naip))
  for (i in 1:7){
    #######################PATTERN MATCHING
    ps<-grep(pattern = paste0("date_",i,"_site_",l,"_indices.tif"), ps_ndvis, value = T)
    print(ps)
    low_res<-raster(file.path(ps_dir, "5_fifth_stage_indices", ps))
    #use the new naip as reference to resample ps
    resample_to_naip<-resample(x = low_res, y = high_res, method = "bilinear")
    writeRaster(resample_to_naip, file.path(ps_dir, "resampled_to_NAIP", ps))
  }

}

###########PARALLEL
library(parallel)
#Setting to use as many cores as states running. Each state should process at one core.
ncores<-detectCores(logical = F)
cl<-makeCluster(ncores-1)
l<-seq(1,49,1) #!!!!!!!!make sure and change seq based on number of states being analyzed!!!!!!!!
clusterApply(cl, l, resample_ps)
stopCluster(cl)
######################################
######################################




resample_dem<-function(l){
  library(raster)
  ##################
  #INPUTS
  ###################
  dems<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/TPI/clipped")
  naips<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected")
  naip_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis"
  dem_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/TPI/clipped"
  # #set origin of NAIP since it's off a little
  #######################PATTERN MATCHING
  naip<-grep(pattern = paste0("Site_",l,"_32615.tif"), naips, value = T, perl = T)
  dem<-grep(pattern = paste0("Site_",l,"_32615"), dems, value = T, perl = T)
  high_res<-stack(file.path(naip_dir, "projected",naip))
  origin(high_res)<-c(0,0) #set origin to 0,0
  #writeRaster(high_res, file.path(naip_dir, "reset_origin", naip))
  low_res<-raster(file.path(dem_dir, dem))
  #use the new naip as reference to resample ps
  resample_to_naip<-resample(x = low_res, y = high_res, method = "bilinear")
  writeRaster(resample_to_naip, file.path(dem_dir, "resampled_to_NAIP", dem))
}

###########PARALLEL
library(parallel)
#Setting to use as many cores as states running. Each state should process at one core.
ncores<-detectCores(logical = F)
cl<-makeCluster(ncores-1)
l<-seq(1,49,1) #!!!!!!!!make sure and change seq based on number of states being analyzed!!!!!!!!
clusterApply(cl, l, resample_dem)
stopCluster(cl)
######################################
######################################

dem_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs"
tpi_resample_list<-list.files(file.path(dem_dir, "TPI", "clipped","resampled_to_NAIP"), full.names = T)

gdalbuildvrt(tpi_resample_list, file.path(dem_dir,"vrts","tpi_resampled.vrt"))





