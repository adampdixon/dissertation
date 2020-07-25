library(rgdal)
library(raster)
library(gdalUtils)


# input_dir<-"D:/Dropbox/A_School/2020_GrassyMargins/2019_data/planet"
# output_dir<-"D:/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
# data_dir<-"D:/Dropbox/A_School/2020_GrassyMargins/2019_data"

input_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet"
output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"

#D:\Dropbox\A_School\2020_GrassyMargins\2019_data\planet

###########################################################
############################################################STEP 1 MOSAIC RAW IMAGES 
############################################################

mosaic_same_date_sat<-function(S){
  library(raster)
  library(gdalUtils)
  path<-input_dir
  planet<-list.dirs(input_dir, recursive = F)
  site_names<-list.dirs(input_dir, recursive = F, full.names = F)
  file_names<-sub(pattern = "_3B.*","", (list.files(paste0(planet[S],"/raw"), pattern = ".tif", full.names = F)))
  files<-list.files(paste0(planet[S],"/raw"), pattern = ".tif", full.names = T)
  match_names <- split(file_names, sub("^(\\d+)_.*_([^_]+)$", "\\1_\\2", file_names))
  #create a dir for vrts and tifs
  dir.create(paste0(path,site_names[S],"/mosaic_new"))
  dir.create(paste0(path,site_names[S],"/mosaic_new/vrts"))
  dir.create(paste0(path,site_names[S],"/mosaic_new/tifs"))
  for (i in 1:length(match_names)) {
    date_sat_match<-match_names[[i]][1]
    date_name<-substr(date_sat_match,1,8)
    longname<-sub(pattern = "_3B.*","", date_sat_match)
    n_char<-nchar(longname)
    satellite<-substr(longname, n_char-3, n_char)
    vrt_name<-paste0(path,site_names[S],"/mosaic_new/vrts/",date_name,"_",satellite,".vrt")
    tif_name<-paste0(path,site_names[S],"/mosaic_new/tifs/",date_name,"_",satellite,".tif")
    vrt_list<-c()
    #note, using lengths with an "s" instead of length. makes a difference
    for (t in 1:lengths(match_names[i])){
      vrt_list<-c(vrt_list, paste0(planet[S],"/raw/",match_names[[i]][t], "_3B_AnalyticMS_SR_clip.tif"))
    }
    print(vrt_list)
    print("\n")
    gdalbuildvrt(vrt_list, vrt_name)
    writeRaster(stack(vrt_name), tif_name)
  }
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores)
tic()
S<-c(1:20)
clusterApply(cl, S, mosaic_same_date_sat)
stopCluster(cl)
toc()
######################################
###################################### 

######################################
###################################### 


######################################STEP 2 THEN MANUALLY REVIEW IMAGES AND PICK BEST ONES


###########################################################
############################################################STEP 3 ENSURE ALL IMAGES CHARACTERISTICS ARE THE SAME  -- USE GDAL TRANSLATE
############################################################


#Make sure all original data is in the same format. This is useful for making VRTs. Using system GDAL Translate. This was easier than figuring out syntax for GDALUtils



#doing a single one
gdal_command <- paste0(gdal_trans_loc," -ot UInt16 ",file.path(input_dir, "ChadTheFarmer", "mosaic_new", "tifs", "20190629_1020.tif"), " ", 
file.path(input_dir, "ChadTheFarmer", "mosaic_new", "tifs", "updated_20190629_1020.tif"), " -b 1 -b 2 -b 3 -b 4 -co COMPRESS=DEFLATE -colorinterp blue,green,red,undefined")
system(gdal_command)


################################################
planetdir<-list.dirs(input_dir, full.names = T, recursive = F)

#gdal_trans_loc<-"C:\\ProgramData\\Anaconda3\\envs\\arosics\\Library\\bin\\gdal_translate.exe"
gdal_trans_loc<-"/usr/local/Cellar/gdal/2.4.2_2/bin/gdal_translate"

count<-0
#running through folder
for (i in 1:length(planetdir)){
  planetdir<-list.dirs(input_dir, recursive = F)
  site_names<-list.dirs(input_dir, recursive = F, full.names = F)
  files<-list.files(file.path(planetdir[i],"mosaic_new","tifs"), full.names = T)
  filenames<-list.files(file.path(planetdir[i],"mosaic_new","tifs"), full.names = F)
  for (t in 1:length(files)) {
    count<-count+1
    gdal_command <- paste0(gdal_trans_loc," -ot UInt16 ",
                           files[t], " ",
                           file.path(input_dir, site_names[i],"mosaic_new","tifs",paste0("updated_", filenames[t])), " ", 
                           " -b 1 -b 2 -b 3 -b 4 -co COMPRESS=DEFLATE -colorinterp blue,green,red,undefined")
    print(paste0("stating this one - number ",count))
    #system(gdal_command)
  }
}


###########################################################
############################################################STEP 4   #Make the vrts from updated images
############################################################


planet<-list.dirs(input_dir, full.names = T, recursive = F)
#indexdata_folders<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet", full.names = F, recursive = F)
#planetdata_processing<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing", full.names = T, recursive = F)

#list with image date order wanted
for (t in 1:7){
  date_wanted<-c()
  for (i in 1:length(planet)){
    scenes<-list.files(file.path(planet[i],"mosaic_new","tifs"), full.names = T, pattern = "updated")
    #print(planetdata[i])
    date_wanted<-c(date_wanted, scenes[t])
    
  }
  print("break")
  print(date_wanted)
  #print(paste0("date", t))
  gdalbuildvrt(date_wanted, output.vrt = file.path(output_dir,"first_stage_mosaic",paste0("all_sites_time_",t,".vrt")))
}


# #for only one date
# date_wanted<-c()
# for (i in 1:length(planet)){
#   scenes<-list.files(paste0(planet[i],"/mosaicked"), full.names = T, pattern = "updated")
#   #print(planetdata[i])
#   date_wanted<-c(date_wanted, scenes[7])
#   gdalinfo(scenes[7], checksum = T)
#   
# }


############################################################
############################################################STEP 5   #clip to 2000m study areas
############################################################


###############################
parallel_crop_function<-function(S){
  input_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet"
  output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
  library(raster)
  library(sf)
  library(rgeos)
  library(rgdal)
  #load up all NDVI scenes as a VRT
  date1<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_1.vrt"))
  date2<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_2.vrt"))
  date3<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_3.vrt"))
  date4<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_4.vrt"))
  date5<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_5.vrt"))
  date6<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_6.vrt"))
  date7<-stack(file.path(output_dir, "first_stage_mosaic", "all_sites_time_7.vrt"))
  #sites
  farms<-readOGR(file.path(data_dir,"sites_shp", "sites_w_data_2019.geojson"))
  buff<-gBuffer(farms[S,], width = 2050, capStyle = 'SQUARE')
  for (t in 1:7){
    img_mask<-mask(crop(eval(as.name(paste0("date",t))), extent(buff)),buff)
    writeRaster(img_mask, file.path(data_dir,"planet_4analysis","second_stage_clipped",paste0("date_",t,"_site_",S,".tif")), overwrite = T) #"_",farms[S,]$site,
  }
}

###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores)
tic()
S<-c(1:49)
clusterApply(cl, S, parallel_crop_function)
stopCluster(cl)
toc()
######################################

#doing a single one
farms<-readOGR("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/sites_shp/sites_w_data_2019.geojson")
date1<-stack("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/first_stage_mosaic/all_sites_time_1.vrt")
buff<-gBuffer(farms[S,], width = 2050, capStyle = 'SQUARE')

img_mask<-mask(crop(eval(as.name(paste0("date",t))), extent(buff)),buff)
writeRaster(img_mask, paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/second_stage_clipped/date_",t,"_site_",S,".tif"), overwrite = T)


############################################################
############################################################STEP 6    #coregister the images to the first date at each site------
############################################################

#make AROSICS command

#THIS SETS UP THE AROSICS PYTHON BATCH FILE



#THIS SETS UP THE AROSICS PYTHON BATCH FILE
#sink("D:\\Dropbox\\A_School\\2020_GrassyMargins\\3_AcousticAgLandscapes\\code\\landcover\\image_coregistration\\coreg_batch4windows20200706.txt")
sink("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/3_AcousticAgLandscapes/code/landcover/image_coregistration/coreg_batch4windows20200706.txt")

count=0

dir<-file.path(output_dir,"second_stage_clipped")

for (i in 1:49){
  names<-list.files(dir, full.names = T, pattern = ".tif$")
  for (t in 2:7){
    count=count+1
    #print(count)
    reference<-file.path(output_dir, "second_stage_clipped",paste0("date_1_site_",i,".tif"))
    target<-file.path(output_dir, "second_stage_clipped",paste0("date_",t,"_site_",i,".tif"))
    cat(noquote(paste0('python arosics_cli.py local ', reference, " ",target,' 50')))
    #cat(targets[t])
    cat("\n")
    cat("\n")
    cat(paste0("ECHO ", count, " have been completed"))
    cat("\n")
    cat("\n")
    
    
  }
  
}
sink()


############################################################
############################################################STEP 7  -- Put bsq files into tif format in a new folder, change to 64bit tiff because MAD and RADCAL seems to like that better
############################################################

planetdir<-list.files(file.path(output_dir,"3_third_stage_coregistered"), full.names = T)

#gdal_trans_loc<-"C:\\ProgramData\\Anaconda3\\envs\\arosics\\Library\\bin\\gdal_translate.exe"



#running through folder
convert_to_64<-function(N){
  output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  gdal_trans_loc<-"/Users/adamdixon/opt/anaconda3/bin/gdal_translate"
  images<-list.files(file.path(output_dir,"3_third_stage_coregistered"), full.names = T)
  image_names<-list.files(file.path(output_dir,"3_third_stage_coregistered"), full.names = F)
  nc<-nchar(image_names[N])
  if (grepl("\\.tif$", images[N])){
    gdal_command <- paste0(gdal_trans_loc," -ot Float64 ",
                           images[N], " ",
                           file.path(output_dir, "4_fourth_stage_convert64",paste0("updated64_", image_names[N])), " ", 
                           " -b 1 -b 2 -b 3 -b 4 -co COMPRESS=DEFLATE -colorinterp blue,green,red,undefined")
    system(gdal_command)
  }
  if (grepl("\\.bsq$", images[N])){
    gdal_command <- paste0(gdal_trans_loc," -ot Float64 -of GTiff ",
                           images[N], " ",
                           file.path(output_dir, "4_fourth_stage_convert64",paste0("updated64_", substring(image_names[N], 1, nc-4),".tif")), " ", 
                           " -b 1 -b 2 -b 3 -b 4 -co COMPRESS=DEFLATE -colorinterp blue,green,red,undefined")
    system(gdal_command)
  }
  
  if (grepl("\\.hdr$", data[N])){
    break;
  }
}

  
  
###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
N<-1:length(list.files(file.path(output_dir,"3_third_stage_coregistered")))
clusterApply(cl, N, convert_to_64)
stopCluster(cl)
toc()
######################################
######################################

############################################ MAKE VRTS


#make new vrts of clips-------This is not entirely necessary
imagery<-list.files(file.path(output_dir, "4_fourth_stage_convert64"), full.names = F, recursive = F)
#indexdata_folders<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet", full.names = F, recursive = F)
#planetdata_processing<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing", full.names = T, recursive = F)


#list with image date order wanted

for (t in c(1:7)){
  print(t)
  scenes<-grep(pattern = paste0("^date_",t), imagery, value=TRUE) #, perl=TRUE)
  gdalbuildvrt(file.path(output_dir,"4_fourth_stage_convert64",scenes), output.vrt = file.path(output_dir, "6_vrts",paste0("all_sites_time_",t,".vrt")))
}




############################################################
############################################################STEP 8   #Calculate vegetation indices using RSToolbox
############################################################


#This calculates a large set of indices and places them into a multibnad object


parallel_veg_index_function<-function(S){
  library(raster)
  library(RStoolbox)
  dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  data<-list.files(file.path(dir,"third_stage_coregistered"), full.names = T, recursive = F)
  data_names<-list.files(file.path(dir,"third_stage_coregistered"), full.names = F, recursive = F)
  if (grepl("\\.tif$", data[S])){
    z<-spectralIndices(brick(data[S]), blue = 1, green = 2,red = 3, nir = 4, indices = c("NDVI","EVI","SAVI","NDWI"), coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5), scaleFactor = 10000)
    writeRaster(z, file.path(dir, "4_fourth_stage_indices", paste0(substr(data_names[S], 1,nchar(data_names[S])-4),"_indices.tif")), overwrite = T)
  }
  if (grepl("\\.bsq$", data[S])){
    z<-spectralIndices(brick(data[S]), blue = 1, green = 2,red = 3, nir = 4, indices = c("NDVI","EVI","SAVI","NDWI"), coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5), scaleFactor = 10000)
    writeRaster(z, file.path(dir, "4_fourth_stage_indices", paste0(substr(data_names[S], 1,nchar(data_names[S])-4),"_indices.tif")), overwrite = T)
}
}

###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
S<-1:length(list.files(file.path(dir,"third_stage_coregistered")))
clusterApply(cl, S, parallel_veg_index_function)
stopCluster(cl)
toc()
######################################
######################################
z<-spectralIndices(brick("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/data/date_8_site_12.tif"), blue = 1, green = 2,red = 3, nir = 4, indices = c("NDVI","EVI","SAVI","NDWI"), coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5), scaleFactor = 10000)
writeRaster(z, paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/indices/date_8_site_12_indices.tif"), overwrite = T)
############################################################
############################################################STEP 9
############################################################

#make vrts of spectral indices


#make new vrts of clips-------This is not entirely necessary
indices<-list.files(file.path(output_dir, "5_fifth_stage_indices"), full.names = F, recursive = F)
#indexdata_folders<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet", full.names = F, recursive = F)
#planetdata_processing<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing", full.names = T, recursive = F)


#list with image date order wanted

for (t in c(1:7)){
  print(t)
  scenes<-grep(pattern = paste0("^date_",t), indices, value=TRUE) #, perl=TRUE)
  gdalbuildvrt(file.path(output_dir,"5_fifth_stage_indices",scenes), output.vrt = file.path(output_dir, "6_vrts",paste0("all_sites_indices_time_",t,".vrt")))
}


############################################################
############################################################STEP 6 - CALC CHANGE BETWEEN DATE 1 AND 5, and Average of all dates, for just EVI
############################################################



calc_change_par<-function(site_numbers){
  output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  library(raster)
  date1_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_1", full.names = T)
  date2_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_2", full.names = T)
  date3_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_3", full.names = T)
  date4_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_4", full.names = T)
  date5_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_5", full.names = T)
  date6_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_6", full.names = T)
  date7_list<-list.files(file.path(output_dir, "4_fourth_stage_indices"), pattern = "date_7", full.names = T)
  date1<-stack(date1_list[site_numbers])[[2]]
  date2<-stack(date2_list[site_numbers])[[2]]
  date3<-stack(date3_list[site_numbers])[[2]]
  date4<-stack(date4_list[site_numbers])[[2]]
  date5<-stack(date5_list[site_numbers])[[2]]
  date6<-stack(date6_list[site_numbers])[[2]]
  date7<-stack(date7_list[site_numbers])[[2]]
  dates<-stack(date1,date2,date3,date3,date4,date5,date6,date7)
  # #date 5 minus date 1, to get soybeans
  # writeRaster(date5-date1, file.path(output_dir, "6_evi_stats",paste0("evi_change_site_",site_numbers,".tif")),overwrite = T)
  # #get average
  # average<-(date1+date2+date3+date4+date5+date6+date7)/7
  # writeRaster(average, file.path(output_dir, "6_evi_stats",paste0("evi_average_site_",site_numbers,".tif")),overwrite = T)
  # #median
  # med<-calc(dates, median)
  # writeRaster(med, file.path(output_dir, "6_evi_stats",paste0("evi_med_site_",site_numbers,".tif")), overwrite = T)
  # #std deviation
  # stddev<-calc(dates, sd)
  # writeRaster(stddev, file.path(output_dir, "7_evi_stats",paste0("evi_stddev_site_",site_numbers,".tif")), overwrite = T)
  # #get slope of change for date 5 to 1 - might be good for soybeans
  # writeRaster(((date5-date1)/4), file.path(output_dir, "7_evi_stats",paste0("evi_4date_slope_site_",site_numbers,".tif")),overwrite = T)
  # #get slope of change for date 5 to 3 - might be good for corn
  writeRaster(((date3-date1)/2), file.path(output_dir, "7_evi_stats",paste0("evi_2date_slope_site_",site_numbers,".tif")),overwrite = T)

}
  
###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
site_numbers<-19:49
clusterApply(cl, site_numbers, calc_change_par)
stopCluster(cl)
toc()
######################################
######################################






new_change_files<-list.files(file.path(output_dir, "7_evi_stats"), pattern = "change", full.names = T)
gdalbuildvrt(new_change_files, output.vrt = file.path(output_dir, "6_vrts","evi_change.vrt"))

new_average_files<-list.files(file.path(output_dir, "7_evi_stats"), pattern = "average", full.names = T)
gdalbuildvrt(new_average_files, output.vrt = file.path(output_dir, "6_vrts","evi_average.vrt"))

new_median_files<-list.files(file.path(output_dir, "7_evi_stats"), pattern = "med", full.names = T)
gdalbuildvrt(new_median_files, output.vrt = file.path(output_dir, "6_vrts","evi_median.vrt"))

new_2dayslope_files<-list.files(file.path(output_dir, "7_evi_stats"), pattern = "2date_slope", full.names = T)
gdalbuildvrt(new_2dayslope_files, output.vrt = file.path(output_dir, "6_vrts","evi_2date_slope.vrt"))

new_4dateslope_files<-list.files(file.path(output_dir, "7_evi_stats"), pattern = "4date_slope", full.names = T)
gdalbuildvrt(new_4dateslope_files, output.vrt = file.path(output_dir, "6_vrts","evi_4date_slope.vrt"))

new_stdev_files<-list.files(file.path(output_dir, "7_evi_stats"), pattern = "stddev", full.names = T)
gdalbuildvrt(new_stdev_files, output.vrt = file.path(output_dir, "6_vrts","evi_stdev.vrt"))
# date1<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/indices", pattern = "date_1")
# date6<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/indices", pattern = "date_6")




############################################################
############################################################STEP 7 - RESAMPLE EVERYTHING TO MATCH NAIP RESOLUTION
############################################################

library(raster)
library(gdalUtils)

#RESAMPLE SET ORIGIN RASTERS, and run through all PS imagery
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

###################################### RESAMPLE ENGINEERED FEATURES
resample_engineered_ps<-function(l){
  library(raster)
  ##################
  #
  #INPUTS   ##########FOR NOW GRABBING STDEV, MEDIAN, and CHANGE
  ###################
  #ps_ndvi_second_order<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/7_evi_stats")
  naips<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected")
  naip_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected"
  vrts<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts"
  ps_ndvi_second_order_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/7_evi_stats"
  # #set origin of NAIP since it's off a little
  #######################PATTERN MATCHING
  naip<-grep(pattern = paste0("Site_",l,"_32615.tif"), naips, value = T, perl = T) #just to note: I didn't really need the grep. I could have just put together the file name
  #REF RAS
  high_res<-stack(file.path(naip_dir,naip))
  origin(high_res)<-c(0,0) #set origin to 0,0   #this is a dubious method, I just don't know a better way
  #####TO RESAMPLE ----NOTE THESE ARE VRTS BEING CROPPED
  ch<-crop(stack(file.path(vrts, "evi_change.vrt")), extent(high_res))
  std<-crop(stack(file.path(vrts, "evi_stdev.vrt")), extent(high_res))
  med<-crop(stack(file.path(vrts, "evi_median.vrt")), extent(high_res))
  #change
  ###############################
  change_resample_to_naip<-resample(x = ch, y = high_res, method = "bilinear")
  writeRaster(change_resample_to_naip, file.path(ps_ndvi_second_order_dir, "resampled_to_NAIP", paste0("evi_change_site_",l,".tif")))
  #stdev
  stdev_resample_to_naip<-resample(x = std, y = high_res, method = "bilinear")
  writeRaster(stdev_resample_to_naip, file.path(ps_ndvi_second_order_dir, "resampled_to_NAIP", paste0("evi_stdev_site_",l,".tif")))
  #median
  med_resample_to_naip<-resample(x = med, y = high_res, method = "bilinear")
  writeRaster(stdev_resample_to_naip, file.path(ps_ndvi_second_order_dir, "resampled_to_NAIP", paste0("evi_med_site_",l,".tif")))
  #reset origin NAIO
  writeRaster(high_res, file.path(naip_dir, "reset_origin", naip))
}


# test1<-raster("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected/reset_origin/Site_3_32615.tif")
# test2<-raster("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/7_evi_stats/resampled_to_NAIP/evi_change_site_3.tif")
# test3<-raster("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/resampled_to_NAIP/date_1_site_3_indices.tif")
# test4<-raster("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/7_evi_stats/resampled_to_NAIP/evi_stdev_site_3.tif")
# stack(test1, test2,test3,test4)

###########PARALLEL
library(parallel)
ncores<-detectCores(logical = F)
cl<-makeCluster(ncores-1)
l<-seq(1,49,1)
clusterApply(cl, l, resample_engineered_ps)
stopCluster(cl)
######################################

###################################### RESAMPLE DEM

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
  origin(high_res)<-c(0,0) #set origin to 0,0   #this is a dubious method, I just don't know a better way
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






