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
gdal_command <- paste0(gdal_trans_loc," -ot UInt16 ","/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet/choquette_salix/mosaic_new/tifs/updated_20190727_merged_32615_2.tif", " ", "/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet/choquette_salix/mosaic_new/tifs/updated_20190727_merged_32615_3.tif", " -b 1 -b 2 -b 3 -b 4 -co COMPRESS=DEFLATE -colorinterp blue,green,red,undefined")
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
  files<-list.files(file.path(planetdir[i],"mosaic_new","tifs"), full.names = T)   #remove 32615
  filenames<-list.files(file.path(planetdir[i],"mosaic_new","tifs"), full.names = F)
  for (t in 1:length(files)) {
    count<-count+1
    gdal_command <- paste0(gdal_trans_loc," -ot UInt16 ",
                           files[t], " ",
                           file.path(input_dir, site_names[i],"mosaic_new","tifs",paste0("updated_", filenames[t])), " ", 
                           " -b 1 -b 2 -b 3 -b 4 -co COMPRESS=DEFLATE -colorinterp blue,green,red,undefined")
    print(paste0("stating this one - number ",count))
    system(gdal_command)
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
    scenes<-list.files(file.path(planet[i],"mosaic_new","tifs"), full.names = T, pattern = "updated", recursive = F)
    #print(planetdata[i])
    date_wanted<-c(date_wanted, scenes[t])
    
  }
  print("break")
  print(date_wanted)
  print(paste0("date", t))
  gdalbuildvrt(date_wanted, output.vrt = file.path(output_dir,"1_first_stage_mosaic",paste0("all_sites_time_",t,".vrt")))
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
  output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
  library(raster)
  library(sf)
  library(rgeos)
  library(rgdal)
  #load up all NDVI scenes as a VRT
  date1<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_1.vrt"))
  date2<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_2.vrt"))
  date3<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_3.vrt"))
  date4<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_4.vrt"))
  date5<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_5.vrt"))
  date6<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_6.vrt"))
  date7<-stack(file.path(output_dir, "1_first_stage_mosaic", "all_sites_time_7.vrt"))
  #sites
  farms<-readOGR(file.path(data_dir,"sites_shp", "sites_w_data_2019.geojson"))
  buff<-gBuffer(farms[S,], width = 2050, capStyle = 'SQUARE')
  for (t in 1:7){
    img_mask<-mask(crop(eval(as.name(paste0("date",t))), extent(buff)),buff)
    writeRaster(img_mask, file.path(data_dir,"planet_4analysis","2_second_stage_clipped",paste0("date_",t,"_site_",S,".tif")), overwrite = T) #"_",farms[S,]$site,
  }
}

###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-2)
tic()
S<-c(50)
clusterApply(cl, S, parallel_crop_function)
stopCluster(cl)
toc()
######################################

#doing a single one
farms<-readOGR("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/sites_shp/sites_w_data_2019.geojson")
date1<-stack("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/1_first_stage_mosaic/all_sites_time_1.vrt")
buff<-gBuffer(farms[S,], width = 2050, capStyle = 'SQUARE')

img_mask<-mask(crop(eval(as.name(paste0("date",t))), extent(buff)),buff)
writeRaster(img_mask, paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/second_stage_clipped/date_",t,"_site_",S,".tif"), overwrite = T)


############################################################
############################################################STEP 6  #Calculate vegetation indices using RSToolbox
############################################################


#This calculates a large set of indices and places them into a multibnad object


parallel_veg_index_function<-function(S){
  library(raster)
  library(RStoolbox)
  main_folder<-"2_second_stage_clipped"
  output_folder<-"5_fifth_stage_indices"
  dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  data<-list.files(file.path(dir, main_folder), full.names = T, recursive = F)
  data_names<-list.files(file.path(dir,main_folder), full.names = F, recursive = F)
  if (grepl("\\.tif$", data[S])){
    z<-spectralIndices(brick(data[S]), blue = 1, green = 2,red = 3, nir = 4, indices = c("NDVI","EVI","SAVI","NDWI"), coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5), scaleFactor = 10000)
    writeRaster(z, file.path(dir, output_folder, paste0(substr(data_names[S], 1,nchar(data_names[S])-4),"_indices.tif")), overwrite = T)
  }
  if (grepl("\\.bsq$", data[S])){
    z<-spectralIndices(brick(data[S]), blue = 1, green = 2,red = 3, nir = 4, indices = c("NDVI","EVI","SAVI","NDWI"), coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5), scaleFactor = 10000)
    writeRaster(z, file.path(dir, output_folder, paste0(substr(data_names[S], 1,nchar(data_names[S])-4),"_indices.tif")), overwrite = T)
  }
}

# parallel_veg_index_function(53)
# 
# EVI<-function(ps_img) {
#   img<-brick(ps_img)
#   L=1
#   C1=6
#   C2=7.5
#   evi<-2.5 * (img[[4]]-img[[3]])/
#   (img[[4]]+C1*img[[3]]-C2*img[[2]]+L)
# }

###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
#S<-1:length(list.files(file.path(dir,"3_third_stage_coregistered")))
S<-101:357
clusterApply(cl, S, parallel_veg_index_function)
stopCluster(cl)
toc()

######################################
######################################
#S<-1:length(list.files(file.path(dir,"4_fourth_stage_convert64")))
S<-grep(pattern = "site_50", list.files(file.path(output_dir,"4_fourth_stage_convert64"), full.names = T))
lapply(S, parallel_veg_index_function)
######################################
######################################
z<-spectralIndices(brick("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/data/date_8_site_12.tif"), blue = 1, green = 2,red = 3, nir = 4, indices = c("NDVI","EVI","SAVI","NDWI"), coefs = list(L = 0.5, G = 2.5, L_evi = 1, C1 = 6, C2 = 7.5), scaleFactor = 10000)
writeRaster(z, paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/indices/date_8_site_12_indices.tif"), overwrite = T)
############################################################
############################################################STEP 7
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
############################################################STEP 8 - CALC CHANGE BETWEEN DATE 1 AND 5, and Average of all dates, for just EVI
############################################################



calc_change_par<-function(site_numbers){
  output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis"
  library(raster)
  date1_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_1", full.names = T)
  date2_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_2", full.names = T)
  date3_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_3", full.names = T)
  date4_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_4", full.names = T)
  date5_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_5", full.names = T)
  date6_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_6", full.names = T)
  date7_list<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "date_7", full.names = T)
  date1_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date1_list,value = T,perl = T)
  date2_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date2_list,value = T)
  date3_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date3_list,value = T)
  date4_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date4_list,value = T)
  date5_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date5_list,value = T)
  date6_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date6_list,value = T)
  date7_grep<-grep(pattern = paste0("site_",site_numbers,"_"), date7_list,value = T)
  date1<-brick(date1_grep)[[2]]
  date2<-brick(date2_grep)[[2]]
  date3<-brick(date3_grep)[[2]]
  date4<-brick(date4_grep)[[2]]
  date5<-brick(date5_grep)[[2]]
  date6<-brick(date6_grep)[[2]]
  date7<-brick(date7_grep)[[2]]
  dates<-brick(date1,date2,date3,date3,date4,date5,date6,date7)
  #date 5 minus date 1, to get soybeans
  writeRaster(date5-date1, file.path(output_dir, "5_fifth_stage_indices",paste0("evi_change_site_",site_numbers,".tif")),overwrite = T)
  #get average
  average<-(date1+date2+date3+date4+date5+date6+date7)/7
  writeRaster(average, file.path(output_dir, "5_fifth_stage_indices",paste0("evi_average_site_",site_numbers,".tif")),overwrite = T)
  #median
  med<-calc(dates, median)
  writeRaster(med, file.path(output_dir, "5_fifth_stage_indices",paste0("evi_med_site_",site_numbers,".tif")), overwrite = T)
  #std deviation
  stddev<-calc(dates, sd)
  writeRaster(stddev, file.path(output_dir, "5_fifth_stage_indices",paste0("evi_stddev_site_",site_numbers,".tif")), overwrite = T)
  #get slope of change for date 5 to 1 - might be good for soybeans
  writeRaster(((date5-date1)/4), file.path(output_dir, "5_fifth_stage_indices",paste0("evi_4date_slope_site_",site_numbers,".tif")),overwrite = T)
  #get slope of change for date 5 to 3 - might be good for corn
  writeRaster(((date3-date1)/2), file.path(output_dir, "5_fifth_stage_indices",paste0("evi_2date_slope_site_",site_numbers,".tif")),overwrite = T)
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-2)
tic()
site_numbers<-50:51
clusterApply(cl, site_numbers, calc_change_par)
stopCluster(cl)
toc()
######################################
######################################


new_change_files<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "change", full.names = T)
gdalbuildvrt(new_change_files, output.vrt = file.path(output_dir, "6_vrts","evi_change.vrt"))

new_average_files<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "average", full.names = T)
gdalbuildvrt(new_average_files, output.vrt = file.path(output_dir, "6_vrts","evi_average.vrt"))

new_median_files<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "med", full.names = T)
gdalbuildvrt(new_median_files, output.vrt = file.path(output_dir, "6_vrts","evi_median.vrt"))

new_2dayslope_files<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "2date_slope", full.names = T)
gdalbuildvrt(new_2dayslope_files, output.vrt = file.path(output_dir, "6_vrts","evi_2date_slope.vrt"))

new_4dateslope_files<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "4date_slope", full.names = T)
gdalbuildvrt(new_4dateslope_files, output.vrt = file.path(output_dir, "6_vrts","evi_4date_slope.vrt"))

new_stdev_files<-list.files(file.path(output_dir, "5_fifth_stage_indices"), pattern = "stddev", full.names = T)
gdalbuildvrt(new_stdev_files, output.vrt = file.path(output_dir, "6_vrts","evi_stdev.vrt"))
# date1<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/indices", pattern = "date_1")
# date6<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_clipped/indices", pattern = "date_6")




############################################################
############################################################STEP 9 - RESAMPLE EVERYTHING TO MATCH NAIP RESOLUTION
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
  #origin(high_res)<-c(0,0) #set origin to 0,0
  #writeRaster(high_res, file.path(naip_dir, "reset_origin", naip))
  for (i in 1:7){
    #######################PATTERN MATCHING
    ps<-grep(pattern = paste0("date_",i,"_site_",l,"_indices.tif"), ps_ndvis, value = T)
    print(ps)
    low_res<-stack(file.path(ps_dir, "5_fifth_stage_indices", ps))[[2]]#set to get EVI
    #use the new naip as reference to resample ps
    resample_to_naip<-resample(x = low_res, y = high_res, method = "bilinear")
    writeRaster(resample_to_naip, file.path(ps_dir, "resampled_to_NAIP2", ps))
  }
  #######################PATTERN MATCHING for engineered features -- STDEV
  stdev<-grep(pattern = paste0("evi_stddev_site_",l,".tif"), ps_ndvis, value = T)
  print(stdev)
  low_res_sd<-stack(file.path(ps_dir, "5_fifth_stage_indices", stdev))
  #use the new naip as reference to resample ps
  resample_to_naip_sd<-resample(x = low_res_sd, y = high_res, method = "bilinear")
  writeRaster(resample_to_naip_sd, file.path(ps_dir, "resampled_to_NAIP2", stdev))
  #######################PATTERN MATCHING for engineered features -- MEDIAN
  med<-grep(pattern = paste0("evi_med_site_",l,".tif"), ps_ndvis, value = T)
  print(med)
  low_res_med<-stack(file.path(ps_dir, "5_fifth_stage_indices", med))
  #use the new naip as reference to resample ps
  resample_to_naip_med<-resample(x = low_res_med, y = high_res, method = "bilinear")
  writeRaster(resample_to_naip_med, file.path(ps_dir, "resampled_to_NAIP2", med))
  #######################PATTERN MATCHING for engineered features -- CHANGE
  change<-grep(pattern = paste0("evi_change_site_",l,".tif"), ps_ndvis, value = T)
  print(change)
  low_res_change<-stack(file.path(ps_dir, "5_fifth_stage_indices", change))
  #use the new naip as reference to resample ps
  resample_to_naip_chg<-resample(x = low_res_change, y = high_res, method = "bilinear")
  writeRaster(resample_to_naip_chg, file.path(ps_dir, "resampled_to_NAIP2", change))
  #######################PATTERN MATCHING for engineered features -- AVERAGE
  avg<-grep(pattern = paste0("evi_average_site_",l,".tif"), ps_ndvis, value = T)
  print(change)
  low_res_change_avg<-stack(file.path(ps_dir, "5_fifth_stage_indices", avg))
  #use the new naip as reference to resample ps
  resample_to_naip_avg<-resample(x = low_res_change_avg, y = high_res, method = "bilinear")
  writeRaster(resample_to_naip_avg, file.path(ps_dir, "resampled_to_NAIP2", avg))
  
}



#make new vrts 
data_list<-list.files(file.path(output_dir, "7_resampled_to_NAIP"), full.names = T, recursive = F)
#indexdata_folders<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet", full.names = F, recursive = F)
#planetdata_processing<-list.dirs("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing", full.names = T, recursive = F)


data<-c("date_1", "date_2", "date_3", "date_4", "date_5", "date_6", "date_7", "stddev", "med", "change", "average")

#list with image date order wanted
count<-0
for (t in 1:length(data)){
  print(t)
  data_bunch<-grep(pattern = data[t], data_list, value=TRUE) #, perl=TRUE)
  count<- count+length(data_bunch)
  print(count)
  print(data_bunch)
  print(length(data_bunch))
  gdalbuildvrt(data_bunch, output.vrt = file.path(output_dir, "6_vrts",paste0("all_sites_indices_resample_", data[t],".vrt")))
}



###################################### RESAMPLE DEM

DEMS<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM", pattern = ".img$", full.names = T, recursive = T)


outdir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs"
batch_gdal_translate(DEMS,outdir,outsuffix = "_conv.tif")


gdal_translate("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/DEM_3M_I_97/DEM_3m_I_97.img",dst_dataset = "/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/DEM_3m_I_97_conv.tif")


#cmake vrt
DEMStiff<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs", pattern = ".tif$", full.names = T, recursive = F)
gdalbuildvrt(DEMStiff, output.vrt = file.path("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/vrts/DEM.vrt"))

############################################################
#############################################################clip to 2000m study areas STEP 10
############################################################

###############################
parallel_tpi_function<-function(S){
  library(gdalUtils)
  library(raster)
  library(rgdal)
  library(rgeos)
  library(sp)
  dir_path<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs"
  #DEM raw
  DEMS<-brick(file.path(dir_path, "vrts", "DEM.vrt"))
  #clip to farm
  data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
  farms<-readOGR(file.path(data_dir,"sites_shp", "sites_w_data_2019.geojson"))
  buff<-gBuffer(farms[S,], width = 2050, capStyle = 'SQUARE')
  crs(DEMS)
  buff_p<-spTransform(buff, CRSobj = "+proj=utm +zone=15 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs ")
  DEM_crop<-crop(DEMS, extent(buff_p))
  writeRaster(DEM_crop, file.path(dir_path,"TPI2","cropped",paste0("DEM_cropped_Site_",S,"_32615.tif")))
  ##DEM cropped needed to be made bc terrain needs a RasterLayer, which is working weird with the crop
  cropped_files<-list.files(file.path(dir_path, "TPI2","cropped"), full.names = T, pattern = ".tif")
  cropped<-grep(pattern = paste0("Site_", S, "_32615.tif"), cropped_files, value = T)
  #run TPI
  TPI<-terrain(raster(cropped), opt="TPI", filename = file.path(dir_path, "TPI2","TPI",paste0("TPI_Site_",S,"_32615.tif")))
  #resample, reproject to NAIP, write Raster
  naip_files<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected", full.names = T, pattern = ".tif")
  naip<-grep(pattern = paste0("Site_", S, "_32615.tif"), naip_files, value = T)
  #tpi files
  tpi_files<-list.files(file.path(dir_path,"TPI2"), full.names = T, recursive = F, pattern = ".tif")
  tpi2<-grep(pattern = paste0("Site_", S, "_32615.tif"), tpi_files, value = T)
  #align resample reproject
  align_rasters(unaligned = tpi2, reference = naip, dstfile = file.path(dir_path,"TPI2","resampled",paste0("TPI_DEM_resampled_aligned_Site_",S,"_32615.tif")))
  
}


###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores)
tic()
S<-c(1:51)
clusterApply(cl, S, parallel_tpi_function)
stopCluster(cl)
toc()
######################################


#######MAKE VRT for TPI
input_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/TPI/resampled"
newTPI<-list.files(file.path(input_dir), pattern = ".tif", full.names = T)
gdalbuildvrt(newTPI, output.vrt = file.path("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/vrts","TPI_DEM_aligned_clipped_32615.vrt"))





