library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(caret)
library(doParallel)

# clip_raster<-function(S){
#   #Indices are in the order of NDVI, EVI, SAVI, and water index
#   buffer_radius<-2050
#   data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
#   farms<-readOGR(file.path(data_dir,"sites_shp", "sites_w_data_2019.geojson"))
# 
#   #######NAIP
#   NAIP<-brick("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected/Site_1_32615.tif")
# 
#   #get the number of bands for use later on, and save for use in dfAll function
#   n_dates<-3
#   # and rename the planet bands for convenience and for further coding
#   #adding the change data
#   buff<-gBuffer(farms[S,], width = buffer_radius, capStyle = 'SQUARE')
#   #perform mask and crop
#   img_mask<-mask(crop(img, extent(buff)),buff)
#   # and rename the planet bands for convenience and for further coding
#   #adding the change data
#   bands<-c(paste("date_", 1:(n_dates), sep=""),"med","chg","stdev","fourDate","twoDate","DEM")
#   names(img_mask)=bands
#   return(img_mask)
# }
# 
# ras_out<-clip_raster(S)
# 
# plot(ras_out[[2]])

#don't need above
NAIP<-stack("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/projected/Site_1_32615.tif")
data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
farms<-readOGR(file.path(data_dir,"sites_shp", "sites_w_data_2019.geojson"))

#farms_select<-farms[farms$site=="Anderson_Huckes"|farms$site=="Anderson_Kludas"|farms$site=="Anderson_Parrotts"|farms$site=="Northwest_research_farm",]
farms_select<-farms[farms$site=="Allee_1",]

training<-shapefile("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/training_data_round2/final_set2/all_boxes_grass_woody_extra.shp")
buffer_radius<-2050
buff<-gBuffer(farms_select, width = buffer_radius, capStyle = 'SQUARE')
training_clip<-raster::intersect(training, buff)
plot(buff)
plot(training_clip)

# 
# #clipping on raster
# clipping_training_data<-function(S){
#   buffer_radius<-2050
#   data_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
#   farms<-readOGR(file.path(data_dir,"sites_shp", "sites_w_data_2019.geojson"))
#   training<-shapefile("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/training_data_round2/final_set2/all_boxes_grass_woody_extra.shp")
#   buff<-gBuffer(farms[S,], width = buffer_radius, capStyle = 'SQUARE')
#   training_clip<-raster::intersect(training, buff)
#   return(training_clip)
# }



table(training_clip$LC6)
head(training_clip)


#extracting training values from input raster

extract_training<-function(ras, train){
  img<-ras
  trainData<-train
  responseCol<-"LC6"
  #get the number of bands for use later on, and save for use in dfAll function
  n_dates_bands<-length(img@layers)
  # and rename ndvi for convenience and for further coding
  #adding the change data
  dates<-c(paste("d", 1:(n_dates_bands), sep=""))
  names(img)=dates
  # Extract training data values from stack
  #number of columns will be number of dates + a column for "class"
  dfAll = setNames(data.frame(matrix(ncol = n_dates_bands+1, nrow = 0)), c(c(dates),"class"))
  for (i in 1:length(unique(trainData[[responseCol]]))){
    category <- unique(trainData[[responseCol]])[i]
    categorymap <- trainData[trainData[[responseCol]] == category,]
    beginCluster()
    dataSet <- raster::extract(img, categorymap)
    endCluster()
    if(is(trainData, "SpatialPointsDataFrame")){
      dataSet <- cbind(dataSet, class = as.numeric(rep(category, nrow(dataSet))))
      dfAll <- rbind(dfAll, dataSet[complete.cases(dataSet),])
    }
    if(is(trainData, "SpatialPolygonsDataFrame")){
      dataSet <- dataSet[!unlist(lapply(dataSet, is.null))]
      dataSet <- lapply(dataSet, function(x){cbind(x, class = as.double(rep(category, nrow(x))))})
      df <- do.call("rbind", dataSet)
      dfAll <- rbind(dfAll, df)
    }
    
  }
  dfAll2<-filter(dfAll, d1 != -3.400000e+38, d2 != -3.400000e+38, d3 != -3.400000e+38)
  return(dfAll2)
}

e<-extract_training(ras = NAIP, train = training_clip)
table(e$class)


adjust_samples<-function(x, samples_per_class, classes_col,crop_perc, noncrop_perc, built_water_perc){
  for (i in 1:length(unique(x[, classes_col]))){
    class.i <- unique(x[, classes_col])[i]
    if((sum(x[, classes_col] == class.i) - samples_per_class) != 0){
      x <- x[-sample(which(x[, classes_col] == class.i),
                     sum(x[, classes_col] == class.i) - samples_per_class), ]
    }
  }
  #return(x2)
  table(x$class)
  #takes a balanced dataset, reclassifies and adjusts according to percent parameter
  corn<-x[x$class==1,]; soy<-x[x$class==2,]; grass<-x[x$class==3,]; woody<-x[x$class==4,]; built<-x[x$class==5,]
  water<-x[x$class==6,]; bare<-x[x$class==7,]
  nrow(corn); nrow(soy); nrow(grass); nrow(woody); nrow(built); nrow(water); nrow(bare)
  soy$class<-1
  crop<-rbind(corn, soy)
  nrow(crop)
  grass$class<-2
  woody$class<-2
  bare$class<-2
  noncrop<-rbind(grass, woody, bare)
  nrow(noncrop)
  built$class<-3
  water$class<-3
  built_water<-rbind(built, water)
  nrow(built_water)
  crop2<-crop[sample(nrow(crop), (nrow(crop)*crop_perc)),]
  noncrop2<-noncrop[sample(nrow(noncrop), (nrow(noncrop)*noncrop_perc)),]
  built_water2<-built_water[sample(nrow(built_water), (nrow(built_water)*built_water_perc)),]
  output<-rbind(crop2, noncrop2, built_water2)
  table(output$class)
  return(output)
}


as<-adjust_samples(x = e, classes_col = "class", samples_per_class = 100, crop_perc = 1, noncrop_perc = 1, built_water_perc = .3)
table(as$class)
head(as)


#SVM CLASSIFICATION
svm_model<-function(train_samples, img_files, PreProcess = NULL){
  todays_date<-gsub("-","",Sys.Date())
  
  if (dir.exists(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date)) == F) {
    dir.create(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date))
  }
  if (dir.exists(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date)) == F) {
    dir.create(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date))
  }
  if (dir.exists(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date,"/svm")) == F) {
    dir.create(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date,"/svm"))
  }
  if (dir.exists(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date,"/tifs")) == F) {
    dir.create(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date,"/tifs"))
  }
  if (dir.exists(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date,"/vrt")) == F) {
    dir.create(paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date,"/vrt"))
  }
  
  dir<-paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date)
  
  balanced<-train_samples
  nb<-length(img_files@layers) #just grabbing the band number length from img
  dates<-c(paste("d", 1:nb, sep=""))
  names(img_files)<-dates
  #############################################
  # Create training and testing datasets
  inTrain <- createDataPartition(y = balanced$class, p = 0.7, list = FALSE)
  training <- balanced[inTrain,]
  testing <- balanced[-inTrain,]
  # #############################################\
  #https://github.com/lgreski/datasciencectacontent/blob/master/markdown/pml-randomForestPerformance.md
  cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
  registerDoParallel(cluster)
  #generate the model equation based on number of band/date variables

  f <- as.formula(
    paste("as.factor(class)", 
          paste("d", 1:(nb), sep = "",collapse = " + "), 
          sep = " ~ "))
  set.seed(123)
  ########################SVM is 5 k-fold cross validated###############################
  tc<-trainControl(method = "cv", number = 5, allowParallel = TRUE, savePredictions = T)
  mod.svm <- train(f, method = "svmLinear", data = training, trControl = tc, PreProcess = PreProcess)
  stopCluster(cluster)
  registerDoSEQ()
  #now testing the accuracy of the model using the testing data
  mod_pred <- predict(mod.svm, testing)
  conf_mat<-confusionMatrix(mod_pred, as.factor(testing$class))

  #apply prediction values to raster
  print("Applying predictions to raster stack")
  beginCluster()
  svm<-raster::predict(img_files, model = mod.svm)
  endCluster()
  #save SVM model in case useful later on
  saveRDS(mod.svm, file.path(dir,"svm", "ndvi_3_classes.rds"))
  writeRaster(svm, file.path(dir,"tifs", "svm.tif"),overwrite=T)
  return(conf_mat)
}


svm_model(train_samples = as, img_files = NAIP)


print(mod)
todays_date<-gsub("-","",Sys.Date())
dir<-paste0("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_200m_",todays_date)
lc_out<-raster(file.path(dir,"tifs", "svm.tif"))

buffer_radius<-200

buff200<-gBuffer(farms_select, width = buffer_radius, capStyle = 'SQUARE')
lc<-mask(crop(lc_out, extent(buff200)),buff200)

if (all(unique(lc) %in% c(1))) {
  mycol=c("yellow")
} else if (all(unique(lc) %in%  c(1,2))) {
  mycol=c("yellow","olivedrab3")
} else if (all(unique(lc) %in%  c(1,3))) {
  mycol=c("yellow","gray")
} else if (all(unique(lc) %in%  c(1,4))) {
  mycol=c("yellow","blue")
} else if (all(unique(lc) %in% c(1,2,3))) {
  mycol=c("yellow","olivedrab3","gray")
}

raster::plot(lc, col=mycol, legend=F, ext=raster::extent(lc),axes=F,box=F)

#run svm

#print model results


#write rasters


