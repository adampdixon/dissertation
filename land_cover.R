library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(caret)
library(doParallel)


outputs<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites"
number<-4
todays_date<-gsub("-","",Sys.Date())

##################################################
#RUN PRIMARY FUNCTION
##################################################
lc_class(outputs, number)
##################################################

##################################################
#COMPONENT FUNCTIONS
##################################################

lc_class<-function(outputs, number){ #, ras, farms, number, training, width, classes_col, plot_width, samples_per_class, crop_perc, noncrop_perc, built_water_perc, svm_lc_in_mem, ...){
  ###################################################################################################################################
  #inputs
  ras<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/vrt/NAIP_32615.vrt"
  farms<-readOGR(file.path("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data","sites_shp", "sites_w_data_2019.geojson"))
  training<-shapefile("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/training_data_round2/final_set2/all_boxes_grass_woody_extra.shp")
  width<-2050
  plot_width<-200
  samples_per_class <- 200
  number<-number
  classes_col<-"class"
  crop_perc <- 1
  noncrop_perc <- 1
  built_water_perc <- 1
  bare_perc <- 1
  svm_lc_in_mem <- "no"
  clip_ras <- "yes" #clip raster to predict to "plot_width amount?
  PreProcess <- NULL
  ##########################################################################################################################################
  #get farm name
  
  #farm_name<-farms[number,]
  #make dirs
  make_output_directories()
  #image clip
  train<-training_clip(training, farms, number, width)
  #raster clip
  img_clip<-image_clip(ras, farms, number, width)
    #extract values, save values
  e<-extract_training(img_clip, train = train)
  table(e$class)
  head(e)
  #adjust values
  as<-adjust_samples(ext_values = e, classes_col = classes_col, samples_per_class = samples_per_class, crop_perc = crop_perc, noncrop_perc = noncrop_perc, built_water_perc = built_water_perc, bare_perc = bare_perc)
  table(as$class)
  head(as)
  #run model, save results
  s<-svm_model(farms, number, plot_width, svm_lc_in_mem, clip_ras, train_samples = as, img_files = img_clip, PreProcess)
  #plot outcome
  plot_lc(number, plot_width, farms, img_files =  img_clip)
}

make_output_directories<-function(){
  message("Setting up folder structure")
  if (dir.exists(file.path(outputs,paste0("landcover_",todays_date))) == F) {
    dir.create(file.path(outputs,paste0("landcover_",todays_date)))
  }
  if (dir.exists(file.path(outputs,paste0("landcover_",todays_date),"training_values")) == F) {
    dir.create(file.path(outputs,paste0("landcover_",todays_date),"training_values"))
  }
  if (dir.exists(file.path(outputs,paste0("landcover_",todays_date),"svm")) == F) {
    dir.create(file.path(outputs,paste0("landcover_",todays_date),"svm"))
  }
  if (dir.exists(file.path(outputs,paste0("landcover_",todays_date),"tifs")) == F) {
    dir.create(file.path(outputs,paste0("landcover_",todays_date),"tifs"))
  }
  if (dir.exists(file.path(outputs,paste0("landcover_",todays_date),"vrt")) == F) {
    dir.create(file.path(outputs,paste0("landcover_",todays_date),"vrt"))
  }
}

#clip training
training_clip<-function(training, farms, number, width){
  message("working on training clip")
  #training input -> training data
  #vector input
  farms_select<-farms[number,]
  #buffer
  buff<-gBuffer(farms_select, width = width, capStyle = 'SQUARE')
  #mask
  training_clip<-raster::intersect(training, buff)
  plot(buff)
  plot(training_clip, add=T)
  message("done with training clip")
  return(training_clip)
}

#clip raster
image_clip<-function(ras, farms, number, width){
  message("working on image clip")
  if (file.exists(file.path(tempdir(), "img_clip.tif"))) {
    m <- stack(file.path(tempdir(), "img_clip.tif"))
    message("using image in temp file")
  } else {  #raster input
    ras1<-stack(ras)
    #vector input
    farms_select<-farms[number,]
    #buffer
    buff<-gBuffer(farms_select, width = width, capStyle = 'SQUARE')
    #mask
    m<-stack(mask(crop(ras1, extent(buff)), buff))
    #plot(buff)
    #plot(m[[1]], add=T)

    
    temp_file <- writeRaster(m, file.path(tempdir(), "img_clip.tif"))
    message("done with image clip")
  }
  plotRGB(m,
          r = 1, g = 2, b = 3,
          stretch = "lin",
          axes = FALSE, margins=TRUE, main = paste0("img"))
  return(m)
}

# extracting training values from input raster
extract_training<-function(image_clip, train){
  message("extract training data")
  if (identical(file.exists(file.path(outputs,paste0("landcover_",todays_date),"training_values","values_extraction.csv")), TRUE)){
    message("extraction already completed. moving on...")
    dfAll2<-read.csv(file.path(outputs,paste0("landcover_",todays_date),"training_values","values_extraction.csv"))
    }else{
      message("extracting training values")
      trainData<-train
      responseCol<-"LC6"
      #get the number of bands for use later on, and save for use in dfAll function
      n_dates_bands<-length(image_clip@layers)
      # and rename ndvi for convenience and for further coding
      #adding the change data
      dates<-c(paste("d", 1:(n_dates_bands), sep=""))
      names(image_clip)=dates
      # Extract training data values from stack
      #number of columns will be number of dates + a column for "class"
      dfAll = setNames(data.frame(matrix(ncol = n_dates_bands+1, nrow = 0)), c(c(dates),"class"))
      for (i in 1:length(unique(trainData[[responseCol]]))){
        category <- unique(trainData[[responseCol]])[i]
        categorymap <- trainData[trainData[[responseCol]] == category,]
        dataSet <- raster::extract(image_clip, categorymap)
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
      message("available number of training points:")
      print(table(dfAll2$class))
      write.csv(dfAll2, "/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_20200723/training_values/values_extraction.csv")
     
       #write.csv(dfAll2, file = file.path(outputs,paste0("landcover_",todays_date),"training_values","values_extraction.csv"))
    }
  return(dfAll2)
  message("done with extraction")
}
    
adjust_samples<-function(ext_values, samples_per_class, classes_col,crop_perc, noncrop_perc, built_water_perc, bare_perc){
  message("adjusting samples")
  #Reclassification and adjust total numbers based on percentage amount
  table(ext_values$class)
  head(ext_values)
  nrow(ext_values)
  #takes a balanced dataset, reclassifies and adjusts according to percent parameter
  corn<-ext_values[ext_values$class==1,]; soy<-ext_values[ext_values$class==2,]; grass<-ext_values[ext_values$class==3,]; woody<-ext_values[ext_values$class==4,]; built<-ext_values[ext_values$class==5,]
  water<-ext_values[ext_values$class==6,]; bare<-ext_values[ext_values$class==7,]
  nrow(corn); nrow(soy); nrow(grass); nrow(woody); nrow(built); nrow(water); nrow(bare)
  soy$class<-1
  crop<-rbind(corn, soy)
  nrow(crop)
  grass$class<-2
  woody$class<-2
  bare$class<-4
  noncrop<-rbind(grass, woody) #, bare)
  nrow(noncrop)
  built$class<-3
  water$class<-3
  built_water<-rbind(built, water)
  nrow(built_water)
  all_vals_rcls<-rbind(crop, noncrop, built_water, bare)  #####COMBINE AGAIN
  head(all_vals_rcls)
  nrow(all_vals_rcls)
  #then balance the sample
  for (i in 1:length(unique(all_vals_rcls[, classes_col]))){
    class.i <- unique(all_vals_rcls[, classes_col])[i]
    if((sum(all_vals_rcls[, classes_col] == class.i) - samples_per_class) != 0){
      all_vals_rcls <- all_vals_rcls[-sample(which(all_vals_rcls[, classes_col] == class.i),
                                       sum(all_vals_rcls[, classes_col] == class.i) - samples_per_class), ]
    }
  }
  head(all_vals_rcls)
  nrow(all_vals_rcls)
  #balance samples
  message("balanced, now applying adjustment")
  #Divide again into respectice classes
  crop2<-all_vals_rcls[all_vals_rcls$class==1,]
  noncrop2<-all_vals_rcls[all_vals_rcls$class==2,]
  built_water2<-all_vals_rcls[all_vals_rcls$class==3,]
  bare2<-all_vals_rcls[all_vals_rcls$class==4,]
  ######ADJUST HERE
  crop3<-crop2[sample(nrow(crop2), (nrow(crop2)*crop_perc)),]
  noncrop3<-noncrop2[sample(nrow(noncrop2), (nrow(noncrop2)*noncrop_perc)),]
  built_water3<-built_water2[sample(nrow(built_water2), (nrow(built_water2)*built_water_perc)),]
  bare3<-bare2[sample(nrow(bare2), (nrow(bare2)*bare_perc)),]
  #rbind final time
  output<-rbind(crop3, noncrop3, built_water3, bare3)
  message("training points adjusted to:")
  print(table(output$class))
  return(output)
  message("done with adjustments")
}


#SVM CLASSIFICATION
svm_model<-function(farms, number, plot_width, svm_lc_in_mem, clip_ras, train_samples, img_files, PreProcess = NULL){
  if (identical(svm_lc_in_mem, FALSE)) {
    message("working on SVM")
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
    # cluster <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
    # registerDoParallel(cluster)
    #generate the model equation based on number of band/date variables
    f <- as.formula(
      paste("as.factor(class)", 
            paste("d", 1:(nb), sep = "",collapse = " + "), 
            sep = " ~ "))
    set.seed(123)
    ########################SVM is 5 k-fold cross validated###############################
    tc<-trainControl(method = "cv", number = 5, allowParallel = TRUE, savePredictions = T)
    mod.svm <- train(f, method = "svmLinear", data = training, trControl = tc, PreProcess = NULL)
    # stopCluster(cluster)
    registerDoSEQ()
    #now testing the accuracy of the model using the testing data
    mod_pred <- predict(mod.svm, testing)
    conf_mat<-confusionMatrix(mod_pred, as.factor(testing$class))
    
    #clip raster for prediction
    if (identical(clip_ras, TRUE)){
      farms_select<-farms[number,]
      buff200<-gBuffer(farms_select, width = plot_width, capStyle = 'SQUARE')
      ras2_predict<-mask(crop(img_files, extent(buff200)),buff200)
    } else {ras2_predict<-img_files}
    
    #apply prediction values to raster
    print("Applying predictions to raster stack")
    svm<-raster::predict(ras2_predict, model = mod.svm)
    #save SVM model in case useful later on
    saveRDS(mod.svm, file.path(outputs,paste0("landcover_",todays_date),"svm", "ndvi_3_classes.rds"))
    writeRaster(svm, file.path(outputs,paste0("landcover_",todays_date),"tifs", "svm.tif"),overwrite=T)
    writeRaster(svm, file.path(tempdir(),"svm.tif"), overwrite = T)
    if (file.exists(file.path(tempdir(),"svm.tif"))){message("tif also saved to temp directory")}
    return(conf_mat)
    message("done with SVM")
  }
  else {
    print(paste0("Are we using LC in memory? Answer: ", svm_lc_in_mem))
    message("svm already done. moving on")
  }
}
  
  plot_lc<-function(number, plot_width, farms, img_files){
    message("plotting output")
    #use LC in temp dir
    lc_out<-raster(file.path(tempdir(),"svm.tif"))
    farms_select<-farms[number,]
    buff200<-gBuffer(farms_select, width = plot_width, capStyle = 'SQUARE')
    image<-mask(crop(img_files, extent(buff200)), buff200)
    lc<-mask(crop(lc_out, extent(buff200)),buff200)
    #get the right color combo
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
    } else if (all(unique(lc) %in% c(1,2,3,4))) {
    mycol=c("yellow","olivedrab3","gray")
  }
    par(mfrow=c(1,2))
    print(raster::plot(lc, col=mycol, legend=F, ext=raster::extent(lc),axes=F,box=F))
    print(plotRGB(image,
                  r = 1, g = 2, b = 3,
                  stretch = "lin",
                  axes = FALSE, margins=TRUE, main = paste0("img")))
    par(mfrow=c(1,1))

}
