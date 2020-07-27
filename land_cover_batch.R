

###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
site_number<-c(1:49)
clusterApply(cl, site_number, batch_all_lcs)
stopCluster(cl)
toc()
######################################

# for (i in c(16, 18, 20, 25, 32, 40, 42, 43, 49)){batch_all_lcs(site_number = i)}

#RUN THE BELOW FIRST
#####################
batch_all_lcs<-function(site_number){
  library(raster)
  library(rgeos)
  library(rgdal)
  library(dplyr)
  library(caret)
  library(doParallel)
  outputs<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites"
  todays_date<-gsub("-","",Sys.Date())
  number<-site_number
  
  ##################################################
  #COMPONENT FUNCTIONS, PRIMARY FUNCTION AT END
  ##################################################
  
  lc_class<-function(outputs, number){ #, ras, farms, number, training, width, classes_col, plot_width, samples_per_class, crop_perc, noncrop_perc, built_water_perc, svm_not_complete, ...){
    ###################################################################################################################################
    #inputs
    naip<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/NAIP_4analysis/vrt/naip_reset_origin.vrt"
    tpi<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/3mDEM/1_tifs/vrts/tpi_resampled_to_naip_326215.vrt"
    evi_date1<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_1.vrt"
    evi_date2<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_2.vrt"
    evi_date3<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_3.vrt"
    evi_date4<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_4.vrt"
    evi_date5<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_5.vrt"
    evi_date6<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_6.vrt"
    evi_date7<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resample_all_sites_indices_time_7.vrt"
    evi_stdev<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resampled_stdev.vrt"
    evi_median<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resampled_median.vrt"
    evi_change<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_4analysis/6_vrts/evi_resampled_change.vrt"
    
    ras_list<-c(naip, tpi, evi_date1, evi_date2, evi_date3, evi_date4, evi_date5, evi_date6, evi_date7, evi_stdev, evi_median, evi_change)
    
    ras<-stack(ras_list) #make raster object
    #make chacter vector of names to be used when extracting values from training points to data frame
    ras_stack_names<-c("naip_nir","naip_red","naip_green", "tpi", "evi_date1", "evi_date2", "evi_date3", "evi_date4", "evi_date5", "evi_date6", "evi_date7", "evi_stdev", "evi_median", "evi_change")
    #set names
    names(ras)<-ras_stack_names
    
    farms<-readOGR(file.path("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data","sites_shp", "sites_w_data_2019.geojson"))
    training<-shapefile("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/training_data_round2/final_set2/all_boxes_grass_woody_extra.shp")
    training<-shapefile("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/training_data_round2/final_set2/all_boxes_grass_woody_extra_artisinal_adding_27July2020.shp")
    width<-2050
    plot_width<-200   #for some reason not passed. Set below too.
    #samples_per_class <- 200 # this will now be set using a minimum number 
    #number<-number
    classes_col<-"class"
    svm_not_complete <- TRUE  #use the LC that is already in memory? FALSE if first time running; I don't know why but this won't get passed on. So make sure and set in the model too.
    crop_perc <- 1
    noncrop_perc <- 1
    built_water_perc <- 1
    bare_perc <- 1
    clip_ras <- TRUE #clip raster to predict to "plot_width amount?
    PreProcess <- NULL #options are "pca" or "nzw"
    use_temp<- FALSE #save some outputs to temp dir?
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
    e<-extract_training(image_clip = img_clip, train = train, ras_stack_names)
    length(ras_stack_names)
    table(e$class)
    head(e)
    #adjust values
    #get select number from class with the minimum number of samples
    as<-adjust_samples(ext_values = e, classes_col = classes_col, crop_perc = crop_perc, noncrop_perc = noncrop_perc, built_water_perc = built_water_perc, bare_perc = bare_perc) #, samples_per_class = samples_per_class
    table(as$class)
    head(as)
    #run model, save results
    s<-svm_model(farms, number, ras_stack_names, plot_width, svm_not_complete, clip_ras, train_samples = as, img_files = img_clip, PreProcess)
    #plot outcome
    #plot_lc(number, plot_width, farms, img_files =  img_clip)
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
    #plot(buff)
    #plot(training_clip, add=T)
    message("done with training clip")
    return(training_clip)
  }
  
  #clip raster
  image_clip<-function(ras, farms, number, width){
    message("working on image clip")
    # if (file.exists(file.path(tempdir(), "img_clip.tif"))) {
    #   m <- stack(file.path(tempdir(), "img_clip.tif"))
    #   message("using image in temp file")
    # } else {  #raster input - single or list of rasters
      ras1<-stack(ras)
      #vector input
      farms_select<-farms[number,]
      #buffer
      buff<-gBuffer(farms_select, width = width, capStyle = 'SQUARE')
      #mask
      m<-stack(mask(crop(ras1, extent(buff)), buff))
      #plot(buff)
      #plot(m[[1]], add=T)
      
      # if (identical(use_temp, TRUE)){
      #   temp_file <- writeRaster(m, file.path(tempdir(), "img_clip.tif"))
      message("done with image clip")
      return(m)
      }
# 
#     }
  #   #plotRGB(m,
  #           # r = 1, g = 2, b = 3,
  #           # stretch = "lin",
  #           # axes = FALSE, margins=TRUE, main = paste0("img"))
  #   return(m)
  # }
  
  # extracting training values from input raster
  extract_training<-function(image_clip, train, ras_stack_names){
    message("extract training data")
    #check if values have already been extracted
    # if (identical(file.exists(file.path(tempdir(),"values_extraction.csv")), TRUE)){
    #   message("extraction already completed. moving on...")
    #   dfAll2<-read.csv(file.path(outputs,paste0("landcover_",todays_date),"training_values","values_extraction.csv"))
    # }else{
      message("extracting training values")
      trainData<-train
      responseCol<-"LC6"
      #get the number of bands for use later on, and save for use in dfAll function
      n_ras_stack_names<-length(ras_stack_names)
      # and rename ndvi for convenience and for further coding
      #adding the change data
      #dates<-c(paste("d", 1:(n_dates_bands), sep=""), ras_stack_names)
      names(image_clip)=ras_stack_names
      # Extract training data values from stack
      #number of columns will be number of dates + a column for "class"
      dfAll = setNames(data.frame(matrix(ncol = n_ras_stack_names+1, nrow = 0)), c(ras_stack_names,"class"))
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
      
      #dfAll2<-filter(dfAll, d1 != -3.400000e+38, d2 != -3.400000e+38, d3 != -3.400000e+38)
      
      #check for missing values
      bad_values<-which(is.na.data.frame(dfAll))
      dfAll2<-dfAll[!row.names(dfAll)%in%bad_values,]
      #check for other way missing values have been showing 
      which(apply(dfAll2, 1, function(r) any(r %in% c(-3.400000e+38))))#doesn't seem to be a problme
      #also look for just NA values
      dfAll3<-na.omit(dfAll2)
      message("available number of training points:")
      print(table(dfAll3$class))
      return(dfAll3)
      # if (identical(use_temp, TRUE)){write.csv(dfAll2, file.path(tempdir(),"values_extraction.csv"))}
    }

  
  adjust_samples<-function(ext_values, classes_col,crop_perc, noncrop_perc, built_water_perc, bare_perc){ #samples_per_class, 
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
    #some classes don't have any representation at certain sites, so use if statement to avoid error message
    if (length(woody$class)>0){woody$class<-2}
    if (length(bare$class)>0){bare$class<-2}
    noncrop<-rbind(grass, woody, bare)
    nrow(noncrop)
    built$class<-3
    if (length(water$class)>0){water$class<-3}
    built_water<-rbind(built, water)
    nrow(built_water)
    all_vals_rcls<-rbind(crop, noncrop, built_water) #, bare)  #####COMBINE AGAIN
    head(all_vals_rcls)
    nrow(all_vals_rcls)
    t<-table(all_vals_rcls$class)
    minimum<-min(t[[1]],t[[2]],t[[3]])
    if (minimum < 200) {
      samples_per_class<-minimum
    } else if (minimum > 200) {
      samples_per_class<-200
    }
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
    #bare2<-all_vals_rcls[all_vals_rcls$class==4,]
    ######ADJUST HERE
    crop3<-crop2[sample(nrow(crop2), (nrow(crop2)*crop_perc)),]
    noncrop3<-noncrop2[sample(nrow(noncrop2), (nrow(noncrop2)*noncrop_perc)),]
    built_water3<-built_water2[sample(nrow(built_water2), (nrow(built_water2)*built_water_perc)),]
    #bare3<-bare2[sample(nrow(bare2), (nrow(bare2)*bare_perc)),]
    #rbind final time
    output<-rbind(crop3, noncrop3, built_water3) #, bare3)
    message("training points adjusted to:")
    print(table(output$class))
    return(output)
    message("done with adjustments")
  }
  
  
  #SVM CLASSIFICATION
  svm_model<-function(farms, number, plot_width, svm_not_complete = TRUE, clip_ras, train_samples, img_files, PreProcess = NULL, ras_stack_names){
    # if (!identical(svm_not_complete, TRUE)) {
      message("working on SVM")
      balanced<-na.omit(train_samples) #make sure not NAs
      #nb<-length(img_files@layers) #just grabbing the band number length from img
      #dates<-c(paste("d", 1:nb, sep=""), ras_stack_names, "class")
      # ras_layers<-c(ras_stack_names,"class")
      # names(img_files)<-ras_layers
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
      # f <- as.formula(
      #   paste("as.factor(class)", 
      #         paste("d", 1:(nb), sep = "",collapse = " + "), 
      #         sep = " ~ "))
      ras_stack_names<-c("naip_nir","naip_red","naip_green", "tpi", "evi_date1", "evi_date2", "evi_date3", "evi_date4", "evi_date5", "evi_date6", "evi_date7", "evi_stdev", "evi_median", "evi_change")
      training$class<-as.factor(training$class)
      class(training$class)
      f <- formula(paste("class", paste(ras_stack_names,collapse = "+"), sep = "~"))
      set.seed(123)
      ########################SVM is 5 k-fold cross validated###############################
      tc<-trainControl(method = "cv", number = 5, allowParallel = TRUE, savePredictions = T)
      mod.svm <- train(f, method = "svmLinear", data = training, trControl = tc, PreProcess = NULL)
      
      #now testing the accuracy of the model using the testing data
      mod_pred <- predict(mod.svm, testing)
      conf_mat<-confusionMatrix(mod_pred, as.factor(testing$class))
      plot_width<-200
      #clip raster for prediction
      if (identical(clip_ras, TRUE)){
        farms_select<-farms[number,]
        buff200<-gBuffer(farms_select, width = plot_width, capStyle = 'SQUARE')
        ras2_predict<-mask(crop(img_files, extent(buff200)),buff200)
      } else {ras2_predict<-img_files}
      
      names(ras2_predict)<-ras_stack_names
      
      #apply prediction values to raster
      print("Applying predictions to raster stack")
      svm<-raster::predict(ras2_predict, model = mod.svm)
      #save SVM model in case useful later on
      saveRDS(mod.svm, file.path(outputs,paste0("landcover_",todays_date),"svm", paste0("ndvi_3_classes_site_",number,".rds")))
      writeRaster(svm, file.path(outputs,paste0("landcover_",todays_date),"tifs", paste0("site_",number,"_lc_svm.tif")),overwrite=T)
      #control when saving to temp dir
      # if (identical(use_temp, TRUE)){writeRaster(svm, file.path(tempdir(),"svm.tif"), overwrite = T)}
      # if (file.exists(file.path(tempdir(),"svm.tif"))){message("tif also saved to temp directory")}
      return(conf_mat)
      message("done with SVM")
    }
  #   else {
  #     print(paste0("Has LC been put in memory already? Answer: ", svm_not_complete))
  #     message("svm already done. moving on")
  #   }
  # }
  

  ##################################################
  #RUN PRIMARY FUNCTION
  ##################################################
  lc_class(outputs, number)
  ##################################################
  
}




#CHECK RESULTS AND PLOT
# g<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_20200726/tifs", pattern = ".tif$")
# 
# for (i in 1:49){
#   gr<-grep(pattern = paste0("site_",i,"_lc_svm.tif"), g, , value = T)
#   print(gr)
# }

# 
# library(raster)
# library(rgdal)
# library(rgeos)
# 
# 
# farms<-readOGR(file.path("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data","sites_shp", "sites_w_data_2019.geojson"))
# dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_20200727/tifs"
# g<-list.files("/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_20200727/tifs", pattern = ".tif$")
# #
# plot_lc<-function(i, g, farms){
#   message("plotting output")
#   gr<-grep(pattern = paste0("site_",i,"_lc_svm.tif"), g, value = T)
#   #use LC in temp dir
#   lc_out<-raster(file.path(dir,gr))
#   farms_select<-farms[i,]
#   buff200<-gBuffer(farms_select, width = 200) #, capStyle = 'SQUARE')
#   #image<-mask(crop(img_files, extent(buff200)), buff200)
#   lc<-mask(crop(lc_out, extent(buff200)),buff200)
#   #get the right color combo
#   if (all(unique(lc) %in% c(1))) {
#     mycol=c("yellow")
#   } else if (all(unique(lc) %in%  c(1,2))) {
#     mycol=c("yellow","olivedrab3")
#   } else if (all(unique(lc) %in%  c(1,3))) {
#     mycol=c("yellow","gray")
#   } else if (all(unique(lc) %in%  c(1,4))) {
#     mycol=c("yellow","blue")
#   } else if (all(unique(lc) %in%  c(2,3))) {
#     mycol=c("olivedrab3","gray")
#   }else if (all(unique(lc) %in% c(1,2,3))) {
#     mycol=c("yellow","olivedrab3","gray")
#   } else if (all(unique(lc) %in% c(1,2,3,4))) {
#     mycol=c("yellow","olivedrab3","gray")
#   }
#   print(raster::plot(lc, col=mycol, legend=F, ext=raster::extent(lc),axes=F,box=F))
#   title(farms_select[[1]], line = -2)
# }
# 
# 
# par(mfrow=c(6,4), mar=c(1,1,1,1))
# e<-extent(raster(file.path(dir,g[1])))
# 
# for (i in 1:24){
#   plot(e)
# }
# 
# 
# for (i in c(16, 18, 20, 25, 32, 40, 42, 43, 49)){
#   plot_lc(i=i, g=g, farms=farms)
# }
# 
# 
# #put outputs into directory
# output_dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_20200726/"
# plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE);
# plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# file.copy(from=plots.png.paths, to=output_dir)
# 
# plots.png.details <- file.info(plots.png.paths)
# plots.png.details <- plots.png.details[order(plots.png.details$mtime),]
# sorted.png.names <- gsub(plots.dir.path, output_dir, row.names(plots.png.details), fixed=TRUE)
# numbered.png.names <- paste0(output_dir, 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)
# #look within NDVI scenes for odd outliers and remove if necessary
