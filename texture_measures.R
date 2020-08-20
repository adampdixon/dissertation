

#CALCULATE TEXTURE MEASURES
calc_morans_and_texture<-function(buffer) {
  library(glcm)
  library(raster)
  library(rgdal)
  library(rgeos)
  library(glcm)
  #################Inputs
  #can be the vrt
  dir<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data"
  date1_evi<-stack(file.path(dir,"planet_4analysis", "6_vrts","all_sites_indices_time_1.vrt"))[[2]]
  LC<-stack(file.path(dir, "PUT PATH HERE"))
  farms<-readOGR(file.path(dir,"sites_shp", "sites_w_data_2019.geojson"))
  output_csv_path<-file.path(dir, "planet_processing_someoutputs/hetrogeneity", paste0("heterogeneity_measures_",buffer,"_radius.csv"))
  buffer<-buffer
  ################
  #depending on imagery you use, change the band combination in "new_crop"
  morans_n_texture<-function(site=farms, img = date1_evi, lc = LC, buffer= buffer){
    buff<-gBuffer(site, width = buffer)
    #buff<-st_sfc(buff)
    ex<-extent(buff)
    #calculate Local Moran's I
    new_crop<-crop(img, ex)
    #Queens's case
    f <- matrix(c(1,1,1,1,0,1,1,1,1), nrow=3)
    x2 <- MoranLocal(new_crop, w=f)
    #get glcm texture measures
    g32<-glcm::glcm(new_crop, n_grey = 32, window = c(3, 3), statistics =c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy"))
    g_mask<-lc[lc==1|lc==3]<-NA
    g1<-mask(g32, g_mask)
    
    g4<-glcm::glcm(new_crop, n_grey = 4, window = c(3, 3), statistics =c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy"))
    g2<-mask(g4, g_mask)
    #get texture mean from buffer
    texture_32GL_mn<-raster::extract(g1, site, buffer=buffer, fun=mean)
    texture_32GL_sd<-raster::extract(g1, site, buffer=buffer, fun=sd)
    texture_4GL_mn<-raster::extract(g2, site, buffer=buffer, fun=mean)
    texture_4GL_sd<-raster::extract(g2, site, buffer=buffer, fun=sd)
    #do zonal stats, get moran's I mean
    moran_mn<-raster::extract(x2,site, buffer=buffer, fun=mean)
    moran_sd<-raster::extract(x2,site, buffer=buffer, fun=sd)
    #to get sd you have to mask the moransI, then use cellstats. I couldn't get extract to do sd
    sd<-raster::cellStats(mask(x2,buff),stat = 'sd')
    columns<-cbind(moran_mn,moran_sd, texture_32GL_mn, texture_32GL_sd, texture_4GL_mn, texture_4GL_sd) #put all measures together
    return(columns)
  }
  
  #this runs the function
  morans_mean_sd_tex <- list()
  for (i in seq_along(farms[[1]])) morans_mean_sd_tex[[i]] <- morans_n_texture(site=farms[i,], img = date1_evi, buffer=buffer) 
  
  #drop geometry to get to the site names
  farms_df<-data.frame()
  #this calculates the cv and put it into a table
  cv_df<-data.frame()
  for (i in seq_along(farms[[1]])){
    #data<-mean_stdev[i]
    sitename<-as.character(farms$site[i])
    cv<-(morans_mean_sd_tex[[i]][2]/morans_mean_sd_tex[[i]][1])
    stats<-cbind(sitename, buffer, morans_mean_sd_tex[[i]][2],morans_mean_sd_tex[[i]][1],cv,morans_mean_sd_tex[[i]][3],
                 morans_mean_sd_tex[[i]][4],morans_mean_sd_tex[[i]][5],morans_mean_sd_tex[[i]][6],morans_mean_sd_tex[[i]][7],morans_mean_sd_tex[[i]][8])
    cv_df<-rbind(cv_df, stats)
  }
  names(cv_df)<-c("site","buffer","morans_sd", "morans_mn", "morans_cv","glcm_mean_32GL_mn", "glcm_variance_32GL_mn", "glcm_homogeneity_32GL_mn", "glcm_contrast_32GL_mn", "glcm_dissimilarity_32GL_mn", "glcm_entropy_32GL_mn","glcm_mean_32GL_sd", "glcm_variance_32GL_sd", "glcm_homogeneity_32GL_sd", "glcm_contrast_32GL_sd", "glcm_dissimilarity_32GL_sd", "glcm_entropy_32GL_sd","glcm_mean_4GL_mn", "glcm_variance_4GL_mn", "glcm_homogeneity_4GL_mn", "glcm_contrast_4GL_mn", "glcm_dissimilarity_4GL_mn", "glcm_entropy_4GL_mn","glcm_mean_4GL_sd", "glcm_variance_4GL_sd", "glcm_homogeneity_4GL_sd", "glcm_contrast_4GL_sd", "glcm_dissimilarity_4GL_sd", "glcm_entropy_4GL_sd")
  write.csv(cv_df, output_csv_path)
}



###########PARALLEL
library(parallel)
library(tictoc)
ncores<-detectCores(logical = T)
cl<-makeCluster(ncores-1)
tic()
buffer<-seq(100,2000,100)
clusterApply(cl, buffer, calc_morans_and_texture)
stopCluster(cl)
toc()
######################################'
  
  
  
  
  
  
  

