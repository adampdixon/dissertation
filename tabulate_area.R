

##############################
##############################20 Aug 2020
##############################TABULATE DATA
##############################This function was originally written for parallel, but I found that the system RAM needed to process out to 2K was more than most
##############################computers could handle. I currently have it set to run the raster::extract function in parallel only and use lapply to loop 
##############################through a list.
library(gdalUtils)
library(sf)
library(raster)
library(plyr)
library(ggplot2)
library(gridExtra)
library(grid)

windows<-"E:\\Dropbox\\A_School\\2020_GrassyMargins\\2019_data\\planet_processing_someoutputs\\all_sites\\landcover_mac_individ_20200802_good\\tifs"
mac<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_mac_individ_20200802_good/tifs"

mac_vrt<-"/Users/adamdixon/Dropbox/A_School/2020_GrassyMargins/2019_data/planet_processing_someoutputs/all_sites/landcover_mac_individ_20200802_good/vrt"
windows_vrt<-"E:\\Dropbox\\A_School\\2020_GrassyMargins\\2019_data\\planet_processing_someoutputs\\all_sites\\landcover_mac_individ_20200802_good\\vrt"

#Built LC vrt
LC_list<-list.files(file.path(windows), pattern = ".tif$", full.names = T)
gdalbuildvrt(LC_list, file.path(windows_vrt, "LCs_windows.vrt"))

#check vrt
lc_win<-raster(file.path(windows_vrt, "LCs_windows.vrt"))

tab_area<-function(number){
  library(raster)
  library(dplyr)
  library(tidyr)
  library(sf)
  library(readr)
  #buffs are an already made multiple ring buffer made in QGIS
  #raster is the lc vrt for all sites
  #number is the site number
  data<-"E:\\Dropbox\\A_School\\2020_GrassyMargins\\2019_data\\planet_processing_someoutputs\\all_sites\\landcover_mac_individ_20200802_good"
  dir<-"E:\\Dropbox\\A_School\\2020_GrassyMargins\\2019_data"
  ras<-raster(file.path(data, "vrt", "LCs_windows.vrt"))
  all_site_buffs<-st_read(file.path(dir, "sites_buffers", "mrb_100_to_2K_2019_sites_w_ID.shp"))
  site_buffs<-filter(all_site_buffs, ID == number)
  
  buf<-mutate(site_buffs, site2 = paste(site, mrb_dist, sep = "_"))
  #http://zevross.com/blog/2015/03/30/map-and-analyze-raster-data-in-r/
  # pull out land uses by region into a list
  # this takes a minute or so on my machine
  beginCluster()
  extall<-raster::extract(ras, site_buffs, method='simple')
  endCluster()
  # Extract the values of the landcover raster for each zone. 
  # This produces a list of raster cells for each region
  # You can do the same calculation using the full state raster
  # ext<-extract(raster, region.sm, method='simple')
  class(extall)  # a list
  length(extall) # three elements, a vector of land use values for each region
  # Function to tabulate land use by region and return 
  # a data.frame
  tabFunc<-function(indx, extracted, region, regname) {
    dat<-as.data.frame(table(extracted[[indx]]))
    dat$name<-region[[regname]][[indx]]
    return(dat)
  }
  
  
  # run through each region and compute a table of the count
  # of raster cells by land use. Produces a list (see below)
  tabs<-lapply(seq(extall), tabFunc, extall, buf, "site2")
  
  # assemble into one data frame
  tabs<-do.call("rbind",tabs )
  
  # name the land uses
  tabs$Var1<-factor(tabs$Var1, levels=c(1,2,3), labels=c("crop", "noncrop", "built_water"))
  
  # use the spread function from tidyr to make nicer
  
  results<-tabs%>%
    group_by(name) %>% # group by region
    mutate(totcells=sum(Freq), # how many cells overall
           percent.area=round(100*Freq/totcells,4)) %>% #cells by landuse/total cells
    dplyr::select(-c(Freq, totcells)) %>% # there is a select func in raster so need to specify
    spread(key=Var1, value=percent.area, fill=0)#%>% # make wide format
  r2<-cbind(data.frame(results), data.frame(extent = site_buffs$mrb_dist))
  write_csv(r2, file.path(data, "tab_area",paste0("lc_area_",number,".csv")))
  #return(results)
}


number<-50:51

lapply(number, tab_area)