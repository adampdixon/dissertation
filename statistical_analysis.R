# Data synthesis 9 April 2023
# For manuscript: Passive monitoring of working lands
# Ecological Applications
#
# Adam Dixon, Matthew Baker, Erle Ellis
# 
library(plyr) # table processing
library(dplyr) # table processing
library(rsq) #adj R sq
library(wiqid) #AICc
library(ggplot2) # vis
library(gridExtra) # vis
library(RVAideMemoire) #for spearman CIs in spatial extent analysis
library(sf) # for spatial autocorrelation test
library(spdep) # for spatial autocorrelation test
library(cocor) # for comparing correlations at two spatial extents
library(stargazer) # for final regression summary table

# todays date for use in output naming
todays_date<-gsub("-","",Sys.Date())

directory<-"C:\\Users\\dixona\\Dropbox\\A_School\\2020_GrassyMargins\\3_AcousticAgLandscapes\\MS_4Pub\\submission 1 - Ecological Applications\\Final_accepted_article"

# Load zonodo data

data_all<-as_tibble(read.csv(file.path(directory, "dixon_eco_applications.csv")))

#Check Moran's I for vocalizing bird richness value to assess if spatial autocorrelation is a problem
#
# Note this analysis included here for review but is not publicly available because the geographic coordinates have been removed. Contact Adam Dixon (adam.dixon@wwfus.org for coordinates.)
#
# points = st_as_sf(filter(data_all, buffer == 300), coords = c("UTM_Longitude", "UTM_Latitude"), crs = 32615)
# coords <- coordinates(as(points, "Spatial"))
# col.knn <- knearneigh(points, k=4)
# 
# nb<-knn2nb(col.knn)
# 
# plot(nb, coords)
# title(main="K nearest neighbours, k = 4")
# 
# plot(st_geometry(points))
# hist(points$VBRICH, main=NULL)
# shapiro.test(points$VBR) # normally distributed? yes
# boxplot(points$VBR, horizontal = TRUE)
# 
# lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# 
# lw$weights[1]
# 
# I <- moran(points$VBR, lw, length(nb), Szero(lw))[1]
# I
# moran.test(points$VBR,lw, alternative="greater")
# 
# MC<- moran.mc(points$VBR, lw, nsim=10000, alternative="greater")
# MC
# plot(MC)

# It appears that there is no evidence for spatial autocorrelation

# Publicly available analysis starts here

# Look at the response correlation curve to determine impact of spatial extent
rcc<-function(x) {
  d = filter(data_all, Extent_meters == x)
  correlation <- cor.test(d$Noncrop_perc, d$VBRICH, method = c("pearson"), conf.level = .95)
  r.rc <- data.frame(rho=correlation$estimate,lower=ifelse(correlation$conf.int[[1]]<0,0,correlation$conf.int[[1]]),upper=correlation$conf.int[[2]],p=correlation$p.value)
  return(r.rc)
}

rcc.nc <- lapply(seq(from=100, to=1000, by = 100), rcc)
rcc.table<-data.frame()
for (i in 1:10) {
  rcc.table<-rbind(rcc.table,data.frame(rcc.nc[i]))
}
rcc.table

col1<-c("All_data"="blue")
col2<-c("Same_date"="red")
ggplot(rcc.table, aes(x=seq(100,1000,100))) +
  geom_point(aes(y=rho), size = 2, stroke = 0.5, shape = 1) +
  geom_point(aes(y=rho), size = 2, stroke = 0.5, shape = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, linetype="dashed", color = "gray") +
  scale_colour_manual(name="Error Bars",values=col1) +
  scale_x_continuous("Spatial extent (m)", seq(from=0, to=1000, by = 100)) +
  scale_y_continuous("Corr. coeff.", breaks = seq(0,1,.1)) +
  theme_classic()+
  theme(legend.position = "none", plot.margin=unit(c(1,1,1,1),"cm"), axis.text.x = element_text(angle = 90))



##############################################################################################################
###################################################### Assess significant difference between correlations

#planetTo100.rccs
paste("The highest corr coeff for noncrop is, ", round(max(rcc.table$rho), digits = 4))
paste("The lowest corr coeff for noncrop is, ", round(min(rcc.table$rho), digits = 4))
paste("The sample size is, ", nrow(filter(data_all, Extent_meters == 100)))
# 

# Check out scatterplot of both spatial extents
both_extents<-select(filter(data_all, Extent_meters == 100 | Extent_meters == 300), VBRICH, Noncrop_perc, Extent_meters)%>%
  dplyr::mutate(spatial_extent = as.factor(Extent_meters)) # Extent_meters needs to be nonnumeric
nrow(both_extents)
head(both_extents)


# look at the two groups with linear model
ggplot(both_extents, aes(x = Noncrop_perc, y = VBRICH, 
                     colour = spatial_extent)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_colour_viridis_d(option = "E")


for_cocor<-data.frame(VBRICH = filter(both_extents, Extent_meters == 300)$VBRICH,
                      Noncrop100 = filter(both_extents, Extent_meters == 100)$Noncrop_perc,
                      Noncrop300=filter(both_extents, Extent_meters == 300)$Noncrop_perc)

# Using test for overlapping populations
#https://psyteachr.github.io/msc-conv/comparing-two-correlations.html
cocor(formula = ~VBRICH + Noncrop100 | VBRICH + Noncrop300, data = for_cocor)

#This test suggests the correlations are significantly different and that the 300 m spatial extent should be used


##############################################################################################################
##############################################################################################################


#What is the noncrop proportion is across sites?
bar<-ggplot(data = filter(data_all, Extent_meters == 300), aes(x = reorder(location, -Noncrop_perc), y = Noncrop_perc)) + geom_bar(stat="identity",fill = "grey50") + xlab("site location") + ylab("Noncrop (%) within 300m") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, angle = 90))
bar


data = select(filter(data_all, Extent_meters == 300), Noncrop_perc)

#Histogram of noncrop vegetation

barlines <- "#1F3552"

p7 <- ggplot(data, aes(x = Noncrop_perc)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = "grey50") +
  scale_x_continuous(name = "Noncrop (%) within 300 m") +
  scale_y_continuous(name = "Count") +
  theme_classic()

p7

library(ggpubr)
figure <- ggarrange(bar, p7,
                    ncol = 2, nrow = 1)
figure


##############################################################################################################
##############################################################################################################
##################### POISSON REGRESSION


# Get data from spatial extent with highest correlation
data_spatial_extent <- filter(data_all, Extent_meters == 300)

##############################################################################################################
##############################################################################################################
##################### Correlation matrix for all variables

#https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples

### Make a simple correlation matrix
set.seed(1164)

library(tidyverse)
library(Hmisc)
#This is easy to convert to excel
#https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/
#' correlation_matrix
correlation_matrix <- function(df, 
                               type = "pearson",
                               digits = 3, 
                               decimal.mark = ".",
                               use = "all", 
                               show_significance = TRUE, 
                               replace_diagonal = FALSE, 
                               replacement = ""){
  
  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  
  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]
  
  # transform input data frame to matrix
  x <- as.matrix(df)
  
  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)
  
  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }
  
  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  
  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }
  
  return(Rnew)
}

#' save_correlation_matrix
#' Creates and save to file a fully formatted correlation matrix, using `correlation_matrix` and `Hmisc::rcorr` in the backend
#' @param df dataframe; passed to `correlation_matrix`
#' @param filename either a character string naming a file or a connection open for writing. "" indicates output to the console; passed to `write.csv`
#' @param ... any other arguments passed to `correlation_matrix`
#'
#' @return NULL
#'
#' @examples
#' `save_correlation_matrix(df = iris, filename = 'iris-correlation-matrix.csv')`
#' `save_correlation_matrix(df = mtcars, filename = 'mtcars-correlation-matrix.csv', digits = 3, use = 'lower')`
save_correlation_matrix = function(df, filename, ...) {
  write.csv2(correlation_matrix(df, ...), file = filename)
}


# save to excel file for manuscript fig
ds_cor <-data_spatial_extent %>%
  select(VBRICH, Noncrop_perc, Texture_Dis, Texture_Var) %>%
  save_correlation_matrix(paste0(file.path(directory, "corrmatrix"), todays_date, ".csv"), digits = 2, use = 'lower')


# or just plot out here
ds_cor <- data_spatial_extent %>%
  select(VBRICH, Noncrop_perc, Texture_Dis, Texture_Var) %>%
  as.matrix() %>%
  rcorr(type = "pearson")

# This shows two tables, first for r, second for P value
ds_cor

################################################
################PAIR PLOTS
################################################
library(GGally)
my_custom_cor <- function(data, mapping, color = I("grey70"), sizeRange = c(3, 3), ...) {
  #get the x and y data to use the other code
  x <- GGally::eval_data_col(data, mapping$x)
  y <- GGally::eval_data_col(data, mapping$y)
  
  ct <- cor.test(x,y, method = c("pearson"))
  sig <- symnum(
    ct$p.value, corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  r <- unname(ct$estimate)
  rt <- format(r, digits=2)[1]
  
  # since we can't print it to get the strsize, just use the max size range
  cex <- max(sizeRange)
  
  # helper function to calculate a useable size
  percent_of_range <- function(percent, range) {
    percent * diff(range) + min(range, na.rm = TRUE)
  }
  
  # plot the cor value
  ggally_text(
    label = as.character(rt), 
    mapping = aes(),
    xP = 0.5, yP = 0.5, 
    size = 5,#I(percent_of_range(cex * abs(r), sizeRange)),
    color = color,
    ...
  ) + 
    # add the sig stars
    geom_text(
      aes_string(
        x = 0.8,
        y = 0.8
      ),
      label = sig, 
      size = I(cex),
      color = color,
      ...
    ) + 
    # remove all the background stuff and wrap it with a dashed line
    theme_classic() + 
    theme(
      panel.background = element_rect(
        color = color, 
        linetype = "solid"))
}

my_custom_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = I("blue"), size = 0.7) + 
    geom_smooth(method = "lm", color = I("black"), size = 0.4, ...) +
    theme_bw() #+

  
}

my_custom_diag <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density() +
    theme_bw()

  
}


# #This is to see the format of the linear model plots without running the entire pair plot
my_custom_smooth(data_spatial_extent, aes(Noncrop_perc, Texture_Var))
my_custom_diag(data_spatial_extent, aes(Noncrop_perc))


data_spatial_extent%>%
  mutate(interaction = Noncrop_perc*Texture_Var) %>%
  select(VBRICH, Noncrop_perc, Texture_Var, interaction) %>%
  ggpairs(
    upper = list(continuous = my_custom_smooth), 
    lower = list(continuous = my_custom_cor),
    diag = list(continous = my_custom_diag),
    progress = T)
################################################################
################################################################
################################################################


# In Poisson regression, instead of reporting R2, we should report explained deviance, following Zuur et al. 2009
# This is the function for that
exdev<-function(x) {
  ndev<-x$null.deviance
  rdev<-x$deviance
  return(100*(((ndev-rdev)/ndev)))
}


#######################################################
###################################################### Poisson Regression

# Models with all species richness (VBRICH)

mod1<-glm(VBRICH~Noncrop_perc, data = data_spatial_extent, family = "poisson") 
mod2<-glm(VBRICH~Texture_Var, data = data_spatial_extent, family = "poisson")
mod3<-glm(VBRICH~Noncrop_perc + Texture_Var, data = data_spatial_extent, family = "poisson") 
mod4<-glm(VBRICH~Noncrop_perc*Texture_Var, data = data_spatial_extent, family = "poisson")

# Models with just grassland bird species richness 

mod1g<-glm(VBRICH_Grass~Noncrop_perc, data = data_spatial_extent, family = "poisson") 
mod2g<-glm(VBRICH_Grass~Texture_Var, data = data_spatial_extent, family = "poisson")
mod3g<-glm(VBRICH_Grass~Noncrop_perc + Texture_Var, data = data_spatial_extent, family = "poisson") 
mod4g<-glm(VBRICH_Grass~Noncrop_perc*Texture_Var, data = data_spatial_extent, family = "poisson")

# Models with just Non-grassland bird species richness 

mod1ng<-glm(VBRICH_Nongrass ~Noncrop_perc, data = data_spatial_extent, family = "poisson") 
mod2ng<-glm(VBRICH_Nongrass ~Texture_Var, data = data_spatial_extent, family = "poisson")
mod3ng<-glm(VBRICH_Nongrass~Noncrop_perc + Texture_Var, data = data_spatial_extent, family = "poisson") 
mod4ng<-glm(VBRICH_Nongrass~Noncrop_perc*Texture_Var, data = data_spatial_extent, family = "poisson")

# # prepare for table

l<-as.list(c('mod1', 'mod2', 'mod3', 'mod4',
             'mod1g', 'mod2g', 'mod3g', 'mod4g',
             'mod1ng', 'mod2ng', 'mod3ng', 'mod4ng'))

# model_order_names

models<-list(mod1, mod2, mod3, mod4, 
             mod1g, mod2g, mod3g, mod4g,
             mod1ng, mod2ng, mod3ng, mod4ng)
# model_column_names<-c('Mod8','Mod5','Mod1','Mod4','Mod7','Mod9','Mod12', 'Mod3','Mod11', 'Mod10', 'Mod6','Mod2')
model_column_names<-c('Model 1','Model 2','Model 3','Model 4',
                      'Model 1A','Model 2A','Model 3A','Model 4A',
                      'Model 1B','Model 2B','Model 3B','Model 4B')

#to get AIC into stargazer
AICc_vector<-c("AICc")
exdev_vector<-c("Explained deviance")
for (i in 1:length(l)){
  AICc_vector<-c(AICc_vector, round(wiqid::AICc(eval(parse(text = l[[i]]))),1))
  exdev_vector<-c(exdev_vector, round(exdev(eval(parse(text = l[[i]]))),1))
}
#place vector in list for stargazer
AICc_list<-list(AICc_vector)
exdev_list<-list(exdev_vector)



stargazer(models,
          add.lines=c(AICc_list,exdev_list),
          column.labels = model_column_names,# model_column_names,
          covariate.labels = c("Noncrop percent", "Noncrop Texture", 
                               "Noncrop percent x Noncrop Texture"),
          dep.var.caption  = "Habitat within 300 m of acoustic recorder",
          dep.var.labels   = c("VBRICH", "VBRICH Grassland", "VBRICH Non-grassland"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          align=FALSE,
          no.space = TRUE,
          report = ('vcps'),
          ci=T,
          model.numbers=FALSE,
          keep.stat=c("n"),
          type="text",
          omit = c("Constant")
          # out = paste0(file.path(figs, "regression_outs"), "//at300m_stargazer_", todays_date,".html"))
)


# For final plot, make VBRICH across all types (All, Grassland, Nongrassland) a single column

vbrich<-mutate(data_spatial_extent, type="VBRICH")%>%select(location, Noncrop_perc, VBRICH, type)
vbrich_nongrass<-mutate(select(data_spatial_extent, -VBRICH), VBRICH = VBRICH_Nongrass,  type="VBRICH Non-grassland")%>%select(location, Noncrop_perc, VBRICH, type)
vbrich_grass<-mutate(select(data_spatial_extent, -VBRICH), VBRICH = VBRICH_Grass, type="VBRICH Grassland")%>%select(location, Noncrop_perc, VBRICH, type)

vbrich_all<-rbind(vbrich, vbrich_nongrass, vbrich_grass)
vbrich_all


ggplot(vbrich_all, aes(x= Noncrop_perc, y=VBRICH, color=type, linetype = type)) +
  geom_point(aes(shape = type), size = 2.5) +
  scale_shape_manual(values = c(1,2,0)) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), size=.5) +
  scale_color_manual(values=c("#000000", "#004D40", "#1E88E5")) +
  xlab("Noncrop %") +
  ylab("VBRICH") +
  theme_classic() +
  theme(legend.title=element_blank())

# for plotting high res
# ggsave(path = file.path(final_figs), filename = 'scatterplot.tif', width = 6, height = 4, device='tiff', dpi=700)
