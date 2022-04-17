#data synthesis 16 April 2022
# Adam Dixon
# 
library(plyr)
library(dplyr)
library(rsq) #adj R sq
library(wiqid) #AICc
library(ggplot2) # vis
library(gridExtra) # vis
# library(caret)
library(RVAideMemoire) #for spearman CIs in RCC
library(MuMIn) # Model averaging
# library(ggrepel)
library(sf) # for spatial autocorrelation test
library(spdep)
library(cocor) # for comparing correlations at two spatial extents

# todays date for use in output naming
todays_date<-gsub("-","",Sys.Date())

directory<-"C:\\Users\\dixona\\Dropbox\\A_School\\2020_GrassyMargins\\3_AcousticAgLandscapes\\data"

data_all<-as_tibble(read.csv(file.path(directory, "dixon_agriculture_acoutics_2019.csv"))) %>%
  mutate(VBR = VBRICH,
         buffer = Extent_meters,
         site = location,
         GLCM_Dissimilarity = Texture_Dis,
         GLCM_Variance = Texture_Var)


#Check Moran's I for vocalizing bird richness value to determine spatial autocorrelation is a problem
#
#Note this data is not publicly available
#
points = st_as_sf(filter(data_all, buffer == 300), coords = c("UTM_Longitude", "UTM_Latitude"), crs = 32615)
coords <- coordinates(as(points, "Spatial"))
col.knn <- knearneigh(points, k=4)

nb<-knn2nb(col.knn)

plot(nb, coords)
title(main="K nearest neighbours, k = 4")

plot(st_geometry(points))
hist(points$VBRICH, main=NULL)
shapiro.test(points$VBR) # normally distributed? yes
boxplot(points$VBR, horizontal = TRUE)

lw <- nb2listw(nb, style="W", zero.policy=TRUE)

lw$weights[1]

I <- moran(points$VBR, lw, length(nb), Szero(lw))[1]
I
moran.test(points$VBR,lw, alternative="greater")

MC<- moran.mc(points$VBR, lw, nsim=10000, alternative="greater")
MC
plot(MC)

# it appears that there is no evidence for spatial autocorrelation

#Look at the response correlation curve
rcc<-function(x) {
  d = filter(data_all, buffer == x)
  correlation <- cor.test(d$Noncrop_perc, d$VBR, method = c("pearson"), conf.level = .95)
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
  #geom_line(aes(y=rho, color="All_data"), linetype = "solid", size=.75) +
  geom_point(aes(y=rho), size = 2, stroke = 0.5, shape = 1) +
  geom_point(aes(y=rho), size = 2, stroke = 0.5, shape = 3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.1, linetype="dashed", color = "gray") +
  scale_colour_manual(name="Error Bars",values=col1) +
  #ggtitle("Songbird richness and noncrop correlation at each circle buffer") +
  scale_x_continuous("Spatial extent (m)", seq(from=0, to=1000, by = 100)) +
  scale_y_continuous("Corr. coeff.", breaks = seq(0,1,.1)) +
  #ggtitle("Correlation of noncrop and species richness across anlaysis extents") +
  theme_classic()+
  theme(legend.position = "none", plot.margin=unit(c(1,1,1,1),"cm"), axis.text.x = element_text(angle = 90))



##############################################################################################################
###################################################### Assess significant difference between correlations

#planetTo100.rccs
paste("The highest corr coeff for noncrop is, ", round(max(rcc.table$rho), digits = 4))
paste("The lowest corr coeff for noncrop is, ", round(min(rcc.table$rho), digits = 4))
paste("The sample size is, ", nrow(filter(data_all, buffer == 100)))
# 

# Check out scatterplot of both spatial extents
both_extents<-select(filter(data_all, buffer == 100 | buffer == 300), VBR, Noncrop_perc, buffer)%>%
  dplyr::mutate(spatial_extent = as.factor(buffer)) # buffer needs to be nonnumeric
nrow(both_extents)
head(both_extents)


# look at the two groups with linear model
ggplot(both_extents, aes(x = Noncrop_perc, y = VBR, 
                     colour = spatial_extent)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_colour_viridis_d(option = "E")


for_cocor<-data.frame(VBR = filter(both_extents, buffer == 300)$VBR,
                      Noncrop100 = filter(both_extents, buffer == 100)$Noncrop_perc,
                      Noncrop300=filter(both_extents, buffer == 300)$Noncrop_perc)

# Using test for overlapping populations
#https://psyteachr.github.io/msc-conv/comparing-two-correlations.html
cocor(formula = ~VBR + Noncrop100 | VBR + Noncrop300, data = for_cocor)

#This test suggests the correlations are significantly different and that the 300 m spatial extent should be used


##############################################################################################################
##############################################################################################################


#What is the noncrop proportion is across sites
bar<-ggplot(data = filter(data_all, buffer == 300), aes(x = reorder(site, -Noncrop_perc), y = Noncrop_perc)) + geom_bar(stat="identity",fill = "grey50") + xlab("site location") + ylab("Noncrop (%) within 300m") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, angle = 90))
bar


data = select(filter(data_all, buffer == 300), Noncrop_perc)

barlines <- "#1F3552"

p7 <- ggplot(data, aes(x = Noncrop_perc)) +
  geom_histogram(aes(y = ..count..), binwidth = 10,
                 colour = barlines, fill = "grey50") +
  scale_x_continuous(name = "Noncrop (%) within 300 m") +
  scale_y_continuous(name = "Count") +
  theme_classic()
  # scale_x_continuous(breaks = seq(0, 80, 20),
  #                    limits=c(0, 80)) +
  # scale_y_continuous(name = "Count")
p7

library(ggpubr)
figure <- ggarrange(bar, p7,
                    ncol = 2, nrow = 1)
figure


##############################################################################################################
##############################################################################################################
##################### POISSON REGRESSION


# Scale texture variables from 0 to 1
scale1to0 <- function(x){(x-min(x))/(max(x)-min(x))}

# just get data from spatial extent
data_spatial_extent <- filter(data_all, buffer == 300)%>%
  dplyr::mutate(Texture_Dis = scale1to0(GLCM_Dissimilarity), Texture_Var = scale1to0(GLCM_Variance))%>%
  dplyr::mutate(Noncrop_tex_dis = Texture_Dis*Noncrop_perc,
         Noncrop_tex_var = Texture_Var*Noncrop_perc)%>%
  select(site,
         buffer,
         VBR,
         Noncrop_perc,
         Noncrop_m2,
         Texture_Dis,
         Texture_Var,
         Noncrop_tex_dis,
         Noncrop_tex_var,
         Pesticide,
         Fertilizer)

##############################################################################################################
##############################################################################################################
##################### Correlation matrix for all variables

#https://sites.google.com/site/rforfishandwildlifegrads/home/mumin_usage_examples

### Making just a simple correlation matrix
set.seed(1164)

library(tidyverse)
library(Hmisc)
#This one below is easier to convert to excel
#https://www.r-bloggers.com/2020/07/create-a-publication-ready-correlation-matrix-with-significance-levels-in-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
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
  select(VBR, Noncrop_perc, Pesticide, Fertilizer, Texture_Dis, Texture_Var) %>%
  save_correlation_matrix(paste0(file.path(figs, "corr_matrix", "corrmatrix"), todays_date, ".csv"), digits = 2, use = 'lower')


# or just plot out here
ds_cor <- data_spatial_extent %>%
  select(VBR, Noncrop_perc, Pesticide, Fertilizer, Texture_Dis, Texture_Var) %>%
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


# #This is to see the format of the linear model plots
my_custom_smooth(data_spatial_extent, aes(Noncrop_perc, Texture_Var))
my_custom_diag(data_spatial_extent, aes(Noncrop_perc))


data_spatial_extent%>%
  mutate(interaction = Noncrop_perc*Texture_Var) %>%
  select(VBR, Noncrop_perc, Texture_Var, interaction) %>%
  ggpairs(
    upper = list(continuous = my_custom_smooth), 
    lower = list(continuous = my_custom_cor),
    diag = list(continous = my_custom_diag),
    progress = T)
################################################################
################################################################
################################################################


#Instead of reporting R2, we should report explained deviance, following Zuur et al. 2009
# This is the function for that
exdev<-function(x) {
  ndev<-x$null.deviance
  rdev<-x$deviance
  return(100*(((ndev-rdev)/ndev)))
}

#First, fit candidate linear models to explain variation in density
mod1<-glm(VBR~Noncrop_perc, data = data_spatial_extent, family = "poisson")
mod2<-glm(VBR~Texture_Var, data = data_spatial_extent, family = "poisson")
mod3<-glm(VBR~Pesticide, data = data_spatial_extent, family = "poisson")

mod4<-glm(VBR~Noncrop_perc + Texture_Var, data = data_spatial_extent, family = "poisson") 
mod5<-glm(VBR~Noncrop_perc + Pesticide, data = data_spatial_extent, family = "poisson") 
mod6<-glm(VBR~Texture_Var + Pesticide, data = data_spatial_extent, family = "poisson") 
mod7<-glm(VBR~Noncrop_perc + Texture_Var + Pesticide, data = data_spatial_extent, family = "poisson") 

mod8<-glm(VBR~Noncrop_perc:Texture_Var, data = data_spatial_extent, family = "poisson") 
mod9<-glm(VBR~Noncrop_perc*Pesticide, data = data_spatial_extent, family = "poisson") 
mod10<-glm(VBR~Noncrop_perc*Texture_Var + Pesticide, data = data_spatial_extent, family = "poisson") 
mod11<-glm(VBR~Noncrop_perc*Pesticide + Texture_Var, data = data_spatial_extent, family = "poisson")
mod12<-glm(VBR~Noncrop_perc*Texture_Var + Noncrop_perc + Texture_Var, data = data_spatial_extent, family = "poisson")

summary(mod1)
exdev(mod1)
summary(mod2)
exdev(mod2)
summary(mod3)
exdev(mod3)
summary(mod4)
exdev(mod4)
summary(mod5)
exdev(mod5)
summary(mod6)
exdev(mod6)
summary(mod7)
exdev(mod7)
summary(mod8)
exdev(mod8)
summary(mod9)
exdev(mod9)
summary(mod10)
exdev(mod10)
AICc(mod10)
summary(mod11)
exdev(mod11)
AICc(mod11)
summary(mod12)
exdev(mod12)
AICc(mod12)


#MODEL AVERAGING

a<-AICc(mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8,mod9,mod10,mod11,mod12)
model_order<-a[order(a$AICc, decreasing = F),]
model_order
model_order_names<-rownames(model_order)

# MODEL AVERAGED
models2<-list(mod8,mod5,mod1,mod4)
avg_mod<-model.avg(models2)
summary(avg_mod)

# Look at effect sizes
library(effectsize)
std<-standardize_parameters(avg_mod, method = "refit", two_sd = TRUE, exponentiate = T)
std

#https://rpubs.com/rowlandw17/456945 # article on model averaging

coef1<-coefficients(avg_mod, full = F)
c<-data.frame(coef1)
#Get confidence 95% confidence intervals of avg_mod
ci1 <- confint(avg_mod, full=F)
ci<-data.frame(ci1)
results<-as_tibble(cbind(row.names(c),c, ci))
names(results)<-c("term","estimate","conf.low","conf.high")

results1<-filter(results, term == "Noncrop_perc:Texture_Var" | term == "Noncrop_perc")
library(dotwhisker)
dwplot(results) +
  # xlim(0, .02) +
  theme_classic()

results2<-filter(results, term != "Noncrop_perc:Texture_Var" | term != "Noncrop_perc")
library(dotwhisker)
dwplot(results2) +
  # xlim(0, .02) +
  theme_classic()

# PLOT OF ONLY NONCROP AND INTERACTION

noncrop<-ggplot(data = data_spatial_extent) +
  geom_point(aes(Noncrop_perc,VBR), size = .8) +
  geom_smooth(aes(Noncrop_perc,VBR), method = "glm", method.args = list(family = "poisson"),
              color = "black", size=0.5, fill = "lightgray") +
  xlim(0, 100) +
  ylim(5, 30) +
  labs(y="VBRICH", x = "NC%") +
  annotate("text", x=32, y=28, label= paste0("Exp. dev. = ", round(exdev(mod1), 1), "%")) +
  annotate("text", x=27, y=25, label= paste0("AICc = ", round(AICc(mod1), 1))) +
  theme_classic()


noncroptexture_interaction<-ggplot(data = data_spatial_extent) +
  geom_point(aes(Noncrop_perc*Texture_Var, VBR), size = .8) +
  geom_smooth(aes(Noncrop_perc*Texture_Var,VBR), method = "glm", method.args = list(family = "poisson"),
              color = "black", size=0.5, fill = "lightgray") +
  xlim(0, 100) +
  ylim(5, 30) +
  labs(y="VBRICH", x = "NC % x TVAR") +
  annotate("text", x=32, y=28, label= paste0("Exp. dev. = ", round(exdev(mod8), 1), "%")) +
  annotate("text", x=27, y=25, label= paste0("AICc = ", round(AICc(mod8), 1))) +
  theme_classic()

grid.arrange(noncrop, noncroptexture_interaction, ncol = 2)





# PREDICTED VS OBSERVED

predicted_data<-predict(avg_mod, select(data_spatial_extent, Noncrop_perc, Pesticide, Texture_Var), full = T, type= 'response')

pred.df<-data.frame(Observed = data_spatial_extent$VBR, Predicted = predicted_data)

cor(pred.df$Observed, pred.df$Predicted)


ggplot(data = pred.df, aes(Observed,Predicted)) +
  geom_point(shape = 1) +
  stat_smooth(method = "lm", color = "black", size=0.5, fill = "gray78") +
  stat_regline_equation(aes(label =  ..adj.rr.label..)) +
  # annotate("text", x = 11, y = 21, label ="paste(italic(R), \" = .68, \", italic(p), \" = .0000\")", parse = TRUE) +
  # stat_cor(label.x = 7, label.y = 21, p.digits = 2) +
  xlim(7, 25) +
  ylim(7, 25) +
  labs(y="Predicted VBRICH", x = "Observed VBRICH") +
  scale_color_manual(values = c("#353436")) +
  theme_classic()

summary(lm(Predicted ~ Observed, data = pred.df))




# prepare for table
model_order_names

l<-as.list(c('mod8','mod1','mod5','mod4','mod7','mod9','mod12', 'mod3','mod11', 'mod10', 'mod6','mod2'))

#to get AICc into stargazer
AICc_vector<-c("AICc")
for (i in 1:length(l)){
  AICc_vector<-c(AICc_vector, round(wiqid::AICc(eval(parse(text = l[[i]]))),1))
}
#place vector in list for stargazer
AICc_list<-list(AICc_vector)

#to get explained deviance into stargazer
exdev_vector<-c("Explained deviance")
for (i in 1:length(l)){
  exdev_vector<-c(exdev_vector, round(exdev(eval(parse(text = l[[i]]))),1))
}
#place vector in list for stargazer
exdev_list<-list(exdev_vector)

model_order_names

models<-list(mod8,mod1,mod5,mod4,mod7,mod9,mod12, mod3,mod11,mod10, mod6,mod2)
# model_column_names<-c('Mod8','Mod5','Mod1','Mod4','Mod7','Mod9','Mod12', 'Mod3','Mod11', 'Mod10', 'Mod6','Mod2')
model_column_names<-c('Model8','Model1','Model5','Model4','Model7','Model9','Model12', 'Model3','Model11', 'Model10', 'Model6','Model2')

library(stargazer)
stargazer(models,
          add.lines=c(AICc_list,exdev_list),
          column.labels = model_column_names,
          covariate.labels = c("Noncrop Percent x Texture", "Noncrop percent","Pesticide", "Texture", "Noncrop percent x Pesticide"),
          dep.var.caption  = "Non-crop habitat within 300 m of acoustic recorder",
          dep.var.labels   = "Dependent variable: Vocalizing bird richness",
          star.cutoffs = c(0.05, 0.01, 0.001),
          align=FALSE,
          no.space = TRUE,
          type="html", out = paste0(file.path(figs, "regression_outs"), "//at300m_stargazer_", todays_date,".txt"))


for (i in model_order_names){
  model_number <- substring(i, nchar(i), nchar(i))
  cat(paste0("<td>Mod. ", model_number,"</td>"))
}
###############################################################################################################################
###############################################################################################################################
