library(targets)
source("/Users/kellybroen/Documents/GitHub/Aim1/R/functions.R")

options(tidyverse.quiet = TRUE)

tar_option_set(packages = c(
  "sp",
  "sf",
  "rstan",
  "readxl",
  "rgdal",
  "raster",
  "tm",
  "readxl",
  "lubridate",
  "here",
  "spdep", 
  "brms",
  "mgcv",
  "Matrix",
  "ape",
  "tidyverse",
  "rgeoda", "mdthemes", "ggplot2", "ggtext", "gridExtra", "ggpubr", "DiagrammeR"))


list(
  ##PfPR data
  tar_target(pf, list(pf00=raster("Data/2020_GBD2019_Global_PfPR_2000.tif"),
                      pf01=raster("Data/2020_GBD2019_Global_PfPR_2001.tif"),
                      pf02=raster("Data/2020_GBD2019_Global_PfPR_2002.tif"),
                      pf03=raster("Data/2020_GBD2019_Global_PfPR_2003.tif"),
                      pf04=raster("Data/2020_GBD2019_Global_PfPR_2004.tif"),
                      pf05=raster("Data/2020_GBD2019_Global_PfPR_2005.tif"),
                      pf06=raster("Data/2020_GBD2019_Global_PfPR_2006.tif"),
                      pf07=raster("Data/2020_GBD2019_Global_PfPR_2007.tif"),
                      pf08=raster("Data/2020_GBD2019_Global_PfPR_2008.tif"),
                      pf09=raster("Data/2020_GBD2019_Global_PfPR_2009.tif"),
                      pf10=raster("Data/2020_GBD2019_Global_PfPR_2010.tif"),
                      pf11=raster("Data/2020_GBD2019_Global_PfPR_2011.tif"),
                      pf12=raster("Data/2020_GBD2019_Global_PfPR_2012.tif"),
                      pf13=raster("Data/2020_GBD2019_Global_PfPR_2013.tif"),
                      pf14=raster("Data/2020_GBD2019_Global_PfPR_2014.tif"),
                      pf15=raster("Data/2020_GBD2019_Global_PfPR_2015.tif"),
                      pf16=raster("Data/2020_GBD2019_Global_PfPR_2016.tif"),
                     pf17=raster("Data/2020_GBD2019_Global_PfPR_2017.tif"))),
  
  tar_target(cases, load_cases()),
  tar_target(Kenya, load_Kenya(pf, cases)),
  tar_target(Tanzania, load_tanzania(pf, cases)),
  tar_target(Uganda, load_Uganda(pf, cases)),
  tar_target(all, load_all_countries(Kenya, Tanzania, Uganda)),
  tar_target(data, make_lag_data(all)),
  tar_target(background, make_background(pf)),
  
 
  tar_target(model12, model_12(data)),#Cumulative malaria variable
  
  tar_target(figure1, make_figure1(data)),
  tar_target(figure2, make_figure2(data, background)),
  tar_target(figure3, make_figure3(data, model12)),
  tar_target(figureA2, make_figureA2(data, background)),
  tar_target(figureA3, make_figureA3(data)),
  tar_target(figureA4, make_figureA4(data, all)),
  tar_target(figureA5, make_figureA5()),
  tar_target(figureA6, make_figureA6(data, background)),
  tar_target(figureA7, make_figureA7(data, background)),
  tar_target(figureA8, make_figureA8(data)),
  
  tar_target(model0, model_0(data)), #Current year's malaria
  tar_target(model1, model_1(data)), #Lag1
  tar_target(model2, model_2(data)), #Lag2
  tar_target(model3, model_3(data)), #Lag3
  tar_target(model4, model_4(data)), #Lag4
  tar_target(model5, model_5(data)), #Lag5
  tar_target(model6, model_6(data)),#Lag 6
  tar_target(model7, model_7(data)),#Lag 7
  tar_target(model8, model_8(data)),#Lag 8
  tar_target(model9, model_9(data)),#Lag 9
  tar_target(model10, model_10(data)),#Lag 10
  tar_target(model11, model_11(data)),#Lags 0-10 included
  tar_target(figureA9, make_figureA9(model0,model1,model2, model3, model4, model5, model6, model7, model8, model9, model10)),
  tar_target(figureA10, make_figureA10(model11))#Lags 0-10 included
  
 
# tar_target(yearly, yearly_bimoran(data)),
#  tar_target(total, total_bimoran(data)),
#  tar_target(moran, single_moran(data))
)
