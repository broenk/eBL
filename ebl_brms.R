require(here)
require(brms)
require(sf)
require(readxl)
require(spdep)
require(readr)
require(tidyr)
require(rstan)

##Load data

U_wide <- st_read(here("CaseData","U_wide.shp"), quiet = TRUE)
T_wide <- st_read(here("CaseData","T_wide.shp"), quiet = TRUE)
K_wide <- st_read(here("CaseData","K_wide.shp"), quiet = TRUE)

##Add country variable
U_wide$country <- "Uganda"
T_wide$country <- "Tanzania"
K_wide$country <- "Kenya"


###Get same columns with same names
K_wide <- K_wide %>% dplyr::select("pop10"    ,
                                   "pop11"     ,"pop12"   ,  "pop13"   ,  "pop14" ,    "pop15"  ,   "pop16"  ,   "case10"    ,"case11"    ,"case12"   , "case13"  ,  "case14"   , "case15"   , "case16"   ,
                                   "rate10"    ,"rate11"   , "rate12" ,   "rate13" ,   "rate14"  ,  "rate15"  ,  "rate16"   , "malar00" ,  "malar01"  , "malar02" ,  "malar03"  , "malar04" ,  "malar05"  ,
                                   "malar06"  , "malar07" ,  "malar08"   ,"malar09"  , "malar10" ,  "malar11" ,  "malar12", "malar13", "malar14", "malar15", "malar16", "country"  )


U_wide <- U_wide %>% dplyr::select("X2010"    ,
                                   "X2011"     ,"X2012"   ,  "X2013"   ,  "X2014" ,    "X2015"  ,   "X2016"  ,   "case10"    ,"case11"    ,"case12"   , "case13"  ,  "case14"   , "case15"   , "case16"   ,
                                   "rate10"    ,"rate11"   , "rate12" ,   "rate13" ,   "rate14"  ,  "rate15"  ,  "rate16"   , "malaria00" ,  "malaria01"  , "malaria02" ,  "malaria03"  , "malaria04" ,  "malaria05"  ,
                                   "malaria06"  , "malaria07" ,  "malaria08"   ,"malaria09"  , "malaria10" ,  "malaria11" ,  "malaria12", "malaria13", "malaria14", "malaria15", "malaria16", "country"  )

colnames(U_wide) <- c("pop10"    ,
                      "pop11"     ,"pop12"   ,  "pop13"   ,  "pop14" ,    "pop15"  ,   "pop16"  ,   "case10"    ,"case11"    ,"case12"   , "case13"  ,  "case14"   , "case15"   , "case16"   ,
                      "rate10"    ,"rate11"   , "rate12" ,   "rate13" ,   "rate14"  ,  "rate15"  ,  "rate16"   , "malar00" ,  "malar01"  , "malar02" ,  "malar03"  , "malar04" ,  "malar05"  ,
                      "malar06"  , "malar07" ,  "malar08"   ,"malar09"  , "malar10" ,  "malar11" ,  "malar12", "malar13", "malar14", "malar15", "malar16", "country", "geometry")


T_wide <-  T_wide %>% dplyr::select("pop10"    ,
                                    "pop11"     ,"pop12"   ,  "pop13"   ,  "pop14" ,    "pop15"  ,   "pop16"  ,   "case10"    ,"case11"    ,"case12"   , "case13"  ,  "case14"   , "case15"   , "case16"   ,
                                    "rate10"    ,"rate11"   , "rate12" ,   "rate13" ,   "rate14"  ,  "rate15"  ,  "rate16"   , "malar00" ,  "malar01"  , "malar02" ,  "malar03"  , "malar04" ,  "malar05"  ,
                                    "malar06"  , "malar07" ,  "malar08"   ,"malar09"  , "malar10" ,  "malar11" ,  "malar12", "malar13", "malar14", "malar15", "malar16", "country"   )

#Set crs
st_crs(U_wide) <- st_crs(K_wide)
U_wide <- st_transform(U_wide, st_crs(K_wide))

##Bind all the data
all_wide <- rbind(U_wide, T_wide, K_wide)

##Get lagged variables and put into a data frame
malaria_nolag <-c(all_wide$malar10, all_wide$malar11, all_wide$malar12, all_wide$malar13, all_wide$malar14, all_wide$malar15, all_wide$malar16)
malaria_lag1 <- c(all_wide$malar09, all_wide$malar10, all_wide$malar11, all_wide$malar12, all_wide$malar13, all_wide$malar14, all_wide$malar15)
malaria_lag2 <- c(all_wide$malar08, all_wide$malar09, all_wide$malar10, all_wide$malar11, all_wide$malar12, all_wide$malar13, all_wide$malar14)
malaria_lag3 <- c(all_wide$malar07, all_wide$malar08, all_wide$malar09, all_wide$malar10, all_wide$malar11, all_wide$malar12, all_wide$malar13)
malaria_lag4 <- c(all_wide$malar06, all_wide$malar07, all_wide$malar08, all_wide$malar09, all_wide$malar10, all_wide$malar11, all_wide$malar12)
malaria_lag5 <- c(all_wide$malar05, all_wide$malar06, all_wide$malar07, all_wide$malar08, all_wide$malar09, all_wide$malar10, all_wide$malar11)
malaria_lag6 <- c(all_wide$malar04, all_wide$malar05, all_wide$malar06, all_wide$malar07, all_wide$malar08, all_wide$malar09, all_wide$malar10)
malaria_lag7 <- c(all_wide$malar03, all_wide$malar04, all_wide$malar05, all_wide$malar06, all_wide$malar07, all_wide$malar08, all_wide$malar09)
malaria_lag8 <- c(all_wide$malar02, all_wide$malar03, all_wide$malar04, all_wide$malar05, all_wide$malar06, all_wide$malar07, all_wide$malar08)

ebl <- c(all_wide$case10, all_wide$case11, all_wide$case12, all_wide$case13, all_wide$case14, all_wide$case15, all_wide$case16)
pop <- c(all_wide$pop10, all_wide$pop11, all_wide$pop12, all_wide$pop13, all_wide$pop14, all_wide$pop15, all_wide$pop16)
year <- c(rep(2010, length(all_wide$pop10)),rep(2011, length(all_wide$pop10)),rep(2012, length(all_wide$pop10)), rep(2013, length(all_wide$pop10)),rep(2014, length(all_wide$pop10)),rep(2015, length(all_wide$pop10)),rep(2016, length(all_wide$pop10)))
country <- rep(all_wide$country, 7)
geometry <- rep(all_wide$geometry,7)
id <- rep(1:length(all_wide$pop10),7)

lag_df <- st_as_sf(data.frame("malaria_nolag" = malaria_nolag,"malaria_lag1"= malaria_lag1,"malaria_lag2" = malaria_lag2, "malaria_lag3" = malaria_lag3,"malaria_lag4" = malaria_lag4,"malaria_lag5" = malaria_lag5,"malaria_lag6" = malaria_lag6,"malaria_lag7" = malaria_lag7,"malaria_lag8" = malaria_lag8,   "cases" = ebl, "pop" = pop, "year" = year,"country" =country, "geometry" = geometry, "id" = id))

lag_df <- lag_df %>%
  dplyr::filter(pop != 0)


##BRMS model 
model1 <- brm(cases ~ 0 + Intercept + malaria_lag1 +malaria_lag2 + malaria_lag3+ malaria_lag4 + malaria_lag5 +  malaria_lag1*malaria_lag2+ malaria_lag2*malaria_lag3 + malaria_lag4*malaria_lag3 + malaria_lag4*malaria_lag5+ country+ (1|id) + offset(log(pop)),  
                    data = lag_df, family = zero_inflated_poisson(), 
                    warmup = 500, iter = 1000, 
                    cores = 2, chains = 2, 
                    sample_prior = TRUE,
                    seed = 123)    




