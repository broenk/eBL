
##Get case data
load_cases <- function(){
  #Load case data 
  cases <- read_xlsx("Data/cases.xlsx")%>%
    #make age numeric
    mutate(age = as.factor(round(as.numeric(Age))),
           #create a date variable by month
           date = mdy(paste(`Month enrolled`,`Day enrolled`,`Year enrolled`, "-")),
           #Label the countries
           country = if_else(Country == 1, "Uganda", if_else(Country == 2, "Tanzania", if_else(Country == 3, "Kenya", "."))),
           #Get month 
           month = month(date),
           #Get year
           `Year enrolled` = as.numeric(`Year enrolled`)
    )
  
  ##Label the sexes
  cases$Gender[cases$Gender == 1] <- "Male"
  cases$Gender[cases$Gender == 2] <- "Female"
  table(cases$Age)
  
  ##aggregate case count by age, sex, year, and district
  cases.2 <- cases %>%
    group_by(District, country, age, Gender, `Year enrolled`)%>%
    summarise("cases" = length(`Subject ID`))%>%
    ##drop any cases with missing data
    drop_na()
  
  ###Left with 668 cases in total
  return(cases.2)
}


load_Kenya <- function(pf, cases){
  
  ##Load population data
  Kenya_pop <- read_excel("data/population.projections.xlsx", 
                          sheet = "Kenya", skip = 3)%>%
    mutate(District = removePunctuation(str_replace_all(District, fixed(" "), ""))) %>%
    na.omit()%>%
    group_by(District)%>%
    summarise_if(is.numeric, sum, na.rm=TRUE)
  
  ##Convert population districts to post 2012 form
  Kenya_pop$District[Kenya_pop$District == "BONDO"]<- "SIAYA" 
  Kenya_pop$District[Kenya_pop$District == "BUNGOMAEAST"|Kenya_pop$District ==  "BUNGOMANORTH"|Kenya_pop$District == "BUNGOMASOUTH"|Kenya_pop$District == "BUNGOMAWEST"|Kenya_pop$District == "MTELGON"] <- "BUNGOMA"
  Kenya_pop$District[Kenya_pop$District == "BUTERE"|Kenya_pop$District == "KAKAMEGACENTRAL"|Kenya_pop$District ==  "KAKAMEGAEAST"|Kenya_pop$District ==  "KAKAMEGANORTH"|Kenya_pop$District ==  "KAKAMEGASOUTH" |Kenya_pop$District ==  "LUGARI"|Kenya_pop$District ==  "MUMIAS"]<-  "KAKAMEGA"
  Kenya_pop$District[Kenya_pop$District == "KISIICENTRAL"|Kenya_pop$District == "GUCHA"] <- "KISII"
  Kenya_pop$District[Kenya_pop$District == "KISUMUEAST"|Kenya_pop$District == "KISUMUWEST"|Kenya_pop$District == "NYANDO"] <- "KISUMU"
  Kenya_pop$District[Kenya_pop$District == "KURIAEAST"|Kenya_pop$District == "KURIAWEST"] <- "MIGORI"
  Kenya_pop$District[Kenya_pop$District == "RACHUONYO"|Kenya_pop$District == "SUBA"]<- "HOMABAY"
  Kenya_pop$District[Kenya_pop$District == "TESONORTH"|Kenya_pop$District == "TESOSOUTH"] <- "BUSIA"
  
  ##Aggregate again with new districts
  Kenya_pop <- Kenya_pop %>%
    group_by(District) %>%
    summarise_if(is.numeric, sum, na.rm=TRUE)
  
  ##restrict cases to Kenya
  K_cases <- cases %>%
    filter(country == "Kenya") %>%
    #get district into uniform char
    mutate(District = removePunctuation(str_replace_all(District, fixed(" "), "")))
  
  ##Convert districts into post 2012 standard
  K_cases$District[K_cases$District == "BUNGOMAEAST"|K_cases$District == "BUNGOMANORTH"|K_cases$District == "BUNGOMASOUTH"|K_cases$District == "BUNGOMAWEST"|K_cases$District == "MTELGON"] <- "BUNGOMA"
  K_cases$District[K_cases$District == "BUNYALA"] <- "BUSIA"
  K_cases$District[K_cases$District == "BUTERE"] <- "KAKAMEGA"
  K_cases$District[K_cases$District == "EMUHAYA"] <- "VIHIGA" 
  K_cases$District[K_cases$District == "GUCHA" | K_cases$District == "HAMISI" ] <- "KISII" 
  K_cases$District[K_cases$District == "KAKAMEGACENTRAL"|K_cases$District == "KAKAMEGAEAST"|K_cases$District == "KAKAMEGANORTH"|K_cases$District == "KAKAMEGASOUTH"|K_cases$District == "LUGARI"|K_cases$District == "MUMIAS"] <- "KAKAMEGA" 
  K_cases$District[K_cases$District == "KAPSABET"] <- "NANDI" 
  K_cases$District[K_cases$District == "KISUMUEAST"|K_cases$District == "KISUMUWEST" |K_cases$District == "NYANDO"] <- "KISUMU" 
  K_cases$District[K_cases$District == "KEIYO"] <- "ELGEYOMARAKWET" 
  K_cases$District[K_cases$District == "KERECHO"] <- "KERICHO" 
  K_cases$District[K_cases$District ==  "KISIICENTRAL" |K_cases$District ==  "KISIISOUTH"] <- "KISII" 
  K_cases$District[K_cases$District == "NANDINORTH" ] <- "NANDI" 
  K_cases$District[K_cases$District == "POKOTNORTH"   ] <- "WESTPOKOT" 
  K_cases$District[K_cases$District == "RACHUONYO"  |  K_cases$District == "SUBA" ] <- "HOMABAY" 
  K_cases$District[K_cases$District == "RONGO"   ] <- "MIGORI" 
  K_cases$District[K_cases$District == "SAMIA" | K_cases$District == "TESOSOUTH" |K_cases$District == "TESONORTH" ] <- "BUSIA" 
  K_cases$District[K_cases$District == "TRANSNZOIA"  ] <- "TRANSNZOIA" 
  K_cases$District[K_cases$District == "UASHINGISHU" | K_cases$District ==  "WARENG’"] <- "UASINGISHU" 
  K_cases$District[K_cases$District == "BONDO"] <- "SIAYA" 
  K_cases$District[K_cases$District == "KURIAWEST"] <- "MIGORI" 
  
  #Aggregate again with new districts
  K_cases <- K_cases %>%
    group_by(District, `Year enrolled`, age, Gender)%>%
    summarise(cases = sum(cases))
  
  ##Load shapefile data
  k <- st_read("data/ken_admbnda_adm1_iebc_20191031.shp", quiet =TRUE)%>%
    ##transform projection
    st_transform(st_crs(pf[[1]])) %>%
    mutate("District" = toupper(removePunctuation(str_replace_all(ADM1_EN, fixed(" "), ""))))%>%
    dplyr::select(District)
  
  ##restrict K to the districts in the case and population data
  k.2 <- k[k$District %in% K_cases$District | k$District %in% Kenya_pop$District,]
  
  k.2 <- left_join(k.2, Kenya_pop, by = "District") %>%
    drop_na() %>%
    st_as_sf() %>%
    dplyr::select(District)
  
  
  ##Get pfpr data
  malK<- list()
  mK <- list()
  k <- c()
  
  for (i in 1:18){
    malK[[i]] <- crop(pf[[i]], k.2)
    mK[[i]] <- raster::extract(pf[[i]], k.2, weights=TRUE,fun= mean, df=TRUE , na.rm =TRUE)
    k <- append(k, mK[[i]][[2]])
  }
  
  mk <- data.frame("malaria" = k, "year" =sort(rep(2000:2017, 10)), "District" = rep(k.2$District,18))
  
  ###Convert pfpr to pfeir
  a <- -0.523	
  b <- .119
  
  mk$pfeir <- NA
  
  for(i in 2000:2016){
    mk$pfeir[mk$year == i] <- ((-mk$malaria[mk$year == i +1] * exp(-a)) / (mk$malaria[mk$year == i +1] - 1) ^ 1/b) *12
  }
  
  mk <- mk %>%
    filter(year < 2017)
  
  ###Make an empty data frame for yers with 0 cases
  id <- 1:length(k.2$District)
  District <- k.2$District
  year <- 2000:2016
  age <- 0:15
  sex <- c("Male", "Female")
  
  t <- crossing(District, year, age, sex)
  
  ###Join empty data with malaria data
  K.3 <- left_join(t, mk, by = c("District" = "District", "year" = "year") )
  ###Add population data 
  K.4 <- left_join(K.3, Kenya_pop, by = c("District" = "District"))
  K_cases$age <- K_cases$age  %>% as.numeric()
  
  ###Join with cases 
  K.final <- left_join(K.4, K_cases, by = c("District" = "District", "year"="Year enrolled","age"="age","sex"="Gender"))
  
  ##Get population data for year and sex by district
  K.final$pop <- NA
  
  K.final$pop[K.final$year == 2012 & K.final$sex == "Female"] <- (c(K.final$`2012: Female 0-4`[K.final$year == 2012 & K.final$sex == "Female" & K.final$age %in% 0:4], K.final$`2012: Female 10-15`[K.final$year == 2012 & K.final$sex == "Female" & K.final$age %in% 10:15], K.final$`2012: Female 5-9`[K.final$year == 2012 & K.final$sex == "Female" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2013 & K.final$sex == "Female"] <- (c(K.final$`2013: Female 0-4`[K.final$year == 2013 & K.final$sex == "Female" & K.final$age %in% 0:4], K.final$`2013: Female 10-15`[K.final$year == 2013 & K.final$sex == "Female" & K.final$age %in% 10:15], K.final$`2013: Female 5-9`[K.final$year == 2013 & K.final$sex == "Female" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2014 & K.final$sex == "Female"] <- (c(K.final$`2014: Female 0-4`[K.final$year == 2014 & K.final$sex == "Female" & K.final$age %in% 0:4], K.final$`2014: Female 10-15`[K.final$year == 2014 & K.final$sex == "Female" & K.final$age %in% 10:15], K.final$`2014: Female 5-9`[K.final$year == 2014 & K.final$sex == "Female" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2015 & K.final$sex == "Female"] <- (c(K.final$`2015: Female 0-4`[K.final$year == 2015 & K.final$sex == "Female" & K.final$age %in% 0:4], K.final$`2015: Female 10-15`[K.final$year == 2015 & K.final$sex == "Female" & K.final$age %in% 10:15], K.final$`2015: Female 5-9`[K.final$year == 2015 & K.final$sex == "Female" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2016 & K.final$sex == "Female"] <- (c(K.final$`2016: Female 0-4`[K.final$year == 2016 & K.final$sex == "Female" & K.final$age %in% 0:4], K.final$`2016: Female 10-15`[K.final$year == 2016 & K.final$sex == "Female" & K.final$age %in% 10:15], K.final$`2016: Female 5-9`[K.final$year == 2016 & K.final$sex == "Female" & K.final$age %in% 5:9]))
  
  K.final$pop[K.final$year == 2012 & K.final$sex == "Male"] <- (c(K.final$`2012: Male 0-4`[K.final$year == 2012 & K.final$sex == "Male" & K.final$age %in% 0:4], K.final$`2012: Male 10-15`[K.final$year == 2012 & K.final$sex == "Male" & K.final$age %in% 10:15], K.final$`2012: Male 5-9`[K.final$year == 2012 & K.final$sex == "Male" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2013 & K.final$sex == "Male"] <- (c(K.final$`2013: Male 0-4`[K.final$year == 2013 & K.final$sex == "Male" & K.final$age %in% 0:4], K.final$`2013: Male  10-15`[K.final$year == 2013 & K.final$sex == "Male" & K.final$age %in% 10:15], K.final$`2013: Male 5-9`[K.final$year == 2013 & K.final$sex == "Male" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2014 & K.final$sex == "Male"] <- (c(K.final$`2014: Male 0-4`[K.final$year == 2014 & K.final$sex == "Male" & K.final$age %in% 0:4], K.final$`2014: Male 10-15`[K.final$year == 2014 & K.final$sex == "Male" & K.final$age %in% 10:15], K.final$`2014: Male  5-9`[K.final$year == 2014 & K.final$sex == "Male" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2015 & K.final$sex == "Male"] <- (c(K.final$`2015: Male 0-4`[K.final$year == 2015 & K.final$sex == "Male" & K.final$age %in% 0:4], K.final$`2015: Male 10-15`[K.final$year == 2015 & K.final$sex == "Male" & K.final$age %in% 10:15], K.final$`2015: Male 5-9`[K.final$year == 2015 & K.final$sex == "Male" & K.final$age %in% 5:9]))
  K.final$pop[K.final$year == 2016 & K.final$sex == "Male"] <- (c(K.final$`2016: Male 0-4`[K.final$year == 2016 & K.final$sex == "Male" & K.final$age %in% 0:4], K.final$`2016: Male 10-15`[K.final$year == 2016 & K.final$sex == "Male" & K.final$age %in% 10:15], K.final$`2016: Male 5-9`[K.final$year == 2016 & K.final$sex == "Male" & K.final$age %in% 5:9]))
  
  ##Make NA case values 0
  K.final$cases[is.na(K.final$cases)] <- 0
  ##Make NA population values 0 (years before the study)
  K.final$pop[is.na(K.final$pop)] <- 0
  
  ##Make pfpr into a % and select variables of interest
  K.final <- K.final %>%
    mutate(malaria = malaria *100) %>%
    dplyr::select(District, age, sex, year, cases, pop, malaria, pfeir)
  
  ##Order by year
  K.final <- K.final[order(K.final$year),]
  
  ##Assume that population is evenly distributed across age groups
  K.final$pop[K.final$age %in% 0:4] <- K.final$pop[K.final$age %in% 0:4]/5
  K.final$pop[K.final$age %in% 10:15] <- K.final$pop[K.final$age %in% 10:15]/6
  K.final$pop[K.final$age %in% 5:9] <- K.final$pop[K.final$age %in% 5:9]/5
  
  K.final$pop[K.final$year == 2012] <-   K.final$pop[K.final$year == 2012]/2
  K.final$pop[K.final$year == 2016] <-   K.final$pop[K.final$year == 2016] * (3/4)
  
  ##Make a country variable, convert to sf
  K.final.2 <- K.final %>%
    mutate(country = "Kenya")%>%
    left_join(k.2, by = "District") %>%
    st_as_sf()%>%
    dplyr::select("age","sex","year","cases","pop","malaria","pfeir","country", "District")
  return(K.final.2)
}


load_tanzania <- function(pf, cases){
  ##Load population data
  Tanzania_pop <- read_excel("data/population.projections.xlsx", 
                             sheet = "Tanzania", skip = 3)%>%
    mutate(District = str_replace_all(District, fixed(" "), "")) %>%
    na.omit()%>%
    group_by(District)%>%
    summarise_if(is.numeric, sum, na.rm=TRUE)
  
  ##Convert districts to post 2010 names
  Tanzania_pop$District[Tanzania_pop$District == "ILEMELAMUNICIPAL"] <- "ILEMELA"
  Tanzania_pop$District[Tanzania_pop$District == "NYAMAGANAMUNICIPAL"] <- "NYAMAGANA"
  Tanzania_pop$District[Tanzania_pop$District == "NYANG'HWALE"] <- "NYANGWALE"
  
  ##re-aggregate
  Tanzania_pop <- Tanzania_pop %>%
    group_by(District) %>%
    summarise_if(is.numeric, sum, na.rm=TRUE)
  
  ##Filter cases to just Tanzania
  T_cases <- cases %>%
    filter(country == "Tanzania") %>%
    mutate(District = removePunctuation(str_replace_all(District, fixed(" "), "")))
  
  ##Convert case dsitricts to post 2010 names
  T_cases$District[T_cases$District == "ILEMELA MUNICIPAL"] <- "ILEMELA" 
  T_cases$District[T_cases$District == "NYANG'HWALE" | T_cases$District == "NYANG'WALE"] <- "NYANGWALE" 
  T_cases$District[T_cases$District == "MUSOMA MUNIC"] <- "MUSOMA"
  T_cases$District[T_cases$District == "NYAMAGANA MUNICIPAL"] <- "NYAMAGANA"
  
  ##Re-aggregate
  T_cases <- T_cases %>%
    group_by(District, `Year enrolled`, age, Gender)%>%
    summarise(cases =sum(cases))
  
  ##Load shapefile
  Tan  = st_read("data/Tanzania_Study_Area_Wards.shp" , quiet=TRUE) %>%
    mutate("District" = removePunctuation(toupper(str_replace_all(District_N, fixed(" "), ""))))%>%
    dplyr::select("District")%>%
    group_by(District) %>%
    summarise()
  
  #Rename a district
  Tan$District[Tan$District == "MUSOMAMUNICIPAL"] <- "MUSOMA"
  
  
  ##Get malaria data
  malT<- list()
  mT <- list()
  t <- c()
  
  for (i in 1:length(pf)){
    malT[i] <-  raster::crop(pf[[i]], Tan)
    mT[[i]] <- raster::extract(malT[[i]], Tan, weights=TRUE,fun= mean, df=TRUE , na.rm =TRUE)
    t <- append(t,mT[[i]][[2]])
  }
  
  ##Make into data frame
  mt <- data.frame("malaria" = t, "year" =sort(rep(2000:2017, 24)), "District" = Tan$District)
  
  a <- -.523
  b <- .119
  mt$pfeir <- NA
  
  for(i in 2000:2016){
    mt$pfeir[mt$year == i] <- ((-mt$malaria[mt$year == i +1] * exp(-a)) / (mt$malaria[mt$year == i +1] - 1) ^ 1/b) *12
  }
  
  mt <- mt %>%
    filter(year < 2017)
  
  ##Build empty data frame
  id <- 1:length(mt$District)
  District <- mt$District
  year <- 2000:2016
  age <- 0:15
  sex <- c("Male", "Female")
  
  t <- crossing(District, year, age, sex)
  
  ##Merge with malaria data
  T.3 <- left_join(t, mt, by = c("District" = "District", "year" = "year") )
  
  ##Make age numeric
  T_cases$age <- T_cases$age  %>% as.numeric()
  
  T.31 <- left_join(T.3, Tanzania_pop, by = "District")
  ##merge long form data with cases
  T.final <- left_join(T.31, T_cases, by = c("District" = "District", "year"="Year enrolled","age"="age","sex"="Gender"))
  
  ##Make population data long form
  T.final$pop <- NA
  
  T.final$pop[T.final$year == 2012 & T.final$sex == "Female"] <- (c(T.final$`2012: Female 0-4`[T.final$year == 2012 & T.final$sex == "Female" & T.final$age %in% 0:4], T.final$`2012: Female 10-15`[T.final$year == 2012 & T.final$sex == "Female" & T.final$age %in% 10:15], T.final$`2012: Female 5-9`[T.final$year == 2012 & T.final$sex == "Female" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2013 & T.final$sex == "Female"] <- (c(T.final$`2013: Female 0-4`[T.final$year == 2013 & T.final$sex == "Female" & T.final$age %in% 0:4], T.final$`2013: Female 10-15`[T.final$year == 2013 & T.final$sex == "Female" & T.final$age %in% 10:15], T.final$`2013: Female 5-9`[T.final$year == 2013 & T.final$sex == "Female" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2014 & T.final$sex == "Female"] <- (c(T.final$`2014: Female 0-4`[T.final$year == 2014 & T.final$sex == "Female" & T.final$age %in% 0:4], T.final$`2014: Female 10-15`[T.final$year == 2014 & T.final$sex == "Female" & T.final$age %in% 10:15], T.final$`2014: Female 5-9`[T.final$year == 2014 & T.final$sex == "Female" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2015 & T.final$sex == "Female"] <- (c(T.final$`2015: Female 0-4`[T.final$year == 2015 & T.final$sex == "Female" & T.final$age %in% 0:4], T.final$`2015: Female 10-15`[T.final$year == 2015 & T.final$sex == "Female" & T.final$age %in% 10:15], T.final$`2015: Female 5-9`[T.final$year == 2015 & T.final$sex == "Female" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2016 & T.final$sex == "Female"] <- (c(T.final$`2016: Female 0-4`[T.final$year == 2016 & T.final$sex == "Female" & T.final$age %in% 0:4], T.final$`2016: Female 10-15`[T.final$year == 2016 & T.final$sex == "Female" & T.final$age %in% 10:15], T.final$`2016: Female 5-9`[T.final$year == 2016 & T.final$sex == "Female" & T.final$age %in% 5:9]))
  
  T.final$pop[T.final$year == 2012 & T.final$sex == "Male"] <- (c(T.final$`2012: Male 0-4`[T.final$year == 2012 & T.final$sex == "Male" & T.final$age %in% 0:4], T.final$`2012: Male 10-15`[T.final$year == 2012 & T.final$sex == "Male" & T.final$age %in% 10:15], T.final$`2012: Male 5-9`[T.final$year == 2012 & T.final$sex == "Male" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2013 & T.final$sex == "Male"] <- (c(T.final$`2013: Male 0-4`[T.final$year == 2013 & T.final$sex == "Male" & T.final$age %in% 0:4], T.final$`2013: Male  10-15`[T.final$year == 2013 & T.final$sex == "Male" & T.final$age %in% 10:15], T.final$`2013: Male 5-9`[T.final$year == 2013 & T.final$sex == "Male" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2014 & T.final$sex == "Male"] <- (c(T.final$`2014: Male 0-4`[T.final$year == 2014 & T.final$sex == "Male" & T.final$age %in% 0:4], T.final$`2014: Male 10-15`[T.final$year == 2014 & T.final$sex == "Male" & T.final$age %in% 10:15], T.final$`2014: Male  5-9`[T.final$year == 2014 & T.final$sex == "Male" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2015 & T.final$sex == "Male"] <- (c(T.final$`2015: Male 0-4`[T.final$year == 2015 & T.final$sex == "Male" & T.final$age %in% 0:4], T.final$`2015: Male 10-15`[T.final$year == 2015 & T.final$sex == "Male" & T.final$age %in% 10:15], T.final$`2015: Male 5-9`[T.final$year == 2015 & T.final$sex == "Male" & T.final$age %in% 5:9]))
  T.final$pop[T.final$year == 2016 & T.final$sex == "Male"] <- (c(T.final$`2016: Male 0-4`[T.final$year == 2016 & T.final$sex == "Male" & T.final$age %in% 0:4], T.final$`2016: Male 10-15`[T.final$year == 2016 & T.final$sex == "Male" & T.final$age %in% 10:15], T.final$`2016: Male 5-9`[T.final$year == 2016 & T.final$sex == "Male" & T.final$age %in% 5:9]))
  
  T.final$cases[is.na(T.final$cases)] <- 0
  T.final$pop[is.na(T.final$pop)] <- 0
  
  ##Make malaria into % and select variables
  T.final <- T.final %>%
    mutate(malaria = malaria *100) %>%
    dplyr::select(District, age, sex, year, cases, pop, malaria,pfeir)
  
  T.final <- T.final[order(T.final$year),]
  
  ##Assume pop is even within age groups
  T.final$pop[T.final$age %in% 0:4] <- T.final$pop[T.final$age %in% 0:4]/5
  T.final$pop[T.final$age %in% 10:15] <- T.final$pop[T.final$age %in% 10:15]/6
  T.final$pop[T.final$age %in% 5:9] <- T.final$pop[T.final$age %in% 5:9]/5
  
  T.final$pop[T.final$year == 2012] <-   T.final$pop[T.final$year == 2012]/2
  T.final$pop[T.final$year == 2016] <-   T.final$pop[T.final$year == 2016] * (3/4)
  
  ##Make a country variable, convert to sf
  T.final.2 <- T.final %>%
    mutate(country = "Tanzania")%>%
    left_join(Tan, by = "District") %>%
    st_as_sf()%>%
    dplyr::select("age","sex","year","cases","pop","malaria","pfeir","country", "District")
  
  return(T.final.2)
}

load_Uganda <- function(pf, cases){
  ##Load shapefile for Uganda
  Uganda  <- st_read("data/Uganda_districts2010.shp", quiet=TRUE) %>%
    mutate("District" = toupper(removePunctuation(str_replace_all(DNAME_2010,fixed(" "), ""))))%>%
    dplyr::select("District", "SUBREGION")%>%
    na.omit() %>%
    st_transform(st_crs(pf[[1]]))%>%
    filter(SUBREGION == "WEST NILE" | SUBREGION =="ACHOLI"| SUBREGION =="LANGO")
  
  ##Load population data and restrict to Uganda
  Uganda_pop <- read_excel("data/population.projections.xlsx", 
                           sheet = "Uganda", skip = 3)%>%
    mutate(District = str_replace_all(District, fixed(" "), "")) %>%
    na.omit()%>%
    group_by(District)%>%
    summarise_if(is.numeric, sum, na.rm=TRUE) %>%
    filter(District %in% Uganda$District)
  
  ##Load case data and restrict to Uganda
  U_cases <- cases %>%
    filter(country == "Uganda") %>%
    mutate(District = removePunctuation(str_replace_all(District, fixed(" "), "")))%>%
    group_by(District, `Year enrolled`, age, Gender)%>%
    summarise(cases = sum(cases))
  
  #Change districts to matchable names
  U_cases$District[U_cases$District == "BULISA"] <- "BULIISA"
  U_cases$District[U_cases$District == "KIYRANDONGO"] <- "KIRYANDONGO"
  U_cases$District[U_cases$District == "MARACHATEREGO"] <- "MARACHA"
  
  ##Aggregate with renamed districts
  U_cases <- U_cases %>%
    group_by(District,`Year enrolled`, age, Gender)%>%
    summarise(cases = sum(cases))
  
  U.2 <- left_join(Uganda, Uganda_pop, by = "District") %>%
    drop_na() %>%
    st_as_sf() %>%
    dplyr::select(District)
  
  ##Get malaria data
  malU<- list()
  mU <- list()
  u <- c()
  
  for (i in 1:length(pf)){
    malU[i] <-  raster::crop(pf[[i]], U.2)
    mU[[i]] <- raster::extract(malU[[i]], U.2, weights=TRUE,fun= mean, df=TRUE , na.rm =TRUE)
    u <- append(u,mU[[i]][[2]])
  }
  
  #Make malaria data frame
  mu <- data.frame("malaria" = u, "year" =sort(rep(2000:2017, 16)), "District" = U.2$District)
  
  a <- -.523
  b <- .119
  mu$pfeir <- NA
  
  for(i in 2000:2016){
    for(i in 2000:2016){
      mu$pfeir[mu$year == i] <- ((-mu$malaria[mu$year == i +1] * exp(-a)) / (mu$malaria[mu$year == i +1] - 1) ^ 1/b) *12
    }    
  }
  
  mu <- mu %>%
    filter(year < 2017)
  
  ##Build empty data frame
  id <- 1:length(U.2$District)
  District <- U.2$District
  year <- 2000:2016
  age <- 0:15
  sex <- c("Male", "Female")
  
  t <- crossing(District, year, age, sex)
  ##Merge with malaria data
  U.3 <- left_join(t, mu, by = c("District" = "District", "year" = "year") )
  
  U_cases$age <- U_cases$age  %>% as.numeric()
  
  ##Join shapefile and population data
  U.2 <- left_join(U.3, Uganda_pop, by = "District")
  
  ##Join long form data with case data
  U.final <- left_join(U.2, U_cases, by = c("District" = "District", "year"="Year enrolled","age"="age","sex"="Gender"))
  U.final$pop <- NA
  
  ##Put pop into long form
  U.final$pop[U.final$year == 2010 & U.final$sex == "Female"] <- (c(U.final$`2010: Female 0-4`[U.final$year == 2010 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2010: Female 10-15`[U.final$year == 2010 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2010: Female 5-9`[U.final$year == 2010 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2011 & U.final$sex == "Female"] <- (c(U.final$`2011: Female 0-4`[U.final$year == 2011 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2011: Female 10-15`[U.final$year == 2011 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2011: Female 5-9`[U.final$year == 2011 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2012 & U.final$sex == "Female"] <- (c(U.final$`2012: Female 0-4`[U.final$year == 2012 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2012: Female 10-15`[U.final$year == 2012 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2012: Female 5-9`[U.final$year == 2012 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2013 & U.final$sex == "Female"] <- (c(U.final$`2013: Female 0-4`[U.final$year == 2013 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2013: Female 10-15`[U.final$year == 2013 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2013: Female 5-9`[U.final$year == 2013 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2014 & U.final$sex == "Female"] <- (c(U.final$`2014: Female 0-4`[U.final$year == 2014 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2014: Female 10-15`[U.final$year == 2014 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2014: Female 5-9`[U.final$year == 2014 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2015 & U.final$sex == "Female"] <- (c(U.final$`2015: Female 0-4`[U.final$year == 2015 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2015: Female 10-15`[U.final$year == 2015 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2015: Female 5-9`[U.final$year == 2015 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2016 & U.final$sex == "Female"] <- (c(U.final$`2016: Female 0-4`[U.final$year == 2016 & U.final$sex == "Female" & U.final$age %in% 0:4], U.final$`2016: Female 10-15`[U.final$year == 2016 & U.final$sex == "Female" & U.final$age %in% 10:15], U.final$`2016: Female 5-9`[U.final$year == 2016 & U.final$sex == "Female" & U.final$age %in% 5:9]))
  
  U.final$pop[U.final$year == 2010 & U.final$sex == "Male"] <- (c(U.final$`2010: Male 0-4`[U.final$year == 2010 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2010: Male 10-15`[U.final$year == 2010 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2010: Male 5-9`[U.final$year == 2010 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2011 & U.final$sex == "Male"] <- (c(U.final$`2011: Male 0-4`[U.final$year == 2011 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2011: Male 10-15`[U.final$year == 2011 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2011: Male 5-9`[U.final$year == 2011 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2012 & U.final$sex == "Male"] <- (c(U.final$`2012: Male 0-4`[U.final$year == 2012 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2012: Male 10-15`[U.final$year == 2012 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2012: Male 5-9`[U.final$year == 2012 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2013 & U.final$sex == "Male"] <- (c(U.final$`2013: Male 0-4`[U.final$year == 2013 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2013: Male  10-15`[U.final$year == 2013 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2013: Male 5-9`[U.final$year == 2013 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2014 & U.final$sex == "Male"] <- (c(U.final$`2014: Male 0-4`[U.final$year == 2014 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2014: Male 10-15`[U.final$year == 2014 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2014: Male  5-9`[U.final$year == 2014 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2015 & U.final$sex == "Male"] <- (c(U.final$`2015: Male 0-4`[U.final$year == 2015 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2015: Male 10-15`[U.final$year == 2015 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2015: Male 5-9`[U.final$year == 2015 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  U.final$pop[U.final$year == 2016 & U.final$sex == "Male"] <- (c(U.final$`2016: Male 0-4`[U.final$year == 2016 & U.final$sex == "Male" & U.final$age %in% 0:4], U.final$`2016: Male 10-15`[U.final$year == 2016 & U.final$sex == "Male" & U.final$age %in% 10:15], U.final$`2016: Male 5-9`[U.final$year == 2016 & U.final$sex == "Male" & U.final$age %in% 5:9]))
  
  U.final$cases[is.na(U.final$cases)] <- 0
  
  ##Make malaria into % and selct variables
  U.final <- U.final %>%
    mutate(malaria = malaria *100) %>%
    dplyr::select(District, age, sex, year, cases, pop, malaria, pfeir)
  
  U.final <- U.final[order(U.final$year),]
  
  ##Assume pop is evenly distributed across age groups
  U.final$pop[U.final$age %in% 0:4] <- U.final$pop[U.final$age %in% 0:4]/5
  U.final$pop[U.final$age %in% 10:15] <- U.final$pop[U.final$age %in% 10:15]/6
  U.final$pop[U.final$age %in% 5:9] <- U.final$pop[U.final$age %in% 5:9]/5
  U.final$pop[U.final$year == 2010] <-  U.final$pop[U.final$year == 2010]*(1/6)
  U.final$pop[U.final$year == 2016] <-  U.final$pop[U.final$year == 2016]*(3/4)
  
  
  ##Add country variable
  U.final.2 <- U.final %>%
    mutate(country = "Uganda")%>%
    left_join(Uganda, by = "District") %>%
    st_as_sf()%>%
    dplyr::select("age","sex","year","cases","pop","malaria","pfeir","country", "District")
  
  return(U.final.2)
}

load_all_countries <- function(Kenya, Tanzania, Uganda){
  
  Uganda <- Uganda %>%
    st_transform(st_crs(Kenya))
  Tanzania <- Tanzania %>%
    st_transform(st_crs(Kenya))
  
  
  all <- rbind(Kenya, Tanzania, Uganda)
}

make_lag_data <- function(all){
  
  pfeir00 <- all$pfeir[all$year == 2000]
  pfeir01 <- all$pfeir[all$year == 2001]
  pfeir02 <- all$pfeir[all$year == 2002]
  pfeir03 <- all$pfeir[all$year == 2003]
  pfeir04 <- all$pfeir[all$year == 2004]
  pfeir05 <- all$pfeir[all$year == 2005]
  pfeir06 <- all$pfeir[all$year == 2006]
  pfeir07 <- all$pfeir[all$year == 2007]
  pfeir08 <- all$pfeir[all$year == 2008]
  pfeir09 <- all$pfeir[all$year == 2009]
  pfeir10 <- all$pfeir[all$year == 2010]
  pfeir11 <- all$pfeir[all$year == 2011]
  pfeir12 <- all$pfeir[all$year == 2012]
  pfeir13 <- all$pfeir[all$year == 2013]
  pfeir14 <- all$pfeir[all$year == 2014]
  pfeir15 <- all$pfeir[all$year == 2015]
  pfeir16 <- all$pfeir[all$year == 2016]
  
  all$lag1 <- all$lag2<- all$lag3<- all$lag4<- all$lag5<- all$lag6<- all$lag7<- all$lag8<- all$lag9<- all$lag10<-all$lag11<-all$lag12<-all$lag13<-all$lag14<-all$lag15<-all$lag16<-all$cumpfeir<- NA
  
  s <- list(pfeir00,pfeir01,pfeir02,pfeir03,pfeir04,pfeir05,pfeir06,pfeir07,pfeir08,pfeir09,pfeir10,pfeir11,pfeir12,pfeir13,pfeir14,pfeir15, pfeir16)
  
  all$lag0 <- all$pfeir
  
  year <- 2001:2016
  
  for(i in 1:16){
    all$lag1[all$year == year[i]] <- (s[[i]])
  }
  
  for(i in 2:16){
    all$lag2[all$year == year[i]] <- (s[[i-1]])
  }
  
  for(i in 3:16){
    all$lag3[all$year == year[i]] <- (s[[i-2]])
  }
  for(i in 4:16){
    all$lag4[all$year == year[i]] <- (s[[i-3]])
  }
  for(i in 5:16){
    all$lag5[all$year == year[i]] <- (s[[i-4]])
  }
  for(i in 6:16){
    all$lag6[all$year == year[i]] <- (s[[i-5]])
  }
  for(i in 7:16){
    all$lag7[all$year == year[i]] <- (s[[i-6]])
  }
  for(i in 8:16){
    all$lag8[all$year == year[i]] <- (s[[i-7]])
  }
  for(i in 9:16){
    all$lag9[all$year == year[i]] <- (s[[i-8]])
  }
  for(i in 10:16){
    all$lag10[all$year == year[i]] <- (s[[i-9]])
  }
  for(i in 11:16){
    all$lag11[all$year == year[i]] <- (s[[i-9]])
  }  
  for(i in 12:16){
    all$lag12[all$year == year[i]] <- (s[[i-9]])
  }  
  for(i in 13:16){
    all$lag13[all$year == year[i]] <- (s[[i-9]])
  }  
  for(i in 14:16){
    all$lag14[all$year == year[i]] <- (s[[i-9]])
  }
  for(i in 15:16){
    all$lag15[all$year == year[i]] <- (s[[i-9]])
  }
  for(i in 16){
    all$lag16[all$year == year[i]] <- (s[[i-9]])
  }
  
  all <- all %>%
    mutate(lag0 = ifelse(is.na(lag0), 0, lag0),
           lag1 = ifelse(is.na(lag1), 0, lag1),
           lag2 = ifelse(is.na(lag2), 0, lag2),
           lag3 = ifelse(is.na(lag3), 0, lag3),
           lag4 = ifelse(is.na(lag4), 0, lag4),
           lag5 = ifelse(is.na(lag5), 0, lag5),
           lag6 = ifelse(is.na(lag6), 0, lag6),
           lag7 = ifelse(is.na(lag7), 0, lag7),
           lag8 = ifelse(is.na(lag8), 0, lag8),
           lag9 = ifelse(is.na(lag9), 0, lag9),
           lag10 = ifelse(is.na(lag10), 0, lag10),
           lag11 = ifelse(is.na(lag11), 0, lag11),
           lag12 = ifelse(is.na(lag12), 0, lag12),
           lag13 = ifelse(is.na(lag13), 0, lag13),
           lag14 = ifelse(is.na(lag14), 0, lag14),
           lag15 = ifelse(is.na(lag15), 0, lag15),
           lag16 = ifelse(is.na(lag16), 0, lag16))
  
  all$cumpfeir[all$age == 0] <-  all$lag0[all$age ==0] 
  all$cumpfeir[all$age == 1] <-  all$lag0[all$age ==1] +all$lag1[all$age == 1]
  all$cumpfeir[all$age == 2] <-  all$lag0[all$age ==2] +all$lag1[all$age == 2]+  all$lag2[all$age == 2]
  all$cumpfeir[all$age == 3] <-  all$lag0[all$age ==3] +all$lag1[all$age == 3]+  all$lag2[all$age == 3]+ all$lag3[all$age == 3]
  all$cumpfeir[all$age == 4] <-  all$lag0[all$age ==4] +all$lag1[all$age == 4]+  all$lag2[all$age == 4]+ all$lag3[all$age == 4]+  all$lag4[all$age == 4]
  all$cumpfeir[all$age == 5] <-  all$lag0[all$age ==5] +all$lag1[all$age == 5]+  all$lag2[all$age == 5]+ all$lag3[all$age == 5]+  all$lag4[all$age == 5]+  all$lag5[all$age == 5]
  all$cumpfeir[all$age == 6] <-  all$lag0[all$age ==6] +all$lag1[all$age == 6]+  all$lag2[all$age == 6]+ all$lag3[all$age == 6]+  all$lag4[all$age == 6]+  all$lag5[all$age == 6]+  all$lag6[all$age == 6]
  all$cumpfeir[all$age == 7] <-  all$lag0[all$age ==7] +all$lag1[all$age == 7]+  all$lag2[all$age == 7]+ all$lag3[all$age == 7]+  all$lag4[all$age == 7]+  all$lag5[all$age == 7]+  all$lag6[all$age == 7]+  all$lag7[all$age == 7]
  all$cumpfeir[all$age == 8] <-  all$lag0[all$age ==8] +all$lag1[all$age == 8]+  all$lag2[all$age == 8]+ all$lag3[all$age == 8]+  all$lag4[all$age == 8]+  all$lag5[all$age == 8]+  all$lag6[all$age == 8]+  all$lag7[all$age == 8]+   all$lag8[all$age == 8]
  all$cumpfeir[all$age == 9] <-  all$lag0[all$age ==9] + all$lag1[all$age == 9]+  all$lag2[all$age == 9]+ all$lag3[all$age == 9]+  all$lag4[all$age == 9]+  all$lag5[all$age == 9]+  all$lag6[all$age == 9]+  all$lag7[all$age == 9]+   all$lag8[all$age == 9]+  all$lag9[all$age == 9]
  all$cumpfeir[all$age == 10] <- all$lag0[all$age ==10] +all$lag1[all$age == 10]+ all$lag2[all$age == 10]+all$lag3[all$age == 10]+ all$lag4[all$age == 10]+ all$lag5[all$age == 10]+ all$lag6[all$age == 10]+ all$lag7[all$age == 10]+  all$lag8[all$age == 10]+ all$lag9[all$age == 10]+ all$lag10[all$age == 10]
  all$cumpfeir[all$age == 11] <- all$lag0[all$age ==11] +all$lag1[all$age == 11]+ all$lag2[all$age == 11]+all$lag3[all$age == 11]+ all$lag4[all$age == 11]+ all$lag5[all$age == 11]+ all$lag6[all$age == 11]+ all$lag7[all$age == 11]+  all$lag8[all$age == 11]+ all$lag9[all$age == 11]+ all$lag10[all$age == 11]+ all$lag11[all$age == 11]
  all$cumpfeir[all$age == 12] <- all$lag0[all$age ==12] +all$lag1[all$age == 12]+ all$lag2[all$age == 12]+all$lag3[all$age == 12]+ all$lag4[all$age == 12]+ all$lag5[all$age == 12]+ all$lag6[all$age == 12]+ all$lag7[all$age == 12]+  all$lag8[all$age == 12]+ all$lag9[all$age == 12]+ all$lag10[all$age == 12]+ all$lag11[all$age == 12]+ all$lag12[all$age == 12]
  all$cumpfeir[all$age == 13] <- all$lag0[all$age ==13] +all$lag1[all$age == 13]+ all$lag2[all$age == 13]+all$lag3[all$age == 13]+ all$lag4[all$age == 13]+ all$lag5[all$age == 13]+ all$lag6[all$age == 13]+ all$lag7[all$age == 13]+  all$lag8[all$age == 13]+ all$lag9[all$age == 13]+ all$lag10[all$age == 13]+ all$lag11[all$age == 13]+ all$lag12[all$age == 13]+ all$lag13[all$age == 13]
  all$cumpfeir[all$age == 14] <- all$lag0[all$age ==14] +all$lag1[all$age == 14]+ all$lag2[all$age == 14]+all$lag3[all$age == 14]+ all$lag4[all$age == 14]+ all$lag5[all$age == 14]+ all$lag6[all$age == 14]+ all$lag7[all$age == 14]+  all$lag8[all$age == 14]+ all$lag9[all$age == 14]+ all$lag10[all$age == 14]+ all$lag11[all$age == 14]+ all$lag12[all$age == 14]+ all$lag13[all$age == 14]+ all$lag14[all$age == 14]
  all$cumpfeir[all$age == 15] <- all$lag0[all$age ==15] +all$lag1[all$age == 15]+ all$lag2[all$age == 15]+all$lag3[all$age == 15]+ all$lag4[all$age == 15]+ all$lag5[all$age == 15]+ all$lag6[all$age == 15]+ all$lag7[all$age == 15]+  all$lag8[all$age == 15]+ all$lag9[all$age == 15]+ all$lag10[all$age == 15]+ all$lag11[all$age == 15]+ all$lag12[all$age == 15]+ all$lag13[all$age == 15]+ all$lag14[all$age == 15]+ all$lag15[all$age == 15]
  
  all$INC <- 1
  for(i in 1:28288){
    if((all$year[i] < 2010 & all$country[i] == "Uganda") | (all$year[i] < 2012 & all$country[i] != "Uganda")){
      all$INC[i] <- NA
    }
    if(all$year[i] < 2010 & all$age[i] > 9){
      all$INC[i] <- NA
    }
    if(all$year[i] < 2011 & all$age[i] > 10){
      all$INC[i] <- NA
    }
    if(all$year[i] < 2012 & all$age[i] > 11){
      all$INC[i] <- NA
    }
    if(all$year[i] < 2013 & all$age[i] > 12){
      all$INC[i] <- NA
    }
    if(all$year[i] < 2014 & all$age[i] > 13){
      all$INC[i] <- NA
    }
    if(all$year[i] < 2015 & all$age[i] > 14){
      all$INC[i] <- NA
    }
  }
  
  all <- all %>%
    drop_na(INC) %>%
    st_as_sf()
  
  all <- all %>%
    filter(pop > 0) %>%
    mutate(year  = as.factor(year - 2009),
           age = as.factor(age),
           cuminf  = cumpfeir*.1,
           p = (cumpfeir*.1) / 100 )
  
  all$age <- relevel(all$age, ref = "10")
  
  return(all)
}


##Model 12: Continuous cumulative PfPR for 10 years / 50
model_12 <- function(data){
  
  m <- brm(cases ~ age + sex +country + p +year + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

make_background <- function(pf){
  k <- st_read("data/ken_admbnda_adm0_iebc_20191031.shp", quiet =TRUE)%>%
    st_transform(st_crs(pf[[1]]))%>%
    dplyr::select("Country" = ADM0_EN)
  
  ##Load shapefile
  Tan  = st_read("data/tza_admbnda_adm0_20181019.shp" , quiet=TRUE) %>%
    st_transform(st_crs(pf[[1]]))%>%
    dplyr::select("Country" = ADM0_EN)
  
  ##Load shapefile for Uganda
  Uganda  <- st_read("data/uga_admbnda_adm0_2020.shp", quiet=TRUE) %>%
    mutate("Country" = "Uganda")
  st_crs(Uganda) <- st_crs(k)
  
  lake <- st_read("data/LAKE.shp") %>%
    mutate(Country = "Lake") %>%
    dplyr::select(Country)
  
  st_crs(lake)<- CRS("+proj=utm +zone=36M+datum=WGS84")
  lake <- lake  %>%
    st_transform(st_crs(Uganda))
  
  ##Merge background
  b <- rbind(k, Tan, Uganda)
  
  p <- list(b, lake)
  
  return(p)
}


make_figure1 <- function(data){
  data$agef <- factor(data$age, levels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
  data$year2 <- as.numeric(data$year) + 2009
  
  f <- ggplot()+
    geom_boxplot(data = data[data$year2 %in% c(2012,2014,2016),], aes(x = agef, y = cumpfeir*.1, group = age))+
    facet_grid( vars(country), vars(year2)) +
    md_theme_bw() +
    labs(x = "Age", y = "Estimated number of *P. falciparum* infections")
  
  return(f)
}

make_figure2 <- function(data, background){
  ##Aggregate data to district
  sf_data <- data %>%
    filter(duplicated(District) == FALSE) %>%
    dplyr::select(District, country)

  agg_data <- as.data.frame(data) %>%
    group_by(District,country) %>%
    summarise(meanpfi = mean(cumpfpr, na.rm =T),
              meanebl = mean(cases, na.rm = T)/ mean(pop, na.rm =T)) %>%
    left_join(sf_data, by = c("District", "country")) %>%
    st_as_sf()

  country <- agg_data %>%
    group_by(country) %>%
    summarise()
  
  background2 <- st_make_valid(background[[1]]) %>%
    sf::st_crop(st_make_valid(agg_data))
  
  ##Plot the average Cumulative malaria infections
  p1<-  ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill = "lightblue", lwd = 0.2)+
    geom_sf(data = agg_data, aes(fill = meanpfi), lwd = 0.2) +
    scale_fill_gradient(low = "yellow", high = "red") +
    labs(fill = "Average Annual Cumulative<br>*P. falciparum* Infections per Child") +
    theme(legend.title =  element_markdown(size=12),
          legend.text=element_text(size=12),
          legend.position = "right", 
          axis.title = element_blank(),
          axis.text =  element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())

###Plot the average cumulative Pf Infections
   p2<-  ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill = "lightblue", lwd = 0.2)+
    geom_sf(data = agg_data, aes(fill = meanebl*1000000), lwd = 0.2) +
    scale_fill_gradient(low = "yellow", high = "red") +
     labs(fill = "Average Annual eBL<br>Incidence per Million") +
     theme(legend.title =  element_markdown(size = 12),
           legend.text=element_text(size=12),
           legend.position = "left",
           axis.title = element_blank(),
           axis.text =  element_blank(),
           axis.ticks = element_blank(),
           panel.background = element_blank(),
           panel.grid = element_blank())

  ###Plot just the countries
  country$X <- st_coordinates(st_centroid(st_make_valid(country)))[,1]
  country$Y <- st_coordinates(st_centroid(st_make_valid(country)))[,2]
    p3 <- ggplot()+
      geom_sf(data = background[[1]], fill = "lightgrey", lwd = 0.2)+
      geom_sf(data = background[[2]], fill = "lightblue", lwd = 0.2)+
      geom_sf(data = agg_data, aes(fill = country, color = country), lwd = 0.01) +
      geom_text(data = country, aes(x = X, y = Y, label = country))+
      guides(color=FALSE, fill = guide_legend("Country"))+
      theme(legend.text=element_text(size=12),
            legend.title = element_text(size=12),
            legend.position = "top", 
            axis.title = element_blank(),
            axis.text =  element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank())
    
    return(list(p2,p3,p1))
}

make_figure3 <- function(data, model12){

###Make an empty dataset and get predictions
cond <- data.frame("sex" = "Male", "country" ="Kenya", "pop" = 1000000, "year" = 1, p =seq(0,3.54, 0.001), "age" = 11)
post <- posterior_epred(model12, newdata = cond, re_formula = NA)

lci <- c()
uci <- c()
irr <- c()

for(i in 1:3541){
  irr[[i]]<- mean(post[,i]/ post[,501])
  lci[[i]] <- quantile(post[,i]/post[,501], 0.025, na.rm =TRUE)
  uci[[i]] <- quantile(post[,i]/post[,501], 0.975, na.rm =TRUE)
}

cond$irr <- unlist(irr)
cond$lci <- unlist(lci)
cond$uci <- unlist(uci)

m12_irr <- ggplot()+
  geom_ribbon(data =cond, aes(x = p*100, ymin = (lci), ymax = (uci)), alpha = 0.25) +
  geom_line(data =cond, aes(x=p*100, y= (irr))) +
  theme_bw()+
  geom_hline(yintercept = 1, color = "red", linetype = "dashed")+
  labs(x = "Estimated number of lifetime *P. falciparum* infections",y = "eBL IRR", title = "A")+# y = "eBL Incidence Rate Ratio\n(Compared to 50 Expected Malaria Infections)")+
  theme(axis.title.x  = element_markdown(size = 12),
        text = element_text(size =12),
        plot.title = element_text(hjust = 0.5))

##Get age specific effects
c1 <- data.frame("sex" = "Male", "country" ="Kenya", "pop" = 1000000, "year" = 1, p =c(seq(.01,.16,.01), seq(.10,1.60,.10), seq(.20,3.20,.20)) ,  "age" = rep(c(0:15),3), v = c(rep("1 Annual Infection", 16),rep("10 Annual Infections", 16), rep("20 Annual Infections", 16)))
c1$v <- factor(c1$v, levels = c("1 Annual Infection","10 Annual Infections","20 Annual Infections"))

post <- posterior_epred(model12, newdata = c1, re_formula = NA)

lci <- c()
uci <- c()
irr <- c()

for(i in 1:48){
  irr[i]<- mean(post[,i] / post[,6])
  lci[i] <- quantile(post[,i] / post[,6], 0.025, na.rm =TRUE)
  uci[i] <- quantile(post[,i] / post[,6], 0.975, na.rm =TRUE)
}

c1$irr <- unlist(irr)
c1$lci <- unlist(lci)
c1$uci <- unlist(uci)


agegrow <- ggplot()+
  geom_ribbon(data = c1, aes(x = age, ymin = lci, ymax = uci), alpha = 0.25)+
  geom_line(data = c1, aes(x = age, y = irr))+
  geom_hline(yintercept = 1, color = "red", linetype = "dashed")+
  theme_bw()+
  labs(x = "Age", y = "Incidence Rate Ratio", title = "B",caption = "Reference is a 5 yo with 6 *P. falciparum* Infections") +
  facet_wrap(~v, nrow =1)+
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_markdown())

p3 <- ggarrange(m12_irr, agegrow, nrow = 2)

return(p3)

}

make_figureA2 <- function(data, background){
  
  background2 <- st_make_valid(background[[1]]) %>%
    st_crop(data)
  
  agg_data <- data %>%
    group_by(District, country) %>%
    summarise(pop = sum(pop, na.rm =T))
  
  p1 <- ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill = "lightblue", lwd = 0.2)+
    geom_sf(data = agg_data, aes(fill = pop), lwd = 0.2) +
    theme(legend.text = element_text(size = 12))+
    theme_void()+
    labs(fill = "Person years contributed\nby District") +
    scale_fill_gradient(low = "yellow", high = "red")

    return(p1)    
}

make_figureA3 <- function(data){
  
  yd <- as.data.frame(data) %>%
    group_by(year, country) %>%
    summarise(pop = sum(pop)) %>%
    mutate(year = as.factor(as.numeric(year) + 2009),
           cn = ifelse(country == "Uganda", .09, ifelse(country == "Tanzania", .5, .89)))
  
  p1 <- ggplot(yd, aes(fill=country, y=pop, x=year)) + 
    geom_bar(position="fill", stat="identity")+
    theme_bw()+
    scale_y_continuous(labels=scales::percent) +
    labs(x = "Year", y = "Percentage of person-years contributed", fill = "Country")+ 
    scale_fill_discrete(guide = guide_legend()) +
    theme(legend.position="top")+
    geom_text(aes( x= year, y = cn, label = scales::comma(round((pop)))), angle = 90)
  
}

make_figureA4 <- function(data, all){
  null_model <- brm(cases ~ year + age + sex + country + (1|District) + offset(log(pop)),
                    data =data,
                    family = negbinomial(),
                    backend = "cmdstanr",
                    cores = 4,
                    threads = threading(4))
  
  cond1 <- data.frame("sex" = "Male", "country" =rep(c("Kenya","Tanzania", "Uganda"),7) , "pop" = 1000000, "year" = sort(rep(c(1:7),3)),"age" = 10) %>%
    filter((year == 1 & country == "Uganda" )|(year == 2 & country == "Uganda" )|year > 2)
  
  p <- predict(null_model, cond1, re_formula = NA)
  
  cond1$estimate <- p[,1]
  cond1$uci <- p[,4]
  cond1$lci <- p[,3]
  cond1$year <- as.numeric(cond1$year) + 2009
  
  p1 <- ggplot()+
    geom_ribbon(data = cond1, aes(x = year, ymin = lci, ymax = uci), alpha = 0.25)+
    geom_line(data = cond1, aes(x = year, y = estimate, color = country))+
    theme_bw()+
    theme(legend.position = "none")+
    labs(x = "Year", y ="eBL Incidence per Million", color = "Country") +
    facet_wrap(~country, scales = "free_y")+
    xlim(2000, 2016)
  
  agg <- as.data.frame(all) %>%
    group_by(year = as.factor(year), country, District) %>%
    summarise(pfeir = mean(pfeir),
              p = pfeir * .1)
  
  pfpr_model2 <- brm(p ~ year + country + year*country + (1|District),
                     data = agg,
                     backend = "cmdstanr",
                     cores = 4,
                     threads = threading(4))
  
  cond2 <- data.frame("sex" = "Male", "country" =rep(c("Kenya","Tanzania", "Uganda"),17) , "pop" = 1000000, "year" = sort(rep(2000:2016,3)), age = 10)
  
  p <- predict(pfpr_model2, cond2, re_formula = NA)
  
  cond2$estimate <- p[,1]
  cond2$uci <- p[,4]
  cond2$lci <- p[,3]
  cond2$var <- "pfpr"
  
  p2 <- ggplot()+
    geom_ribbon(data = cond2, aes(x = year, ymin = lci, ymax = uci, group = country), alpha = 0.25)+
    geom_line(data = cond2, aes(x = year, y = estimate, color = country, group =country))+
    md_theme_bw()+
    theme(legend.position = "none")+
    labs(x = "Year", y ="Annual *P. falciparum* Incidence per Child", color = "Country") +
    facet_grid(~country)+
    xlim(2000, 2016)
  
  p3 <- ggarrange(p2,p1, ncol = 1)
  return(p3)
  }

make_figureA5 <- function(){
  p <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']


      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4-> tab5;
      }

      [1]: 'Suspected cases captured in eBL study regions\\n n = 862'
      [2]: 'Restricted to cases confirmed by historical and clinical review\\n n =697'
      [3]: 'Restricted to cases with complete data\\n n = 668'
      [4]: 'Restricted to ditricts with population data\\n n = 598'
      [5]: 'Restricted to cases with entire\\nlifespan included in MAP data\\n n = 552'

      ")
  return(p)
}

##Model 0:
model_0 <- function(data){
  m <- brm(cases ~ lag0 + age + sex +country + year + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}
##Model 1:

model_1 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year + lag1 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 2: Only lag2 as a predictor
model_2 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag2 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 3: Only lag3 as a predictor
model_3 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag3 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 4: Only lag4 as a predictor
model_4 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag4 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 5: Only lag5 as a predictor
model_5 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag5 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 6: Only lag6 as a predictor
model_6 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag6 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 7: Only lag7 as a predictor
model_7 <- function(data){
  m <- brm(cases ~ age + sex +country + year +lag7 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 8: Only lag8 as a predictor
model_8 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag8 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 9: Only lag5 as a predictor
model_9 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag9 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 10: Only lag5 as a predictor
model_10 <- function(data){
  
  m <- brm(cases ~ age + sex +country + year +lag10 + (1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

##Model 11: Lags 0-10 as predictors
model_11 <- function(data){
  m <- brm(cases ~ age + sex +country + year + lag0 +lag1+lag2 +lag3+lag4 +lag5 +lag6+ lag7+ lag8 +lag9+ lag10 +(1|District) + offset(log(pop)),
           data = data,
           family = negbinomial(),
           backend = "cmdstanr",
           cores = 4)
  return(m)
}

make_figureA6 <- function(data, background){
  agg_data <- data %>%
    group_by(District, year, country) %>%
    summarise(cuminc = mean(cumpfeir*.1),
              rate = sum(cases)/sum(pop))
  
    agg_U <- agg_data %>%
    filter(country == "Uganda")
  agg_KT <- agg_data %>%
    filter(country != "Uganda")
  
  U_lm <- data.frame()
  
  year <- 1:7
  for(i in 1:7){
    dU <- agg_U %>%
      filter(year == i)
    w <- queen_weights(dU[dU$year == i,])
    
    dU$lmebl <- lisa_clusters(local_moran(w, dU["rate"]))
    dU$lmpf <- lisa_clusters(local_moran(w, dU["cuminc"]))
    
    U_lm <- rbind(U_lm, dU)
  }
  
  KT_lm <- data.frame()
  years <- c(3:7)
  
  for(i in 1:5){
    dKT <- agg_KT %>%
      filter(year == years[i])
    w <- queen_weights(dKT[dKT$year == years[i],])
    
    dKT$lmebl <- lisa_clusters(local_moran(w, dKT["rate"]))
    dKT$lmpf <- lisa_clusters(local_moran(w, dKT["cuminc"]))
    
    KT_lm <- rbind(KT_lm, dKT)
  }
  
  lm <- rbind(KT_lm, U_lm) %>%
    group_by(country, District) %>%
    summarise(high_ebl = sum(lmebl %in% c(2,5)),
              low_ebl = sum(lmebl %in% c(3,4)),
              high_pf = sum(lmpf %in% c(2,5)),
              low_pf = sum(lmpf %in% c(4,3)))

  background2 <- st_make_valid(background[[1]]) %>%
    st_crop(data)
  
  p1 <- ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill= "lightblue")+
    geom_sf(data = lm, aes(fill = as.factor(high_ebl)), lwd = 0.2)+
    theme_void()+    
    ggtitle("eBL Incidence")+
    labs(fill = "Years with Significant\nHigh Lisa Values")+
    scale_fill_brewer(palette = "OrRd", limits=as.factor(c(0:5)))
  
  p2 <- ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill= "lightblue")+
    geom_sf(data = lm, aes(fill = as.factor(low_ebl)), lwd = 0.2)+
    labs(fill = "Years with Significant\nLow Lisa Values")+
    scale_fill_brewer(palette = "Blues", limits=as.factor(c(0:5)))+
    theme(plot.title =  element_markdown(size=12),
          text=element_text(size=12),
          legend.position = "right", 
          axis.title = element_blank(),
          axis.text =  element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())
  
  p3 <- ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill= "lightblue")+
    geom_sf(data = lm, aes(fill = as.factor(high_pf)), lwd = 0.2)+
    labs(fill = "Years with Significant\nHigh Lisa Values")+
    scale_fill_brewer(palette = "OrRd", limits=as.factor(c(0:5)))+
    ggtitle("Cumulative *P. falciparum*<br>incidence")+
    theme(plot.title =  element_markdown(size=12),
          text=element_text(size=12),
          legend.position = "right", 
          axis.title = element_blank(),
          axis.text =  element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())

  p4 <- ggplot()+
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill= "lightblue")+
    geom_sf(data = lm, aes(fill = as.factor(low_pf)), lwd = 0.2)+
    theme_void()+
    labs(fill = "Years with Significant\nLow Lisa Values")+
    scale_fill_brewer(palette = "Blues", limits=as.factor(c(0:5)))


g1 <- ggarrange(p3, p1, common.legend =T, nrow =1, ncol = 2, legend = "right")

g2 <- ggarrange(p4, p2, common.legend =T, nrow =1, ncol = 2, legend = "right")



p <- ggarrange(g1, g2, nrow = 2, ncol =1)
return(p)
}

make_figureA7 <- function(data, background){
 
  agg_data <- data %>%
    group_by(District, country) %>%
    summarise(cuminc = mean(cumpfeir*.1),
              rate = sum(cases)/sum(pop))
  
  agg_U <- agg_data %>%
    filter(country == "Uganda")
  agg_KT <- agg_data %>%
    filter(country != "Uganda")

    w <- queen_weights(agg_U)
    agg_U$lbm <- lisa_clusters(local_bimoran(w, agg_U[c("cuminc", "rate")]))

    w <- queen_weights(agg_KT)
    
    agg_KT$lbm <- lisa_clusters(local_moran(w, agg_KT[c("cuminc", "rate")]))
  
    d <- rbind(agg_U, agg_KT)
      
  labels <- c("Not significant", 
              "High cumulative *P. falciparum* infections -<br>High eBL Incidence",
              "Low cumulative *P. falciparum* infections -<br>Low eBL Incidence",
              "Low cumulative *P. falciparum* infections -<br>High eBL Incidence",
              "High cumulative *P. falciparum* infections -<br>Low eBL Incidence",
              "Not Significant")
  labels <- as.factor(labels)
  
  colors  <-  c("#eeeeee", "#FF0000", "#0000FF", "#a7adf9", "#f4ada8", "#464646", "#eeeeee")         

    background2 <- st_make_valid(background[[1]]) %>%
    st_crop(data)
  
  p <- ggplot() + 
    geom_sf(data = background2, fill = "white", lwd = 0.2)+
    geom_sf(data = background[[2]], fill = "lightblue", lwd = 0.2)+
    geom_sf(data = d, aes(fill = as.factor(lbm)), lwd = 0.2)+
    scale_fill_manual(values = colors, labels = labels)+
    labs(fill = "Bivariate Moran's *I*")+
    ggtitle("Bivariate Moran's *I* for cumulative<br>*P. falciparum* infections<br>and eBL Incidence")+
    theme(plot.title =  element_markdown(size=12),
          legend.text = element_markdown(size = 12),
          legend.title = element_markdown(size =12),
          legend.position = "right", 
          axis.title = element_blank(),
          axis.text =  element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank())
sum(d$lbm %in% c(2,1))
sum(d$lbm %in% c(2,3))
d$lbm
return(p)
}

make_figureA8 <- function(model12){
  ##Get age specific effects
  cond1 <- data.frame("sex" = "Male", "country" ="Kenya", "pop" = 1000000, "year" = 1, p =rep(seq(0,3.54, 0.001),16), "age" = sort(rep(c(0:15), 3541)))

  post <- posterior_epred(model12, newdata = cond1, re_formula = NA)
  
  lci <- c()
  uci <- c()
  irr <- c()
  
  for(i in 1:56656){
    irr[i]<- mean(post[,i] / post[,35511])
    lci[i] <- quantile(post[,i] / post[,35511], 0.025, na.rm =TRUE)
    uci[i] <- quantile(post[,i] / post[,35511], 0.975, na.rm =TRUE)
  }

  cond1$irr <- unlist(irr)
  cond1$lci <- unlist(lci)
  cond1$uci <- unlist(uci)


 p <- ggplot()+
    geom_ribbon(data = cond1[cond1$age %in% c(2,5,8,11,15),], aes(x = p*100, ymin = lci, ymax = uci), alpha = 0.25)+
    geom_line(data = cond1[cond1$age %in% c(2,5,8,11,15),], aes(x = p*100, y = irr))+
    geom_hline(yintercept = 1, color = "red", linetype = "dashed")+
    labs(x = "Estimated number of lifetime *P. falciparum* infections", y = "eBL IRR",
         subtitle = "Age")+
    facet_wrap(~age, nrow =1)+
    theme(text = element_text(size = 15),
          axis.title.x = element_markdown(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5)) 
return(p)
}

make_figureA9 <- function(model0,model1,model2, model3, model4, model5, model6, model7, model8, model9, model10){
  
  df <-  data.frame("Covariate" = c("No Lag","1 Year Lag", "2 Year Lag", "3 Year Lag", "4 Year Lag", "5 Year Lag", "6 Year Lag", "7 Year Lag", "8 Year Lag", "9 Year Lag", "10 Year Lag"), 
                    "Estimate" = c(fixef(model0)[2,][1],fixef(model1)[26,][1],fixef(model2)[26,][1],fixef(model3)[26,][1],fixef(model4)[26,][1], fixef(model5)[26,][1],fixef(model6)[26,][1],fixef(model7)[26,][1],fixef(model8)[26,][1],fixef(model9)[26,][1],fixef(model10)[26,][1]),
                    "Lower 95% CI" = c(fixef(model0)[2,][3],fixef(model1)[26,][3],fixef(model2)[26,][3],fixef(model3)[26,][3],fixef(model4)[26,][3], fixef(model5)[26,][3], fixef(model6)[26,][3],fixef(model7)[26,][3], fixef(model8)[26,][3],fixef(model9)[26,][3], fixef(model10)[26,][3]),
                    "Upper 95% CI" = c(fixef(model0)[2,][4],fixef(model1)[26,][4],fixef(model2)[26,][4],fixef(model3)[26,][4],fixef(model4)[26,][4], fixef(model5)[26,][4], fixef(model6)[26,][4],fixef(model7)[26,][4], fixef(model8)[26,][4],fixef(model9)[26,][4], fixef(model10)[26,][4]))
  
  #Make the covariate variable a factor
  df$Covariate <- factor(df$Covariate,levels=(unique(df$Covariate)))
  
  #Individual lag models plot (for appendix)
 p <-  ggplot(data = df, aes(y=Covariate, x=exp(Estimate))) + 
    geom_point()+
    geom_vline(xintercept = 1, 
               color = "red", size=.5)+
    geom_errorbar(aes(xmin=exp(Lower.95..CI), xmax=exp(Upper.95..CI)))+
    theme_bw()+
    xlab("Incidence Rate Ratio")+
    ylab("")+
    theme(text = element_text(size = 15)) 
  
 return(p)
  
}

make_figureA10 <- function(m11){

  df2 <- data.frame("Covariate" = c("No Lag", "1 Year Lag","2 Year Lag","3 Year Lag","4 Year Lag", "5 Year Lag", "6 Year Lag", "7 Year Lag", "8 Year Lag", "9 Year Lag", "10 Year Lag"),
                    "Estimate" = c(fixef(m11)[26],fixef(m11)[27],fixef(m11)[28],fixef(m11)[29],fixef(m11)[30],fixef(m11)[31],fixef(m11)[32],fixef(m11)[33],fixef(m11)[34],fixef(m11)[35],fixef(m11)[36]),
                    "Lower 95% CI" = c(fixef(m11)[26,][3],fixef(m11)[27,][3],fixef(m11)[28,][3],fixef(m11)[29,][3],fixef(m11)[30,][3],fixef(m11)[31,][3],fixef(m11)[32,][3],fixef(m11)[33,][3],fixef(m11)[34,][3],fixef(m11)[35,][3],fixef(m11)[36,][3]),
                    "Upper 95% CI" = c(fixef(m11)[26,][4],fixef(m11)[27,][4],fixef(m11)[28,][4],fixef(m11)[29,][4],fixef(m11)[30,][4],fixef(m11)[31,][4],fixef(m11)[32,][4],fixef(m11)[33,][4],fixef(m11)[34,][4],fixef(m11)[35,][4],fixef(m11)[36,][4]))
  
  df2$Covariate <- factor(df2$Covariate,levels=(unique(df2$Covariate)))
  
  #Plot model 11
 p<-  ggplot(df2, aes(y=Covariate, x=exp(Estimate))) + 
    geom_point()+
    geom_vline(xintercept = 1, 
               color = "red", size=.5)+
    geom_errorbarh(aes(xmin=exp(Lower.95..CI), xmax=exp(Upper.95..CI)))+
    theme_bw()+ 
    xlab("Incidence Rate Ratio")+
    ylab("")+
    theme(text = element_text(size = 15)) 
  
 return(p)
  
  }
  