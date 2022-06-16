####Put my functions
read_data <- function(){
  ##Load data
  d <- readRDS("Data/limited--latest--df.rds")
  sp <- readRDS("Data/spatial--latest--df.rds")
  
  ##Tracts shapefile
  t <- tigris::tracts("MI")
  
  ##Join the deaths data with geocoding and transform to sf object
  df <- left_join(d, sp, by = "record_id") %>%
    filter(death_date_y > 2014) %>%
    drop_na(longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84") %>%
    st_transform(st_crs(t))
  
  ##Join the death data with tracts
  df2 <- st_join(df, t)  

  ##Convert to data frame
  ##Mutate age, race, and sex to be usable
  ##Group by GEOID, date, age, race, and sex
  ##Deaths is equal to the link of GEOID
  ##Drop missing age, sex, and race
  df3 <- df2 %>%
    as.data.frame()%>%
    mutate(age = ifelse(age_years >59, "60+", "Under60"),
           race = ifelse(ethnicity_hispanic == "yes", "hispanic", ifelse(race_1 %in% c("other_asian", "vietnamese", "asian_indian", "other_pacific_islander", "filipino", "japanese", "korean", "guaman_or_chamorro", "chinese", "native_hawaiian", "samoan"), "AAPI", ifelse(race_1 == "american_indian", "other_race", race_1))),
           month = month(death_date_ymd),
           year = year(death_date_ymd))%>%
    group_by(GEOID,month,year, age, race,) %>%
    summarize(deaths = length(GEOID)) %>%
    drop_na(age) %>%
    filter(race != "unknown")

##Make an empty dataframe  
month <- 1:12
year <- 2015:2020
age <- c("60+", "Under60")
race <- unique(df3$race)
id <- unique(df3$GEOID)
  
emp <- crossing(id, month, year, age, race)
  
##Join the empty dataframe with the death data
df4 <- left_join(emp, df3, by = c("age","race", "month","year", "id" = "GEOID")) %>%
  filter(race != "other_race")

##make missing deaths 0s
df4$deaths[is.na(df4$deaths)] <- 0
  
###Add population data

v <- c(paste("B01001A_00",3:9, sep = ""), paste("B01001A_0",10:16, sep = ""),
       paste("B01001A_0",18:31, sep = ""),
       paste("B01001B_00",3:9, sep = ""), paste("B01001B_0",10:16, sep = ""),
       paste("B01001B_0",18:31, sep = ""),
       paste("B01001D_00",3:9, sep = ""), paste("B01001D_0",10:16, sep = ""),
       paste("B01001D_0",18:31, sep = ""),
       paste("B01001E_00",3:9, sep = ""), paste("B01001E_0",10:16, sep = ""),
       paste("B01001E_0",18:31, sep = ""),
       paste("B01001I_00",3:9, sep = ""), paste("B01001I_0",10:16, sep = ""),
       paste("B01001I_0",18:31, sep = ""))

years <- 2015:2020

pop <- data.frame()

for(i in 1:6){
  p <- get_acs(geography = "tract", variables = v, year = years[i],state= "MI", key = Sys.getenv("CENSUS_API_KEY")) %>%
    mutate(year = years[i])
  pop <- rbind(pop, p)
}

pop2 <- pop %>%
  mutate(d = substr(variable, 7,7),
         race = ifelse(d == "A", "white", ifelse(d == "B", "black", ifelse(d == "D"| d == "E", "AAPI", "hispanic"))),
         a  = as.numeric(substr(variable, 10, 11)),
         age = ifelse(a %in% c(3:17, 27:41), "Under60", "60+")) %>%
  group_by(GEOID, year, age, race) %>%
  summarise(pop = sum(estimate, na.rm = TRUE))

pop3 <- pop2 %>%
  filter(pop > 0)


  df5 <- left_join(df4, pop3, by = c("id" = "GEOID", "age", "race", "year"))%>%
    mutate(pop = ifelse(is.na(pop), 0, pop))

    return(df5)
}

sub_data <- function(data) {

  t <- crossing(age= unique(data$age), race = unique(data$race))
  
  df <- list()
  
  for(i in 1:8){
    
    df[[i]] <- data %>%
      filter(age == t$age[i] & race == t$race[i]) %>%
      mutate(t = as.numeric(as_date(paste("1", month, year, sep = "-"), format = "%d-%m-%Y")))
  
 
  }
  
  return(df)
}

gam_models <- function(subdf){
  models <-list()
  for(i in 1:12){
    
    data = sub_df[[i]] %>%
      filter(year < 2020) %>%
      mutate(pop = pop/12,
             id = as.factor(id))
    
    models[[i]] <- gam(deaths ~ s(id, bs = "re") + 
                    s(t) +
                    s(month, bs = "cc", k = 4) +
                    s(month, id)+
                    s(year, k = 4),
                  knots = list(wday=c(0.5, 12.5)),
                  data = data,
                  family = nb(),
                  method = "REML")
  }
}
