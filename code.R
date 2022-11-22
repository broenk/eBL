Packages <- c("dplyr", "ggplot2", "rstan", "readr", "tidyverse", "rgeoda","raster", "targets", "sf","spdep", "mdthemes", "brms", "lubridate", "ggtext")
lapply(Packages, library, character.only = TRUE)
setwd("/Users/kellybroen/Documents/GitHub/Aim1")

###Figure 1
figure1 <- tar_read(figure1)

ggsave("images/figure1.jpg", dpi = 1000)


###Figure 2
tar_make(figure2)
figure2 <- tar_read(figure2)
ggsave("images/figure2A.jpg",figure2[[1]],  height = 6, width = 6, dpi = 1000)
ggsave("images/figure2B.jpg",figure2[[2]],  height = 6, width = 6, dpi = 1000)
ggsave("images/figure2C.jpg",figure2[[3]],  height = 6, width = 6, dpi = 1000)

##Figure 3
tar_make(figure3)
figure3 <- tar_read(figure3)
ggsave("images/figure3.jpg",figure3,  height = 10, width = 8,dpi = 1000)

###Table 1 statistics

###Cases by country
data <- tar_read(data)

tapply(data$cases, data$country, sum)
p <- tapply(data$pop, data$country, sum)
(tapply(data$cases, data$country, sum)/p)*1000000
(sum(data$cases)/sum(data$pop))*1000000 

t1 <- aov(ebl ~ country, data = data)
summary(t1)

###Cases by sex
tapply(data$cases, list(data$sex, data$country), sum)
tapply(data$cases, list(data$sex), sum)
p <- tapply(data$pop, data$sex, sum)
(tapply(data$cases, list(data$sex), sum)/p)*1000000
p2 <- tapply(data$pop, list(data$sex, data$country), sum)
(tapply(data$cases, list(data$sex, data$country), sum)/p2)*1000000

t2 <- aov(ebl ~ sex, data = data)
summary(t2)

##Cases by age group
data <- data %>%
  mutate(age_f = ifelse(as.numeric(age) <5, "0-4", ifelse(as.numeric(age) < 11, "5-10", "10-15")))

tapply(data$cases, list(data$age_f, data$country), sum)
tapply(data$cases, list(data$age_f), sum)
p <- tapply(data$pop, data$age_f, sum)
(tapply(data$cases, list(data$age_f), sum)/p)*1000000
p2 <- tapply(data$pop, list(data$age_f, data$country), sum)
(tapply(data$cases, list(data$age_f, data$country), sum)/p2)*1000000

t3 <- aov(ebl ~ age_f, data = data)
summary(t3)

##Cases by year
tapply(data$cases, list(data$year, data$country), sum)
tapply(data$cases, list(data$year), sum)
p <- tapply(data$pop, data$year, sum)
(tapply(data$cases, list(data$year), sum)/p)*1000000
p2 <- tapply(data$pop, list(data$year, data$country), sum)
(tapply(data$cases, list(data$year, data$country), sum)/p2)*1000000

t4 <- aov(ebl ~ year, data = data)
summary(t4)

###Median annual malaria infections
tapply(data$pfeir*.1, data$country, median)
tapply(data$pfeir*.1, data$country, sd)

median(data$pfeir*.1)
sd(data$pfeir*.1)


###Figure A2
tar_make(figureA2)
figureA2 <- tar_read(figureA2)
ggsave("images/figureA2.jpg", figureA2, height = 6, width = 6)

###Figure A3
tar_make(figureA3)
figureA3 <- tar_read(figureA3)
ggsave("images/figureA3.jpg",figureA3,  height = 6, width = 6)

###Figure A4
tar_make(figureA4)
figureA4 <- tar_read(figureA4)
ggsave("images/figureA4.jpg",figureA4,  height = 6, width = 6)

###Figure A5
tar_make(figureA5)
figureA5 <- tar_read(figureA5)

###Figure A6
tar_make(figureA6)
figureA6 <- tar_read(figureA6)

ggsave("images/figureA6.jpg",figureA6,  height = 10, width = 8)

###Figure A7
tar_make(figureA7)
figureA7 <- tar_read(figureA7)

ggsave("images/figureA7.jpg",figureA7,  height = 8, width = 6)

###Figure A8
tar_make(figureA8)
figureA8 <- tar_read(figureA8)

ggsave("images/figureA8.jpg",figureA8,  height = 6, width = 8)


###Figure A9
tar_make(figureA9)
figureA9 <- tar_read(figureA9)
figureA9
ggsave("images/figureA9.jpg",figureA9,  height = 6, width = 8)




