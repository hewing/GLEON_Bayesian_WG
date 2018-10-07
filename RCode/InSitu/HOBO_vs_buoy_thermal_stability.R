##Comparison of CV of HOBO temp data to stability metrics from buoy data
##Author: Mary Lofton
##Date: 19SEP18
##Aim: compare the coefficient of variation of HOBO temperature data logged at 10 minute 
#intervals at 4 sites in Lake Sunapee to daily thermal stability metrics calculated in 
#LakeAnalyzer from buoy depth profiles of temperature at the deepest site of the lake to
#determine if it would be appropriate to use CV of temperature at Gloeo sites as a proxy of
#mixing/stability

#install.packages(pacman)
pacman::p_load(tidyverse, lubridate)

#loading in data and data wrangling
#dropping flags for simplicity's sake
#gathering sites to make plotting easier
##SWITCH OVER TO USING ONSET DATA IN SUMMARIZED DATA FOLDER ON GITHUB
hobo <- read_csv("./Datasets/Sunapee/Level1/temp_2006-2016_L1_20Oct2017.csv") %>%
  select(-c(coffin_flag:midge_flag)) %>%
  gather(coffin:midge, key = "site", value = "temp_C") %>%
  mutate(temp_C = as.double(temp_C))

#initial visualization to see when we have a time series of all sites
plot1 = ggplot(hobo, aes(x = datetime, y = temp_C, group = site, colour = site))+
  geom_line(size = 1)+
  theme_bw()
plot1
#it looks like we have data from all the sites for 2007-onward

#calculate CV of temp at each site by day
#doing this as a proxy measure of stability/mixing for comparison to thermal stability
#metrics from buoy data
#co.var <- function(x) ( (sd(x)/mean(x))*100 )
#we actually want sd

hobo_sd = aggregate(temp_C ~ date+site, hobo, sd)%>%
  rename(temp_C_sd = temp_C)

#plot CV just to see how it looks
plot2 = ggplot(hobo_sd, aes(x = date, y = temp_C_sd, group = site, colour = site))+
  geom_line(size = 1)+
  theme_bw()
plot2

#zooming into one season just for funsies 
years = seq(2006,2016,1)

for (i in 1:length(years)){

season <- hobo_sd %>%
  filter(year(date) == years[i])

plot3 = ggplot(season, aes(x = date, y = temp_C_sd, group = site, colour = site))+
  geom_line(size = 1)+
  ggtitle(year(season$date[1]))+
  theme_bw()
plot3

filename = paste0("./Figs/HOBO_Temp_SD_",years[i],".png")

ggsave(plot3, filename = filename, device = "png")

}

hobo_sd1 <- hobo_sd %>% mutate(Year = as.factor(year(date)))

plot4 = ggplot(hobo_sd1, aes(x = Year, y = temp_C_sd, fill = site))+
  geom_boxplot()+
  theme_bw()
plot4
ggsave(plot4, filename = "./Figs/HOBO_Temp_boxplot.png", device = "png",
       width = 8, height = 4, units = "in")
