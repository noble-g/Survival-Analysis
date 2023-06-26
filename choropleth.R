library(naijR)
#library(maps)
#library(maptools)
#library(tmap)
#library(tmaptools)
#library(sf)
#library(dplyr)

##### using the naijR package
#map of states in a particular gpz
map_ng(states(gpz = "ss"))
#list of states in a particular region
states(gpz = "ss")


##to clear the plot pane,use ...
dev.off()

map_ng(
  region = ss,
  title ="south south zone of respondents",
  fill = TRUE,
  col = 15)



#how do i fit my regional data in?
zone_data<- c(82,3,7,11,12,92)
zone_df<- data.frame("ne"= 82,"nc" = 3,"nw" = 7,"ss" = 11, "se" = 12, "sw" = 92)


#create a list of all states by their respective gpz
gpz<-c("ne","nc","nw","ss","se","sw")
##shishi, i don't understand from here, and i want to implement my data
statelist<- 
  lapply(gpz,function(x){
    data.frame(State = states(gpz = x), GPZ = toupper(x))
  })
gpz_df<- Reduce(rbind, statelist)
gpz_df<- rbind(gpz_df, c("Federal Capital Territory","FCT"))
map_ng(data = gpz_df,x = GPZ, 
       title = "regional distribution of responses",col = "YlOrBr",
       leg.title = "legend")
# the beautiful coloring above was from the package "RcolorBrewer"

zone_data<- c(82,3,7,11,12,92)

vignette("nigeria-maps")



####---------------------###########
st_drivers()
m<- st_read("C:/Users/User/Desktop/final project/new_lga_nigeria_2003.kml", copy = TRUE, )
m

inner_join(m,zone_data)
