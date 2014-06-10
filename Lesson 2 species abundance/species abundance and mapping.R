rm(list=ls())

################################### load dataframe from data folder ############################################
# get your current working directory
getwd()

# set new working directory
setwd("C:/Users/Remi-Work/Documents/GitHub/R_tutorial_ocean/Lesson 2 species abundance")

# check the contents of your working directory
list.files(getwd())

# easiest method is to load a .csv
test_csv <- read.csv("data/Master Data Sheet.csv")


# If you you really want to load a MS Excel file read on, otherwise skip to the next section

# loading Excel files requires the installation of a special package
install.packages("XLConnect")            # this command installs the package "XLConnect"
install.packages("rJava")                # the above package is depedent on this package
# the rJava package depends on Java, and on my computer, I need to set the path to Java
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7')

# alternatively, you can install from the "Packages" tab in RStudio (bottom right)

# after you have succesfully installed the package, you need to load it before using
require(XLConnect)                       # this loads the packages, making it available
require(rJava)

# load older Excel files (.xls)
test_xls <-  readWorksheetFromFile("data/Master Data Sheet.xls", sheet=1)   # this should load the first worksheet
# or the newer ones (.xlsx)
test_xlsx <-  readWorksheetFromFile("data/Master Data Sheet.xlsx", sheet=1)   # this should load the first worksheet

# by now, there is a good chance you will have encountered an error loading the file or the package
# there are other ways of trying to open and excel file (packages xlsx, gdata, etc)
# but none that work consistently on all operating systems
# converting from Excel format to .csv (comma separated values) is as simple as clicking "save as" in Excel and selecting the right format

################################### manipulating dataframes: merging ############################################
rm(list=ls())      # re-clear the data

# load the species data
species_data <- read.csv("data/Master Data Sheet.csv")
View(species_data)

# load the site data
site_data <- read.csv("data/site locations.csv")
View(site_data)

# you will notice that the names in species_data$location and site_data$site_name
unique(species_data$location)             #this command returns all the unique values
unique(site_data$site_name)

unique(species_data$location)==unique(site_data$site_name)  # check if they match up

# make new dataframe by merging the species and site data by location/site_name
data <- merge(species_data,site_data, by.x="location",by.y="site_name")
View(data)


###################################### calculate seawater density #############################################
install.packages("oce")       # install this great package for analysis of oceanographic data
require("oce")

# calculate density
swRho(data$site_salinity_psu,data$buoy_SST, pressure = 10, eos = "teos", data$Longitude, data$Latitude)

################################### manipulating dataframes: summary stats ############################################
install.packages("plyr")       # install this great package for splitting, applying and combining data
require(plyr)                  # load the package

# calculate mean, sd, or se one at a time
# by location
Sb_mean <- tapply(data$pc_und_Semibalanus_balanoides,INDEX=data$location, mean, na.rm = TRUE)
Sb_sd <- tapply(data$pc_und_Semibalanus_balanoides,INDEX=data$location, sd, na.rm = TRUE)
Sb_se <- tapply(data$pc_und_Semibalanus_balanoides,INDEX=data$location, sd, na.rm = TRUE)/sqrt(count(data,vars="location")$freq)

# plot
mp <- barplot(Sb_mean,ylim=c(0,1.5))              # plots the barplot and saves the midpoints in mp
segments(mp, Sb_mean + Sb_se, mp,Sb_mean, lwd=2)  # plots positive error bar centered on mp
segments(mp - 0.1, Sb_mean + Sb_se, mp + 0.1, Sb_mean + Sb_se, lwd=2)  #plots error bar caps
title(ylab=expression("Abundance (ind. m"^{2}~")"))


# by location and strata
Sb_mean <- tapply(data$pc_und_Semibalanus_balanoides,INDEX=list(data$location,data$strata), mean, na.rm = TRUE)
Sb_sd <- tapply(data$pc_und_Semibalanus_balanoides,INDEX=list(data$location,data$strata), sd, na.rm = TRUE)
Sb_se <- tapply(data$pc_und_Semibalanus_balanoides,INDEX=list(data$location,data$strata), sd, na.rm = TRUE)/sqrt(count(data,vars=c("location","strata"))$freq)


########################################### creating custom functions #################################################
# lets calculate the same for multiple species, but first lets create 
# our own custom function to calculate standard Errors to make things easier
stdErr <- function(x) {
  x2=x[is.na(x)==F]              # remove the NA's
  sd(x2)/ sqrt(length(x2))       # calculate SE
  } 
# the new function called "stdErr" will calculate SE for vector x
# and is delimited by the curly brackets

# lets test the new function, create a vector:
test=c(3,4,5,6,1,2)
# the old way
sd(test)/sqrt(6)
# or with the new function
stdErr(test)

################################### manipulating dataframes: summary stats pt2 ############################################

# by location and strata, multiple species in 1 data frame
head(data)
data2 <- ddply(data,.(location,strata),summarize,
               Sb_mean=mean(pc_und_Semibalanus_balanoides,na.rm = TRUE),
               Sb_sd=sd(pc_und_Semibalanus_balanoides,na.rm = TRUE),
               Sb_se=stdErr(pc_und_Semibalanus_balanoides),
               Fc_mean=mean(pc_can_Fucus,na.rm = TRUE),
               Fc_sd=sd(pc_can_Fucus,na.rm = TRUE),
               Fc_se=stdErr(pc_can_Fucus),
               As_mean=mean(pc_can_Ascophyllum,na.rm = TRUE),
               As_sd=sd(pc_can_Ascophyllum,na.rm = TRUE),
               As_se=stdErr(pc_can_Ascophyllum)
)

########################################### making a basic site map ######################################################
# install new mapping packages
install.packages("marmap")
install.packages("maps")
install.packages("mapdata")

require(maps)
require(mapdata)
# setlat/long limits
xlim=c(-67,-62)
ylim=c(43,46)

# plot basic map
map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map
map.axes()                                                                        # add axes
map.scale(relwidth=0.5)                                                           # add scale bar proportioned to 50% width
points(site_data$Longitude,site_data$Latitude,pch=16,cex=1.5)                       # add site locations


#################################### plot map with relative abundance ###################################################
Nl_mean <- tapply(data$abun_Nucella_lapillus,INDEX=data$location, mean, na.rm = TRUE)

map("worldHires", xlim=xlim, ylim=ylim, col="gray90", fill=TRUE, resolution=0)    # make base map
map.axes()                                                                        # add axes
map.scale(relwidth=0.5)                                                           # add scale bar proportioned to 50% width
for(site in 1:length(Nl_mean)){
  size=log10(Nl_mean[site]+1)*10                                                                     # calculate symbol size
  points(site_data$Longitude[site],site_data$Latitude[site],pch=16,cex=size)                       # add site locations
}

####################################### plot with bathymetry #####################################################
require(marmap)
#download the bathymetric data
NS <- getNOAA.bathy(lon1 = xlim[1], lon2 = xlim[2], lat1 = ylim[1], lat2 = ylim[2],resolution = 1)

#plot it!
plot.bathy(NS,
           image = TRUE,
           deep=c(-1000,0),
           shallow=c(0,0),
           lwd=c(1,1),
           lty=c(1,1),
           step = 30
           )
points(site_data$Longitude,site_data$Latitude,pch=16,cex=1.5)                       # add site locations

