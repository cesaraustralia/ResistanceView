library(plyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(dismo)
wd <- 'C:/Users/james/Dropbox (Personal)/My Manuscripts/RLEM/paper 2/2. Global Ecology and Biogeography/R/data'
setwd(wd)

sdat<-read.csv('RLEM_paddock insecticide history info_April 2015.csv', stringsAsFactors=FALSE, skip = 0)

######################## OPTIONS ##############################
regions <- list(SE_Aus=c('VIC','NSW', 'SA'), WA=c('WA'))
# regions <- list(VIC='VIC',NSW='NSW',SA= 'SA', WA='WA')
###############################################################
head(sdat)

# fill row labels down
curstate<-sdat$State[1]
curreg <- sdat$Region[1]
cursite <- sdat$Site[1]
for (i in 1:nrow(sdat)){
  # state
  if (sdat$State[i] == ""){
    sdat$State[i] = curstate
  } else {
    curstate = sdat$State[i]
  }
  # region
  if (sdat$Region[i] == ""){
    sdat$Region[i] = curreg
  } else {
    curreg = sdat$Region[i]
  }
  # site
  if (sdat$Site[i] == ""|is.na(sdat$Site[i])){
    sdat$Site[i] = cursite
  } else {
    cursite = sdat$Site[i]
  }
}
head(sdat)
sd<-sdat

# make standardised chem list
chems<-c("bifenthrin" ,
         "chlorpyrifos", 
         "cyhalothrin",
         "cypermethrin", 
         "dimethoate"  ,
         "endosulfan" ,
         "fipronil" ,
         "gammacyhalotthin",
         "imidacloprid"  ,
         "methidathion", 
         "nilspraysapplied", 
         "omethoate",
         "paraffinic oil",
         "phosmet",
         "sulfoxaflor",
         "thiamethoxam")

# make standardised crop list
crops<-c( "pasture",  "wheat",   
          "canola",  "barley" , 
          "lupin" ,   "oats"   , 
          "vetch" ,   "lucerne" ,
          "cereal",   "ryegrass")

# standardise chemical treatment names
tr = tolower(unlist(sd[,7:16]))
tr1 = tr
for (chem in chems){
  tr1[grepl(chem, tr)]<-chem
}
tr1[grepl('pyrifos', tr1)]<-"chlorpyrifos" # catch remainder
tr1[grepl('bifethrin 200ml/ha', tr1)]<-"bifenthrin" # catch remainder
unique(tr1)

# standardise crop names
cr = tolower(unlist(sd$Croppinghistory))
for (crop in crops){
  cr[grepl(crop, cr)]<-crop
}
unique(cr)

tr2<-array(tr1,c(nrow(sd),length(7:16)))
tr2[is.na(tr2)]<-''
spraytimes<- c('Presowing', 'Presowing', 'Seed_treatment',	'Presowing', 'Presowing', 'Presowing',		'Seedling_emergence',	'Seedling_emergence',		'Other_sprays',	'Other_sprays')
colnames(tr2)<-spraytimes
d<-array('',c(nrow(tr2),length(unique(spraytimes))))
colnames(d)<-unique(spraytimes)

# combine multiple sprays into one string that is alphabetically sorted
for(spray_type in unique(spraytimes)){
  cols<- which(spraytimes==spray_type)
  for (i in 1:nrow(d)){
    si=''
    ichems<-tr2[i,cols]
    ichems<-ichems[order(ichems)]
    for (j in 1:length(ichems)){
      if (ichems[j]!=''&si!=''){
        si <-paste(si,ichems[j], sep = '.')
      }else{
        si <-paste0(si,ichems[j])
      }
    } 
    d[i,spray_type]<-si
  }
}

df<-data.frame(d)
df$crop <- cr
df$year   <- sdat$Year
df$state  <- sdat$State
df$region <- sdat$Region
df$lat    <- NA
df$long   <- NA
df$NorthSouth <-NA

# estimate longlat for each region and insert in df
geo<-geocode(paste0(unique(paste0(sdat$Region,', ',sdat$State)),', Australia'))
geo$region <- sapply(strsplit(sapply(geo$originalPlace,'toString'),','),function(x) x[1])
for(region in unique(sdat$Region)){
  df$lat [df$region==region] <- geo$latitude [geo$region==region]
  df$long[df$region==region] <- geo$longitude[geo$region==region]
}
df$NorthSouth <- ifelse(df$lat < -32, 'South', 'North')

head(df)
df[]<-lapply(df,as.character)
df[df=='']<-'nil sprays'
df$grower<-1:nrow(df)
df.long <- melt(df, id.vars = c('grower','crop','year','state','region','lat','long','NorthSouth'), factorsAsStrings = TRUE) 
head(df.long)
unique(as.vector(df.long$value))
unique(c(as.matrix(df[,1:4])))
names(df.long)[9:10]<-c('period','chem')
df.long[]<-lapply(df.long,as.character)

#count SP sprays
df.long$SPcount<-NA
df.long$myRegion <- NA
for(i in 1:nrow(df.long)){
  # count SPs applied
  SPcount = 0
  for(chem in c('cyhalothrin','cypermethrin','bifenthrin')){
      SPcount = SPcount + sum(strsplit(df.long$chem[i], '[.]')[[1]]%in%chem)
  }
  df.long$SPcount[i]<-SPcount
  # pool state to regions
  for (j in 1:length(regions)){
      if(df.long$state[i]%in%regions[[j]]) df.long$myRegion[i] <-names(regions[j])
  }
}

crop2com<-matrix(c("pasture", 8,
                   "wheat", 24,
                   "canola", 26,
                   "barley", 24,
                   "lupin", 25,
                   "oats", 24,
                   "vetch", 8,
                   "lucerne", 8,
                   "cereal", 24,
                   "ryegrass", 8),ncol=2,byrow=TRUE)
fromCrop <- crop2com[,1]
toCom <-  crop2com[,2]
df.long$commod <- as.numeric(mapvalues(df.long$crop, from = fromCrop, to = toCom))
spray_rates_grower<-ddply(df.long,c('grower', 'myRegion', 'commod'),summarise, 
                                      SPcount  = sum(SPcount)
                                    )

sprayrates<-ddply(spray_rates_grower,c('myRegion', 'commod'),summarise, 
      mean.sprays = mean(SPcount),
      se.sprays = sd(SPcount)/sqrt(length((SPcount))),
      N = length((SPcount))
      )

# # the below is how I am currently calculating so will be interesting to compare methods
# sprayrates<-read.csv(file = 'C:/Users/james/Dropbox/Programming/R/MetaPopGen/git/RLEM-resistance-model/land_usage_data/SprayRates.csv') 
# sprayrates$myRegion <- NA
# for(i in 1:nrow(sprayrates)){
#   # pool state to regions
#   for (j in 1:length(regions)){
#     if(sprayrates$state[i]%in%regions[[j]]) sprayrates$myRegion[i] <-names(regions[j])
#   }
# }
# ddply(sprayrates,c('myRegion', 'commod'),summarise, 
#       mean.sprays = mean(apps_per_year),
#       N = length(SPcount)
# )
# 


# add layers for annual sprays in each chem group  
# these layer were converted from the orginal projection in 'C:\Users\james\Dropbox\Programming\R\MetaPopGen\git\RLEM-resistance-model\land_usage_data\land_usage.R'
load('./commodity_layer10.Rdata')
load('./commodity_layer05.Rdata')

# map spray rates for east australia and west aus
fromWA<-1:26
toWA  <-rep(0,26)
toWA[sprayrates$commod[5:8]]<-  sprayrates$mean.sprays[5:8]
toWA[12]<-sprayrates$mean.sprays[5] # set pasture seed to pasture spray rate
fromEA<-1:26
toEA  <-rep(0,26)
toEA[sprayrates$commod[1:4]]<-  sprayrates$mean.sprays[1:4]
toEA[12]<-sprayrates$mean.sprays[1] # set pasture seed to pasture spray rate


r<-r_com10
r[]<-0
SP10WA<-r
SP10EA<-r
SP05WA<-r
SP05EA<-r

SP10WA[]<-mapvalues(r_com10[],from=fromWA, to = toWA)
SP10EA[]<-mapvalues(r_com10[],from=fromEA, to = toEA)
SP05WA[]<-mapvalues(r_com05[],from=fromWA, to = toWA)
SP05EA[]<-mapvalues(r_com05[],from=fromEA, to = toEA)

# WA border at x = 129
xy <- xyFromCell(SP10WA, cell =1:SP10WA@ncols*SP10WA@nrows)
SP10<-SP10WA
SP10[xy[,1]>129]<-SP10EA[xy[,1]>129]

WA_col<-colFromX(SP10WA,129)
SP10<- SP10EA
SP10[,1:WA_col]<-SP10WA[,1:WA_col]
SP05<- SP05EA
SP05[,1:WA_col]<-SP05WA[,1:WA_col]

SP_hires<-(SP05+SP10)/2
  
save(SP_hires,file='./SP_hires.Rdata')

