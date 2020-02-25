#reading data
kisumu<-read.csv("./data/Kisumu data.csv")

#selecting columns to work with
kisumu<-kisumu[,c(5,6,7,13,14,15,17,18,19,20,21,22,23,26,28,29,30,31,32,33,34)]

#summary statistics
(sum(kisumu$DwellingsWithPipedWaterOnPlot,na.rm = T)/sum(kisumu$Dwellings))*100
(sum(kisumu$DwellingsWithWaterSourceOnPlot)/sum(kisumu$Dwellings))*100
(sum(kisumu$DwellingsWithFlushToilets)/sum(kisumu$Dwellings))*100
(sum(kisumu$DwellingsWithOtherImproved)/sum(kisumu$Dwellings))*100
(sum(kisumu$DwellingsWithUnimproved)/sum(kisumu$Dwellings))*100
(sum(kisumu$DwellingsWithOD)/sum(kisumu$Dwellings))*100

#converting water access & sanitation to percentages of total dwellings
# kisumu["Percentage Dwellings with Piped Water On Plot"]<-round((kisumu$DwellingsWithPipedWaterOnPlot/kisumu$Dwellings)*100,0)
# kisumu["Percentage Dwellings with Water Source On Plot"]<-round((kisumu$DwellingsWithWaterSourceOnPlot/kisumu$Dwellings)*100,0)
# kisumu["Percentage Dwellings with Flush Toilets"]<-round((kisumu$DwellingsWithFlushToilets/kisumu$Dwellings)*100,0)
# kisumu["Percentage Dwellings with Other Improved"]<-round((kisumu$DwellingsWithOtherImproved/kisumu$Dwellings)*100,0)
# kisumu["Percentage Dwellings with Unimproved"]<-round((kisumu$DwellingsWithUnimproved/kisumu$Dwellings)*100,0)
# kisumu["Percentage Dwellings with Open Defecation"]<-round((kisumu$DwellingsWithOD/kisumu$Dwellings)*100,0)

#selecting columns
kisumu<-kisumu[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,22,23,24,25,26,27)]

#replacing NAs with 0s
kisumu[is.na(kisumu)]<-0

#renaming the columns
colnames(kisumu)[c(1,2,3,4,7,8,11,12,13,14)]=c("location Name","Sublocation Name","Area Name",
                                             "Area Type","Water Table Dry","Water Table Rainy",
                                              "Soil Self Supporting","Area Location","Area Topology",
                                              "Population Per KM2"
                                             )
#kisumu<- kisumu[tolower(kisumu$`Area Name`),]
#saving final output
write.csv(kisumu,"./data/kisumu.csv")



#MERGING SHAPEFILE WITH DATA
library(raster)
library(rgdal)

#loading data
kisumu<-read.csv("./data/kisumu.csv")
#kisumu<- kisumu[tolower(kisumu$Area.Name),]
#selecting columns
# kisumu<-kisumu[,c(3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)]
#loading shape file
lias<-readOGR(dsn = path.expand("./LIAS"),
              layer="KIWASCO_Low_Income_Areas")
# ksm<-as.data.frame(lias)
#toupper(lias@data$Name)

lias1<-merge(lias,kisumu,by.x="Name",by.y="Area.Name")

#projecting the coordinates to default CRS
lias2<-spTransform(lias1, CRS("+proj=longlat +datum=WGS84"))
shapefile(lias2,"./LIAS/kisumu.shp")
lias2<-lias2[!is.na(lias2$location.Name),]

#Remove NAs 
unknown<-lias2[is.na(lias2$location.Name),]
# unknown<-as.data.frame(unknown)
# class(unknown)
# 
# unknown[is.na(unknown)]<-0
shapefile(unknown,"./LIAS/unknown.shp")

