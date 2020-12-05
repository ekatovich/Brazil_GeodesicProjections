################################################################
#Construct Orthogonal and Parallel Coastal Projections for 
#Brazilian Municipalities, Used in Oil & Gas Royalty Distribution
################################################################

#By: Erik Katovich
#Last modified: 9/1/2020
#Contact: Github: ekatovich
#         Email: ekatovich@wisc.edu

#Does: This R script reconstructs the geodesic (orthogonal and parallel) coastal projections of
#municipal boundaries implemented by the Institudo Brasileiro de Geografia e Estatistica for 
#the purposes of determining offshore oil and gas royalty distributions to affected municipalities.

#The script also plots every offshore oil well registered with the National Oil Agency (ANP) since 
#2000 and allocates each well to the municipalities that are aligned parallel or orthogonal to it.

#All procedures (and special exceptions) implemented in this script are designed to replicate
#the specifications made in the documents included in the accompanying folder called "Documents." 
#All attempts have been made to make these projections as true to the official IBGE projections 
#as possible, but slight inaccuracies persist and the results of this script should not be considered
#as reflecting official boundaries or distributions.

#Latitude and longitude of municipal boundary points were identified manually in Tableau, and are subject 
#to minor innaccuracies. Projection lengths are specified manually in the 
# Coastal_Coordinates_Municipalities.csv file. These are also subject to minor inprecisions. 


################################################################
#1. Setup
################################################################

#First, copy the "Brazil_GeodesicProjections" folder to the desired place on your computer.

#Clear workspace by removing all variables from local environment
rm(list=ls())

#Set working directory 
#Adjust this file path to the location where you have saved the Brazil_GeodesicProjections file on your computer
setwd("C:/Users/17637/Documents/GitHub/Brazil_GeodesicProjections")

#Load libraries
library(geobr)
library(ggplot2)
#library(gganimate)
#library(gifski)
library(sf)
library(sp)
library(dplyr)
library(rio)
library(geosphere)
library(foreign)
library(magick)
library(spData)
library(maptools)
library(ptinpoly)
library(ggsflabel)
library(data.table)

#####################
#Import required data
#####################
#Use GeoBR package to download required shapefiles (requires internet connection)
# Download all states in Brazil
state <- read_state(year=2010)
# Download all municipalities in Brazil
muni <- read_municipality(year=2010)

#Coastal line
coastal_line <- st_read("Inputs/Shapefiles/Linha_de_Costa_2018_20190308.shp")

#Continental limit
continental_limit <- st_read("Inputs/Shapefiles/lim_outros_limites_oficiais_l.shp")

#IBGE coastal line points
coastal_points <- read.csv("Inputs/Data/Coastal_Line_Coordinates_States.csv")
coastal_munic <- st_read("Inputs/Shapefiles/Mun_Linha_de_Costa_2018_20190308_Atrib.shp")

#Municipal boundary points and angles
munic_points <- read.csv("Inputs/Data/Coastal_Coordinates_Municipalities.csv")

#####################
#Import optional data
#####################
#These datasets are not required in this script, but may be added to maps for illustrative purposes.

#Pre-sal polygon
pre_sal_polygon <- st_read("Inputs/Shapefiles/Poligono_Pre_Sal.shp")

#Bathimetric curves 
bath_curves <- st_read("Inputs/Shapefiles/rel_curva_batimetrica_l.shp")

#Oil fields
production_fields <- st_read("Inputs/Shapefiles/Campos_de_Producao.shp")

######################################
# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())

################################################################
#2. Wells Data
################################################################

#Import wells data
wells <- read.csv("Inputs/Data/Wells_Table.csv", header = TRUE, sep = ",")

#Clean latitude and longitude points for mapping
wells$LATITUDE_BASE_DD <- as.character(wells$LATITUDE_BASE_DD)
wells$LONGITUDE_BASE_DD <- as.character(wells$LONGITUDE_BASE_DD)

wells$LATITUDE_BASE_DD <- gsub(",",".", wells$LATITUDE_BASE_DD) 
wells$LONGITUDE_BASE_DD <- gsub(",",".", wells$LONGITUDE_BASE_DD) 

wells$LATITUDE_BASE_DD <- as.numeric(wells$LATITUDE_BASE_DD)
wells$LONGITUDE_BASE_DD <- as.numeric(wells$LONGITUDE_BASE_DD)

#Extract year of beginning, end, and conclusion of drilling
wells$INICIO <- as.character(wells$INICIO)
wells$CONCLUSAO <- as.character(wells$CONCLUSAO)
wells$TERMINO <- as.character(wells$TERMINO)
wells[which(wells$INICIO==""),'INICIO'] <- "NA/NA/NA"
wells[which(wells$CONCLUSAO==""),'CONCLUSAO'] <- "NA/NA/NA"
wells[which(wells$TERMINO==""),'TERMINO'] <- "NA/NA/NA"
inicio <- strsplit(wells$INICIO, '/')
inicio <- do.call('rbind', inicio)
inicio <- strsplit(inicio[,3], " ")
inicio <- do.call('rbind', inicio)
conclu <- strsplit(wells$CONCLUSAO, '/')
conclu <- do.call('rbind', conclu)
conclu <- strsplit(conclu[,3], " ")
conclu <- do.call('rbind', conclu)
term <- strsplit(wells$TERMINO, '/')
term <- do.call('rbind', term)
term <- strsplit(term[,3], " ")
term <- do.call('rbind', term)

wells$Begin_Year <- as.numeric(as.character(inicio[,1]))
wells$Conclusion_Year <- as.numeric(as.character(conclu[,1]))
wells$End_Year <- as.numeric(as.character(term[,1]))

#Categorize wells by "Type": Development, Exploratory, or Other
wells[wells$TIPO=="Explotatório", "Type"] <- "Development"
wells[wells$TIPO=="Exploratório", "Type"] <- "Exploratory"
wells[wells$TIPO=="", "Type"] <- "Other"

#Categorize wells into "Successfull" and "Unsuccessfull" by "REclassificacao"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR ACIDENTE MECÂNICO ", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR ERUPÇÃO ", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR IMPOSSIBILIDADE DE AVALIAÇÃO", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR OBJETIVO FORA DE PREVISÃO", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR OBJETIVO/ALVO NÃO ATINGIDO", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR OUTRAS RAZÕES", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="ABANDONADO POR PERDA CIRCULAÇÃO", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="CONTROLE DE ERUPÇÃO", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="DESCARTE DE ÁGUA", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE CAMPO COM GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE CAMPO COM GÁS NATURAL E CONDENSADO ", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE CAMPO COM PETRÓLEO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE CAMPO COM PETRÓLEO E GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE CAMPO COM PETRÓLEO, GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE NOVA JAZIDA GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE NOVA JAZIDA GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE NOVA JAZIDA PETRÓLEO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE NOVA JAZIDA PETRÓLEO E GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="DESCOBRIDOR DE NOVA JAZIDA PETRÓLEO, GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="EXPERIMENTAL", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="EXTENSÃO PARA GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="EXTENSÃO PARA GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="EXTENSÃO PARA PETRÓLEO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="EXTENSÃO PARA PETRÓLEO E GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="EXTENSÃO PARA PETRÓLEO, GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="INDEFINIDO", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="INJEÇÃO DE ÁGUA", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="INJEÇÃO DE ÁGUA ADITIVADA ", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="INJEÇÃO DE CO2", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="INJEÇÃO DE GÁS NATURAL", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="INJEÇÃO DE QUALQUER OUTRO FLUIDO", "Success"] <- "Other" 
wells[wells$RECLASSIFICACAO=="INJEÇÃO DE VAPOR", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="OBSERVAÇÃO", "Success"] <- "Other" 
wells[wells$RECLASSIFICACAO=="OUTRAS FINALIDADES", "Success"] <- "Other"
wells[wells$RECLASSIFICACAO=="PESQUISA MINERAL", "Success"] <- "Other" 
wells[wells$RECLASSIFICACAO=="PORTADOR DE GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PORTADOR DE GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PORTADOR DE PETRÓLEO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PORTADOR DE PETRÓLEO E GÁS NATURAL", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PORTADOR DE PETRÓLEO, GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PRODUÇÃO DE ÁGUA", "Success"] <- "Other" 
wells[wells$RECLASSIFICACAO=="PRODUTOR COMERCIAL DE GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PRODUTOR COMERCIAL DE GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PRODUTOR COMERCIAL DE PETRÓLEO", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PRODUTOR COMERCIAL DE PETRÓLEO E GÁS NATURAL", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PRODUTOR COMERCIAL DE PETRÓLEO, GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PRODUTOR SUBCOMERCIAL DE GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PRODUTOR SUBCOMERCIAL DE GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PRODUTOR SUBCOMERCIAL DE PETRÓLEO", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="PRODUTOR SUBCOMERCIAL DE PETRÓLEO E GÁS NATURAL", "Success"] <- "Successfull"
wells[wells$RECLASSIFICACAO=="PRODUTOR SUBCOMERCIAL DE PETRÓLEO, GÁS NATURAL E CONDENSADO", "Success"] <- "Successfull" 
wells[wells$RECLASSIFICACAO=="SECO COM INDÍCIOS DE GÁS NATURAL E CONDENSADO", "Success"] <- "Unsuccessfull" 
wells[wells$RECLASSIFICACAO=="SECO COM INDÍCIOS DE PETRÓLEO", "Success"] <- "Unsuccessfull"
wells[wells$RECLASSIFICACAO=="SECO COM INDÍCIOS DE PETRÓLEO E GÁS NATURAL", "Success"] <- "Unsuccessfull" 
wells[wells$RECLASSIFICACAO=="SECO COM INDÍCIOS GÁS NATURAL", "Success"] <- "Unsuccessfull" 
wells[wells$RECLASSIFICACAO=="SECO SEM INDÍCIOS", "Success"] <- "Unsuccessfull" 
wells[wells$RECLASSIFICACAO=="TREINAMENTO", "Success"] <- "Other" 
wells[wells$RECLASSIFICACAO=="", "Success"] <- "Other" 

#Create new dataset that includes only off-shore wells that were successful and unsuccessfull
#wells_offshore <- wells[which(wells$Success!="Other" & wells$TERRA_MAR == "M"),]
wells_offshore <- wells[which(wells$TERRA_MAR == "M"),]

################################################################
#3. Map 1: Brazilian States and Official Coastal Line
################################################################

ggplot() +
  geom_sf(data=state, color="black", size=.3, show.legend = FALSE) +
  theme_minimal() + no_axis +
  geom_sf(data=bath_curves, fill="orchid", color="lightskyblue2", size=1, show.legend = FALSE)+
  geom_sf(data=continental_limit, color="blue", size=.2, show.legend = FALSE) +
  geom_path(data = coastal_points, aes(x = longitude, y = latitude), colour = "red", size = 1.5)+
  geom_point(data = coastal_points, aes(x = longitude, y = latitude), size = 2,shape = 21, fill="yellow", color="black")+
  geom_spoke(data = coastal_points, aes(x = longitude, y = latitude, angle = radians, radius = length), colour = "red")+
  ggsave("Outputs/Maps/CoastalLine_and_StateBoundaries.pdf")


#######################################################################################
#4. Compute orthogonal angles from municipal boundary points
#######################################################################################


# Calculate slope perpendicular to coastal line for each stretch between coastal points
coastal_points$lead_lat <- lead(coastal_points$latitude, n=1L)
coastal_points$lead_lon <- lead(coastal_points$longitude, n=1L)
coastal_points$slope <- (coastal_points$latitude-coastal_points$lead_lat)/(coastal_points$longitude-coastal_points$lead_lon)
coastal_points$perp_slope <- -1/coastal_points$slope
#Designate state segments which use different stretches of official coastal line to determine orthogonal angles
coastal_points[1,'state'] <- "AP"
coastal_points[2,'state'] <- "PA"
coastal_points[3,'state'] <- "MA1"
coastal_points[4,'state'] <- "MA2"
coastal_points[5,'state'] <- "PI"
coastal_points[6,'state'] <- "CE1"
coastal_points[7,'state'] <- "CE2"
coastal_points[8,'state'] <- "RN1"
coastal_points[9,'state'] <- "RN2"
coastal_points[10,'state'] <- "PB"
coastal_points[11,'state'] <- "PE"
coastal_points[12,'state'] <- "AL"
coastal_points[13,'state'] <- "SE"
coastal_points[14,'state'] <- "BA1"
coastal_points[15,'state'] <- "BA2"
coastal_points[16,'state'] <- "ES"
coastal_points[17,'state'] <- "RJ1"
coastal_points[18,'state'] <- "RJ2"
coastal_points[19,'state'] <- "SP1"
coastal_points[20,'state'] <- "SP2"
coastal_points[21,'state'] <- "PR"
coastal_points[22,'state'] <- "SC1"
coastal_points[23,'state'] <- "SC2"
coastal_points[24,'state'] <- "RS"
coastal_perp <- coastal_points[, c('state', 'perp_slope', 'slope')]

#Convert from slope to radians using arctangent
munic_points$state <- as.character(munic_points$state)
munic_points$state <- gsub(" ", "", munic_points$state)
munic_points_2 <- merge(munic_points, coastal_perp, by=c('state'), all.x=T) 
munic_points_2$radians_orthogonal <- atan(munic_points_2$perp_slope)
#Flip perpendicular for CE1
munic_points_2[which(munic_points_2$state == "CE1"), "radians_orthogonal"] = munic_points_2[which(munic_points_2$state == "CE1"), "radians_orthogonal"]*pi 

#Correct for special case in northern Rio, where orthogonal projections do not obey orthogonal rule
munic_points_2[which(munic_points_2$point == 82), "radians_orthogonal"] = -.05
munic_points_2[which(munic_points_2$point == 81), "radians_orthogonal"] = -.05
munic_points_2[which(munic_points_2$point == 80), "radians_orthogonal"] = -.9
munic_points_2[which(munic_points_2$point == 79), "radians_orthogonal"] = -.94
munic_points_2[which(munic_points_2$point == 262), "radians_orthogonal"] = -.84
munic_points_2[which(munic_points_2$point == 78), "radians_orthogonal"] = -.8
munic_points_2[which(munic_points_2$point == 329), "radians_orthogonal"] = -.7
#munic_points_2[which(munic_points_2$point == 72), "radians_orthogonal"] = -.7
munic_points_2[which(munic_points_2$point == 73), "radians_orthogonal"] = -.7
munic_points_2[which(munic_points_2$point == 74), "radians_orthogonal"] = -.7
munic_points_2[which(munic_points_2$point == 76), "radians_orthogonal"] = -.7
munic_points_2[which(munic_points_2$point == 77), "radians_orthogonal"] = -.7


#Correct for special case in southern Rio, where orthogonal projections do not obey orthogonal rule
munic_points_2[which(munic_points_2$point == 301), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 60), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 66), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 261), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 260), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 70), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 293), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 67), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 69), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 263), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 68), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 75), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 71), "radians_orthogonal"] = -1.48
munic_points_2[which(munic_points_2$point == 328), "radians_orthogonal"] = -1.48

#Correct for special case in Piaui
munic_points_2[which(munic_points_2$point == 211), "radians_orthogonal"] = 1.25
munic_points_2[which(munic_points_2$point == 213), "radians_orthogonal"] = 1.17

#Insert angles of state boundary munic lines
#This step fills in gaps in coverage at beginning and end of each state segment
munic_points_2[which(munic_points_2$point == 294), "radians_orthogonal"] = -0.613700752
munic_points_2[which(munic_points_2$point == 295), "radians_orthogonal"] = -0.56
munic_points_2[which(munic_points_2$point == 296), "radians_orthogonal"] = -0.613700752
munic_points_2[which(munic_points_2$point == 297), "radians_orthogonal"] = -0.999911468
munic_points_2[which(munic_points_2$point == 298), "radians_orthogonal"] = -0.66
munic_points_2[which(munic_points_2$point == 299), "radians_orthogonal"] = 0.313365385
munic_points_2[which(munic_points_2$point == 300), "radians_orthogonal"] = 1.111322362
munic_points_2[which(munic_points_2$point == 302), "radians_orthogonal"] = -0.459457542
munic_points_2[which(munic_points_2$point == 303), "radians_orthogonal"] = -0.66
munic_points_2[which(munic_points_2$point == 304), "radians_orthogonal"] = -0.56
munic_points_2[which(munic_points_2$point == 305), "radians_orthogonal"] = -0.044260904
munic_points_2[which(munic_points_2$point == 306), "radians_orthogonal"] = -0.440912965
munic_points_2[which(munic_points_2$point == 307), "radians_orthogonal"] = 0.313365385
munic_points_2[which(munic_points_2$point == 308), "radians_orthogonal"] = -0.044260904
munic_points_2[which(munic_points_2$point == 309), "radians_orthogonal"] = -0.304993756
munic_points_2[which(munic_points_2$point == 310), "radians_orthogonal"] = 1.111322362
munic_points_2[which(munic_points_2$point == 311), "radians_orthogonal"] = -0.304993756
munic_points_2[which(munic_points_2$point == 312), "radians_orthogonal"] = -0.440912965
munic_points_2[which(munic_points_2$point == 313), "radians_orthogonal"] = -0.71813286
munic_points_2[which(munic_points_2$point == 314), "radians_orthogonal"] = 0.781344427
munic_points_2[which(munic_points_2$point == 315), "radians_orthogonal"] = 0.781344427
munic_points_2[which(munic_points_2$point == 213), "radians_orthogonal"] =  1.095480444
munic_points_2[which(munic_points_2$point == 317), "radians_orthogonal"] =   1.25
munic_points_2[which(munic_points_2$point == 234), "radians_orthogonal"] =   1.095480444




#Plot to check orthogonal lines
ggplot() +
  geom_sf(data=coastal_munic, color="black", size=.001, show.legend = FALSE) +
  theme_minimal() + no_axis +
  geom_sf(data=continental_limit, color="blue", size=.2, show.legend = FALSE) +
  #geom_point(data = munic_points, aes(x = longitude, y = latitude), size = .005, shape = 16, fill="yellow", color="yellow")+
  geom_spoke(data = munic_points_2, aes(x = longitude, y = latitude, angle = radians_orthogonal, radius = length_orthogonal, colour = state), size=.005)+
  #geom_spoke(data = munic_points_2, aes(x = longitude, y = latitude, angle = radians_parallel, radius = length_parallel, colour = state), size=.005)+
  geom_spoke(data = coastal_points, aes(x = longitude, y = latitude, angle = radians, radius = length), colour = "red", size=.1)+
  #geom_point(data = coastal_points, aes(x = longitude, y = latitude), size = .02,shape = 21, fill="blue", color="blue")+
  geom_sf(data=coastal_line, color="black", size=.02, show.legend = FALSE)+
  ggsave("Outputs/Maps/Municipalities_Orthogonal_Projections.pdf")

#Plot to check parallel lines
ggplot() +
  geom_sf(data=coastal_munic, color="black", size=.001, show.legend = FALSE) +
  theme_minimal() + no_axis +
  geom_sf(data=continental_limit, color="blue", size=.2, show.legend = FALSE) +
  #geom_point(data = munic_points, aes(x = longitude, y = latitude), size = .005, shape = 16, fill="yellow", color="yellow")+
  #geom_spoke(data = munic_points_2, aes(x = longitude, y = latitude, angle = radians_orthogonal, radius = length_orthogonal, colour = state), size=.005)+
  geom_spoke(data = munic_points_2, aes(x = longitude, y = latitude, angle = radians_parallel, radius = length_parallel, colour = state), size=.005)+
  geom_spoke(data = coastal_points, aes(x = longitude, y = latitude, angle = radians, radius = length), colour = "red", size=.1)+
  #geom_point(data = coastal_points, aes(x = longitude, y = latitude), size = .02,shape = 21, fill="blue", color="blue")+
  geom_sf(data=coastal_line, color="black", size=.02, show.legend = FALSE)+
  ggsave("Outputs/Maps/Municipalities_Parallel_Projections.pdf")


#######################################################################################
#5. Create Orthogonal Polygons
#######################################################################################

#Compute coordinates of endpoints for each municipality boundary point using trigonometric properties
munic_points_2$end_longitude <- munic_points_2$longitude+munic_points_2$length_orthogonal*cos(munic_points_2$radians_orthogonal)
munic_points_2$end_latitude <- munic_points_2$latitude+munic_points_2$length_orthogonal*sin(munic_points_2$radians_orthogonal)

#Divide points into "chunks", each of which will be sorted by latitude or longitude, depending on its orientation
munic_points_2[which(munic_points_2$state=="AP"), 'chunk'] <- 1
munic_points_2[which(munic_points_2$state=="PA"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="MA1"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="MA2"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="PI"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="CE1"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="CE2"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="RN1"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="RN2"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="PB"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="PE"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="AL"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="SE"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="BA1"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="BA2"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="ES"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="RJ1"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="RJ2"), 'chunk'] <- 4
munic_points_2[which(munic_points_2$state=="SP1"), 'chunk'] <- 4
munic_points_2[which(munic_points_2$state=="SP2"), 'chunk'] <- 4
munic_points_2[which(munic_points_2$state=="PR"), 'chunk'] <- 5
munic_points_2[which(munic_points_2$state=="SC1"), 'chunk'] <- 5
munic_points_2[which(munic_points_2$state=="SC2"), 'chunk'] <- 5
munic_points_2[which(munic_points_2$state=="RS"), 'chunk'] <- 5
munic_points_list <- split(munic_points_2, as.factor(munic_points_2$chunk))

#Appropriately sort each chunk by longitude or latitude
for(i in 1:length(munic_points_list)){
  if(i==2|i==4){
    mi <- munic_points_list[[i]]
    mi <- mi[order(mi$longitude),]
    if(length(which(is.na(mi$length_orthogonal)==T))!=0) mi <- mi[-which(is.na(mi$length_orthogonal)==T),]
    
  }else{
    mi <- munic_points_list[[i]]
    if(length(which(is.na(mi$length_orthogonal)==T))!=0) mi <- mi[-which(is.na(mi$length_orthogonal)==T),]
    mi <- mi[order(mi$latitude),]
  }

  #Now that chunks are appropriately sorted, identify the next point down the map for each municipal boundary point
  mi$lead_lat <- lead(mi$latitude, n=1L)
  mi$lead_lon <- lead(mi$longitude, n=1L)
  mi$lead_end_lat <- lead(mi$end_latitude, n=1L)
  mi$lead_end_lon <- lead(mi$end_longitude, n=1L)
  #Fix special cases of chunk edges, where a missing value is created and needs to be filled in
  if(i==2){
    mi[nrow(mi), 'lead_lat'] <- -5.380000
    mi[nrow(mi), 'lead_lon'] <- -35.3300
    mi[nrow(mi), 'lead_end_lat'] <- -3.208982
    mi[nrow(mi), 'lead_end_lon'] <- -32.34625
  } 
  if(i==3){
    mi[1, 'lead_lat'] <- -22.940
    mi[1, 'lead_lon'] <- -42.2800
    mi[1, 'lead_end_lat'] <- -26.39571
    mi[1, 'lead_end_lon'] <- -41.96537
    mi <- rbind(mi, mi[1,])
    mi[nrow(mi), 'longitude'] <- -42.04
    mi[nrow(mi), 'latitude'] <- -22.93
    mi[nrow(mi), 'lead_lat'] <- -22.82
    mi[nrow(mi), 'lead_lon'] <- -41.97
    mi[nrow(mi), 'lead_end_lat'] <- -22.82+3.6*sin(-.7)
    mi[nrow(mi), 'lead_end_lon'] <- -41.97 +3.6*cos(-.7)
    mi[nrow(mi), 'end_latitude'] <- -22.93 +3.6*sin(-.7)
    mi[nrow(mi), 'end_longitude'] <- -42.04 +3.6*cos(-.7)
  } 
  polid <- paste('chunk', i, sep='_')
  mi$pol_ID_orthogonal <- paste(polid, seq(1, nrow(mi), 1), sep='_')
  munic_points_list[[i]] <- mi
  
}

munic_points_3 <- do.call('rbind', munic_points_list)
munic_points_3 <- munic_points_3[-which(is.na(munic_points_3$lead_lat)==T),]
my_polygon_orthogonal <- list()

#Fix special cases where an additional point needs to be added to some polygons.
#First, Campos dos Goytacazes
for(i in 1:nrow(munic_points_3)){
  x <- t(t(as.numeric(c(munic_points_3[i, c('longitude', 'lead_lon','lead_end_lon', 'end_longitude')]))))
  y <- t(t(as.numeric(c(munic_points_3[i, c('latitude', 'lead_lat','lead_end_lat','end_latitude')]))))
  if(munic_points_3[i,"pol_ID_orthogonal"]=="chunk_3_9"){
    new_point_lon <- -40.96+3.58*cos(-0.459457542)
    new_point_lat <- -21.3+3.58*sin(-0.459457542)
    x <- t(t(c(x[1:3], new_point_lon, x[4])))
    y <- t(t(c(y[1:3], new_point_lat, y[4])))
  }
  #Next, Ilha Bela
  if(munic_points_3[i,"pol_ID_orthogonal"]=="chunk_4_13"){
    new_point_lon <- -44.73+3.76*cos(-0.999911468)
    new_point_lat <- -23.37+3.76*sin(-0.999911468)
    x <- t(t(c(x[1:3], new_point_lon, x[4])))
    y <- t(t(c(y[1:3], new_point_lat, y[4])))
  }
  md_orthogonal <- data.frame(cbind(x,y,munic_points_3[i,'pol_ID_orthogonal']))
  md_orthogonal$X1 <- as.numeric(as.character(md_orthogonal$X1))
  md_orthogonal$X2 <- as.numeric(as.character(md_orthogonal$X2))
  #x <- coords2Polygons(md, ID=paste('polygon', i, sep=''))
  my_polygon_orthogonal[[i]] <- md_orthogonal
}


#Combine each point, the next leading point, and each of their corresponding endpoints into a four-sided polygon
df_my_polygon_orthogonal <- do.call('rbind', my_polygon_orthogonal)
colnames(df_my_polygon_orthogonal) <- c('V1', 'V2', 'pol_ID_orthogonal')
ids <- unique(df_my_polygon_orthogonal$pol_ID_orthogonal)
for(g in ids){
  di <- df_my_polygon_orthogonal[which(df_my_polygon_orthogonal$pol_ID_orthogonal==g), c('V1', 'V2')]
  di <- na.omit(di)
  if(nrow(di)==4){
    p1 <- st_point(as.numeric(di[1,]))
    p2 <- st_point(as.numeric(di[2,]))
    p3 <- st_point(as.numeric(di[3,]))
    p4 <- st_point(as.numeric(di[4,]))
    geom <- st_sfc(c(p1,p2,p3,p4))
    df = st_as_sf(geom)
    coord_df <- st_coordinates(st_centroid(df))
    df_my_polygon_orthogonal[which(df_my_polygon_orthogonal$pol_ID_orthogonal==g), 'lon_centroid_orthogonal'] <- coord_df[1]
    df_my_polygon_orthogonal[which(df_my_polygon_orthogonal$pol_ID_orthogonal==g), 'lat_centroid_orthogonal'] <- coord_df[2]
  }
  
  print(g)
}

#Plot orthogonal polygons to check
ggplot() +
  theme_minimal() + no_axis +
  geom_sf(data=coastal_munic, color="black", size=.001, show.legend = FALSE) +
  geom_polygon(data=df_my_polygon_orthogonal, aes(x=V1, y=V2, group=pol_ID_orthogonal), show.legend = FALSE, colour='white', fill='black', size=.001)+
  ggsave("Outputs/Maps/Municipalities_OrthgonalPolygons_Check.pdf")


#######################################################################################
#6. Determine whether each geolocated well falls within which orthogonal polygon
#######################################################################################

wells_offshore <- data.frame(wells_offshore)

all_indicators <- NULL

#Loop through each well and each polygon to identify which wells are in which polygons;
#Use point.in.polygon function
for(i in 1:nrow(wells_offshore)){
  
  wi <- wells_offshore[i,]
  
  all_indicators_r <- NULL
  
  for(j in 1:length(my_polygon_orthogonal)){
    
    pol_i <- my_polygon_orthogonal[[j]]
    pol_i$X3 <- NULL
    is_in_pol <- point.in.polygon(point.x=as.numeric(wi$LONGITUDE_BASE_DD), point.y=as.numeric(wi$LATITUDE_BASE_DD), pol.x = pol_i$X1, pol.y=pol_i$X2)
    all_indicators_r <- cbind(all_indicators_r, as.numeric(is_in_pol))
  }
  
  all_indicators <- rbind(all_indicators, all_indicators_r)
  
  #Print to track progress (should go up to 6779)
  print(i)
}

all_indicators <- data.frame(all_indicators)
colnames(all_indicators) <- unique(df_my_polygon_orthogonal$pol_ID_orthogonal)

#Add polygon id numbers to new data frame
pol_ID_orthogonal <- NULL
for(p in 1:nrow(all_indicators)){
  pol_ID_p <- c(which(all_indicators[p,]!=0))
  pol_ID_number <- colnames(all_indicators)[pol_ID_p]
  if(length(pol_ID_p)!=0){
    pol_ID_p <- cbind(p, pol_ID_number)
    pol_ID_orthogonal <- rbind(pol_ID_orthogonal, pol_ID_p)
  }
  print(p)
}

#Merge orthogonal polygons dataset with municipality points dataset
pol_ID_orthogonal <- data.frame(pol_ID_orthogonal)
colnames(pol_ID_orthogonal) <- c('well_ID','pol_ID_orthogonal')
pol_ID_orthogonal$well_ID <- as.numeric(as.character(pol_ID_orthogonal$well_ID))
wells_offshore$well_ID <- seq(1, nrow(wells_offshore),1)
wells_offshore_with_polygons <- merge(pol_ID_orthogonal, wells_offshore, by=c('well_ID'), all=T)

#Merge in lat and lon of each polygon to label polygons (this will be useful for a future step)
df_my_polygon_orthogonal_withcentroids <- merge(df_my_polygon_orthogonal, munic_points_3, by=c('pol_ID_orthogonal'))


#######################################################################################
#7. Create Parallel Polygons
#######################################################################################
#Repeat all steps above for parallel polygons

munic_points_2$end_longitude <- munic_points_2$longitude+munic_points_2$length_parallel*cos(munic_points_2$radians_parallel)
munic_points_2$end_latitude <- munic_points_2$latitude+munic_points_2$length_parallel*sin(munic_points_2$radians_parallel)

munic_points_2[which(munic_points_2$state=="AP"), 'chunk'] <- 1
munic_points_2[which(munic_points_2$state=="PA"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="MA1"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="MA2"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="PI"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="CE1"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="CE2"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="RN1"), 'chunk'] <- 2
munic_points_2[which(munic_points_2$state=="RN2"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="PB"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="PE"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="AL"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="SE"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="BA1"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="BA2"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="ES"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="RJ1"), 'chunk'] <- 3
munic_points_2[which(munic_points_2$state=="RJ2"), 'chunk'] <- 4
munic_points_2[which(munic_points_2$state=="SP1"), 'chunk'] <- 4
munic_points_2[which(munic_points_2$state=="SP2"), 'chunk'] <- 4
munic_points_2[which(munic_points_2$state=="PR"), 'chunk'] <- 5
munic_points_2[which(munic_points_2$state=="SC1"), 'chunk'] <- 5
munic_points_2[which(munic_points_2$state=="SC2"), 'chunk'] <- 5
munic_points_2[which(munic_points_2$state=="RS"), 'chunk'] <- 5

munic_points_list <- split(munic_points_2, as.factor(munic_points_2$chunk))

for(i in 1:length(munic_points_list)){
  if(i==2){
    mi <- munic_points_list[[i]]
    mi <- mi[order(mi$longitude),]
    if(length(which(is.na(mi$length_parallel)==T))!=0) mi <- mi[-which(is.na(mi$length_parallel)==T),]
  }
  if(i==4){
    mi <- munic_points_list[[i]]
    mi1 <- mi[which(mi$state=="SP2"),]
    mi2 <- mi[which(mi$state=="SP1"),]
    mi3 <- mi[which(mi$state=="RJ2"),]
    mi1 <- mi1[order(mi1$longitude),]
    mi2 <- mi2[order(mi2$latitude),]
    mi3 <- mi3[order(mi3$longitude),]
    if(length(which(is.na(mi1$length_parallel)==T))!=0) mi1 <- mi1[-which(is.na(mi1$length_parallel)==T),]
    if(length(which(is.na(mi2$length_parallel)==T))!=0) mi2 <- mi2[-which(is.na(mi2$length_parallel)==T),]
    if(length(which(is.na(mi3$length_parallel)==T))!=0) mi3 <- mi3[-which(is.na(mi3$length_parallel)==T),]
    mi <- rbind(mi1,mi2,mi3)
  }
  if(i==1|i==3|i==5){
    mi <- munic_points_list[[i]]
    if(length(which(is.na(mi$length_parallel)==T))!=0) mi <- mi[-which(is.na(mi$length_parallel)==T),]
    mi <- mi[order(mi$latitude),]
  }
  mi$lead_lat <- lead(mi$latitude, n=1L)
  mi$lead_lon <- lead(mi$longitude, n=1L)
  mi$lead_end_lat <- lead(mi$end_latitude, n=1L)
  mi$lead_end_lon <- lead(mi$end_longitude, n=1L)
  if(i==2){
    mi[nrow(mi), 'lead_lat'] <- -5.380000
    mi[nrow(mi), 'lead_lon'] <- -35.3300
    mi[nrow(mi), 'lead_end_lat'] <- -5.380000
    mi[nrow(mi), 'lead_end_lon'] <- -31.81
  } 
  if(i==3){
    mi[1, 'lead_lat'] <- -23.01000
    mi[1, 'lead_lon'] <- -42.00000
    mi[1, 'lead_end_lat'] <- -23.010
    mi[1, 'lead_end_lon'] <- -38.1000
    mi <- rbind(mi, mi[1,])
    mi[nrow(mi), 'longitude'] <- -42.04
    mi[nrow(mi), 'latitude'] <- -22.93
    mi[nrow(mi), 'lead_lat'] <- -22.82
    mi[nrow(mi), 'lead_lon'] <- -41.97
    mi[nrow(mi), 'lead_end_lat'] <- -22.82
    mi[nrow(mi), 'lead_end_lon'] <- -41.97 + 4
    mi[nrow(mi), 'end_latitude'] <- -22.93 
    mi[nrow(mi), 'end_longitude'] <- -42.04 + 4
  } 
  polid <- paste('chunk', i, sep='_')
  mi$pol_ID_parallel <- paste(polid, seq(1, nrow(mi), 1), sep='_')
  # if(length(which(mi$pol_ID_parallel=="chunk_4_12"))!=0) mi <- mi[-which(mi$pol_ID_parallel=="chunk_4_8"),]
  # if(length(which(mi$pol_ID_parallel=="chunk_4_12"))!=0) mi <- mi[-which(mi$pol_ID_parallel=="chunk_4_9"),]
  # if(length(which(mi$pol_ID_parallel=="chunk_4_12"))!=0) mi <- mi[-which(mi$pol_ID_parallel=="chunk_4_10"),]
  # if(length(which(mi$pol_ID_parallel=="chunk_4_11"))!=0) mi <- mi[-which(mi$pol_ID_parallel=="chunk_4_11"),]
  # if(length(which(mi$pol_ID_parallel=="chunk_4_12"))!=0) mi <- mi[-which(mi$pol_ID_parallel=="chunk_4_12"),]
  
  munic_points_list[[i]] <- mi
  
}

munic_points_3 <- do.call('rbind', munic_points_list)
munic_points_3 <- munic_points_3[-which(is.na(munic_points_3$lead_lat)==T),]


my_polygon_parallel <- list()

for(i in 1:nrow(munic_points_3)){
  x <- t(t(as.numeric(c(munic_points_3[i, c('longitude', 'lead_lon','lead_end_lon', 'end_longitude')]))))
  y <- t(t(as.numeric(c(munic_points_3[i, c('latitude', 'lead_lat','lead_end_lat','end_latitude')]))))
  if(munic_points_3[i,"pol_ID_parallel"]=="chunk_4_13"){
    new_point_lon <- -40.96+3.58*cos(-0.459457542)
    new_point_lat <- -21.3+3.58*sin(-0.459457542)
    x <- t(t(c(x[1:3], new_point_lon, x[4])))
    y <- t(t(c(y[1:3], new_point_lat, y[4])))
  }
  md_parallel <- data.frame(cbind(x,y,munic_points_3[i,'pol_ID_parallel']))
  md_parallel$X1 <- as.numeric(as.character(md_parallel$X1))
  md_parallel$X2 <- as.numeric(as.character(md_parallel$X2))
  my_polygon_parallel[[i]] <- md_parallel
}



df_my_polygon_parallel <- do.call('rbind', my_polygon_parallel)

colnames(df_my_polygon_parallel) <- c('V1', 'V2', 'pol_ID_parallel')

ids <- unique(df_my_polygon_parallel$pol_ID_parallel)
for(g in ids){
  di <- df_my_polygon_parallel[which(df_my_polygon_parallel$pol_ID_parallel==g), c('V1', 'V2')]
  di <- na.omit(di)
  if(nrow(di)==4){
    p1 <- st_point(as.numeric(di[1,]))
    p2 <- st_point(as.numeric(di[2,]))
    p3 <- st_point(as.numeric(di[3,]))
    p4 <- st_point(as.numeric(di[4,]))
    geom <- st_sfc(c(p1,p2,p3,p4))
    df = st_as_sf(geom)
    coord_df <- st_coordinates(st_centroid(df))
    df_my_polygon_parallel[which(df_my_polygon_parallel$pol_ID_parallel==g), 'lon_centroid_parallel'] <- coord_df[1]
    df_my_polygon_parallel[which(df_my_polygon_parallel$pol_ID_parallel==g), 'lat_centroid_parallel'] <- coord_df[2]
  }
  
  print(g)
}


#Check polygons
ggplot() +
  theme_minimal() + no_axis +
  geom_sf(data=coastal_munic, color="black", size=.001, show.legend = FALSE) +
  geom_polygon(data=df_my_polygon_parallel, aes(x=V1, y=V2, group=pol_ID_parallel), show.legend = FALSE, colour='white', fill='black', size=.001)+
  ggsave("Outputs/Maps/Municipalities_ParallelPolygons_Check.pdf")


#######################################################################################
#8. Determine whether each geolocated well falls within which parallel polygon
#######################################################################################

wells_offshore <- data.frame(wells_offshore)

all_indicators <- NULL

for(i in 1:nrow(wells_offshore)){
  
  wi <- wells_offshore[i,]
  
  all_indicators_r <- NULL
  
  for(j in 1:length(my_polygon_parallel)){
    
    pol_i <- my_polygon_parallel[[j]]
    pol_i$X3 <- NULL
    #is_in_pol <- pip2d(as.matrix(pol_i), t(as.numeric(wi[,c('LONGITUDE_BASE_DD','LATITUDE_BASE_DD')])))
    is_in_pol <- point.in.polygon(point.x=as.numeric(wi$LONGITUDE_BASE_DD), point.y=as.numeric(wi$LATITUDE_BASE_DD), pol.x = pol_i$X1, pol.y=pol_i$X2)
    all_indicators_r <- cbind(all_indicators_r, as.numeric(is_in_pol))
  }
  
  all_indicators <- rbind(all_indicators, all_indicators_r)
  print(i)
}

all_indicators <- data.frame(all_indicators)
colnames(all_indicators) <- unique(df_my_polygon_parallel$pol_ID_parallel)

pol_ID_parallel <- NULL
for(p in 1:nrow(all_indicators)){
  pol_ID_p <- c(which(all_indicators[p,]!=0))
  pol_ID_number <- colnames(all_indicators)[pol_ID_p]
  if(length(pol_ID_p)!=0){
    pol_ID_p <- cbind(p, pol_ID_number)
    pol_ID_parallel <- rbind(pol_ID_parallel, pol_ID_p)
  }
  print(p)
}

pol_ID_parallel <- data.frame(pol_ID_parallel)
colnames(pol_ID_parallel) <- c('well_ID','pol_ID_parallel')
pol_ID_parallel$well_ID <- as.numeric(as.character(pol_ID_parallel$well_ID))
wells_offshore$well_ID <- seq(1, nrow(wells_offshore),1)
wells_offshore_with_polygons_both <- merge(pol_ID_parallel[, c('well_ID', 'pol_ID_parallel')], wells_offshore_with_polygons, by=c('well_ID'), all=T)

df_my_polygon_parallel_withcentroids <- merge(df_my_polygon_parallel, munic_points_3, by=c('pol_ID_parallel'))                       


#######################################################################################
#9. Associate polygons with municipality codes
#######################################################################################

#First compute centroid of each coastal municipality
coord <- st_coordinates(st_centroid(coastal_munic))
coastal_munic_with_centroids <- cbind(coastal_munic, coord)

#Rename state to state_segment 
#Create new variable state that matches state_list
df_my_polygon_orthogonal_withcentroids <- data.frame(df_my_polygon_orthogonal_withcentroids)
colnames(df_my_polygon_orthogonal_withcentroids)[which(colnames(df_my_polygon_orthogonal_withcentroids)=="state")] <- "state_segment"
df_my_polygon_orthogonal_withcentroids$state <- df_my_polygon_orthogonal_withcentroids$state_segment
df_my_polygon_orthogonal_withcentroids$state <- as.character(df_my_polygon_orthogonal_withcentroids$state)
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "MA1"] <- "MA"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "MA2"] <- "MA"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "SP1"] <- "SP"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "SP2"] <- "SP"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "SC1"] <- "SC"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "SC2"] <- "SC"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "RN1"] <- "RN"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "RN2"] <- "RN"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "RJ1"] <- "RJ"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "RJ2"] <- "RJ"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "CE1"] <- "CE"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "CE2"] <- "CE"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "BA1"] <- "BA"
df_my_polygon_orthogonal_withcentroids$state[df_my_polygon_orthogonal_withcentroids$state == "BA2"] <- "BA"

#Repeat for parallels
df_my_polygon_parallel_withcentroids <- data.frame(df_my_polygon_parallel_withcentroids)
colnames(df_my_polygon_parallel_withcentroids)[which(colnames(df_my_polygon_parallel_withcentroids)=="state")] <- "state_segment"
df_my_polygon_parallel_withcentroids$state <- df_my_polygon_parallel_withcentroids$state_segment
df_my_polygon_parallel_withcentroids$state <- as.character(df_my_polygon_parallel_withcentroids$state)
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "MA1"] <- "MA"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "MA2"] <- "MA"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "SP1"] <- "SP"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "SP2"] <- "SP"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "SC1"] <- "SC"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "SC2"] <- "SC"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "RN1"] <- "RN"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "RN2"] <- "RN"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "RJ1"] <- "RJ"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "RJ2"] <- "RJ"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "CE1"] <- "CE"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "CE2"] <- "CE"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "BA1"] <- "BA"
df_my_polygon_parallel_withcentroids$state[df_my_polygon_parallel_withcentroids$state == "BA2"] <- "BA"

#Plot for one state at a time
state_list <- unique(coastal_munic$CD_UF_SIG)

#Plot orthogonal polygons with labels
for(i in state_list){
  
  p=ggplot() +
    geom_sf(data=coastal_munic[which(coastal_munic$CD_UF_SIG==i),], color="black", size=.001, show.legend = FALSE) +
    theme_minimal() + no_axis +
    geom_polygon(data=df_my_polygon_orthogonal_withcentroids[which(df_my_polygon_orthogonal_withcentroids$state==i),], aes(x=V1, y=V2, group=pol_ID_orthogonal), show.legend = FALSE, colour='white', fill='black', size=.001, alpha=.5)+
    geom_text(data = coastal_munic_with_centroids[which(coastal_munic_with_centroids$CD_UF_SIG==i),], aes(X, Y, label = CD_GCMUN), size = 1, color='red')+
    geom_text(data = df_my_polygon_orthogonal_withcentroids[which(df_my_polygon_orthogonal_withcentroids$state==i),], aes(lon_centroid_orthogonal, lat_centroid_orthogonal, label = pol_ID_orthogonal), size = 1, color='green')
  filename <- paste('map_state_orthogonal', i, sep='_')
  filename <- paste(filename, '.pdf', sep='')
  filename <- paste('Outputs/Maps/', filename, sep='')
  ggsave(filename, p)
  
}

#Plot parallel polygons with labels
for(i in state_list){
  
  p=ggplot() +
    geom_sf(data=coastal_munic[which(coastal_munic$CD_UF_SIG==i),], color="black", size=.001, show.legend = FALSE) +
    theme_minimal() + no_axis +
    geom_polygon(data=df_my_polygon_parallel_withcentroids[which(df_my_polygon_parallel_withcentroids$state==i),], aes(x=V1, y=V2, group=pol_ID_parallel), show.legend = FALSE, colour='white', fill='black', size=.001, alpha=.5)+
    geom_text(data = coastal_munic_with_centroids[which(coastal_munic_with_centroids$CD_UF_SIG==i),], aes(X, Y, label = CD_GCMUN), size = 1, color='red')+
    geom_text(data = df_my_polygon_parallel_withcentroids[which(df_my_polygon_parallel_withcentroids$state==i),], aes(lon_centroid_parallel, lat_centroid_parallel, label = pol_ID_parallel), size = 1, color='green')
  filename <- paste('map_state_parallel', i, sep='_')
  filename <- paste(filename, '.pdf', sep='')
  filename <- paste('Outputs/Maps/', filename, sep='')
  ggsave(filename, p)
  
}

#Write out csv of orthogonal polygons to manually link to municipality codes 
write.csv(df_my_polygon_orthogonal_withcentroids, file="Outputs/Data/Polygon_Codes_Orthogonal.csv", row.names=TRUE)

#Write out csv of parallel polygons to manually link to municipality codes 
write.csv(df_my_polygon_parallel_withcentroids, file="Outputs/Data/Polygon_Codes_Parallel.csv", row.names=TRUE)


#######################################################################################
#11. Use crosswalk to merge municipality ID numbers with wells dataset
#######################################################################################

#Import polygon crosswalks and merge with wells data on pol_id_orthogonal and pol_id_parallel
polygon_crosswalk_orthogonal <- read.csv("Inputs/Data/Municipality_Polygon_Crosswalk_Orthogonal.csv")
polygon_crosswalk_parallel <- read.csv("Inputs/Data/Municipality_Polygon_Crosswalk_Parallel.csv")

wells_offshore_with_polygons_and_munics <- merge(wells_offshore_with_polygons_both, polygon_crosswalk_orthogonal[, c('pol_ID_orthogonal', 'munic_code_orthogonal')], by=c('pol_ID_orthogonal'), all.x=T) 
wells_offshore_with_polygons_and_munics <- merge(wells_offshore_with_polygons_and_munics, polygon_crosswalk_parallel[, c('pol_ID_parallel', 'munic_code_parallel')], by=c('pol_ID_parallel'), all.x=T) 

#Export csv file of wells with municipalities 
write.csv(wells_offshore_with_polygons_and_munics, file="Outputs/Data/Wells_with_Municipalities.csv", row.names=TRUE)


######################################################################################
#Plot orthogonal polygons with labels
for(i in state_list){

  p=ggplot() +
    geom_sf(data=coastal_munic[which(coastal_munic$CD_UF_SIG==i),], color="black", size=.001, show.legend = FALSE) +
    theme_minimal() + no_axis +
    geom_polygon(data=df_my_polygon_orthogonal_withcentroids[which(df_my_polygon_orthogonal_withcentroids$state==i),], aes(x=V1, y=V2, group=pol_ID_orthogonal), show.legend = FALSE, colour='white', fill='black', size=.001, alpha=.5)+
    geom_point(data = wells_offshore_with_polygons_and_munics[which(wells_offshore_with_polygons_and_munics$ESTADO==i),], aes(x = LONGITUDE_BASE_DD, y = LATITUDE_BASE_DD, group=Success, fill=Success), size = 1,
               shape = 21)+
    geom_text(data = coastal_munic_with_centroids[which(coastal_munic_with_centroids$CD_UF_SIG==i),], aes(X, Y, label = CD_GCMUN), size = 1, color='red')+
    geom_text(data = df_my_polygon_orthogonal_withcentroids[which(df_my_polygon_orthogonal_withcentroids$state==i),], aes(lon_centroid_orthogonal, lat_centroid_orthogonal, label = pol_ID_orthogonal), size = 1, color='green')
  filename <- paste('map_state_orthogonal_withwells', i, sep='_')
  filename <- paste(filename, '.pdf', sep='')
  filename <- paste('Outputs/Maps/', filename, sep='')
  ggsave(filename, p)

}

#Plot parallel polygons with labels
for(i in state_list){

  p=ggplot() +
    geom_sf(data=coastal_munic[which(coastal_munic$CD_UF_SIG==i),], color="black", size=.001, show.legend = FALSE) +
    theme_minimal() + no_axis +
    geom_polygon(data=df_my_polygon_parallel_withcentroids[which(df_my_polygon_parallel_withcentroids$state==i),], aes(x=V1, y=V2, group=pol_ID_parallel), show.legend = FALSE, colour='white', fill='black', size=.001, alpha=.5)+
    geom_point(data = wells_offshore_with_polygons_and_munics[which(wells_offshore_with_polygons_and_munics$ESTADO==i),], aes(x = LONGITUDE_BASE_DD, y = LATITUDE_BASE_DD, group=Success, fill=Success), size = 1,
               shape = 21)+
    geom_text(data = coastal_munic_with_centroids[which(coastal_munic_with_centroids$CD_UF_SIG==i),], aes(X, Y, label = CD_GCMUN), size = 1, color='red')+
    geom_text(data = df_my_polygon_parallel_withcentroids[which(df_my_polygon_parallel_withcentroids$state==i),], aes(lon_centroid_parallel, lat_centroid_parallel, label = pol_ID_parallel), size = 1, color='green')
  filename <- paste('map_state_parallel_withwells', i, sep='_')
  filename <- paste(filename, '.pdf', sep='')
  filename <- paste('Outputs/Maps/', filename, sep='')
  ggsave(filename, p)

}

# 
# #Plot orthogonal polygons with labels
# for(i in state_list){
#   
#   p=ggplot() +
#     geom_sf(data=coastal_munic[which(coastal_munic$CD_UF_SIG==i),], color="black", size=.001, show.legend = FALSE) +
#     theme_minimal() + no_axis +
#     geom_polygon(data=df_my_polygon_orthogonal_withcentroids[which(df_my_polygon_orthogonal_withcentroids$state==i),], aes(x=V1, y=V2, group=pol_ID_orthogonal), show.legend = FALSE, colour='white', fill='black', size=.001, alpha=.5)+
#     geom_point(data = wells_offshore_with_polygons_and_munics[which(wells_offshore_with_polygons_and_munics$ESTADO==i),], aes(x = LONGITUDE_BASE_DD, y = LATITUDE_BASE_DD), size = .4, 
#                shape = 21, color='blue', fill='blue')+
#     geom_text(data = coastal_munic_with_centroids[which(coastal_munic_with_centroids$CD_UF_SIG==i),], aes(X, Y, label = CD_GCMUN), size = 1, color='red')+
#     geom_text(data = df_my_polygon_orthogonal_withcentroids[which(df_my_polygon_orthogonal_withcentroids$state==i),], aes(lon_centroid_orthogonal, lat_centroid_orthogonal, label = pol_ID_orthogonal), size = 1, color='green')
#   filename <- paste('map_state_orthogonal_withwells', i, sep='_')
#   filename <- paste(filename, '.pdf', sep='')
#   filename <- paste('Outputs/Maps/', filename, sep='')
#   ggsave(filename, p)
#   
# }
# 
# #Plot parallel polygons with labels
# for(i in state_list){
#   
#   p=ggplot() +
#     geom_sf(data=coastal_munic[which(coastal_munic$CD_UF_SIG==i),], color="black", size=.001, show.legend = FALSE) +
#     theme_minimal() + no_axis +
#     geom_polygon(data=df_my_polygon_parallel_withcentroids[which(df_my_polygon_parallel_withcentroids$state==i),], aes(x=V1, y=V2, group=pol_ID_parallel), show.legend = FALSE, colour='white', fill='black', size=.001, alpha=.5)+
#     geom_point(data = wells_offshore_with_polygons_and_munics[which(wells_offshore_with_polygons_and_munics$ESTADO==i),], aes(x = LONGITUDE_BASE_DD, y = LATITUDE_BASE_DD), size = .4, 
#                shape = 21, color='blue', fill='blue')+
#     geom_text(data = coastal_munic_with_centroids[which(coastal_munic_with_centroids$CD_UF_SIG==i),], aes(X, Y, label = CD_GCMUN), size = 1, color='red')+
#     geom_text(data = df_my_polygon_parallel_withcentroids[which(df_my_polygon_parallel_withcentroids$state==i),], aes(lon_centroid_parallel, lat_centroid_parallel, label = pol_ID_parallel), size = 1, color='green')
#   filename <- paste('map_state_parallel_withwells', i, sep='_')
#   filename <- paste(filename, '.pdf', sep='')
#   filename <- paste('Outputs/Maps/', filename, sep='')
#   ggsave(filename, p)
#   
# }
# 
# 
