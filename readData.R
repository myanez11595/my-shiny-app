library(sf)
library(sp)
library(dplyr)
library(magrittr)
library(shinycustomloader)
library(terra)
library(sfarrow)
#   ____________________________________________________________________________
#   Shapes                                                                  ####
suppressMessages({

    HEX=st_read_parquet("data/DATA/HEXAGONOS/HEX_PREDICT2.parquet")
    #HEX=st_read("data/DATA/marzo 2024/PARR_QUITO.gpkg")
    BORDER <- st_union(HEX) %>% st_as_sf()
    BUS_STOP=rast("data/DATA/bus_stops_tt.img")
    EDUCATION=rast("data/DATA/education_tt.img")
    HEALTHCAR=rast("data/DATA/healthcare_tt.img")
    LANDFILL=rast("data/DATA/landfill_tt.tif")
    SHOPS=rast("data/DATA/shops_tt.img")
    INDUSTRIES=rast("data/DATA/industries_tt.img")
    UNSAFE=rast("data/DATA/plg.img")
    POLICESTATION=rast("data/DATA/police_tt.img")
    DOWNTOWN=rast("data/DATA/urbcenters_tt.tif")
    #SHOPS=rast("data/DATA/SHOPS_TT.img")
    #HEX=HEX[1:1000,]
    #DEPARTAMENTOS_tif <- rast('C:/Users/mjyanez/Downloads/intelligentsia-master(1)/intelligentsia-master/data/DATA/INFO RECORTADA 2023/DEPARTAMENTOS.tif')

})
