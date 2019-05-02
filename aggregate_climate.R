
library(raster)
library(tidyverse)

files <- list.files("F:/chelsa/monthly48", full.names=T)

for(f in files){
      message(f)
      outfile <- paste0("e:/hydrography/hydrography/climate/",
                        basename(f))
      if(file.exists(outfile)) next()
      f %>% raster() %>% aggregate(10, filename=outfile)
}

