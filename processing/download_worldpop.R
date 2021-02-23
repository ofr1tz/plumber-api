require(tidyverse)
require(glue)

years <- 2000:2020
url <- glue(
	"ftp://ftp.worldpop.org.uk/GIS/Population/Global_2000_2020/{years}",
	"/0_Mosaicked/ppp_{years}_1km_Aggregated.tif"
)

# download yearly worldpop 1k global mosaics if not existent
load_worldpop <- function(url, path) {
	file <- file.path(path, basename(url))
	if(!file.exists(file)) download.file(url, file, mode="wb")
}

url %>% walk(load_worldpop, path = here::here("/data/worldpop")) 
