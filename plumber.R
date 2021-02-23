require(plumber)
require(tidyverse)
require(sf)

#* @apiTitle Just a bunch of useful API endpoints

#* Extract WorldPop values on a buffer around a geographic point
#* @serializer unboxedJSON 
#* @param lon:number Point longitude
#* @param lat:number Point latitude
#* @param radius:number Radius of the buffer in meter
#* @param step_dg:numeric Step of bearings in degree for point density of buffer edge
#* @param fun:[str] Summary function(s) for the raster extraction
#* @param year:[int] Year(s) of WorldPop data to extract
#* @param geom:boolean Response GeoJSON with buffer geometry?
#* @post /exact_extract/circle/worldpop
#* @get /exact_extract/circle/worldpop
function(
	lon = 0, lat = 0, radius = 1000, step_dg = 10, 
	fun = "sum", year = 2020, 
	geom = FALSE
) {
	lon <- as.numeric(lon)
	lat <- as.numeric(lat)
	radius <- as.numeric(radius)
	year <- as.integer(year)
	geom <- as.logical(geom)
	step_dg <- as.numeric(step_dg)
	
	funs <- c(
		"sum", "count", "min", "max", "mean", "median", "mode",	"majority", 
		"minority", "variety", "variance", "stdev", "coefficient_of_variation"
	)
	
	years <- 2000:2020
	
	if(any(!(fun %in% funs))) stop(paste(
		"Summary function(s)", 
		paste(year[!(year %in% years)], collapse = ", "), 
		"not available."
	))
	if(any(!(year %in% years))) stop(paste(
		"Year(s)", 
		paste(year[!(year %in% years)], collapse = ", "), 
		"not available."
	))

	# make geodesic buffer
	buff <- sf::st_point(c(lon, lat), "XY") %>%
		sf::st_sfc(crs = 4326) %>%
		sf::st_sf() %>%
		geobuffer::geobuffer_pts(dist_m = radius, step_dg = step_dg, output = "sf")
		
	# function to extract worldpop data
	extract_year <- function(year) {
		path <- paste0("data/worldpop/ppp_", year, "_1km_Aggregated.tif")
		r <- raster::raster(path)
		names(r) <- paste0("worldpop", year)
		exactextractr::exact_extract(x = r, y = buff, fun = fun, force_df = T, full_colnames = T)
	}
	
	# extract worldpop data for years requested
	results <- year %>% map(extract_year) %>% reduce(cbind)

	# return (geo)json
	if(geom) { 
		list(
			type = "Feature",
			properties = jsonlite::unbox(results), 
			geometry = (wellknown::sf_convert(buff) %>% wellknown::wkt2geojson())$geometry
		)
	} else { 
		results 
	}
}

