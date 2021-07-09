


library("tidyjson")

d <- suppressWarnings(lapply(addresses, function(address) {
  #set the elapsed time counter to 0
  t <- Sys.time()
  #calling the nominatim OSM API
  api_output <- nominatim_osm(address)
  #get the elapsed time
  t <- difftime(Sys.time(), t, 'secs')
  #return data.frame with the input address, output of the nominatim_osm function and elapsed time
  return(data.frame(address = address, api_output, elapsed_time = t))
}) %>%
  #stack the list output into data.frame
  bind_rows() %>% data.frame())
#output the data.frame content into console
