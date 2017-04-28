#' @title SUMO reference class object
#' @name sumo
#' @description Reference class implementation that enables easy use of SUMO functionality
#' through the maintaining of consistent reference class states. Suitable for simulation.
#' @import methods sp xml2
#' @export sumo
#' @exportClass sumo
sumo <- setRefClass("sumo", fields = c("source", "routes", "trips", "network", "config", "configuration"))

sumo$methods(initialize = function(...){
  callSuper(...)
  if (nchar(Sys.getenv("SUMO_HOME")) <= 1) stop("sumo expects the environmental variable 'SUMO_HOME' to be set")
  if (nchar(Sys.getenv("SUMO_TMP")) <= 1){
    sumo_tmp <-  paste0(normalizePath(tempdir()), "/")
  } else { sumo_tmp <- Sys.getenv("SUMO_TMP") }
  config <<- list(SUMO_HOME = Sys.getenv("SUMO_HOME"), SUMO_TMP = sumo_tmp)
})

sumo$methods(setSource=function(source, type="osm", ...){
  if (class(source) != "character") stop("sumo expects the source object to be a filepath")

})

#' @title netconvert
#' @name netconvert
#' @description Converts an OSM map to a SUMO road network suitable for simulation.
sumo$methods(netconvert = function(src_net = NA, net_file = "tmp.net.xml", net_type = "osmNetconvert.typ.xml", urban = T, pedestrian=F, polygons=F,
                                   defaults = "--geometry.remove --roundabouts.guess --ramps.guess --junctions.join --tls.guess-signals --tls.discard-simple --tls.join",
                                   flags="") {
  if (missing(src_net) && is.null(source)) { stop("source has not been populated") }
  if (!missing(src_net)){ source <<- src_net }
  network <<- net_file
  SUMO_HOME <- config$SUMO_HOME
  SUMO_TMP <- config$SUMO_TMP

  ## Use OSM and built-in OSM-to-SUMO defaults to build the SUMO network
  cat("Converting to SUMO network...\n")
  base <- "netconvert"
  base_type <- paste("--type-files", paste0(SUMO_HOME, "/typemap/", net_type))
  type_file <- base_type
  if (urban) { type_file <- paste(type_file, paste0(SUMO_HOME, "/typemap/osmNetconvertUrbanDe.typ.xml"), sep = ",") }
  if (pedestrian) { type_file <- paste(type_file, paste0(SUMO_HOME, "/typemap/osmNetconvertPedestrians.typ.xml"), sep = ",") }

  ## Cleaning process, replace file
  osm_file <- paste("--osm-files", paste0(SUMO_TMP, source))
  out_file <- paste("--output-file", paste0(SUMO_TMP, network))
  flags <- paste(defaults, flags)

  ## Piece command together and initiate it
  command <- paste(base, type_file, osm_file, out_file, flags)
  cat(paste(command, "\n"))
  system(command)

  ## If polygons provided, import them
  if (polygons) {
    base_poly <- "polyconvert"
    net_file <- paste("--net-file", paste0(SUMO_TMP, network))
    type_file <- paste("--type-file", paste0(SUMO_HOME, "/typemap/osmPolyconvert.typ.xml"))
    out_file <- paste("-o", paste0(SUMO_TMP, "tmp.poly.xml"))
    poly_command <- paste(base_poly, net_file, osm_file, type_file, out_file)
    cat(paste(poly_command, "\n"))
    system(poly_command)
    config$polygon <<- TRUE
  }

  sumo_net <- read_xml(paste0(SUMO_TMP, network))
  config$location <<- attributes(xml2::as_list(xml_find_first(sumo_net, "//location")))
  config$CRS <<- CRS(config$location[["projParameter"]])
  config$bbox <<- sp::bbox(matrix(as.numeric(unlist(strsplit(config$location$origBoundary, ","))), nrow=2, byrow = T))
})


#' @title plot
#' @name plot
#' @description Converts an OSM map to a SUMO road network suitable for simulation.
sumo$methods(plot = function(x = NULL, add = F, type = "leaflet", src = c("arcgis", "leaflet", "osm")) {
  if (missing(src) || src == "arcgis")
    template <- "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
  map <- leaflet() %>%
      setView(lng = apply(config$bbox, 1, mean)[[1]], lat = apply(config$bbox, 1, mean)[[2]], zoom = 16)
})

#' @title getEdges
#' @name getEdges
#' @description Converts an OSM map to a SUMO road network suitable for simulation.
sumo$methods(getEdges = function(internal=F, nested =F) {
  tmp_net_file <- paste0(config$SUMO_TMP, network)
  xpath <- ifelse(internal, "edge", "edge[@from]")
  edges <- xml2::xml_find_all(xml2::read_xml(tmp_net_file), xpath)
  if (nested){
    ## todo
    res <- xml2::as_list(xml2::xml_find_all(xml2::read_xml(tmp_net_file), xpath))
  } else {
    edge_xml <- xml2::xml_attrs(xml2::xml_find_all(xml2::read_xml(tmp_net_file), xpath))
    return(data.table::rbindlist(lapply(edge_xml, as.list), fill = T))
  }
})


#' @title randomTrips2
#' @name randomTrips2
#' @description Uses a SUMO road network to generate road traffics
sumo$methods(randomTrips2 = function(start=0, end=3600, p = 1, n=1, person = F){

  ## SUMO-RandomTrips generates routes and trips
  cat("Running Road Traffic Simulation...\n")
  SUMO_TMP <- config$SUMO_TMP
  base <- "sumo-randomTrips"
  net_inp <- paste0("-n ", SUMO_TMP, network)
  route_inp <- paste0("-r ", SUMO_TMP, "tmp.rou.xml")
  trip_inp <- paste0("-o ", SUMO_TMP, "tmp.trips.xml")
  time_inp <- paste0("-s ", start, " -e ", end)
  type_inp <- paste0("-p ", p, " -l --binomial=", n)

  ## Assemble command and simulate
  command <- paste(base, net_inp, route_inp, trip_inp, time_inp, type_inp)
  if (person) command <- paste(command, "--pedestrians")
  cat("Running command: ")
  cat(command)
  config$sim_status <<- system(command)
  if (config$sim_status == 0){
    trips <<- paste0(SUMO_TMP, "tmp.trips.xml")
    routes <<- paste0(SUMO_TMP, "tmp.rou.xml")
  }
})

#' @title simulate
#' @name simulate
#' @description Uses a SUMO road network to generate road traffics
sumo$methods(randomTrips = function(start=0, end=3600, p = 1, n=1, person = F, flags=""){

  ## SUMO-RandomTrips generates routes and trips
  cat("Running Road Traffic Simulation...\n")
  SUMO_TMP <- config$SUMO_TMP
  base <- "sumo-randomTrips"
  net_inp <- paste0("-n ", SUMO_TMP, network)
  route_inp <- paste0("-r ", SUMO_TMP, "tmp.rou.xml")
  trip_inp <- paste0("-o ", SUMO_TMP, "tmp.trips.xml")
  time_inp <- paste0("-s ", start, " -e ", end)
  type_inp <- paste0("-p ", p, " -l --binomial=", n)

  ## Assemble command and simulate
  command <- paste(base, net_inp, route_inp, trip_inp, time_inp, type_inp)
  if (person) command <- paste(command, "--pedestrians")
  command <- paste(command, flags)
  cat("Running command: ")
  cat(command)
  config$sim_status <<- system(command)
  if (config$sim_status == 0){
    trips <<- paste0(SUMO_TMP, "tmp.trips.xml")
    routes <<- paste0(SUMO_TMP, "tmp.rou.xml")
  } else {
    stop(paste0("Failed to save trips and routes file: status code ", config$sim_status))
  }
})


#' @title Open SUMO GUI and run simulation
#' @name sumo_viewSimulation
#' @description Retrieves a map from OpenStreetMap to use for SUMO simulation
sumo$methods(viewSimulation = function(flags=""){
  ## Open GUI for viewing simulation
  command <- paste0("sumo-gui -n ", config$SUMO_TMP, "tmp.net.xml -r ", config$SUMO_TMP, "tmp.rou.xml", " ", flags)
  if (config$polygon){ command <- paste0(command, " -a ", config$SUMO_TMP, "tmp.poly.xml") }
  system(command)
})

#' @title Get OpenStreetMap source
#' @name getOSM
#' @description Retrieves a map from OpenStreetMap to use for SUMO simulation
sumo$methods(getOSM = function(bbox, file="tmp.osm", overwrite=FALSE) {
  if(!"matrix" %in% class(bbox)) { stop("Need 2x2 bounding box matrix to extract from OSM") }
  coords <- paste0(bbox, collapse = ",")
  url <- paste0("http://api.openstreetmap.org/api/0.6/map?bbox=", coords)
  if (file.exists(paste0(config$SUMO_TMP, file)) && overwrite == FALSE){
    warning("Detected existing OSM file with that name. Setting source to that file.")
    source <<- file
  } else {
    cat("Downloading OSM from: ", url, "\n")
    curl::curl_download(url = url, destfile = paste0(config$SUMO_TMP, file))
    source <<- file
  }
})

#' @title Get SUMO Junctions
#' @name getJunctions
#' @description Retrieves junctions in SUMO network
sumo$methods(getJunctions = function(){
  sumo_net <- read_xml(paste0(config$SUMO_TMP, network))
  res <- as_list(xml_find_all(sumo_net, "//junction"))
  res <- lapply(res, function(junc) append(attributes(junc), list(requests=unlist(junc))))
  res
})

#' @title Get SUMO Polygons
#' @name getPolygons
#' @description Retrieves Polygons in SUMO network
sumo$methods(getPolygons = function(poly_file =  "tmp.poly.xml"){
  sumo_net <- read_xml(paste0(SUMO_TMP, "tmp.poly.xml"))
  res <- as_list(xml_find_all(sumo_net, "//poly"))
  res <- lapply(res, function(poly) attributes(poly))
  res
})


#' @title Project coordinates defined in the SUMO network to a target Coordinate Reference System
#' @name projectCRS
#' @description Retrieves junctions in SUMO network
sumo$methods(projectCRS= function(x, crs=CRS("+proj=longlat +ellps=WGS84")){
  if (!is.matrix(x)) stop("projectCRS expects an nx2 matrix of numeric points.")
  offset <- as.numeric(unlist(strsplit(config$location$netOffset, ",")))
  ref <- cbind(x[, 1] - offset[[1]], x[, 2] - offset[[2]])
  spoints <- sp::SpatialPoints(ref, proj4string = config$CRS)
  sp::spTransform(spoints, crs)
})

#' @title Generate SUMO Configuration
#' @name genConfig
#' @description Generates a sumo configuration file suitable for simulation
sumo$methods(genConfig = function(begin, end, file="tmp.sumocfg"){
  if (is(network, "uninitializedField")) stop("network file is uninitialized")
  if (is(routes, "uninitializedField")) stop("routes file is uninitialized")

  config_doc <- XML::newXMLDoc()
  config_tag <- XML::newXMLNode("configuration", doc = config_doc)

  input <- XML::newXMLNode("input", parent = config_tag)
  XML::newXMLNode("net-file", attrs = c(value=network), parent = input)
  XML::newXMLNode("route-files", attrs = c(value=routes), parent = input)

  ## simulation time
  time <- XML::newXMLNode("time", parent = config_tag)
  XML::newXMLNode("begin", attrs = c(value=begin), parent = time)
  XML::newXMLNode("end", attrs = c(value=end), parent = time)
  # XML::newXMLNode("step-length", attrs = c(value=0.01), parent = time)

  ## disable the automatic removal of vehicles which wait too long in front of an intersection.
  XML::newXMLNode("time-to-teleport", attrs = c(value="-1"), parent = config_tag)

  # Configuration file
  XML::saveXML(config_doc, paste0(config$SUMO_TMP, file))
  configuration <<- paste0(config$SUMO_TMP, file)
})

#' @title simulate
#' @name simulate
#' @description Uses a SUMO road network to generate road traffics
sumo$methods(simulate = function(flags=""){
  if (is(osu_net$configuration, "uninitializedField")){
    stop("SUMO configuration file not found. Has genConfig been called?")
  } else {
    cat(paste0("sumo -c ", osu_net$configuration, " ", flags))
    system(paste0("sumo -c ", osu_net$configuration, " ", flags))
  }
})


sumo$methods(vehicleProbe = function(probe = list(id="probe1", type="DEFAULT_VEHTYPE", freq="1"), flags=""){
  config_doc <- XML::newXMLDoc()
  additional <- XML::newXMLNode("additional", doc = config_doc)
  probe$file <- paste0(config$SUMO_TMP, "tmp_vehprobe.xml")
  vTypeProbe <- XML::newXMLNode("vTypeProbe", parent = additional, attrs = probe)
  settings_files <- paste0(config$SUMO_TMP, "tmp_vehprobe_settings.xml")
  XML::saveXML(config_doc, file = settings_files)
  command <- paste0("sumo -c ", osu_net$configuration, " ", flags, " --additional-files ", settings_files)
  cat(command)
  status <- system(command)

  if (status == 0){
    timesteps <- xml2::as_list(xml2::read_xml(probe$file))
    return(data.table::rbindlist(unname(lapply(timesteps, function(veh_list) {
      data.table::rbindlist(lapply(veh_list, function(vehicle) attributes(vehicle)))
    })), idcol = "timestep"))
  } else {
    warning(paste0("SUMO returned non-0 status code: ", status))
    return(NULL)
  }
})

sumo$methods(vehicleAcc = function(traj_file, flags=""){
  if (file.exists(traj_file)){
    timesteps <- xml2::read_xml(traj_file)
    motion_states <- xml2::xml_attrs(xml2::xml_find_all(timesteps, "//motionState"))
    return(data.table::rbindlist(lapply(motion_states, as.list)))
  } else {
    warning(paste0("File doesn't exist: ", traj_file))
    return(NULL)
  }
})

sumo$methods(getRoutes = function(flags = ""){

})

sumo$methods(vehicleStates = function(fcd_file, flags=""){
  if (file.exists(fcd_file)){
    fcd <- xml2::read_xml(fcd_file)
    timesteps <- xml2::xml_find_all(fcd, "//timestep")
    steps <- xml2::xml_attr(timesteps, "time")

    ## Extract vehicle info per timestep
    res <- new.env(parent=emptyenv())
    mapply(function(key, container) invisible(res[[key]] <- xml2::xml_attrs(xml2::xml_children(container))), steps, timesteps)
    res <- as.list(res)

    ## Cast if columns match schema and return
    result1 <- lapply(res, function(states) data.table::rbindlist(lapply(states, as.list)))
    result2 <- list()

    i <- 1
    while(i < 25){
      result2 <- try(data.table::rbindlist(result1, idcol="timestep"), silent = T)
      if ("try-error" %in% class(result2)){ i <- i + 1 }
      else break;
    }

    if (i == 25){
      res <- do.call(rbind, result1)
      res$timestep <- length(unlist(unname(sapply(names(result1), function(timestep) rep(as.numeric(timestep), nrow(result1[[timestep]]))))))
      return(res)
    } else { return(result2) }

    # schema <- c("timestep", "id", "x", "y", "angle", "type", "speed", "pos", "lane", "slope", "signals")
    # if (all(colnames(results) == schema)){
    #   col_types <- c(as.numeric, as.integer, as.numeric, as.numeric, as.numeric, as.factor, as.numeric, as.numeric,
    #                  as.character, as.numeric, as.integer)
    #   result[, (schema) := .(as.numeric(timestep),
    #                          as.integer(id),
    #                          as.numeric(x),
    #                          as.numeric(y),
    #                          as.numeric(angle),
    #                          as.factor(type),
    #                          as.numeric(speed),
    #                          as.numeric(pos),
    #                          as.character(lane),
    #                          as.numeric(slope),
    #                          as.integer(signals))]
    #   result <- result[order(timestep)]
    # }
  } else {
    stop(paste0("File doesn't exist: ", fcd_file))
  }
})




