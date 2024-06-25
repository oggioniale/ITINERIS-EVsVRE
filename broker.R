library(dplyr)
library(magrittr)
library(ReLTER)
#require(rdflib)
library(stringr)
library(pangaear)
library(sf)
library(zen4R)
library(crosstalk)

#' Factory method to get a stateful controller (broker of data).
#' @description The factory creates a
#' controller object. It has state (selection of LTER site and Essential Variable). Broker of data.
#' 
#' The object exposes the methods:
#' 
#' setSite: set selected site (via its deimsUUID. It must be present in the sites table)
#' getSite: return the currently selected site record as a `tibble` with one row
#
#' siteList: return th named `list` of site deimsUUID, useful for selectizeInput in shiny UI
#' EVsList: return the named `list` of (current site's) EV ids, useful for selectizeInput in shiny UI
#
#' siteNames: return a `character` array with the available site names
#' evNames
#' 
#' getCurrentEVs: get the available EVs for the currently selected site (they depend on the domain of the site)
#' setEv: Set the selected EV. Warn if the `id` is not present in the object returned by `getCurrentEVs`
#' getEv: return the currently selected EV record as a tibble with one row
#' 
#' f_getSiteInfo
#' 
#' @return the controller object
#' @author Paolo Tagliolato (ptagliolato)
#' @author Alessandro Oggioni (oggioniale)
#' 
getBroker = function() {
  self = list()
  
  # declare internal vars
  {
    #' deimsUUID (this is the UUID not the url)
    selected_site <- ""
    #' id of the EV as in the EVs tibble
    selected_ev <- ""
    #' tibble datasets with selectable sites and EVs
    sites <- NULL
    EVs <- NULL
    evdatasets <- NULL
    
    # a list to store info for sites, in order not to call many times external repo (DEIMS)
    cacheInfoSite <- list()
  }
  
  # utility functions for caching objects in RDS files
  {
    getCachePath<-function(methodName, siteUUID, evID, basepath=tempdir()){
      rdsname <- paste0(paste(methodName,siteUUID,evID, sep="§"),".RDS")
      paste0(basepath,"/",rdsname)
    }
    
    readFromCache <- function(methodName, siteUUID, evID){
      rdsname <- getCachePath(methodName, siteUUID, evID)
      res<-NULL
      if(file.exists(rdsname)){
        res<-readRDS(file = rdsname)  
      }
      return(res)
    }
    
    writeToCache<-function(x, methodName, siteUUID, evID){
      rdsname <- getCachePath(methodName, siteUUID, evID)
      saveRDS(x,rdsname)
    }
  }
  
  #' set selected site (via its deimsUUID. It must be present in the sites table)
  setSite <- function(deimsUUID) {
    if(!deimsUUID %in% sites$deimsUUID) {
      warning("Site must be specified by its deimsUUID")
      return
    }
    selected_site <<- deimsUUID
  }
  
  #' return the currently selected site record as a tibble with one row
  getSite <- function() {
    sites %>%
      dplyr::filter(deimsUUID == selected_site)
  }
  
  #' get the available EVs for the currently selected site (they depend on the domain of the site)
  getCurrentSiteAvailableEVs <- function(){
    suppressMessages(
      ll <- EVs %>%
        dplyr::inner_join(
          getSite() %>%
            dplyr::select(domain)
        )
    )
    if (length(ll)==0) return("")
    ll
  }
  
  #' Set the selected EV. Warn if the `id` is not present in the `EVs` object
  setEv <- function(id){
    if(!id %in% getCurrentSiteAvailableEVs()$id) {
      warning("EV must be specified by its id and must be available for the current site")
      return
    }
    selected_ev <<- id
  }
  
  #' return the currently selected EV record as a tibble with one row
  getEv <- function() {
    EVs %>%
      dplyr::filter(
        id == selected_ev
      ) 
  }
  
  #' return th named list of site deimsUUID, useful for selectizeInput in shiny UI
  siteList <- function() {
    sites$deimsUUID %>%
      magrittr::set_names(
        sites$name
      )
  }
  
  #' return the named list of (current site's) EV ids, useful for selectizeInput in shiny UI
  EVsList <- function() {
    curev <- getCurrentSiteAvailableEVs() %>% 
      dplyr::mutate(
        label = sprintf("%s (%s) - %s", name, type, domain)
      )
    ll <- curev$id %>%
      magrittr::set_names(curev$label)
    append(list("<select one>"=""),ll)
  }
  
  
  init <- function() {
    # init internal vars
    sites <<- readRDS(file = "static_data/Sites_list.RDS")
    EVs <<- readRDS(file = "static_data/EVs.RDS")
    evdatasets <<- readRDS(file = "static_data/datasets.RDS")
    deimsUUID = "f30007c4-8a6e-4f11-ab87-569db54638fe"
    setSite(deimsUUID)
  }
  init()
  
  # TODO: METODI PER RETRIEVAL IN BASE ALLA SCELTA DEL SITO E DELLA EV
  # 1. info generali sul sito -> lista (Key:Val)?  contiene anche geometria per fare plot
  # 2. info generali sulla EV -> lista (Key:Val)?
  # -- -- --
  # 3. dataset che contribuiscono alla EV selezionata (questi li stiamo raccogliendo, saranno inseriti in data storage)
  #      qui per ora solo la boa https://deims.org/locations/45409faa-b33f-496e-919d-e442921bd923
  # 
  # 4. dataset accessori es. accessibili da ReLTER come GBIF. Altri (presenti in DEIMS/meteo preso da qualche parte) ?
  #    (see: https://docs.ropensci.org/ReLTER/articles/occurrences_into_site.html)
  #      esporre: numero dei dataset (fisso? oppure lancio il calcolo con relter e conto solo quelli con almeno un dato?)
  #      esporre tabella dei dataset con: nome, fonte, PID.
  #         ho bisogno di esempio. Per GBIF etc: GBIF <numero occorrenze trovate> <pid ???? se fosse un link per scaricarsi il CSV? >
  #  
  # 5. dataset (METADATI) che rispondono alle parole chiave nome sito e (nome variabile (?)) in repo quali: 
  #    TODO: nella tabella dei siti (excel) aggiungere nomi alternativi del sito da usare nella ricerca p.es. su zenodo ("lake maggiore" e non "lago maggiore")
  #    DEIMS (related resources), Zenodo, Pangea, B2Share
  #      esporre: numero dei dataset
  #      esporre tabella dei dataset con: nome, fonte, PID (link al metadato)
  
  #' Info object for the selected site
  #' @description the function returns, for the current selected site, a list with several slot<s.
  #' slots have the naming convention: <type>_<name>. the content of the slot is given by <type>:
  #' <type> = val: single value |
  #'          tbl: a tibble |
  #'          stats3num: c(min, mean, max) |
  #'          sfc: an `sf` object |
  #'          wkt: a well known text (e.g. coordinates POINT(...)) 
  #'          
  #' @return list with slots:
  #'  sfc_boundariesPolygon 
  #'  val_title 
  #'  val_uri 
  #'  wkt_coordinates 
  #'  val_geoBonBiome 
  #'  val_biogeographicalRegion 
  #'  tbl_eunisHabitats 
  #'  stats3num_elevation 
  #'  val_airTempYearlyAvg 
  #'  val_precipitation 
  #'  tbl_observedProperties 
  #'  tbl_relatedResources 
  #'  tbl_dataPolicyRights
  #'  
  info_site <- function() {
    if(is.null(cacheInfoSite[[selected_site]])) {
      res <- list()
      # presentare:
      # nome sito (linkabile)
      # elevation (min, mean, max) per es. con sliders? Altrimenti un plot su scala fissa con tre punti o un boxplot
      #   così quando cambia il sito si vede a occhio la collocazione della quota
      # coordinate, ev. fare mappa italia con il solo punto, non zoomabile. C'è fx in ReLTER?
      #
      # -> da EnvCharacts
      #   - bioma
      # -> da Infrastructure 
      #   - prenderei eventualmente i rights generalInfo.data.policy.rights[[1]] # array di testo
      #   - 
      # 
      res1 <- ReLTER::get_site_info(
        deimsid = paste0("https://deims.org/", selected_site),
        category = c(
          #"Boundaries", 
          "EnvCharacts",
          #"Affiliations",
          #"observedProperties",
          "RelateRes"#,
          #"Infrastructure"
        )
      )
      
      res$tbl_generalInfo <- res1 %>%
        dplyr::select(geoBonBiome,
          biogeographicalRegion#, 
          #all_of(starts_with("geoElev.")),
          #all_of(starts_with("airTemperature."))
        )
      res$val_title                 <- res1$title
      res$val_uri                   <- res1$uri
      res$wkt_coordinates           <- res1$geoCoord
      # res$sfc_boundariesPolygon     <- NULL #res1$boundaries
      # res$val_geoBonBiome           <- res1$geoBonBiome
      # res$val_biogeographicalRegion <- res1$biogeographicalRegion
      # elevunit<-units::as_units("m") # elev unit in deims is msl, not recognized by udunits
      # res$stats3num_elevation    <- units::set_units(c(min  = res1$geoElev.min, 
      #                                                  mean = res1$geoElev.avg, 
      #                                                   max = res1$geoElev.max), 
      #                                                 "m")
      # res$val_airTempYearlyAvg   <- units::set_units(res1$airTemperature.yearlyAverage, 
      #                                        res1$airTemperature.unit, mode = "standard")
      # res$val_precipitation      <- units::set_units(res1$precipitation.yearlyAverage, 
      #                                     res1$precipitation.unit,mode = "standard")
      # tables
      res$tbl_eunisHabitats      <- res1$eunisHabitat[[1]] %>% as_tibble() %>% dplyr::select(-uri)
      # res$tbl_observedProperties <- res1$observedProperties[[1]] %>% as_tibble()
      res$tbl_relatedResources   <- res1$relatedResources[[1]] %>% as_tibble()
      # res$tbl_dataPolicyRights   <- res1$generalInfo.data.policy.rights[[1]] %>% as_tibble()
      cacheInfoSite[[selected_site]] <<- res
    }
    return(cacheInfoSite[[selected_site]])
  }
  
  info_ev <- function() {
    
  }
  
  # search EVsData ----
  
  # results EVsData ----
  getEVsData <- function() {
    evdatasets %>%
      dplyr::filter(deimsUUID == selected_site, ev_id == selected_ev) %>% 
      dplyr::mutate(
          url = sprintf("<a href='%s' target='_blank'>%s<a>", url, url),
          resources = "dataset",
          source = paste0("<a href='", repo, "' target = '_blank'><img src='", icon_url, "' height='52'/></a>"),
          title = datasetname,
          .keep = "unused"
        ) %>%
      dplyr::select(source, url, title, resources)
    
  }
  
  # searches OtherResData ----
  # GBIF
  search_gbif <- function() {
    occ <- ReLTER::get_site_speciesOccurrences(
      deimsid = paste0("https://deims.org/", selected_site),
      list_DS = "gbif",
      exclude_inat_from_gbif = TRUE,
      show_map = FALSE,
      limit = 500
    )
    occ <- occ$gbif
  }
  # iNat
  search_inat <- function() {
    occ <- ReLTER::get_site_speciesOccurrences(
      deimsid = paste0("https://deims.org/", selected_site),
      list_DS = "inat",
      show_map = FALSE,
      limit = 500
    )
    occ <- occ$inat
  }
  # OBIS
  search_obis <- function() {
    occ <- ReLTER::get_site_speciesOccurrences(
      deimsid = paste0("https://deims.org/", selected_site),
      list_DS = "obis",
      show_map = FALSE,
      limit = 500
    )
    occ <- occ$obis
  }
  # other Mica's datasets
  # TODO: ...
  
  # results OtherResData [cached] ----
  getOtherResData <- function() {
    
    cached <- readFromCache("getOtherResData", selected_site, selected_ev)
    if (is.null(cached)) {
      
    if (nrow(search_gbif()) == 500) {
      gbif_occ <- "more than 500"
    } else {
      gbif_occ <- nrow(search_gbif())
    }
    gbif_uri <- search_gbif() %>%
      dplyr::select(datasetKey) %>%
      `st_geometry<-`(., NULL) %>%
      unique()
    resultsGBIF <- tibble::tibble(
      source = "<a href='https://gbif.org/' target = '_blank'><img src='https://www.gbif.no/services/logo/gbif-dot-org.png' height='52'/></a>",
      url = sprintf("<a href='https://www.gbif.org/dataset/%s' target='_blank'>https://www.gbif.org/dataset/%s<a>", gbif_uri, gbif_uri),
      title = "Species occurrences in the area surrounding the site",
      resources = paste(gbif_occ, "specie occurrences")
    )
    
    # resultsINat
    if (nrow(search_inat()) == 500) {
      inat_occ <- "more than 500"
    } else {
      inat_occ <- nrow(search_inat())
    }
    site_name <- getSite() %>% pull(name) %>% stringr::str_replace(pattern = " ", replacement = "-") %>% stringr::str_to_lower()
    resultsINat <- tibble::tibble(
      source = "<a href='https://www.inaturalist.org' target = '_blank'><img src='https://static.inaturalist.org/sites/1-logo_square.png' height='52'/></a>",
      url = paste0(
        "<a href='https://www.inaturalist.org/projects/lter-site-",
        site_name, "' target = '_blank'>",
        "https://www.inaturalist.org/projects/lter-site-", site_name,
        "</a>"
      ),
      title = "Species occurrences in the area surrounding the site",
      resources = paste("more than", inat_occ, "specie occurrences")
    )
    
    # resultsOBIS
    if (nrow(search_obis()) == 500) {
      obis_occ <- "more than 500"
    } else {
      obis_occ <- nrow(search_obis())
    }
    resultsOBIS <- tibble::tibble(
      source = "<a href='https://obis.org' target = '_blank'><img src='https://classroom.oceanteacher.org/pluginfile.php/43689/course/overviewfiles/obis-logo-moodle.png' height='52'/></a>",
      url = paste0("-"),
      title = "Species occurrences in the area surrounding the site",
      resources = paste("more than", obis_occ, "specie occurrences")
    )
    
    results <- resultsGBIF %>% 
      dplyr::add_row(resultsINat) %>%
      dplyr::add_row(resultsOBIS)
    
    writeToCache(results ,"getOtherResData", selected_site, selected_ev)
    cached<-results
    }
    return(cached)
  }
  
  keyword_data <- function() {
    return("TBD")
  }
  
  # # TODO: check this if useful
  {
  # listFromSparql<-function(){
  #   ev<-getEv()
  #   ev<-"Phenology of marine spring phytoplankton bloom"
  #   #ev<-"seagrass"
  #   ev<-"phytoplankton"
  #   #site<-getSite()  \
  #   endpoint<-"http://graph.oceaninfohub.org/blazegraph/namespace/oih/sparql/oih/sparql"
  #   
  #   query<-paste0('
  #   PREFIX sc: <http://purl.org/science/owl/sciencecommons/>
  #   PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
  #   PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  #   PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
  #   prefix prov: <http://www.w3.org/ns/prov#>
  #   PREFIX schema: <https://schema.org/>
  #   PREFIX schemaold: <http://schema.org/>
  #   
  #   SELECT * WHERE {
  #   {
  #     SELECT ?resource ?name
  #     WHERE  {
  #       ?resource schema:keywords "',stringr::str_to_title(ev),'" .
  #       ?resource schema:name ?name
  #     }
  #   }
  #     UNION
  #   {
  #     SELECT ?resource ?name
  #     WHERE {
  #       ?resource schema:keywords "',ev,'" .
  #       ?resource schema:name ?name
  #     }
  #   }
  #     UNION
  #   {
  #     SELECT ?resource ?name
  #     WHERE {
  #       ?resource schema:description ?description .
  #       ?resource schema:name ?name
  #         filter contains(?description,"',ev,'") 
  #     }
  #   }}')
  #   rdf <- rdflib::rdf_parse(endpoint)
  #   df<-rdf_query(rdf, query)
  # df  
  # }
  # 
  # 
  # define self (what to export)
  }
  
  {
  # get_site_bbx<-function(deimsUUID){
  #   prep<-"https://deims.org/geoserver/deims/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=deims:deims_sites_bboxes&srsName=EPSG:4326&outputFormat=application%2Fjson&CQL_FILTER=deimsid="
  #   
  #   #comp<-"https://deims.org/geoserver/deims/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=deims:deims_sites_bboxes&srsName=EPSG:4326&outputFormat=application%2Fjson&CQL_FILTER=deimsid=%27https://deims.org/6869436a-80f4-4c6d-954b-a730b348d7ce%27"
  #   url.geoserver<-paste0(prep, URLencode(paste0("'https://deims.org/", deimsUUID,"'")))
  #   geoBoundaries <- geojsonsf::geojson_sf(url.geoserver)
  #   plot(geoBoundaries)
  #   return(geoBoundaries)
  # }
  }
  
  # https://deims.org/geoserver/deims/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=deims:deims_sites_boundaries&srsName=EPSG:4326&CQL_FILTER=deimsid=%27https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe%27&outputFormat=application%2Fjson
  
  # https://deims.org/geoserver/deims/ows?service=WFS&version=2.0.0&request=GetFeature&typeName=deims:deims_sites_bboxes&srsName=EPSG:4326&outputFormat=application%2Fjson&CQL_FILTER=deimsid=%27https://deims.org/6869436a-80f4-4c6d-954b-a730b348d7ce%27
  
  # searches OtherRepoData ----
  # Pangaea dataset ----
  search_pangaea <- function() {
    deimsid <- paste0("https://deims.org/", selected_site)
    #deimsid<-selected_site
    #ReLTER::get_site_boundaries(deimsid)
    boundary <- ReLTER::get_site_info(
      deimsid,
      category = "Boundaries"
    )
    if (is.null(boundary) || !inherits(boundary, "sf")) {
      print("No boundary for requested DEIMS site.")
      return(NULL)
    } else {
      bbox <- sf::st_bbox(boundary) %>%
        as.double()
    }
    pgRecords <- pangaear::pg_search(
      query = '*',
      bbox = c(bbox[1], bbox[2], bbox[3], bbox[4])
    )

    return(pgRecords)
  }
  # Zenodo dataset ----
  search_zenodo <- function() {
    site_name <- getSite() %>% pull(alt_name)
    zenodo <- zen4R::ZenodoManager$new(
      url = "https://zenodo.org/api",
      logger = "INFO"
    )
    zenodo_records <- zenodo$getRecords(
      q = URLencode(
        sprintf('title:"%s" description:"%s"', site_name, site_name)
      ),
      size = 10
    )
    records <- zenodo_records %<>% 
      lapply(function(n) {
        tibble::tibble(
          title = n$metadata$title,
          uri = n$pids$doi$identifier,
          resources = n$metadata$resource_type$id
        )
      }) %>%
      dplyr::bind_rows()
  }
  # results OtherRepoData [cached] ----
  getOtherRepoData <- function() {
    cached <- readFromCache("getOtherRepoData", selected_site, selected_ev)
    if (is.null(cached)) {
    resultsPangaea <- search_pangaea() %>% 
      dplyr::mutate(
        url = sprintf("<a href='https://doi.org/%s' target='_blank'>%s<a>", doi, doi),
        resources = paste(size, size_measure),
        source = "<a href='https://pangaea.de' target = '_blank'><img src='https://store.pangaea.de/documentation/PANGAEA-Wiki/Logo/PANGAEA_Logo_2.png' height='52'/></a>",
        title = citation,
        .keep = "unused"
        ) %>%
      dplyr::select(source, url, title, resources)
    
    # TODO: complete decoding the resources types in DEIMS SDR.
    # TODO: create png for sources from their original images, in order not to request them too many time.
    resultsDEIMS <- tibble::tibble(
      source = NA,
      url = NA,
      title = NA,
      resources = NA
    )
    debug_insi <- info_site()$tbl_relatedResources
    if ("relatedResourcesId" %in% names(debug_insi)) {
      debug_insi <- debug_insi %>%
        dplyr::select(
          relatedResourcesTitle, relatedResourcesChanged, uri = relatedResourcesId
        )
    }
    if(any(!is.na(debug_insi$uri))) {
      resultsDEIMS <- debug_insi %>%
        dplyr::mutate(
          title = relatedResourcesTitle,
          url = sprintf("<a href='https://doi.org/%s' target='_blank'>%s<a>", uri, uri),
          source = "<a href='https://deims.org/' target = '_blank'><img src='https://elter-ri.eu/storage/app/uploads/public/637/61a/13d/63761a13d4ca2866772974.svg' height='52'/></a>",
          resources = case_when(
            stringr::str_detect(uri, stringr::fixed("dataset")) ~ "dataset",
            stringr::str_detect(uri, stringr::fixed("sensor")) ~ "sensor",
            TRUE ~ "other"
          ),
          .keep="unused"
        ) %>%
        dplyr::select(source, url, title, resources)
    }
    
    # TODO: filter only dataset tip. Query for elasticsearch is "q = resource_type.type:dataset"
    resultsZenodo <- search_zenodo() %>%
      dplyr::mutate(
        title = title,
        url = sprintf("<a href='https://doi.org/%s' target='_blank'>%s<a>", uri, uri),
        source = "<a href='https://zenodo.org/' target = '_blank'><img src='https://about.zenodo.org/static/img/logos/zenodo-gradient-200.png' height='52'/></a>",
        resources = resources,
        .keep="unused"
      ) %>%
      dplyr::select(source, url, title, resources)
    
    # TODO: complete the following with other sources e.g. ??? etc.
    
    # NOTE: Columns must be source, url, resources, title
    # TODO: transform source columns as factor example: vinili$`Collection Media Condition` = factor(vinili$`Collection Media Condition`, labels = c("Mint (M)", "Near Mint (NM or M-)"))
    results <- resultsDEIMS %>% 
      dplyr::add_row(resultsPangaea) %>%
      dplyr::add_row(resultsZenodo)

    writeToCache(results ,"getOtherRepoData", selected_site, selected_ev)
    cached<-results
    }
    
    return(cached)
  }
  
  self <- list(
    "setSite" = setSite,
    "getSite" = getSite,
    "siteList" = siteList,
    "siteNames" = sites$name,
    "getCurrentEVs" = getCurrentSiteAvailableEVs,
    "EVsList" = EVsList,
    "evNames" = EVs$name,
    "setEv" = setEv,
    "getEv" = getEv,
    "getInfo_Site" = info_site,
    "getOtherResData" = getOtherResData,
    "getOtherRepoData" = getOtherRepoData,
    "getEVsData"=getEVsData
  )
  
  
  # self$getSelectedSiteDeimsid<-function(){
  #   return(selected_site)
  # }
  
  print("broker.R: finished preparation")
  # export
  return(self)
}

# example usage
if(FALSE){
  b <- getBroker()
  deimsUUID = "f30007c4-8a6e-4f11-ab87-569db54638fe"
  b$setSite(deimsUUID)
  b$getSite()
  b$getInfo_Site() # the first call for a given site takes more time, result is then cached and quickly accessible
  
  b$setSite(b$siteList()[2])
  b$getSite()
  b$getInfo_Site()
  
  
  b$getCurrentEVs()
  b$setEv(b$getCurrentEVs()$id[1]) # select the first available EV for the selected site
  b$getSite()
  b$getEv()
  
  # alt name of site
  b$getSite() %>% dplyr::pull(alt_name)
  
}
#
