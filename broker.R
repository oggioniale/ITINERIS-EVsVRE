require(dplyr)
require(magrittr)
require(ReLTER)
require(rdflib)

#' Factory method to get a stateful controller (broker of data).
#' @description The factory creates a
#' controller object. It has state (selection of LTER site and Essential Variable). Broker of data.
#' 
#' The object exposes the methods:
#' 
#' setSite: set selected site (via its deimsid. It must be present in the sites table)
#' getSite: return the currently selected site record as a `tibble` with one row
#
#' siteList: return th named `list` of site deimsid, useful for selectizeInput in shiny UI
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
#' 
getBroker=function(){
  self=list()
  
  # declare internal vars
  {
    #' deimsid (not the url)
    selected_site <- ""
    #' id of the EV as in the EVs tibble
    selected_ev <- ""
    #' tibble datasets with selectable sites and EVs
    sites <- NULL
    EVs <- NULL
    
    # a list to store info for sites, in order not to call many times external repo (DEIMS)
    cacheInfoSite <- list()
  }
  
  #' set selected site (via its deimsid. It must be present in the sites table)
  setSite<-function(deimsid){
    if(!deimsid %in% sites$deimsid) {
      warning("Site must be specified by its deimsid")
      return
    }
    selected_site<<-deimsid
  }
  
  #' return the currently selected site record as a tibble with one row
  getSite<-function(){
    sites %>% dplyr::filter(deimsid==selected_site)
  }
  
  #' get the available EVs for the currently selected site (they depend on the domain of the site)
  getCurrentSiteAvailableEVs<-function(){
    suppressMessages(
    ll<-EVs %>% dplyr::inner_join(getSite() %>% dplyr::select(domain))
    )
    if(length(ll)==0) return("")
    ll
  }
  
  #' Set the selected EV. Warn if the `id` is not present in the `EVs` object
  setEv<-function(id){
    if(!id %in% getCurrentSiteAvailableEVs()$id) {
      warning("EV must be specified by its id and must be available for the current site")
      return
    }
    selected_ev <<- id
  }
  
  #' return the currently selected EV record as a tibble with one row
  getEv<-function(){
    EVs %>% dplyr::filter(id==selected_ev) 
  }
  
  #' return th named list of site deimsid, useful for selectizeInput in shiny UI
  siteList<-function(){
    sites$deimsid %>% magrittr::set_names(sites$name)
  }
  
  #' return the named list of (current site's) EV ids, useful for selectizeInput in shiny UI
  EVsList<-function(){
    curev<-getCurrentSiteAvailableEVs() %>% 
      dplyr::mutate(label=sprintf("%s (%s) - %s", name, type, domain))
    ll<-curev$id %>% magrittr::set_names(curev$label)
    append(list("<select one>"=""),ll)
  }
  
  
  init<-function(){
    # init internal vars
    sites <<- readRDS(sites,file = "static_data/Sites_list.RDS")
    EVs <<- readRDS(EVs,file = "static_data/EVs.RDS")
    deimsid = "f30007c4-8a6e-4f11-ab87-569db54638fe"
    setSite(deimsid)
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
  info_site<-function(){
    if(is.null(cacheInfoSite[[selected_site]])){
      res<-list()
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
      res1<-ReLTER::get_site_info(deimsid = selected_site, category = c(#"Boundaries", 
        "EnvCharacts", "Affiliations", "observedProperties", "RelateRes", "Infrastructure"))
      
      res$tbl_generalInfo <- res1 %>% dplyr::select(title, uri, geoCoord, 
                                                    envCharacteristics.geoBonBiome, envCharacteristics.biogeographicalRegion, 
                                                    all_of(starts_with("geoElev.")),
                                                    all_of(starts_with("envCharacteristics.airTemperature."))
                                                    )
                                                    
                                                           
      
      res$val_title                 <- res1$title
      res$val_uri                   <- res1$uri
      res$wkt_coordinates           <- res1$geoCoord
      
      res$sfc_boundariesPolygon     <- NULL #res1$boundaries
      
      res$val_geoBonBiome           <- res1$envCharacteristics.geoBonBiome
      res$val_biogeographicalRegion <- res1$envCharacteristics.biogeographicalRegion
      
      #elevunit<-units::as_units("m") # elev unit in deims is msl, not recognized by udunits
      res$stats3num_elevation    <- units::set_units(c(min  = res1$geoElev.min, 
                                                       mean = res1$geoElev.avg, 
                                                        max = res1$geoElev.max), 
                                                      "m")
      res$val_airTempYearlyAvg   <- units::set_units(res1$envCharacteristics.airTemperature.yearlyAverage, 
                                             res1$envCharacteristics.airTemperature.unit, mode = "standard")
      res$val_precipitation      <- units::set_units(res1$envCharacteristics.precipitation.yearlyAverage, 
                                          res1$envCharacteristics.precipitation.unit,mode = "standard")
      
      # tables
      res$tbl_eunisHabitats      <- res1$envCharacteristics.eunisHabitat[[1]] %>% as_tibble() %>% dplyr::select(-uri)
      res$tbl_observedProperties <- res1$observedProperties[[1]] %>% as_tibble()
      res$tbl_relatedResources   <- res1$relatedResources[[1]] %>% as_tibble()
      res$tbl_dataPolicyRights   <- res1$generalInfo.data.policy.rights[[1]] %>% as_tibble()
      
      cacheInfoSite[[selected_site]] <<- res
    }
    return(cacheInfoSite[[selected_site]])
  }
  
  info_ev<-function(){
    
  }
  
  # in parte questi possono essere direttamente quelli 
  ancillary_data<-function(){
    info_site()$tbl_relatedResources
  }
  
  keyword_data<-function(){return("TBD")}
  
  
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
  
  self <- list(
    "setSite"=setSite,
    "getSite"=getSite,
    "siteList"=siteList,
    "siteNames"=sites$name,
    "getCurrentEVs"=getCurrentSiteAvailableEVs,
    "EVsList"=EVsList,
    "evNames"=EVs$name,
    "setEv"=setEv,
    "getEv"=getEv,
    "getInfo_Site"=info_site
  )
  
  
  self$test<-function(){
    return(selected_site)
  }
  
  
  print("broker.R: finished preparation")
  # export
  return(self)
}

# example usage
if(FALSE){
  b <- getBroker()
  deimsid = "f30007c4-8a6e-4f11-ab87-569db54638fe"
  b$setSite(deimsid)
  b$getSite()
  b$getInfo_Site() # the first call for a given site takes more time, result is then cached and quickly accessible
  
  b$setSite(b$siteList()[2])
  b$getSite()
  b$getInfo_Site()
  
  
  b$getCurrentEVs()
  b$setEv(b$getCurrentEVs()$id[1]) # select the first available EV for the selected site
  b$getSite()
  b$getEv()
  
}
#