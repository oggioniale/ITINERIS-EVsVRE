require(dplyr)
require(magrittr)
require(ReLTER)

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
    #' tibble datasets with sites and EVs
    sites <- NULL
    EVs <- NULL
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
  
  #' return the info requested to the underlying ReLTER method get_site_info for the specified deimsid and category
  getSiteInfo<-function(deimsid, category=c("Boundaries", "EnvCharacts" )){
    ReLTER::get_site_info(deimsid, category)
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
  # 4. dataset accessori es. accessibili da ReLTER come GBIF. Altri (presenti in DEIMS/meteo preso da qualche parte) ?
  # 5. dataset che rispondono alle parole chiave nome sito e variabile in repo quali: DEIMS, Zenodo, Pangea, B2Share
  
  
  
  # define self (what to export)
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
    #
    "f_getSiteInfo"=getSiteInfo
  )
  
  print("broker.R: finished preparation")
  # export
  return(self)
}

if(FALSE){
  b <- getBroker()
  deimsid = "f30007c4-8a6e-4f11-ab87-569db54638fe"
  b$setSite(deimsid)
  b$getCurrentEVs()
  b$setEv(b$getCurrentEVs()$id[1]) # select the first available EV for the selected site
  b$getSite()
  b$getEv()
  b$selected_site()
}
#
