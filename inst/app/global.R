#source("broker.R")

broker<-getBroker()
# meglio qui o nel broker/controller?

# DONE: convert these lists into tbl with columns "deimsid", "name", "domain", "active"
# where `domain` contains one of "marine", "terrestrial", "freshwater"
# and active is a (temporary) flag to indicate if the site is selectable by the user or not.
# We can also avoid to put them to the list, but I would consider this for future use
# sites_list <- list(
#   "Lago Maggiore - Italy" = "https://deims.org/f30007c4-8a6e-4f11-ab87-569db54638fe",
#   "Delta del Po e Costa Romagnola - Italy" = "https://deims.org/6869436a-80f4-4c6d-954b-a730b348d7ce"
# )

# DONE: convert into tbl.
# Columns:
# - ev_id (the same used in deliverable and excel SelectedEvs...)
# - name
# - domain: same as the sites table
# - definition url
# - type (ebv|ecv)
# - description
# - ... (add anything useful)
# This table can be linked to another one with single distinct sub-variables (we can simply read Foglio2 of Excel file, if it is useful)
# EVs_list <- list(
#   "Ocean, Biogeochemical: Nutrients" = "https://gcos.wmo.int/en/essential-climate-variables/nutrients/",
#   "Ocean, Biogeochemical: Ocean Colour" = "https://gcos.wmo.int/en/essential-climate-variables/ocean-colour/",
#   "Ocean, Biogeochemical: Oxygen" = "https://gcos.wmo.int/en/essential-climate-variables/oxygen/",
#   "Phenology of marine spring phytoplankton bloom" = "https://github.com/EuropaBON/EBV-Descriptions/wiki/Marine-Phenology-of-marine-spring-phytoplankton-bloom",
#   "Marine ecosystem productivity" = "https://github.com/EuropaBON/EBV-Descriptions/wiki/Marine-Marine-ecosystem-productivity",
#   "Surface temperature" = "https://gcos.wmo.int/en/essential-climate-variables/land-temperature/",
#   "Precipitation" = "https://gcos.wmo.int/en/essential-climate-variables/precipitation",
#   "Terrestrial ecosystem productivity" = "https://github.com/EuropaBON/EBV-Descriptions/wiki/Terrestrial-Terrestrial-ecosystem-productivity",
#   "Species distributions of selected terrestrial plants" = "https://github.com/EuropaBON/EBV-Descriptions/wiki/Terrestrial-Species-distributions-of-selected-terrestrial-plants",
#   "Terrestrial ecosystem phenology" = "https://github.com/EuropaBON/EBV-Descriptions/wiki/Terrestrial-Terrestrial-ecosystem-phenology",
#   "Lake Surface Water Temperature (LSWT)" = "https://gcos.wmo.int/en/essential-climate-variables/lakes",
#   "Lake Water Level (LWL)" = "https://gcos.wmo.int/en/essential-climate-variables/lakes",
#   "Freshwater ecosystem productivity" = "https://github.com/EuropaBON/EBV-Descriptions/wiki/Freshwater-Freshwater-ecosystem-productivity"
# )
