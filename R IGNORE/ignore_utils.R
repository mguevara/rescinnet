#'ignore, just used to build data
#'create data for Scimago Taxonomy
create_scimago_taxo <- function()
{
  scimago_nodes <- read.csv("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/TAXONOMIES/SCIMAGO/nodes.csv")    
  save(scimago_nodes, file=file.path("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/codes/scimaps/data", "scimago.RData"))
}

create_ucsd_taxo <- function()
{
  ucsd_nodes <- read.csv("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/TAXONOMIES/UCSD/nodes.csv")    
  save(ucsd_nodes, file=file.path("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/codes/scimaps/data", "ucsd.RData"))
}

create_data_rs_ucsd <- function()
{
  load("/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/RESEARCH_SPACE/UCSD/RESEARCH SPACE OUTPUT/RS_iGRAPH_UCSD.RData")
  
  rs_ucsd <- upgrade_graph(rs)
  save(rs_ucsd, file=file.path("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/codes/scimaps/data", "rs_ucsd.RData"))
}