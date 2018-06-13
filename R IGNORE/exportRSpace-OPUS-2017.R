#Export RSpace
#source PLOTRSPace.R
#' @export
export_rspace <- function()
{
  nodes<-load_taxonomy(taxo="scimago")
  scimago<- create_rs_simago()
  
  
  rs.edges <- as.data.frame(get.edgelist(scimago))
  rs.edges['weight'] <- E(scimago)$weight
  rs.edges['used'] <- E(scimago)$used
  
  write.csv(rs.edges, file= "/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/RESEARCH_SPACE/SCIMAGO/RESEARCH\ SPACE\ OUTPUT/scimago_links.csv") 
  
  
  new_nodes = data.frame(nodes$Id, nodes$subd_name)
  names(new_nodes) = c("id", "name_category")
  write.csv(new_nodes, file="/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/TAXONOMIES/SCIMAGO/nodes_id_names.csv")
  l = layout()
  
}
