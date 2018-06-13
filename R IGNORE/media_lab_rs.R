#tracking papert 
#load utils first!!

process.publications <- function(pp_data)
{
  pp_data <- pp_data[pp_data$Source != '',]
  pp_data <- pp_data[pp_data$Year != '',]
  
  pp_data['journal_min'] <- sapply(pp_data[, c('Source')], function(x) reduce_name_journal(x))
  
  pp_data['journal_id'] <- sapply(pp_data[, c('journal_min')], function(x) find_id_journal(x))
  
  print("Total number of publications:")
  print(nrow(pp_data))
  
  #keeping data with id_journal
  pp_data <- pp_data[!is.nan(pp_data$journal_id),c(1,2,3,4,21)]
  print("Number of publications matched with a journal:")
  print(nrow(pp_data))
  
  return(pp_data)
  
}

example_proc_media <- function()
{
  #hidden as function
  
  setwd("/Users/mguevara/Dropbox/UPLA/INVESTIGACION/P ESTRELLAS/RESEARCH SPACE TO LIVE")
  
  #processing the complete datasets, only if csv file not created yet.
  datasets = c("DATA") #directories
  
  pub_total = data.frame()
  for(dataset in datasets )
  {
    print(paste("DATASET: ", dataset))
    files = list.files(dataset)
    
    for(file in files)
    {
      print(paste("    File: ", file))
      pub <- read.csv(file.path(dataset, file)) 
      pub_total <- rbind(pub_total, process.publications(pub))
      
    }
    
  }
  
  #removing duplicates
  print(paste("Number of publications matched with journals", nrow(pub_total)))
  pub_total <- pub_total[!duplicated(pub_total),]
  names(pub_total)[1] <- "Cites"
  print(paste("Number of UNIQUE publications matched with journals", nrow(pub_total)))
  
  print(paste("Numero total de journals unicos", length(unique(pub_total$journal_id))))
  
  
  #backuping publications to work with UNCOMENT
  #write.csv(pub_total, file = "DATA_OUTPUT/publications_impact.csv", row.names = FALSE)
  
  
  #IF FILE ALREADY CREATED!
  pub_total <- read.csv(file = "DATA_OUTPUT/publications_impact.csv")
  nodes <- load_taxonomy(taxo="scimago")
  j_c <- load_journals_category(taxo="scimago")
  # Pub. 8+568+645+559+615+555+643+252+546+628+464+431+373+481+304+314+342+196
  
  #merging publications, journals, categories
  publications <- merge(pub_total, j_c, by.x = "journal_id", by.y = "id_journal")
  
  #adding names to categories
  publications <- merge(publications, nodes, by.x="id_category", by.y = "Id")
  
  library(d3plus) #name, group, value, color
  d3plus(type="tree", data= publications[,c("Title", "subd_name", "area_name", "Cites", "color") ], depth=0 )
  
  #save HTML to external file
  s<-d3plus(type="tree", data= publications[,c("Title", "subd_name", "area_name", "Cites", "color") ], depth=0 )
  htmlwidgets::saveWidget(s,"categories.html", selfcontained = FALSE)
  
  #FRECUENCIAS, para calcular SIZE
  publications_category <- as.data.frame(table(publications$id_category))
  publications_category <- merge(publications_category, nodes, by.x = "Var1", by.y="Id")
  
  #NETWORK Research Space
  #adding size
  #rs_scimago <- load(file="/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/RESEARCH_SPACE/SCIMAGO/RESEARCH\ SPACE\ OUTPUT/New_rs_sim_pr_1971_2010_n_-1_aw_0_jf_0_awjf_0.1_min_prod_0.RData")
  #LOAD PLOTRSpaceFirst!!
  rs_scimago_m <- create_rs_simago() #object iGraph!
  #create a function here
  
  get_position <- function(g, seed=69)
  {
    set.seed(seed)
    position= as.data.frame(layout.fruchterman.reingold(g))
    names(position) <- c("x","y")
    position['id'] <- as.integer(V(g)$name)
    return(position)
  }
  position = get_position(rs_scimago_m)
  
  edges <- as.data.frame(get.edgelist(rs_scimago_m, names = TRUE))
  names(edges)[1:2] <- c("source", "target")
  #edges_1 <- merge(edges, nodes, by.x='source', by.y='Id')
  nodes['id'] <- nodes$Id
  #e_source <- as.data.frame(edges[,1])
  #names(e_source) <- "source"
  #e_target <- as.data.frame(edges[,2])
  #names(e_target) <- "target"
  #e_source <- merge(e_source, nodes[,c(1,3)], by.x="source", by.y="Id")
  
  used <- E(rs_scimago_m)$used
  weight <- E(rs_scimago_m)$weight
  edges <- cbind(edges,weight, used)
  
  
  #nodes["id"] <- nodes$subd_name
  nodes["name"] <- nodes$subd_name
  #names(nodes)[1] <- "Id_back"
  #names(nodes)[1] <- "Id"
  
  nodes['group'] <- nodes$area_name
  publications_category['size'] <- publications_category$Freq
  
  #here starts the publication nodes configuration
  nodes_plot <- merge(nodes, publications_category[,c("Var1","size")], by.x="Id", by.y = "Var1" , all.x = TRUE )
  
  nodes_plot <- merge(nodes_plot, position, by="id")
  #nodes_plot['size'] <- nodes$size.x *50
  #droplevels(nodes_plot)
  
  #nodes_plot$color <- droplevels(nodes_plot$color)
  #nodes_plot$color <- lapply(nodes_plot$color, FUN = "toString")
  #nodes_plot$color <- nodes_plot$color
  nodes_plot[is.na(nodes_plot$size), c("color")] <-NA
  #droplevels(nodes_plot)
  #nodes_plot[is.na(nodes_plot$size), "color"] <- "undefined"
  #nodes_plot[, c("color")] <- "#ffffff"
  #nodes_plot$size<- nodes_plot$size.x/max(nodes_plot$size.x, na.rm = TRUE)
  #nodes_plot$size<-100
  #nodes_plot$size <- nodes_plot$size.x
  #nodes_plot['size.x'] <- NULL
  #nodes_plot['size.y'] <- NULL
  nodes_plot[is.na(nodes_plot$size),c('size')] <- 1
  #nodes_plot$size <- nodes_plot$size *100
  d3plus(type="network", edges[edges$used>0,], nodes=nodes_plot )
  pl_cat <- d3plus(type="network", edges[edges$used>0,], nodes=nodes_plot)
  htmlwidgets::saveWidget(pl_cat,"rs_KK.html", selfcontained = FALSE)
  
  
}
