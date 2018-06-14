##plot research space
#frebrua
#remember to source config.R and createResearchSpace.R
#config(cl='ucsd')
#' @export
create_rs_ucsd <- function()
{
	file_rdata<- "New_rs_sim_pr_1971_2010_n_-1_aw_0_jf_0_awjf_0.1_min_prod_0.RData"
	mean_degree <- 10
	#for(pl_seed in c(113,155,799,19,70,72)) to make a search for the most similar to the SCIMago 72
	#	plot_rs_final(file_rs = file_rdata, mean_degree = mean_degree, mst = mst, pl_seed = pl_seed, mode="max")
		plot_map(file_rs = file_rdata, mean_degree = mean_degree, mst = mst, pl_seed = 72, mode="max") #pc lab utfsm seed 72 
}

#' @title Create Scimago Research Space
#' @description Open RSpace on Scimago taxonomy, plots and returns an iGraph object. 
#' @param data A numeric matrix with entities \eqn{i} in the rows and categories \eqn{j} in the columns. Cells show the respective value (value of abundance) of entity \eqn{i} in the category \eqn{j}. It can also be a transpose of the previous matrix, that is, a matrix with categories in the rows and entities in the columns. Yet in that case, the argument "category_row" has to be set to TRUE. The matrix must include names for the rows and the columns. The argument "data", also accepts a dataframe with three columns in the following order: entity, category and value. 
#' 
#' @param category_row A flag to indicate that categories are in the rows. The analysis assumes that the categories are in the columns of the matrix. If the categories are in the rows and the entities in the columns, then the argument "category_row" has to be set to TRUE. The default value is FALSE.
#' @examples 
#' create_rs_scimago()
#' @return An iGraph object.
#' @export
create_rs_simago <- function()
{
	file_rdata<- "New_rs_sim_pr_1971_2010_n_-1_aw_0_jf_0_awjf_0.1_min_prod_0.RData"
	mean_degree <- 10
	nodes <- load_taxonomy("scimago")
	plot_map(file_rs = file_rdata, nodes=nodes, mean_degree = mean_degree, mst = mst, pl_seed = 69, v_min_size = 10, cex=1, mode="max")
}


#mode is the mode that you want to use in the graph
#' @export
plot_map <- function(file_rs, nodes, mean_degree=5, mode="max", mst=TRUE, pl_seed=69, prop_to_lab=0.2, cex=1, pl=TRUE, pl_mst=FALSE, title='', subtitle='Subt', v_min_size=10, v_col='orig')
{
	title<- toupper(mode)
	library("igraph")
	#TODO, take this to the place of DATA!!! 
	#path_rs <- "/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/RESEARCH_SPACE/SCIMAGO/RESEARCH\ SPACE\ OUTPUT" #HARD CODED
	#path_rs <- "/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/RESEARCH_SPACE/UCSD/RESEARCH\ SPACE\ OUTPUT" #HARD CODED
	#load(file.path(path_rs, file_rs)) #load complete information of the research space wanted all variables are rs_
	#adj <- dis_categories(pantheon)
	#pheatmap(adj, cluster_rows=FALSE, cluster_cols=FALSE)
	#load(file_rs)
	adj_orig <- rs_adj
	#diag(adj_orig) <- 0
	adj_clean <- adj_orig
	diag(adj_clean) <-0
	#cleanning NA
	adj_clean[is.na(adj_clean)] <- 0
	
	cat_not_connected <- rownames(adj_clean)[rowSums(adj_clean) == 0 | colSums(adj_clean) == 0]
	print(paste("Number of categories not conected", length(cat_not_connected)))
	
	cat_connected <- rownames(adj_clean)[rowSums(adj_clean) != 0 & colSums(adj_clean) != 0] #note that in the case of probability the Min of upper triangular and lower triangular is taken, so connection in both upper and lower triangle of the matrix is required!!!
	
	#cleanning un connected categories
	adj_cc <- adj_orig[cat_connected, cat_connected]
	adj_cc[is.na(adj_cc)] <- 0 #in case NAs
	#creating full graph for union future process 
	#adj_full <-  (adj_cc-min(adj_cc,na.rm=TRUE))/(max(adj_cc, na.rm=TRUE)-min(adj_cc, na.rm=TRUE)) #normalizing between zero and one
	adj_full <- adj_cc
	g_full <- graph.adjacency(adjmatrix = adj_full, mode = mode, weighted = TRUE, diag = FALSE) #note minimum because the min probability ORIGINAL
	#g_full <- graph.adjacency(adjmatrix = adj_full, mode = "directed", weighted = TRUE, diag = FALSE) #modified to be directed
	
		print("Computing minimum spnanning tree")
		#normalize similarities
		#dist_matrix <- 1 - (adj_cc-min(adj_cc,na.rm=TRUE))/(max(adj_cc, na.rm=TRUE)-min(adj_cc, na.rm=TRUE)) #normalizing between zero and one
		dist_matrix <- 1 - (adj_cc)/(max(adj_cc, na.rm=TRUE)) #normalizing per le maximum, numerator should not has values of zero!
		#MST must use the DISTANCE matrix NOT the similarities!!!!!
	  mode_mst<- mode
		if(mode=="max")
			mode_mst<- "min"
		if(mode=="min")
			mode_mst <- "max"
		g_mst <-  graph.adjacency(adjmatrix = dist_matrix, mode = mode_mst, weighted = TRUE, diag = FALSE) #max is the worst case ORIGINAL
		#g_mst <-  graph.adjacency(adjmatrix = dist_matrix, mode = "directed", weighted = TRUE, diag = FALSE) #max is the worst case
		#g_mst <- simplify(g_mst, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight="min", "ignore")) #taking the minimum of the probability
		#E(g_mst)$sim <- -1*(E(g_mst)$weight -1)
		#g_original <<- g_mst
		#g_mst=delete.edges(g_mst, which(E(g_mst)$weight <=1)) # here's my condition.
		
		g_mst <- minimum.spanning.tree(g_mst)
		
		if(pl_mst==TRUE)
		{
			main <- paste( title, "\n Minimum Spanning Tree")
			par(mfrow=c(1,2))
			plot_graph_smart_2(g_mst, main=main, lay = 'fr', v_label = 'no', v_size='degree', cex = cex, v_min_size=v_min_size) #force directed
			plot_graph_smart_2(g_mst, main=main, lay = 'rt', v_label = 'no', v_size='degree', cex = cex, v_min_size=v_min_size) #tree
			par(mfrow=c(1,1))
			plot_graph_smart_2(g_mst, main=main, lay = 'rt', v_label = 'com', v_size=2, cex = cex, v_min_size=v_min_size) #tree
		}
		
		
		#flagging edges of Minimum ST for future combination
		E(g_mst)$mst <- 1
	 
	
	#######THERSHOLD up to certain mean degree (ideally 4)

		print("Computing threshold graph")
		filter <- min(rs_adj) #initial filter 
		#if(rs_sim=='n') increment <- 1
		#if(rs_sim=='pr') increment <- 0.001 
		
		increment <- 0.001
		degree_rs <- mean_degree + 1 #to go into the loop
		adj_th <- adj_cc
		#adj_th <-  (adj_th-min(adj_th,na.rm=TRUE))/(max(adj_th, na.rm=TRUE)-min(adj_th, na.rm=TRUE)) #normalizing between zero and one
		
		while(degree_rs > mean_degree)
		#while(kk > kkk )
		{
			#print("here!!")
			adj_th[adj_th < filter] <- 0
			cat_non_zero <- rownames(adj_th)[rowSums(adj_th)!=0]
			adj_th <- adj_th[cat_non_zero, cat_non_zero]
			#print(adj_th)
			g <- graph.adjacency(adjmatrix = adj_th, mode = mode, weighted = TRUE, diag = FALSE) #note minimum because the min probability
			#g <- graph.adjacency(adjmatrix = adj_th, mode = "directed", weighted = TRUE, diag = FALSE) #note mi
			degree_rs <- mean(degree(g))
			#print(paste("Degree_RS", degree_rs))
			filter <- filter + increment
		}
		#print(mean(degree(g)))
		#print("Degreeeeee")
		#print(degree(g))
		#print(max(degree(g),na.rm=TRUE))
		
		#print(V(g)$size)
		#fc <- fastgreedy.community(g); colors <- rainbow(max(membership(fc)))
		#V(g)$color = colors[membership(fc)]
		#V(g)$membership <- fc$membership
		
		#if(length(V(g)) < (prop_to_lab * length(rownames(nodes))) )
		#	V(g)$label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
		#else
		#{
		#print("Filtering labels...")
		#	nod_max_com <- get_max_com(g)  #get the nodes with max degree per community
		#print(nod_max_com)
		#	V(g)$label <- ''
		#V(g)$label[V(g)$name %in% nod_max_com] <- as.character(nodes$subd_name[match(nod_max_com, nodes$Id)])
		#	nodes_to_label <- V(g)$name[V(g)$name %in% nod_max_com] #to get the order of iGraph
		#	V(g)$label[V(g)$name %in% nod_max_com] <- as.character(nodes$subd_name[match(nodes_to_label, nodes$Id)])
		#}
		# set.seed(pl_seed)
		# g$layout <- layout.fruchterman.reingold(g)
		
			#title_th <- paste("Only threshold" , title)
			#par(mfrow=c(1,1))
			#plot_graph_smart_2(g, main = title_th, sub_add = paste(subtitle, "Seed plot: ",
			#	pl_seed, '| Threshold for links: ', filter-increment),lay = "fr", cex = cex, v_min_size=v_min_size)
		g_th <- g
		#flagging edges from threshold graph to future combination
		E(g_th)$threshold <- 1

	
	#MERGING GRAPHS.....
	#E(g)$weight <- E(g)$weight/max(E(g)$weight, na.rm=TRUE) #NORMALIZING BETWEEN 0 AND 1
	#first Full + MST
	print("Merging graphs")
	g <- graph.union(g_full, g_mst, g_th) #note union is computed by NAME, not by ID, so it is correct to our propose
	print("Union created")
	E(g)$weight <- E(g)$weight_1
	E(g)$mst[is.na(E(g)$mst)] <- 0
	E(g)$threshold[is.na(E(g)$threshold)] <- 0
	E(g)$used <- E(g)$mst + E(g)$threshold #union
	#print("Deliting edges ...")
	#g <- delete.edges(g, E(g)[E(g)$used < 1]) #podding edges that are not in mst and not in Threshold graph MOVED TO EACH FUNCTION
	#E(g)$weight_used[E(g)$used >= 1] <- E(g)$weight[E(g)$used >= 1] #storing weights of edges that are the ones to apply the filter
	
	#print("Edges_deleted...")
	#E(g)$weight[!is.na(E(g)$sim)] <- E(g)$sim #adding mst weights in mst they are in sim not in weight since in weight_1 they are distances.
	#E(g)$weight[is.na(E(g)$weight)] <- E(g)$weight_2[is.na(E(g)$weight)]
	#E(g)$weight <- E(g)$sim #adding mst weights in mst they are in sim not in weight since in weight_1 they are distances.
	#E(g)$weight[is.na(E(g)$weight)] <- E(g)$weight_2[is.na(E(g)$weight)]
	
	

		lab='com'
	
		if(length(V(g)) < (prop_to_lab * length(rownames(nodes))) )
			lab='all'
		title= paste("MST+Threshold ",title, "\nMean degree:", mean_degree, "SEED", pl_seed) 
		par(mfrow=c(1,1))
		#original community over all the graph before deletion
		fc <- infomap.community(g); colors <- rainbow(length(fc))
		V(g)$color <- colors[membership(fc)]
		
		print("Ploting threshold graph")
		g_to_plot <- delete.edges(g, E(g)[E(g)$used < 1])
		plot_graph_smart_2(g_to_plot, nodes=nodes, main = title, sub_add = paste(subtitle, "Seed plot: ",
			pl_seed, '| Threshold for links: ', filter-increment),lay = "fr", v_label=lab, cex = cex, v_min_size=v_min_size, v_col='orig', pdf_w = FALSE, file_name=paste('Research Space ', taxo, ' - ClColors', sep=''), pl_seed=pl_seed ) #v_col color based on community com, original = orig, NULL for the community over all the matrix
		#ploting with other colors
		plot_graph_smart_2(g_to_plot,nodes=nodes, main = title, sub_add = paste(subtitle, "Seed plot: ",
			pl_seed, '| Threshold for links: ', filter-increment),lay = "fr", v_label=lab, cex = cex, v_min_size=v_min_size, v_col='com', pdf_w = FALSE, file_name=paste('Research Space ', taxo, ' - ComColors', sep=''), pl_seed=pl_seed ) #v_col color based on community com, original = orig, NULL for the community over all the matrix
		
		#par(mfrow=c(1,2))
		#plot_graph_smart_2(g_to_plot, main = paste('Research Space in', taxo, 'classification'), sub = paste("Original Taxonomy Colors \n Communities:",length(unique(nodes$color))),lay = "fr", v_label='no', v_col='orig', cex = cex, v_min_size=v_min_size)
		#plot_graph_smart_2(g_to_plot, main = paste('Research Space in', taxo, 'classification'), sub='Autodetected Community Colors Assigned', lay = "fr", v_label='no', v_col='com', cex = cex, v_min_size=v_min_size)

	
	g_merge_copy <<- g
	
	return(g)
}


##ploting graphs with smart labeling and colors
#sub_add add text to the default sub or to the sub parameter
#sub subtitle to plot
#main, main title of the plot
#g the graph to plot
#cex a factor to scale all the things in the plot
#pl_seed the seed to use in the plot
#lay the layout to use, a mnemonic fr or drl or cir
#' @export
plot_graph_smart_2 <- function(g, nodes, main='', sub=NULL, sub_add='', cex=1, pl_seed="69", lay='fr', v_label='com', v_size='degree', v_col='com', v_min_size=10, pdf_w=FALSE, file_name=NULL)
{
	
	######GRAPH PROPERTIES
	set.seed(pl_seed)
	if(lay=='fr') #force directed
		g$layout <- layout.fruchterman.reingold(g)
	if(lay=='drl')
		g$layout <- layout.drl(g)
	if(lay=='rt') #tree oriented 
		g$layout <- layout.reingold.tilford(g)
	if(lay=='sph') #sphere
		g$layout <- layout.sphere(g)
	#fc <- fastgreedy.community(g, weights = NULL); colors <- rainbow(length(fc))
	print("Detecting communities...")
	#fc <- fastgreedy.community(g); colors <- rainbow(length(fc))
	fc <- infomap.community(g); colors <- rainbow(length(fc))
	print("Degree computing...")
	mean_degree <- trunc(mean(degree(g)))
	sub_default <- paste( 'Number of nodes: ', length(V(g)), '/', nrow(nodes), '  | Mean degree: ', mean_degree, ' | Detected communities: ', length(fc))
	
	if(is.null(sub))
		sub <-	sub_default
	
	#adding sub_add to the subtitle 
	if(!is.null(sub_add))
		sub <- paste(sub_add, '\n', sub)
	V(g)$membership <- fc$membership
	
	######## VERTICES PROPERTIES
	print("labeling...in")
	if(v_label =='com')
	{
		#g_mst_copy<<- g_mst
		nod_max_com <- get_max_com(g)  #get the nodes with max degree per community
		#print(nod_max_com)
		V(g)$label <- ''
		nodes_to_label <- V(g)$name[V(g)$name %in% nod_max_com] #to get the order of iGraph
		V(g)$label[V(g)$name %in% nod_max_com] <- as.character(nodes$subd_name[match(nodes_to_label, nodes$Id)])
		#V(g)$label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)])
	}
	if(v_label == 'all')
		V(g)$label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)])
	if(v_label == 'no')
		V(g)$label <- ''
	
	print("Coloring .... in")
	#color
	if(v_col == 'com')
		V(g)$color <- colors[membership(fc)]
	if(v_col == 'orig')
		V(g)$color <- as.character(nodes$color[match(V(g)$name, nodes$Id)]) 
	
	sub <- paste(sub, '\n Number of full communities:', length(unique(V(g)$color)))
	#size
	print("Sizing Nodes...")
	if(is.numeric(v_size)==TRUE)
		V(g)$size <- v_size
	if(v_size=='degree')
		V(g)$size <- ((degree(g)+v_min_size)/max(degree(g))) * v_min_size
	if(v_size == 'orig')
	{
		V(g)$size <- as.numeric(nodes$size[match(V(g)$name, nodes$Id)]) 
		V(g)$size <- (V(g)$size/max(V(g)$size)) *5
	}
	
	#EDGES PROPERTIES
	E(g)$arrow.size=0.1
	E(g)$curved <- TRUE
	E(g)$color <- 'lightgray'
	#edge.label.color="gray",
	
	print("Plotting Graph....in ")
	#id <- tkplot(g)
	#tkconfigure(igraph:::.tkplot.get(id)$canvas, "bg"="black")
	par(lend = 1)           # square line ends for the color legend
	#areas_df <- subset(nodes, select=c('Discipline','color'))
	nodes["Discipline"] <- nodes$area_name #HARDCODED!
	areas_df <- subset(nodes, select=c('Discipline','color'))
	areas_df <-unique(areas_df)
	areas_df$color <- factor(areas_df$color)
	
	areas_df$Discipline <- factor(areas_df$Discipline)
	
	#legend = c(paste("Inactive", num_ina), paste("Undeveloped", num_und), paste("Growing", num_grw), paste("Developed", num_dev), paste("Opportunity", num_rcm)) # category labels 
	#deleted rs_plot
	 plot.igraph(g, 
		vertex.label.cex=0.1*cex, 
		main= list(main, cex=1*cex),
		vertex.label.font=0, 
		vertex.label.family='Helvetica', 
		vertex.label.color='black', 
		edge.label.cex=0.6*cex,
		edge.label.family="Helvetica",
		sub=list(sub,	cex=0.8*cex),
		asp=FALSE
	)
	# legend("bottomleft",      # location of the legend on the heatmap plot
	# 	legend = as.vector(areas_df$Discipline),
	# 	pch=19, merge=FALSE,
	# 	pt.bg = as.vector(areas_df$color),
	# 	col = as.vector(areas_df$color),
	# 	#lty= NA,             # line style
	# 	lwd = 1,           # line width
	# 	pt.cex = 0.1*cex,
	# 	box.col = "lightgrey",
	# 	#fill = "white",
	# 	cex= 1*cex
	# )
	print("Should have printed...")
	
	#EXPORTING PDF file
	if(pdf_w ==TRUE)
	{
	  path_rs =""
		dev_file_name <- file.path(path_rs,'RESEARCH SPACE OUTPUT', paste(file_name,'.pdf', sep=''))
		#dev.print(pdf, file=dev_file_name, widht=6, height=3 );
		pdf(dev_file_name, width=16, height=12, family='Helvetica', pointsize=8)
		plot.igraph(g, 
			vertex.label.cex=1.1*cex, 
		#	main= list(main, cex=1*cex),
			vertex.label.font=0, 
			vertex.label.family='Helvetica', 
			vertex.label.color='black', 
			edge.label.cex=0.6*cex,
			edge.label.family="Helvetica",
			#edge.width=0.1,
		#	sub=list(sub,	cex=0.8*cex),
			asp=FALSE
		)
		legend("bottomleft",      # location of the legend on the heatmap plot
			legend = as.vector(areas_df$Discipline),
			pch=19, merge=FALSE,
			pt.bg = 'black',
			col = as.vector(areas_df$color),
			#lty= NA,             # line style
			lwd = 1,           # line width
			pt.cex = 3*cex,
			box.col = "lightgrey",
			#fill = "white",
			cex= 1.5*cex
		)
		
		dev.off()
	}
	rs <- g
	#exporting to an R object to use it for overlays and others
	#save(rs, rs_plot, file = file.path(path_rs,'RESEARCH SPACE OUTPUT', paste('RS_iGRAPH_', taxo, '.RData', sep='')))
}


#get a vector of nodes with max degree value inside each community
# graph with membership
# n_com_max number of maximum values per category, in case they are equal max values
#' @export
get_max_com <- function(g, n_com_max=1, n_com)
{
	V(g)$degree <- degree(g)
	#getting betweenness centrality
	#g_lcc <- g
	
	#	decompose.graph(g)
	#g_lcc <- g_lcc[[1]] #largest connected component
	#print(E(g_lcc)$weight==0)
	#V(g_lcc)$betweenness <- betweenness(g_lcc, directed = FALSE)
	#E(g_lcc)[E(g_lcc)$weight<=0] <- NULL
	#E(g_lcc)[is.na(E(g_lcc)$weight)] <- NULL
	#V(g)$betweenness[V(g_lcc)$name] <- V(g_lcc)$betweenness
	
	V(g)$betweenness <- betweenness(g)
	
	if(is.null(V(g)$membership))
	{
		#fc <- fastgreedy.community(g)
		fc <- infomap.community(g)
		V(g)$membership <- fc$membership
	}
	
	communities <- unique(V(g)$membership)
	max_total <- vector()
	#print(communities)
	for(com in communities)
	{
		#print(paste("comm", com))
		max_deg <- sort(V(g)$degree[V(g)$membership==com ], decreasing=TRUE)
		max_betw <- sort(V(g)$betweenness[V(g)$membership==com], decreasing = TRUE)
		
		n_max <- as.integer(log(length(max_deg), base=2))  #maximum nodes to label as a function of log base 2 
		
		max_com <- V(g)$name[V(g)$membership==com & V(g)$degree > max_deg[n_max]]	
		max_com <- unique(c(max_com, V(g)$name[V(g)$membership==com & V(g)$betweenness > max_betw[n_max]]))
		
		#if(length(max_com) > n_com_max) #avoiding too much max in the same community
		#max_com <- max_com[1:n_com_max]
		
		if(length(max_com)==0 && length(max_deg) > 1) #in case no one node has been labaled in the community
		{
			nodes_com <- V(g)$name[V(g)$membership==com]	
			max_com <- nodes_com[trunc(length(nodes_com)/2)]
		}
		
		max_total <- c(max_total, max_com)
	}
	
	return(max_total)
}
