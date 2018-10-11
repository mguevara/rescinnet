#generate a network of similarities between areas of science.
#source config.R first!
#library(diverse)

#aggregate and filtrate raw data to get an aggregated data to build a research space
get_data_rs <- function(n=600, init=2001, end=2012, pl=FALSE, min_prod=0, min_awgh=0, min_jfrac=0, min_awjf=0, feat='authorship_count', agg_fun=sum, list_prod=NA)
{
    #get sample of producers
    print(paste("Subseting data in interval: ", init, "_", end, sep=''))
    data_interval <- load_data_interval(init = init, end = end, agg=NaN) #without aggregation #change here the sep!!!
    num_years <- end-init
    #ensuring a minimum of publications min_prod per number of years in the interval of time
    #data_interval <<- subset(data_interval, authorship_count > (num_years * min_prod)) #subset according number of authorships
    #calculating totals 
    print(paste("Total authors in interval",  ":", length(unique(data_interval$id_author)), sep=''))
    #unique(data_interval$id_author)
    data_sample <- data_interval
    ##filtering
    if(min_prod != 0)  #by average production in total interval 
    {
        print("Filtering by total production...")
	      totals_producers <- aggregate(authorship_count~id_author , data_interval, FUN=sum)  
	      #print(head(totals_producers))
	      #str(totals_producers)
	      #totals_producers <- aggregate(wgh_jfrac~id_author, data_interval, FUN=sum)  
	      #producers accomplish min production
	      #producers <- totals_producers$id_author[totals_producers$authorship_count> (num_years * min_prod)]
	      producers_ids <- totals_producers$id_author[totals_producers[, feat]> (num_years * min_prod)]
	      data_sample <- subset(data_interval, id_author %in% producers_ids)  #watch out with the name of the column of producers.
	    
    }
    producers_ids <- unique(data_sample$id_author) #
    #filtering per features of authors
    print(paste("Authors:", length(producers_ids)))
    #filtering per weighted authorship
    if(min_awgh != 0) #by author weighted times journal fractional assigment 
    {
      print("Filtering by weighted authorship...")
      data_sample <- subset(data_sample, authorship_wgh >= min_awgh)  #watch out with the name of the column of
      producers_ids <- unique(data_sample$id_author)
      print(paste("Authors:", length(producers_ids)))
    }
    if(min_jfrac != 0) #by author weighted times journal fractional assigment 
    {
      print("Filtering by journal fractional assignment to categories...")
      data_sample <- subset(data_sample, count_jfrac >= min_jfrac)  #watch out with the name of the column of
      producers_ids <- unique(data_sample$id_author) #
      print(paste("Authors:", length(producers_ids)))
    }
    if(min_awjf != 0) #by author weighted times journal fractional assigment 
    {
      print("Filtering by weighted authorship times journal fractional assignment...")
      data_sample <- subset(data_sample, wgh_jfrac >= min_awjf)  #watch out with the name of the column of
      producers_ids <- unique(data_sample$id_author) #
      print(paste("Authors:", length(producers_ids)))
    }
    #choosing a subset according to parameter n
    if(n!=-1)
    {
      if(n > length(producers_ids))
        stop("The sample required N, is less than the total number of producers")
      print(paste("Choosing a sample of", n, "authors..."))
      producers_sample <- producers_ids[sample(length(producers_ids), n,replace = FALSE)]
      data_sample <- subset(data_sample, id_author %in% producers_sample)  #watch out with the name of the column of producers.
      producers_ids <- unique(data_sample$id_author) #
      print(paste("Authors:", length(producers_ids)))
    }
    rs_n_auth <<- length(producers_ids)
    rs_n_authship <<- sum(data_sample$authorship_count)
    print(paste("Total authorships:", rs_n_authship))
    
    print("Cleanning NA values")
    print( data_sample[is.na(data_sample[,feat]),])
    data_sample <- data_sample[!is.na(data_sample[,feat]),]
    
    print("Testing NA values")
    print( data_sample[is.na(data_sample[,feat]),])
    
    print(paste("Aggregating data per", feat,"..."))
    data_agg <-  aggregate(x=data_sample[feat], by=list(id_author = data_sample$id_author, id_category = data_sample$subdiscipline_id), FUN=agg_fun) #total of that category
    #data_agg<- load_data_interval(init=init, end=end, feat='authorship_count', agg=agg_fun)
    data_to_use_for_rs <<- data_agg
    producers_used_for_rs <<- producers_ids
    
  return(data_agg)
}

# n number of authors to use after filter
# init initial year to make the aggregation
# end final year to make the aggregation
# pl boolean to indicate wheter to plot or not the rspace
# min_prod minimum production for each year
# min_awgh minimum weighted coauthorship (i.e. 0.5 2 total authors in the paper)
# min_jfrac minimum fractional assigment of the journal to the category
# min_awjf minimum weighted coauthorship and fractional journal assignement


#creates a research space
create_rs <- function(n=100, init=2012, end=2014, pl=FALSE,  min_prod=0, min_awgh=0, min_jfrac=0, min_awjf=0, feat='authorship_count', agg_fun=sum,  sim='n', mean_degree=4, mst=TRUE, pl_seed=3, dir_otn='')
{
  library(diverse)
  library(pheatmap)
  data_authorship <- get_data_rs(n=n, init=init, end=end, min_prod=min_prod, min_awgh=min_awgh, min_jfrac=min_jfrac, min_awjf=min_awjf,feat=feat, agg_fun=agg_fun) #returns an edge list
  #cleaning factors or perish!
  data_authorship$id_author <- factor(data_authorship$id_author)
  data_authorship$id_category <- factor(data_authorship$id_category)
  #cleaning NAs
  
  #not so eficient
    #data_mat <- values(data_authorship)
    #mat_dis <- dis_categories(data=data_authorship, method ='cosine' )
  print("Creating matrix Authors-Categories...")  
  mat_values <- values(data_authorship) #depends on package DIVERSE
  print("Binarizing matrix...")
  mat_values[mat_values>0] <- 1  #BINARIZATION NEEDED! we want to know if a user publishes in some field, just that.
  #dot product to find the number of joint authorships
  print("Computing number of authors in joint areas...")
  mat_dis_values <- t(mat_values) %*% (mat_values) 
  
  
  #mat_dis_values_deb <<- mat_dis_values
  rs_cat_not_connected <- rownames(mat_dis_values)[rowSums(mat_dis_values) == 0]
  
  print(paste("Number of categories not conected in CALCULATIONS", length(rs_cat_not_connected)))
  
  cat_connected <- rownames(mat_dis_values)[rowSums(mat_dis_values) != 0]
  
  #cleanning un connected categories
  mat_dis_values <- mat_dis_values[cat_connected, cat_connected]
  
  rs_inner_values <- mat_dis_values #to store joint occurrence
  
  if(sim=='pr')
  {
    tot_cat <- diag(mat_dis_values)
    print("total cat")
    print(tot_cat)
    mat_tot_cat <- matrix(tot_cat, nrow = length(tot_cat), ncol = length(tot_cat), byrow = TRUE)#the diagonal contains the totals per category
    #print("Mat total cat")
    #print(mat_tot_cat)
    mat_dis_values <- mat_dis_values / mat_tot_cat  #computing the conditional probability in each cell
    print("Mat with prob")
    print(mat_dis_values)
  }
  adj <- mat_dis_values
  
  #pheatmap(adj, cluster_rows=FALSE, cluster_cols=FALSE)
  #hist(adj)
  #adj[adj < 210] <- 0  #cosine 0.06 adj[adj > 0.06
  #hist(adj)
  mat_final <<- adj
  mat_values <<- mat_dis_values
  print("Writing matrices to external files...")
  file_name <- paste('New_rs_sim',sim, init, end, 'n',n , 'aw',min_awgh, 'jf', min_jfrac, 'awjf', min_awjf, 'min_prod', min_prod, sep='_'  )
  #write.csv(mat_final, file=file.path(path_rs, paste(file_name,".csv",sep='')))
	#print(paste("matrix of similarities in: ", file.path(path_rs, file_name)))
	
	rs_adj <- adj
	rs_taxo <- taxo
	rs_data <- dataset
	rs_init <- init
	rs_end <- end
	rs_n <- n
	rs_sim <- sim
	rs_min_prod <- min_prod
	rs_min_awgh <- min_awgh
	rs_min_jfrac <- min_jfrac
	rs_min_awjf <- min_awjf

	
	file_rdata <- file.path(path_rs, dir_otn,paste(file_name,".RData",sep=""))
	save(rs_adj, rs_inner_values, rs_data,  rs_taxo, rs_init, rs_end, rs_n, rs_sim, rs_min_prod, rs_min_awgh, rs_min_jfrac, rs_min_awjf, rs_n_auth, rs_n_authship, rs_cat_not_connected, file=file_rdata)
	print(paste("Data of RS created in: ", file_rdata))
	
  if(pl==TRUE)
  {
    print("Plotting RS...")
    plot_rs(path_rs = file_rdata, mean_degree = mean_degree, mst = mst, pl_seed = pl_seed)
  }
	
	return(adj)
}

create_rs_otn <- function(mean_degree=4, pl_mst=FALSE)
{
  g <- empty_graph() #to accumulate the MSTs of each interval
  path_rs <- file.path(path_rs,'OTN')
  files <- list.files(path = path_rs)
  i <- 1
  for(file in files)
  {
    g_int <- plot_rs(path_rs = file.path(path_rs, file), pl=FALSE, mean_degree = 1, mst = TRUE, pl_mst=pl_mst)
    
    E(g_int)$weight_orig <- E(g_int)$weight
    
    if(i==1)
    {
      g <- g_int
      E(g)$n_present <- 1
    }
    if(i>1)
    {
      print(paste("IIII:",i))
      #print(paste("Entering this interaction", list.edge.attributes(g)) )
      print(E(g_int)$weight)
      
      E(g_int)$active[E(g_int)$weight>0] <- 1  #detecting active links
      g <- graph.union(g, g_int)
      
      #increasing count for edges in MST in the working interval
      E(g)$n_present[is.na(E(g)$n_present)] <- 0 #some new commers might have arrived!
      E(g)$n_present[!is.na(E(g)$active)] <- E(g)$n_present[!is.na(E(g)$active)] + 1 #increasing the count of presence
      g <- remove.edge.attribute(g, "active")
      
      #print(paste("AFTER remove ACTIVE", list.edge.attributes(g)))
      #summing the weights of the previous cummulative graph and the actual interval graph
      E(g)$weight[!is.na(E(g)$weight_1) & !is.na(E(g)$weight_2)] <- E(g)$weight_1[!is.na(E(g)$weight_1) & !is.na(E(g)$weight_2)] + E(g)$weight_2[!is.na(E(g)$weight_1) & !is.na(E(g)$weight_2)]
      
      g <- remove.edge.attribute(g, "weight_1")
      g <- remove.edge.attribute(g, "weight_2")
      
      #print(paste("AFTER weight_1, weight_2 removal", list.edge.attributes(g)))
    }
    
    #print(E(g)$n_present)
    i <- i +1 
  }
  
  #computing the average of the Weights cummulated in the total graph
  E(g)$weight <- E(g)$weight/E(g)$n_present
  fil <- 1
  while(trunc(mean(degree(g))) > mean_degree)
  {
    g <- delete.edges(g, E(g)[E(g)$n_present < fil]) #aprox mean degree - 4
    isolated_nodes <- V(g)[degree(g)==0]
    g <- delete.vertices(g, isolated_nodes)
    fil <- fil + 1
  }
  
  
  print("Isolated nodes:")
  print(isolated_nodes)
  print("Ploting OTN Graph:")
  
  file_name <- paste('rs_sim',sim, init, end, 'n',n , 'aw',min_awgh, 'jf', min_jfrac, 'awjf', min_awjf, 'min_prod', min_prod, sep='_'  )
  
  file_rdata <- file.path(path_rs,paste(file_name,".RData",sep=""))
  save(rs_adj, rs_data,  rs_taxo, rs_init, rs_end, rs_n, rs_sim, rs_min_prod, rs_min_awgh, rs_min_jfrac, rs_min_awjf, rs_n_auth, rs_n_authship, file=file_rdata)
  print(paste("Data of RS created in: ", file_rdata))
  
  plot_graph_smart(g)
  print("Filtered per n_presence=", fil-1)
  g_otn <<- g
  return(g)
  
}

# prop_to_lab threshold of number of nodes to show all labels of the nodes
#' @title Plot Research Space
#' @description this function takes a MATRIX OF ADJACENCY of a research space previously created with function create_rs() in order to filter it and plot it Use i.e:rs_sim_pr_1987_2014_n_-1_aw_0_jf_0_awjf_0.75_min_prod_0, rs_sim_pr_2000_2009_n_-1_aw_0_jf_0_awjf_0.75_min_prod_0
#' @param path_rs a path to the file containing the research space
#' @param mean_degree a number of degree to filter links
#' @param mst if TRUE it adds links of the Minimum Spanning Tree
#' @param pl_seed the seed to plot
#' @param prop_to_lab proportion to label a vertex
#' @param pl if False it will not plot the map
#' @param pl_mst to plot or not to plot the MST alone
#' 
#' @examples 
#' plot_rs()
#' @return an iGraph.
#' @export
plot_rs <- function(map=NULL, mean_degree=4, mst=TRUE, pl_seed=1, prop_to_lab=0.2, cex=1, pl=FALSE, pl_mst=FALSE)
{
  library("igraph")
  #load(path_rs) #load complete information of the research space wanted all variables are rs_
  load("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/RESEARCH_SPACE/UCSD/New_rs_sim_pr_1971_2010_n_-1_aw_0_jf_0_awjf_0.1_min_prod_0.RData") #hard coded!!!
  #adj <- dis_categories(pantheon)
  #pheatmap(adj, cluster_rows=FALSE, cluster_cols=FALSE)
  adj_orig <- rs_adj
	
#       	filters <-'Filters applied per author: '
#       	if(rs_min_awgh>0) filters <- paste(filters, " Yearly weighted authorship > ", rs_min_awgh , "|")
#       	if(rs_min_jfrac>0) filters <- paste(filters, " Yearly fractional journal assignment < ", rs_min_jfrac, "|")
#       	if(rs_min_awjf>0) filters <-paste(filters, " Yearly weighted authorship and fractional journal assignment < ", rs_min_awjf, "|")
#       	if(rs_min_prod>0) filters <- paste(filters, " Yearly minimum average production:", rs_min_prod)
#       	
#       	if(rs_sim=='n') sim_used <- "Number of authors producing in both areas"
#       	if(rs_sim=='pr') sim_used <- "Conditional probability of an author publishing in both areas"
#       
#       	title <- paste('Research Space\n','Data: ', rs_data, '| Taxonomy:' , rs_taxo, '| Interval:' ,rs_init, '-', rs_end) 
#       	subtitle <- paste("Size of nodes: degree | Colors of nodes: automatic community detection | ", "Layout type force-directed | ", " Seed plot ", pl_seed,
#       		"\n","Similarity between nodes:", sim_used, "| Amount of authors: ", format(rs_n_auth, big.mark = ','), " |  Amount of authorships: ", format(rs_n_authship, big.mark=","), 
#       		"\n", filters)
	
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
  #g_full <- graph.adjacency(adjmatrix = adj_full, mode = "min", weighted = TRUE, diag = FALSE) #note minimum because the min probability ORIGINAL
  g_full <- graph.adjacency(adjmatrix = adj_full, mode = "directed", weighted = TRUE, diag = FALSE) #modified to be directed
  
  #HARDCODED
  #if(mst==TRUE)
  if(TRUE==TRUE)
  {
  	print("Computing minimum spnanning tree")
  	#normalize similarities
  	#dist_matrix <- 1 - (adj_cc-min(adj_cc,na.rm=TRUE))/(max(adj_cc, na.rm=TRUE)-min(adj_cc, na.rm=TRUE)) #normalizing between zero and one
  	dist_matrix <- 1 - (adj_cc)/(max(adj_cc, na.rm=TRUE)) #normalizing per le maximum, numerator should not has values of zero!
  	#MST must use the DISTANCE matrix NOT the similarities!!!!!
    #g_mst <-  graph.adjacency(adjmatrix = dist_matrix, mode = "max", weighted = TRUE, diag = FALSE) #max is the worst case ORIGINAL
    g_mst <-  graph.adjacency(adjmatrix = dist_matrix, mode = "directed", weighted = TRUE, diag = FALSE) #max is the worst case
    #g_mst <- simplify(g_mst, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(weight="min", "ignore")) #taking the minimum of the probability
    #E(g_mst)$sim <- -1*(E(g_mst)$weight -1)
    #g_original <<- g_mst
    #g_mst=delete.edges(g_mst, which(E(g_mst)$weight <=1)) # here's my condition.
    
    g_mst <- minimum.spanning.tree(g_mst)
    
    if(pl_mst==TRUE)
    {
      main <- paste( title, "\n Minimum Spanning Tree")
      par(mfrow=c(1,2))
      plot_graph_smart(g_mst, main=main, lay = 'fr', v_label = 'no', v_size='degree', cex = cex) #force directed
      plot_graph_smart(g_mst, main=main, lay = 'rt', v_label = 'no', v_size='degree', cex = cex) #tree
      par(mfrow=c(1,1))
      plot_graph_smart(g_mst, main=main, lay = 'rt', v_label = 'com', v_size=2, cex = cex) #tree
    }
  	
    
  		#flagging edges of Minimum ST for future combination
  		E(g_mst)$mst <- 1
  } 

  #######THERSHOLD up to certain mean degree (ideally 4)

  for(mean_deg in mean_degree )
  {
  		print("Computing threshold graph")
  	  filter <- min(rs_adj) #initial filter 
  	  #if(rs_sim=='n') increment <- 1
  	  #if(rs_sim=='pr') increment <- 0.001 
  	  
  	  #HARDCODED
  	  increment <- 1
  		#end HARDCODED
  	  
  	  degree_rs <- mean_deg + 1 #to go into the loop
  		adj_th <- adj_cc
  		#adj_th <-  (adj_th-min(adj_th,na.rm=TRUE))/(max(adj_th, na.rm=TRUE)-min(adj_th, na.rm=TRUE)) #normalizing between zero and one
  		
		  #while(degree_rs > mean_deg)#HARDCODED
  		kk <- 1
  		kkk <- 0
  		while(kk > kkk )
		  {
		  	#print("here!!")
		    adj_th[adj_th < filter] <- 0
		    cat_non_zero <- rownames(adj_th)[rowSums(adj_th)!=0]
		    adj_th <- adj_th[cat_non_zero, cat_non_zero]
		    #print(adj_th)
		    #g <- graph.adjacency(adjmatrix = adj_th, mode = "min", weighted = TRUE, diag = FALSE) #note minimum because the min probability
		    g <- graph.adjacency(adjmatrix = adj_th, mode = "directed", weighted = TRUE, diag = FALSE) #note mi
		    degree_rs <- mean(degree(g))
		  	#print(paste("Degree_RS", degree_rs))
		    filter <- filter + increment
		    
		    kk <- 0
		    kkk<-1
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
		 
		    if(pl==TRUE) #IN SOME cases just want the graph but not the plot
		    {
		    	title_th <- paste("Only threshold" , title)
		    	par(mfrow=c(1,1))
		    	plot_graph_smart(g, main = title_th, sub_add = paste(subtitle, "Seed plot: ",
		    			                pl_seed, '| Threshold for links: ', filter-increment),lay = "fr", cex = cex)
		    }
		   g_th <- g
		   #flagging edges from threshold graph to future combination
		   E(g_th)$threshold <- 1
  }#end mean degree elements
  
  #g_mst_copy <<- g_mst
  #g_th_copy <<- g_th
  
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
	print("Deliting edges ...")
	#g <- delete.edges(g, E(g)[E(g)$used < 1]) #podding edges that are not in mst and not in Threshold graph MOVED TO EACH FUNCTION
	#E(g)$weight_used[E(g)$used >= 1] <- E(g)$weight[E(g)$used >= 1] #storing weights of edges that are the ones to apply the filter
	
  print("Edges_deleted...")
  #E(g)$weight[!is.na(E(g)$sim)] <- E(g)$sim #adding mst weights in mst they are in sim not in weight since in weight_1 they are distances.
  #E(g)$weight[is.na(E(g)$weight)] <- E(g)$weight_2[is.na(E(g)$weight)]
  #E(g)$weight <- E(g)$sim #adding mst weights in mst they are in sim not in weight since in weight_1 they are distances.
  #E(g)$weight[is.na(E(g)$weight)] <- E(g)$weight_2[is.na(E(g)$weight)]
  
  
  if(pl==TRUE) #IN SOME cases just want the graph but not the plot
  {
  	lab='com'
  	if(length(V(g)) < (prop_to_lab * length(rownames(nodes))) )
  		lab='all'
		title= paste("MST+Threshold ",title, "\nMean degree:", mean_deg) 
		par(mfrow=c(1,1))
		print("Ploting threshold graph")
		g_to_plot <- delete.edges(g, E(g)[E(g)$used < 1])
		plot_graph_smart(g_to_plot, main = title, sub_add = paste(subtitle, "Seed plot: ",
				pl_seed, '| Threshold for links: ', filter-increment),lay = "fr", v_label=lab, cex = cex)
  	
		par(mfrow=c(1,2))
		plot_graph_smart(g_to_plot, main = 'Original Colors Taxonomy', sub = paste("Communities:",length(unique(nodes$color))),lay = "fr", v_label='no', v_col='orig', cex = cex)
		plot_graph_smart(g_to_plot, main = 'Autodetected Community Color Assigned',lay = "fr", v_label='no', v_col='com', cex = cex)
  }
  
  g_merge_copy <- g
  
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
plot_graph_smart <- function(g, main='', sub=NULL, sub_add='', cex=1, pl_seed="69", lay='fr', v_label='com', v_size='degree', v_col='com')
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
	
	#size
	print("Sizing Nodes...")
	if(is.numeric(v_size)==TRUE)
		V(g)$size <- v_size
	if(v_size=='degree')
		V(g)$size <- (degree(g)/max(degree(g))) * 5
	if(v_size == 'orig')
	{
		V(g)$size <- as.numeric(nodes$size[match(V(g)$name, nodes$Id)]) 
		V(g)$size <- (V(g)$size/max(V(g)$size)) *5
	}
		
	
	#edge.label.color="gray",
	
	print("Plotting Graph....in ")
	plot.igraph(g, 
		vertex.label.cex=0.7*cex, 
		main= list(main, cex=1*cex),
		vertex.label.font=0, 
		vertex.label.family='Helvetica', 
		vertex.label.color='black', 
		edge.label.cex=0.6*cex,
		edge.label.family="Helvetica",
		sub=list(sub,	cex=0.8*cex),
		asp=FALSE
	)
}


#get a vector of nodes with max degree value inside each community
# graph with membership
# n_com_max number of maximum values per category, in case they are equal max values
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

#plots an animation of rs varying its mean degree
#if name of a file not defined file_mat, then adj must be defined. 
# file_mat is the name of the file csv that contains a matrix in path_rs 
# name without extension and point
# file_name used in case file_mat not provided, a name for the resulting GiF
# prop_to_lab threshold of number of nodes to show all labels of the nodes
plot_rs_gif <- function( file_mat=NULL, mean_degree=c(4),  mst=FALSE, pl_seeds=1, name_file='UnNamed', width=1600, height=1600, prop_to_lab=0.2)
{
	library(animation)
	
	
	prev_wd <- getwd()
	setwd(file.path(path_rs,"GIFs"))
	
	for(seed in pl_seeds)
	{
		file_name <- paste(file_mat,'_sd', seed,  '.gif', sep='')
		if(file.exists(file.path(path_rs, file_name) ) )
			file.remove(file_name) #if not removed it will not be updated
		#file_movie <- file.path(path_rs,file_name)
		saveGIF({
			count =0
			for(i in mean_degree){
				if(count==0 & mst==TRUE)
					plot_rs(path_rs = file.path(path_rs,paste(file_mat,".RData",sep='')), mean_degree = i, mst=TRUE, pl_seed=seed, prop_to_lab=prop_to_lab)
				else
					plot_rs(path_rs = file.path(path_rs,paste(file_mat,".RData",sep='')), mean_degree = i, mst=FALSE, pl_seed=seed, prop_to_lab=prop_to_lab)
				count <- count + 1
			}
			
		}, interval = 3, movie.name = file_name, ani.width = width, ani.height = height)
	}
	setwd(prev_wd)
	print("Done!")
}	


read_mat_rs <- function(file_mat)
{
	adj_df <- read.csv(file.path(path_rs, paste(file_mat,".csv",sep='')), check.names=FALSE)
	row.names(adj_df) <- adj_df[,1]; adj_df[,1]<-NULL
	adj <- as.matrix(adj_df)
	return(adj)
}

#create in batch a bunge of research spaces
#batch_create_rs(n=-1, init_years = c(1987, 1987, 1990, 2000, 2010), end_years = c(2014, 1989, 1999, 2009, 2014), min_awjf = c(0.25,0.50,0.75,1,1.25,1.5,1.75,2,2.5))
#useful to generate RS for the Overlapping Tree Method OTN
#batch_create_rs(n=-1, init_years = c(1971, 1971, 1980, 1990, 2000, 2010), end_years = c(2014, 1980, 1989, 1999, 2009, 2014), min_awjf = c(0.25,0.50,0.75,1,1.25,1.5,1.75,2,2.5), dir_otn = '')

batch_create_rs <- function(n=-1,init_years, end_years, min_awjf, min_prod, min_awgh, min_jfrac, sim='pr', mst=TRUE, dir_otn='OTN')
{
	for(i in 1:length(init_years) )
	{
		for(j in 1:length(min_awjf))
			create_rs(n=n, init = init_years[i], end = end_years[i], min_awjf = min_awjf[j], pl = TRUE, sim = sim, mst = mst, dir_otn=dir_otn)
		#for(j in 1:length(min_prod))
		#	create_rs(n=n,init = init_years[i], end = end_years[i], min_prod = min_prod[j], pl = TRUE)
		#for(j in 1:length(min_awgh))
		#		create_rs(n=n, init = init_years[i], end = end_years[i], min_awgh = min_awgh[j], pl = TRUE)
		#for(j in 1:length(min_jfrac))
			#create_rs(n=n,init = init_years[i], end = end_years[i], min_jfrac = min_jfrac[j], pl = TRUE)
	}
}


batch_create_gif <- function(mean_degree=c(4,5,6,11,17, 28, 35), mst=TRUE)
{
	files <- list.files(path = path_rs)
	for(file in files)
	{
		if(grepl(".RData", file) )
		{
			file_rdata <- substr(file, start = 1, stop = nchar(file)-6)
			plot_rs_gif(file_mat = file_rdata, mean_degree = mean_degree, mst = mst )
		}
			
	}
}


#batch analyzis of research spaces stored in file rs_path, to compare them against the 
#benchmark map, or the original map of the classification
#getting data to overlay
#n_eval number of rs in the top
#config(db='gscholar', cl='ucsd', pr='cnt')
#data_to_overlay <- get_data_to_overlay(n=-1, init=2001, end=2010, by = 5, cum=FALSE, min_prod = 500, what_agg="wgh_jfrac", agg_fun = sum, list_prod = NaN)
batch_evaluation_rs <- function(data_to_overlay, mean_degrees=c(9), n_eval=7, pl_roc=FALSE)
{
	maps_top <- data.frame()  #to add the resulting top maps per mean_degree
	files <- list.files(path = path_rs) #folder with research spaces in evaluation
	
	#storing benchmark map
	data_rocs <- overlay_data_3(data_to_use = data_to_overlay, what_eval = 'rca', min_to_grw = 0.5, min_to_dev = 1, pl_base=FALSE, pl = FALSE, pl_roc = FALSE,pl_base_mst = FALSE)
	rocs_agg_bench <- plot_rocs(data_eval_roc = data_rocs, pl=FALSE)
	
	for(mean_degree in mean_degrees)
	{
				#adding first the benchmark map for this iteration
				data_auc_total <- data.frame() #dataframe to store the complete information of evaluation
				map <- taxo
				rocs_agg <-data.frame(map, mean_degree, rocs_agg_bench)
				
				data_auc_total <- rbind(data_auc_total, rocs_agg) 
				
				# agregating to total dataframe
				print("Computing and aggregating ROC results to a data frame. Analyzing all RS")
				for(file in files)
				{
					if(grepl(".RData", file) )
					{
						rs_name <- substr(file, start = 1, stop = nchar(file)-6)
						data_rocs <- overlay_data_3(data_to_use = data_to_overlay, what_eval = 'rca', min_to_grw = 0.5, min_to_dev = 1, pl_base=FALSE, pl = FALSE, pl_roc = pl_roc, rs_to_eval = rs_name, rs_mean_degree=mean_degree, pl_base_mst = FALSE)
						rocs_agg <- plot_rocs(data_eval_roc = data_rocs, pl=FALSE)
						map <- rs_name
						rocs_agg <-data.frame(map, mean_degree, rocs_agg)
						
						data_auc_total <- rbind(data_auc_total, rocs_agg) # agregating to total dataframe
						
					}
					
				}
			
				producers <- data_auc_total$prod[data_auc_total$map == taxo]; 
				prev_int <- data_auc_total$prev_interval[1]
				interval <- data_auc_total$inter[[1]]
				#choosing only the most useful maps per ranking 
				maps_o <- unique(data_auc_total$map)
				
				#ranking per mean of auc_I_A
				means_maps <- aggregate(auc_I_A~map, data_auc_total, FUN=mean)
				means_maps_auc_g_d <- aggregate(auc_G_D~map, data_auc_total, FUN=mean)
				means_maps_auc_u_d <- aggregate(auc_U_D~map, data_auc_total, FUN=mean)
				means_maps <- merge(means_maps, means_maps_auc_g_d, by='map')
				means_maps <- merge(means_maps, means_maps_auc_u_d, by='map')
				
				maps_sort <- means_maps[order(means_maps$auc_U_D, decreasing = TRUE),]
				
				#sorting per three criterias in herarchy order
				maps_sort <- maps_sort[order( -trunc(maps_sort$auc_U_D*100),-trunc(maps_sort$auc_G_D*100), -trunc(maps_sort$auc_I_A*100) ),]
				
				#excluding per most of NAs in evaluation (few areas)
				maps_excluded <- vector() #excluding per number of NAs in their auc_I_A
				for(map in maps_o)
				{
				  	if(sum(is.na(data_auc_total[data_auc_total$map == map, 'auc_I_A'])) > length(producers)*0.35 ) 
					  	maps_excluded <- c( maps_excluded, map) 		
				}
				
				#defining maps ranked to eval
				maps_eval <- maps_sort[!(maps_sort$map %in% maps_excluded),]
				nmaps <- nrow(maps_eval)
				
				#PLOTING 
				plot_maps_comparation(data_auc_total = data_auc_total , maps_eval=maps_eval, n_eval=n_eval) #ploting only the top n_eval
				
				rank <- c(1:nmaps)
				
				maps_top_mean_deg <- data.frame(prev_int, interval, rank, mean_degree, maps_eval)
				rownames(maps_top_mean_deg) <- rank
				
				#adding to top plots 
				maps_top <- rbind(maps_top, maps_top_mean_deg)
				
	}#end mean_degree LOOP
	
	return(maps_top)	
}


plot_maps_comparation <- function(data_auc_total, maps_eval, n_eval=1, mean_degree=4)
{
	colors <- rainbow(n_eval)
	linetype <- c(1,1, c(1:(n_eval-2)))
	plotchar <- c(1:n_eval)
	producers_ax <- unique(data_auc_total$prod)
	prev_int <- data_auc_total$prev_interval[1]
	interval <- data_auc_total$inter[[1]]
	#starting evaluation plots
	par(mfrow=c(2,2)) #starts template for roc plots 
	for(auc_eval in c('auc_I_A', 'auc_G_D', 'auc_U_D'))
	{
		plot(data_auc_total[data_auc_total$map==taxo, auc_eval], xaxt='n', ylab=auc_eval, xlab="Producer", col="red",  ylim = c(0,1), type="n")
		axis(1, labels=producers_ax, at =(1:length(producers_ax)) )
		i <- 1
		for(map in maps_eval$map[1:n_eval])#considering the same interval
		{
			lines(producers_ax, data_auc_total[data_auc_total$map == map, auc_eval], type="b", col=colors[i], lty=linetype[i], pch=plotchar[i])
			i <- i+1
		}
	}	
	#legend in another plot
	plot.new()
	if(i<4)  maps_eval$map <- strtrim(maps_eval$map, 15)
	legend <- paste(maps_eval[1:n_eval,'map'], '|', 
		round(maps_eval[1:n_eval,'auc_I_A'],3), '|',
		round(maps_eval[1:n_eval,'auc_G_D'],3), '|',
		round(maps_eval[1:n_eval,'auc_U_D'],3))
	legend("topleft", legend=legend,  col=colors, pch=plotchar, lty=linetype, title="Map | avg(AUC_I_A) | avg(AUC_G_D) | avg(AUC_U_D)", cex=0.5, pt.cex = 0.8)
	
	
	
}


#boxplots
plot_maps_comparation_box <- function(data_auc_total, maps_eval, n_eval=1, mean_degree=4)
{
  colors <- rainbow(n_eval)
  #linetype <- c(1,1, c(1:(n_eval-2)))
  #plotchar <- c(1:n_eval)
  producers_ax <- unique(data_auc_total$prod)
  prev_int <- data_auc_total$prev_interval[1]
  interval <- data_auc_total$inter[[1]]
  maps_eval$map <- strtrim(maps_eval$map, 4)
  data_auc_total$map <- strtrim(data_auc_total$map, 4)
  #starting evaluation plots
  par(mfrow=c(1,3)) #starts template for roc plots 
  boxplot(auc_I_A~map, data=data_auc_total, ylab='AUC Inactive to Active')
  aov_I_A <- aov(auc_I_A~map, data=data_auc_total)
  print("ANOVA Analysis for Inactive to Active transition")
  print(summary(aov_I_A))
  
  boxplot(auc_G_D~map, data=data_auc_total, ylab='AUC Growing to Developed')
  aov_G_D <- aov(auc_G_D~map, data=data_auc_total)
  print("ANOVA Analysis for Growing to Developed transition")
  print(summary(aov_G_D))
  
  boxplot(auc_U_D~map, data=data_auc_total, ylab='AUC Undeveloped to Developed')
  
  aov_U_D <- aov(auc_U_D~map, data=data_auc_total)
  print("ANOVA Analysis for Undeveloped to Developed transition")
  print(summary(aov_U_D))
  print("Maps' means")
  print(maps_eval)
  #plot.new()
  #legend <- paste(maps_eval[1:n_eval,'map'], '|', 
                  #round(maps_eval[1:n_eval,'auc_I_A'],3), '|',
                  #round(maps_eval[1:n_eval,'auc_G_D'],3), '|',
                  #round(maps_eval[1:n_eval,'auc_U_D'],3))
  
  #legend("topleft", legend=legend,   title="Map | avg(AUC_I_A) | avg(AUC_G_D) | avg(AUC_U_D)")
  #print(legend)
  
}

#the graph g must include weight and weight_bench
plot_maps_correlation <- function(g)
{
	par(mfrow=c(1,1))
	x <- E(gms)$weight/max(E(gms)$weight, na.rm = TRUE)
	E(gms)$weight_t <- E(gms)$weight/max(E(gms)$weight, na.rm = TRUE)
	
	y <- E(gms)$weight_bench/max(E(gms)$weight_bench, na.rm = TRUE)
	E(gms)$weight_bench_t <- E(gms)$weight_bench/max(E(gms)$weight_bench, na.rm = TRUE)
	x_from <- E(gms)[from(E(gms))]
	
	temp_mat <- data.frame(x,y)
	
	x_temp <- E(gms)$weight_t[E(gms)$weigth_t>0.8]
	y_temp <- 
	plot(x, y,
		main = paste("Correlation between links Research-Space and " , taxo),
		xlab = "Research Space normalized link weights",
		ylab = paste(taxo,"normalized link weights."),
		col = "Blue",
		asp = FALSE,
		xlim = c(0,1),
		ylim = c(0,1)
		
		)
	
	lines(c(0,1), c(0,1), col="lightgray", lty=3)
	abline(lm(y~x), col="grey", lwd=3) # regression line (y~x)
	
	corr <- cor.test(y,x)
	#x_quar<-1; y_quar<-4
	#if(corr$estimate<0){x_quar<-1;y_quar<-2} 
	#x_cor <- quantile(x,na.rm=TRUE)[x_quar]
	#y_cor <- quantile(y,na.rm=TRUE)[y_quar]
	text(0.8,0.1, paste("R=",round(corr$estimate,2),sep=""),font=16, pos=4)
	
}
