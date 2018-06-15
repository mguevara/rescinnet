#evaluates prediction by using density idea of developed and underdeveloped
#config(cl="ucsd")
overlay_complete <- function(n=1, init=2007, end=2012, by=3,cum=FALSE, min_prod=1, what_agg='wgh_jfrac', agg_fun=sum, list_prod=NaN, what_eval='share', min_to_grw =-1, min_to_dev=-1, pl=FALSE, pl_roc=FALSE, pl_min_dens=0.15, pl_node_sc=2.5, pl_min_size=0.7, pl_num_lab = 5 )
{
  #n number of sample to conduct the analysis. n=-1 implies over all the producers
  #init initial year to take the data
  #end final year to take the data
  #by number of years to split the data in between init and end
  #pl true or false if you want to generete an overlay map for each producer
  #min_prod number of publications required in the complete interval of time
  #list_prod a vector of IDs of producers to be analyzed c('xhCWdtMAAAAJ', 'YirSp_cAAAAJ')
  #mit_to_grw minimum share/production-value for a node to be considered as growing(emerging) area. This value should be less than min_to_dev and growing areas are > min_to_grw and < min_to_dev
  #min_to_dev minimum share/production-value for a node to be considered as developed. -1 will be understood as mean of shares.
  #dens_int number of intervals for density between 0.000001 and 1
  #pl_min_dens the minimum density required to be presented/evaluated over the plot of the producer
  #pl_node_sc a number that scales the size of the nodes
  #pl_min_size minimum value for the size of the nodes
  #pl_num_lab number of developed nodes to be labeled in the plot
  #pred implies which is the transition to evaluate posible values are 'i_a', 'i_u', 'u_d' it uses initial for each state.
  #what_eval rca, prop, ai what should the function evaluate to 
  #what_agg which is the name of the column in the raw data that you want to aggregate? author_count, wgh_jfr
  #cum should the aggregation be accumulative since the very beginning. TRUE for positive
  
  ############get data
  data_to_use <- get_data_to_overlay(n=n, init=init, end=end, by=by,cum=cum, min_prod=min_prod, what_agg=what_agg, agg_fun=agg_fun, list_prod=list_prod)
  data_to_use_copy <<- data_to_use
  
	##overlay_data and compute densities
	overlay_data_3(data_to_use, what_eval=what_eval, min_to_grw =min_to_grw, min_to_dev=min_to_dev, pl=pl, pl_roc = pl_roc)
  
}

#dens_links Which links to use in the evaluation of density.  'all' 'bench' just links in benchmark, 'filter' for filtered links
overlay_data_3 <- function(data_to_use, what_eval='share', min_to_grw =-1, min_to_dev=-1,  pl_base=TRUE, pl_base_mst=TRUE,  pl=TRUE, pl_roc=TRUE, pl_min_dens=0.15, pl_node_sc=2.5, pl_min_size=0.7, pl_num_lab = 5, rs_to_eval=NULL, rs_mean_degree=4, cex=1, pl_w_pdf=FALSE, pl_seed=69, dens_links='all')
{
	############overlay data benchmark magp
		#print("Loading Base Map")
	#use this options a will, fire a will 
	#if(dens_links=='all')
	g_orig <- get_benchmark_map(rs = rs_to_eval, mean_degree=rs_mean_degree, pl=pl_base, pl_mst = pl_base_mst, cex=cex, pl_seed=pl_seed) #does not have filtered links, evaluate everithing
	if(dens_links=='filter')
		g_orig <- delete.edges(g_orig, E(g_orig)[E(g_orig)$used < 1 |  is.na(E(g_orig)$used)]) #to evaluate the poded tree
	#print("Edges to deleeeete")
	#print(E(g_orig)$weight_bench[is.na(E(g_orig)$weight_bench)])
	if(dens_links=='bench')
		g_orig <- delete.edges(g_orig, E(g_orig)[is.na(E(g_orig)$weight_bench)] ) #to evaluate only nodes that are in the benchmark map
  
	
	evaluation_data_total <- data.frame()   #to store final results
	
	id_producers <- unique(data_to_use$id_producer)
	
	for(producer in id_producers)
	{
		for(interval in intervals)
		{
			#load data interval
			i <- match(interval, intervals) #index of parameter interval 
			#year_s <- year_start[i]; year_e <- year_end[i] #USING NOT AGGREGATION of time
	
			
			#get original blank graph
			g <- g_orig
			data_producer <- data_to_use[data_to_use$id_producer==producer & data_to_use$interval == interval,]
			data_producer$id_category <- strtoi(data_producer$id_category)
			#data_p <<- data_producer
			#droplevels(data_producer$id_category)
			#defining first form options for nodes
			g$producer <- producer
			g$interval <- interval
			g$what_eval <- what_eval
			
			
			dev_nodes_flag <- FALSE
			grw_nodes_flag <- FALSE
			und_nodes_flag <- FALSE
			V(g)$developed <- NA
			V(g)$undeveloped <- NA
			V(g)$growing <- NA
			V(g)$density <- 0
			V(g)$density_2 <- 0 #using all the matrix of similarities
			active_nodes <- NA
			grw_nodes <- NA
			und_nodes <- NA
			dev_nodes <- NA
			sum_production <- NA
			variety <- NA
			measures <- NA
			
			#verify that info of producer for this time is not empty
			if(nrow(data_producer)>0) 
			{
				#ACTIVE NODES ---------------------------
				active_nodes <- as.vector(data_producer[,'id_category']) #which areas are present in the data of the producer 
				active_nodes <- strtoi(active_nodes)
				nodes_info_act <- nodes[nodes$Id %in% active_nodes,]
				nodes_info_act <- merge(nodes_info_act, data_producer, by.x='Id', by.y='id_category')
				
				#####ORDERING
				active_nodes_g <- V(g)$name[V(g)$name %in% active_nodes] #TO GET THE order that provides iGraph
				nodes_info_act <- nodes_info_act[match(active_nodes_g, nodes_info_act$Id),] #reordering
				
				V(g)$share[V(g)$name %in% active_nodes]<- as.numeric(nodes_info_act[,'share']) 
				V(g)$value_eval[V(g)$name %in% active_nodes]<- as.numeric(nodes_info_act[,what_eval]) 
				V(g)$active <- 0
				V(g)$inactive <- 1
				V(g)$active[V(g)$name %in% active_nodes] <- 1
				V(g)$inactive[V(g)$name %in% active_nodes] <- 0 #inactive means zero production
				
				V(g)$names_actives[V(g)$name %in% active_nodes] <- as.character(nodes_info_act$subd_name)
				#V(g)$size[is.na(V(g)$size)] <- pl_min_size  #deliting size of 
				# vg_df_actives <<- as.data.frame(list( name=V(g)$name, shares=V(g)$shares, size=V(g)$size,names_actives=V(g)$names_actives,color= V(g)$color), stringsAsFactors = FALSE)
				
				#defining DEVELOPED nodes
				cut_off_to_dev <- min_to_dev
				cut_off_to_grw <- min_to_grw
				quant_eval <- quantile(data_producer[,what_eval], na.rm = TRUE)
				if(min_to_dev == -1)#if not defined value
				{
					#cut_off_to_dev <- mean(data_producer[,'share'])  #take the mean by default, useful for personal producers
					cut_off_to_dev <- quant_eval[4]
				}
				dev_nodes_df <- data_producer[data_producer[what_eval]>= cut_off_to_dev,] #condition of development nodes
				dev_nodes_df <- dev_nodes_df[order(dev_nodes_df[,what_eval], decreasing = TRUE),] #ordering for what eval
				
				if(min_to_grw == -1) #if default value
				{
					cut_off_to_grw <- quant_eval[2]
				}
				
				g$cut_off_to_grw <- cut_off_to_grw
				g$cut_off_to_dev <- cut_off_to_dev
				
				grw_nodes_df <- data_producer[data_producer[what_eval]>= cut_off_to_grw & data_producer[what_eval] < cut_off_to_dev,] #grwoing nodes
				und_nodes_df <- data_producer[data_producer[what_eval]< cut_off_to_grw,] #undeveloped nodes
				
				if(nrow(und_nodes_df)>0)
				{
					und_nodes <- as.vector(und_nodes_df[,'id_category'])
					#ensuring for scimago
					und_nodes <- strtoi(und_nodes)
					und_nodes_flag <- TRUE
					nodes_info_und <- nodes[nodes$Id %in% und_nodes,]
					nodes_info_und <- merge(nodes_info_und, data_producer, by.x='Id', by.y='id_category')
					#ordering
					und_nodes_g <- V(g)$name[V(g)$name %in% und_nodes]
					nodes_info_und <- nodes_info_und[match(und_nodes_g, nodes_info_und$Id), ]
					V(g)$undeveloped <- NA #only active nodes could be evaluated
					V(g)$undeveloped[V(g)$name %in% active_nodes] <- 0 #only active nodes could be evaluated
					V(g)$undeveloped[(V(g)$name %in% und_nodes)] <- 1 #is it UNoccupied
				}
				if(nrow(grw_nodes_df)>0)
				{
					grw_nodes <- as.vector(grw_nodes_df[,'id_category'])
					grw_nodes <- strtoi(grw_nodes)
					grw_nodes_flag <- TRUE
					nodes_info_grw <- nodes[nodes$Id %in% grw_nodes,]
					nodes_info_grw <- merge(nodes_info_grw, data_producer, by.x='Id', by.y='id_category')
					V(g)$growing <- NA
					V(g)$growing[V(g)$name %in% active_nodes] <- 0
					V(g)$growing[V(g)$name %in% grw_nodes] <- 1
				}
				if(nrow(dev_nodes_df)>0)
				{
					dev_nodes <- as.vector(dev_nodes_df[,'id_category'])
					dev_nodes <- strtoi(dev_nodes)
					V(g)$developed <- NA #only active nodes could be evaluated
					V(g)$developed[V(g)$name %in% active_nodes] <- 0 #only active nodes could be evaluated
					V(g)$developed[V(g)$name %in% dev_nodes] <- 1 #is it occupied
				}

				
			}#end no data production
			else
			{
				#DEFINE ZERO for a period without data, this is needed because otherwise it is impossible to plot the graph
				print(paste("NO DATA HERE!! For producer:", producer, "INterval:", interval))
				#go to next interval since there is no data
				#flag_no_data_user <- TRUE
				break #jump next interval
			}
			
			####MEASURING DENSITies
			#V(g)$sum_prox <- graph.strength(g) #weighted degree used to calculate densities ORIGINAL
			#V(g)$sum_prox <- graph.strength(g, mode = "in") #weighted degree used to calculate densities MODIFIED
			V(g)$sum_prox <- graph.strength(g, mode = "out") #weighted degree used to calculate densities MODIFIED
			
			#g <- delete.edges(g, E(g)[E(g)$used < 1])
			
			#which are the nodes you want to asume as active or developed.
			for(pred in c('act', 'dev')) #activation or developing
			{
				E(g)$weight_bool <- 0
				if(pred == 'act')
				{
					V(g)$dev_temp <- V(g)$active	
					#print(paste("Sum Act Temp"))
					#print(sum(V(g)$dev_temp))
					#print(V(g)$dev_temp)
				}
				if(pred=='dev')
				{
					V(g)$dev_temp <- V(g)$developed	
					V(g)$dev_temp[is.na(V(g)$dev_temp)] <- 0
					#print(paste("Sum DEV Temp"))
					#print(sum(V(g)$dev_temp))
					#print(V(g)$dev_temp)
				}
				
				#V(g)$dev_temp[is.na(V(g)$dev_temp)] <- 0
				#E(g)$weight_bool[ E(g)[from (V(g)$name[V(g)$dev_temp ==1 ])] ] <- 1 #make 0 those edges that are not connecting occupaid nodes, and 1 those edges connected to nodes active. NOTE that, by using this technnique,for active nodes, density will be 1
				E(g)$weight_bool[ E(g)[to(V(g)$name[V(g)$dev_temp ==1 ])] ] <- 1 #make 0 those edges that are not connecting occupaid nodes, and 1 those edges connected to nodes active. NOTE that, by using this technnique,for active nodes, density will be 1
				#print(paste("LINKS IN ", pred))
				#print(sum(E(g)$weight_bool))
				
				E(g)$weight_occ <- E(g)$weight * E(g)$weight_bool #copy weight for active links
				V(g)$sum_prox_occ <- graph.strength(g,weights=E(g)$weight_occ)  #
				if(pred =='act')
				{
					V(g)$dens_act <- V(g)$sum_prox_occ/V(g)$sum_prox  #densities
					V(g)$dens_act[V(g)$name %in% dev_nodes] <- 0  #cleanning density of active nodes that is 1.
				}
				if(pred =='dev')
				{ 
					V(g)$dens_dev <- V(g)$sum_prox_occ/V(g)$sum_prox  #densities
					V(g)$dens_dev[V(g)$name %in% dev_nodes] <- 0  #cleanning density of developed nodes that is 1.
				}
			}#end for type of prediction   
			
			evaluation<-''
			
			if(i > 1) #there is a previous state
			{
				#defining states transition means that became developed
				V(g)$ts_dragons <- V(g_prev)$inactive * V(g)$inactive
				V(g)$ts_introduction <- V(g_prev)$inactive * V(g)$undeveloped
				V(g)$ts_growth <- V(g_prev)$undeveloped * V(g)$growing
				
				V(g)$ts_decline <- V(g_prev)$developed * V(g)$undeveloped
				#totally binary from inactive to active
				V(g)$ts_activated <- V(g_prev)$inactive * V(g)$active
				V(g)$ts_transition <- V(g_prev)$undeveloped * V(g)$developed
				V(g)$ts_maturity <- V(g_prev)$growing * V(g)$developed
				
				#print("values..")
				#verify_g<<- data.frame(V(g)$occupied, V(g)$name, V(g)$label,V(g)$active, V(g_prev)$active, V(g_prev)$inactive, V(g)$st_transition)#
				prev_interval <- intervals[i-1]
				#evaluation_roc <- data.frame(producer, prev_interval, interval, V(g)$name, V(g)$Label, V(g_prev)$active,  V(g_prev)$inactive,  V(g_prev)$undeveloped, V(g_prev)$growing, V(g_prev)$developed, V(g)$inactive, V(g)$active, V(g)$undeveloped, V(g)$growing,V(g)$developed, V(g)$ts_dragons, V(g)$ts_introduction,V(g)$ts_growth,V(g)$ts_maturity ,V(g)$ts_decline, V(g)$ts_activated, V(g)$ts_transition )#
				evaluation_roc <- data.frame(what_eval, producer, prev_interval, V(g_prev)$value_eval, V(g_prev)$dens_act, V(g_prev)$dens_dev, interval, V(g)$value_eval, V(g)$name,  V(g_prev)$active,  V(g)$active, V(g)$ts_activated, V(g_prev)$undeveloped, V(g)$developed, V(g)$ts_transition, V(g_prev)$growing, V(g)$developed, V(g)$ts_maturity) #
				
				#print(evaluation_data)
				evaluation_data_total <- rbind(evaluation_data_total, evaluation_roc)
				#print(paste("interval:", i, "  Added user:", producer))
				#label for plot
			
				if(pl==TRUE)
				{
					#pending plot if needed
					#Pass evaluation
					par(mfrow=c(1,1)) #starts template for roc plots 
					plot_overlay_data(g_prev, pl_min_dens=pl_min_dens, pl_node_sc=pl_node_sc, pl_min_size=pl_min_size, pl_num_lab = pl_num_lab, cex=cex, pl_w_pdf = pl_w_pdf)
					
					plot_overlay_data(g, pl_min_dens=pl_min_dens, pl_node_sc=pl_node_sc, pl_min_size=pl_min_size, pl_num_lab = pl_num_lab, cex=cex, pl_w_pdf = pl_w_pdf)
					#print("Here plotting!")
				}#end plotting
				
				
				if(pl_roc == TRUE)
				{
				  #print("Ploting ROC Curves")
				  roc_results_prod <- plot_rocs(data_eval_roc = evaluation_roc, pl=TRUE)
				}
				
			}#end loop i >1 means state with a previous state to evaluate

			#save for next iteration
			g_prev <- g
			#g_kk <<- g
			prev_num_act <- length(na.omit(active_nodes)) #any production 
		}#end interval
		
		#if(flag_no_data_user == TRUE)
		#	next #jump next user if no data for user in interval
	}#end producers
	
	#evaluating data of 
  print(evaluation_data_total)
	str(evaluation_data_total)
	#file_n <- file.path(path_interval_overlay,paste("DATA_EVALUATION_ROC",interval_label,"min_prod",min_prod,"samp",n, "trans", pred, ".csv"))
	#write.csv(x=evaluation_data_total, file=file_n, row.names=FALSE)
	#print(paste("wrote file", file_n ))
	data_eval_roc <<- evaluation_data_total
	
	return(evaluation_data_total)
}

#plots data overlaid and evaluated using density and transitions
#g graph containing transitions ande previous states
plot_overlay_data <- function(g,  pl_min_dens=0.15, pl_node_sc=2.5, pl_min_size=0.7, pl_num_lab = 5, prop_to_lab=0.2, cex=1, pl_w_pdf=FALSE)
{
	#color palette <- INactive, Active, Growing, Advantage, Opportunity
	#green palette
	#color_palette <- c('#edf8e9', '#bae4b3', '#74c476', '#238b45','white','blue' )
	#yellow-red heat palette 
	#color_palette <- c('#ededed', '#fecc5c', '#fd8d3c','#e31a1c', 'white', 'green')
	color_palette <- c('#ededed', '#fecc5c', '#fd8d3c','#e31a1c', 'white', 'blue')
	node.colors <- c("white", rev(heat.colors(3)), "white") # from empty to developed 4 states
	edge.colors <- c('#ededed', '#ededed', '#ededed', "#bcbcbc", "#bcbcbc" ) #from inactive, connecting active, connecting developed
	frame.colors <- c('#ededed', '#ededed', '#ededed', "#bcbcbc",  "black") #from base, to recommended, to True Positive
	
	#defining some values
	num_dragons <- length(V(g)$ts_dragons[V(g)$ts_dragons==1 & !is.na(V(g)$ts_dragons)])
	num_introduction <-  length(V(g)$ts_introduction[V(g)$ts_introduction==1 & !is.na(V(g)$ts_introduction)])
	num_growth <-  length(V(g)$ts_growth[V(g)$ts_growth==1 & !is.na(V(g)$ts_growth)])
	num_maturity <-  length(V(g)$ts_maturity[V(g)$ts_maturity==1 & !is.na(V(g)$ts_maturity)])
	num_decline <- length(V(g)$ts_decline[V(g)$ts_decline==1 & !is.na(V(g)$ts_decline)] )
	num_activated <- length(V(g)$ts_activated[V(g)$ts_activated==1 & !is.na(V(g)$ts_activated)] )
	num_transition <- length(V(g)$ts_transition[V(g)$ts_transition==1 & !is.na(V(g)$ts_transition)] )
	
	#setting default values for vertices 
	V(g)$color <- node.colors[1]
	V(g)$frame.color <- frame.colors[1]
	V(g)$size <- pl_min_size
	#print(paste("HEREE MIn size", pl_min_size))
	#V(g)$label_orig <- V(g)$label  #to export
	V(g)$Label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
	V(g)$label <- ''
	#setting default values for edges
	E(g)$color <- edge.colors[2] #coloring edges connecting active nodes
	
	#ACTIVE NODES  ######
	active_nodes <- V(g)$name[V(g)$active == 1]
	V(g)$color[V(g)$name %in% active_nodes] <- node.colors[2]
	#V(g)$share[V(g)$name %in% active_nodes]<- as.numeric(nodes_info_act[,'share']) 
	max_share <- max(V(g)$share, na.rm=TRUE);	min_share <- min(V(g)$share, na.rm=TRUE)
	#print(paste("max", max_share, "min", min_share))
	#V(g)$size[V(g)$name %in% active_nodes] <-  V(g)$share[V(g)$name %in% active_nodes]
	V(g)$size[V(g)$name %in% active_nodes] <- (((V(g)$share[V(g)$name %in% active_nodes] - min_share )/(max_share-min_share) ) + pl_min_size) * pl_node_sc
	
	#UNDEVELOPED NODES
	und_nodes <- V(g)$name[V(g)$undeveloped == 1]
	
	#GROWING NODES  #####
	grw_nodes <- V(g)$name[V(g)$growing == 1]
	V(g)$color[V(g)$name %in% grw_nodes] <- node.colors[3]
	
	
	#DEVELOPED NODES #######
	dev_nodes <- V(g)$name[V(g)$developed == 1]
	V(g)$color[V(g)$name %in% dev_nodes] <-node.colors[4]
	if(length(dev_nodes)>0)
	{
		#labeling all edges from recommended nodes
		#E(g)$label[ E(g)[from (V(g)[V(g)$density>pl_min_dens]) ] ] <-  E(g)$weight[ E(g)[from (V(g)[V(g)$density>pl_min_dens]) ] ] #bug here, something happen 
		E(g)$color[E(g)$weight_bool==1] <- edge.colors[4] #coloring edges connecting active nodes
		#if(length(dev_nodes) < (prop_to_lab * length(rownames(nodes))) )
		 # V(g)$label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
		#else
		#{
		  #print("Filtering labels...")
		  nod_max_com <- get_max_com(g)  #get the nodes with max degree per community
		  #print(nod_max_com)
		  V(g)$label <- ''
		  #V(g)$label[V(g)$name %in% nod_max_com] <- as.character(nodes$subd_name[match(nod_max_com, nodes$Id)])
		  nodes_to_label <- V(g)$name[V(g)$name %in% intersect(nod_max_com,dev_nodes)] #to get the order of iGraph
		  V(g)$label[V(g)$name %in% intersect(nod_max_com,dev_nodes)] <- as.character(nodes$subd_name[match(nodes_to_label, nodes$Id)])
		#}
		
	}
		
	#RECOMMENDED NODES #### (density > 0)
		#adding labels to nodes
		active_nodes_density_lab <- V(g)$name[V(g)$dens_dev>0] #nodes that you have to LABEL becausse of the density.
		nodes_info_density <- nodes[nodes$Id %in% active_nodes_density_lab,]
		
		#V(g)$label[V(g)$name %in% active_nodes_density_lab] <- paste(strtrim(nodes_info_density$subd_name,5), as.character(round(V(g)$density[V(g)$name %in% active_nodes_density_lab],2))) #label with name and value of density
		#V(g)$label[V(g)$name %in% active_nodes_density_lab] <- paste( as.character(round(V(g)$density[V(g)$name %in% active_nodes_density_lab],2))) #label with name and value of density
		
		#V(g)$size[V(g)$name %in% active_nodes_density_lab] <- V(g)$density[V(g)$name %in% active_nodes_density_lab] #size of recommended nodes, according density
		#V(g)$size[V(g)$name %in% active_nodes_density_lab] <- pl_min_size
		#create instead a color map from white to black for recommended values according to density
		#density_nodes_g <- V(g)$name[V(g)$name %in% active_nodes_density_lab] #TO GET THE order that provides iGraph
		#nodes_info_act <- nodes_info_act[match(active_nodes_g, nodes_info_act$Id),] #reordering
		
		#V(g)$color[V(g)$name %in% active_nodes_density_lab] <- color_palette[5]
		V(g)$frame.color[V(g)$name %in% active_nodes_density_lab] <- frame.colors[5] #color_palette[6]
		#V(g)$color[V(g)$name %in% active_nodes_density_lab] <- node.colors[5] #color_palette[6]
		
		
		#LEGEND AND TITLES ###
		#MUST USE AS.CHARACTER, OTHERWISE it will take an integer resulting in extrange colors
		#V(g)$color[V(g)$name %in% active_nodes_density_lab] <- as.character(nodes_info_density$color)
		num_act <- length(na.omit(active_nodes)) #any production 
		num_dev <- length(na.omit(dev_nodes)) #developed area
		num_grw <- length(na.omit(grw_nodes)) #growing area
		num_und <- length(na.omit(und_nodes)) #underdeveloped area 
		num_ina <- vcount(g)-num_act
		measures <- paste('Undeveloped <', round(g$cut_off_to_grw, 4),  '< Growing Areas <', round(g$cut_off_to_dev, 4), 'Developed Areas', '\n')

		num_rcm <- length(active_nodes_density_lab)
		densities <- ""
		#densities <- paste("Transition:" ,"Undeveloped to Developed."," Recommended nodes (connected to developed nodes) for the next period (dark gray):",as.character(num_rcm))
		measures <- paste(measures,densities, sep="  ")

		
		#titles
		prod_name <- get_prod_name(g$producer)
		prod_domain <- get_prod_domain(g$producer)
		title <- paste( prod_name," ", g$interval,  '\nBase Map: ', taxo, ' - Overlay Data: ', dataset, " - Evaluating: ", toupper(g$what_eval),sep="")
		file_name <- paste('OverlayMap', taxo, prod_domain,g$interval, g$what_eval, sep='_')
		#subtitle <- paste('Layout: Frughtermand Rengold', '| Size: Share of authorships', '| Color: Areas of Science (original colors) \n' , measures, '\n', evaluation)
		subtitle <- paste('Layout: Fruchterman–Reingold', '| Size: Share of authorships', '| Color: Values of ', toupper(g$what_eval), " \n Agg. function: ", "sum", '\n' , measures)
		#exporting graph to a dataframe
		#V(g)$cut_off_to_dev <- cut_off_to_dev
		#vg_final<<- data.frame(  V(g)$name, V(g)$label, V(g)$color, V(g)$active,V(g)$inactive,V(g)$size, V(g)$shares, V(g)$cut_off_to_dev, V(g)$developed, V(g)$undeveloped )#
		par(lend = 1)           # square line ends for the color legend
		legend = c(paste("Inactive", num_ina), paste("Undeveloped", num_und), paste("Growing", num_grw), paste("Developed", num_dev), paste("Opportunity", num_rcm)) # category labels   
		plot_graph_overlay(g, layout='fr', title = title , subtitle=subtitle, file_name=file_name, legend=legend, l_pt.bg=node.colors, l_col=frame.colors, cex=cex, pl_w_pdf=pl_w_pdf )
		
}


#used to plot overlay maps over benchmark maps
plot_graph_overlay <- function(g,layout='fr',title=paste('Overlay Map -',taxo), subtitle='', file_name=paste('Overlay Map -',taxo), legend=NULL, l_pt.bg,l_col, cex=1, pl_w_pdf=FALSE )
{
	set.seed(77)
	#gms$layout <- layout.drl(gms)
	#plotMST(gms,'drl')
	#gms$layout <- layout.auto(gms)
	#plotMST(gms,'auto')
	#gms$layout <- layout.circle(gms)
	#plotMST(gms,'circle')
	if(layout=='fr')
	{
		g$layout <- layout.fruchterman.reingold(g) 
		lay <- 'Fruchterman Reingold'
	}
	
	#V(g)$Label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
	#higher aggregation
	#V(g)$Field = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
	#V(g)$Area = as.character(nodes$Discipline[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
	#adding color to vertex
	#V(g)$label = V(g)$Field
	# V(gms)$size = (degree(gms)/max(degree(gms))) * 6
	
	#V(g)$color = as.character(nodes$color[match(V(g)$name, nodes$Id)])  
	#plot.igraph(g, vertex.size=5, vertex.label.cex=0.5, asp=FALSE,main=title)
	# plot.igraph(g,  vertex.label.cex=0.7, edge.curved=TRUE,  vertex.label.font=0, vertex.label.family='Arial', vertex.label.color='black', asp=FALSE,main=title)
	#plot.igraph(g, sub=paste('Layout:' ,subt, '. Size: degree.', 'Colored by Area of Science.'), vertex.label.cex=0.7, vertex.label.font=0, vertex.label.family='Arial', vertex.label.color='black',main=title, asp=FALSE)
	#info_credit <- bench_credit
	#info_template <- paste('Layout WE applied:' , lay, '| Size:', 'production', '| Colored by:', bench_color)
	#info_interval <- paste('Data time: ', bench_interval, ' | Data Source: ', bench_source, ' | Unit of analysis: ', bench_unit, '| Technnique: ', bench_tech)
	info_credit <- 'info'
	info_template<- 'fr'
	info_interval <- '2001'
	
	plot.igraph(g, 
		sub=list(paste(subtitle),cex=0.7*cex), 
		vertex.label.cex=0.6*cex, 
		vertex.label.font=0, 
		vertex.label.family='Helvetica', 
		vertex.label.color='black', 
		edge.label.cex=0.5*cex, 
		edge.label.font=0, 
		edge.label.family='Helvetica', 
		main=list(title,cex=0.9*cex), 
		asp=FALSE)
	#dev.off()
	legend("bottomleft",      # location of the legend on the heatmap plot
		legend = legend,
		pch=21, merge=FALSE,
		pt.bg = l_pt.bg,
		col = l_col,
		lty= c(1,1,1,1,NA),             # line style
		lwd = 1,           # line width
		pt.cex = 0.8*cex,
		box.col = "lightgrey",
		#fill = "white"
		cex= 0.6*cex
	)
	
	if(pl_w_pdf==TRUE)
	{
		dev_file_name <- file.path(path_interval_overlay, paste(file_name,'.pdf', sep=''))
		#dev.print(pdf, file=dev_file_name, widht=6, height=3 );
		pdf(dev_file_name, width=16, height=12, family='Helvetica', pointsize=8)
		#, edge.label.color='black'
		
		plot.igraph(g, 
			sub=list(paste(subtitle), cex=0.8*cex), 
			vertex.label.cex=0.8*cex, 
			vertex.label.font=0, 
			vertex.label.family='Helvetica',  
			vertex.label.color='black',  
			edge.label.cex=0.5*cex, 
			edge.label.font=0,
			edge.label.family='Helvetica', 
			main=title, 
			asp=FALSE)
		
		legend("bottomleft",      # location of the legend on the heatmap plot
			legend = legend,
			pch=21, merge=FALSE,
			pt.bg = l_pt.bg,
			col = l_col,
			lty= c(1,1,1,1,NA),             # line style
			lwd = 3,           # line width
			pt.cex = 2,
			box.col = "lightgrey",
			#fill = "white"
			cex= 1
		)
		
		dev.off()
		#dev.copy(pdf,filename=dev_file_name, family='Helvetica');
		#dev.off ();
	}
	
	
}


#this function read data in intervals, aggregate it and computes measures of normalization of the data as RCA. Also subset and filter per a list or value of producers
get_data_to_overlay <- function(n=10, init=2013, end=2015, by=1, cum=FALSE, min_prod=1, what_agg='wgh_jfrac', agg_fun=sum, list_prod=NaN )
{
  #get sample of producers
  data_interval <- load_data_interval(init = init, end = end, agg=NaN) #without aggregation
  num_years <- end-init
  colnames(data_interval)[match(what_agg,colnames(data_interval))] <- 'value'
  #if there is no a list of target producers
  if(is.na(list_prod)[[1]])
  {
    #totals_producers <- aggregate(authorship_count~id_author , data_interval, FUN=sum)  
    totals_producers <- aggregate(value~id_author, data_interval, FUN=sum)  
    #producers accomplish min production
    #producers <- totals_producers$id_author[totals_producers$authorship_count> (num_years * min_prod)]
    producers_ids <- totals_producers$id_author[totals_producers$value> (num_years * min_prod)] #FILTER HERE!!!!
    
    if(n==-1)
    {
      n <- length(producers_ids)
    }
    #producers <- unique(data_interval[,1])
    #print(paste("Number producers:",length(producers)))
    producers_sample <- producers_ids[sample(length(producers_ids), n,replace = FALSE)]
  }#end sampling producers
  else
  {
    producers_sample <<- list_prod
  }
  #print(paste("Number producers SAMPLE:",length(producers_sample)))
  
  ##get intervals
  get_intervals_overlay(year_ini=init, year_fin=end,win=by)
  
  #data with producers choosen
  data_sample <- subset(data_interval, id_author %in% producers_sample)  #watch out with the name of the column of producers.
  
  #aggregating data for interval, producer, category, value, and calculating Shares, RCAs
  data_to_use <- data.frame()
  for(interval in intervals)
  {
    i <- match(interval, intervals) #index of parameter interval
    if(cum == TRUE)
      year_s <- init
    else
      year_s <- year_start[i]  
    
    year_e <- year_end[i] #simplify names #USING LOOONG AGGREGATION of time
    data_int_raw <- subset(data_sample, year >= year_s & year <= year_e)
    #data_int <- aggregate(x=data_int_raw['authorship_count'], by=list(id_author=data_int_raw$id_author, id_category = data_int_raw$subdiscipline_id), FUN=agg_fun)
    data_int <- aggregate(x=data_int_raw['value'], by=list(id_author=data_int_raw$id_author, id_category = data_int_raw$subdiscipline_id), FUN=agg_fun) #totalof production of a producer in a category
    total_global <- sum(data_int$value)
    data_int <- data.frame(interval, data_int)
    #data_prod_total <- aggregate(x=data_int['authorship_count'], by=list(id_author=data_int$id_author), FUN=sum)  #finding total production by producer
    data_prod_total <- aggregate(x=data_int['value'], by=list(id_author=data_int$id_author), FUN=sum)  #finding total production by producer
    #print(data_prod_total)
    data_int <- merge(data_int,data_prod_total, by='id_author') #adding total production to the table
    #total per category
    colnames(data_int)[colnames(data_int)=='value.x'] <- 'value'
    colnames(data_int)[colnames(data_int)=='value.y'] <- 'total_producer'
    #data_int['share'] <- data_int$authorship_count.x / data_int$authorship_count.y #computing shares
    data_int['share'] <- data_int$value / data_int$total_producer #computing shares
    #aggregate
    
    data_categ_total <-  aggregate(x=data_int['value'], by=list( id_category = data_int$id_category), FUN=agg_fun) #total of that category
    data_int <- merge(data_int, data_categ_total, by='id_category')
    colnames(data_int)[colnames(data_int)=='value.x'] <- 'value'
    colnames(data_int)[colnames(data_int)=='value.y'] <- 'total_category'
    
    data_int['total_global'] <- total_global
    data_int['share_category'] <- data_int$total_category / total_global
    data_int['rca'] <- data_int$share/data_int$share_category
    data_int['rca_bool'] <- data_int['rca']
    data_int$rca_bool[data_int$rca_bool < 1] <- 0
    data_int$rca_bool[data_int$rca_bool != 0] <- 1
    
    data_to_use <- rbind(data_to_use,data_int)
  	
  }
  
	colnames(data_to_use)[colnames(data_to_use) == 'id_author'] <- 'id_producer'
	
  return(data_to_use)
  #________________________
}#end function

#evaluate roc curves
#evaluate roc for each producer
plot_rocs <- function(data_eval_roc, pl=FALSE)
{
  require(MESS) #getting area under the curve
  
  intervals_pred <- unique(data_eval_roc$interval)
  data_eval_roc_act <- data.frame()
  roc_total <- data.frame()
  
  for(prod in unique(data_eval_roc$producer))
  {
  	roc_prod <- data.frame()
  	
    for(inter in intervals_pred)
    {
    	
      #analyzing ROC curves
      #print(prod)
      #print(inter)
      data_eval_prod <- subset(data_eval_roc, producer==prod & interval==inter)
      prev_interval <- data_eval_prod$prev_interval[1]
      
      roc_int <- data.frame(prod, prev_interval, inter) 
      #print(nrow(data_prev_inactive))
      #roc_int['producer'] <- prod
      #roc_int['interval'] <- inter 
      roc_int['auc_I_A'] <- NA; roc_int['n_I_A'] <- NA;  roc_int['tp_I_A'] <- NA
      roc_int['auc_U_D'] <- NA; roc_int['n_U_D'] <- NA; roc_int['tp_U_D'] <- NA
      roc_int['auc_G_D'] <- NA; roc_int['n_G_D'] <- NA; roc_int['tp_G_D'] <- NA
      
      data_prev_inactive <- subset(data_eval_prod, V.g_prev..active==0)
      
      if(pl==TRUE)
      	par(mfrow=c(2,3)) #starts template for roc plots 
      
      ############EVALUATING INACTIVE TO ACTIVE
      if(nrow(data_prev_inactive)==0) #no previous InActive data
      {
        print(paste("producer:" , prod, "Has no previous INactive data in interval", inter))
      }
      else
      {
        data_eval_sor <- data_prev_inactive[order(-data_prev_inactive$V.g_prev..dens_act),]
        data_eval_roc_act <- rbind(data_eval_roc_act, data_eval_sor)  #aggregate to see data used to evaluate
        res_ts <- plot_roc_interval(data_eval_sor, positive="V.g..active", trans="Inactive to Active", col='brown', pl=pl)
        roc_int['auc_I_A'] <- res_ts[1]
        roc_int['n_I_A'] <- res_ts[2]
        roc_int['tp_I_A'] <- res_ts[3]
      }#end else no data
      
      
      
      ##############EVALUATING GROWING  TO  DEVELOPED
      data_prev_growing <- subset(data_eval_prod, V.g_prev..growing==1)
      if(nrow(data_prev_growing)==0) #no previous InActive data
      {
        print(paste("producer:" , prod, "Has no previous UNDEVELOPED data in interval", inter))
      }
      else
      {
        data_eval_sor <- data_prev_growing[order(data_prev_growing$V.g_prev..dens_dev, decreasing = TRUE),]
        data_eval_roc_act <- rbind(data_eval_roc_act, data_eval_sor)  #aggregate to see data used to evaluate
        res_ts <- plot_roc_interval(data_eval_sor, positive="V.g..developed", trans="Growing to Developed", col='orange', pl=pl)
        roc_int['auc_G_D'] <- res_ts[1]
        roc_int['n_G_D'] <- res_ts[2]
        roc_int['tp_G_D'] <- res_ts[3]
      }#end else No previous data
      
      
        ##############EVALUATING UNDEV TO  DEVELOPED 
      data_prev_undeveloped <- subset(data_eval_prod, V.g_prev..undeveloped==1)
      if(nrow(data_prev_undeveloped)==0) #no previous InActive data
      {
        print(paste("producer:" , prod, "Has no previous UNDEVELOPED data in interval", inter))
      }
      else
      {
	      data_eval_sor <- data_prev_undeveloped[order(data_prev_undeveloped$V.g_prev..dens_dev, decreasing = TRUE),]
	      data_eval_roc_act <- rbind(data_eval_roc_act, data_eval_sor)  #aggregate to see data used to evaluate
	      res_ts <- plot_roc_interval(data_eval_sor, positive="V.g..developed", trans="Undeveloped to Developed", pl=pl)
	      roc_int['auc_U_D'] <- res_ts[1]
	      roc_int['n_U_D'] <- res_ts[2]
	      roc_int['tp_U_D'] <- res_ts[3]
      }#end else No previous data
      
      if(pl==TRUE)
      {
	      	#labaling entitlining main template of roc curves
	      prev_interval <- data_eval_prod$prev_interval[1]
	      what_eval <- toupper(data_eval_prod$what_eval[1])
	      mtext(paste(get_prod_name(prod), "| From: ", prev_interval, "To:", inter, "| Evaluating:", what_eval), side=3, outer=TRUE, line=-3)
      }
      
      roc_prod <- rbind(roc_prod, roc_int) #accumulates intervals for the same producer
    }#end for interval
  	roc_total <- rbind(roc_total, roc_prod) #accumulates producers
  }#end for producer
  
  
  roc_kk<<- roc_total
  data_eval_roc_used <<- data_eval_roc_act
  return(roc_total)
}

#ploting ROC curves for density evaluatio
plot_roc_interval <- function(data_eval_sor, positive, trans="", col="skyblue", pl= TRUE  )
{
  #print("HEEEYYYY")
  prev_interval <- data_eval_sor$prev_interval[1]
  interval <- data_eval_sor$interval[1]
  prod <- as.character(droplevels(data_eval_sor$producer[[1]]))
  what_eval <- data_eval_sor$what_eval[1]
  
  #setting default or null values
  auc_val <- NA #area under the curve
  n_to_eval <- NA #n of sample to evaluate
  n_tp <- NA #n of true positives
  
  #changing NA per 0
  data_eval_sor[, positive][is.na(data_eval_sor[, positive])] <- 0
  
  if(sum(!data_eval_sor[,positive], na.rm=TRUE) != 0 && sum(data_eval_sor[1:(length(data_eval_sor)-1),positive], na.rm=TRUE)!= 0 ) #if the last value is positive AUC is divergent and an error is produced!!!!
  {
    x <- cumsum(!data_eval_sor[,positive]) / sum(!data_eval_sor[,positive], na.rm=TRUE)
    y <- cumsum(data_eval_sor[,positive]) / sum(data_eval_sor[,positive], na.rm=TRUE)
    #plot(x,y)
    #print(paste(x,y))
    n_to_eval <- nrow(data_eval_sor) #n of sample to evaluate
    n_tp <- sum(data_eval_sor[,positive]) #n of true positives
    
    auc_val <- auc(x,y, type = 'spline') #area under the curve #this gives error if there is only one TP (y value) and it is at the end!!!.
    
	  
    if(pl == TRUE)
    {
    	plot(x, y, 
    		#main=paste(get_prod_name(prod),  trans, toupper(what_eval), sep="  |  "), 
    		#sub=paste(" From: ", prev_interval, "To:", interval),
    		sub=paste(paste(trans)),
    		xlab="False Positives",
    		ylab = "True Positives",
    		asp = 1,
    		col = col, 
    		cex.sub =1.2,
    		xlim = c(0,1),
    		ylim = c(0,1)
    	)
    	lines(c(0,1), c(0,1), col="lightgray")
    	#abline(0, 1,  col = "lightgray") #diagonal
    	legend("bottomright", 
    		legend=c(paste("AUC=",round(auc_val,3)), paste("N to EVAL=", n_to_eval),  paste("TP= ",n_tp ), paste("FP= ",(n_to_eval-n_tp) )),
    		cex=0.6)
    }
	    
    
  }   
  else
  {
    print(paste("No possible evaluation for", prod, " In interval ", interval, " Transition", trans))
  	auc_val <- NA
  }
  
  return(c(auc_val, n_to_eval, n_tp))
     
}


get_prod_name <- function(id_prod)
{
	return(prod_name <- as.character(producers$name[producers$id==id_prod]))
}
get_prod_domain <- function(id_prod)
{
	prod_domain <- as.character(producers$i.domain[producers$id==id_prod])
}

