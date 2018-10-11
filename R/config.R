config <- function( db='gscholar', cl='ucsd', ms='prob', pr='ins')
{
  #db data base to use
  #cl classifciation
  #ms measure to find the proximity
  #pr producer level, i.e. institution ins, individual ind, country cnt
  library(igraph)
  #library(ape)
  dataset <<- toupper(db)
  taxo <<- toupper(cl)
  granu <<- toupper(pr)
  path_data <<- "~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA"
  path_data_set <<-file.path(path_data, paste(dataset,taxo,granu, sep = '_') )
  path_taxonomy <<- file.path(path_data, 'TAXONOMIES', taxo)
  path_producers <<- file.path(path_data, 'LINKS_PRODUCERS')
  producers <<- get_producers()
  path_raw <<- file.path(path_data_set, 'RAW')
  path_benchmark <<- file.path(path_data, 'BENCHMARK')
  path_overlay <<- file.path(path_data_set, 'OVERLAYS')
  path_rs <<- file.path(path_data, 'RESEARCH_SPACE', taxo )
  nodes <<- read.csv(file.path(path_taxonomy,'nodes.csv')) #we assume that a file nodes.csv exists in each taxonomy folder
  links <<- read.csv(file.path(path_taxonomy, 'edges.csv')) #we assume that there is a file edges.csv in each taxonomy folder
  setup_benchmarks()
  setup_datasets()
}

get_producers <- function()
{
  if(granu=='INS')
  {
    prod <- read.csv(file.path(path_producers, 'institutions.csv'))
    colnames(prod)[2] <- 'name'
  }
  if(granu=='CNT')
  {
    prod <- read.csv(file.path(path_producers, 'countries.csv'))
    prod <- prod[,c(3,2,1)]
    colnames(prod) <- c('id','name','id_opus')
  }
  if(granu=='IND')
  {
    prod <- read.csv(file.path(path_producers,'authors.csv'), colClasses=c("factor", NA, 'NULL', 'NULL', 'NULL',NA,'NULL','NULL','NULL') )
    colnames(prod) <- c('id', 'name', 'ins_domain')
  }
  return(prod)  
}

setup_benchmarks <- function()
{
  if(taxo == "UCSD")
  {
    bench_credit <<- "Börner, K. et al. Design and Update of a Classification System: The UCSD Map of Science. PLoS ONE 7, e39464 (2012)."
    bench_source <<- "ISI and SCOPUS"
    bench_interval <<- "2001-2010"
    bench_color <<- "13 Areas of Science (original colors)"
    bench_size <<- "Production of Science"
    bench_unit <<- "Journals"
    bench_tech <<- "Bibliographic Coupling" #K50
  }
  if(taxo == "SOM")
  {
    bench_credit <<- "Rafols, I., Porter, A. L. & Leydesdorff, L. Science overlay maps: A new tool for research policy and library management. Journal of the American Society for Information Science and Technology 61, 1871–1887 (2010)."
    bench_source <<- "ISI"
    bench_interval <<- "2007"
    bench_color <<- "18 Areas of Science  (original colors)"
    bench_size <<- "Production of Science"
    bench_unit <<- "Journals"
    bench_tech <<- "Inter Citation" #cosine similarity
  }
  if(taxo == "KSA")
  {
    bench_credit <<- "King of South Arabia Report (2013)."
    bench_source <<- "Google Scholar with SCimago Categories"
    bench_interval <<- "2000-2012"
    bench_color <<- "27 Areas of Science  (original colors)"
    bench_size <<- "Production of Science"
    bench_unit <<- "Researchers"
    bench_tech <<- "Conditional Probability" #cosine similarity
  }
  if(taxo == "RSPACE-RCA")
  {
    bench_credit <<- "Research Space Experiments 2015."
    bench_source <<- "Google Scholar with UCSD Categories"
    bench_interval <<- "2000-2010"
    bench_color <<- "13 Areas of Science  (original colors)"
    bench_size <<- "Production of Science"
    bench_unit <<- "Researchers"
    bench_tech <<- "RCA with cosine similarity" #cosine similarity
  }
  
  if(taxo == "RSPACE-TFIDF")
  {
    bench_credit <<- "Research Space Experiments 2015."
    bench_source <<- "Google Scholar with UCSD Categories"
    bench_interval <<- "2000-2010"
    bench_color <<- "13 Areas of Science  (original colors)"
    bench_size <<- "Production of Science"
    bench_unit <<- "Researchers"
    bench_tech <<- "TfIdf with cosine similarity" #cosine similarity
  }
  
}

setup_datasets <- function()
{
  if(dataset == 'GSCHOLAR')
  {
    col_classes <<- c(NA, 'factor', 'factor',NA,NA,NA,NA)
    data_abr <<- 'GS'
  }
	
	if(dataset == 'SCIMAGO')
	{
		col_classes <<- c( NA, 'factor','factor', NA,NA) #year MUST NOT be a factor but an integer
		data_abr <<- 'SCIMAGO'
	}
}

define_paths_interval <- function()
{
  path_interval <<- file.path(path_data_set, 'ANALYSIS', interval_label)
    dir.create(path_interval, showWarnings=FALSE)
  gephi_dir <<- file.path(path_interval, 'GEPHI')
    dir.create(gephi_dir, showWarnings=FALSE)
  export_dir <<- file.path(path_interval, 'PROCESSED')
    dir.create(export_dir, showWarnings=FALSE)
  graphs_dir <<- file.path(path_interval,'GRAPHS')
    dir.create(graphs_dir, showWarnings=FALSE)
  images_dir <<- file.path(path_interval, 'IMAGES')
    dir.create(images_dir, showWarnings=FALSE)
}


#' @title Load Raw Data
#' @description this function read a folder and import raw data to a dataframe. It automatically finds separator.
#' @param n Number of lines to use- -1 implies all the lines
#' @param path_raw_files A path to local files where the files with data are located
#' @param col_classes A vector with configuration of what column to read and of what type. Use factor for IDs or labels and NA for default numeric values
#' @param sep A separator for columns in raw files. If NaN, an automatic process will be conducted to detect the separator.
#' @examples 
#' load_data()
#' @return a Data Frame to overlay.
#' @export
load_raw_data <- function(n=-1, path_raw_files="data_raw", col_classes= NaN, sep=NaN){
  print("Loading RAW DATA")
  #if(!exists("dataset")) config()
  print("looking files in ")
  print(file.path(path_raw_files))
  
  data <- data.frame()
  files <- list.files(file.path(path_raw_files))
  print("Files here:")
  print(files )
  
  sepr = sep
  
  for(file in files){
    #finding a proper separator
    if(is.na(sep)==TRUE) #defined by user
    {
      seps<- c(';',',','\t')
      i <- 1
      mat <- data.frame()
      while(length(mat) <= 1) #detecting separator automaticaly 
      {
        print(paste("Trying to read file", file, "with separator", seps[i]))
        mat <- read.csv(file.path(path_raw_files,file), nrows=3, sep=seps[i])
        if(length(mat)>1)
        {
          print("Choosen separator")
          sepr <- seps[i] #separator found
          print(sepr)
        }
          
        i <- i + 1
      } 
    }
    if(is.na(col_classes)==TRUE)
    {
      data <- rbind(data, read.csv(file.path(path_raw_files,file), nrows=n, sep=sepr))
      #data <<- read.csv(file.path(path_raw,file), nrows=10, sep=';', header = TRUE)  
    }
    else
    {
      data <- rbind(data, read.csv(file.path(path_raw_files,file), nrows=n, sep=sepr, colClasses=col_classes))
    #data <<- read.csv(file.path(path_raw,file), nrows=10, sep=';', header = TRUE)
    }
  }
  #print(summary(data))
  #changing name
  #colnames(data)[2] <- 'id_author' #assuming second column has the proper id producer
  return(data)
}

#' @title Load data intervals
#' @description this function look into data_raw and then subset by intervals aggregating information
#' @param data_raw a dataframe with production data use function load_raw_data()
#' @param init an integer pointing the initial year
#' @param end an integer pointing the final year
#' @param producer name of the column in data_raw that contains the name or id of producers. It should be factor.
#' @param category name of the column in data_raw that contains the name or id of categories. It should be factor.
#' @param value name of the column in data_raw that contains the feature to aggregate. should be numeric
#' @param agg function to aggregate, usually sum or mean. If NaN, no aggregation is computed
#' 
#' @examples 
#' load_data_interval()
#' @return a Data Frame to overlay.
#' @export
load_data_interval <- function(data_raw=load_raw_data(), init=NaN, end=NaN, year = 'year', producer, category, value="value", agg='mean')
{
  #print("Loading RAW DATA...")
  #if(!exists("data_raw"))   data_raw <<- load_data() 
  if(is.na(init)){
    init <- min(data_raw[year])
    print("Defined min year to ")
    print(init)
  }
  if(is.na(end)){
    end <- max(data_raw[year])
    print("Defined max year to ")
    print(end)
  }
  #print("Loading Interval DATA...")
  data <- subset(data_raw, year>= init & year<= end)
  #print(summary(data))
  
  #global variables for the names and information of the interval
  #file_name <<- paste(data_abr, taxo, feat, init, end, agg, sep="_")
  #n_registers <<- length(data$year)
  #n_years <<- length(unique(data$year))
  #n_authors <<- length(unique(data$id_author))
  #n_authorships <<- sum(data$authorship_count)
  #n_fields <<- length(unique(data$subdiscipline_id))
  
  #aggregating 
  if(agg=='sum') fun_agg<- sum
  if(agg=='mean') fun_agg<- mean
  if(agg=='median') fun_agg<-median
  if(!is.na(agg))
  {
    data <- aggregate(x=data[value], by=list(producer=data[producer][[1]], category = data[category][[1]]), FUN=fun_agg)  
  }
  
  
  #print(summary(data))
  
  return(data)
}
  
#MAIN FUNCTION
get_network <- function(init=-1, end=-1, by=-1, n=-1)  #main function
{
  data_raw <<- load_data(n)  
  get_intervals(year_ini=init, year_fin=end, win=by)
  
  for(interval in intervals)
  {
    i <- match(interval, intervals) #index of parameter interval
    data <- load_data_interval(init=year_start[i], end=year_end[i])
    net <- cond_prob(data) 
    geph <- export_gephi(net)  
  }
  return(net)
}



#' @title Get a dataframe with intervals
#' @description this function computes intervals according period of time
#' @param years Vector of years in raw data
#' @param year_ini Initital year
#' @param year_fin Final year
#' @param win time window
#' @param decade deprected
#' 
#' @examples 
#' get_intervals()
#' @return a Data Frame with year start, year end and interval name.
#' @export
get_intervals <- function(years, year_ini, year_fin, win=-1, decade=FALSE)
{
  
  if(win==-1){ win <- year_fin - year_ini + 1}  #take the whole interval
  
  year_start <- vector()
  year_end <- vector()
  interval <- vector()
  
  if(year_ini < min(years))
  {
    print(paste("Initial year Must be greater than", min(years)))
    stop("Stopped")
  } else
  {
    n_intervals <- floor(((year_fin -year_ini)+1)/win)
    y_s <- year_ini
    for(i in 1:n_intervals)
    {
      year_start[i] <- y_s
      year_end[i] <- y_s + (win - 1)
      interval[i] <- paste(year_start[i],year_end[i],sep="_")
      y_e <- year_end[i]
      y_s <- y_s + win
    }  
    #interval_label <<- paste("pan",year_ini,y_e,"by",win, sep="_")
    #path_interval_overlay <<- file.path(path_overlay, interval_label)
    #dir.create(path_interval_overlay, showWarnings=FALSE)
  }
  intervals <- data.frame(interval, year_start, year_end)
  #print(paste("year_start", year_start))
  #print(paste("year_end", year_end))
  return(intervals)
}




cond_prob <- function(data)
{
  # data must has this shape  producer | category | value
  library("reshape2")
  
  M_raw <- acast(data, id_author~id_category, fun.aggregate = sum) #use count for binary matrix
  categ <- colnames(M_raw)
  n_ctg <- length(categ)
  
  
  freq <- matrix(0.0, nrow=n_ctg, ncol=n_ctg) #creating empty matrix
  colnames(freq) <- categ
  row.names(freq) <- categ
  
  
  prob_cond <- freq
  prob_join <- freq
  phi <- freq # matrix of distances using Max conditional probability according to P. Space  
  
  M <- M_raw / rowSums(M_raw)  #shares or values, this means a normalization by the total production of the author
  k <- colSums(M) #sum of values over the categories
  
  freq <- t(as.matrix(M)) %*% as.matrix(M)
  prob_join <- freq/n_ctg  #simetric matrix Not used but maybe useful in the future
  prob_conditional <- freq / k  #k stores the total , NOT simetric matrix
  
  write.csv(prob_conditional, file.path(export_dir,paste(file_name,'_cond_prob_both.csv', sep="")))
  #defining the matrix of distances or proximities with the Min case, this is the Max Ubiquity in the denominator
  #phi is the matrix of distances
  for(i in 1:n_ctg){ #product p
    for(j in 1:n_ctg)
    {
      phi[i,j]<-phi[j,i]<-min(prob_conditional[i,j],prob_conditional[j,i])
    }
  }
  write.csv(prob_conditional, file.path(export_dir,paste(file_name,'_cond_prob_min.csv',sep="")))
  
  #computing MST
  #preparing export of edges
  
  
  get_mst(phi_for_mst)
  
  phi_ret <- melt(phi, na.rm = TRUE)  #EDGES LIST or PANEL SHAPE
  #return(phi_ret)
  return(prob_conditional)
}

get_graph <- function(phi,file_n=file_name, type='MST') #TYPE indicates which type of graph, as MST | TTest | Both
{
  #phi is a matrix equitriangular with proximities or similarities
  phi[phi==0] <- NA
  phi[upper.tri(phi, diag=TRUE)] <- NA
  phi_graph <- phi
  phi_graph[is.na(phi_graph)] <- 0
  phi_graph <- as.matrix(phi_graph)
  
  if(type=='MST')
  {
    #Create MinumumSpanningTree
    #d <<- dist(phi) # we are using euclidean distance for default
    d <<- 1-phi_graph  #In order to calculate MST we need a matrix of DISTANCES, this is 1 minus proximity phi.
    phi_mst_binary <- mst(d) #matrix with binary values of mst
    phi_graph <- as.matrix(phi_mst_binary)  #matrix with real values of mst 
  }
  
  
  
  gms <<- graph.adjacency(adjmatrix = phi_graph,mode = 'undirected',weighted = TRUE)
  
  if(type=='TTest')
  {
    g_temp <- decompose.graph(gms)
    gms <<- g_temp[[1]] #biggest connected component
  }
  
  #lower aggregation
  V(gms)$Label = as.character(nodes$subd_name[match(V(gms)$name, nodes$Id)]) #watch out with the name of the column!!
  #higher aggregation
  V(gms)$Field = as.character(nodes$subd_name[match(V(gms)$name, nodes$Id)]) #watch out with the name of the column!!
  V(gms)$Area = as.character(nodes$Discipline[match(V(gms)$name, nodes$Id)]) #watch out with the name of the column!!
  #adding color to vertex
  V(gms)$label = V(gms)$Field
  V(gms)$size = (degree(gms)/max(degree(gms))) * 6
  
  V(gms)$color = as.character(nodes$color[match(V(gms)$name, nodes$Id)])
  #gms$layout <- layout.drl(gms)
  #plotMST(gms,'drl')
  #gms$layout <- layout.auto(gms)
  #plotMST(gms,'auto')
  #gms$layout <- layout.circle(gms)
  #plotMST(gms,'circle')
  gms$layout <- layout.fruchterman.reingold(gms)
  plot_graph(gms,lay='fruchterman.reingold',title=paste(file_n,type,sep='_'))
  
  write.graph(gms,file=file.path(graphs_dir, paste(file_n,'_',type,'.gml', sep='')), format='gml')
  return(phi_graph)
}

#used to plot created maps (by us)
plot_graph <- function(g,lay='',title=paste('UNtype',file_name))
{
  set.seed(77)
  #plot.igraph(g, vertex.size=5, vertex.label.cex=0.5, asp=FALSE,main=title)
 # plot.igraph(g,  vertex.label.cex=0.7, edge.curved=TRUE,  vertex.label.font=0, vertex.label.family='Arial', vertex.label.color='black', asp=FALSE,main=title)
  #plot.igraph(g, sub=paste('Layout:' ,subt, '. Size: degree.', 'Colored by Area of Science.'), vertex.label.cex=0.7, vertex.label.font=0, vertex.label.family='Arial', vertex.label.color='black',main=title, asp=FALSE)
 info_template <- paste('| Layout:' ,lay, '| Size: degree.', '| Colored by Area of Science.')
 info_interval <- paste('Registers:' , n_registers, '| Authors:', n_authors,'| Authorships:', n_authorships, '| Fields:', n_fields, '| Years:', n_years)
 
 plot.igraph(g, sub=paste(info_interval, info_template), vertex.label.cex=0.7, vertex.label.font=0, vertex.label.family='Helvetica', vertex.label.color='black',main=title, asp=FALSE)
 #dev.off()
 dev_file_name <- file.path(images_dir,paste(title,'.pdf', sep='_'))
 #dev.print(pdf, file=dev_file_name, widht=6, height=3 );
 pdf(dev_file_name, width=16, height=12, family='Helvetica', pointsize=8)
   
   
  plot.igraph(g, sub=paste(info_interval, info_template), vertex.label.cex=0.7, vertex.label.font=0, vertex.label.family='Helvetica',  vertex.label.color='black',main=title, asp=FALSE)
 dev.off()
  #dev.copy(pdf,filename=dev_file_name, family='Helvetica');
  #dev.off ();
 
}

#used to plot benchmark Maps of Science
plot_graph_base <- function(g,layout='fr',title=paste('Benchmark Map -',taxo), size=bench_size, lab=FALSE, cex=1)
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
  
  V(g)$Label = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
  #higher aggregation
  V(g)$Field = as.character(nodes$subd_name[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
  V(g)$Area = as.character(nodes$Discipline[match(V(g)$name, nodes$Id)]) #watch out with the name of the column!!
  #adding LABELS IF REQUIRED
  V(g)$label<-''
  if(lab==TRUE)
  {
    V(g)$label = V(g)$Field
  }
  # V(gms)$size = (degree(gms)/max(degree(gms))) * 6
  if(size=='degree')
  {
    V(g)$size = (degree(g)/max(degree(g))) * 4  
  }
  
  V(g)$color = as.character(nodes$color[match(V(g)$name, nodes$Id)])  
  #plot.igraph(g, vertex.size=5, vertex.label.cex=0.5, asp=FALSE,main=title)
  # plot.igraph(g,  vertex.label.cex=0.7, edge.curved=TRUE,  vertex.label.font=0, vertex.label.family='Arial', vertex.label.color='black', asp=FALSE,main=title)
  #plot.igraph(g, sub=paste('Layout:' ,subt, '. Size: degree.', 'Colored by Area of Science.'), vertex.label.cex=0.7, vertex.label.font=0, vertex.label.family='Arial', vertex.label.color='black',main=title, asp=FALSE)
  info_credit <- bench_credit
  info_template <- paste('Layout WE applied:' , lay, '| Size:', size, '| Colored by:', bench_color)
  info_interval <- paste('Data time: ', bench_interval, ' | Data Source: ', bench_source, ' | Unit of analysis: ', bench_unit, '| Technnique: ', bench_tech)
  
  plot.igraph(g, 
  	sub=list(paste(info_credit,  info_interval, info_template, sep='\n'), cex=0.8*cex), 
  	vertex.label.cex=0.6*cex, 
  	vertex.label.font=0, 
  	vertex.label.family='Helvetica', 
  	vertex.label.color='black',
  	main=list(title,cex=1*0.8), 
  	asp=FALSE)
  #dev.off()
  dev_file_name <- file.path(path_benchmark,paste(title,'.pdf', sep=''))
  #dev.print(pdf, file=dev_file_name, widht=6, height=3 );
  pdf(dev_file_name, width=16, height=12, family='Helvetica', pointsize=8)
  
  
  plot.igraph(g, 
  	sub=list(paste(info_credit, info_interval,  info_template,  sep='\n'), cex=0.8*cex), 
  	vertex.label.cex=0.7, vertex.label.font=0, vertex.label.family='Helvetica',  
  	vertex.label.color='black',
  	main=list(title,cex=1*cex),
  	asp=FALSE)
  dev.off()
  #dev.copy(pdf,filename=dev_file_name, family='Helvetica');
  #dev.off ();
  
}





export_gephi <- function(phi)
{
  Source<-phi[,1]
  Target<-phi[,2]
  Weight <- phi[,3]
  Label <- phi[,3]
  #Type <- character()
  #MST <- character(0) 
  #Filter <- character()
  #filter <- 0.56621325 # average 4 for scimago 2012
  filter <- 0.2780202 # average 4 for gscholar 2000-2013 
  matr_list <- data.frame(Source,Target,Label,Weight)
  f_name <- paste(file_name,'_EDGES.csv',sep="")
  write.csv(matr_list, file.path(gephi_dir,f_name), row.names=FALSE)
  
  return(matr_list)
}

export_igraph <- function(phi)
{
  library("igraph")
}

#' @title Get a benchmark map
#' @description this function returns an igraph object based on links or in .Rdata created previously
#' @param data_raw a dataframe with production data use function load_raw_data()
#' @param init an integer pointing the initial year
#' @param end an integer pointing the final year
#' @param producer name of the column in data_raw that contains the name or id of producers. It should be factor.
#' @param category name of the column in data_raw that contains the name or id of categories. It should be factor.
#' @param value name of the column in data_raw that contains the feature to aggregate. should be numeric
#' @param agg function to aggregate, usually sum or mean. If NaN, no aggregation is computed
#' 
#' @examples 
#' load_data_interval()
#' @return a Data Frame to overlay.
#' @export
get_benchmark_map<- function(layout='fr', rs=NULL, pl=FALSE, lab=FALSE, pl_mst=FALSE, mean_degree=4, cex=1, mst=FALSE, pl_seed=pl_seed)
{
	
	
		#bench_map <- graph.data.frame(links, directed = TRUE)
		

		#print(list.edge.attributes(bench_map))
		
	if(is.null(rs))
	{
			#create benchmark maps
		gms <- bench_map
		E(gms)$weight <- E(gms)$weight #just to avoid problems in density full calculation
		
		if(pl==TRUE)
		{
			par(mfrow=c(1,1))
			plot_graph_base(gms,size='degree', layout=layout, lab=lab)  
		}
	}
	else
	{
		path_rs <- file.path(path_rs, paste(rs,".RData",sep=""))
		gms <- plot_rs(path_rs=path_rs, mean_degree=mean_degree, pl_mst=pl_mst, mst=mst, pl_seed=pl_seed, prop_to_lab=0.3, cex=cex, pl=pl)
		gms <- remove.edge.attribute(gms, 'weight_1')
		gms <- remove.edge.attribute(gms, 'weight_2')
		#E(gms)$weight_bench[E(bench_map)] <- E(bench_map)$weight[E(bench_map)] 
		#print(list.edge.attributes(gms))
		#gms <- graph.union(gms, bench_map) #WHY??!!!
		E(gms)$weight <- E(gms)$weight_1
		E(gms)$weight_bench <- E(gms)$weight_2 #just to track which are the links used by bench map
		gms <- delete.edges(gms, E(gms)[is.na(E(gms)$weight)])
		
		gms <- remove.edge.attribute(gms, 'weight_1')
		gms <- remove.edge.attribute(gms, 'weight_2')
	}
	
	
		print(list.edge.attributes(gms))
	
	return(gms)
	
}
