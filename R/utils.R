#' @export
reduce_name_journal <- function(j_name)
{
  c_name <- tolower(j_name)
  
  #take only text before part
  if(grepl(' part ', c_name))
  {
    c_name <- strsplit(' Part ', c_name )[1]
  }
  
  #deleting ...
  #c_name <- sub('\\...', '', c_name)
  c_name <- gsub('\\(.+?\\)', '', c_name)
  c_name <- gsub('<..>', '', c_name) #<e2><80><a6>
  c_name <- gsub('\342\200\246', '', c_name) #...
  c_name <- gsub(',', '', c_name)
  c_name <- gsub('-', '', c_name)
  c_name <- gsub(':', '', c_name)
  c_name <- gsub('/', '', c_name)
  c_name <- gsub(';', '', c_name)
  c_name <- gsub("'", '', c_name)
  c_name <- gsub('"', '', c_name)
  c_name <- gsub("[[:punct:]]", '', c_name)
  c_name <- gsub("[[:space:]]", '', c_name)
  
  return(c_name)
}

#given a minimal journal name, returns an id in the database of scimago
find_id_journal <- function(journal_min)
{
  journals_scimago <<- load_journals_scimago()
  
  if(journal_min %in% rownames(journals_scimago))
  {
    return( journals_scimago[journal_min, 'id_journal'])  
  }
  else
  {
    return(NaN)
  }
  
}

#' @export
load_journals_scimago <- function()
{
  if(!exists("journals_scimago"))
  {
    journals_scimago <- read.csv("/Users/mguevara/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/TAXONOMIES/SCIMAGO/scimago_journals")
    
    journals_scimago['journal_min'] <- sapply(journals_scimago[, c('journal')], function(x) reduce_name_journal(x))
    
    #deleting duplicated jounals
    journals_duplicated_frequency <- table(journals_scimago$journal_min)
    journals_duplicated <- names(journals_duplicated_frequency[journals_duplicated_frequency>1])
    journals_scimago <- journals_scimago[!journals_scimago$journal_min %in% journals_duplicated,  ]
    journals_scimago[journals_scimago$journal_min %in% journals_duplicated, ]
    
    rownames(journals_scimago) <- journals_scimago$journal_min
  }
  return(journals_scimago)
  
}

#returns nodes and edges of some catetory, also a dataframe to track areas
#' @export
load_taxonomy <- function(taxo="scimago")
{
  if(taxo=="scimago")
  {
    nodes <- scimago_nodes    
  }
  if(taxo=="ucsd")
  {
    nodes <- ucsd_nodes
  }

  return(nodes)
}


#returns dataframen with id_journal and id_category according taxonomy
#algorithm to make assignation between categories and journals UNIQUE
#' @export
load_journals_category <- function(taxo="scimago")
{
  if(taxo=="scimago")
  {
    #journal_category <- read.csv("~/Dropbox/doctorado/MIT_PROJECT/TESIS_RESEARCH_SPACE/DATA/TAXONOMIES/SCIMAGO/Journals-Categories-Sep2014.csv", sep = "\t")
    journal_category <- read.csv("~/Dropbox/UPLA/INVESTIGACION/P\ ESTRELLAS/RESEARCH\ SPACE\ TO\ LIVE/DATA_OUTPUT/scimago_journal_category_indicator_2013", sep = ",")
  }
  journal_category$quartile <- as.numeric(substr(journal_category$quartile,2,2))
  #hardocing indicators for 2014-2017
  j_c_2013 <- subset(journal_category, id_year="2013")
  j_c_2014 <- j_c_2013; j_c_2014$id_year <- as.integer(2014)
  j_c_2015 <- j_c_2013; j_c_2015$id_year <- as.integer(2015)
  j_c_2016 <- j_c_2013; j_c_2016$id_year <- as.integer(2016)
  j_c_2017 <- j_c_2013; j_c_2017$id_year <- as.integer(2017)
  
  journal_category <- rbind(journal_category,j_c_2014, j_c_2015, j_c_2016, j_c_2017)
  
  #sorting descendent by category and descending by quartile
  journal_category <- journal_category[order(journal_category[,4], journal_category[,2]),]
  
  j_c_final=data.frame()
  
  for(year in unique(journal_category$id_year))
  {
    print(year)
    j_c_temp <- subset(journal_category, id_year==year)
    j_c_temp <- j_c_temp[ !duplicated(j_c_temp$id_journal),]
    j_c_final <- rbind(j_c_final, j_c_temp)
    
  }
  
  return(j_c_final)
}
