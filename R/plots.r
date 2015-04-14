#' Standardized plotting function that can be passed to the plotfunction arguments in the \code{\link{createTopicBrowser}} function.
#' 
#' One of the standardized plotting function used in the Topicbrowser package to manage how topics and additional information are visualized.
#' This specific function plots a wordcloud below a graph showing the number of words assigned to the topic per time period
#' 
#' @param info The output of the \code{\link{clusterinfo}} function
#' @param topic_nr the index number of a topic
#' @param date_interval if not NULL, a string indicating what interval to use for plotting the time/date graph. Can be 'year', 'month', 'week' and 'day'. If NULL, the date_interval will be chosen based on the number of days in the analysis.
#' @param date_label a string indicating what the date object is in the meta data.
#' @return nothing, only plots
#' @export
pf.wordcloud_time <- function(info, topic_nr, date_interval=NULL, date_label='date') {
  par(mar = c(4.5, 3, 2, 1), cex.axis = 1.7)
  layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), widths = c(2.5, 1.5), heights = c(1, 2))  
  pf.time(info, topic_nr, date_interval, date_label)
  par(mar=c(0,0,0,0))
  pf.wordcloud(info, topic_nr)
  par(mfrow = c(1, 1), mar=c(5,4,4,2) + 0.1) # reset to default
}

#' Standardized plotting function that can be passed to the plotfunction arguments in the \code{\link{createTopicBrowser}} function.
#' 
#' One of the standardized plotting function used in the Topicbrowser package to manage how topics and additional information are visualized.
#' This specific function plots a wordcloud
#' 
#' @param info The output of the \code{\link{clusterinfo}} function
#' @param topic_nr the index number of a topic
#' @return nothing, only plots
#' @export
pf.wordcloud <- function(info, topic_nr) {
  plot.wordcloud(info$topics_per_term, topic_nr)
}

#' Standardized plotting function that can be passed to the plotfunction arguments in the \code{\link{createTopicBrowser}} function.
#' 
#' One of the standardized plotting function used in the Topicbrowser package to manage how topics and additional information are visualized.
#' This specific function plots a graph showing the number of words assigned to the topic per time period
#' 
#' @param info The output of the \code{\link{clusterinfo}} function
#' @param topic_nr the index number of a topic
#' @param date_interval if not NULL, a string indicating what interval to use for plotting the time/date graph. Can be 'year', 'month', 'week' and 'day'. If NULL, the date_interval will be chosen based on the number of days in the analysis.
#' @param date_label a string indicating what the date object is in the meta data.
#' @return nothing, only plots
#' @export
pf.time <- function(info, topic_nr, date_interval=NULL, date_label='date') {
  date_var = info$meta[,date_label]
  if(is.null(date_interval) & class(date_vector) == 'Date') date_interval = selectDateInterval(date_var)
  plot.time(info$topics_per_doc, topic_nr, date_var, date_interval=date_interval) 
}

pf.topicdistribution <- function(info, topic_nr){
  topic_pct_per_document = info$topics_per_doc[topic_nr,] / colSums(info$topics_per_doc)
  par(mar=c(5,6,1,4)) # Set the most suitable margins
  hist(topic_pct_per_document, main='', xlab='% of document assignments', ylab='Number of documents')
  par(mar=c(5,4,4,2) + 0.1) # reset to default
}

## dependency based plotfunction defaults
#' Standardized plotting function that can be passed to the plotfunction arguments in the \code{\link{createTopicBrowser}} function.
#' 
#' One of the standardized plotting function used in the Topicbrowser package to manage how topics and additional information are visualized.
#' This specific function plots a wordcloud below a graph showing the number of words assigned to the topic per time period
#' 
#' @param info The output of the \code{\link{clusterinfo}} function
#' @param topic_nr the index number of a topic
#' @return nothing, only plots
#' @export
pf.semnet <- function(info, topic_nr, backbone_alpha=0.05, nwords=100) {
  require(semnet)
  dtm = createTopicDtm(info$topics_per_term, info$wordassignments, topic_nr, nwords)
  adj = wordAdjacency(dtm, measure='cosine')
  g = graph.data.frame(adj, directed=F)
  g = getBackboneNetwork(g, alpha=backbone_alpha)
  V(g)$cluster = edge.betweenness.community(g)$membership
  
  wordfreq = data.frame(term=colnames(dtm), freq=colSums(dtm))
  V(g)$size = wordfreq$freq[match(V(g)$name, wordfreq$term)]
  
  g = setNetworkAttributes(g, size_attribute='size', cluster_attribute='cluster')
  V(g)$label.cex = V(g)$label.cex * 1.5
  par(mar=c(0,0,0,0))
  plot(g)
  par(mar=c(5,4,4,2) + 0.1) # reset to default}
}

createTopicDtm <- function(topics_per_term, wordassignments, topic_nr, nwords){
  wordfreq = topics_per_term[topic_nr,]
  wordfreq = wordfreq[order(-wordfreq)][1:nwords]
  words = names(wordfreq)
  wa = wordassignments[wordassignments$topic == topic_nr,]
  wa = wa[wa$term %in% words,]
  
  docs = unique(wa$aid)
  terms = unique(wa$term)
  dtm = spMatrix(nrow=length(docs), ncol=length(terms), i = match(wa$aid, docs), j = match(wa$term, terms), rep(1, nrow(wa)))
  colnames(dtm) = terms
  dtm
}


### Basic plotting functions
selectDateInterval <- function(date_var){ 
  ndays = as.numeric(difftime(min(date_var), max(date_var), units='days'))
  date_interval = 'year'
  if(ndays < 20*30) date_inteval = 'month'
  if(ndays < 20*7) date_inteval = 'week'
  if(ndays < 20) date_inteval = 'day'
  #message('Date interval = ', date_interval)
  date_interval
}

prepare.time.var <- function(time_var, date_interval){
  if(class(time_var) == 'Date'){
    if(date_interval == 'day') time_var = as.Date(format(time_var, '%Y-%m-%d'))
    if(date_interval == 'month') time_var = as.Date(paste(format(time_var, '%Y-%m'),'-01',sep=''))
    if(date_interval == 'week') time_var = as.Date(paste(format(time_var, '%Y-%W'),1), '%Y-%W %u')
    if(date_interval == 'year') time_var = as.Date(paste(format(time_var, '%Y'),'-01-01',sep=''))
  } 
  time_var
}

#' Add empty values for pretty plotting
#' 
#' When plotting a timeline, gaps in date_intervals are ignored. For the attention for topics gaps should be considered as having value 0.   
#' 
#' @param d A data.frame with the columns 'time' (Date) and 'value' (numeric)  
#' @param date_interval The date_interval is required to know what the gaps are
#' @return A data.frame with the columns 'time' (Date) and 'value' (numeric)  
#' @export
fill.time.gaps <- function(d, date_interval){
  if(class(d$time) == 'numeric') time_sequence = min(d$time):max(d$time)
  if(class(d$time) == 'Date') time_sequence = seq.Date(from=min(d$time), to=max(d$time), by=date_interval)
  
  emptytime = time_sequence[!time_sequence %in% d$time]
  
  if(length(emptytime) > 0) d = rbind(d, data.frame(time=emptytime, value=0))
  d[order(d$time),]
}

prepare.plot.values <- function(document_topic_matrix, break_var, topic_nr, pct=F, value='total', filter=NULL){
  hits = document_topic_matrix[topic_nr,]
  d = aggregate(hits, by=list(break_var=break_var), FUN='sum') 
  if(value == 'relative'){
    total_hits = colSums(document_topic_matrix)  
    totals = aggregate(total_hits, by=list(break_var=break_var), FUN='sum')
    d$x = d$x / totals$x
  }
  if(pct == T) d$x = d$x / sum(d$x)
  d
}

plot.time <- function(document_topic_matrix, topic_nr, time_var, date_interval='day', pct=F, value='total', return.values=F){
  par(mar=c(3,3,3,1))
  time_var = prepare.time.var(time_var, date_interval)  
  d = prepare.plot.values(document_topic_matrix, break_var=time_var, topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('time','value')
  d = fill.time.gaps(d, date_interval)
  plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey')
  par(mar=c(3,3,3,3))
  if(return.values==T) d
}

plot.wordcloud <- function(topic_term_matrix, topic_nr, wordsize_scale=0.75, relative_to_term_total=F){
  x = topic_term_matrix[topic_nr,]
  if(relative_to_term_total==T) {
    x = x / colSums(topic_term_matrix)
    x = x / sum(x)
  }
  x = sort(x, decreasing=T)[1:50]
  x = x[!is.na(x)]
  names = sub("/.*", "", names(x))
  freqs = x^wordsize_scale
  pal <- brewer.pal(6,"YlGnBu")
  par(mar=c(0,0,0,0))
  wordcloud(names, freqs, scale=c(3,.5), min.freq=1, max.words=50, random.order=FALSE, rot.per=.15, colors=pal)
  par(mar=c(5,4,4,2) + 0.1) # reset to default
}

topics.plot.category <- function(document_sums, topic_nr, category_var, pct=T, value='relative'){
  p = par(mar=c(3,3,3,1))
  d = prepare.topics.plot.values(document_sums, break_var=as.character(category_var), topic_nr=topic_nr, pct=pct, value=value)
  colnames(d) = c('category','value')
  barplot(as.matrix(t(d[,c('value')])), main='', beside=TRUE,horiz=FALSE,
          density=NA,
          col='darkgrey',
          xlab='',
          ylab="",
          axes=T, names.arg=d$category, cex.names=1, cex.axis=0.7, adj=1, las=2)
  par(mar=c(5,4,4,2) + 0.1) # reset to default
}
