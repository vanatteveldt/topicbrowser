
prepare.time.var <- function(time_var, date_interval){
  if(class(time_var) == 'Date'){
    if(date_interval == 'day') time_var = as.Date(format(time_var, '%Y-%m-%d'))
    if(date_interval == 'month') time_var = as.Date(paste(format(time_var, '%Y-%m'),'-01',sep=''))
    if(date_interval == 'week') time_var = as.Date(paste(format(time_var, '%Y-%W'),1), '%Y-%W %u')
    if(date_interval == 'year') time_var = as.Date(paste(format(time_var, '%Y'),'-01-01',sep=''))
  } 
  time_var
}

fill.time.gaps <- function(d, date_interval){
  if(class(d$time) == 'numeric'){
    for(t in min(d$time):max(d$time)) 
      if(!t %in% d$time) d = rbind(d, data.frame(time=t, value=0))
  }
  if(class(d$time) == 'Date'){
    date_sequence = seq.Date(from=min(d$time), to=max(d$time), by=date_interval)
    for(i in 1:length(date_sequence)){
      t = date_sequence[i]
      if(!t %in% d$time) d = rbind(d, data.frame(time=t, value=0))
    }
  }
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
  plot(d$time, d$value, type='l', xlab='', main='', ylab='', xlim=c(min(d$time), max(d$time)), ylim=c(0, max(d$value)), bty='L', lwd=5, col='darkgrey', yaxt='n')
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
  wordcloud(names, freqs, scale=c(3,.5), min.freq=1, max.words=50, random.order=FALSE, rot.per=.15, colors=pal)
}

plot.topicoverview <- function(topic_term_matrix, topic_document_matrix, date_var, topic_nr, date_interval='year', value='relative', wordsize_scale=0.5){
  par(mar = c(4.5, 3, 2, 1), cex.axis = 1.7)
  layout(matrix(c(1, 1, 2, 2), 2, 2, byrow = TRUE), widths = c(2.5, 1.5), heights = c(1, 2))  
  plot.time(topic_document_matrix, topic_nr = topic_nr, time_var = date_var, date_interval = date_interval, value = value)
  title(main=paste("Topic", topic_nr))
  plot.wordcloud(topic_term_matrix, topic_nr = topic_nr, wordsize_scale = wordsize_scale)
  par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))
}


prepare.topics.plot.values <- function(document_sums, break_var, topic_nr, pct=F, value='total', filter=NULL){
  hits = document_sums[topic_nr,]
  d = aggregate(hits, by=list(break_var=break_var), FUN='sum') 
  if(value == 'relative'){
    total_hits = colSums(document_sums)  
    totals = aggregate(total_hits, by=list(break_var=break_var), FUN='sum')
    d$x = d$x / totals$x
  }
  if(pct == T) d$x = d$x / sum(d$x)
  d
}

#' Plots topic values per category
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
  par(p)
}