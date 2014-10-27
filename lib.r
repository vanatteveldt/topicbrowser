library(RColorBrewer)
library(wordcloud)
library(Matrix)
library(reshape2)

### general functions

knit2html_to_folder <- function(envir, template, folder_name, file_name, css=NULL){
  ## to output knit html to another folder, the workingdir needs to be changed: http://www.rforge.net/doc/packages/knitr/knit.html
  wd = getwd()
  if(!file.exists(folder_name)) dir.create(file.path(wd, folder_name))
  setwd(file.path(wd, folder_name))
  tryCatch(knit2html(file.path(wd,template), output=file_name, envir=envir, quiet=T, stylesheet=css, title=file_name), finally=setwd(wd))
}

getWordAssignments <- function(m, article_ids=m@documents){
  docfilter = m@wordassignments$i %in% which(m@documents %in% article_ids)
  data.frame(article_id = m@documents[m@wordassignments$i[docfilter]], 
             term = m@terms[m@wordassignments$j[docfilter]], 
             topic = m@wordassignments$v[docfilter])
}

createTopicBrowser <- function(tokens_topics, wordassignments, meta, folder_name='topicbrowser', ntopdocuments=100, topic_ids=1:nrow(matrices$topic_document_matri), date_interval='year'){
  matrices = splitWordAssignments(wordassignments)
  topicOverviewHtml(matrices$topic_term_matrix, matrices$topic_document_matrix, meta, folder_name, topic_ids, date_interval)
  articleHighlightHtml(tokens_topics, matrices$topic_term_matrix, matrices$topic_document_matrix, meta, folder_name, topic_ids, ntopdocuments, date_interval)
}

##################################################
########## TOPIC OVERVIEW FUNCTIONS ##############
##################################################
topicOverviewHtml <- function(topic_term_matrix, topic_document_matrix, meta, folder_name='topicbrowser', topic_ids=1:m@k, date_interval='year'){
  meta = meta[match(colnames(topic_document_matrix), meta$id),]
  
  e = new.env()
  e$topic_ids = topic_ids
  e$date = meta$date
  e$date_interval = date_interval
  e$topic_term_matrix = topic_term_matrix
  e$topic_document_matrix = topic_document_matrix
    
  knit2html_to_folder(e, 'topic_overview_template.Rmd', folder_name, 'topicoverview.html')
}

#' Split assignments of a topic model into the number of times a) topics are assigned to each document and b) words are assigned to each topic
#'
#' Extracts two matrices from the (word)assignments of a topic model
#' a) a matrix giving the number of words per document (columns) that are assigned to a topic (rows).
#' b) a matrix giving the number of times a term (columns) is assigned to a topic (rows)
#' LDA (in the topicmodels package) assigns a topic to each unique word in a document. If you also want to take into account how often this word occured, the document term matrix (as used in the input for topmod.lda.fit) must be included in the weight.by.dtm argument.
#' 
#' @param assignments A data.frame with the columns: article_id, term & topic. 
#' @param weight.by.dtm If you want to weight the topic assignment of a word to the number of times the word occured, give the document term matrix for this argument
#' @return A list with two matrices
#' @export
splitWordAssignments <- function(assignments, weight.by.dtm=NULL){
  if(!is.null(weight.by.dtm)){
    dtm = weight.by.dtm[m@documents,m@terms]
    dtm = data.frame(article_id=dtm$i, term=dtm$j, count=dtm$v)
    assignments = merge(assignments, dtm, by=c('article_id','term'), all.x=T)
    topicdocmatrix = acast(assignments, topic ~ article_id, value.var='count', fun.aggregate=sum)
    topictermmatrix = acast(assignments, topic ~ term, value.var='count', fun.aggregate=sum)
  } else {
    topicdocmatrix = acast(assignments, topic ~ article_id, value.var='term', fun.aggregate=length) 
    topictermmatrix = acast(assignments, topic ~ term, value.var='article_id', fun.aggregate=length)
  }
  list(topic_document_matrix=topicdocmatrix, topic_term_matrix=topictermmatrix)
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
  plot.wordcloud(topic_term_matrix, topic_nr = topic_nr, wordsize_scale = wordsize_scale)
  par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))
}

##################################################
######### ARTICLES FUNCTIONS ############
##################################################

articleHighlightHtml <- function(tokens_topics, topic_term_matrix, topic_document_matrix, meta, folder_name='topicbrowser', topic_ids=1:m@k, ntopdocuments=100, date_interval='year'){
  e = new.env()
  e$meta = meta
  css = htmlStyle(tokens_topics$topic)
  
  
  for(topic_id in topic_ids){
    topicass = topic_document_matrix[topic_id,]
    topdocuments = as.character(na.omit(names(topicass[order(topicass, decreasing=T)[1:ntopdocuments]])))
    e$tokens_topics = tokens_topics[tokens_topics$aid %in% topdocuments,]
    e$unique_ordered_aids = topdocuments
    knit2html_to_folder(e, 'articles_template.Rmd', folder_name, file_name=paste('t',topic_id,sep=''), css)
  }
}

tagTokens <- function(token, topic){
  token = addHtmlTags(token, topic)
  addEmptySpaces(token)
}

buildArticle <- function(token, token_index, topic, article_id=NULL, headline=NULL, medium=NULL, date=NULL, maxwords=NULL){
  header = paste('<h1>', headline, '<i> (',article_id, ')</i></h1>', '<h3><i> ', date, ' - ', medium, '</i></h3>', sep='')
  if(!is.null(maxwords)) art = na.omit(token[order(token_index)][1:maxwords]) else art = token[order(token_index)]
  art = paste(art, collapse='')
  if(!is.null(maxwords)) art = paste(art, '[...]')
  paste(header, art)
}

addHtmlTags <- function(token, topic){
  token = as.character(token)
  token = gsub("`", "'", token)
  notna = which(!is.na(topic))
  top = topic[!is.na(topic)]
  token[notna] = paste("<a href='t", top, ".html'>",
                       "<span ", "title='", top, "'>", 
                       '<t',top,'>', token[notna], '</t',top,'>',  
                       "</span></a>", sep="")
  token
}

addEmptySpaces <- function(tagged_word){
  nospace = ifelse(substr(tagged_word, 1, 1) %in% c(',','.','"', "'", ':', ';', '?', '!', ')', '-', '`'), 1, 0)
  tagged_word[!nospace] = paste(' ', tagged_word[!nospace], sep='') 
  tagged_word
}

htmlStyle <- function(topics){
  utopics = unique(topics[!is.na(topics)])
  colo = substr(rainbow(length(utopics)), 1,7)
  css = htmlLinkStyle()
  for(i in 1:length(utopics)) {
    tcolor = paste('t', utopics[i], ' {background-color:',colo[i], "}\n", sep='')
    css = paste(css, tcolor, sep='')
  }
  css
}

htmlLinkStyle <- function(){
  "a{color:#000000; text-decoration:none}\n"
}

