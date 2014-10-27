library(RColorBrewer)
library(wordcloud)
library(Matrix)
library(reshape2)

source("plots.r")

### general functions

getWordAssignments <- function(m, article_ids=m@documents){
  docfilter = m@wordassignments$i %in% which(m@documents %in% article_ids)
  data.frame(article_id = m@documents[m@wordassignments$i[docfilter]], 
             term = m@terms[m@wordassignments$j[docfilter]], 
             topic = m@wordassignments$v[docfilter])
}

createTopicBrowser <- function(tokens_topics, wordassignments, meta, folder_name='topicbrowser',
                               topic_ids=sort(unique(wordassignments$topic)),
                               date_interval='year') {
  # prepare variables
  topics_per_doc = acast(wordassignments, topic ~ article_id, value.var='term', fun.aggregate=length) 
  topics_per_term = acast(wordassignments, topic ~ term, value.var='article_id', fun.aggregate=length)
  
  meta = meta[match(colnames(topics_per_doc), meta$id),]
  #topicOverviewHtml(topics_per_term, topics_per_doc, meta, folder_name, topic_ids=topic_ids, date_interval=date_interval)
  if (!file.exists(folder_name)) dir.create(folder_name, recursive=T)
  
  fn = file.path(folder_name, "index.html")
  message("Rendering index page to ", fn)
  html = render_overview(topics_per_term, topics_per_doc, meta, topic_ids)
  cat(html, file=fn)
  for (topic in topic_ids) {
    fn = file.path(folder_name, topic_filename(topic))
    message("Rendering topic ", topic, " to ", fn)
    html = render_topic(topic, tokens_topics, meta, topics_per_doc, topics_per_term)
    cat(html, file=fn)
  }
}

topic_filename <- function(topic) paste("t", topic, ".html", sep="")

##################################################
########## TOPIC OVERVIEW FUNCTIONS ##############
##################################################
render_overview <- function(topics_per_term, topics_per_doc, meta, topic_ids, date_interval='year') {
  TEMPLATE="topic_overview_template.Rmd"
  knit2html(text=readLines(TEMPLATE, warn=F), stylesheet="style.css")
}

##################################################
######### ARTICLES FUNCTIONS ############
##################################################

#' Render a single topic
render_topic <- function(topic_id, tokens_topics, meta, topics_per_doc, topics_per_term, nmaxdoc=10, date_interval='year') {
  topicass = topics_per_doc[topic_id,]
  docs = names(head(topicass[order(-topicass)], n=nmaxdoc))
  
  render = function(aid) {
    tt = tokens_topics[tokens_topics$aid == aid, ]
    render_article(tt$word, tt$topic, meta[meta$id == aid,])
  }
  
  top_articles = lapply(docs, render)
  
  TEMPLATE="articles_template.Rmd"
  knit2html(text=readLines(TEMPLATE, warn=F), stylesheet="style.css")
}

#' Render a single article
#' 
#' @param terms: a vector of words
#' @param topics: a vector containing the topic of each word
#' @param meta: a 1-row data frame containing meta information
#' @param fragment.only: passed to knit2html (if T, do not output html header etc)
#' @return a raw html character vector
render_article <- function(terms, topics, meta, fragment.only=T) {
  TEMPLATE = "article.Rmd"
  tokens = tagTokens(terms, topics)
  out = knit2html(text=readLines(TEMPLATE, warn=F), fragment.only=fragment.only)
  out
}

#' Add title, href, and class to tokens
#' 
#' @param tokens: a vector of words
#' @param topics: a vector of topics, can include NA's for words without a topic
#' @return: a vector of raw html per token
tagTokens <- function(tokens, topics){
  tokens = as.character(tokens)
  tokens = gsub("`", "'", tokens)

  # add hrefs
  tokens = ifelse(is.na(topics), tokens, paste("<a href='t", topics, ".html'>", tokens, "</a>", sep=""))
  # add span class and title
  tokens = paste("<span",
                 ifelse(is.na(topics), 
                        " class='notopic'",
                        paste(" class='t", topics, "' title='", topics, "'", sep="")),
                 ">", tokens, "</span>", sep="")
  tokens
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

