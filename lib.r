library(RColorBrewer)
library(wordcloud)
library(Matrix)
library(reshape2)

source("plots.r")

#' Create a topic browser
#' 
#' This will create a set of linked html pages to browse the given topic model
#' 
#' @param m: the fitted LDA model object from the topicmodels package
#' @param terms: a vector of terms, which should be sorted in their original order and match m@terms
#' @param documents: a vector of the same length as terms, indicating the document of each term, matching m@documents
#' @param meta: a data frame with meta data about the documents, should have columns aid, headline, medium, and date (todo: make more flexible)
#' @param folder_name: the name of the folder to save the browser to
#' @param topic_ids: optionally restrict output to a selection of topics
#' @param date_interval: specify the interval for plotting the meta$data
#' @param words: if given, use instead of terms for displaying document
#' @return: (invisible) the file url of the index page
createTopicBrowser <- function(m, terms, documents, meta, folder_name=NULL, topic_ids=1:m@k, date_interval='year', words=terms) {
  wordassignments = data.frame(aid = m@documents[m@wordassignments$i], 
                               term = m@terms[m@wordassignments$j], 
                               topic = m@wordassignments$v)
  tokens_topics = data.frame(id=1:length(terms), term=terms, aid=documents, word=words)
  tokens_topics = merge(tokens_topics, wordassignments, all.x=T) # match by the names for the article_id and term columns. Be sure to use all.x=T, or you'll drop all the unassigned terms
  tokens_topics = tokens_topics[order(tokens_topics$aid, tokens_topics$id), ]

  # create topic browser
  if (is.null(folder_name)) folder_name=tempdir()
  opts_knit$set(base.dir = tempdir())
  message("Saving pictures to ", opts_knit$get("base.dir"))
  
  topics_per_doc = acast(wordassignments, topic ~ aid, value.var='term', fun.aggregate=length) 
  topics_per_term = acast(wordassignments, topic ~ term, value.var='aid', fun.aggregate=length)
  
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
  invisible(file.path("file:/", folder_name, "index.html"))
}

topic_filename <- function(topic) paste("t", topic, ".html", sep="")

getWordAssignments <- function(m, article_ids=m@documents){
  docfilter = m@wordassignments$i %in% which(m@documents %in% article_ids)
  data.frame(aid = m@documents[m@wordassignments$i[docfilter]], 
             term = m@terms[m@wordassignments$j[docfilter]], 
             topic = m@wordassignments$v[docfilter])
}

##################################################
########## TOPIC OVERVIEW FUNCTIONS ##############
##################################################
render_overview <- function(topics_per_term, topics_per_doc, meta, topic_ids, date_interval='year') {
  TEMPLATE="topic_overview_template.Rmd"
  css = get_css(topic_ids)
  knit2html(text=readLines(TEMPLATE, warn=F), stylesheet=css, quiet = T)
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
  css = get_css(rownames(topics_per_doc))

  TEMPLATE="articles_template.Rmd"
  knit2html(text=readLines(TEMPLATE, warn=F), stylesheet=css, quiet = T)
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
  out = knit2html(text=readLines(TEMPLATE, warn=F), fragment.only=fragment.only, quiet = T)
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



get_css <- function(topic_ids) {
  CSS_TEMPLATE="style.css"
  css = readLines(CSS_TEMPLATE, warn=F)
  colo = substr(rainbow(length(topic_ids)), 1,7)
  colorcss = paste(".t", topic_ids, " {background-color: ",colo, "}", sep="")
  c(css, colorcss)
}


