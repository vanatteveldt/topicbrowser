#' Create a topic browser
#' 
#' This will create a set of linked html pages to browse the given topic model
#' 
#' @param m: the fitted LDA model object from the topicmodels package
#' @param terms: a vector of terms, which should be sorted in their original order and match m@@terms
#' @param documents: a vector of the same length as terms, indicating the document of each term, matching m@@documents
#' @param meta: a data frame with meta data about the documents, should have columns aid, headline, medium, and date (todo: make more flexible)
#' @param topic_ids: optionally restrict output to a selection of topics
#' @param date_interval: specify the interval for plotting the meta$data
#' @param words: if given, use instead of terms for displaying document
#' @param output: an optional file name to write the browser html to
#' @return the html generated or (invisible) the filename
#' @export
createTopicBrowser <- function(m, terms, documents, meta, topic_ids=1:m@k, date_interval='year', words=terms, output=NULL, browse=interactive()) {
  if (is.null(output)) {
    output = tempfile("topicbrowser_", fileext = ".html")
    message("Writing html to ", output)
  }
  
  message("Preparing variables")
  # consolidate terms,documets,words in one data frame, filter on existing metadata, and add rank id
  keep = documents %in% meta$id
  tokens = data.frame(id=1:sum(keep), term=terms[keep], aid=documents[keep], word=words[keep])
  
  # build aid / term / topic triplet frame and topics x {doc, term} matrices
  wordassignments = data.frame(aid = m@documents[m@wordassignments$i], 
                               term = m@terms[m@wordassignments$j], 
                               topic = m@wordassignments$v)
  wordassignments = wordassignments[wordassignments$aid %in% meta$id, ]
  
  topics_per_doc = acast(wordassignments, topic ~ aid, value.var='term', fun.aggregate=length) 
  topics_per_term = acast(wordassignments, topic ~ term, value.var='aid', fun.aggregate=length)
  
  # order meta for plots
  meta = meta[match(colnames(topics_per_doc), meta$id),]
  
  sink(output)
  tryCatch(render_html(wordassignments, tokens, topics_per_term, topics_per_doc, meta, topic_ids, date_interval), 
           finally=sink())
  
  if (browse) browseURL(output)
  message("HTML written to ", output)
  invisible(output)
}

### HTML rendering

render_html <- function(wordassignments, tokens, topics_per_term, topics_per_doc, meta, topic_ids, date_interval) {
  TEMPLATE = system.file("template/template.html", package="topicbrowser", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  css = paste(get_css(topic_ids), collapse="\n")
  html = gsub("$CSS$", css, html, fixed = T)
  
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  #header
  cat(parts[1])
  
  # tabs
  cat('<ul class="nav nav-tabs" role="tablist" id="topictab">\n')
  cat('<li class="active"><a href="#home" role="tab" data-toggle="tab">Overview</a></li>\n')
  for (topic_id in topic_ids) 
    cat('<li><a href="#t', topic_id, '" role="tab" data-toggle="tab">Topic ', topic_id, '</a></li>\n', sep = "")
  cat('</ul>\n')
  
  # content
  cat('<div class="tab-content">\n')
  cat('<div class="tab-pane fade in active" id="home">\n')
  message("Rendering overview")
  render_overview(topics_per_term, topics_per_doc, meta, topic_ids, date_interval)
  cat('</div>\n')
  
  for (topic_id in topic_ids) {
    message("Rendering topic ", topic_id)
    cat('<div class="tab-pane fade in" id="t',topic_id,'">\n', sep="")
    render_topic(wordassignments, topics_per_term, topics_per_doc, meta, topic_id, date_interval, tokens)
    cat('</div>\n')
  }
  
  cat('</div>\n')
  
  # footer
  cat(parts[2])
  
}

tab_content_html <- function(id, content) {
  paste('<div class="tab-pane fade" id="', id, '">', content, '</div>', sep="")
}

tab_html <- function(id, name) {
  paste('<li><a href="#', id, '" role="tab" data-toggle="tab">', name, '</a></li>', sep="")
}

#' Render the index page
#' @param topics_per_term: a matrix of terms by topics
#' @param topics_per_doc: a matrix of documents by topics
#' @param meta: a data frame with meta information about the documents
#' @param topic_ids: a vector of topic ids to render
#' @param date_interval: optional date interval to use for time graphs
#' @return a raw html string (character)
render_overview <- function(topics_per_term, topics_per_doc, meta, topic_ids, date_interval='year') {
  for(topic_id in topic_ids){
    cat('<a href="#" onclick="showTab(', topic_id, ');">', sep='')
    png  = plot_topic_to_file(300, topics_per_term, topics_per_doc, meta, topic_id, date_interval)
    cat(img(png))
    cat("</a>")
  }
}

#' plot a topic overview to a file and return the file name. See plot.topicoverview for arguments
plot_topic_to_file <- function(size=500,  topics_per_term, topics_per_doc, meta, topic_id, date_interval) {
  pattern = paste("topic", topic_id, "_", sep="")
  fn = tempfile(fileext = ".png", pattern=pattern)
  png(filename = fn, width = size, height = size)
  tryCatch(plot.topicoverview(topics_per_term, topics_per_doc, meta$date, topic_id, date_interval),
           finally=dev.off())
  fn
}


#' Render a single topic
#' @param topic_id: the topic id to render
#' @param tokens_topics: a data frame with tokens (id, aid, term, word)
#' @param meta: a data frame with meta information about the documents
#' @param topics_per_term: a matrix of terms by topics
#' @param topics_per_doc: a matrix of documents by topics
#' @param nmaxdoc: the number of top documents to show
#' @param date_interval: optional date interval to use for time graphs
#' @return a raw html string (character)
render_topic <- function(wordassignments, topics_per_term, topics_per_doc, meta, topic_id, date_interval, tokens, nmaxdoc=10) {
  png = plot_topic_to_file(500, topics_per_term, topics_per_doc, meta, topic_id, date_interval)
  cat(img(png))
  
  topicass = topics_per_doc[topic_id,]
  docs = names(head(topicass[order(-topicass)], n=nmaxdoc))
  
  for (doc in docs) render_article(doc, tokens, wordassignments, meta, 100)
}

#' Render a single article
#' 
#' @param terms: a vector of words
#' @param topics: a vector containing the topic of each word
#' @param meta: a 1-row data frame containing meta information
#' @param fragment.only: passed to knit2html (if T, do not output html header etc)
#' @return a raw html character vector
render_article <- function(aid, tokens, wordassignments, meta, maxwords=NULL) {
  # header
  cat('<h3>', aid, '</h3>')
  
  # meta
  cat('<table>')
  for (name in colnames(meta)) 
    cat('<tr><th>', name, '</th><td>', as.character(meta[meta$id == aid, name]), '</td></tr>')
  cat('</table>')
  
  # render words
  cap <- function(x, n) if (is.null(n)) x else head(x,n)
  tok = tokens[tokens$aid == aid, ]
  tok = tok[cap(order(tok$id), maxwords), ]
  wa = wordassignments[wordassignments$aid == aid, c("term", "topic")]
  topics = wa$topic[match(tok$term, wa$term)]
  cat(tagTokens(tok$word, topics))
}

### HTML functions

#' Get the filename for a topic
topic_filename <- function(topic) paste("t", topic, ".html", sep="")

#' Add title, href, and class to tokens
#' 
#' @param tokens: a vector of words
#' @param topics: a vector of topics, can include NA's for words without a topic
#' @return a vector of raw html per token
tagTokens <- function(tokens, topics){
  tokens = as.character(tokens)
  tokens = gsub("`", "'", tokens)

  # add hrefs
  tokens = ifelse(is.na(topics), tokens, paste("<a href='#'>", tokens, "</a>", sep=""))
  # add span class and title
  tokens = paste("<span",
                 ifelse(is.na(topics), 
                        " class='notopic'",
                        paste(" onclick='showTab(",topics,")' class='t", topics, "' title='", topics, "'", sep="")),
                 ">", tokens, "</span>", sep="")
  tokens
}



get_css <- function(topic_ids) {
  CSS_TEMPLATE = system.file("template/style.css", package="topicbrowser", mustWork=T)
  css = readLines(CSS_TEMPLATE, warn=F)
  colo = substr(rainbow(length(topic_ids)), 1,7)
  colorcss = paste(".t", topic_ids, " {background-color: ",colo, "}", sep="")
  c(css, colorcss)
}


