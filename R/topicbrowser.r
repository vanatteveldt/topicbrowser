#' Create a 'clusterinfo' object that can be used for the individual render methods
#' 
#' @param m: the fitted LDA model object from the topicmodels package
#' @param terms: a vector of terms, which should be sorted in their original order and match m@@terms
#' @param documents: a vector of the same length as terms, indicating the document of each term, matching m@@documents
#' @param words: if given, use instead of terms for displaying document. 
#' @param meta: Optional meta data. Vectors or a dataframe of which the elements/rows match m@@documents. 
#' @return a list with tokens, wordassignments, and other items
#' @export
clusterinfo <- function(m, terms=NULL, documents=NULL, words=terms, meta=NULL) {
  meta = as.data.frame(meta)
  tokens = if(length(terms) > 0) data.frame(id=length(terms), term=terms, aid=documents, word=words) else NULL
  
  # build aid / term / topic triplet frame and topics x {doc, term} matrices
  wordassignments = data.frame(aid = m@documents[m@wordassignments$i], 
                               term = m@terms[m@wordassignments$j], 
                               topic = m@wordassignments$v)
  topics_per_term = acast(wordassignments, topic ~ term, value.var='aid', fun.aggregate=length)
  topics_per_doc = acast(wordassignments, topic ~ aid, value.var='term', fun.aggregate=length) 
  topics_per_doc = topics_per_doc[,match(m@documents, colnames(topics_per_doc))]
  
  # order meta for plots  
  list(lda_model=m, tokens=tokens, wordassignments=wordassignments, topics_per_doc=topics_per_doc, topics_per_term=topics_per_term, meta=meta)
}


#' Get a dataframe of topics per docment, with an identifier column
#' 
#' @param lda_model An lda model fitted using topicmodels::LDA
#' @param clusterinfo The output of the \code{\link{clusterinfo}} function
#' @param posterior use the posterior distribution or the actual word assignment (default)
#' @export
topics_per_document <- function(lda_model=NULL, clusterinfo=NULL, use.posterior=F) 
{
  if (is.null(clusterinfo)) {
    if (is.null(lda_model)) stop("Either lda_model or clusterinfo needs to be provided")
    clusterinfo = clusterinfo(lda_model)
  }

  if (use.posterior) {
    tpd = posterior(clusterinfo$lda_model)$topics
  } else {
    tpd = t(clusterinfo$topics_per_doc)
  }
  
  ids = as.numeric(clusterinfo$lda_model@documents)
  cbind(id = ids, data.frame(tpd))
}



#' Create a topic browser
#' 
#' This will create a set of linked html pages to browse the given topic model
#' 
#' @param clusterinfo The output of the \code{\link{clusterinfo}} function
#' @param plotfunction.overview A standardized function (see the `plot.` functions). Determines how the topics in the general topic overview are visualized 
#' @param plotfunction.pertopic A standardized function (see the `plot.` functions). Determines what information is visualized in the pages per topic. Can be a list of multiple functions, in which case multiple plots are made. 
#' @param output an optional file name to write the browser html to
#' @param nmaxdoc an integer, indicating the maximum number of top articles to be printed for each topic. 
#' @param ... Parameters to be passed to the plotfunctions.
#' @return the html generated or (invisible) the filename
#' @export
createTopicBrowser <- function(clusterinfo, 
                               plotfunction.overview=plot_wordcloud, plotfunction.pertopic=c(plot_wordcloud, plot_topicdistribution),  
                               output=NULL, nmaxdoc=10, nmaxwords=100, browse=interactive(), topic_nrs=1:nrow(info$topics_per_doc), ...) {
  
  url = wrap_html(render_html(clusterinfo, topic_nrs, plotfunction.overview, plotfunction.pertopic, nmaxdoc, nmaxwords, ...),
                  clusterinfo=clusterinfo, output=output, topic_nrs=topic_nrs)
  if (browse) browseURL(url)
  invisible(url)
}

#' Extract a topic X document matrix from a topicmodels object
#' 
#' @param m a topic model in the format of the topicmodels package.
#' @return a topic X document matrix
#' @export
topicsPerDoc <- function(m) {
  wordassignments = data.frame(aid = m@documents[m@wordassignments$i], 
                               term = m@terms[m@wordassignments$j], 
                               topic = m@wordassignments$v)
  acast(wordassignments, topic ~ aid, value.var='term', fun.aggregate=length) 
}

#' Extract a topic X term matrix from a topicmodels object
#' 
#' @param m a topic model in the format of the topicmodels package.
#' @return a topic X term matrix
#' @export
topicsPerTerm <- function(m) {
  wordassignments = data.frame(aid = m@documents[m@wordassignments$i], 
                               term = m@terms[m@wordassignments$j], 
                               topic = m@wordassignments$v)
  acast(wordassignments, topic ~ term, value.var='aid', fun.aggregate=length)
}


### HTML rendering

#' Wrap a call that 'cats' html fragments and save it as a html file with header and footer
#' 
#' Since R evaluates arguments lazily, call with the actual wrapped call, i.e. wrap_html(render_html(...))
#' 
#' @param wrapped the wrapped call
#' @param output an optional filename to output to. If NULL, a tempfile will be used
#' @return the output filename
#' @export
wrap_html <- function(wrapped, clusterinfo, topic_nrs, output=NULL) {
  if (is.null(output)) {
    output = tempfile("topicbrowser_", fileext = ".html")
    message("Writing html to ", output)
  }
  sink(output)
  tryCatch({
    cat(html_header(topic_nrs))
    force(wrapped)
    cat(html_footer())
  }, finally=sink())
  output
}

html_header <- function(topic_nrs) {
  TEMPLATE = system.file("template/template.html", package="topicbrowser", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  css = paste(get_css(topic_nrs), collapse="\n")
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  footer = parts[1]
  gsub("$CSS$", css, footer, fixed = T)
}

html_footer <- function() {
  TEMPLATE = system.file("template/template.html", package="topicbrowser", mustWork=T)
  html = readChar(TEMPLATE, file.info(TEMPLATE)$size)
  parts = unlist(strsplit(html, "$CONTENT$", fixed = T))
  parts[2]
}

#' Get top documents for a topic
#' 
#' 
#' @param topic_nr: the topic nr to render
#' @param n the number of top documents
#' @export
getTopDocuments <- function(clusterinfo, topic_nrs, ndocs){
  topdocs = list()
  for(topic_nr in topic_nrs){
    topicass = clusterinfo$topics_per_doc[topic_nr,]
    topdocs[[paste('t',topic_nr)]] = names(head(topicass[order(-topicass)], n=ndocs))
  }
  topdocs
}

#' Render the tabbed html containing overview and individual tabs
#' 
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#' 
#' @param info the cluster_info object (list)
#' @export
render_html <- function(clusterinfo, topic_nrs, plotfunction.overview, plotfunction.pertopic, nmaxdoc, nmaxwords, ...) {
  # tabs
  cat('<ul class="nav nav-tabs" role="tablist" id="topictab">\n')
  cat('<li class="active"><a href="#home" role="tab" data-toggle="tab">Overview</a></li>\n')
  for (topic_nr in topic_nrs) 
    cat('<li><a href="#t', topic_nr, '" role="tab" data-toggle="tab">Topic ', topic_nr, '</a></li>\n', sep = "")
  cat('</ul>\n')
  
  # content
  cat('<div class="tab-content">\n')
  cat('<div class="tab-pane fade in active" id="home">\n')
  message("Rendering overview")
  render_overview(clusterinfo, topic_nrs, plotfunction.overview, ...)
  cat('</div>\n')
  
  topdocs = getTopDocuments(clusterinfo, topic_nrs, nmaxdoc)
  if(!is.null(clusterinfo$tokens)) clusterinfo$tokens = clusterinfo$tokens[clusterinfo$tokens$aid %in% unlist(topdocs),]
  for (topic_nr in topic_nrs) {
    message("Rendering topic ", topic_nr)
    cat('<div class="tab-pane fade in" id="t',topic_nr,'">\n', sep="")
    render_topic(clusterinfo, topic_nr, plotfunction.pertopic, topdocs[[paste('t',topic_nr)]], nmaxwords, ...)
    cat('</div>\n')
  }  
  cat('</div>\n')
}

tab_content_html <- function(id, content) {
  paste('<div class="tab-pane fade" id="', id, '">', content, '</div>', sep="")
}

tab_html <- function(id, name) {
  paste('<li><a href="#', id, '" role="tab" data-toggle="tab">', name, '</a></li>', sep="")
}

#' Render the index page
#' 
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#' 
#' @param info the cluster_info object (list)
#' @export
render_overview <- function(clusterinfo, topic_nrs, plotfunction, ...) {
  for(topic_nr in topic_nrs){
    cat('<a href="#" title="Topic ', topic_nr, '" onclick="showTab(', topic_nr, ');">', sep='')
    arguments = listFunctionArguments(plotfunction, clusterinfo=clusterinfo, topic_nr=topic_nr, ...)
    cat_plot(do.call(plotfunction, arguments), width=300)
    cat("</a>")
  }
}

#' Plot the plot in plotfun and cat as a base64 encoded <img>
cat_plot <- function(plotfun, ...)  {
  png  = plot_to_file(plotfun, ...)
  cat(img(png))  
}

#' Plot the plot in plotfun to a tempfile and return the filename
plot_to_file <- function(plotfun, width=500, height=width) {
  fn = tempfile(fileext = ".png", pattern="plot_")
  png(filename = fn, width = width, height = height)
  tryCatch(force(plotfun), finally=dev.off())
  fn
}

#' Render a single topic
#' 
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#' 
#' 
#' @param topic_id: the topic id to render
#' @param info the cluster_info object (list)
#' @export
render_topic <- function(clusterinfo, topic_nr, plotfunction, docs, nmaxwords, ...) {
  cat("<h1>Topic", topic_nr, "</h1>")
  
  for(pfunc in c(plotfunction)) {
    arguments = listFunctionArguments(pfunc, clusterinfo=clusterinfo, topic_nr=topic_nr, ...)
    cat_plot(do.call(pfunc, arguments), width=500, height=500)
  }
  
  cat("<h2>Top articles</h2>")
  for (doc in docs) render_article(clusterinfo, doc, maxwords=nmaxwords)
}

#' Render a single article
#' 
#' This function does not return the html, but rather cats it directly
#' So you might wish to use wrap_html or sink to capture the output
#' 
#' 
#' @param doc: the document id (dtm rowname) to render
#' @param info the cluster_info object (list)
#' @export
render_article <- function(clusterinfo, doc, maxwords=NULL) {
  # header
  cat('<h3>', doc, '</h3>')
  
  # meta
  cat('<table>')
  for (name in colnames(clusterinfo$meta)) 
    cat('<tr><th>', name, '   </th><td>', as.character(clusterinfo$meta[clusterinfo$meta$id == doc, name]), '</td></tr>')
  cat('</table>')
  
  
  # render words
  if(!is.null(clusterinfo$tokens)){
    cap <- function(x, n) if (is.null(n)) x else head(x,n)
    tok = clusterinfo$tokens[clusterinfo$tokens$aid == doc, ]
    tok = tok[cap(order(tok$id), maxwords), ]
    wa = clusterinfo$wordassignments[clusterinfo$wordassignments$aid == doc, c("term", "topic")]
    topics = wa$topic[match(tok$term, wa$term)]
    cat(tagTokens(tok$word, topics))
  }
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
  stopics = topics[!is.na(topics)]
  stokens = tokens[!is.na(topics)]
  tokens[!is.na(topics)] = paste("<span", " onclick='showTab(",stopics,")' class='t", stopics, "' title='", stopics, "'", ">", stokens, "</span>", sep="")
  tokens
}



get_css <- function(topic_nrs) {
  CSS_TEMPLATE = system.file("template/style.css", package="topicbrowser", mustWork=T)
  css = readLines(CSS_TEMPLATE, warn=F)
  colo = substr(rainbow(length(topic_nrs), s=0.6,alpha=0.5), 1,7)

  colorcss = paste(".t", topic_nrs, " {background-color: ",colo, "}", sep="")
  c(css, colorcss)
}

#' Match elipsis arguments to the permitted arguments of a function
#'  
#' @param func the function
#' @param ... any argument
#' @return a list with all and only given arguments (...) that can be used by the function
#' @export
listFunctionArguments <- function(func, ...){
  arguments = list(...)
  function_args = names(as.list(args(func)))
  if(!'...' %in% function_args) arguments = arguments[names(arguments) %in% function_args] 
  arguments
}