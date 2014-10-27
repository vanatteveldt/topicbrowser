library(tm)
library(slam)
library(topicmodels)

#' Cast data.frame to sparse matrix
#' 
#' Create a sparse matrix from matching vectors of row indices, column indices and values
#' 
#' @param rows a vector of row indices: [i,]
#' @param columns a vector of column indices: [,j]
#' @param values a vector of the values for each (non-zero) cell: [i,j] = value
#' @return a sparse matrix of the dgTMatrix class (\code{\link{Matrix}} package) 
#' @export
cast.sparse.matrix <- function(rows, columns, values=NULL) {
  if(is.null(values)) values = rep(1, length(rows))
  d = data.frame(rows=rows, columns=columns, values=values)
  if(nrow(d) > nrow(unique(d[,c('rows','columns')]))){
    message('(Duplicate row-column matches occured. Values of duplicates are added up)')
    d = aggregate(values ~ rows + columns, d, FUN='sum')
  }
  unit_index = unique(d$rows)
  char_index = unique(d$columns)
  sm = spMatrix(nrow=length(unit_index), ncol=length(char_index),
                match(d$rows, unit_index), match(d$columns, char_index), d$values)
  rownames(sm) = unit_index
  colnames(sm) = char_index
  sm
}

#' Create a document term matrix from a list of tokens
#' 
#' Create a \code{\link{DocumentTermMatrix}} from a list of document ids, terms, and frequencies. 
#' 
#' @param documents a vector of document names/ids
#' @param terms a vector of words of the same length as documents
#' @param freqs a vector of the frequency a a term in a document
#' @return a document-term matrix  \code{\link{DocumentTermMatrix}}
#' @export
dtm.create <- function(documents, terms, freqs=rep(1, length(documents))) {
  # remove NA terms
  d = data.frame(ids=documents, terms=terms, freqs=freqs)
  if (sum(is.na(d$terms)) > 0) {
    warning("Removing ", sum(is.na(d$terms)), "rows with missing term names")
    d = d[!is.na(d$terms), ]
  }
  sparsemat = cast.sparse.matrix(rows=d$ids, columns=d$terms, values=d$freqs)
  as.DocumentTermMatrix(sparsemat, weighting=weightTf)
}

#' Get word assignments from LDA_GIBBS class (output of topmod.lda.fit). This is similar to the documentsums object that comes as the output of lda.collapsed.gibbs.sampler
#' 
#' Get word assignments from LDA_GIBBS class (output of topmod.lda.fit). This is similar to the documentsums object that comes as the output of lda.collapsed.gibbs.sampler
#' LDA assigns a topic to each unique word in a document. If you also want to take into account how often this word occured, the document term matrix (as used in the input for topmod.lda.fit) must be included in the weight.by.dtm argument.
#' 
#' @param m The output from one of the topicmodeling functions in the topicmodels package (e.g., LDA_GIBBS)
#' @param weight.by.dtm If you want to weight the topic assignment of a word to the number of times the word occured, give the document term matrix for this argument
#' @return A matrix where rows are topics and columns are documents. Values represent the number of times the topic is assigned to a word in this document (essentially this is the same as the documentsums object in the output of lda.collapsed.gibbs.samler)
#' @export
documentsums <- function(m, weight.by.dtm=NULL){
  assignments = data.frame(i=m@wordassignments$i, j=m@wordassignments$j, v=m@wordassignments$v)
  if(!is.null(weight.by.dtm)){
    dtm = weight.by.dtm[m@documents,m@terms]
    dtm = data.frame(i=dtm$i, j=dtm$j, count=dtm$v)
    assignments = merge(assignments, dtm, by=c('i','j'), all.x=T)
    docsums = acast(assignments, v ~ i, value.var='count', fun.aggregate=sum)
  } else docsums = acast(assignments, v ~ i, value.var='j', fun.aggregate=length) 
  docsums
}

#' Compute some useful corpus statistics for a dtm
#' 
#' Compute a number of useful statistics for filtering words: term frequency, idf, etc.
#' 
#' @param dtm a document term matrix (e.g. the output of \code{\link{dtm.create}})
#' @return A data frame with rows corresponding to the terms in dtm and the statistics in the columns
#' @export
term.statistics <- function(dtm) {
  dtm = dtm[row_sums(dtm) > 0,col_sums(dtm) > 0]    # get rid of empty rows/columns
  vocabulary = colnames(dtm)
  data.frame(term = vocabulary,
             characters = nchar(vocabulary),
             number = grepl("[0-9]", vocabulary),
             nonalpha = grepl("\\W", vocabulary),
             termfreq = col_sums(dtm),
             docfreq = col_sums(dtm > 0),
             reldocfreq = col_sums(dtm > 0) / nDocs(dtm),
             tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0)))
}