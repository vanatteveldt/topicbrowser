
tagTokens <- function(token, topic){
  token = addHtmlTags(token, topic)
  addEmptySpaces(token)
}

buildArticle <- function(token, token_index, topic, article_id=NULL, headline=NULL, date=NULL, maxwords=NULL){
  header = paste('<h3>', headline, '<i> (',article_id, ')</i></h3>', '<i> ', date, '</i>', sep='')
  if(!is.null(maxwords)) art = na.omit(token[order(token_index)][1:maxwords]) else art = token[order(token_index)]
  art = paste(art, collapse='')
  if(!is.null(maxwords)) art = paste(art, '[...]')
  paste(header, art)
}

addHtmlTags <- function(token, topic){
  token = as.character(token)
  token = gsub("`", "'", token)
  notna = which(!is.na(topic))
  token[notna] = paste("<a href='~/", topic[notna], "'>",
                        "<span title='", topic[notna], "'>", token[notna], 
                        "</span></a>", sep="")
  token
}

addEmptySpaces <- function(tagged_word){
  nospace = ifelse(substr(tagged_word, 1, 1) %in% c(',','.','"', "'", ':', ';', '?', '!', ')', '-', '`'), 1, 0)
  tagged_word[!nospace] = paste(' ', tagged_word[!nospace], sep='') 
  tagged_word
}

