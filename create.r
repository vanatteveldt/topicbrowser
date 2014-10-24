library(knitr)
library(tm)
library(topicmodels)
library(Matrix)

source('topicmodel_lib.r')

load('beer.rdata') # engelse liedjes in de nederlandse top 40 met het woord "beer".
dtm = dtm.create(tokens$aid, terms=tokens$lemma)

tstats = term.statistics(dtm)
tstats = tstats[tstats$termfreq > 5 & tstats$reldocfreq < 0.25 & tstats$nonalpha==F,]

dtm = dtm[,colnames(dtm) %in% tstats$term]
m = LDA(dtm, k=3, method='Gibbs')

docsums = documentsums(m)
meta = meta[match(m@documents, meta$id),]

### topic overview html
e = new.env()
e$topic_ids = 1:3
e$m = m
e$date = meta$date
e$docsums = docsums

knit2html("topic_overview_template.Rmd", envir=e, )


### articles html
wa = getWordAssignments(m=m)
tokens2 = merge(tokens, wa, by.x=c('aid','lemma'), by.y=c('article_id','term'), all.x=T)
head(tokens2)

e = new.env()
e$tokens = data.frame(token=tokens2$word, token_index=tokens2$id, article_id=tokens2$aid, topic=tokens2$topic)
e$meta = meta
e$unique_ordered_aids = unique(tokens$aid)
css = '.hip {color:red}'

knit2html("articles_template.Rmd", envir=e, stylesheet=css)

#Je topic was: `r topic`!
