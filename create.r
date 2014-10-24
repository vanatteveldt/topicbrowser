library(knitr)
?knitr

knit("test.Rmd")
e = new.env()
e$topic = "bla"
css = '.hip {color:red}'

knit2html("test.Rmd", envir=e, stylesheet=css)
knit2html()
e$wc <- function(words, freqs=rep(1, length(words))) {
  library(RColorBrewer)
  library(wordcloud)
  pal <- brewer.pal(6,"YlGnBu")
  wordcloud(words, freqs, scale=c(2,.1), min.freq=1, max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal)
}