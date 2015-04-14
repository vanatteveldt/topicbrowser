Creating a topicbrowser
========================================================

First, get Topicmodel data. To create a topicbrowser three elements are required:
1. tokens: A data.frame in which each row is a token (word or lemma) in an article. There should be a column for unique article ids. Rows should be ordered in the same order as the words occur in the articles. To create token data, see manuals for tokenization. It is advised to use natural language processing (NLP) techniques to get stemmed or lemmatized tokens, and to be able to filter on part-of-speech tags. This can be obtained using free software such as nlp.core (english), frog (Dutch) and parzu (German).
2. meta: A data.frame giving additional information per article. There should be an 'id' column that matches the article id column in the tokens data.frame. Label columns representing article date 'date' and use the `Date` format.
3. topicmodel: A topicmodel in the format of the `topicmodels` package. See the `corpustools` package for a manual on how to create a topicmodel based on a the data.frame with tokens.


```r
library(topicbrowser)
```

```
## Loading required package: RColorBrewer
## Loading required package: wordcloud
## Loading required package: Rcpp
## Loading required package: Matrix
## Loading required package: reshape2
## Loading required package: markdown
## Loading required package: base64
## 
## Attaching package: 'topicbrowser'
## 
## The following object is masked from 'package:knitr':
## 
##     render_html
```

```r
data(sotu)
head(sotu.tokens)
```

```
##         word sentence  pos      lemma offset       aid id pos1 freq
## 1         It        1  PRP         it      0 111541965  1    O    1
## 2         is        1  VBZ         be      3 111541965  2    V    1
## 3        our        1 PRP$         we      6 111541965  3    O    1
## 4 unfinished        1   JJ unfinished     10 111541965  4    A    1
## 5       task        1   NN       task     21 111541965  5    N    1
## 6         to        1   TO         to     26 111541965  6    ?    1
```

```r
head(sotu.meta)
```

```
##          id   medium     headline       date
## 1 111541965 Speeches Barack Obama 2013-02-12
## 2 111541995 Speeches Barack Obama 2013-02-12
## 3 111542001 Speeches Barack Obama 2013-02-12
## 4 111542006 Speeches Barack Obama 2013-02-12
## 5 111542013 Speeches Barack Obama 2013-02-12
## 6 111542018 Speeches Barack Obama 2013-02-12
```

```r
sotu.m
```

```
## Loading required package: topicmodels
```

```
## A LDA_Gibbs topic model with 10 topics.
```


The first step for creating the topicbrowser is to use the clusterinfo function. This creates a single list in which relevant information is combined. (This approach makes it easier to manage visualizations).


```r
info = clusterinfo(sotu.m, sotu.tokens$lemma, sotu.tokens$aid, meta = sotu.meta, 
    words = sotu.tokens$word, date_interval = "year")
names(info)
```

```
## [1] "tokens"          "wordassignments" "topics_per_doc"  "topics_per_term"
## [5] "topic_ids"       "meta"            "date_interval"
```


We can now already create a default topicbrowser using the createTopicBrowser function.   


```r
createTopicBrowser(info)
```

```
## Writing html to /tmp/Rtmp9lqX6m/topicbrowser_4ff6bb548b4.html
## Rendering overview
```

```
## Warning: chance could not be fit on page. It will not be plotted.
## Warning: success could not be fit on page. It will not be plotted.
## Warning: standard could not be fit on page. It will not be plotted.
## Warning: struggle could not be fit on page. It will not be plotted.
## Warning: training could not be fit on page. It will not be plotted.
## Warning: Chamber could not be fit on page. It will not be plotted.
## Warning: course could not be fit on page. It will not be plotted.
## Warning: demand could not be fit on page. It will not be plotted.
## Warning: possible could not be fit on page. It will not be plotted.
## Warning: prepare could not be fit on page. It will not be plotted.
## Warning: science could not be fit on page. It will not be plotted.
## Warning: learn could not be fit on page. It will not be plotted.
## Warning: middle could not be fit on page. It will not be plotted.
## Warning: many could not be fit on page. It will not be plotted.
## Warning: math could not be fit on page. It will not be plotted.
## Warning: coverage could not be fit on page. It will not be plotted.
## Warning: improve could not be fit on page. It will not be plotted.
## Warning: medical could not be fit on page. It will not be plotted.
## Warning: decision could not be fit on page. It will not be plotted.
## Warning: private could not be fit on page. It will not be plotted.
## Warning: savings could not be fit on page. It will not be plotted.
## Warning: funding could not be fit on page. It will not be plotted.
## Warning: affordable could not be fit on page. It will not be plotted.
## Warning: receive could not be fit on page. It will not be plotted.
## Warning: extend could not be fit on page. It will not be plotted.
## Warning: address could not be fit on page. It will not be plotted.
## Warning: raise could not be fit on page. It will not be plotted.
## Warning: dollar could not be fit on page. It will not be plotted.
## Warning: spend could not be fit on page. It will not be plotted.
## Warning: spending could not be fit on page. It will not be plotted.
## Warning: growth could not be fit on page. It will not be plotted.
## Warning: Washington could not be fit on page. It will not be plotted.
## Warning: agree could not be fit on page. It will not be plotted.
## Warning: earn could not be fit on page. It will not be plotted.
## Warning: Government could not be fit on page. It will not be plotted.
## Warning: single could not be fit on page. It will not be plotted.
## Warning: income could not be fit on page. It will not be plotted.
## Warning: Republicans could not be fit on page. It will not be plotted.
## Warning: Democrats could not be fit on page. It will not be plotted.
## Warning: relief could not be fit on page. It will not be plotted.
## Warning: priority could not be fit on page. It will not be plotted.
## Warning: week could not be fit on page. It will not be plotted.
## Warning: number could not be fit on page. It will not be plotted.
## Warning: lower could not be fit on page. It will not be plotted.
## Warning: one could not be fit on page. It will not be plotted.
## Warning: stay could not be fit on page. It will not be plotted.
## Warning: investment could not be fit on page. It will not be plotted.
## Warning: encourage could not be fit on page. It will not be plotted.
## Warning: greater could not be fit on page. It will not be plotted.
## Warning: retirement could not be fit on page. It will not be plotted.
## Warning: account could not be fit on page. It will not be plotted.
## Warning: innovation could not be fit on page. It will not be plotted.
## Warning: source could not be fit on page. It will not be plotted.
## Warning: legislation could not be fit on page. It will not be plotted.
## Warning: example could not be fit on page. It will not be plotted.
## Warning: launch could not be fit on page. It will not be plotted.
## Warning: information could not be fit on page. It will not be plotted.
## Warning: enemy could not be fit on page. It will not be plotted.
## Warning: military could not be fit on page. It will not be plotted.
## Warning: serve could not be fit on page. It will not be plotted.
## Warning: peace could not be fit on page. It will not be plotted.
## Warning: Afghanistan could not be fit on page. It will not be plotted.
## Warning: democracy could not be fit on page. It will not be plotted.
## Warning: attack could not be fit on page. It will not be plotted.
## Warning: Qaida could not be fit on page. It will not be plotted.
## Warning: remain could not be fit on page. It will not be plotted.
## Warning: friend could not be fit on page. It will not be plotted.
## Warning: defend could not be fit on page. It will not be plotted.
## Warning: mission could not be fit on page. It will not be plotted.
## Warning: liberty could not be fit on page. It will not be plotted.
## Warning: past could not be fit on page. It will not be plotted.
## Warning: stop could not be fit on page. It will not be plotted.
## Warning: coalition could not be fit on page. It will not be plotted.
## Warning: determine could not be fit on page. It will not be plotted.
## Warning: honor could not be fit on page. It will not be plotted.
## Warning: Middle could not be fit on page. It will not be plotted.
## Warning: service could not be fit on page. It will not be plotted.
## Warning: partner could not be fit on page. It will not be plotted.
## Warning: pursue could not be fit on page. It will not be plotted.
## Warning: region could not be fit on page. It will not be plotted.
## Warning: defeat could not be fit on page. It will not be plotted.
## Warning: God could not be fit on page. It will not be plotted.
## Warning: administration could not be fit on page. It will not be plotted.
## Warning: financial could not be fit on page. It will not be plotted.
## Warning: industry could not be fit on page. It will not be plotted.
## Warning: recession could not be fit on page. It will not be plotted.
## Warning: necessary could not be fit on page. It will not be plotted.
## Warning: housing could not be fit on page. It will not be plotted.
## Warning: factory could not be fit on page. It will not be plotted.
## Warning: Americans could not be fit on page. It will not be plotted.
## Warning: agreement could not be fit on page. It will not be plotted.
## Warning: compete could not be fit on page. It will not be plotted.
## Warning: entrepreneur could not be fit on page. It will not be plotted.
```

```
## Rendering topic 1
```

```
## Warning: struggle could not be fit on page. It will not be plotted.
## Warning: Chamber could not be fit on page. It will not be plotted.
## Warning: possible could not be fit on page. It will not be plotted.
## Warning: prepare could not be fit on page. It will not be plotted.
## Warning: science could not be fit on page. It will not be plotted.
## Warning: learn could not be fit on page. It will not be plotted.
## Warning: middle could not be fit on page. It will not be plotted.
## Warning: teacher could not be fit on page. It will not be plotted.
## Warning: many could not be fit on page. It will not be plotted.
## Warning: reward could not be fit on page. It will not be plotted.
## Warning: math could not be fit on page. It will not be plotted.
```

```
## Rendering topic 2
```

```
## Warning: insurance could not be fit on page. It will not be plotted.
## Warning: savings could not be fit on page. It will not be plotted.
## Warning: affordable could not be fit on page. It will not be plotted.
## Warning: doctor could not be fit on page. It will not be plotted.
## Warning: extend could not be fit on page. It will not be plotted.
## Warning: address could not be fit on page. It will not be plotted.
```

```
## Rendering topic 3
## Rendering topic 4
```

```
## Warning: Congress could not be fit on page. It will not be plotted.
## Warning: raise could not be fit on page. It will not be plotted.
## Warning: spend could not be fit on page. It will not be plotted.
## Warning: spending could not be fit on page. It will not be plotted.
## Warning: break could not be fit on page. It will not be plotted.
## Warning: growth could not be fit on page. It will not be plotted.
## Warning: mean could not be fit on page. It will not be plotted.
## Warning: Washington could not be fit on page. It will not be plotted.
## Warning: agree could not be fit on page. It will not be plotted.
## Warning: Government could not be fit on page. It will not be plotted.
## Warning: single could not be fit on page. It will not be plotted.
## Warning: Republicans could not be fit on page. It will not be plotted.
## Warning: Democrats could not be fit on page. It will not be plotted.
## Warning: relief could not be fit on page. It will not be plotted.
## Warning: priority could not be fit on page. It will not be plotted.
## Warning: week could not be fit on page. It will not be plotted.
## Warning: half could not be fit on page. It will not be plotted.
## Warning: number could not be fit on page. It will not be plotted.
## Warning: lower could not be fit on page. It will not be plotted.
## Warning: stay could not be fit on page. It will not be plotted.
```

```
## Rendering topic 5
```

```
## Warning: develop could not be fit on page. It will not be plotted.
## Warning: greater could not be fit on page. It will not be plotted.
## Warning: account could not be fit on page. It will not be plotted.
## Warning: innovation could not be fit on page. It will not be plotted.
## Warning: eliminate could not be fit on page. It will not be plotted.
## Warning: price could not be fit on page. It will not be plotted.
## Warning: foreign could not be fit on page. It will not be plotted.
## Warning: legislation could not be fit on page. It will not be plotted.
## Warning: example could not be fit on page. It will not be plotted.
## Warning: information could not be fit on page. It will not be plotted.
```

```
## Rendering topic 6
```

```
## Warning: freedom could not be fit on page. It will not be plotted.
## Warning: enemy could not be fit on page. It will not be plotted.
## Warning: military could not be fit on page. It will not be plotted.
## Warning: terror could not be fit on page. It will not be plotted.
## Warning: peace could not be fit on page. It will not be plotted.
## Warning: Afghanistan could not be fit on page. It will not be plotted.
## Warning: democracy could not be fit on page. It will not be plotted.
## Warning: attack could not be fit on page. It will not be plotted.
## Warning: troops could not be fit on page. It will not be plotted.
## Warning: remain could not be fit on page. It will not be plotted.
## Warning: friend could not be fit on page. It will not be plotted.
## Warning: danger could not be fit on page. It will not be plotted.
## Warning: mission could not be fit on page. It will not be plotted.
## Warning: safe could not be fit on page. It will not be plotted.
## Warning: East could not be fit on page. It will not be plotted.
## Warning: past could not be fit on page. It will not be plotted.
## Warning: stop could not be fit on page. It will not be plotted.
## Warning: coalition could not be fit on page. It will not be plotted.
## Warning: determine could not be fit on page. It will not be plotted.
## Warning: honor could not be fit on page. It will not be plotted.
## Warning: Middle could not be fit on page. It will not be plotted.
## Warning: service could not be fit on page. It will not be plotted.
## Warning: partner could not be fit on page. It will not be plotted.
## Warning: pursue could not be fit on page. It will not be plotted.
## Warning: region could not be fit on page. It will not be plotted.
## Warning: defeat could not be fit on page. It will not be plotted.
## Warning: God could not be fit on page. It will not be plotted.
```

```
## Rendering topic 7
```

```
## Warning: economy could not be fit on page. It will not be plotted.
## Warning: business could not be fit on page. It will not be plotted.
## Warning: administration could not be fit on page. It will not be plotted.
## Warning: necessary could not be fit on page. It will not be plotted.
## Warning: factory could not be fit on page. It will not be plotted.
## Warning: Americans could not be fit on page. It will not be plotted.
## Warning: agreement could not be fit on page. It will not be plotted.
## Warning: compete could not be fit on page. It will not be plotted.
## Warning: burden could not be fit on page. It will not be plotted.
## Warning: entrepreneur could not be fit on page. It will not be plotted.
```

```
## Rendering topic 8
## Rendering topic 9
## Rendering topic 10
```


Pretty cool. But you might want to use different visualizations instead of the default wordcloud and time graph. For this purpose, we can use the `plotfunction` parameters of the createTopicBrowser function. These parameters take as input special, standardized functions, that produce a plot based on the `info` object as created by the clusterinfo function. We developed several of such `plotfunction` functions (all starting with `pf.`) and it is easy to implement new ones yourself. 

Here are some of the `plotfunction` defaults included in this package. The input for these functions is the `info` object, and an integer representing which topic to plot.


```r
pf.wordcloud_time(info, topic_nr = 2)  # the default visualization used for plotfunction.overview: wordcloud and time graph
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
pf.wordcloud(info, topic_nr = 2)  # the default first visualization used for plotfunction.pertopic: wordcloud
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 

```r
pf.time(info, topic_nr = 2)  # the default second visualization used for plotfunction.pertopic: time graph
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-43.png) 


Note that the intervalls for the time graph are based on the date_interval parameter in the info object. This can either be changed when creating the object using the clusterinfo function, or afterwards. 

We mentioned that pf.wordcloud is the first, and pf.time is the second default visualization in the plotfunction.pertopic. This is because multiple plot functions can be used here. See the following silly but illustrative example


```r
createTopicBrowser(info, plotfunction.pertopic = list(pf.time, pf.wordcloud, 
    pf.time, pf.wordcloud))
```

```
## Writing html to /tmp/Rtmp9lqX6m/topicbrowser_4ff50d12e18.html
## Rendering overview
```

```
## Warning: struggle could not be fit on page. It will not be plotted.
## Warning: training could not be fit on page. It will not be plotted.
## Warning: demand could not be fit on page. It will not be plotted.
## Warning: possible could not be fit on page. It will not be plotted.
## Warning: prepare could not be fit on page. It will not be plotted.
## Warning: class could not be fit on page. It will not be plotted.
## Warning: learn could not be fit on page. It will not be plotted.
## Warning: teacher could not be fit on page. It will not be plotted.
## Warning: many could not be fit on page. It will not be plotted.
## Warning: reward could not be fit on page. It will not be plotted.
## Warning: race could not be fit on page. It will not be plotted.
## Warning: math could not be fit on page. It will not be plotted.
## Warning: coverage could not be fit on page. It will not be plotted.
## Warning: improve could not be fit on page. It will not be plotted.
## Warning: control could not be fit on page. It will not be plotted.
## Warning: decision could not be fit on page. It will not be plotted.
## Warning: savings could not be fit on page. It will not be plotted.
## Warning: funding could not be fit on page. It will not be plotted.
## Warning: return could not be fit on page. It will not be plotted.
## Warning: affordable could not be fit on page. It will not be plotted.
## Warning: address could not be fit on page. It will not be plotted.
## Warning: spend could not be fit on page. It will not be plotted.
## Warning: spending could not be fit on page. It will not be plotted.
## Warning: break could not be fit on page. It will not be plotted.
## Warning: growth could not be fit on page. It will not be plotted.
## Warning: Washington could not be fit on page. It will not be plotted.
## Warning: agree could not be fit on page. It will not be plotted.
## Warning: Government could not be fit on page. It will not be plotted.
## Warning: single could not be fit on page. It will not be plotted.
## Warning: income could not be fit on page. It will not be plotted.
## Warning: Republicans could not be fit on page. It will not be plotted.
## Warning: Democrats could not be fit on page. It will not be plotted.
## Warning: few could not be fit on page. It will not be plotted.
## Warning: relief could not be fit on page. It will not be plotted.
## Warning: priority could not be fit on page. It will not be plotted.
## Warning: week could not be fit on page. It will not be plotted.
## Warning: number could not be fit on page. It will not be plotted.
## Warning: lower could not be fit on page. It will not be plotted.
## Warning: one could not be fit on page. It will not be plotted.
## Warning: stay could not be fit on page. It will not be plotted.
## Warning: research could not be fit on page. It will not be plotted.
## Warning: double could not be fit on page. It will not be plotted.
## Warning: public could not be fit on page. It will not be plotted.
## Warning: develop could not be fit on page. It will not be plotted.
## Warning: encourage could not be fit on page. It will not be plotted.
## Warning: greater could not be fit on page. It will not be plotted.
## Warning: retirement could not be fit on page. It will not be plotted.
## Warning: account could not be fit on page. It will not be plotted.
## Warning: innovation could not be fit on page. It will not be plotted.
## Warning: eliminate could not be fit on page. It will not be plotted.
## Warning: foreign could not be fit on page. It will not be plotted.
## Warning: legislation could not be fit on page. It will not be plotted.
## Warning: example could not be fit on page. It will not be plotted.
## Warning: information could not be fit on page. It will not be plotted.
## Warning: enemy could not be fit on page. It will not be plotted.
## Warning: military could not be fit on page. It will not be plotted.
## Warning: Afghanistan could not be fit on page. It will not be plotted.
## Warning: democracy could not be fit on page. It will not be plotted.
## Warning: troops could not be fit on page. It will not be plotted.
## Warning: remain could not be fit on page. It will not be plotted.
## Warning: friend could not be fit on page. It will not be plotted.
## Warning: danger could not be fit on page. It will not be plotted.
## Warning: defend could not be fit on page. It will not be plotted.
## Warning: mission could not be fit on page. It will not be plotted.
## Warning: thank could not be fit on page. It will not be plotted.
## Warning: East could not be fit on page. It will not be plotted.
## Warning: liberty could not be fit on page. It will not be plotted.
## Warning: past could not be fit on page. It will not be plotted.
## Warning: coalition could not be fit on page. It will not be plotted.
## Warning: determine could not be fit on page. It will not be plotted.
## Warning: honor could not be fit on page. It will not be plotted.
## Warning: Middle could not be fit on page. It will not be plotted.
## Warning: service could not be fit on page. It will not be plotted.
## Warning: pursue could not be fit on page. It will not be plotted.
## Warning: region could not be fit on page. It will not be plotted.
## Warning: defeat could not be fit on page. It will not be plotted.
## Warning: administration could not be fit on page. It will not be plotted.
## Warning: industry could not be fit on page. It will not be plotted.
## Warning: recession could not be fit on page. It will not be plotted.
## Warning: become could not be fit on page. It will not be plotted.
## Warning: necessary could not be fit on page. It will not be plotted.
## Warning: housing could not be fit on page. It will not be plotted.
## Warning: factory could not be fit on page. It will not be plotted.
## Warning: Americans could not be fit on page. It will not be plotted.
## Warning: agreement could not be fit on page. It will not be plotted.
## Warning: compete could not be fit on page. It will not be plotted.
## Warning: entrepreneur could not be fit on page. It will not be plotted.
```

```
## Rendering topic 1
```

```
## Warning: success could not be fit on page. It will not be plotted.
## Warning: standard could not be fit on page. It will not be plotted.
## Warning: struggle could not be fit on page. It will not be plotted.
## Warning: training could not be fit on page. It will not be plotted.
## Warning: Chamber could not be fit on page. It will not be plotted.
## Warning: possible could not be fit on page. It will not be plotted.
## Warning: science could not be fit on page. It will not be plotted.
## Warning: teacher could not be fit on page. It will not be plotted.
## Warning: reward could not be fit on page. It will not be plotted.
## Warning: struggle could not be fit on page. It will not be plotted.
## Warning: Chamber could not be fit on page. It will not be plotted.
## Warning: course could not be fit on page. It will not be plotted.
## Warning: demand could not be fit on page. It will not be plotted.
## Warning: possible could not be fit on page. It will not be plotted.
## Warning: prepare could not be fit on page. It will not be plotted.
## Warning: science could not be fit on page. It will not be plotted.
## Warning: middle could not be fit on page. It will not be plotted.
## Warning: teacher could not be fit on page. It will not be plotted.
```

```
## Rendering topic 2
```

```
## Warning: savings could not be fit on page. It will not be plotted.
## Warning: funding could not be fit on page. It will not be plotted.
## Warning: extend could not be fit on page. It will not be plotted.
## Warning: private could not be fit on page. It will not be plotted.
## Warning: savings could not be fit on page. It will not be plotted.
## Warning: funding could not be fit on page. It will not be plotted.
## Warning: affordable could not be fit on page. It will not be plotted.
## Warning: receive could not be fit on page. It will not be plotted.
## Warning: extend could not be fit on page. It will not be plotted.
## Warning: address could not be fit on page. It will not be plotted.
```

```
## Rendering topic 3
## Rendering topic 4
```

```
## Warning: increase could not be fit on page. It will not be plotted.
## Warning: Congress could not be fit on page. It will not be plotted.
## Warning: percent could not be fit on page. It will not be plotted.
## Warning: spending could not be fit on page. It will not be plotted.
## Warning: growth could not be fit on page. It will not be plotted.
## Warning: mean could not be fit on page. It will not be plotted.
## Warning: Washington could not be fit on page. It will not be plotted.
## Warning: agree could not be fit on page. It will not be plotted.
## Warning: debt could not be fit on page. It will not be plotted.
## Warning: earn could not be fit on page. It will not be plotted.
## Warning: Government could not be fit on page. It will not be plotted.
## Warning: single could not be fit on page. It will not be plotted.
## Warning: income could not be fit on page. It will not be plotted.
## Warning: Republicans could not be fit on page. It will not be plotted.
## Warning: Democrats could not be fit on page. It will not be plotted.
## Warning: priority could not be fit on page. It will not be plotted.
## Warning: week could not be fit on page. It will not be plotted.
## Warning: lower could not be fit on page. It will not be plotted.
## Warning: one could not be fit on page. It will not be plotted.
## Warning: stay could not be fit on page. It will not be plotted.
## Warning: Congress could not be fit on page. It will not be plotted.
## Warning: reduce could not be fit on page. It will not be plotted.
## Warning: Federal could not be fit on page. It will not be plotted.
## Warning: percent could not be fit on page. It will not be plotted.
## Warning: dollar could not be fit on page. It will not be plotted.
## Warning: spend could not be fit on page. It will not be plotted.
## Warning: spending could not be fit on page. It will not be plotted.
## Warning: break could not be fit on page. It will not be plotted.
## Warning: growth could not be fit on page. It will not be plotted.
## Warning: mean could not be fit on page. It will not be plotted.
## Warning: Washington could not be fit on page. It will not be plotted.
## Warning: agree could not be fit on page. It will not be plotted.
## Warning: Government could not be fit on page. It will not be plotted.
## Warning: income could not be fit on page. It will not be plotted.
## Warning: Republicans could not be fit on page. It will not be plotted.
## Warning: Democrats could not be fit on page. It will not be plotted.
## Warning: priority could not be fit on page. It will not be plotted.
## Warning: week could not be fit on page. It will not be plotted.
## Warning: number could not be fit on page. It will not be plotted.
## Warning: stay could not be fit on page. It will not be plotted.
```

```
## Rendering topic 5
```

```
## Warning: research could not be fit on page. It will not be plotted.
## Warning: develop could not be fit on page. It will not be plotted.
## Warning: encourage could not be fit on page. It will not be plotted.
## Warning: retirement could not be fit on page. It will not be plotted.
## Warning: eliminate could not be fit on page. It will not be plotted.
## Warning: foreign could not be fit on page. It will not be plotted.
## Warning: legislation could not be fit on page. It will not be plotted.
## Warning: example could not be fit on page. It will not be plotted.
## Warning: information could not be fit on page. It will not be plotted.
## Warning: promise could not be fit on page. It will not be plotted.
## Warning: research could not be fit on page. It will not be plotted.
## Warning: public could not be fit on page. It will not be plotted.
## Warning: encourage could not be fit on page. It will not be plotted.
## Warning: greater could not be fit on page. It will not be plotted.
## Warning: retirement could not be fit on page. It will not be plotted.
## Warning: account could not be fit on page. It will not be plotted.
## Warning: innovation could not be fit on page. It will not be plotted.
## Warning: eliminate could not be fit on page. It will not be plotted.
## Warning: example could not be fit on page. It will not be plotted.
## Warning: launch could not be fit on page. It will not be plotted.
## Warning: information could not be fit on page. It will not be plotted.
```

```
## Rendering topic 6
```

```
## Warning: freedom could not be fit on page. It will not be plotted.
## Warning: enemy could not be fit on page. It will not be plotted.
## Warning: military could not be fit on page. It will not be plotted.
## Warning: peace could not be fit on page. It will not be plotted.
## Warning: Afghanistan could not be fit on page. It will not be plotted.
## Warning: democracy could not be fit on page. It will not be plotted.
## Warning: attack could not be fit on page. It will not be plotted.
## Warning: troops could not be fit on page. It will not be plotted.
## Warning: remain could not be fit on page. It will not be plotted.
## Warning: friend could not be fit on page. It will not be plotted.
## Warning: danger could not be fit on page. It will not be plotted.
## Warning: mission could not be fit on page. It will not be plotted.
## Warning: East could not be fit on page. It will not be plotted.
## Warning: liberty could not be fit on page. It will not be plotted.
## Warning: past could not be fit on page. It will not be plotted.
## Warning: stop could not be fit on page. It will not be plotted.
## Warning: coalition could not be fit on page. It will not be plotted.
## Warning: determine could not be fit on page. It will not be plotted.
## Warning: Middle could not be fit on page. It will not be plotted.
## Warning: service could not be fit on page. It will not be plotted.
## Warning: partner could not be fit on page. It will not be plotted.
## Warning: pursue could not be fit on page. It will not be plotted.
## Warning: region could not be fit on page. It will not be plotted.
## Warning: defeat could not be fit on page. It will not be plotted.
## Warning: States could not be fit on page. It will not be plotted.
## Warning: woman could not be fit on page. It will not be plotted.
## Warning: freedom could not be fit on page. It will not be plotted.
## Warning: Afghanistan could not be fit on page. It will not be plotted.
## Warning: democracy could not be fit on page. It will not be plotted.
## Warning: troops could not be fit on page. It will not be plotted.
## Warning: remain could not be fit on page. It will not be plotted.
## Warning: friend could not be fit on page. It will not be plotted.
## Warning: iraqi could not be fit on page. It will not be plotted.
## Warning: danger could not be fit on page. It will not be plotted.
## Warning: defend could not be fit on page. It will not be plotted.
## Warning: mission could not be fit on page. It will not be plotted.
## Warning: thank could not be fit on page. It will not be plotted.
## Warning: East could not be fit on page. It will not be plotted.
## Warning: liberty could not be fit on page. It will not be plotted.
## Warning: past could not be fit on page. It will not be plotted.
## Warning: stop could not be fit on page. It will not be plotted.
## Warning: coalition could not be fit on page. It will not be plotted.
## Warning: determine could not be fit on page. It will not be plotted.
## Warning: honor could not be fit on page. It will not be plotted.
## Warning: Middle could not be fit on page. It will not be plotted.
## Warning: service could not be fit on page. It will not be plotted.
## Warning: partner could not be fit on page. It will not be plotted.
## Warning: pursue could not be fit on page. It will not be plotted.
## Warning: region could not be fit on page. It will not be plotted.
## Warning: defeat could not be fit on page. It will not be plotted.
## Warning: God could not be fit on page. It will not be plotted.
```

```
## Rendering topic 7
```

```
## Warning: administration could not be fit on page. It will not be plotted.
## Warning: depend could not be fit on page. It will not be plotted.
## Warning: recession could not be fit on page. It will not be plotted.
## Warning: become could not be fit on page. It will not be plotted.
## Warning: happen could not be fit on page. It will not be plotted.
## Warning: necessary could not be fit on page. It will not be plotted.
## Warning: housing could not be fit on page. It will not be plotted.
## Warning: China could not be fit on page. It will not be plotted.
## Warning: factory could not be fit on page. It will not be plotted.
## Warning: Americans could not be fit on page. It will not be plotted.
## Warning: compete could not be fit on page. It will not be plotted.
## Warning: burden could not be fit on page. It will not be plotted.
## Warning: entrepreneur could not be fit on page. It will not be plotted.
## Warning: business could not be fit on page. It will not be plotted.
## Warning: recession could not be fit on page. It will not be plotted.
## Warning: happen could not be fit on page. It will not be plotted.
## Warning: necessary could not be fit on page. It will not be plotted.
## Warning: housing could not be fit on page. It will not be plotted.
## Warning: factory could not be fit on page. It will not be plotted.
## Warning: agreement could not be fit on page. It will not be plotted.
## Warning: compete could not be fit on page. It will not be plotted.
## Warning: burden could not be fit on page. It will not be plotted.
## Warning: entrepreneur could not be fit on page. It will not be plotted.
```

```
## Rendering topic 8
## Rendering topic 9
## Rendering topic 10
```


There are now 4 visualizations in the topic pages: first pf.time, then pf.wordcloud, and then another pf.time and another pf.wordcloud.

