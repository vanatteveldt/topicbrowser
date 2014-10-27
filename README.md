Browse LDA Topic Models
========================================================

This package allows you to create a set of HTML files to browse a topic model.
It creates a word cloud and time-graph per topic, and annotates a selection of documents with the topic for each word.

See the [example output](https://cdn.rawgit.com/vanatteveldt/topicbrowser/master/sotu/index.html) for a collection of State of the Union addresses.

Installing 
----


```r
if (!require(devtools)) {install.packages("devtools"); library(devtools)}
install_github("vanatteveldt/topicbrowser")
library(topicbrowser)
```

Creating a topic browser
----

To create a topic browser, you need to have:

- A model fit using `topicmodels::LDA`
- The set of original tokens used to create the document term matrix, and the document ids these tokens are from
- The metadata of the documents, containing aid, headline, and date

The provided data file 'sotu' contains this data from the state of the union addresses. 
Make sure that the tokens are ordered in the way they appeared in the article


```r
data(sotu)
tokens = tokens[order(tokens$aid, tokens$id), ]
class(m)
```

```
## [1] "LDA_Gibbs"
## attr(,"package")
## [1] "topicmodels"
```

```r
head(tokens)
```

```
##          aid      lemma       word sentence  pos offset id pos1 freq
## 20 111541965         it         It        1  PRP      0  1    O    1
## 10 111541965         be         is        1  VBZ      3  2    V    1
## 40 111541965         we        our        1 PRP$      6  3    O    1
## 39 111541965 unfinished unfinished        1   JJ     10  4    A    1
## 32 111541965       task       task        1   NN     21  5    N    1
## 38 111541965         to         to        1   TO     26  6    ?    1
```

```r
head(meta)
```

```
##          id       date   medium     headline
## 1 111541965 2013-02-12 Speeches Barack Obama
## 2 111541995 2013-02-12 Speeches Barack Obama
## 3 111542001 2013-02-12 Speeches Barack Obama
## 4 111542006 2013-02-12 Speeches Barack Obama
## 5 111542013 2013-02-12 Speeches Barack Obama
## 6 111542018 2013-02-12 Speeches Barack Obama
```

With these data, you can create a topic browser as follows:


```r
output = file.path(tempdir(), "topics")
createTopicBrowser(m, tokens$lemma, tokens$aid, words=tokens$word, meta=meta, folder_name=output)
```

```
## Saving pictures to /tmp/Rtmpshv1IU/topics
## Rendering index page to /tmp/Rtmpshv1IU/topics/index.html
## Rendering topic 1 to /tmp/Rtmpshv1IU/topics/t1.html
## Rendering topic 2 to /tmp/Rtmpshv1IU/topics/t2.html
## Rendering topic 3 to /tmp/Rtmpshv1IU/topics/t3.html
## Rendering topic 4 to /tmp/Rtmpshv1IU/topics/t4.html
## Rendering topic 5 to /tmp/Rtmpshv1IU/topics/t5.html
## Rendering topic 6 to /tmp/Rtmpshv1IU/topics/t6.html
## Rendering topic 7 to /tmp/Rtmpshv1IU/topics/t7.html
## Rendering topic 8 to /tmp/Rtmpshv1IU/topics/t8.html
## Rendering topic 9 to /tmp/Rtmpshv1IU/topics/t9.html
## Rendering topic 10 to /tmp/Rtmpshv1IU/topics/t10.html
```

This produces the results shown in the [sotu](https://cdn.rawgit.com/vanatteveldt/topicbrowser/master/sotu/index.html) folder
