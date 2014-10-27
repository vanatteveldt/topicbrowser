library(knitr)

source('lib.r')

# get data
load(file='sotu.rdata')

# get wordassignments
wordassignments = getWordAssignments(m) 

# Match tokens with wordassignments
colnames(tokens) # Check names for the column for article ids, and column for the tokens used in the LDA model
colnames(wordassignments) # Match to tokens with the article_id and term columns
tokens_topics = merge(tokens, wordassignments, by.x=c('aid','lemma'), by.y=c('article_id','term'), all.x=T) # match by the names for the article_id and term columns. Be sure to use all.x=T, or you'll drop all the unassigned terms

# create topic browser
createTopicBrowser(tokens_topics, wordassignments, meta, folder_name='topicbrowser')


