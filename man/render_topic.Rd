% Generated by roxygen2 (4.0.1): do not edit by hand
\name{render_topic}
\alias{render_topic}
\title{Render a single topic}
\usage{
render_topic(topic_id, tokens_topics, meta, topics_per_doc, topics_per_term,
  nmaxdoc = 10, date_interval = "year")
}
\arguments{
\item{topic_id:}{the topic id to render}

\item{tokens_topics:}{a data frame with tokens (id, aid, term, word)}

\item{meta:}{a data frame with meta information about the documents}

\item{topics_per_term:}{a matrix of terms by topics}

\item{topics_per_doc:}{a matrix of documents by topics}

\item{nmaxdoc:}{the number of top documents to show}

\item{date_interval:}{optional date interval to use for time graphs}
}
\value{
a raw html string (character)
}
\description{
Render a single topic
}
