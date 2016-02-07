###
### Use tm (text mining) package
###

require(tm)

# build a corpus
cT.corpus <- Corpus(VectorSource(cT$Name))

# make each letter lowercase
cT.corpus <- tm_map(cT.corpus, content_transformer(tolower)) 

# remove punctuation 
cT.corpus <- tm_map(cT.corpus, removePunctuation)

# remove generic and custom stopwords
# my_stopwords <- c(stopwords('english'), 'prolife', 'prochoice')
# cT.corpus <- tm_map(cT.corpus, removeWords, my_stopwords)

# build a term-document matrix
cT.dtm <- TermDocumentMatrix(cT.corpus)

# inspect the document-term matrix
cT.dtm

# inspect most popular words
findFreqTerms(cT.dtm, lowfreq=30)

cT.dtm2 <- removeSparseTerms(cT.dtm, sparse=0.996)

bag.df <- t(as.data.frame(inspect(cT.dtm2)))

