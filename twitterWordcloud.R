library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

#setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)


hashtag <- searchTwitter("#weather", n=2500)
hashtag_text <- sapply(hashtag, function(x) x$getText())
hashtag_text_corpus <- Corpus(VectorSource(hashtag_text))
hashtag_text_corpus <- tm_map(hashtag_text_corpus,
                                     content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                                     mc.cores=1
)
hashtag_text_corpus <- tm_map(hashtag_text_corpus, content_transformer(tolower), mc.cores=1)
hashtag_text_corpus <- tm_map(hashtag_text_corpus, removePunctuation, mc.cores=1)
hashtag_corpus <- tm_map(hashtag_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)

pal2 <- brewer.pal(8,"Dark2")
wordcloud(hashtag_text_corpus,min.freq=4,max.words=100, random.order=T, colors=pal2)