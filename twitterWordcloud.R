#This work was developed over 2 days during a Cloud Hackathon. I was completely new to Amazon Web Services, so some time
#was taken up configuring Instances and VMs. It was also the first time I had used an API for grab data.
#This is a proof of concept to run data analysis in the browser. The code has some errors with respect to the 
#TermDocumentMatrix() function - line 32. 
#In general the code does the following:
#Retrieves top 500 tweets for the user defined #hashtag.
#Removes twitter handles, tidies the words up (remove punctuation & take the stem of common words)
#Generates wordcloud from the frequency of words, saves as .png

require(twitteR)
rdmTweets <- searchTwitter('#exeter', n=500)
tw.df <- do.call("rbind", lapply(rdmTweets, as.data.frame))
 
RemoveAtPeople <- function(tweet) {
  gsub("@\\w+", "", tweet)
}

tweets <- as.vector(sapply(tw.df$text, RemoveAtPeople))
 

require(tm)
#Call with eg: tw.c=generateCorpus(tw.df$text)
generateCorpus= function(df,my.stopwords=c()){
  tw.corpus= Corpus(VectorSource(df))
  tw.corpus = tm_map(tw.corpus, removePunctuation)
  tw.corpus = tm_map(tw.corpus, tolower)
  tw.corpus = tm_map(tw.corpus, removeWords, stopwords('english'))
  tw.corpus = tm_map(tw.corpus, removeWords, my.stopwords)
  tw.corpus = tm_map(tw.corpus, stemDocument)
 
  tw.corpus
}

wordcloud.generate=function(tw.corpus,min.freq=3){
  require(wordcloud)
  doc.m = TermDocumentMatrix(tw.corpus,control = list(minWordLength = 1))
  dm = as.matrix(doc.m)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  #Generate the wordcloud
  wc=wordcloud(d$word, d$freq, min.freq=min.freq)
  wc
}
 
print(wordcloud.generate(generateCorpus(tweets,'dev8d'),7))
 
##Generate an image file of the wordcloud
png(filename='~R/plots/twitter1')
wordcloud.generate(generateCorpus(tweets,'dev8d'),7)
dev.off()
 
