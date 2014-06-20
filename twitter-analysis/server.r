library(twitteR)
library(stringr)
library(ROAuth)
library(RCurl)
library(ggplot2)
library(reshape)
library(tm)
library(RJSONIO)
library(wordcloud)
library(gridExtra)
library(plyr)
library(shinyIncubator)
library(shiny)

load("twitteR_credentials")
registerTwitterOAuth(twitCred)

# Function to create a data frame from tweets
shinyServer(function(input, output,session) {
  
  # Function to clean tweets, Stanton 2013
  CleanTweets<-function(tweets)
  {
    # Remove redundant spaces
    tweets <- str_replace_all(tweets," "," ")
    # Get rid of URLs
    tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
    # Take out retweet header, there is only one
    tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
    # Get rid of hashtags
    tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
    # Get rid of references to other screennames
    tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
    return(tweets)
    
  }
  
  #Search tweets and create a data frame 
  TweetFrame<-function(searchTerm, maxTweets)
  {
    twtList<-searchTwitter(searchTerm,n=maxTweets,cainfo="cacert.pem",lang="en")
    twtList1<- do.call("rbind",lapply(twtList,as.data.frame))
    twtList1$text<-iconv(twtList1$text, 'UTF-8', 'ASCII') #WILL THIS SOLVE THE UTF ENCODING PROBLEM: http://lists.hexdump.org/pipermail/twitter-users-hexdump.org/2013-May/000335.html
    return(twtList1)
    
  }
  
  # function to calculate number of tweets (input is text column, if the entire data frame was submitted, 
  #could've used nrow(), as done at a different place below)
  
  numoftweets<-function(entity1,entity2,entity1entry,entity2entry){
    ent1numtweets<-nrow(entity1)
    ent2numtweets<-nrow(entity2)
    notweets<-c(ent1numtweets,ent2numtweets)
    names(notweets)<-c(entity1entry,entity2entry)
    notweets
  } 
  
  
  # function for word cloud 
  
  wordcloudentity<-function(entitycleantext)
  {
    tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english')),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=TRUE)
    print(wcloudentity)
  }
  
  # Scoring sentiment expressed - Breen's algorithm
  #Jeffrey Breen: http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/ 
  #via Gaston Sanchez's twitter mining project: https://sites.google.com/site/miningtwitter/questions/sentiment/analysis   
  
  score.sentiment = function(sentences, pos.words, neg.words)
  {
       
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    
    scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)))
    return(scores.df)
  }
  
  #calling the above sentiment scoring function, the text of tweets serve as inputs
  
  sentimentalanalysis<-function(entity1text,entity2text,entity1entry,entity2entry){

    # A compiled list of words expressing positive and negative sentiments ----
    #http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
    # List of words and additional information on the original source from Jeffrey Breen's github site at:
    #https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English
  
    positivewords=readLines("positive_words.txt")
    negativewords=readLines("negative_words.txt")
    
    #Applying score.sentiment algorithm to cleaned tweets and getting data frames of tweets, net sentiment score for a tweet 
    #(number of positive sentiments minus negative sentiments)
    
    entity1score = score.sentiment(CleanTweets(entity1text),positivewords,negativewords)
    entity2score = score.sentiment(CleanTweets(entity2text),positivewords,negativewords)
    
    # Adding a dummy variable useful for a ggplot
    entity1score$entity = entity1entry
    entity2score$entity = entity2entry

    #combine all of this
    entityscores<-rbind(entity1score,entity2score)
    
    }   
  
  # Time for execution
  
  # Reading in values for the two entities
  entity1<-reactive({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    entity1<-TweetFrame(input$entity1, input$maxTweets)}
    )
  entity2<-reactive({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    entity2<-TweetFrame(input$entity2, input$maxTweets)}
    )
  
  
  #Creating sentiment scores
  entityscores<-reactive({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    entityscores<-sentimentalanalysis(entity1()$text,entity2()$text,input$entity1,input$entity2)})
  
  #Preparing the output in a series of tabs
  
 #tab 1  - number of tweets for the two entities and also plotting the probability of arrival of a new tweet 
  #within a particular time t
  
  #number of tweets
  output$notweets<-renderPrint({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    numoftweets(entity1(),entity2(),input$entity1,input$entity2)})
 
  
  #tab 1: Not all chatter may be good. So a box plot to see the distribution of scores of sentiments 
  
  output$sentiboxplot<-renderPlot({
    if(input$actb>=0 ){ 
    withProgress(session, min=1, max=15, expr={
      for(i in 1:15) {
        setProgress(message = 'Calculation in progress',
                    detail = 'This may take a while...',
                    value=i)
        
        Sys.sleep(0.1)
      }
    })}
    cutoff <- data.frame(yintercept=0, cutoff=factor(0))
    sentiboxplot<-ggplot(entityscores(),aes(x=size,y=score))+
                                     facet_grid(entity ~ .)+
                                     geom_point(color = "black",size = 2, alpha = 1/2)+
                                     geom_smooth(method = "loess",se=FALSE,col='red',size=1.5, alpha = 0.7)+
                                     geom_hline(aes(yintercept=yintercept, linetype=cutoff), data=cutoff)+
                                     xlab('Tweet number')+
                                     ylab('Sentiment Score')+
                                     theme_bw()
                                   print(sentiboxplot)})
    
  # getting a feel for how sentiments were scored by scanning 4 tweets per entity and sentiment scores - data frame entity scores shown
  output$sentiheadtable<-renderTable({tab<-head(entityscores(),4)})
  output$sentitailtable<-renderTable({tab<-tail(entityscores(),4)})
  
  #tab 2 - Word Clouds to highlight terms used in tweets associated with the two entities
  output$entity1wc<-renderText({
    
    input$entity1})
  output$entity1wcplot<-renderPlot({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    wordcloudentity(entity1()$text)})
                                    
  output$entity2wc<-renderText({input$entity2})
  output$entity2wcplot<-renderPlot({
    if(input$actb>=0 ){ 
      withProgress(session, min=1, max=15, expr={
        for(i in 1:15) {
          setProgress(message = 'Calculation in progress',
                      detail = 'This may take a while...',
                      value=i)
          
          Sys.sleep(0.1)
        }
      })}
    wordcloudentity(entity2()$text)})
  
  
  #tab  3: Raw tweets of entity 1
  output$tableentity1 <- renderTable({tab<-entity1()[1]})
  
  #tab 4: Raw tweets of entity 2
  
  output$tableentity2<-renderTable({tab<-entity2()[1]})
  
    
})

