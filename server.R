##############################################
##### Twitter Sentiment Analysis Web App #####
##############################################

library(shiny) # load shiny at beginning at both scripts
library(gplots)
library(ggplot2)
library(GGally)
library(xtable)
library(gridExtra)
library(plyr)
library(stringr)
#library(reShape)
library(rCharts)
#options(RCHART_WIDTH = 800)
#load file
filen<-"tweetsconso.csv"
tweets_all<-read.csv(filen)
tweets_all$date<-as.Date(tweets_all$date)

##Function for scoring of strings against list of positive and negative words
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
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
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}
#######################################################
####################SETUP DATA#########################
#######################################################

#load list of positive and negative words from http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
positivewords=scan('positive-words.txt', what='character', comment.char=';')
negativewords=scan('negative-words.txt', what='character', comment.char=';')

#score the tweets by comparing against the list of positive and negative words, using the score.sentiment function
#append the score to the tweets_all dataframe
tweets_all$score=score.sentiment(tweets_all$text, positivewords, negativewords, .progress="text")$score

#aggregate the scores by the Twitter username and dates
sentiment_count<-aggregate(score ~ screenName+date, data = tweets_all, FUN = sum)
#sentiment_count$date<-as.Date(sentiment_count$date)
# # aggregate the number of tweets by the Twitter username and dates
tweet_count<-aggregate(text ~ screenName+date, data = tweets_all, FUN = length)
names(tweet_count)[3]<-"numtweets"
fave_count<-aggregate(favoriteCount ~ screenName+date, data = tweets_all, FUN = sum)
retweet_count<-aggregate(retweetCount ~ screenName+date, data = tweets_all, FUN = sum)

####building a matrix of sentiment scores; and a matrix of names - descending by number of tweets
sentiment_count_sum<-aggregate(score ~ screenName, data = sentiment_count, FUN = sum)
tweet_count_sum<-aggregate(numtweets ~ screenName, data = tweet_count, FUN = sum)

tweet_merge<-merge(sentiment_count_sum, tweet_count_sum, by="screenName")
tweets_merge_sorted<- tweet_merge[order(-tweet_merge$numtweets),]
tweet_sorted_mat<-matrix(tweets_merge_sorted[1:25,]$score, nrow=5, ncol=5, byrow=TRUE)
tweet_sorted_mat_name<-matrix(tweets_merge_sorted[1:25,]$screenName, nrow=5, ncol=5, byrow=TRUE)

fave_count_sum<-aggregate(favoriteCount ~ screenName, data = fave_count, FUN = sum)
retweet_count_sum<-aggregate(retweetCount ~ screenName, data = retweet_count, FUN = sum)

tweet_merge_count_fave_score<-merge(tweet_merge, fave_count_sum, by="screenName")
tweet_merge_count_fave_score_retweet<-merge(tweet_merge_count_fave_score, retweet_count_sum, by="screenName")

sort_tweet_merge_count_fave_score_retweet<-tweet_merge_count_fave_score_retweet[order(-tweet_merge_count_fave_score_retweet$score),]


#######################################################

shinyServer(function(input, output) { 

################SETUP REACTIVE DATA#####################
	 SortAll <- reactive({
		
		sort_all<-sort_tweet_merge_count_fave_score_retweet
		
		sort_all
	  })
	 
	 CountData4 <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		Tweeter1<-input$Tweeter1
		Tweeter2<-input$Tweeter2
		Tweeter3<-input$Tweeter3
		Tweeter4<-input$Tweeter4
		
		tweet_compare_4<-subset(tweet_count, tweet_count$date>=StartDate & tweet_count$date<=EndDate & (tweet_count$screenName==Tweeter1 | tweet_count$screenName==Tweeter2 | tweet_count$screenName==Tweeter3 | tweet_count$screenName==Tweeter4))
		
		tweet_compare_4
	  })
	  
	  CountDataAll <-reactive ({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)

		tweet_compare_all<-subset(tweet_count, tweet_count$date>=StartDate & tweet_count$date<=EndDate)
		
		tweet_compare_all
	  })
	   
	 SentimentData4 <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		Tweeter1<-input$Tweeter1
		Tweeter2<-input$Tweeter2
		Tweeter3<-input$Tweeter3
		Tweeter4<-input$Tweeter4
		
		sentiment_compare_4<-subset(sentiment_count, sentiment_count$date>=StartDate & sentiment_count$date<=EndDate & (sentiment_count$screenName==Tweeter1 | sentiment_count$screenName==Tweeter2 | sentiment_count$screenName==Tweeter3| tweet_count$screenName==Tweeter4))
		
		sentiment_compare_4
	  })
	  
	  SentimentDataAll <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		
		sentiment_compare_all<-subset(sentiment_count, sentiment_count$date>=StartDate & sentiment_count$date<=EndDate)
		
		sentiment_compare_all
	  })


	  FaveDataAll <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		
		fave_compare_all<-subset(fave_count, fave_count$date>=StartDate & fave_count$date<=EndDate)
		
		fave_compare_all
	  })
	  
	  FaveData4 <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		Tweeter1<-input$Tweeter1
		Tweeter2<-input$Tweeter2
		Tweeter3<-input$Tweeter3
		Tweeter4<-input$Tweeter4
		
		fave_compare_4<-subset(fave_count, fave_count$date>=StartDate & fave_count$date<=EndDate & (fave_count$screenName==Tweeter1 | fave_count$screenName==Tweeter2 | fave_count$screenName==Tweeter3| fave_count$screenName==Tweeter4))
		
		fave_compare_4
	  })

	  RetweetDataAll <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		
		retweet_compare_all<-subset(retweet_count, retweet_count$date>=StartDate & retweet_count$date<=EndDate)
		
		retweet_compare_all
	  })
	  
	  RetweetData4 <- reactive({
		
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		Tweeter1<-input$Tweeter1
		Tweeter2<-input$Tweeter2
		Tweeter3<-input$Tweeter3
		Tweeter4<-input$Tweeter4
		
		retweet_compare_4<-subset(retweet_count, retweet_count$date>=StartDate & retweet_count$date<=EndDate & (retweet_count$screenName==Tweeter1 | retweet_count$screenName==Tweeter2 | retweet_count$screenName==Tweeter3| retweet_count$screenName==Tweeter4))
		
		retweet_compare_4
	  })
	  
	  AllData <- reactive ({
	  
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		
		if(input$CountSentimentFaveRetweet=="NumTweets"){
		data_all<-subset(tweet_count, tweet_count$date>=StartDate & tweet_count$date<=EndDate)
		}
		if(input$CountSentimentFaveRetweet=="SentimentCount"){
		data_all<-subset(sentiment_count, sentiment_count$date>=StartDate & sentiment_count$date<=EndDate)	
		}
		if(input$CountSentimentFaveRetweet=="FaveCount"){
		data_all<-subset(fave_count, fave_count$date>=StartDate & fave_count$date<=EndDate)
		}
		if(input$CountSentimentFaveRetweet=="RetweetCount"){
		data_all<-subset(retweet_count, retweet_count$date>=StartDate & retweet_count$date<=EndDate)
		}
		names(data_all)[3]<-"score"		
		data_all
	  })

	 AllData4 <- reactive ({
	  
		StartDate<-as.Date(input$StartDate)
		EndDate<-as.Date(input$EndDate)
		Tweeter1<-input$Tweeter1
		Tweeter2<-input$Tweeter2
		Tweeter3<-input$Tweeter3
		Tweeter4<-input$Tweeter4
		
		if(input$CountSentimentFaveRetweet=="NumTweets"){
		data_all4<-subset(tweet_count, tweet_count$date>=StartDate & tweet_count$date<=EndDate & (tweet_count$screenName==Tweeter1 | tweet_count$screenName==Tweeter2 | tweet_count$screenName==Tweeter3 | tweet_count$screenName==Tweeter4))
		}
		if(input$CountSentimentFaveRetweet=="SentimentCount"){
		data_all4<-subset(sentiment_count, sentiment_count$date>=StartDate & sentiment_count$date<=EndDate & (sentiment_count$screenName==Tweeter1 | sentiment_count$screenName==Tweeter2 | sentiment_count$screenName==Tweeter3| tweet_count$screenName==Tweeter4))
		}
		if(input$CountSentimentFaveRetweet=="FaveCount"){
		data_all4<-subset(fave_count, fave_count$date>=StartDate & fave_count$date<=EndDate & (fave_count$screenName==Tweeter1 | fave_count$screenName==Tweeter2 | fave_count$screenName==Tweeter3| fave_count$screenName==Tweeter4))
		}
		if(input$CountSentimentFaveRetweet=="RetweetCount"){
		data_all4<-subset(retweet_count, retweet_count$date>=StartDate & retweet_count$date<=EndDate & (retweet_count$screenName==Tweeter1 | retweet_count$screenName==Tweeter2 | retweet_count$screenName==Tweeter3| retweet_count$screenName==Tweeter4))
		}
		names(data_all4)[3]<-"score"		
		data_all4
	  })
	 ########################################################
	 ######################OUTPUTS###########################
	 ########################################################

		output$TweeterComparison<- renderPlot({
		if(input$SimplePlotType=="ggplot2graphics"){
			if(input$CountSentimentFaveRetweet=="NumTweets"){
				print(ggplot(data=CountData4(), aes(x=date, y=numtweets, color=screenName, group=screenName)) + geom_line()+ theme(legend.title=element_blank(), legend.position="bottom"))
			}
			if(input$CountSentimentFaveRetweet=="SentimentCount"){
				print(ggplot(data=SentimentData4(), aes(x=date, y=score, color=screenName, group=screenName)) + geom_line()+ theme(legend.title=element_blank(), legend.position="bottom"))
			}
			if(input$CountSentimentFaveRetweet=="FaveCount"){
				print(ggplot(data=FaveData4(), aes(x=date, y=favoriteCount, color=screenName, group=screenName)) + geom_line()+ theme(legend.title=element_blank(), legend.position="bottom"))
			}
			if(input$CountSentimentFaveRetweet=="RetweetCount"){
				print(ggplot(data=RetweetData4(), aes(x=date, y=retweetCount, color=screenName, group=screenName)) + geom_line()+ theme(legend.title=element_blank(), legend.position="bottom"))
			}
		}
		
		if(input$SimplePlotType=="parallelcoord"){
		print(ggparcoord(SortAll(),columns = c(3:5, 2), groupColumn = 1, showPoints=TRUE) + geom_line() + ylab("Counts") + xlab("") + ggtitle("Comparing tweets") + geom_point() + geom_text(aes(label=screenName),hjust=0, vjust=0, cex= 3))
		}
		
		
    	})
		
		output$rCharts <- renderChart({
		
		if(input$RPlotType=="linechart"){
 
		nmul <- nPlot(score ~ date, group =  'screenName', data = AllData(), type = "lineChart", dom = 'rCharts', width = 800)
		nmul$xAxis(
		  tickFormat =   "#!
			  function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}
			!#",
		  rotateLabels = -90
		)
		
		#rPlot(score ~ date, group =  'screenName', data = SentimentDataAll(), type = "point")
		
		nmul$save('line.html')
		return(nmul)
		}
		
		if(input$RPlotType=="linechartfocus"){ 
		nmul <- nPlot(score ~ date, group =  'screenName', data = AllData(), type = "lineWithFocusChart", dom = 'rCharts', width = 800)
		nmul$xAxis(
		  tickFormat =   "#!
			  function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}
			!#",
		  rotateLabels = -90
		)

		nmul$save('linefocus.html')
		return(nmul)
		}
		
		
		if(input$RPlotType=="scatterplot"){ 
		
		nmul <- nPlot(score ~ date, group =  'screenName', data = AllData4(), type = "scatterChart", dom = 'rCharts', width = 800)
		nmul$xAxis(
		tickFormat =   "#!
		function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}
		!#",
		rotateLabels = -90
		)
			
		nmul$save('scatter.html')
		return(nmul)
		}
		
		if(input$RPlotType=="barchart"){ 
		
		nmul <- nPlot(score ~ date, group =  'screenName', data = AllData4(), type = "multiBarChart", dom = 'rCharts', width = 800)
		nmul$xAxis(
		tickFormat =   "#!
		function(d) {return d3.time.format('%Y-%m-%d')(new Date(d*1000*3600*24));}
		!#",
		rotateLabels = -90
		)
			
		nmul$save('bar.html')
		return(nmul)
		}
		
		})
		

		# output$SentimentComparison<- renderPlot({
    
		# print(ggplot(data=SentimentData(), aes(x=date, y=score, color=screenName, group=screenName)) + geom_line()+ theme(legend.title=element_blank(), legend.position="bottom"))
    
		# })
		
		# output$HM<- renderPlot({
		
		  # heatmap(as.matrix(TweetMergeS()),
		          # Rowv=NA,
		          # Colv=NA,
		          # scale="column", 
		          # labRow=tweets_merge_sorted[,1],
		          # labCol=c("score", "number"), margins = c(5,10)
		  # )
			# })
   
		# output$HM2<- renderPlot({
		# my.breaks <- c(seq(0,50,length=100), seq(50,100,length=100), seq(100,200,length=100))
		# my_palette <- colorRampPalette(c("red", "orange", "green")) (length(my.breaks)-1)
		# heatmap.2(MatrixData(), trace = "none", density.info = "none", cellnote=MatrixDataName(), notecex=0.95, 
	             # notecol="black", dendrogram="none", keysize=0.62, col=my_palette, Rowv=FALSE, Colv=FALSE, 
	             # lhei=c(0.5, 4), lwid=c(0.5, 4), breaks=my.breaks)
	   
			# })
			
		# output$textDisplayLine1<- renderText({ 

		# print(MatrixData())
   
		# })
   
})

