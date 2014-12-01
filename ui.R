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

#options(RCHART_LIB = 'NVD3')

#load file and read screenNames
filen<-"tweetsconso.csv"
tweets_all<-read.csv(filen)
tweets_all$date<-as.Date(tweets_all$date)
TweetNames<-unique(tweets_all$screenName)
LatestDate<-max(tweets_all$date)
EarliestDate<-min(tweets_all$date)

shinyUI(
  
  fluidPage( 
  #titlePanel(""), # give the interface a title
  
  #hr(),
  
  fluidRow( 
  
  column(3,
			 h6("| Simple Plots |"),
			 selectInput(inputId = "SimplePlotType",label = "Select plot type", 
			 choices = list("ggplot2"="ggplot2graphics", 
			 "parallel coordinates"="parallelcoord")),
			 h6("| rCharts |"),	
			 selectInput(inputId = "RPlotType",label = "Select plot type", 
			 choices = list("Line Chart" = "linechart", "Line Chart with Focus" = "linechartfocus", "Scatter Plot"="scatterplot", 
			 "Barchart"="barchart")),
			 
			 
			 h6("| Options (where applicable) |"),
			 h6("| Period of Analysis |"),
			 textInput(inputId = "StartDate",  label = "START DATE (YYYY-MM-DD).", value = EarliestDate), 
			 textInput(inputId = "EndDate",  label = "END DATE (YYYY-MM-DD).", value = LatestDate),
			 radioButtons("CountSentimentFaveRetweet", label = "Select data to plot", choices = list("No. of Tweets" = "NumTweets", "Sentiment Count" = "SentimentCount","Favorited Total" = "FaveCount", "Retweeted Total" = "RetweetCount"), selected = "NumTweets"),
			 selectInput(inputId = "Tweeter1", label = "Select tweeter users to compare between", choices = TweetNames, selected = "TheEconomist"),
			 selectInput(inputId = "Tweeter2", label = "", choices = TweetNames, selected = "FinancialTimes"),
			 selectInput(inputId = "Tweeter3", label = "", choices = TweetNames, selected = "BloombergNews"),
			 selectInput(inputId = "Tweeter4", label = "", choices = TweetNames, selected = "ReutersBiz")
			 
			 
			 )
   
  ,
  
column(9,	
	
			 # h6("| Choose plot type |"),
			 # selectInput(inputId = "SimplePlotType",label = "Select plot type", 
			 # choices = list("ggplot2"="ggplot2graphics", 
			 # "parallel coordinates"="parallelcoord")),
			 # h6("| rCharts |"),	
			 # selectInput(inputId = "RPlotType",label = "Select plot type", 
			 # choices = list("Line Chart" = "linechart", "Line Chart with Focus" = "linechartfocus", "Scatter Plot"="scatterplot", 
			 # "Barchart"="barchart"))
			 
			  tabsetPanel( 
    
				tabPanel("Simple Plots", plotOutput("TweeterComparison")), 
				tabPanel("rCharts", showOutput("rCharts", "nvd3"))

    
			)
			 
			 
	)
)

# ,

# fluidRow( 
  
  # # tabsetPanel( 
    
	# # #tabPanel("Heatmap", textOutput("textDisplayLine1"), h6("Twitter accounts are arranged in descending order (left to right, top to bottom) based on number of tweets. The heatmap indicates the sentiment score."), plotOutput("HM",  height = "100%"))
# # #	tabPanel("Comparison", h6("No of tweets"), plotOutput("TweeterComparison"), h6("Sentiment"), plotOutput("SentimentComparison")),
	# # tabPanel("Simple Plots", plotOutput("TweeterComparison")), 
	# # tabPanel("rCharts", showOutput("rCharts", "nvd3"))
	# # # tabPanel("Comparison3", h6("No of tweets"), plotOutput("HM2", width = "10%"))
	# # #tabPanel("Heatmap", plotOutput("SentimentComparison",  height = "100%"))
    
	# # )
	
  # )
  

))