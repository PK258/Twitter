library(googleLanguageR)
library(cld2)
library(purrr)
library(RJSONIO)
library(googleAuthR)
library(httr)
library(RCurl)
library(rjson)
library(plotly)
library(data.table)
library(wordcloud)

# Text for the analysis
json_data <- fromJSON(sprintf("[%s]", paste(readLines('C:/Users/pradeep.kuravi/Desktop/Work/Data/Twitter/febiac-merged-tweets.json'),collapse=",")))

lang  = NULL
tweet = NULL
reach = NULL
date  = NULL
counter = 1

for (i in 1:length(json_data)) {
  if( (is.null(json_data[[i]]$retweeted_status) == TRUE) & (is.element(json_data[[i]]$lang,c("de", "en", "fr", "nl")) == TRUE)) {
    lang[counter] = json_data[[i]]$lang
    tweet[counter] = json_data[[i]]$text
    reach[counter] = json_data[[i]]$user$followers_count
    date[counter] = json_data[[i]]$created_at
    
    counter = counter +1
  } 
}

tweet = gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tweet)
tweet = gsub("#", "", tweet)
tweet = gsub("@", "",tweet)
tweet = gsub('\"', "",tweet)
tweet = iconv(tweet, "latin1", "ASCII", sub="")

for (i in 1:length(tweet)) {
  if (nchar(tweet[i]) == 0)
  {
    tweet[i] = "No Tweet"
  }
}
translated = NULL
gl_auth("C:/Users/pradeep.kuravi/Desktop/Work/Codes/Twitter/SentimentAnalysis-bd0de21542b2.json")
for (i in 1:length(tweet)) {
  translated[i] = ifelse(lang[i] =="nl", gl_translate(tweet[i]),tweet[i])
}
senti = gl_nlp(translated)
scores = NULL
magnitude =NULL
sentences = NULL
datatable = NULL
for(i in 1:length(senti$sentences)){
scores[i] = min(senti$sentences[[i]]$score)
magnitude[i] = min(senti$sentences[[i]]$magnitude)
sentences[i] = senti$sentences[[i]]$content
}

datatable = data.table(text = tweet, reach= reach, score =  scores, magnitude=magnitude) 
dataframe = as.data.frame(datatable)
resultPolarity = data.frame("Polarity" = c("Positive","Negative","Neutral"),"Count"= c(length(which(scores > 0)),length(which(scores < -0.7)),length(which(scores >= -0.7 & scores < 0))))

pie <- plot_ly(resultPolarity, labels = ~Polarity, values = ~Count, type = 'pie',textposition = 'inside',
             textinfo = 'label+percent') %>%
  layout(title = 'Sentiment Analysis',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
scatter_reach_Sentiment = plot_ly(data = dataframe,x = ~reach, y=~score,type = 'scatter',mode ="markers")

wc1 = wordcloud(unique(tweet), max.words = 100, random.order = FALSE,colors ="black")
wc2 = wordcloud(unique(tweet[which(scores < -0.7)]),max.words = 100,random.order = FALSE,colors = "red")
wc3 = wordcloud(unique(tweet[which(scores > 0.2)]),max.words = 100,random.order = FALSE,colors = "blue")


Sys.setenv("plotly_username"="PK123")
Sys.setenv("plotly_api_key"="AOvtytOfd8TPVy6Et2OH")

api_create(pie, filename = "Sentiment_piechart")
api_create(scatter_reach_Sentiment, filename = "scatter_reach_Sentiment")

ggplot(as.data.frame(table(datatable))) + geom_bar(stat="identity")


