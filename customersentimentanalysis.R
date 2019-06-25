#install packages
library(tidyverse)
library(stringr)

#import dataset
CSA <- read_csv(file.choose()) #replace function with path

tbc <- as_tibble(CSA)
head(tbc)

# Data prep and pattern
library(mice)
md.pattern(tbc)

# sentiment analysis
library ("plyr")
library(ggplot2)
# storing feedback into df
fb <- as.data.frame(tbc[11])
head(fb)

#sentiment score function
score.sentiment = function(sentences, pos.words, neg.words,.progress='none')  
{  
  require(plyr)  
  require(stringr)       
  
  # we got a vector of sentences. plyr will handle a list  
  # or a vector as an "l" for us  
  # we want a simple array ("a") of scores back, so we use   
  # "l" + "a" + "ply" = "laply":  
  
  good.smiley <- c(":)")
  bad.smiley <- c(":(",";)",":'",":P") 
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {  
    
    # clean up sentences with R's regex-driven global substitute, gsub():  
    
    sentence = gsub(":)", 'awsum', sentence)
    
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

#Load sentiment word lists
hu.liu.pos = scan('C://Users//sahithi//Documents//R//My projects//twitter sentiment analysis//positive-words.txt', what='character', comment.char=';')
hu.liu.neg = scan('C://Users//sahithi//Documents//R//My projects//twitter sentiment analysis//negative-words.txt', what='character', comment.char=';')

#Add words to list
pos.words = c(hu.liu.pos, 'excellent', 'nice', 'satisfied','Thank', 'good', 'great', 'happy')
neg.words = c(hu.liu.neg, 'poor', 'not', 'worse', 'dissatisfied', "unprofessional", 'irresponsible', 'bad')

#convert text to factor
(fb$Feedback <- as.factor(fb$Feedback))


# calculate score
fb.scores = score.sentiment(fb$Feedback,pos.words,neg.words, .progress='text') 
head(fb.scores$score)
#Check the negative sentences. What made them negative
fb.scores.2 = subset(fb.scores,fb.scores$score < 0)

head(fb.scores.2)

# Final outputs
hist(fb.scores$score)

table(fb.scores$score)

getwd()
write.csv(file = "Feedbackscore.csv", x=fb.scores)

# data vizualisation
ggplot(tbc, mapping = aes('City Name')) +
  geom_point()


