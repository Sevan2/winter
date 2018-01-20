# ensure_package("package_name") function will check if the particular package is installed in RStudio
# If yes, then will do nothing, if not, then will download and install the required package

ensure_package<-function(x)
    {
  
  x<-as.character(x)

  if (!require(x, character.only = TRUE))
      {
        install.packages(pkgs=x,repos="http://cran.r-project.org")
        require(x,character.only=TRUE)
      }
    }
 
# prepare_twitter() function will ensure necessary packages to be installed and will setup twitter oauth

prepare_twitter<-function()
{
  ensure_package("bitops")
  ensure_package("RCurl")
  ensure_package("RJSONIO")
  ensure_package("twitteR")
  ensure_package("tm")
  ensure_package("wordcloud")
  ensure_package("stringr")
  
  
  consumer_key<-'GjVl4LfD4uT5EUCqa5qlCGnkE'
  consumer_secret<-'gXPZaIkTjwSZ4QjtRLiPsQeop3sHSS9ZASwxSKx8UO966FkpOc'
  access_token<-'232269184-lub1ArD0qc5RwsT89StdHHRKWmTIHgsJlbW4PVCq'
  access_secret<- 's1Cdwvf7qBzwJL9SIT3eCVYMcvK6ageFBnvic8mBNYYt1'
  setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
  
}

prepare_twitter()
 

prepare<-function()
{
  consumer_key<-'GjVl4LfD4uT5EUCqa5qlCGnkE'
  consumer_secret<-'gXPZaIkTjwSZ4QjtRLiPsQeop3sHSS9ZASwxSKx8UO966FkpOc'
  access_token<-'232269184-lub1ArD0qc5RwsT89StdHHRKWmTIHgsJlbW4PVCq'
  access_secret<- 's1Cdwvf7qBzwJL9SIT3eCVYMcvK6ageFBnvic8mBNYYt1'
  setup_twitter_oauth(consumer_key,consumer_secret,access_token, access_secret)
  
  
  
}

# This function tweet(search_term,maxtweet) will get the tweets using the arguments 
# then will convert the result into a dataframe and subset the dataframe created (time) variable


tweet<-function(search_term,maxtweet)
{
  
  twitt<-searchTwitter(search_term,n=maxtweet, resultType = "recent")
  twittframe<-do.call("rbind",lapply(twitt,as.data.frame))
  return(twittframe[ordered(as.integer(twittframe$created)),])
  
}

#### Now, to start tweet mining, we need to run these 3 functions first
# ensure_package() , prepare_twitter() and tweet(), then we are ready to do tweeter mining

prepare_twitter()



# To visualize cumulative probabilty distribution 

ArrivalProbability<-function(times,increment,max)
{
  #initialize an empty vector
  
  plist<-NULL
  
  # Probability is defined over the size of this sample of arrival times
  
  timLen<-length(times)
  
  # may not be necessary but checks for input mistakes
  if(increment>max) {return(NULL)}
  
  for (i in seq(increment,max,by=increment))
  { 
    # diff() requires a sorted list of times 
    # diff() calculates the delays between neighboring times
    # logical test <i provides a list of TRUE and FALSE values
    #of length=timeLen,then sum() counts the TRUEs. 
    # Divide by timeLen to calculate a proportion
    plist<-c( plist,(sum(as.integer(diff(times))<i ))/timLen)
  }
  return(plist)
  
}


# Here we examine interarrival times of tweets

climate<-tweet("#climate",500)

# Step 1: Find the experimental probabilities 
# What is the probability that the next tweet will arrive within 10 seconds? 

attach(climate)

View(climate)

interarrival<-diff(created)

hist(as.numeric(interarrival))

# Examining experimental probabilities
# What is the probability that the next tweet will arrive within 10 seconds? 

length(created)

sum(as.integer(interarrival)<10)


# Experimental probability: 114/499 = 0.2285


# Step 2 : Examine theoretical probability using : P(X<t)= 1- e^(-lambda*t) formula
# where lambda is 1/mu, mu is the mean of the inter-arrival times

mean(interarrival)
# mu = 37.97, therefore lambda is 1/37.97

# P(T<10)= 1- e^(-10/37.97)= 0.2315

# Comment: We get very close estimation of probability 


# Now xploring cumulative distributions  

plot(ArrivalProbability(interarrival,10,260))

# Which topic is more popular now? 
# iphone, galaxy
# taxreform , healthinsurance
# guncontrol, deathpenalty 
# R, Python
# Toyota, Honda

# Conducting poisson.test confidence interval calculations 

poisson.test(101,500)$conf.int

# Chapter Challenge: Write a function that takes two search strings as arguments and that returns
# the results of a Poission rate ratio test on the arrival rates of tweets on the two topics. 
# Your function should first run the necessary Twitter searches, then sort the twets by ascending time 
# of arrival and calculate the two vectors of time differentials. Use the mean of one of these vectors 
# as the basis for comparision and for each vector, count how many events are at or below the mean. 
# Use the information and the numbers of tweets requested to run the poission.test()rate comparision. 








### Text mining in tweets 

# We would need the following packages: "tm"(text mining ), "wordcloud", 

# Step 1: Get a list of tweets using searchTweeter() 

tax<-searchTwitter("tax",500)

# alternative search with more arguments 

tax<-searchTwitter("#taxreform",n=500,lang="en",resultType="recent")

class(tax)
head(tax)



# Step 2: Extract the text part/section from the list

taxtext<-sapply(tax,function(x) x$getText())

Encoding(taxtext)  <- "ASCII"
 
head(taxtext)


# Step 3: Getting core part of the text /Corpus using tm package commands

taxcorpus<-Corpus(VectorSource(taxtext))
head(taxcorpus)

# to check the results we would need to use inspect command

inspect(taxcorpus[1:3])


#Step 4: Cleaning text : remove punctuation 

taxclean1<-tm_map(taxcorpus,removePunctuation)

inspect(taxclean1[1])


#Step 5 : Cleaning text: convert to lowercase 

taxclean2<-tm_map(taxclean1,content_transformer(tolower))

inspect(taxclean2[1:3])
#Step 6 : Remove stopwords 

taxclean3<-tm_map(taxclean2,removeWords,stopwords("english"))

inspect(taxclean3[1:3])

#Step 7  Remove numbers

taxclean4<-tm_map(taxclean3,removeNumbers)
inspect(taxclean4[1:3])

#Step 8  Remove white space

taxclean5<-tm_map(taxclean4,stripWhitespace)

inspect(taxclean5[1:3])

# Step 9 This is optional to remove words, if necessary

taxclean6<-tm_map(taxclean5,removeWords, c("tax","gop"))

inspect(taxclean6)

# Now we can create word cloud

wordcloud(taxclean6)


# We can format wordcloud by using random.order=F command

wordcloud(taxclean6,random.order=F)

# Change scale of words by defining maximum and then minimum size

wordcloud(taxclean6,random.order=F,scale=c(2,0.5))


# Add color or rainbow effect

wordcloud(taxclean6,random.order=F,scale=c(3,0.5),color="red")

wordcloud(taxclean6,random.order=F,scale=c(3,0.5),color=rainbow(5))

# Restrict the number of words (maximum number of words)


wordcloud(taxclean6,random.order=F,scale=c(3,0.5),color=rainbow(5), max.words = 40)

wordcloud(taxclean6,min.freq=4)


#### Creating a function to draw the wordcloud 

wordart<-function(x)
  
{ 
  text<-searchTwitter(x,n=500,lang="en",resultType = "recent")
  
  text<-sapply(text,function(x) x$getText())
  Encoding(text)<-"ASCII"

  goodcorp<-Corpus(VectorSource(text))

  gt<-tm_map(goodcorp,removePunctuation)

  gt<-tm_map(gt,tolower)

  gt<-tm_map(gt,removeNumbers)

  gt<-tm_map(gt,removeWords,stopwords("english"))

  gt<-tm_map(gt,stripWhitespace)

  gt<-tm_map(gt,removeWords,c("x","#x"))

  wordcloud(gt,scale=c(3,0.5),random.order=F,color=rainbow(5))
}

wordart("#missuniverse") 


wordart("healthinsurance")

library(stringr)


# Sentiment Analyses 
# Given a character vector we can combine values

a<-c("Hello","World","Name","is","R")

b<-paste(a,collapse = " ")

b

# and we can split a statement into separate words

c<-str_split(b,pattern="\\s+")

c

words<-str_split(taxclean6,pattern = "\\s+") 

#converting list into character vector 

words<-unlist(words)

str(words)

# save positive words
#http://ptrckprry.com/course/ssd/data/positive-words.txt

#save negative words 
#http://ptrckprry.com/course/ssd/data/negative-words.txt

getwd()

# We match positive words with the tweeter clean words 

match(words,pos)
ls()

getwd()
match(words,neg)


sum(!is.na(match(words,positive-words)))

sum(!is.na(match(words,negative-words)))

sum(!is.na(match(words,file.choose())))

a<-c("abolish","abominable")
 
match(a,file.choose())

pos<-read.table("pos.txt",header=T,sep="\t")
pos

neg<-read.table("neg.txt",header=T,sep="\t")
neg

match(words,neg)
match(words(pos))
match(words,pos)
sum(!is.na(match(words,neg)))
sum(!is.na(match(words,pos)))
head(pos)
pos
match("humor",pos)
str(pos)
str(words)
head(pos)
pos<-as.character(pos$X............................................................................)
pos
match(words,pos)
sum(!is.na(match(words,pos)))

neg<-as.character(neg$X..............................................................................)
sum(!is.na(match(words,neg)))
