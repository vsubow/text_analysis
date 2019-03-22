############################################################
#                 Text Mining using R                      #
#                     Case Study # 1                       #
#                 Scooter Use in DC                        #  
############################################################


############################################################
####            Setting up your analysis                ####
############################################################

install.packages("RCurl") #read data from github repository
library(RCurl)
install.packages("tm") # most common text mining package in R 
library(tm) 
install.packages("tidyverse") # string of packages to manipulate & analyze data 
library(tidyverse)
install.packages("wordcloud") # word-cloud generator
library(wordcloud)
install.packages("RColorBrewer") # color palettes
library(RColorBrewer)
install.packages("Hmisc") #statistics package, used for e.g. describe() function
library(Hmisc)


##### Exploratory questions we want to address:

#Why are people using scooters? 
#Does everyone like the scooters? 

############################################################
####           Download & explore data set              ####
############################################################

### Download the dataset from github 

url_data <- getURL("https://raw.githubusercontent.com/vsubow/text_analysis/master/scoot_along_responses.csv") 
scooter_data <- read.csv(text = url_data) #read data into data frame 

### View the data set we just read in 
View(scooter_data)

### Insepct the "head" and "tail" of the data-set 
head(scooter_data)
tail(scooter_data)

### Now let's have a look at our variables and see some summary statistics
str(scooter_data) # The str() function shows the structure of your dataset and details the type of variables that it contains
summary(scooter_data) # The summary() function provides for each variable in your dataset the minimum, mean, maximum and quartiles

### Continue to explore data set using visualizations  

#How many of the respondents have used a scooter? 
plot(scooter_data$Have.you.used.a.shared.scooter.yet.)

# You can get to specific questions by using the "$" (dollar sign))
plot(scooter_data$Do.you.think.you.ll.use.a.shared.scooter.)

# Let's look at the distribution of one more questions 
plot(scooter_data$Where.would.you.ride.these.scooters.)


############################################################
####                     Text Mining                    ####
############################################################

# The question "why would you use the scooter" is a bit more tricky to quantify. 
# Just browsing through the comments, what do you think is the most cited reason?
# It is difficult to get an idea because there are many reasons listed multiple times. 

# OK! Now we are good to go to extract those reasons from the free text format. 

### Creat a "corpus" of the answers 

# Extract the comments from the column "Why would you use the scooters"
commentCorpus = VCorpus(VectorSource(scooter_data$Why.would.you.use.the.scooters.))

# Examine corpus 
inspect(commentCorpus) #each of the comments is now a "document" 

# Look at a specific document (i.e. comment)
commentCorpus[[45]]$content #print the content of the comment no 45 

# Using the "tm_map" function, we can apply the necessary transformations to all our documents: 

# Strip white-space 
commentCorpus <- tm_map(commentCorpus, stripWhitespace) 
commentCorpus[[45]]$content #check for the results 

#set words to lower 
commentCorpus <- tm_map(commentCorpus, content_transformer(tolower))
commentCorpus[[45]]$content

# Remove punctuation 
commentCorpus <- tm_map(commentCorpus, removePunctuation)
commentCorpus[[45]]$content

# Set all words to lower case
commentCorpus <- tm_map(commentCorpus, content_transformer(tolower))
commentCorpus[[45]]$content

#remove stop words 
commentCorpus <- tm_map(commentCorpus, removeWords, stopwords("english"))
commentCorpus[[45]]$content

#look at list of stopwords removed from corpus 
stopwords()

### Once we have pre-processed our infromation, create a docuemnt-term-matrix 

# Extract words from the comments
dtm = DocumentTermMatrix(commentCorpus) #creates matrix with documents as rows and words as columns
dtm 

# What is the document-term-matrix (DTM)?
?DocumentTermMatrix

#create a table displaying our words & their counts per comment (term-frequency)
View(as.matrix(dtm))

#inspect dtm 
inspect(dtm)

#### The document term matrix can be used for input to further statistical analysis. 

### Explore newly created table 

# finding most frequent words/ tokens that are mentioned at least 5 times 
findFreqTerms((dtm),5) #find most frequent terms that are used 5x at least

#find associations between terms 
findAssocs(dtm, "cool", 0.8) # find correlations between the term "cool" and other terms with r >= 0.8
findAssocs(dtm, "convenient", 0.4) # find correlations between the term "convenient" and other terms with r >= 0.8

############################################################
####            Creating a word-cloud                   ####
############################################################

# What is the most cited reason for using the scooters? 

# Term - Document - Matrix (transposed dtm)
tdm = TermDocumentMatrix((commentCorpus))
View(as.matrix(tdm))

# view the top words
m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 20)

# word cloud???
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

######### Connect your findings back to your initial questions and see how you can answer them.


############### End of script ##################################################
