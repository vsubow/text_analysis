
############################################################
#                 Text Mining using R                      #
#                     Case Study # 2                       #
#                 Analyzing Instagram revies               #  
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
install.packages("Hmisc") #statistics package, used for e.g. describe() function
library(Hmisc)
install.packages("tm") # text mining pakage 
library(tm)
install.packages("tidyverse") # selection of packages to make data transformation easy 
library(tidyverse)
install.packages("tidytext")
library("tidytext")

##### Exploratory questions we want to address:

#What are people saying about my App? 
#What is the average rating of my App? 
#Is the rating for my App going up or down? 

#Before we are going to answer our questions, we are going to examine our data-set according to 
#most common practices. 


############################################################
####           Download & explore data set              ####
############################################################

### Download the dataset from github 
url_data <- getURL("https://raw.githubusercontent.com/vsubow/text_analysis/master/instagram_reviews.csv") 
my_insta_data <- read.csv(text = url_data) #read data into data frame 

# read in the csv file which contains the data set (DELETE once on github)
my_insta_data <- read.csv(file = "C:\\Users\\Valentina\\Documents\\wids_uva\\instagram_reviews.csv")

# Inspect data 
my_insta_data

### View the data set we just read in 
View(my_insta_data)

### Insepct the "head" and "tail" of the data-set 
head(my_insta_data)
tail(my_insta_data)

# Explore data via summary and descriptive functions 
summary(my_insta_data)
describe(my_insta_data)

### Continue to explore data set using visualizations  

#frequency distriubtion of reviews 
hist(my_insta_data$star_rating)

#distribution of app versions 
plot(my_insta_data$version)

#distribution of dates
plot(my_insta_data$review_date)

############################################################
####                     Text Mining                    ####
############################################################

###  Pre-processing corpus 

# Extract the comments from the column "Why would you use the scooters"
reviewCorpus = VCorpus(VectorSource(my_insta_data$review))

# Examine corpus 
inspect(reviewCorpus) #each of the comments is now a "document" 

# Look at a specific document (i.e. review)
reviewCorpus[[45]]$content

# Using the "tm_map" function, we can apply transformations to all our documents 
reviewCorpus <- tm_map(reviewCorpus, stripWhitespace)
reviewCorpus[[45]]$content

# Set words to lower 
reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))
reviewCorpus[[45]]$content

# Remove punctuation 
reviewCorpus <- tm_map(reviewCorpus, removePunctuation)
reviewCorpus[[45]]$content

# Remove stopwords 
reviewCorpus <- tm_map(reviewCorpus, content_transformer(tolower))
reviewCorpus[[45]]$content

# Remove stop words 
reviewCorpus <- tm_map(reviewCorpus, removeWords, stopwords("english"))

# Example 
reviewCorpus[[45]]$content

######## From unstructured to structured data 

# Extract words from the comments
dtm = DocumentTermMatrix(reviewCorpus)

#### The document term matrix can be used for input to further statistical analysis. 

### Explore newly created table 
dtm
View(as.matrix(dtm))

### Explore newly created table 

# finding most frequent words/ tokens that are mentioned at least 5 times 
findFreqTerms((dtm),5) #find most frequent terms that are used 5x at least

#find associations between terms 
findAssocs(dtm, "instagram", 0.3) # find correlations between the term "cool" and other terms with r >= 0.8
findAssocs(dtm, "slow", 0.4) # find correlations between the term "convenient" and other terms with r >= 0.8


############################################################
####            Topic Modelling                         ####
############################################################


# Topic Modelling (Excursion)

install.packages("topicmodels")
library(topicmodels)

rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words

#set seed so that output of the model is predicable 
insta_lda <- LDA(dtm.new, k = 2) # k = 2 gives us two distinct topics 

insta_topics <- tidy(insta_lda, matrix = 'beta') #what does beta coefficient do? 
insta_topics

# need to set seed, need to find out why I have to set seed 

insta_top_terms <- insta_topics%>%
  mutate(term = reorder(term, beta))%>%
  group_by(topic)%>%
  top_n(20, beta)%>%
  ungroup()%>%
  arrange(topic, -beta) 

insta_top_terms
  
insta_top_terms%>%
  mutate(term = reorder(term, beta))%>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE)+
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

######### Connect your findings back to your initial questions and see how you can answer them.


############### End of script ##################################################

