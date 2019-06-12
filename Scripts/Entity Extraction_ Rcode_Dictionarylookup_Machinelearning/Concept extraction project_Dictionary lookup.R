#######-------------------Entity Extraction based on Dictionary based approach-----------########

##Installing the required packages
install.packages("dplyr")
library(dplyr)
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")

for (i in c('SnowballC','slam','tm','RWeka','Matrix')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

if(!require("rtweet")) install.packages("rtweet"); library("rtweet")
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if (!require("udpipe")) install.packages("udpipe", quiet=TRUE) ; require("udpipe")
library(lattice)

##-------------------Extracting the data from twitter for FOX News and CNN----------------#######

#Providing the key tokens for authentication onto to twitter and using rtweet for extracting the information
consumer_key <- "OD0A8Z1EY65la2l40GFHGoHmN"
consumer_secret <- "nHVQp9G7GIcIdq7bnZdvJW1Wy6qP11xO60BA0HXbbdslZO88ud"
access_token <- "569483185-NiO006TdngN8jy4dy2GOiiunVL6zwFLtmlrsxJtG"
access_secret <- "37x3kUAyQOTB2rHR9HZWSdmunvnqPuuWEBVGZkrQEAGzj"

appname <- "SMA"
twitter_token = create_token(app = appname,consumer_key = consumer_key,consumer_secret = consumer_secret,
                             access_token = access_token,access_secret = access_secret)


#Extracting the follower user details of CNN

followers <- get_followers("CNN")

user_details <- lookup_users(followers$user_id)


#Extracting the follower user details of Fox News
followers1 <- get_followers("FoxNews", n=90000)

user_details1 <- lookup_users(followers1$user_id)


####------------------------------------------Data Preprocessing----------------------#####

##CNN data Preprocessing
raw_data = user_details[c("user_id","description","friends_count","location","country","account_lang","lang")]



CNN_followers = raw_data %>% filter(friends_count > 250, !is.na(description),description != "",account_lang == "en",lang == "en")


#Reading the CNN data saved as a CSV file from twitter
CNNdetails <- read.csv("CNN_followers.txt")

#Extract only the description column which contains all values for entity recognition
CNNDescription <- CNNdetails$description
CNNDescription <- as.character(CNNDescription)

#Checking the structure of the description
str(CNNDescription)

##Convert the description details to a corpus to allow pre-processing

CNNCorpus <- VCorpus(VectorSource(CNNDescription))

CNNCorpus[[1]]

as.character(CNNCorpus[[1]])

#Initiate Data Pre-processing by removing punctuation,whitespace,numbers,numbers and stopwords

CNNCorpus <- tm_map(CNNCorpus, removePunctuation)

CNNCorpus <- tm_map(CNNCorpus, stripWhitespace)

CNNCorpus <- tm_map(CNNCorpus,content_transformer(tolower))

CNNCorpus <- tm_map(CNNCorpus, removeNumbers)

CNNCorpus = tm_map(CNNCorpus,removeWords,stopwords('english'))

#Unlist the corpus for annotation and tokenisation

CNNfinal <- unlist(sapply(CNNCorpus,"content"))

#Annotation of the cleaned CNN data to do tokenisation, POS tagging and some better insights
ud_model_CNN <- udpipe_download_model(language = "english")
ud_model_CNN <- udpipe_load_model(ud_model_CNN$file_model)

CNN_details <- udpipe_annotate(ud_model_CNN, x = CNNfinal)

CNN_details <- as.data.frame(CNN_details)

View(CNN_details)

##Data Preprocessing for FoxNews.
raw_data1 = user_details1[c("user_id","description","friends_count","location","country","account_lang","lang")]



fox_followers = raw_data1 %>% filter(friends_count > 250, !is.na(description),description != "",account_lang == "en",lang == "en")

#reading the saved fox news twitter data
fox_news = read.csv("fox_followers.csv")

#Extract only the description column which contains all values for entity recognition
fox_description = fox_news$description

fox_description = as.character(fox_description)

#Checking the structure of fox description column
str(fox_description)

#Convert the description details to a corpus to allow pre-processing

FoxCorpus <- VCorpus(VectorSource(fox_description))

FoxCorpus[[1]]

as.character(FoxCorpus[[1]])

#Initiate Data Pre-processing by removing punctuation,whitespace,numbers,numbers and stopwords

FoxCorpus <- tm_map(FoxCorpus, removePunctuation)

FoxCorpus <- tm_map(FoxCorpus, stripWhitespace)
FoxCorpus <- tm_map(FoxCorpus,content_transformer(tolower))

FoxCorpus <- tm_map(FoxCorpus, removeNumbers)

FoxCorpus = tm_map(FoxCorpus,removeWords,stopwords('english'))

#Unlist the Corpus fox file
Foxfinal <- unlist(sapply(FoxCorpus,"content"))


#Annotation and tokenisation using udpipe package for Fox news description

ud_model_fox <- udpipe_download_model(language = "english")
ud_model_fox <- udpipe_load_model(ud_model_fox$file_model)

fox_details <- udpipe_annotate(ud_model_fox, x = Foxfinal)

fox_details <- as.data.frame(fox_details)

View(fox_details)

#Saving the tokenised foxfile and CNNfile for dictionary lookup
write.csv(CNN_details,file="CNN_details.csv")
write.csv(fox_details,file="fox_details.csv")


##-----------------------Dictionary Lookup approach for Entity Extraction-------------####

###Dictionary lookup for the data in CNN for the different entities

#Reading the Entity dictionary file
dictionaryEntity = read.csv("Dictionary_Entity.csv")

names(dictionaryEntity)[1] = 'Value'

#Changing the type to character and changing the words in dictionary to lower case
dictionaryEntity$Value = as.character(dictionaryEntity$Value)
dictionaryEntity$Value = tolower(dictionaryEntity$Value)
dictionaryEntity$Entity = as.character(dictionaryEntity$Entity)

#Checking the Structure of the dataframes
str(dictionaryjob)
str(CNN_details)


#Mapping the values from the Dictionary into a new column for the entity types based on the tokens

CNN_details$Entity <- plyr::mapvalues(CNN_details$token, from = dictionaryEntity$Value, to = dictionaryEntity$Entity)

#Extracting the entities Profession which has already matched
mappedvalue_CNN_Profession = CNN_details[which(CNN_details$Entity == "Profession"),]
mappedvalue_CNN_Hobbies = CNN_details[which(CNN_details$Entity == "Hobbies"),]
mappedvalue_CNN_Personality = CNN_details[which(CNN_details$Entity == "Personality"),]
mappedvalue_CNN_Religion = CNN_details[which(CNN_details$Entity == "Religion"),]
mappedvalue_CNN_Organisation = CNN_details[which(CNN_details$Entity == "Organization"),]

#Saving all the matched entities into a seperate data set for CNN
Entity_CNN = CNN_details[which(CNN_details$Entity == "Organization" | CNN_details$Entity == "Religion" |
                                 CNN_details$Entity == "Personality" | CNN_details$Entity == "Hobbies" |
                                 CNN_details$Entity == "Profession"),]

##Dictionary lookup for data in Foxnews

#Mapping the values from the Dictionary into a new column for the entity types based on the tokens for Fox news

fox_details$Entity <- plyr::mapvalues(fox_details$token, from = dictionaryEntity$Value, to = dictionaryEntity$Entity)

#Extracting the entities Profession which has already matched
mappedvalue_fox_Profession = fox_details[which(fox_details$Entity == "Profession"),]
mappedvalue_fox_Hobbies = fox_details[which(fox_details$Entity == "Hobbies"),]
mappedvalue_fox_Personality = fox_details[which(fox_details$Entity == "Personality"),]
mappedvalue_fox_Religion = fox_details[which(fox_details$Entity == "Religion"),]
mappedvalue_fox_Organisation = fox_details[which(fox_details$Entity == "Organization"),]

#Saving all the matched entities into a seperate data set for Fox news
Entity_fox = fox_details[which(fox_details$Entity == "Organization" | fox_details$Entity == "Religion" |
                                 fox_details$Entity == "Personality" | fox_details$Entity == "Hobbies" |
                                 fox_details$Entity == "Profession"),]


#Saving the files with entity matched ones into CSV
Entity_CNN_Final = Entity_CNN[,c(1,4,6,8,15)]
Entity_Fox_Final = Entity_fox[,c(1,4,6,8,15)]

write.csv(Entity_CNN_Final,file = "Entity_CNN_Final.csv")
write.csv(Entity_Fox_Final,file = "Entity_Fox_Final.csv")


##---------------------------visual analysis and comparison of Fox News and CNN News--------------####


###########################Graphical Presentation Per Channel ##############################################

#Read the data
Entity_CNN_Final = read.csv("Entity_CNN_Final.csv")
Entity_Fox_Final = read.csv("Entity_Fox_Final.csv")

####  CNN entity Distribution

c <- ggplot(Entity_CNN_Final) + aes(Entity)+
  geom_bar(fill="red2",stat="count") +
  ggtitle("CNN Entity Distribution") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

##FOX CNN entity Distribution

f<- ggplot(Entity_Fox_Final) + aes(Entity)+
  geom_bar(fill="royalblue4",stat="count") +
  ggtitle("FOX Entity Distribution") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

grid.arrange(c,f,ncol=2)


########Stack plot for Entity types

CNNfreq <- as.data.frame(table(Entity_CNN_Final$country))
CNNfreq['NewsChannel'] = "CNN"
names(CNNfreq)[names(CNNfreq) == 'Var1'] <- 'Country'


FOXfreq <- as.data.frame(table(Entity_Fox_Final$country))
FOXfreq['NewsChannel'] = "FOX"
names(FOXfreq)[names(FOXfreq) == 'Var1'] <- 'Country'

names(FOXfreq) <- names(CNNfreq)
grouped <-bind_rows(CNNfreq,FOXfreq)
grouped <-grouped[with(grouped,order(-Country)), ]

ggplot(grouped, aes(fill= NewsChannel, y=Freq, x=Country,label= Freq)) + 
  geom_bar(position="dodge", stat="identity")+
  coord_flip()+
  ggtitle("Fox/CNN Entity Distribution") + 
  scale_fill_manual(values = c("red2","royalblue4"))+
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

###CNN Profession

CTopProfessions = Entity_CNN_Final[Entity_CNN_Final$Entity == "Profession",]
CFreq1 <- as.data.frame(sort(table(CTopProfessions$token)))
CFreq <- CFreq1[CFreq1$Freq >= 25,]
names(CFreq)[names(CFreq) == 'Var1'] <- 'Profession'

c <- ggplot(data= CFreq, aes(x=Profession, y=Freq)) +
  coord_flip()+
  geom_bar(fill="red2",stat="identity")+
  ggtitle("Top CNN Professions") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

###FOX Profession

FTopProfessions = Entity_Fox_Final[Entity_Fox_Final$Entity == "Profession",]
Freq1 <- as.data.frame(sort(table(FTopProfessions$token)))
Freq <- Freq1[Freq1$Freq >= 25,]
names(Freq)[names(Freq) == 'Var1'] <- 'Profession'

f <- ggplot(data= Freq, aes(x=Profession, y=Freq)) +
  coord_flip()+
  geom_bar(fill="royalblue4",stat="identity")+
  ggtitle("Top FOX Professions") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

grid.arrange(c,f,ncol=2)

###CNN Personalities

CTopPersonality = Entity_CNN_Final[Entity_CNN_Final$Entity == "Personality",]
CFreq1 <- as.data.frame(sort(table(CTopPersonality$token)))
CFreq <- CFreq1[CFreq1$Freq >= 15,]
names(CFreq)[names(CFreq) == 'Var1'] <- 'Personality'

c <- ggplot(data= CFreq, aes(x=Personality, y=Freq)) +
  coord_flip()+
  geom_bar(fill="red2",stat="identity")+
  ggtitle("Top CNN Personalities") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

###FOX Personality

FTopPersonality = Entity_Fox_Final[Entity_Fox_Final$Entity == "Personality",]
Freq1 <- as.data.frame(sort(table(FTopPersonality$token)))
Freq <- Freq1[Freq1$Freq >= 15,]
names(Freq)[names(Freq) == 'Var1'] <- 'Personality'

f <- ggplot(data= Freq, aes(x=Personality, y=Freq)) +
  coord_flip()+
  geom_bar(fill="royalblue4",stat="identity")+
  ggtitle("Top FOX Personalities") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

grid.arrange(c,f,ncol=2)


###CNN Hobbies

CTopHobbies = Entity_CNN_Final[Entity_CNN_Final$Entity == "Hobbies",]
CFreq1 <- as.data.frame(sort(table(CTopHobbies$token)))
CFreq <- CFreq1[CFreq1$Freq >= 5,]
names(CFreq)[names(CFreq) == 'Var1'] <- 'Hobbies'

c <- ggplot(data= CFreq, aes(x=Hobbies, y=Freq)) +
  coord_flip()+
  geom_bar(fill="red2",stat="identity")+
  ggtitle("Top CNN Hobbies") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

###FOX Hobbies

FTopHobbies = Entity_Fox_Final[Entity_Fox_Final$Entity == "Hobbies",]
Freq1 <- as.data.frame(sort(table(FTopHobbies$token)))
Freq <- Freq1[Freq1$Freq >= 5,]
names(Freq)[names(Freq) == 'Var1'] <- 'Hobbies'

f <- ggplot(data= Freq, aes(x=Hobbies, y=Freq)) +
  coord_flip()+
  geom_bar(fill="royalblue4",stat="identity")+
  ggtitle("Top FOX Hobbies") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

grid.arrange(c,f,ncol=2)


###CNN Organization

CTopOrganization = Entity_CNN_Final[Entity_CNN_Final$Entity == "Organization",]
CFreq1 <- as.data.frame(sort(table(CTopOrganization$token)))
CFreq <- CFreq1[CFreq1$Freq >= 5,]
names(CFreq)[names(CFreq) == 'Var1'] <- 'Organization'

c <- ggplot(data= CFreq, aes(x=Organization, y=Freq)) +
  coord_flip()+
  geom_bar(fill="red2",stat="identity")+
  ggtitle("Top CNN Organization") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

###FOX Organization

FTopOrganization = Entity_Fox_Final[Entity_Fox_Final$Entity == "Organization",]
Freq1 <- as.data.frame(sort(table(FTopOrganization$token)))
Freq <- Freq1[Freq1$Freq >= 5,]
names(Freq)[names(Freq) == 'Var1'] <- 'Organization'

f <- ggplot(data= Freq, aes(x=Organization, y=Freq)) +
  coord_flip()+
  geom_bar(fill="royalblue4",stat="identity")+
  ggtitle("Top FOX Organization") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

grid.arrange(c,f,ncol=2)


###CNN Religion

CTopReligion = Entity_CNN_Final[Entity_CNN_Final$Entity == "Religion",]
CFreq1 <- as.data.frame(sort(table(CTopReligion$token)))
CFreq <- CFreq1[CFreq1$Freq >= 1,]
names(CFreq)[names(CFreq) == 'Var1'] <- 'Religion'

c <- ggplot(data= CFreq, aes(x=Religion, y=Freq)) +
  coord_flip()+
  geom_bar(fill="red2",stat="identity")+
  ggtitle("Top CNN Religion") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

###FOX Religion

FTopReligion = Entity_Fox_Final[Entity_Fox_Final$Entity == "Religion",]
Freq1 <- as.data.frame(sort(table(FTopReligion$token)))
Freq <- Freq1[Freq1$Freq >= 1,]
names(Freq)[names(Freq) == 'Var1'] <- 'Religion'

f <- ggplot(data= Freq, aes(x=Religion, y=Freq)) +
  coord_flip()+
  geom_bar(fill="royalblue4",stat="identity")+
  ggtitle("Top FOX Religion") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face="bold"))

grid.arrange(c,f,ncol=2)

#################################################################################################







