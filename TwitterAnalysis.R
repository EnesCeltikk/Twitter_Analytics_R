#install.packages("twitteR")
library(twitteR)
#install.packages("ROAuth")
library(ROAuth)
#install.packages("tm")
library(tm)
#install.packages("tidyverse")
library(tidyverse)

library(lubridate)

#SSL
library(RCurl) ##install.packages("RCurl")
options(RCurlOptions=list(cainfo=system.file("CurlSSL","cacert.pem", package = "RCurl")))

#Twitter'dan elde eldilen kodlar(veriler)
api_key <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx-xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

#registerTwitterOAuth(twitCred)
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret) #Gelen onay sorusuna 'Yes' cevabi vermemiz gerekiyor

#Tweet atma, fotograf yukleme icin mediaPath parametresini gonderiyoruz
updateStatus("Test tweet from R")#, mediaPath = file.choose())


# ------Hashtag"lerden Veri Cekmek ------#
availableTrendLocations() #Dunya geneline ya da spesifik sehirlere ait Trend Topic'lere 
#ulasmak icin gereken ID degerlerini gosterir. 'Worldwide=1'

availableTrendLocations() %>% filter(country=="Turkey") #Turkiye'deki sehirlerin ID degerlerini getirir. 'Adana=2343678' 'Ankara=2343732'

HashTags <- getTrends(2343678)
View(HashTags)

#Spesifik hashtag hakkinda atilan tweet'leri cekme
HashTag_Tweets1 <- searchTwitter("10Kasım", n=2000) #n sayisi cekecegi tweet sayisini belirtir
dataFrame_HashTagTweets1 <- twListToDF(HashTag_Tweets1) #List formatinda olan degiskeni, daha okunabilir olmasi icin dataframe formatina ceviriyoruz 
View(dataFrame_HashTagTweets)
#----------------------------------------#


# ------ Profilden Veri Çekmek ------#
userTweets <- userTimeline('OSYMbaskanligi', n=100) #Ilk paramtre twitter profilidir
df_userTweets <- twListToDF(userTweets) #List degiskenini dataframe'e cevirme islemi
View(df_userTweets)
#------------------------------------#


# ------ Profilden Temel Bilgilerin Cekilmesi ------#
twitter_User <- getUser('trthaber')
str(twitter_User) #Degiskene ait olan verileri ve isimlerini goruyoruz
twitter_User$name #Isim
twitter_User$id #Hesabin ID degeri
twitter_User$screenName #Diger kullanicilara gorunen adi
twitter_User$created #Hesabin olusturulma tarihi
twitter_User$url #Hesabin url link'i
twitter_User$location #Hesabin hangi ulkede oldugu bilgisi
twitter_User$statusesCount #Hesabin retweet ve attigi tweet sayisi
twitter_User$followersCount #Takipci sayisi
twitter_User$favoritesCount #Favorilerine aldigi tweet sayisi
twitter_User$friendsCount #Arkadas sayisi
twitter_User$profileImageUrl #Profil resminin URL uzantisi
#--------------------------------------------------#

# ------ Profilin Enleri ------#
user_Timeline <-userTimeline('haluklevent', n=200)
dataFrame_UserTimeline <- twListToDF(user_Timeline)

dataFrame_UserTimeline %>% #Secilen kullanicinin en cok favorilere eklenen 10 tweet'i
  select(text, favoriteCount) %>%
  arrange(desc(favoriteCount)) %>%
  top_n(10) %>%
  View()

dataFrame_UserTimeline %>% #Secilen kullanicinin en cok retweet'lenen 10 tweet'i
  select(text, retweetCount) %>%
  arrange(desc(retweetCount)) %>%
  top_n(10) %>%
  View()
#------------------------------#

# ------ Tweet-Saat Dagilimlari ------#
HashTag_Tweets2 <- searchTwitter("ELAZIĞdeprem", n=2000) #Belirtilen Hashtag'e ait istatistikler
dataFrame_HashTagTweets2 <- twListToDF(HashTag_Tweets2)

hist(hour(dataFrame_HashTagTweets2$created), col="purple",
     xlab = "Saat Araligi",
     ylab = "Tweet Sayisi",
     xlim = c(0,25))

user_Timeline2 <- userTimeline('haluklevent', n=2000) #Belirtilen kullaniciya ait istatistikler
dataFrame_UserTimeline2 <- twListToDF(user_Timeline2)
hist(hour(dataFrame_UserTimeline2$created), 
     main = c(dataFrame_UserTimeline2$screenName[1],"adli kullanicinin istatistikleri"),
     col="purple",
     xlab = "Saat Araligi",
     ylab = "Tweet Sayisi",
     xlim = c(0,25))
#-------------------------------------#


# ------ Tweeet Atma Kaynaklarina Erisim ------#
user_Timeline3 <- userTimeline("trthaber", n=2000)
dataFrame_UserTimeline3 <- twListToDF(user_Timeline3)
kaynaklar <- dataFrame_UserTimeline3$statusSource
kaynaklar <- gsub("</a>","",kaynaklar)
kaynaklar <- strsplit(kaynaklar, ">")
kaynaklar <- sapply(kaynaklar, function(x) x[2])
kaynaklar_tablosu <- table(kaynaklar)
pie(kaynaklar_tablosu, radius = 1, border = 8)  
#----------------------------------------------#

#--------------------------------------------------------------#
#   ------ Hashtag'e Katilan Essiz Kullanici Sayisi ----- #    #
HashTag_Tweets3 <- searchTwitter("#datascience", n=2000)       #
dataFrame_HashTagTweets3 <- twListToDF(HashTag_Tweets3)        #
#
dataFrame_HashTagTweets3 %>% distinct(screenName) %>% count()  #
#
# ---- Hashtag'e En Cok Katilim Saglayan Kullanicilar ---- #   #
dataFrame_HashTagTweets3 %>% group_by(screenName) %>%          #  
  summarise(n=n()) %>%                                         #
  arrange(desc(n)) %>%                                         #
  top_n(10)                                                    #
#
# ------ Hashtag'te En Cok 'Favori' Alan Kullanici ------ #    #
dataFrame_HashTagTweets3 %>%                                   #
  select(text,screenName,favoriteCount) %>%                    #
  arrange(desc(favoriteCount)) %>%                             #     
  top_n(10) %>% View()                                         # 
#
# ------ Hashtag'te En Cok 'Retweet' Alan Kullanici ------ #   #
dataFrame_HashTagTweets3 %>%                                   #
  select(text,screenName,retweetCount) %>%                     #
  arrange(desc(retweetCount)) %>%                              #     
  top_n(10) %>% View()                                         # 
#
#--------------------------------------------------------------#


#   ------ Secilen Kullanicinin En Cok Kullandigi Kelimeler ----- #    
user_Timeline4 <-userTimeline('haluklevent', n=200)
dataFrame_UserTimeline4 <- twListToDF(user_Timeline4)

doc.corpus <- Corpus(VectorSource(dataFrame_UserTimeline4$text))
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation))
doc.corpus <- tm_map(doc.corpus, content_transformer(removeNumbers))

removeUrl <- function(x) gsub("http[[:alnum:]]*","",x)
myCorpus <- tm_map(doc.corpus, removeUrl)
myCorpus <- tm_map(myCorpus, stripWhitespace)
tdm <- TermDocumentMatrix(myCorpus)
findFreqTerms(tdm, lowfreq = 10)
#----------------------------------------------------------------#


