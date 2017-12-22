#Loading the libraries

if(!require("ggplot2")) install.packages("ggplot2")
if(!require("scales")) install.packages("scales")
if(!require("gridExtra")) install.packages("gridExtra")
if(!require("caret")) install.packages("caret")
if(!require("tm")) install.packages("tm")
if(!require("cluster")) install.packages("cluster")
if(!require("HSAUR")) install.packages("HSAUR")
if(!require("factoextra")) install.packages("factoextra")
if(!require("dplyr")) install.packages("dplyr")

#Loading the data
Air = read.csv("Airplane_Crashes_and_Fatalities_Since_1908.csv")
Air$Date = as.Date(Air$Date, format = "%m/%d/%Y")
#summary(Air)
#str(Air)

Air$Survived = Air$Aboard - Air$Fatalities
#Basic EDA
dead_per_year = ggplot(Air, aes(Date, Fatalities)) +  
  geom_bar(na.rm = TRUE, stat="identity", position="identity", colour="red") + 
  scale_x_date() + xlab("Year") + ylab("Fatalities") + ggtitle("Dead Per Year")
crash_per_year = ggplot(Air, aes(Date)) +  
  geom_histogram(binwidth=1000, fill="lightblue", col="black") + 
  scale_x_date() + xlab("Year") +
  ylab("Crashes") + ggtitle("Planes crashed per year")
aboard_per_year = ggplot(Air, aes(Date, Aboard)) +  
  geom_bar(na.rm = TRUE, stat="identity", position="identity", colour="orange") + 
  scale_x_date() + xlab("Year") + ylab("Aboard") + ggtitle("People Aboard Per Year")
survived_per_year = ggplot(Air, aes(Date, Survived)) +  
  geom_bar(na.rm = TRUE, stat="identity", position="identity", colour="green") + 
  scale_x_date() + xlab("Year") + ylab("Aboard") + ggtitle("People Survived Per Year")
grid.arrange(crash_per_year, aboard_per_year, dead_per_year, survived_per_year, ncol=2)



#Text mining
Air$Fatalities[is.na(Air$Fatalities)] = 0
corpus = VCorpus(VectorSource(Air$Summary))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
dtm = DocumentTermMatrix(corpus, control = list(stopwords = c("aircraft", "plane", "crashed", "crash", "flight", "flew", "killed", "due", "resulted", "cause", "caused", "one", "two")))
#dtm
dtm = removeSparseTerms(dtm, 0.97)
#inspect(dtm[1:10, 501:510])
#str(dtm)

freq_terms = findFreqTerms(dtm,100)
print('100 most frequent terms:')
for( i in freq_terms)
  cat(i, " ")

### Remove empty documents from 
nRows = apply(dtm , 1, sum)
dtm = dtm[nRows> 0, ]

dtm_tfxidf = weightTfIdf(dtm)

#inspect(dtm_tfxidf[1:10, 501:510])

### k-means (this uses euclidean distance)
m = as.matrix(dtm_tfxidf)
rownames(m) = 1:nrow(m)

preproc = preProcess(m)
m_norm = predict(preproc, m)
cl = kmeans(m_norm, centers = 7, iter.max = 50, nstart = 10)

print('Clusters:')
table(cl$cluster)




cl$centers
cl$size

fviz_cluster(cl, data = m_norm, geom = "text", show.clust.cent = FALSE, repel = TRUE, labelsize = 20) +
  theme(legend.position = "none") +
  labs(title = "", x = "", y = "")

clusplot(m_norm, cl$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

### show clusters using the first 2 principal components
plot(prcomp(m_norm)$x, col=cl$cl)
legend("topleft", inset = .05, title = "Cluster Colors",legend= c(1:7)
       ,fill = c(1:7), horiz=TRUE)
#PCA
X.pca = prcomp(m_norm, center = TRUE, scale. = TRUE)
print(X.pca$rotation)
print(summary(X.pca))


#PCA Eigenvalue plot
if(!require("factoextra")) install.packages("factoextra")
fviz_screeplot(X.pca,choice='eigenvalue')

#PCA plot
fviz_pca_var(X.pca)



freq_terms_1 = findFreqTerms(dtm[cl$cluster==1,], 50)
freq_terms_2 = findFreqTerms(dtm[cl$cluster==2,], 50)
freq_terms_3 = findFreqTerms(dtm[cl$cluster==3,], 50)
freq_terms_4 = findFreqTerms(dtm[cl$cluster==4,], 50)
freq_terms_5 = findFreqTerms(dtm[cl$cluster==5,], 50)
freq_terms_6 = findFreqTerms(dtm[cl$cluster==6,], 50)
freq_terms_7 = findFreqTerms(dtm[cl$cluster==7,], 50)
print('50 most frequnt terms in cluster 1:')
for( i in freq_terms_1)
  cat(i, " ")
print('50 most frequnt terms in cluster 2:')
for( i in freq_terms_2)
  cat(i, " ")
print('50 most frequnt terms in cluster 3:')
for( i in freq_terms_3)
  cat(i, " ")
print('50 most frequnt terms in cluster 4:')
for( i in freq_terms_4)
  cat(i, " ")
print('50 most frequnt terms in cluster 5:')
for( i in freq_terms_5)
  cat(i, " ")
print('50 most frequnt terms in cluster 6:')
for( i in freq_terms_6)
  cat(i, " ")

print('Fatalities in cluster 1:')
sum(Air$Fatalities[which(cl$cluster==1)])
print('Fatalities in cluster 2:')
sum(Air$Fatalities[which(cl$cluster==2)])
print('Fatalities in cluster 3:')
sum(Air$Fatalities[which(cl$cluster==3)])
print('Fatalities in cluster 4:')
sum(Air$Fatalities[which(cl$cluster==4)])
print('Fatalities in cluster 5:')
sum(Air$Fatalities[which(cl$cluster==5)])
print('Fatalities in cluster 6:')
sum(Air$Fatalities[which(cl$cluster==6)])
print('Fatalities in cluster 7:')
sum(Air$Fatalities[which(cl$cluster==7)])


#To complete this first semantic analysis, we can look at the most frequent terms, and their correlation with other terms.
#We begin by plotting the 20 most frequent terms. All of them are obviously included in the above cluster analysis, 
#but here we get a sense of their frequency relatively to each others.

### Order terms by frequency
freq <- colSums(as.matrix(dtm))
freq <- 
  freq %>%
  data.frame(term = names(freq), frequency = freq) %>%
  select(term, frequency) %>%
  arrange(desc(frequency)) 

### Plot most frequent terms
ggplot(freq[1:20, ], aes(x = frequency, y = reorder(term, frequency))) + 
  geom_point(colour = "#2b83ba") + 
  geom_segment(aes(xend = 0, yend = term), size = 1, colour = "#2b83ba") +
  geom_text(aes(label = term, vjust = "middle", hjust = "left"), nudge_x = 10, size = 3.5) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(colour = "#f7f7f7"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.y = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks.y = element_blank()) +
  labs(title = "Occurences of top 20 most frequent terms")




#Some hypothesis we can do on the top 5 terms:
#Pilot: is it only because this is a generic term, or indicating that pilot is the cause ?
#Approach: this suggest that accidents often happen in the runway approach phase
#Engine: probably one of the most common causes
#Runway: relates to the approach phase
#Failure: this is too generic to draw conclusions, we'll some more context
#To add more context to the list, we have to look at which terms are most correlated with these 20 frequent terms. For each of them, let's plot the top 5 terms that have a correlation higher than 0.17.



### Terms correlation
assocs <- findAssocs(dtm, as.character(freq[1:20, 1]), corlimit = 0.17)
print(assocs)


#This is quite enlightening. Let's look at some of the terms associations:
#Pilot: 'error' is one of the most correlated words, which is consistent with the fact that 60% of crashes are due to pilot errors
#Approach: the accidents in final approach phase seem to be often caused by confusion in reading instruments and low visibility ('ils', 'instruments', 'visual', 'missed')
#Engine seems related to shutdown of engine and/or loss of power
#Runway is associated with 'short', 'end' and 'overran', that could be as well in takeoff or landing phases
#Failure: we have more context here, suggesting that it can be pilot, maintainance, procedure or system failures
#Landing: this shows that it is not necessarily about the standard landing phase, but rather about landing gears, or emergency landings
#Weather and Conditions suggest that visibility is one of the most important crashes factors in bad weather





# library(HSAUR)
# 
# dissE <- daisy(m_norm) 
# dE2   <- dissE^2
# sk2   <- silhouette(cl$cl, dE2)
# plot(sk2)


