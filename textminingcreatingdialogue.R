# work directory
wd <- '~/Text mining the great unread/TextAnalysisWithR' 
setwd(wd)
getwd()
### set data directory
dd <-paste(wd,'/BLE',sep="")
library(tm)
# import plain text files, create corpus
books.cor  <- Corpus(DirSource(dd, encoding = "UTF-8"), readerControl = list(language = "lat"))
names(books.cor) <- gsub("\\..*","",names(books.cor))# fjerne endelse i filnavn
filenames <- names(books.cor)
#preprocessing - remove numbers 
books.cor <- tm_map(books.cor, removeNumbers)
# search parameters for determining comments, replies and comments with replies
result.mat <- matrix(rep(0,length(books.cor)*26), ncol = 26)
for (i in 1:length(books.cor)){
  tmp <- books.cor[[i]]$content
  result.mat[i,1] <- length(which(tmp == " Like"))
  result.mat[i,2] <- length(which(tmp == " Like  Like"))
  result.mat[i,3] <- length(which(tmp == " Like  Likes"))
  result.mat[i,4] <- length(which(tmp == " Like Reply"))
  result.mat[i,5] <- length(which(tmp == " Like  Like Reply"))
  result.mat[i,6] <- length(which(tmp == " Like  Likes Reply"))
  #doubel spaces
  result.mat[i,7] <- length(which(tmp == "  Like"))
  result.mat[i,8] <- length(which(tmp == "  Like  Like"))
  result.mat[i,9] <- length(which(tmp == "  Like  Likes"))
  result.mat[i,10] <- length(which(tmp == "  Like Reply"))
  result.mat[i,11] <- length(which(tmp == "  Like  Like Reply"))
  result.mat[i,12] <- length(which(tmp == "  Like  Likes Reply"))
  #Regulate for own participation in course
  result.mat[i,13] <- length(which(tmp == " You like this commentLiked  Likes"))
  result.mat[i,14] <- length(which(tmp == " You like this commentLiked  Like"))
  result.mat[i,15] <- length(which(tmp == " You like this commentLiked  Like Reply"))
  result.mat[i,16] <- length(which(tmp == " You like this commentLiked  Likes Reply"))
  result.mat[i,17] <- length(which(tmp == " Like Reply Edit"))
  result.mat[i,18] <- length(which(tmp == " Like  Like Reply Edit"))
  result.mat[i,19] <- length(which(tmp == " Like  Likes Reply Edit"))
  result.mat[i,20] <- length(which(tmp == " Like  Like Edit"))
  result.mat[i,21] <- length(which(tmp == " Like  Likes Edit"))
  result.mat[i,22] <- length(which(tmp == " Like Edit"))
  #reply and following
  result.mat[i,23] <- length(which(tmp == "Follow"))
  result.mat[i,24] <- length(which(tmp == "Following"))
  result.mat[i,25] <- length(which(tmp == "Reply"))
  result.mat[i,26] <- length(which(tmp == "This comment has been removed by a FutureLearn moderator"))
}

#Construct variables for comments, comment with replies and replies
kommentarer <- rowSums(result.mat[,1:22])+result.mat[,26]
follow <- rowSums(result.mat[,13:24])
svar <- rowSums(result.mat[,c(1,2,3,7,8,9,13,14,15,16,20,21,22)])-result.mat[,25]
traademedsvar <- result.mat[,25]
traadeudensvar <- rowSums(result.mat[,c(4, 5, 6, 10, 11, 12, 15, 16, 17, 18, 19, 25)])
#Check that variables are correct
mean(svar/kommentarer*100)#23,56%
mean(traadeudensvar/kommentarer*100)#76,44%
#Percentage of replies of all comments
mean(traademedsvar/traadeudensvar*100)#18,34%
##average replies per comment with reply
mean(svar/traademedsvar)#1,8 svar/traad med svar
mean(svar/traadeudensvar)#0,33 svar/samlet kommentarer

#Plot of comments, replies and comments with relies
library(ggplot2)
library(reshape2)
sektioner <- 1:53
df3 <- data.frame(kommentarer, svar, traademedsvar, sektioner)
colnames(df3)<- c("Comments", "Replies", "Comments with Replies", "sektioner")
#melt columns
newdf3 <- melt(df3, 'sektioner')
#plot with more variables 
ggplot(newdf3, aes(x=sektioner, y=value,group=variable,color=variable))+geom_line()+xlab("Comment sections")+ylab("Count")+ggtitle("Comment and Reply Distribution")
##################################################################+stat_smooth()


#Finding each individual commenters names 
totpos.l <- list()
navne.l <- list()
total.l <- list()
for(j in 1:length(books.cor)){
  kommentar.position.v <- grep(c("Follow|Following|Kim Haagen Mathiesen"), books.cor[[j]]$content)#tag positioner
  ##-1 gives all names
  for(i in 1:length(kommentar.position.v)){
    kommentar.position.v[i] <- kommentar.position.v[i]-1
    navne <- books.cor[[j]]$content[kommentar.position.v] 
  }
  totpos.l[[j]] <- kommentar.position.v #all positions
  navne.l[[j]] <- navne #all names 
}  



#Visualizing individuals number of comments
navne.t <- table(unlist(navne.l))
sortednavne.t <- sort(navne.t, decreasing = TRUE)
sortednavne.df <- as.data.frame(sortednavne.t)
names(sortednavne.df)[1] <- paste("pernr")
person <- 1:907
ny <-cbind(person, sortednavne.df)

#Plot of individual comment distribution
ggplot(ny, aes(x=person, y=Freq))+geom_point()





#finding top 10 names
sortednavne.df
#take name 1-10
#Organize names in matrix, seach each name in the matrix row below, names are anonymous
toptinavn.mat <- matrix(rep(0,length(navne.l)*10), ncol = 10)
for (i in 1:length(navne.l)){
  tmp2 <- navne.l[[i]]
  toptinavn.mat[i,1] <- length(which(tmp2 == "name1"))
  toptinavn.mat[i,2] <- length(which(tmp2 == "name2"))
  toptinavn.mat[i,3] <- length(which(tmp2 == "name3"))
  toptinavn.mat[i,4] <- length(which(tmp2 == "name4"))
  toptinavn.mat[i,5] <- length(which(tmp2 == "name5"))
  toptinavn.mat[i,6] <- length(which(tmp2 == "name6"))
  toptinavn.mat[i,7] <- length(which(tmp2 == "name7"))
  toptinavn.mat[i,8] <- length(which(tmp2 == "name8"))
  toptinavn.mat[i,9] <- length(which(tmp2 == "name9"))
  toptinavn.mat[i,10] <- length(which(tmp2 == "name10"))
}

#plot of top 10 commenters in one plot
ggplot(newdf, aes(x=sektioner, y=value,group=variable,color=variable))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Top 10 commenters")

#Plot of top 10 comments, one plot for each in same frame
sektioner <- 1:53
df <- as.data.frame(toptinavn.mat)
colnames(df)<- c("Person1","Person2","Person3","Person4","Person5","Person6","Person7","Person8","Person9","Person10")
dfmedsek <- cbind(df, sektioner)
newdf <- melt(dfmedsek, 'sektioner')
p1 <- ggplot(dfmedsek, aes(x=sektioner, y=Person1))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 1")+labs(fill="dose")
p2 <- ggplot(dfmedsek, aes(x=sektioner, y=Person2))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 2")+labs(fill="dose")
p3 <- ggplot(dfmedsek, aes(x=sektioner, y=Person3))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 3")+labs(fill="dose")
p4 <- ggplot(dfmedsek, aes(x=sektioner, y=Person4))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 4")+labs(fill="dose")
p5 <- ggplot(dfmedsek, aes(x=sektioner, y=Person5))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 5")+labs(fill="dose")
p6 <- ggplot(dfmedsek, aes(x=sektioner, y=Person6))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 6")+labs(fill="dose")
p7 <- ggplot(dfmedsek, aes(x=sektioner, y=Person7))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 7")+labs(fill="dose")
p8 <- ggplot(dfmedsek, aes(x=sektioner, y=Person8))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 8")+labs(fill="dose")
p9 <- ggplot(dfmedsek, aes(x=sektioner, y=Person9))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 9")+labs(fill="dose")
p10 <- ggplot(dfmedsek, aes(x=sektioner, y=Person10))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Person 10")+labs(fill="dose")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, ncol=2, top="Comment Pattern for Top 10 Commenters")


#People who commented 16 times, same procedure as above 
#find person
which(sortednavne.t == 16)
#arranging persons in matrix
sekstenkom.mat <- matrix(rep(0,length(navne.l)*10), ncol = 10)
for (i in 1:length(navne.l)){
  tmp4 <- navne.l[[i]]
  sekstenkom.mat[i,1] <- length(which(tmp4 == "name1"))
  sekstenkom.mat[i,2] <- length(which(tmp4 == "name2"))
  sekstenkom.mat[i,3] <- length(which(tmp4 == "name3"))
  sekstenkom.mat[i,4] <- length(which(tmp4 == "name4"))
  sekstenkom.mat[i,5] <- length(which(tmp4 == "name5"))
  sekstenkom.mat[i,6] <- length(which(tmp4 == "name6"))
  sekstenkom.mat[i,7] <- length(which(tmp4 == "name7"))
  sekstenkom.mat[i,8] <- length(which(tmp4 == "name8"))
  sekstenkom.mat[i,9] <- length(which(tmp4 == "name9"))
  sekstenkom.mat[i,10] <- length(which(tmp4 == "name10"))
}
sektioner <- 1:53
df2 <- as.data.frame(sekstenkom.mat)
colnames(df2)<- c("Person1","Person2","Person3","Person4","Person5","Person6","Person7","Person8","Person9","Person10")
dfmedsek2 <- cbind(df2, sektioner)
newdf2 <- melt(dfmedsek2, 'sektioner')
ggplot(newdf2, aes(x=sektioner, y=value,group=variable,color=variable))+geom_line()+xlab("Comment Sections")+ylab("Comments")+ggtitle("Comment Pattern for Participants Commenting 16 Times")+labs(fill="dose")
