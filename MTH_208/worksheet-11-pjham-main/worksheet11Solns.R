load("IMDB_movies.Rdata")
#Histogram
ratings <- dat['rating']
rat <- unlist(ratings)
hist(rat,main = "Histogram of ratings",col = 'white' , xlab = "Ratings")
#Boxplot
boxplot(x=rat,main = "Boxplot of Ratings",col="pink")
#Side by Side Boxplot
mrat <- dat['men_rating']
wrat <- dat['women_rating']
boxplot(c(mrat,wrat),main="Men and Women Ratings",names =c("Men Ratings","Women Ratings"),col=c("blue","pink"))
#Overlapped histograms
hist(dat$men_rating,col = "yellow",xlim = range(c(mrat,wrat)))
hist(dat$women_rating,add = TRUE, col =adjustcolor("blue",alpha.f = .50))
legend("topright",fill= c("yellow","blue"),legend = c("men","women"))
