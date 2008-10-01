# R for Aquamacs stats

prod <- "Aquamacs"
check.period.days <- 3

# setwd("/Users/dr/Projects/Aquamacs/aquamacs/Statistics")

startups <- read.table("startups.txt", header=TRUE, fill=TRUE)

# 
usage_duration <- read.table("usage-duration.txt", header=TRUE, fill=TRUE)

versions <- read.table("versions.txt", header=TRUE, fill=TRUE)
versions.timeline<- read.table("version-timeline.txt", header=FALSE, fill=TRUE)

countries <- read.table("countries.txt", header=TRUE, fill=TRUE)

conversionrate <- read.table("conversionrate.txt", header=TRUE, fill=TRUE)
exposure <- read.table("user-exposure.txt", header=TRUE, fill=TRUE)


 

pdf(file="startups.pdf")
startups <- subset(startups, no.users>4)
c=c()
for(i in startups$no.startups) {
  c <- append(c,  sprintf("%i-%i", i, i+1))
}
barplot(startups$no.users, names.arg=c,  main=sprintf("Mean %s startups per day", ylab="# users", xlab="# Aquamacs .app startups", prod))

dev.off()



pdf(file="usage-duration.pdf") 
barplot(usage_duration$no.users, names.arg=usage_duration$duration, main=sprintf("%s User experience", prod), sub="Distribution of install and use duration [days]")
dev.off()

if (length(versions$version) > 2) {

pdf(file="versions.pdf")
versions <- subset(versions, !is.na(no.users))
numo <- sum(subset(versions, no.users<=20)$no.users)
versions <- subset(versions, no.users>20)
levels(versions$version) <- c(levels(versions$version), "other")
versions <- rbind(versions, c(version="other", no.users=as.integer(numo)))
pie(as.integer(versions$no.users), versions$version)
title(main=sprintf("%s versions in use", prod), sub="Proportions of versions used during the last 5 days")
dev.off()
}

pdf(file="version-timeline.pdf", width=10, height=5)
# Plotting the Timeseries directly didn't produce useful effects,
# in particular since ts.plot takes several arguments with each series
# separately instead of a matrix with all the series. 
 
n <- versions.timeline[1]
vt <-  ts(t(versions.timeline))
rightborder <- length(vt[,1])+10

# normalize, so we get proportions
for (day in 2:length(vt[,1]))
  {
    numvec <- as.numeric(vt[day,])
    s <-  sum(numvec, na.rm = TRUE)
   vt[day,] <- numvec  #/s # let's not normalize
  }

 
#plot.default(x=c(0), y=c(0), xlim=c(0,rightborder), ylim=c(0,1), col=0, xlab="Days", ylab="proportion")

plot.default(x=c(0), y=c(0),  xlim=c(0,rightborder), ylim=c(0,as.integer(max(vt, na.rm=T))), col=0, xlab="Days", ylab="queries")
title(main=paste(prod, " versions in use"), col="Black")

l.x = c() # labels
l.y = c()
l.c = c()
l.l = c()
for (i in 1:length(n[,1]))
{
  timeseries <-  as.numeric(vt[,i])
 
  # moving average
  fil <- rep(1/as.integer(check.period.days*5), as.integer(check.period.days*5))
  timeseries <-  filter(timeseries,fil)
  lastvalue <- NA
  for(j in length(timeseries):1)
    {
      lastvalue <- timeseries[j]
      if (!is.na(lastvalue)) {break}
     }
  color <- i%%6+2
  lines(timeseries, col=color)
   

  if ((!is.na(lastvalue)) & lastvalue > 0.1)
    {
       l.x = c(l.x, rightborder*0.97)
        l.y = c(l.y, lastvalue)
        l.c = c(l.c, color)
        l.l = c(l.l, as.character(n[i,])) 
    } else
  {
    max.i <- which.max(timeseries)
    if (require("plotrix"))
      {
        l.x = c(l.x, max.i)
        l.y = c(l.y, timeseries[max.i]+0.02)
        l.c = c(l.c,color)
        l.l = c(l.l, as.character(n[i,]))
#        boxed.labels(max.i, timeseries[max.i]+0.02, bg=i%%7+1, col="black", labels=n[i,])
      }
        
   #  text(x=max.i, y=timeseries[max.i]+0.02, labels=n[i,], col=i%%7+1, bg="White" )
  }
}
#thigmophobe.labels(l.x, l.y, labels=l.l, col=l.c)
par(col="black")
 if (require("plotrix"))
  {
boxed.labels(l.x, l.y, labels=l.l, col=l.c, xpad=0.6, ypad=0.6, border=FALSE, family="sans" ) 
} else
{
labels(l.x, l.y, labels=l.l, col=l.c, xpad=0.6, ypad=0.6, border=FALSE, family="sans" ) 
}
dev.off()


pdf(file="countries.pdf")
countries <- subset(countries, (!is.na(no.users)))
othercountries <- subset(countries, no.users<=20)
mostcountries <- subset(countries, no.users>20)
c <- rbind(mostcountries, data.frame(country="Others", no.users=sum(othercountries$no.users)))

pie(c$no.users, c$country, main=sprintf("%s User location", prod))
dev.off()


pdf(file="newusers.pdf")
conversionrate2 <- subset(conversionrate, (!is.na(no.converted)))
 
with(conversionrate2, barplot (rbind( no.converted,no.new-no.converted), names.arg=day,  col=c("green","red"), main="First-time Trials and Conversion Rate", sub=sprintf("Number of first-time users per day (after introduction) \n and how many of them are still using %s today.", prod), ylab="# of users", xlab="Day\n"))

dev.off()



pdf(file="userbase.pdf")

conversionrate$ubase <- as.vector(filter(conversionrate$no.users, rep(1/7,7), sides=1))
with(conversionrate, plot(ubase~day, type="l",     main="User Base (regular users)",  ylab="# of users", xlab="Day", sub="User base estimated from number of version checks"))
dev.off()

pdf(file="conversionrate.pdf")
conversionrate2 <- subset(conversionrate, (!is.na(no.converted) & no.converted>0))
conversionrate2$ratio <- conversionrate2$no.converted /conversionrate2$no.new
with(conversionrate2, plot (ratio ~ day, type="l",  main=" Conversion Rate",  ylab="ratio of users", xlab="Day\n"))
conversionrate2$avg <- filter(conversionrate2$ratio, rep(1/7,7), sides=1)
lines(conversionrate2$avg, col="blue")
dev.off()



pdf(file="user-exposure.pdf")
c=c()
## for(i in exposure) {
##   c <- append(c,  sprintf("%i-%i", i, i+1))
## }
#  names.arg=c, 
barplot(exposure$no.users, main= "Different Users per Week", ylab="# users", xlab="Week")
dev.off()
