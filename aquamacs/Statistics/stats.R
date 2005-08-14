# R for Aquamacs stats

prod <- "Aquamacs"

# setwd("/Users/dr/Projects/Aquamacs/Statistics")

startups <- read.table("startups.txt", header=TRUE, fill=TRUE)


usage_duration <- read.table("usage-duration.txt", header=TRUE, fill=TRUE)

versions <- read.table("versions.txt", header=TRUE, fill=TRUE)

countries <- read.table("countries.txt", header=TRUE, fill=TRUE)

conversionrate <- read.table("conversionrate.txt", header=TRUE, fill=TRUE)




pdf(file="startups.pdf")
startups <- subset(startups, no_users>4)
barplot(startups$no_users, names.arg=startups$no_startups,  main=sprintf("Mean %s startups per day", prod))
dev.off()



pdf(file="usage-duration.pdf")
barplot(usage_duration$no_users, names.arg=usage_duration$duration, main=sprintf("%s User experience", prod), sub="Distribution of install and use duration [days]")
dev.off()

pdf(file="versions.pdf")
versions <- subset(versions, !is.na(no_users))
pie(versions$no_users, versions$version, main=sprintf("%s versions in use", prod), sub="Proportions of versions used during the last 10 days")
dev.off()

pdf(file="countries.pdf")
countries <- subset(countries, (!is.na(no_users)))
othercountries <- subset(countries, no_users<=40)
mostcountries <- subset(countries, no_users>40)
c <- rbind(mostcountries, data.frame(country="Others", no_users=sum(othercountries$no_users)))

pie(c$no_users, c$country, main=sprintf("%s User location", prod))
dev.off()


pdf(file="newusers.pdf")
conversionrate2 <- subset(conversionrate, (!is.na(no_converted)))
 
with(conversionrate2, barplot (rbind( no_converted,no_new-no_converted), names.arg=day,  col=c("green","red"), main="First-time Trials and Conversion Rate", sub=sprintf("Number of first-time users per day (after introduction) \n and how many of them are still using %s today.", prod), ylab="# of users", xlab="Day\n"))

dev.off()


pdf(file="conversionrate.pdf")
conversionrate2 <- subset(conversionrate, (!is.na(no_converted) & no_converted>0))
conversionrate2$ratio <- conversionrate2$no_converted /conversionrate2$no_new
with(conversionrate2, plot (ratio ~ day, type="l",  main=" Conversion Rate",  ylab="ratio of users", xlab="Day\n"))
conversionrate2$avg <- filter(conversionrate2$ratio, rep(1/7,7), sides=1)
lines(conversionrate2$avg, col="blue")
dev.off()
