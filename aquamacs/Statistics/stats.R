# R for Aquamacs stats

prod <- "Aquamacs"

# setwd("/Users/dr/Projects/Aquamacs/aquamacs/Statistics")

startups <- read.table("startups.txt", header=TRUE, fill=TRUE)


usage_duration <- read.table("usage-duration.txt", header=TRUE, fill=TRUE)

versions <- read.table("versions.txt", header=TRUE, fill=TRUE)

countries <- read.table("countries.txt", header=TRUE, fill=TRUE)

conversionrate <- read.table("conversionrate.txt", header=TRUE, fill=TRUE)




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

pdf(file="versions.pdf")
versions <- subset(versions, !is.na(no.users))
pie(versions$no.users, versions$version, main=sprintf("%s versions in use", prod), sub="Proportions of versions used during the last 10 days")
dev.off()

pdf(file="countries.pdf")
countries <- subset(countries, (!is.na(no.users)))
othercountries <- subset(countries, no.users<=40)
mostcountries <- subset(countries, no.users>40)
c <- rbind(mostcountries, data.frame(country="Others", no.users=sum(othercountries$no.users)))

pie(c$no.users, c$country, main=sprintf("%s User location", prod))
dev.off()


pdf(file="newusers.pdf")
conversionrate2 <- subset(conversionrate, (!is.na(no.converted)))
 
with(conversionrate2, barplot (rbind( no.converted,no.new-no.converted), names.arg=day,  col=c("green","red"), main="First-time Trials and Conversion Rate", sub=sprintf("Number of first-time users per day (after introduction) \n and how many of them are still using %s today.", prod), ylab="# of users", xlab="Day\n"))

dev.off()



pdf(file="userbase.pdf")

conversionrate$ubase <- as.vector(filter(conversionrate$no.users, rep(1/7,7), sides=1))
with(conversionrate, plot(ubase~day, type="l",     main="User Base",  ylab="# of users", xlab="Day", sub="User base estimated from number of version checks"))
dev.off()

pdf(file="conversionrate.pdf")
conversionrate2 <- subset(conversionrate, (!is.na(no.converted) & no.converted>0))
conversionrate2$ratio <- conversionrate2$no.converted /conversionrate2$no.new
with(conversionrate2, plot (ratio ~ day, type="l",  main=" Conversion Rate",  ylab="ratio of users", xlab="Day\n"))
conversionrate2$avg <- filter(conversionrate2$ratio, rep(1/7,7), sides=1)
lines(conversionrate2$avg, col="blue")
dev.off()
