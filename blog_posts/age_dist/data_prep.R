#data_prep.R
#reads data from MT reviews and rekognition results and aggregates data based on age groups
setwd("/Users/myazdaniUCSD/Documents/selfies/Age-Grouped-Selfie-Rate-Plot/blog_posts/age_dist")

library(data.table)
library(ggplot2)
selfies.raw.tags = read.csv("../../data/tags.csv", header = TRUE, stringsAsFactors = FALSE)

ids = unique(selfies.raw.tags$id)

selfies.raw.tags.DT = data.table(selfies.raw.tags)
setkey(selfies.raw.tags.DT, id)
median.age = sapply(ids, FUN = function(x) median(selfies.raw.tags.DT[x]$age))
mean.age = sapply(ids, FUN = function(x) mean(selfies.raw.tags.DT[x]$age))
sd.age = sapply(ids, FUN = function(x) sd(selfies.raw.tags.DT[x]$age))
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

gender.agreement.level = function(x) {
  ux <- unique(x)
  length(which(x == ux[which.max(tabulate(match(x, ux)))] ))/ length(x) 
}

mode.gender = sapply(ids, FUN = function(x) Mode(selfies.raw.tags.DT[x]$gender))
gender.conf = sapply(ids, FUN = function(x) gender.agreement.level(selfies.raw.tags.DT[x]$gender))

cities = sapply(ids, FUN = function(x) selfies.raw.tags.DT[x]$city[1])
selfies.tags = data.frame(id = ids, median.age = median.age, mean.age = mean.age, sd.age= sd.age, mode.gender = mode.gender, gender.conf = gender.conf, city = cities)
levels(selfies.tags$city) = c("Bangkok", "Berlin", "Moscow", "New York", "Sao Paulo", "Tokyo")
levels(selfies.tags$mode.gender) = c("Female", "Male")

num.selfies.per.city = data.frame(city = names(summary(as.factor(selfies.tags$city))), total.selfies = summary(as.factor(selfies.tags$city)))

selfies.tags$age_group = cut(selfies.tags$median.age, c(0, 17, 21, 25, 29, max(selfies.tags$median.age)))
selfie.age.group.rate = aggregate(selfies.tags$median.age, selfies.tags[c("mode.gender", "city", "age_group")], length)
names(selfie.age.group.rate)[ncol(selfie.age.group.rate)] = "num.selfies"
selfie.age.group.rate = merge(selfie.age.group.rate, num.selfies.per.city, by = "city")
selfie.age.group.rate$rate = selfie.age.group.rate$num.selfies/selfie.age.group.rate$total.selfies

selfie.age.group.rate$age_group = factor(selfie.age.group.rate$age_group)
selfie.age.group.rate$mode.gender = factor(selfie.age.group.rate$mode.gender)

#levels(selfie.age.group.rate$age_group) = c("Under 18", "18 - 24", "24-32", "Over 32")

selfie.age.group.all = aggregate(selfies.tags$median.age, selfies.tags[c("mode.gender", "city")], length)
names(selfie.age.group.all)[ncol(selfie.age.group.all)] = "num.selfies"
selfie.age.group.all = merge(selfie.age.group.all, num.selfies.per.city, by = "city")
selfie.age.group.all$rate = selfie.age.group.all$num.selfies/selfie.age.group.all$total.selfies

selfie.age.group.all$mode.gender = factor(selfie.age.group.all$mode.gender)
########################################################################################## 
#----------------------------------plotting----------------------------------------------#
##########################################################################################
size_of_point_markers = 3
size_of_city_names = 1 
size_of_percentage_labels = 1
size_of_legend_text = 1

fig_width = 7
fig_height = 6

library(ggplot2) #use install.packages("ggplot2") if gglot2 not installed

ggplot(selfie.age.group.rate, aes(x = rate, y = city)) + 
  geom_segment(aes(yend=city), xend = 0, colour = "grey50") + 
  geom_point(size = rel(size_of_point_markers), aes(colour = mode.gender)) +  
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        axis.text.x = element_text(size = rel(size_of_percentage_labels)), 
        axis.text.y = element_text(size = rel(size_of_city_names)),
        legend.text = element_text(size = rel(size_of_legend_text)),
        legend.key = element_blank(),
        legend.title = element_blank()) +
  facet_grid(age_group ~ ., scales = "free_y", space = "free_y") +
  ylab("") +
  xlab("Percentage") 

ggsave(file="age_dist_age_group_update.pdf", width=fig_width, height=fig_height)

ggplot(selfie.age.group.all, aes(x = rate, y = city)) + 
  geom_segment(aes(yend=city), xend = 0, colour = "grey50") + 
  geom_point(size = rel(size_of_point_markers), aes(colour = mode.gender)) +  
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        axis.text.x = element_text(size = rel(size_of_percentage_labels)), 
        axis.text.y = element_text(size = rel(size_of_city_names)),
        legend.text = element_text(size = rel(size_of_legend_text)),
        legend.key = element_blank(),
        legend.title = element_blank()) +
  ylab("") +
  xlab("Percentage") 
ggsave(file="age_dist_all_update.pdf", width=fig_width, height=fig_height)

calculate.female.odds = function(x) {
  p = x[which(x$mode.gender == "Female"), "rate"] 
  return(log(p / (1 - p)))
}
selfie.odds = by(selfie.age.group.all, selfie.age.group.all$city, calculate.female.odds)
selfie.odds = as.data.frame.table(selfie.odds, responseName="Odds")
colnames(selfie.odds)[1] = "city"

ggplot(selfie.odds, aes(x = Odds, y = city)) + 
  geom_segment(aes(yend=city), xend = 0, colour = "grey50") + 
  geom_point(size = rel(size_of_point_markers)) +  
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        axis.text.x = element_text(size = rel(size_of_percentage_labels)), 
        axis.text.y = element_text(size = rel(size_of_city_names))) +
  ylab("") +
  xlab("Odds") 
ggsave(file="log_odds.pdf", width=fig_width, height=fig_height)

calculate.female.odds.age.group = function(x) {
  num.females =  x[which(x$mode.gender == "Female"), "num.selfies"]
  num.males = x[which(x$mode.gender == "Male"), "num.selfies"]
  total.number = num.females + num.males
  p = num.females/total.number
  return(log(p / (1 - p)))
}

selfie.odds.group = by(selfie.age.group.rate, selfie.age.group.rate[c("city", "age_group")], calculate.female.odds.age.group)
selfie.odds.group = as.data.frame.table(selfie.odds.group, responseName="Odds")
colnames(selfie.odds.group)[1] = "city"

ggplot(selfie.odds.group, aes(x = Odds, y = city)) + 
  geom_segment(aes(yend=city), xend = 0, colour = "grey50") + 
  geom_point(size = rel(size_of_point_markers)) +  
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        axis.text.x = element_text(size = rel(size_of_percentage_labels)), 
        axis.text.y = element_text(size = rel(size_of_city_names))) +
  ylab("") +
  facet_grid(age_group ~ ., scales = "free_y", space = "free_y") +
  xlab("Odds") 
ggsave(file="log_odds_group.pdf", width=fig_width, height=fig_height)
