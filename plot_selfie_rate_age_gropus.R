#displayes selfie_rates_age_groups.csv as dotplot
#selfie_rates_age_groups.csv shows the selfie rate for different age groups, cities, and genders

size_of_point_markers = 3
size_of_city_names = 1 
size_of_percentage_labels = 1
size_of_legend_text = 1

fig_width = 7
fig_height = 6

library(ggplot2) #use install.packages("ggplot2") if gglot2 not installed

selfie.rates.age.groups = read.csv("./data/selfie_rates_age_groups.csv", header = TRUE, stringsAsFactors = FALSE)

selfie.rates.age.groups$Age.Group = factor(selfie.rates.age.groups$Age.Group, c("Under 13", "13-17", "18-24", "25-32", "Over 32"))
selfie.rates.age.groups$Gender = factor(selfie.rates.age.groups$Gender)



ggplot(selfie.rates.age.groups, aes(x = rate, y = City)) + 
  geom_segment(aes(yend=City), xend = 0, colour = "grey50") + 
  geom_point(size = rel(size_of_point_markers), aes(colour = Gender)) +  
  theme_bw() + 
  theme(panel.grid.major.y = element_blank(), 
        axis.text.x = element_text(size = rel(size_of_percentage_labels)), 
        axis.text.y = element_text(size = rel(size_of_city_names)),
        legend.text = element_text(size = rel(size_of_legend_text)),
        legend.key = element_blank(),
        legend.title = element_blank()) +
  facet_grid(Age.Group ~ ., scales = "free_y", space = "free_y") +
  ylab("") +
  xlab("Percentage") 

ggsave(file="age_dist.pdf", width=fig_width, height=fig_height)