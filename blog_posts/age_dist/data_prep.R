#data_prep.R
#reads data from MT reviews and rekognition results and aggregates data based on age groups
setwd("/Users/myazdaniUCSD/Documents/selfies/Age-Grouped-Selfie-Rate-Plot/blog_posts/age_dist")
mt.rek = read.table("../../data/selfies_rek_MT_meta.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

