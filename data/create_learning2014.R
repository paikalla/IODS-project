# install.packages("dplyr")
library(dplyr)

url <- "http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"
lrn14 <- read.table(url, sep="\t", header=TRUE, stringsAsFactors = TRUE)
str(lrn14)

# Creating an analysis dataset with the variables gender, age, attitude, deep, stra, surf and points by combining questions in the learning2014 data
# Scaling all combination variables to the original scales (by taking the mean). Excluding observations where the exam points variable is zero. 
# The data will have 166 observations and 7 variables.

# 'deep' 

deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)

#  'surf'

surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)

# 'stra'

strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

# Creating the analysis data set:

keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(lrn14,one_of(keep_columns))
learning2014$Attitude <- learning2014$Attitude / 10
colnames(learning2014)[c(2, 3, 7)] <- c("age", "attitude", "points")
colnames(learning2014)
learning2014 <- filter(learning2014, points > 0)
str(learning2014)

# write data as csv and check data quality

setwd("data/")
write.csv(learning2014, file = "learning2014.csv", row.names = FALSE)
data_test <- read.csv("learning2014.csv", stringsAsFactors = TRUE)
str(data_test)

