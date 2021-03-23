library(rdrop2)
library(tidyverse)
library(ggplot2)
# read in the dropbox token
drop_auth(rdstoken = "pilot-analysis/token.rds")

## list the files in the learnr_results folder
files <- drop_dir("/D'Agostino McGowan Data Science Lab/learnr_results/")

files <- files %>%
  filter(grepl("assessment_(1|2)", name))

f <- function(path) {
  drop_download(path, local_path = "temp.rds", overwrite = TRUE)
  d <- readRDS("temp.rds")
  d$user_info <- path
  d
}

d <- map_df(files$path_lower, f)

## make this a wider data frame with a column for each question per assessment per user
## In the end, we'd have 1 or two rows per username (depending on whether they completed the second assessment)
## The exploratory analysis could subset to only assessment 1 responses and then only assessment 2 responses by arm (A or B) and see if there is a differece
## Score each questions

d <- d %>%
  pivot_wider(names_from = question, values_from= response)

d2 <- d %>%
  group_by(username) %>%
  add_count(username) %>%
  filter(n==2) %>%
  select(-n) %>%
  arrange(username)


dA <- filter(d2, grepl("GA",username))

dB <- filter(d2, grepl("GB",username))

# Grading their assessments for group A
dA$pt1_1 <- as.integer(grepl('read.csv|read_csv',dA$`ex1-1`))
dA$pt2_1 <- as.integer(grepl("8602",dA$`ex2-1`))
dA$pt3_1 <- as.integer(grepl("9",dA$`ex3-1`))
dA$pt4_1 <- as.integer(grepl("1997",dA$`ex4-1`))

dA$pt1_2 <- as.integer(grepl('read.csv|read_csv',dA$`ex1-2`))
dA$pt2_2 <- as.integer(grepl("8602",dA$`ex2-2`))
dA$pt3_2 <- as.integer(grepl("9",dA$`ex3-2`))
dA$pt4_2 <- as.integer(grepl("1997",dA$`ex4-2`))

# Grading their assessments for group B
dB$pt1_1 <- as.integer(grepl('read.csv|read_csv',dB$`ex1-1`))
dB$pt2_1 <- as.integer(grepl("8602",dB$`ex2-1`))
dB$pt3_1 <- as.integer(grepl("9",dB$`ex3-1`))
dB$pt4_1 <- as.integer(grepl("1997",dB$`ex4-1`))

dB$pt1_2 <- as.integer(grepl('read.csv|read_csv',dB$`ex1-2`))
dB$pt2_2 <- as.integer(grepl("8602",dB$`ex2-2`))
dB$pt3_2 <- as.integer(grepl("9",dB$`ex3-2`))
dB$pt4_2 <- as.integer(grepl("1997",dB$`ex4-2`))

# Adding up the points
dA %>% 
  mutate(totalpt = rowSums(pick(pt1_1:pt4_2)))

# Plot for Group 
Competence_A <- data.frame(assessment = rep(c("1","2")), competence = c(2,3,6,7,3,3,2,2,6,6), group = rep("A"))
Competence_B <- data.frame(assessment = rep(c("1","2")), competence = c(5,4,1,2,8,9,3,5,5,5), group = rep("B"))

Competence <- rbind(Competence_A,Competence_B)

ggplot(Competence, aes(x=assessment, y=competence, group=group, color=group)) + geom_point() + geom_line()







## Pull contact information for those that completed

files <- drop_dir("/D'Agostino McGowan Data Science Lab/learnr_results/")

files <- files %>%
  filter(grepl("contact", name))

f <- function(path) {
  drop_download(path, local_path = "temp.rds", overwrite = TRUE)
  d <- readRDS("temp.rds")
  d$user_info <- path
  d
}

d <- map_df(files$path_lower, f)

emails <- d$email

emails

