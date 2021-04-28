library(rdrop2)
library(tidyverse)
library(ggplot2)
library(likert)
library(ggpubr)

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

# Adding up the points and plotting the changes


FigA <- dA %>%
  mutate(baseline = sum(pt1_1, pt2_1, pt3_1, pt4_1, na.rm = TRUE),
         `post test` = sum(pt1_2, pt2_2, pt3_2, pt4_2, na.rm = TRUE)) %>%
  distinct(username, baseline, `post test`) %>%
  pivot_longer(2:3) %>%
  ggplot(aes(x = value, y = username)) + geom_point(aes(shape = name, color = name)) + geom_line() + labs(title = "Changes in Assessment Scores of Group A", color = "Assessment", shape = "Assessment") + ggsave("Group A.tiff", width = 10, height = 6, device='tiff', dpi=700) 


FigB <- dB %>%
  mutate(baseline = sum(pt1_1, pt2_1, pt3_1, pt4_1, na.rm = TRUE),
         `post test` = sum(pt1_2, pt2_2, pt3_2, pt4_2, na.rm = TRUE)) %>%
  distinct(username, baseline, `post test`) %>%
  pivot_longer(2:3) %>%
  ggplot(aes(x = value, y = username)) + geom_point(aes(shape = name, color = name)) + geom_line() + labs(title = "Changes in Assessment Scores of Group B", color = "Assessment", shape = "Assessment") + ggsave("Group B.tiff", width = 10, height = 6, device='tiff', dpi=700) 

figure <- ggarrange(FigA, FigB, labels = c("A", "B"), ncol = 1, nrow = 2)  
figure
annotate_figure(figure, top = text_grob("Figure 1. Comparison of Assessment Scores between Groups", size = 14, face = "bold")) + ggsave("Figure 1.tiff", width = 10, height = 6, device='tiff', dpi=300) 


# Plot for Competence
# 1 being not competent, 10 being very competent
Competence_A <- data.frame(assessment = rep(c("1","2")), competence = c(2,3,6,7,3,3,2,2,6,6), group = rep("A"))
Competence_B <- data.frame(assessment = rep(c("1","2")), competence = c(5,4,1,2,8,9,3,5,5,5), group = rep("B"))



Competence_A <- mutate(Competence_A, competence=as.factor(competence))
Competence_A$competence <- factor(Competence_A$competence, levels = c("1","2","3","4","5","6","7","8","9","10"))
Competence_B <- mutate(Competence_B, competence=as.factor(competence))
Competence_B$competence <- factor(Competence_B$competence, levels = c("1","2","3","4","5","6","7","8","9","10"))
Competence <- rbind(Competence_A,Competence_B)


#Likert for A
likert_competence_A <- Competence_A %>%
  dplyr::select(competence) %>%
  rename("Response to Competence" = competence) %>%
  likert(grouping = Competence_A$assessment)

plot_comA <- plot(likert_competence_A, group.order = c ("1","2")) + ggtitle("Responses on the Competence Level of Group A") 

#Likert for B
likert_competence_B <- Competence_B %>%
  dplyr::select(competence) %>%
  rename("Response to Competence" = competence) %>%
  likert(grouping = Competence_B$assessment)

plot_comB <- plot(likert_competence_B, group.order = c ("1","2")) + ggtitle("Responses on the Competence Level of Group B")


CompetenceAB <- ggarrange(plot_comA, plot_comB, labels = c("A", "B"), ncol = 1, nrow = 2)  
CompetenceAB
annotate_figure(CompetenceAB, top = text_grob("Figure 2. Comparison of Competence Level between Groups", size = 14, face = "bold")) + ggsave("Figure 2.tiff", width = 10, height = 6, device='tiff', dpi=300) 

#Competence_1 between Groups
#Com1 <- filter(Competence, grepl("1",assessment))

#likert_com1 <- Com1 %>%
#  dplyr::select(competence) %>%
#  rename("Response to Competence" = competence) %>%
#  likert(grouping = Com1$group)

#plot(likert_com1, group.order = c ("A","B")) + ggtitle("Fig 3. Responses on the Competence Level for Assessment 1")


#Competence_2 between Groups
#Com2 <- filter(Competence, grepl("2", assessment))

#likert_com2 <- Com2 %>%
#  dplyr::select(competence) %>%
#  rename("Response to Competence" = competence) %>%
#  likert(grouping = Com2$group)

#plot(likert_com2, group.order = c ("A","B")) + ggtitle("Fig 4. Responses on the Competence Level for Assessment 2")



# Plot for Master
Master_A <- data.frame(assessment = rep(c("1","2")), master = as.factor(c(8,9,9,9,6,5,5,5,7,6)), group = rep("A"))
Master_A$master <- factor(Master_A$master, levels = c("1","2","3","4","5","6","7","8","9","10"))
Master_B <- data.frame(assessment = rep(c("1", "2")), master = as.factor(c(7,6,9,9,10,10,10,10,8,5)), group = rep("B"))
Master_B$master <- factor(Master_B$master, levels = c("1","2","3","4","5","6","7","8","9","10"))
Master <- rbind(Master_A, Master_B)

# Likert for Mas_A
likert_mas_A <- Master_A %>%
  dplyr::select(master) %>%
  rename("Response to Master" = master) %>%
  likert(grouping = Master_A$assessment)

plot_masA <- plot(likert_mas_A, group.order = c ("1","2")) 

#Likert for Mas_B
likert_mas_B <- Master_B %>%
  dplyr::select(master) %>%
  rename("Response to Master" = master) %>%
  likert(grouping = Master_B$assessment)

plot_masB <- plot(likert_mas_B, group.order = c ("1","2")) 

MasterAB <- ggarrange(plot_masA, plot_masB, labels = c("A", "B"), ncol = 1, nrow = 2)  
MasterAB
annotate_figure(MasterAB, top = text_grob("Figure 3. Comparison of Master Level between Groups", size = 14, face = "bold")) + ggsave("Figure 3.tiff", width = 10, height = 6, device='tiff', dpi=300) 


#Master_1 between Groups
#Mas1 <- filter(Master, grepl("1",assessment))

#likert_mas1 <- Mas1 %>%
#  dplyr::select(master) %>%
#  rename("Response to Master" = master) %>%
#  likert(grouping = Mas1$group)

#plot(likert_mas1, group.order = c ("A","B"))


#Master_2 between Groups
#Mas2 <- filter(Master, grepl("2", assessment))

#likert_mas2 <- Mas2 %>%
#  dplyr::select(master) %>%
#  rename("Response to Master" = master) %>%
#  likert(grouping = Mas2$group)

#plot(likert_mas2, group.order = c ("A","B"))


# Plot for Enjoying
Enjoying_A <- data.frame(assessment = rep(c("1","2")), enjoying = as.factor(c(4,5,9,9,5,6,6,7,4,6)), group = rep("A"))
Enjoying_A$enjoying <- factor(Enjoying_A$enjoying, levels = c("1","2","3","4","5","6","7","8","9","10"))
Enjoying_B <- data.frame(assessment = rep(c("1","2")), enjoying = as.factor(c(10,10,9,8,9,10,6,6,9,5)), group = rep("B"))
Enjoying_B$enjoying <- factor(Enjoying_B$enjoying, levels = c("1","2","3","4","5","6","7","8","9","10"))


Enjoying <- rbind(Enjoying_A, Enjoying_B)

# Likert for Enjoy_A
likert_enjoy_A <- Enjoying_A %>%
  dplyr::select(enjoying) %>%
  rename("Response to Enjoying" = enjoying) %>%
  likert(grouping = Enjoying_A$assessment)

plot_enjoyA <- plot(likert_enjoy_A, group.order = c ("1","2")) 

#Likert for Enjoy_B
likert_enjoy_B <- Enjoying_B %>%
  dplyr::select(enjoying) %>%
  rename("Response to Enjoying" = enjoying) %>%
  likert(grouping = Enjoying_B$assessment)

plot_enjoyB <- plot(likert_enjoy_B, group.order = c ("1","2")) 


EnjoyAB <- ggarrange(plot_enjoyA, plot_enjoyB, labels = c("A", "B"), ncol = 1, nrow = 2)  
EnjoyAB
annotate_figure(EnjoyAB, top = text_grob("Figure 4. Comparison of Enjoying Level between Groups", size = 14, face = "bold")) + ggsave("Figure 4.tiff", width = 10, height = 6, device='tiff', dpi=300)

#Enjoy_1 between Groups
Enjoy1 <- filter(Enjoying, grepl("1",assessment))

likert_enjoy1 <- Enjoy1 %>%
  dplyr::select(enjoying) %>%
  rename("Response to Enjoying" = enjoying) %>%
  likert(grouping = Enjoy1$group)

plot(likert_enjoy1, group.order = c ("A","B"))


#Enjoy_2 between Groups
Enjoy2 <- filter(Enjoying, grepl("2",assessment))

likert_enjoy2 <- Enjoy2 %>%
  dplyr::select(enjoying) %>%
  rename("Response to Enjoying" = enjoying) %>%
  likert(grouping = Enjoy2$group)

plot(likert_enjoy2, group.order = c ("A","B"))



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

