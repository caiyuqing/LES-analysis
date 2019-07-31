################## analysis of les questionnaires ##############################
# 
# Author      Date(yy-mm-dd)   Change History
#==========================================
# Cai, Y-Q    19-07-30         The first version
# 
# 
#
###### input######
#
#
###### output #####
# 
#
#
#
######################## Start of the script ###########################
### clean the memory to avoid unnecessary errors:
rm(list = ls())

### set directory to the folder of analytic data

# Get the directory of the current R script
curWD <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set the working directory to the directory where this script is 
setwd(curWD)

# load the packages needed, if not exist, download from cran
if (!require(tidyverse)) {install.packages("tidyverse",repos = "http://cran.us.r-project.org"); require(tidyverse)}
if (!require(dplyr)) {install.packages("dplyr",repos = "http://cran.us.r-project.org"); require(dplyr)}
if (!require(psych)) {install.packages("psych",repos = "http://cran.us.r-project.org"); require(psych)}
library("psych")
library("dplyr")
library("tidyverse")

#######Adolescence LES 1st with table
#load data
dc1 <- read.csv("wenjuan.csv", header=T)
summary(dc1)
#filter data that could be used
#Q6_R10 filter question
data <- dc1 %>%
  dplyr::filter(Q6_R10 == 3)
#descriptive data
table(data$Q2) 
##M:38 F:48
table(data$Q3)
##age: 0-18:6; 18-26:80
les <- data %>%
  dplyr::select(Q5_R1,Q5_R2,Q5_R3,Q5_R4,Q5_R5,Q5_R6,Q5_R7,Q5_R8,Q5_R9,Q5_R10,
                Q5_R11,Q5_R12,Q5_R13,Q5_R14,Q5_R15,Q5_R16,Q5_R17,Q5_R18,Q5_R19,Q5_R20,
                Q5_R21,Q5_R22,Q5_R23,Q5_R24,Q5_R25,Q5_R26,Q5_R28)
names(les) <- c("l1", "l2","l3","l4","l5","l6","l7","l8","l9","l10",
                "l11", "l12","l13","l14","l15","l16","l17","l18","l19","l20",
                "l21", "l22","l23","l24","l25","l26","l27")
depr <- data %>%
  dplyr::select(Q6_R1,Q6_R2,Q6_R3,Q6_R4,Q6_R5,Q6_R6,Q6_R7,Q6_R8) %>%
  dplyr::mutate(Q6_R4 = recode(Q6_R4, '1' = 4, '2'= 3, '3' = 2, '4' = 1),
                Q6_R6 = recode(Q6_R6, '1' = 4, '2'= 3, '3' = 2, '4' = 1))
names(depr) <- c("d1", "d2","d3","d4","d5","d6","d7","d8")
#Reliability of les
lesAlpha <-  psych::alpha(les)  # calculate the alpha coefficient of JS
print(lesAlpha$total)  # print the alpha for JS
# alpha = .96
lesOmega <- psych::omega(les)  
print(c(lesOmega$omega_h,lesOmega$omega.tot)) 
# omega.tot = .97
#reliability of CES-D
deprAlpha <-  psych::alpha(depr)  # calculate the alpha coefficient of JS
print(deprAlpha$total)  # print the alpha for JS
# alpha = .77
lesOmega <- psych::omega(les)  
print(c(lesOmega$omega_h,lesOmega$omega.tot)) 
# omega.tot = .98

#descriptive 
#无影响=1, 轻度=2, 中度=3, 重度=4, 极重=5
les$les_n <- rowSums(les == 1) 
les$les_y <- 27-les$les_n
les <- les %>%
  dplyr::mutate(sum_les= l1 +l2 +l3 +l4 +l5 +l6 +l7 +l8 +l9 +l10 +l11 +l12 +l13 +l14 +l15 +l16 +l17 +l18 +l19 +l20 +l21 +l22 +l23 +l24 +l25 +l26 +l27) 
depr <- depr %>%
  dplyr::mutate(sum_depr = d1 +d2+d3+d4+d5+d6+d7+d8)
#correlation
#with severity
cor(les$sum_les, depr$sum_depr)
# cor = .47
cor.test(les$sum_les, depr$sum_depr)
# p < 0.01

#without severity
cor(les$les_y, depr$sum_depr)
# cor = .52
cor.test(les$les_y, depr$sum_depr)
# p < 0.01