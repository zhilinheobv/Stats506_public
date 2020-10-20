## Week 6 Activity
##
## This script 
##
## Author: Group 5: Zhilin He, Yan Chen, Tianshi Wang, Hao He
## Updated: October 13, 2020
# 79: -------------------------------------------------------------------------

## Read data

library(tidyverse)

pwc = function(n) {
  # Format a function
  # Inputs: n, an integer; Outputs: a formatted integer string
  format(N, trim = FALSE, big.mark = ',')
}

url1 = paste("https://github.com/jbhender/Stats506_F20/raw/master/",
             "problem_sets/data/nhanes_demo.csv", sep = "")
url2 = paste("https://github.com/jbhender/Stats506_F20/raw/master/",
             "problem_sets/data/nhanes_ohxden.csv", sep = "")

if(!file.exists("nhanes_demo.csv")) {
  download.file(url1, "nhanes_demo.csv")
}
if(!file.exists("nhanes_ohxden")) {
  download.file(url2, "nhanes_ohxden.csv")
}
demo = read.csv("nhanes_demo.csv")
ohxden = read.csv("nhanes_ohxden.csv")

## Merge the data

ohxden_1 = ohxden %>% select(SEQN, OHDDESTS) 
merged = left_join(demo, ohxden_1, by = "SEQN")

## Create a clean dataset

clean = merged %>% 
  mutate(gender = factor(RIAGENDR), 
         under_20 = ifelse(RIDAGEYR<20, "Y", "N"),
         college = ifelse(under_20 == "Y", "No college",
                          ifelse(DMDEDUC2>=4, "College", "No college"))
         ) %>% select(id = SEQN, age = RIDAGEYR, exam_status = RIDSTATR,
                      ohx_status = OHDDESTS, gender, under_20, college)

## Add a variable
  
clean = clean %>% mutate(ohx = ifelse(!is.na(ohx_status),
                               ifelse(exam_status == 2 & ohx_status == 1, 
                               "complete", "missing"), "missing"))

## Filter the data

df = clean %>% filter(exam_status == 2)

## Construct a table

table1 = df %>% group_by(under_20, ohx) %>% summarise(n=n()) %>%
  pivot_wider(names_from = ohx, values_from = n)

table2 = df %>% group_by(gender, ohx) %>% summarise(n=n()) %>%
  pivot_wider(names_from = ohx, values_from = n)

table3 = df %>% group_by(college, ohx) %>% summarise(n=n()) %>%
  pivot_wider(names_from = ohx, values_from = n)

table = rbind(table1, table2, table3)

table[,1] = c("Not under 20", "Under 20", "Male", "Female", 
              "College", "No college")

table = table %>% select(type = under_20, complete, missing)

## Add a percentage

table_p = table %>% 
  mutate(p_complete = complete/(complete+missing), 
         p_missing = missing/(complete+missing),
         complete = sprintf("%5d (%.1f%%)",complete, p_complete*100),
         missing = sprintf("%5d (%.1f%%)",missing, p_missing*100)) %>%
  select(type, complete, missing)
print(table_p)

## add p-values

library(MASS)

chisq.test(table1[,c(2,3)])
chisq.test(table2[,c(2,3)])
chisq.test(table3[,c(2,3)])
m=df[df$sex=="Male",]
f=df[df$sex=="Female",]
aggregate(m[, "_score"], list(m$v_score_text), mean)
aggregate(f[, "is_violent_recid"], list(f$v_score_text), mean)
