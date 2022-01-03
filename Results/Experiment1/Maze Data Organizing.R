#exp1 staub rep
setwd("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes")
library(tidyverse)
library(stringr)
library(urltools)
library(brms)
library(data.table)

#options (contrasts = c("contr.sum","cont.sum"))

read_in_data <- function(filename){
  #reads data in generically because different format for different controllers
  data <- read_csv(filename, comment="#", col_names=c("time", "MD5", "controller", "item", "elem",
                                                      "type", "group", "col_8", "col_9", "col_10", "col_11", "col_12", "col_13", "col_14"), col_types=cols(
                                                        time=col_integer(),
                                                        MD5=col_character(),
                                                        controller=col_character(),
                                                        item=col_integer(),
                                                        elem=col_integer(),
                                                        type=col_character(),
                                                        group=col_integer(),
                                                        col_8=col_character(),
                                                        col_9=col_character(),
                                                        col_10=col_character(),
                                                        col_11=col_character(),
                                                        col_12=col_character(),
                                                        col_13=col_character(),
                                                        col_14=col_character()
                                                      )) %>% mutate_all(url_decode) #deal with %2C issue
  
  order_seen <- data %>% 
    select(time, MD5, group) %>% 
    filter(!is.na(group)) %>% 
    unique() %>% 
    group_by(time, MD5) %>% 
    mutate(trial_num = 1:n()) %>% 
    type_convert()
  
  
  #take the Maze task results, relabel and type appropriately
  maze<- filter(data, controller=="Maze") %>% 
    select(time, MD5, type, group, word_num=col_8, word=col_9, distractor=col_10, on_right=col_11, correct=col_12, rt=col_13, sentence=col_14) %>% 
    type_convert(col_types=cols(
      time=col_integer(),
      MD5=col_character(),
      type=col_character(),
      group=col_integer(),
      word_num=col_integer(),
      word=col_character(),
      distractor=col_character(),
      on_right=col_logical(),
      correct=col_character(),
      rt=col_integer(),
      sentence=col_character()
    )) %>% 
    left_join(order_seen, by=c("time", "MD5", "group"))
  maze
}

gulo_maze = read_in_data("results.csv") %>% 
  mutate(subject=paste(MD5, time),
         subject=factor(subject, levels=unique(subject), labels=1:length(unique(subject)))) %>% 
  select(-MD5, -time)

length(unique(gulo_maze$subject)) ##146

data_for_analysis_mean_correct = as.data.table(gulo_maze)

data_for_analysis_mean_correct = data_for_analysis_mean_correct %>%
  mutate(correct = ifelse(correct == "no",0,1))

data_for_analysis_mean_correct = data_for_analysis_mean_correct[, MeanCor := mean(correct), by="subject"]

data_for_analysis = data_for_analysis_mean_correct[!(data_for_analysis_mean_correct$type=="practice" | data_for_analysis_mean_correct$type =="filler"),]

length(gulo_maze$type=="implausible_novel" & gulo_maze$group=="18")

data_for_analysis = data_for_analysis %>%
  filter(str_detect(type, "filler", negate = TRUE))

data_for_analysis = data_for_analysis %>%
  separate(type, c("Type1", "Type2"), sep = "_")

data_for_analysis2 = data_for_analysis %>%
  filter(data_for_analysis$MeanCor > .70)
length(unique(data_for_analysis$subject)) #146
length(unique(data_for_analysis2$subject)) #116
###eliminated subjects responses for having below 70% accuracy
### 23 subjects eliminated, 54 left afterwards

data_for_analysis = data_for_analysis2
key = read_csv("key.csv")
colnames(data_for_analysis)
colnames(key)

key = key %>% 
  rename(sentence = Sentence)

key$"sentence" = sprintf('%s.', key$"sentence")



data_for_analysis_v2 = left_join(data_for_analysis, key)
#convert all the numbers to integers
data_for_analysis_v2$group = as.integer(data_for_analysis_v2$group)
data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
data_for_analysis_v2$trial_num = as.integer(data_for_analysis_v2$trial_num)
data_for_analysis_v2$subject = as.integer(data_for_analysis_v2$subject)


data_for_analysis_v2 = data_for_analysis_v2 %>% #this is all of our RTs
  separate(`Compound Noun`, c("N1", "N2"), sep = " ")

data_for_analysis_v2$whichfirst = data_for_analysis_v2$word == data_for_analysis_v2$N1

data_for_analysis_v2$whichfirst = as.integer(data_for_analysis_v2$whichfirst)
## 1 means the word is N1, 0 means the word is N2
data_for_analysis_v2 = data_for_analysis_v2[!(data_for_analysis_v2$group=="18"),]

data_for_analysis_v2b = data_for_analysis_v2 %>% #RTs only for the critical words
  filter((data_for_analysis_v2$word == data_for_analysis_v2$N1) | (data_for_analysis_v2$word == data_for_analysis_v2$N2))

sort(unique(data_for_analysis_v2b$Type1))
sort(unique(data_for_analysis_v2b$Type2))

use = data_for_analysis_v2b$rt > 200 & data_for_analysis_v2b$rt < 5000 #change in accordance to Boyce
data_for_analysis_v2b = data_for_analysis_v2b[use,]
hist(log(data_for_analysis_v2b$rt))

data_for_analysis_v2b$rt = log(data_for_analysis_v2b$rt) #log transform it --> double check with what Boyce does
hist(data_for_analysis_v2b$rt)

data_for_analysis_v2b = rename(data_for_analysis_v2b, plausibility = Type1)
data_for_analysis_v2b = rename(data_for_analysis_v2b, familiarity = Type2)

data_for_analysis_v2b$group = as.numeric(data_for_analysis_v2b$group)
data_for_analysis_v2b$word_num = as.numeric(data_for_analysis_v2b$word_num)
data_for_analysis_v2b$correct = as.numeric(data_for_analysis_v2b$correct)
data_for_analysis_v2b$rt = as.numeric(data_for_analysis_v2b$rt)
data_for_analysis_v2b$trial_num = as.numeric(data_for_analysis_v2b$trial_num)
data_for_analysis_v2b$subject = as.numeric(data_for_analysis_v2b$subject)
data_for_analysis_v2b$MeanCor = as.numeric(data_for_analysis_v2b$MeanCor)
data_for_analysis_v2b$whichfirst = as.numeric(data_for_analysis_v2b$whichfirst)

write_csv(data_for_analysis_v2b, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/data for analysis.csv")
data_for_analysis_v2b = read_csv("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/data for analysis.csv")

data_correct_only = data_for_analysis_v2b[!(data_for_analysis_v2b$correct=="no"),]
data_n1 = data_correct_only[(data_correct_only$whichfirst==1),]
data_n2 = data_correct_only[(data_correct_only$whichfirst==0),]
data_correct_and_incorrect = data_for_analysis_v2b
data_correct_and_incorrect_n1 = data_correct_and_incorrect[(data_correct_and_incorrect$whichfirst=="1"),]
data_correct_and_incorrect_n2 = data_correct_and_incorrect[(data_correct_and_incorrect$whichfirst=="0"),]

length(unique(data_n1$subject)) #n=116
length(unique(data_n2$subject)) #n=116
length(data_n1$subject) #tokens = 4995
length(data_n2$subject) #tokens = 4999
###############################
###Time for our model###

##log transform values
##make a table of word and sentence --> don't need to include word? double check by making tables

##to change to treatment coding 
options (contrasts = c('contr.treatment','contr.treatment'))
###

data_n1 = data_n1 %>%
  rename(
    item = group
  )

data_n2 = data_n2 %>%
  rename(
    item = group
  )
###change to sum coding
options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1


test = data_n1

model_test =
  brm (rt ~ plausibility, data = test, chains = 4, cores = 5, 
       warmup = 500, iter = 4000, thin = 4, 
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept")))
         

            
model_n1 =
  brm (rt ~ plausibility*familiarity + (1 + plausibility*familiarity|subject) + (1 + plausibility|item), data = data_n1, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, 
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

saveRDS(model_n1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_n1_sum_coding.rds")
saveRDS(model_n1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_n1_treatment_coding.rds")
treat = readRDS("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_n1_treatment_coding.rds")

model_n2 = brm (rt ~ plausibility*familiarity + (1 + plausibility*familiarity|subject) + (1 + plausibility|item), data = data_n2, chains = 4, cores = 5, 
                warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
                prior = c(set_prior("student_t(3, 6.87, 100)", class = "Intercept"),
                          set_prior("student_t(3, 0, 100)", class = "b"),
                          set_prior("student_t(3, 0, 100)", class = "sd")))

saveRDS(model_n2, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_n2_sum_coding.rds")
saveRDS(model_n2, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_n2.rds")


model_correct_n1 = brm (correct ~ plausibility*familiarity + (1|subject) + (1|group) + (1|sentence) + (1|distractor), data = data_correct_and_incorrect_n1, chains = 4, cores = 5, 
                     warmup = 1000, iter = 6000, thin = 4, family="bernoulli", control = list(adapt_delta = 0.99),
                     prior = c(set_prior("student_t(3, 0, 100)", class = "Intercept"),
                               set_prior("student_t(3, 0, 100)", class = "b"),
                               set_prior("student_t(3, 0, 100)", class = "sd")))
saveRDS(model_correct_n1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_correct_n1_sum_coding.rds")
saveRDS(model_correct_n1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_correct_n1.rds")

model_correct_n1_sum_coding = readRDS("model_correct_n1_sum_coding.rds")
model_correct_n1_treatment_coding = readRDS("model_correct_n1.rds")



model_correct_n2 = brm (correct ~ plausibility*familiarity + (1|subject) + (1|group) + (1|sentence) + (1|distractor), data = data_correct_and_incorrect_n2, chains = 4, cores = 5, 
                        warmup = 1000, iter = 6000, thin = 4, family="bernoulli", control = list(adapt_delta = 0.99),
                        prior = c(set_prior("student_t(3, 0, 100)", class = "Intercept"),
                                  set_prior("student_t(3, 0, 100)", class = "b"),
                                  set_prior("student_t(3, 0, 100)", class = "sd")))

saveRDS(model_correct_n2, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_correct_n2_sum_coding.rds")
saveRDS(model_correct_n2, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_correct_n2.rds")

model_correct_n2_sum_coding = readRDS("model_correct_n2_sum_coding")
model_correct_n2_treatment = readRDS("model_correct_n2.rds")

### analyze N1 and N2 separately

#save model

fil = tempfile("model1", fileext = ".rds")
saveRDS(model1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model1.rds")

##load model

test = readRDS("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model1.rds")
test
######

##

hist(data_for_analysis_v2b$rt)

###log transform rt or exponential link

use = data_for_analysis_v2$rt > 200 & data_for_analysis_v2$rt < 10000
new_data = data_for_analysis_v2[use,]
hist(log(new_data$rt))

use2 = data_for_analysis_v2b$rt > 300 & data_for_analysis_v2b$rt < 10000
new_data2 = data_for_analysis_v2b[use2,]
hist(log(new_data2$rt))

###include a factor for which word occurs first (if word == N1)


###models placed here for convenience


###treatment coded models:

n1_treat = readRDS("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Codes/model_n1_treatment_coding.rds")
n2_treat = readRDS("model_n2.rds")
  
n1_correct_treat = readRDS("model_correct_n1.rds")
n2_correct_treat = readRDS("model_correct_n2.rds")

###sum coded models:

n1_sum = readRDS("model_n1_sum_coding.rds")
n2_sum = readRDS("model_n2_sum_coding.rds")

n1_correct_sum = readRDS("model_correct_n1_sum_coding.rds")
n2_correct_sum = readRDS("model_correct_n2_sum_coding.rds")

n1_treat
n2_treat
n1_correct_treat
n2_correct_treat

n1_sum
n2_sum
n1_correct_sum
n2_correct_sum


setwd("D:/PhD Stuff/Linguistics Stuff/Flash Experiment Data")
source("BRM Plot.R")
par(mfrow=c(1,2))
brmplot(ranef(n1_sum)$subject[,,1], col="darkblue", labels="", xlab="Subject",ylab="Subject Intercepts", main="Subject Intercepts") #estimates of the intercept
brmplot(ranef(n1_sum)$subject[,,2], col="aquamarine3", labels="", xlab="Subject",ylab="By-subject Random Slopes", main="By-Subject Random Slopes")  #estimates of which to flash by subject intercepts


model_formula = brmsformula(
  rt ~ plausibility*familiarity + (1|subject) + (1|group) + (1|sentence) + (1|distractor),
  sigma ~ (1|subject)
)

model_test = brms::brm (model_formula,
                        data = data_n1, chains = 4, cores = 5, 
                        warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
                        prior = c(set_prior("student_t(3, 837, 100)", class = "Intercept"),
                                  set_prior("student_t(3, 0, 100)", class = "b"),
                                  set_prior("student_t(3, 0, 100)", class = "sd")))

model_test2 = brms::brm (model_formula,
                         data = data_n2, chains = 4, cores = 5, 
                         warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
                         prior = c(set_prior("student_t(3, 837, 100)", class = "Intercept"),
                                   set_prior("student_t(3, 0, 100)", class = "b"),
                                   set_prior("student_t(3, 0, 100)", class = "sd")))       



###### plots
conditional_effects(n1_model1)

n1_plot = conditional_effects(model_n1)
n2_plot = conditional_effects(model_n2)
n1_correct_plot = conditional_effects(model_correct_n1)
n2_correct_plot = conditional_effects(model_correct_n2)


plot(n1_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.65, 7)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility")

plot(n2_plot, plot = FALSE)[[3]] +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility")

plot(n1_correct_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(0.868, 0.99)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Correctness", title = "Correctness ~ Plausibility")

plot(n2_correct_plot, plot = FALSE)[[3]] +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Correctness", title = "Correctness ~ Plausibility")



