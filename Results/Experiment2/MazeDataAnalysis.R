#Exp5
setwd("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task")

library(tidyverse)
library(stringr)
library(urltools)
library(brms)
library(data.table)

source("BRM Plot.R")

#data = read_csv("Key.csv")

#data = data[,2]

#data = unique(data)

#data = data %>%
#separate(`Compound Noun`, c("N1", "N2"), sep=" ")

#write_csv(data, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp4/outputforngrams.csv", col_names = FALSE)


options (contrasts = c("contr.sum","cont.sum"))

read_in_data <- function(filename){
  #reads data in generically because different format for different controllers
  data = read.table(filename, quote="", header = FALSE, sep = ",", col.names = paste0(c("time", "MD5", "item", "elem",
                                                                                             "type", "group", "col_7", "col_8", "col_9", 
                                                                                             "col_10", "col_11", "col_12", "col_13", "col_14")), 
                    fill = TRUE) %>% mutate_all(url_decode) %>% #deal with %2C issue 
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
    ))
  
  
  
  
  
  
  order_seen <- data %>% 
    select(time, MD5, group) %>% 
    filter(!is.na(group)) %>% 
    unique() %>% 
    group_by(time, MD5) %>% 
    mutate(trial_num = 1:n()) %>% 
    type_convert()
  
  maze<- filter(data, type=="Plausible"|type=="Implausible") %>% 
    select(time, MD5, type, group, word_num=col_7, word=col_8, distractor=col_9, on_right=col_10, correct=col_11, rt=col_12, sentence=col_13) %>% 
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

results1 = read_in_data("results 1 - 31.csv")
results2 = read_in_data("results 32 - 50.csv")
results3 = read_in_data("results 51 - 70.csv")
results4 = read_in_data("results 71 - 85.csv")
results5 = read_in_data("results 86 - 105.csv")

results = results1 %>%
  full_join(results2) %>%
  full_join(results3) %>%
  full_join(results4) %>%
  full_join(results5) %>%
  mutate(subject=paste(MD5, time),
         subject=factor(subject, levels=unique(subject), labels=1:length(unique(subject)))) %>% 
  select(-MD5, -time)

gulo_maze = results

#gulo_maze = read_in_data("results.csv") %>% 
  #mutate(subject=paste(MD5, time),
         #subject=factor(subject, levels=unique(subject), labels=1:length(unique(subject)))) %>% 
  #select(-MD5, -time)

length(unique(gulo_maze$subject)) ##105

data_for_analysis_mean_correct = as.data.table(gulo_maze)

data_for_analysis_mean_correct = data_for_analysis_mean_correct %>%
  mutate(correct = ifelse(correct == "no",0,1))

data_for_analysis_mean_correct = data_for_analysis_mean_correct[, MeanCor := mean(correct), by="subject"]

data_for_analysis = data_for_analysis_mean_correct[!(data_for_analysis_mean_correct$type=="practice" | data_for_analysis_mean_correct$type =="filler"),]

data_for_analysis = data_for_analysis %>%
  filter(str_detect(type, "filler", negate = TRUE))


data_for_analysis2 = data_for_analysis %>%
  filter(data_for_analysis$MeanCor > .70)
length(unique(data_for_analysis$subject)) #105
length(unique(data_for_analysis2$subject)) #86
###eliminated subjects responses for having below 70% accuracy
### 39 subjects eliminated, 96 left afterwards

data_for_analysis = data_for_analysis2
key = read_csv("Key.csv")

#oddsratio_calculations = key

#my_data = unique(oddsratio_calculations[,2])

#oddsratio_calculations = my_data %>%
#separate(`Compound Noun`, c("N1", "N2"), " ")

#write_csv(oddsratio_calculations, "D:/PhD Stuff/Linguistics Stuff/Google NGRAMS/words for oddsratio calculations.csv")

#oddsratio = read_csv("csvoutput_POS_unspecified.csv")
#oddsratio = oddsratio %>%
  #unite("Compound Noun", N1:N2, sep = " ", remove = FALSE)

#key = left_join(key, oddsratio)
key$logoddsratio = log(as.numeric(key$`Odds Ratio`))
#key$logdeltaP = log(key$`Delta P`)

plot(density(key$`Delta P of Match Count`)) #this looks pretty normalized, but it has negative values
plot(density(as.numeric(key$`Odds Ratio`))) #this is left skewed
plot(density(key$logoddsratio))
plot(density(log(key$`Delta P`)))

#### 

colnames(data_for_analysis)
colnames(key)

key = key %>% 
  rename(sentence = Sentences)

key$sentence = paste0(key$sentence, ".")


data_for_analysis_v2 = left_join(data_for_analysis, key)
#convert all the numbers to integers
#data_for_analysis_v2$group = as.integer(data_for_analysis_v2$group)
#data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
#data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
#data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
#data_for_analysis_v2$word_num = as.integer(data_for_analysis_v2$word_num)
#data_for_analysis_v2$trial_num = as.integer(data_for_analysis_v2$trial_num)
#data_for_analysis_v2$subject = as.integer(data_for_analysis_v2$subject)


data_for_analysis_v2 = data_for_analysis_v2 %>% #this is all of our RTs
  separate(`Compound Noun`, c("N1", "N2"), sep = " ")

data_for_analysis_v2$whichfirst = data_for_analysis_v2$word == data_for_analysis_v2$N1

data_for_analysis_v2$whichfirst = as.integer(data_for_analysis_v2$whichfirst)
## 1 means the word is N1, 0 means the word is N2
data_for_analysis_v2$word = gsub("\\.", "", data_for_analysis_v2$word)
data_for_analysis_v2$word = gsub(",", "", data_for_analysis_v2$word)

data_for_analysis_v2$distractor = gsub("\\.", "", data_for_analysis_v2$distractor)
data_for_analysis_v2$distractor = gsub(",", "", data_for_analysis_v2$distractor)

data_for_analysis_v2b = data_for_analysis_v2 %>% #RTs only for the critical words
  filter((data_for_analysis_v2$word == data_for_analysis_v2$N1) | (data_for_analysis_v2$word == data_for_analysis_v2$N2))


use = data_for_analysis_v2b$rt > 200 & data_for_analysis_v2b$rt < 5000 #change in accordance to Boyce
data_for_analysis_v2b = data_for_analysis_v2b[use,]
hist(log(data_for_analysis_v2b$rt))

data_for_analysis_v2b$rt = log(data_for_analysis_v2b$rt) #log transform it --> double check with what Boyce does
hist(data_for_analysis_v2b$rt)



data_for_analysis_v2b = rename(data_for_analysis_v2b, plausibility = type)

data_for_analysis_v2b$group = as.numeric(data_for_analysis_v2b$group)
data_for_analysis_v2b$word_num = as.numeric(data_for_analysis_v2b$word_num)
data_for_analysis_v2b$correct = as.numeric(data_for_analysis_v2b$correct)
data_for_analysis_v2b$rt = as.numeric(data_for_analysis_v2b$rt)
data_for_analysis_v2b$trial_num = as.numeric(data_for_analysis_v2b$trial_num)
data_for_analysis_v2b$subject = as.numeric(data_for_analysis_v2b$subject)
data_for_analysis_v2b$MeanCor = as.numeric(data_for_analysis_v2b$MeanCor)
data_for_analysis_v2b$whichfirst = as.numeric(data_for_analysis_v2b$whichfirst)

data_for_analysis_v2b = rename(data_for_analysis_v2b, deltap = `Delta P of Match Count`)
data_for_analysis_v2b = rename(data_for_analysis_v2b, oddsratio = `Odds Ratio`)


data_for_analysis_v2b$lowerbounddeltap = subset(data_for_analysis_v2b$deltap, data_for_analysis_v2b$deltap > 0)
data_for_analysis_v2b$lowerbounddeltap = ifelse(data_for_analysis_v2b$deltap > 0, data_for_analysis_v2b$deltap, NA)

data_for_analysis_v2b$deltap[data_for_analysis_v2b$deltap < 0] = 0.0000001
data_for_analysis_v2b$logdeltaP = log(data_for_analysis_v2b$deltap)

#center predictors
data_for_analysis_v2b$PlausibilityValue = data_for_analysis_v2b$PlausibilityValue - mean(data_for_analysis_v2b$PlausibilityValue)
data_for_analysis_v2b$logoddsratio = data_for_analysis_v2b$logoddsratio - mean(data_for_analysis_v2b$logoddsratio)

##################### loading data
#write_csv(data_for_analysis_v2b, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/data for analysis.csv")
data_for_analysis_v2b = read_csv("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/data for analysis.csv")
#######################



data_correct_only = data_for_analysis_v2b[!(data_for_analysis_v2b$correct=="no"),]
data_n1 = data_correct_only[(data_correct_only$whichfirst==1),]
data_n2 = data_correct_only[(data_correct_only$whichfirst==0),]
data_correct_and_incorrect = data_for_analysis_v2b
data_correct_and_incorrect_n1 = data_correct_and_incorrect[(data_correct_and_incorrect$whichfirst=="1"),]
data_correct_and_incorrect_n2 = data_correct_and_incorrect[(data_correct_and_incorrect$whichfirst=="0"),]

length(unique(data_n1$subject)) #n=86
length(unique(data_n2$subject)) #n=86
length(data_n1$subject) #tokens = 3189
length(data_n2$subject) #tokens = 3194
###############################
###Time for our model###

##log transform values
##make a table of word and sentence --> don't need to include word? double check by making tables

##to change to treatment coding 
# (contrasts = c('contr.treatment','contr.treatment'))
###

data_n1 = data_n1 %>%
  rename(
    item = group
  )

data_n2 = data_n2 %>%
  rename(
    item = group
  )
## save data_n1 and data_n2

#write_csv(data_n1, "N1 Data.csv")
#write_csv(data_n2, "N2 Data.csv")

graphing_data = data_n1 %>%
  select(plausibility, rt, PlausibilityValue, item, Predictability) %>%
  group_by(plausibility, item, Predictability) %>%
  summarise_each(funs(mean))


graphing_data_v2 = graphing_data %>%
  group_by(item) %>%
  mutate(differenceRT = as.numeric(first(rt)) - as.numeric(last(rt))) %>%
  mutate(differencePlaus = as.numeric(first(PlausibilityValue)) - as.numeric(last(PlausibilityValue)))

graphing_data_v3 = graphing_data_v2 %>%
  group_by(item) %>%
  filter(plausibility == "Implausible")

ggplot(graphing_data_v3, aes(differencePlaus, differenceRT, color=Predictability)) +
  geom_point() +
  stat_smooth(method="lm")


#graphing data is implausible values - plausible values

ggplot(graphing_data_v3, aes(differencePlaus, differenceRT, color=Predictability)) +
  geom_point() +
  stat_smooth(method="lm")

###change to sum coding
options (contrasts = c("contr.sum","cont.sum")) ## set plausible to plausibility1

n1_model_plausibilityvalue =
  brm (rt ~ PlausibilityValue*Predictability + (1 + PlausibilityValue*Predictability|subject) + (1 + PlausibilityValue|item), data = data_n1, chains = 4, cores = 5, 
       warmup = 2000, iter = 7000, thin = 4, control = list(adapt_delta = 0.9),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

n1_model_plausibilityvalue2 =
  brm (rt ~ PlausibilityValue*logoddsratio + (1 + PlausibilityValue*logoddsratio|subject) + (1 + PlausibilityValue|item), data = data_n1, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.9),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

n1_model1 =
  brm (rt ~ plausibility*Predictability + (1 + plausibility*Predictability|subject) + (1 + plausibility|item), data = data_n1, chains = 4, cores = 4, 
       warmup = 1000, iter = 6000, thin = 4,
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

 saveRDS(n1_model1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N1_simple1.rds")

n1_model2 =
  brm (rt ~ plausibility*logoddsratio + (1 + plausibility*logoddsratio|subject) + (1 + plausibility|item), data = data_n1, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))
 saveRDS(n1_model2, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N1_simple2.rds")


n1_model3 =
  brm (rt ~ plausibility*logdeltaP + (1 + plausibility*logdeltaP|subject) + (1 + plausibility|item), data = data_n1, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))
saveRDS(n1_model3, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N1_simple3.rds")

n2_model_plausibilityvalue =
  brm (rt ~ PlausibilityValue*Predictability + (1 + PlausibilityValue*Predictability|subject) + (1 + PlausibilityValue|item), data = data_n2, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.9),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

n2_model_plausibilityvalue2 =
  brm (rt ~ PlausibilityValue*logoddsratio + (1 + PlausibilityValue*logoddsratio|subject) + (1 + PlausibilityValue|item), data = data_n2, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(c(max_treedepth = 15, adapt_delta = 0.99)),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

n2_model1 =
  brm (rt ~ plausibility*Predictability + (1 + plausibility*Predictability|subject) + (1 + plausibility|item), data = data_n2, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))
saveRDS(n2_model1, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N2_simple1.rds")

n2_model2 =
  brm (rt ~ plausibility*logoddsratio + (1 + plausibility*logoddsratio|subject) + (1 + plausibility|item), data = data_n2, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))
saveRDS(n2_model2, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N2_simple2.rds")


n2_model3 =
  brm (rt ~ plausibility*logdeltaP + (1 + plausibility*logdeltaP|subject) + (1 + plausibility|item), data = data_n2, chains = 4, cores = 5, 
       warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
       prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                 set_prior("student_t(3, 0, 100)", class = "b"),
                 set_prior("student_t(3, 0, 100)", class = "sd")))

saveRDS(n2_model3, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N2_simple3.rds")

large_n1_model = 
  brm(rt ~ (Predictability + logoddsratio + logdeltaP)*plausibility + (1 + (Predictability + logoddsratio + logdeltaP)*plausibility|subject) + (1 + plausibility|item), data = data_n1, chains = 4, cores = 5,
      warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
      prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                set_prior("student_t(3, 0, 100)", class = "b"),
                set_prior("student_t(3, 0, 100)", class = "sd")))

saveRDS(large_n1_model, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/Large N1 Model.rds")
large_n2_model = 
  brm(rt ~ (Predictability + logoddsratio + logdeltaP)*plausibility + (1 + (Predictability + logoddsratio + logdeltaP)*plausibility|subject) + (1 + plausibility|item), data = data_n1, chains = 4, cores = 5,
      warmup = 1000, iter = 6000, thin = 4, control = list(adapt_delta = 0.99),
      prior = c(set_prior("student_t(3, 6.83, 100)", class = "Intercept"),
                set_prior("student_t(3, 0, 100)", class = "b"),
                set_prior("student_t(3, 0, 100)", class = "sd")))

saveRDS(large_n2_model, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/Large N2 Model.rds")

n1_model1 = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N1_simple1.rds")
n1_model2 = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N1_simple2.rds")
n1_model3 = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N1_simple3.rds")

n2_model1 = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N2_simple1.rds")
n2_model2 = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N2_simple2.rds")
n2_model3 = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/N2_simple3.rds")

large_n1_model = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/Large N1 Model.rds")
large_n2_model = read_rds("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Maze Task/Large N2 Model.rds")

ggplot(data = data_for_analysis_v2b) +
  geom_histogram(aes(x = logoddsratio, fill = Predictability))

####################Plots##################

n1_plausibilityvalue_plot = conditional_effects(n1_model_plausibilityvalue)
n1_plausibilityvalue2_plot = conditional_effects(n1_model_plausibilityvalue2)

n1_predictability_plot = conditional_effects(n1_model1)
n1_oddsratio_plot = conditional_effects(n1_model2)
n1_deltap_plot = conditional_effects(n1_model3)

n2_plausibilityvalue_plot = conditional_effects(n2_model_plausibilityvalue)
n2_plausibilityvalue2_plot = conditional_effects(n2_model_plausibilityvalue2)

n2_predictability_plot = conditional_effects(n2_model1)
n2_oddsratio_plot = conditional_effects(n2_model2)
n2_deltap_plot = conditional_effects(n2_model3)

n1_long_model_plot = conditional_effects(large_n1_model)
n2_long_model_plot = conditional_effects(large_n2_model)

###no background
#plot(n1_predictability_plot, plot = FALSE)[[3]] +
  #theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #labs(x = "Plausibility Value", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility Value")

###grid only, white background

########### HSP ABSTRACT PLOTS###############################################

#############################################################################


plot1 = plot(n1_predictability_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.7, 7.1)) +
  theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none", axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Log Reaction Time (Log ms)")


plot2 = plot(n2_predictability_plot, plot = FALSE)[[3]] +
  theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Log Reaction Time (Log ms)")

plot3 = plot(n1_predictability_plot2, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.68, 6.96)) +
  theme_bw() + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), legend.position="none", axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"))
grid.arrange(plot3, plot1, plot2, ncol=3, widths = c(2.5,2.5,4), top="Reaction Time(log ms) ~ Plausibility", left="Reaction Time(Log ms)", bottom = "Plausibility")

###########################################################################

############################################################################
plot(n1_plausibilityvalue_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.65, 7.15)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility Value", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility Value")

plot(n1_plausibilityvalue2_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.7, 7)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility Value", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility Value")

plot(n1_predictability_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.7, 7.1)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility")

plot(n1_oddsratio_plot, plot = FALSE)[[3]] +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Log Odds Ratio", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Log Odds Ratio")

plot(n1_deltap_plot, plot = FALSE)[[3]] +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Log \U0394P(N2|N1)", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Log \U0394P(N2|N1)", subtitle = "N1 Region")

plot(n2_plausibilityvalue_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.7, 7)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility Value", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility Value")

plot(n2_plausibilityvalue2_plot, plot = FALSE)[[3]] +
  coord_cartesian(ylim = c(6.5, 7.2)) +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility Value", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility Value")

plot(n2_predictability_plot, plot = FALSE)[[3]] +
  theme_bw() + 
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Plausibility", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Plausibility", subtitle = "N2 Region")

plot(n2_oddsratio_plot, plot = FALSE)[[3]] +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Log Odds Ratio", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Log Odds Ratio", subtitle = "N2 Region")

plot(n2_deltap_plot, plot = FALSE)[[3]] +
  theme_bw() +
  theme(axis.text.x = element_text(color = "black"), axis.text.y = element_text(color = "black"), plot.title = element_text(hjust = 0.5, face="bold"), axis.title = element_text(face="bold")) +
  labs(x = "Log \U0394P(N2|N1)", y = "Log Reaction Time (Log ms)", title = "Log Reaction Time ~ Log \U0394P(N2|N1)", subtitle = "N2 Region")

#############################################

n1_loo1 = loo(n1_model1)
n1_loo2 = loo(n1_model2)
n1_loo3 = loo(n1_model3)

loo_compare(n1_loo1, n1_loo2, n1_loo3)

#           elpd_diff se_diff
#n1_model2  0.0       0.0   
#n1_model3 -0.9       2.1   
#n1_model1 -3.7       3.1 

n2_loo1 = loo(n2_model1)
n2_loo2 = loo(n2_model2)
n2_loo3 = loo(n2_model3)

loo_compare(n2_loo1, n2_loo2, n2_loo3)



#           elpd_diff se_diff
#n2_model2  0.0       0.0   
#n2_model1 -0.9       3.5   
#n2_model3 -1.1       3.3   

model.matrix(~plausibility*Predictability, data_n1)
