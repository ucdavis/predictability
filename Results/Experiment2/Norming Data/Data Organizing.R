##exp5
library(tidyverse)
library(data.table)
setwd("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Norming")
data = read_csv("Norming Stimuli.csv")
cleaned_data = as.tibble(data)

colnames(cleaned_data) <- as.character(unlist(cleaned_data[1,]))
cleaned_data = cleaned_data[-1, ] #fix up the headers

cleaned_data = cleaned_data[, !duplicated(colnames(cleaned_data))] #remove duplicated columns

cleaned_data = cleaned_data %>%
  filter(grepl('English',`Native language (what you learned to speak with your family as a child): - Selected Choice`))

cleaned_data = cleaned_data %>%
  select(`Example: Please rate how well the last word fits in the sentence - My friend has three cats...`:`What do you think this experiment was about?`)

cleaned_data_v2 = cleaned_data %>% #get rid of the people who didn't answer the test q right
  subset(`Example: Please rate how well the last word fits in the sentence - My friend has three cats...` >= "5")

cleaned_data_v3 = cleaned_data_v2 %>% #get rid of the people who didn't answer the test q right
  subset(`Example: Please rate how well the last word fits in the sentence - Jenny went over in her house...` <= "3")


cleaned_data_v0 = cleaned_data_v3

names(cleaned_data_v0) = gsub(pattern = ".*-", replacement = "", x = names(cleaned_data_v0)) #fix up the headers a bit more


 
cleaned_data_v0 = rename(cleaned_data_v0, 'While I was looking at my reflection in the rearview mirror...' = ' While I was looking at my reflection in the rearview mirror..')

mean_values = cleaned_data_v0


name_count = data.frame(cn = names(mean_values)) %>%
  group_by(cn) %>%
  summarize(cnt = n())
name_count # check which columns are duplicated

mean_values = mean_values[, !duplicated(colnames(mean_values))] #remove duplicated columns

name_count = data.frame(cn = names(mean_values)) %>%
  group_by(cn) %>%
  summarize(cnt = n())
name_count # confirm that the duplicated columns were deleted


mean_values_v1 = mean_values %>%
  sapply(as.numeric) #coerce the df into numeric

#mean_values_v2 = as.tibble(mean_values) %>%
  #sapply(as.numeric)

# names(mean_values_v1) = colnames(mean_values) ended up not using this code but saving it for reference

#mean_values_v2 = as.tibble(mean_values_v2)

#mean_values_v3 = bind_rows(mean_values_v2, colMeans(mean_values_v2, na.rm = TRUE)) #doesn't work but rbind does work

#managed to get the last few lines of code to work using tidyverse, keeping it in the script for future reference
  
mean_values_v1 = rbind(mean_values_v1, colMeans(mean_values_v1, na.rm = TRUE))

final_mean_values = as.tibble(mean_values_v1)

organized_data = as.data.frame(t(final_mean_values))

ncol(organized_data)

colnames(organized_data)[ncol(organized_data)] = "Means"
organized_data[1,]

only_means = organized_data %>%
  select("Means")


implausible_items = filter(only_means, only_means[,1] < 4)
plausible_items = filter(only_means, only_means[,1] > 4)

sentence_conditions = read_csv("Sentence Conditions.csv")

###############################unused code######################################################
#organized_sentence_conditions1 = sentence_conditions[,1] #data wrangling
#organized_sentence_conditions1 = organized_sentence_conditions1 %>%
  #mutate("Condition" = "V1N1")

#organized_sentence_conditions2 = sentence_conditions[,2]
#organized_sentence_conditions2 = organized_sentence_conditions2 %>%
  #mutate("Condition" = "V1N1N2")

#organized_sentence_conditions3 = sentence_conditions[,3]
#organized_sentence_conditions3 = organized_sentence_conditions3 %>%
  #mutate("Condition" = "V2N1")

#organized_sentence_conditions4 = sentence_conditions[,4]
#organized_sentence_conditions4 = organized_sentence_conditions4 %>%
  #mutate("Condition" = "V2N1N2")

#organized_sentence_conditions = organized_sentence_conditions1 %>%
  #left_join(organized_sentence_conditions2)

#organized_sentence_conditions1 = rename(organized_sentence_conditions1, Sentences = V1N1)
#organized_sentence_conditions2 = rename(organized_sentence_conditions2, Sentences = V1N1N2)
#organized_sentence_conditions3 = rename(organized_sentence_conditions3, Sentences = V2N1)
#organized_sentence_conditions4 = rename(organized_sentence_conditions4, Sentences = V2N1N2)

#organized_sentence_conditions = organized_sentence_conditions1 %>%
  #full_join(organized_sentence_conditions2)
#organized_sentence_conditions = organized_sentence_conditions %>%
  #full_join(organized_sentence_conditions3)
#organized_sentence_conditions = organized_sentence_conditions %>%
  #full_join(organized_sentence_conditions4)

######################################################################################################

organized_sentence_conditions = sentence_conditions
organized_sentence_conditions$Sentences = trimws(organized_sentence_conditions$Sentences, which = c("left"))
setDT(only_means, keep.rownames = TRUE)
colnames(only_means)[1] = "Sentences"
only_means$Sentences = trimws(only_means$Sentences, which = c("left"))






organized_sentence_conditions = drop_na(organized_sentence_conditions)

final_data = organized_sentence_conditions %>%
  left_join(only_means)

sorted_data = final_data %>%
  group_by(Number)
sorted_data


#write_csv(sorted_data, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Preliminary Norming Data/Exp3.csv")
#write_csv(sorted_data, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Preliminary Norming Data/Normed Data with Staub Items Means.csv")

#sorted_data = read_csv("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Preliminary Norming Data/Normed Data.csv")
#data_with_all_staub_items = read_csv("Normed Data with Staub Items Means.csv")
################# T test 

sentence_conditions = read_csv("Sentence Conditions 2.csv")

organized_sentence_conditions = sentence_conditions
organized_sentence_conditions$Sentences = trimws(organized_sentence_conditions$Sentences, which = c("left"))
setDT(only_means, keep.rownames = TRUE)
colnames(only_means)[1] = "Sentences"
only_means$Sentences = trimws(only_means$Sentences, which = c("left"))






organized_sentence_conditions = drop_na(organized_sentence_conditions)

final_data = organized_sentence_conditions %>%
  left_join(only_means)

sorted_data = final_data %>%
  group_by(Number)
sorted_data

data = sorted_data %>%
  filter(Condition == "V1N1"|Condition == "V2N1")
write_csv(data, "Key.csv")


t_test_data = sorted_data

t_test_data = t_test_data %>%
  filter(Condition == "V1N1"|Condition == "V2N1")

t_test_data_v2 = t_test_data %>%
  group_by(Number) %>%
  mutate(difference = as.numeric(first(Means)) - as.numeric(last(Means)))

t_test_data_v3 = t_test_data_v2 %>%
  filter(Condition == "V1N1")

t_test_data_v4 = t_test_data_v3


t_test_data_v4 = t_test_data_v4 %>%
  mutate(Predictability = ifelse(Whose=="Ours", "High", "Low")) 


low_predictability = t_test_data_v4 %>%
  filter(Predictability=="Low")
high_predictability = t_test_data_v4 %>%
  filter(Predictability=="High")

t_test = t.test(low_predictability$difference, high_predictability$difference)
t_test
par(mfrow =c(1,2))
hist(low_predictability$difference)
hist(high_predictability$difference)

library(gginference)
ggttest(t.test(low_predictability$difference, high_predictability$difference))

plot1 = ggttest(t.test(low_predictability$difference, high_predictability$difference))

################### determine staub (and our) items with lowest difference ################

low_predictability_test = low_predictability %>%
  arrange(difference)

low_predictability_test = low_predictability_test[1:22,]

low_predictability_test = low_predictability_test[-2,]

high_predictability_test = high_predictability %>%
  arrange(difference)

high_predictability_test = high_predictability_test[5:25,]
  
mean(high_predictability_test$difference)
mean(low_predictability_test$difference)

library(gginference)

#data_for_key = high_predictability_test %>%
  #full_join(low_predictability_test)


ggttest(t.test(low_predictability_test$difference, high_predictability_test$difference)) +
  labs(title="Low Predictability Items vs High Predictability Items")

plot1 = ggttest(t.test(low_predictability_test$difference, high_predictability_test$difference))

###############################################

mean(low_predictability_test$difference)

low_difference_items = sorted_data

low_difference_items = low_difference_items %>%
  filter(Condition == "V1N1"|Condition == "V2N1")

low_difference_items_v2 = low_difference_items %>%
  group_by(Number) %>%
  mutate(difference = as.numeric(first(Means)) - as.numeric(last(Means)))

low_difference_items_v3 = low_difference_items_v2 %>%
  filter(Condition == "V1N1")

low_difference_items_v4 = low_difference_items_v3

low_difference_items_v4 = low_difference_items_v4[1:33,] %>%
  mutate(Predictability = "High")



low_difference_items_v5 = low_difference_items_v3

low_difference_items_v5 = low_difference_items_v5[34:74,] %>%
  mutate(Predictability = "Low")


low_predictability = low_difference_items_v5
high_predictability = low_difference_items_v4

low_predictability = low_predictability[order(low_predictability$difference),]
high_predictability = high_predictability[order(-high_predictability$difference),]

low_predictability  = low_predictability[-17,]

mean(low_predictability$difference[1:28])
mean(high_predictability$difference[1:28])


ggttest(t.test(low_predictability$difference[6:30], high_predictability$difference[1:25]))

###through the 24th column gives us a significant difference in the opposite direction

########################

########################### Visualize Data ################################

sorted_data = read_csv("Normed Data with Means.csv")

library(ggplot2)
ggplot(data = sorted_data, aes(x = Sentences, y = Means, fill = factor(Condition) )) +
  geom_bar(stat = "identity",  width = 0.4,
           position=position_dodge(width = 0.5))+
  theme_bw() # way too big to work with this visual

final_data2 <- final_data %>% 
  mutate(counter = 1 + cumsum(c(0,as.numeric(diff(Number))!=0)), # this counter starting at 1 increments for each new Number
         subgroup = as.factor(ceiling(counter/1)))

for (i in 1:length(unique(final_data2$subgroup))) {
    final_data2 %>%
    filter(subgroup == i) -> df 
    

  ggplot(data = df, aes(x = c(Sentences[2]), y = Means, fill = factor(Condition) ))  + xlab("Sentence") +
    geom_bar(stat = "identity",  width = 0.4,
             position=position_dodge(width = 0.5))+
    theme_bw() -> graphic 
  print(graphic)
} ### bar graph for each Number

###export graphs

plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

file.copy(from=plots.png.paths, to="D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Norming/Graphs")

plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, "D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Norming/Graphs", row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0("D:/PhD Stuff/Linguistics Stuff/Staub Replication/Maze Results/Exp5/Norming/Graphs/", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)
#######################################################################

write_csv(organized_sentence_conditions, "organized_sentence_conditions.csv")
write_csv(only_means, "Mean_Values.csv")
write_csv(final_data, "Final Data.csv")


### shortcut for loading data

only_means = read_csv("Mean_Values.csv")
organized_sentence_conditions = read_csv("organized_sentence_conditions.csv")
final_data = read_csv("Final Data.csv")
### ^^^^^^^


view(final_data)



### setting up maze date for maze task ###

library(tidyverse)
library(data.table)
setwd("D:/PhD Stuff/Linguistics Stuff/Staub Replication/")
data = read_csv("Maze Data - Our Maze Data.csv")
data
data_v = data[!(data$Condition=="V1N1" | data$Condition=="V2N1"),]
data_v1 = data.frame(lapply(data_v, function(x) {
  gsub("V1N1N2", "Plausible", x)
}))

data_v2 = data.frame(lapply(data_v1, function(x) {
  gsub("V2N1N2", "Implausible", x)
}))

write_csv(data_v2, "Our Maze Data.csv")





