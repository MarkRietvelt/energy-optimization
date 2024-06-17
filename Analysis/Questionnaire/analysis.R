# Load Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(xtable)

# Import data
data = read.csv('questionnaire_responses.csv')

# Preprocessing
## Make years of developer experience numeric
data[data$Identifier=='P3', 'Exp.Developer'] = 3
data[data$Identifier=='P7', 'Exp.Developer'] = 10
data[data$Identifier=='P9', 'Exp.Developer'] = 11
data$Exp.Developer = as.numeric(data$Exp.Developer)

## Make factors
### Define function
make_factor = function(df, cols, lvls) {
  for(col in cols) {
    df[,col] = factor(df[,col], levels=lvls)
  }; rm(col)
  return(df)
}
### Agreement scale, questions and factors
agreement_scale = c('Strongly disagree', 'Somewhat disagree', 'Somewhat agree', 'Strongly agree')
agreement_questions = c('Proxies', 'Multiple.Useful', 'Multiple.Hierarchy', 'Proximity', 'Goals.Understand', 'Goals.Hotspot')
data = make_factor(data, agreement_questions, agreement_scale)
### Agreement + no answer scale, questions and factors
agreementplus_questions = c('HC.Useful', 'US.Useful')
agreementplus_scale = c(agreement_scale, '')
data = make_factor(data, agreementplus_questions, agreementplus_scale)
### Frequency scale, questions and factors
frequency_scale = c('Never', 'Rarely', 'Sometimes', 'Often')
frequency_questions = c('Exp.Performance', 'Exp.Energy')
data = make_factor(data, frequency_questions, frequency_scale)
#### Make other questions factor
data$Power = factor(data$Power, levels=c('Power draw is much more useful', 'Power draw is slightly more useful', 'Energy consumption is slightly more useful', 'Energy consumption is much more useful'))
data$Executions = factor(data$Executions, levels=c('A single execution', 'Several executions', 'All possible executions'))
data$CPU = factor(data$CPU, levels=c('Central processing unit is much more useful', 'Central processing unit is slightly more useful', 'External hardware components is slightly more useful', 'External hardware components is much more useful'))
data$HC.Function = factor(data$HC.Function, levels=c('Always one specific hardware combination', 'Often one specific hardware combination', 'Often different hardware combinations', 'Always different hardware combinations'))
data$US.Function = factor(data$US.Function, levels=c('Always one usage scenario', 'Often one usage scenario', 'Often different usage scenarios', 'Always different usage scenarios'))

# Make plots
## Plot MC questions
### Define plot function
mc_plot = function(df, col) {
  return(ggplot(df, aes(x=.data[[col]], fill=.data[[col]])) +
    geom_bar() +
    scale_x_discrete(drop=FALSE))
}
### List questions
mc_questions = c('Proxies', 'Power', 'Executions', 'CPU', 'Multiple.Useful', 'Multiple.Hierarchy', 'Proximity', 'HC.Function', 'HC.Useful', 'US.Function', 'US.Useful', 'Goals.Understand', 'Goals.Hotspot', 'Exp.Performance', 'Exp.Energy')
### Create plots
for(mc_question in mc_questions) {
  png(sprintf('Images/%s.png', mc_question), width=600, height=350)
  print(mc_plot(data,mc_question))
  dev.off() 
}; rm(mc_question)

## Visualize abstraction ranks
### List questions and labels
abstraction_labels = c('Code statements', 'Code branches', 'Functions', 'Source code files', 'The complete project', 'Libraries')
abstraction_questions = paste0('Abstractions.', c('Statements', 'Branches', 'Functions', 'Files', 'Project', 'Libraries'))
### Make subset
rank_data = data[,c('Identifier', abstraction_questions)]
rank_data = rank_data %>% rename_with(~ abstraction_labels, all_of(abstraction_questions))
### Make long format
rank_data <- gather(rank_data, key=Abstraction, value=Rank, 2:7, factor_key=TRUE)
### Convert abstraction numbers to labels
for(i in 1:length(abstraction_labels)) {
  rank_data$Rank[rank_data$Rank==i] = paste('Rank', i)
}; rm(i)
### Order factors
rank_data$Rank = factor(rank_data$Rank, levels=paste('Rank',seq(6,1)))
rank_data$Abstraction = factor(rank_data$Abstraction, levels=abstraction_labels)
### Plot
png('Images/Abstraction.Ranking.png', width=600, height=350)
ggplot(rank_data, aes(x=Identifier, y=Rank, fill=Abstraction)) +
  geom_raster()
dev.off()

## Visualize multiple abstractions
### Make subset
multiple_data = data[,c('Identifier', 'Multiple.Which')]
colnames(multiple_data)[colnames(multiple_data) == 'Multiple.Which'] = 'Abstraction'
### Convert to long format
multiple_data <- multiple_data %>%
  separate_rows(Abstraction, sep=",") 
### Order Abstraction factor
multiple_data$Abstraction = factor(multiple_data$Abstraction, levels=abstraction_labels)
### Plot
png('Images/Abstraction.Multiple.png', width=600, height=350)
ggplot(multiple_data, aes(x=Identifier, y=Abstraction, fill=Abstraction)) +
  geom_raster() +
  theme(legend.position="none") +
  scale_y_discrete(limits=rev)
dev.off()

# Tabulate experience
## Make subset
experience_data = data[,c('Exp.Developer', 'Exp.Performance', 'Exp.Energy', 'Exp.Sector', 'Exp.Applications', 'Exp.Job')]
colnames(experience_data) = c('Experience', 'Performance', 'Energy', 'Sector', 'Application', 'Job')
rownames(experience_data) = data$Identifier
xtable(experience_data)