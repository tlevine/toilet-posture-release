library('ProjectTemplate')
load.project()
library(ggplot2)

ggplot(subset(posture.df, cleanliness == 'unspecified')) + aes(x = task, y = privacy, color = posture, group = sex)
ggplot(subset(posture.df, cleanliness == 'unspecified' & privacy == 'public' & task == 'urinate' & sex == 'male')) + aes(x = sex, fill = posture) + coord_polar(theta = 'y') + geom_bar()
ggplot(posture.df.counts) + aes(x = paste(task, privacy), y = posture, color = sex, size = count) + geom_point()

ggplot(expected.posture.df) + aes(x = privacy, y = cleanliness, color = expected.posture, group = participantId) + geom_point()

print('Of the times that "other" was marked, the scenario was usually a dirty toilet. I thus interpret "other" as meaning that they wouldn\'t use the toilet, no matter what.')
subset(posture.df, posture == 'other')

ddply(posture.df, 'participantId', function(df){length(unique(df$posture))})

#Counts of males and females in the simulation
hist(rowSums(crosstabs.fake.1[,c('0', '1')]))  

ddply(subset(posture.df, posture == "other"), id.vars, nrow) 
