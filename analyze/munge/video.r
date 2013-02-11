video.posture.df <- posture.df
video.posture.df$posture <- factor(posture.df$posture)
video.posture.df$sex <- factor(posture.df$sex)
video.posture.df$expected <- (posture.df$sex == 'female' & posture.df$posture == 'sit') |
  (posture.df$sex == 'male' & posture.df$task == 'urinate' & posture.df$posture == 'stand') |
  (posture.df$sex == 'male' & posture.df$task == 'defecate' & posture.df$posture == 'sit')
video.posture.df$expected <- factor(video.posture.df$expected, levels = c('TRUE', 'FALSE'))
levels(video.posture.df$expected) <- c('Yes', 'No')

# Counts
video.counts <- dcast(video.posture.df, sex + task + privacy + cleanliness ~ expected, value.var = 'participantId', fun.aggregate = length)
video.counts$Odds <- video.counts$Yes / video.counts$No
video.counts <- video.counts[order(video.counts$Odds),]
video.counts$Level <- factor(paste(video.counts$sex, video.counts$task, video.counts$privacy, video.counts$cleanliness))
video.counts$Level <- factor(video.counts$Level, levels = video.counts$Level)

video.posture.df$Level <- factor(
  paste(video.posture.df$sex, video.posture.df$task, video.posture.df$privacy, video.posture.df$cleanliness),
  levels = video.counts$Level
)


