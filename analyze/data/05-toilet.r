# codebook <- sqldf('select * from codebook', dbname = DB)
# thought.experiment <- sqldf('select * from "thought.experiment"', dbname = DB)
# demographic <- sqldf('select * from demographic', dbname = DB)
# publicness <- sqldf('select * from publicness', dbname = DB)

# Each row is a person
.sql <- "select \"thought.experiment\".participantId, task, privacy, cleanliness, posture, sex from \"thought.experiment\" join demographic on \"thought.experiment\".participantId = demographic.participantId where \"thought.experiment\".questionnaire_round = 'test' and \"demographic\".questionnaire_round = 'test'"
posture.df <- sqldf(.sql, dbname = DB)
posture.df[is.na(posture.df$posture),]$posture <- 'did.not.respond'

posture.df$participantId <- factor(posture.df$participantId)

# Order these so that the first one is less likely to follow societal expectations.
posture.df$sex <- factor(posture.df$sex,
  levels = c('male', 'female'))
posture.df$task <- factor(posture.df$task,
  levels = c('defecate', 'urinate'))

# Put privacy in order of desireability
posture.df$privacy <- factor(posture.df$privacy,
  levels = c('private', 'public'))

# Put 'unspecified' first for cleanliness, then order it by desirability.
posture.df$cleanliness <- factor(posture.df$cleanliness,
  levels = c('unspecified', 'clean', 'dirty'))

# print('posture.df groups people who answered or "other", "squat" for posture or who did not respond')
posture.df[posture.df$posture %in% c('did.not.respond', 'other', 'squat'),]$posture <- 'other'


# Aggregate persons
.sql <- "select task, privacy, cleanliness, posture, sex, count(*) as \"count\" from \"thought.experiment\" join demographic on \"thought.experiment\".participantId = demographic.participantId where \"thought.experiment\".questionnaire_round = 'test' group by task, privacy, cleanliness, posture, sex and \"demographic\".questionnaire_round = 'test'"
posture.df.counts <- sqldf(.sql, dbname = DB)
posture.df.counts[is.na(posture.df.counts$posture),]$posture <- 'did.not.respond'
posture.df.counts <- subset(posture.df.counts, posture %in% c('hover', 'sit', 'stand'))

# print('posture.df.counts ignores 7 people who answered or "other", "squat" for posture or who did not respond')
sum(subset(posture.df.counts, posture == 'other')$count) / 12

# Clean up
rm(.sql)
