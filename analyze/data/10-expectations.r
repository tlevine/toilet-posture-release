if ('expected.posture.df' %in% ls()){
  rm(expected.posture.df)
}
expected.posture.df <- sqldf('select * from "expected.posture.df"', dbname = DB)
expected.posture.df$expected <- as.logical(expected.posture.df$expected)
expected.posture.df$participantId <- factor(expected.posture.df$participantId)

# Order these so that the first one is more likely to follow societal expectations.
expected.posture.df$sex <- factor(expected.posture.df$sex,
  levels = c('male', 'female'))
expected.posture.df$task <- factor(expected.posture.df$task,
  levels = c('defecate', 'urinate'))

# Put privacy in order of desireability
expected.posture.df$privacy <- factor(expected.posture.df$privacy,
  levels = c('private', 'public'))

# Put 'unspecified' first for cleanliness, then order it by desirability.
expected.posture.df$cleanliness <- factor(expected.posture.df$cleanliness,
  levels = c('unspecified', 'clean', 'dirty'))

# expected.posture.network <- melt(expected.posture.df, c('participantId', 'sex', 'task'))
# .personal.expectations <- function(df){
#   # df has six rows for privacy * cleanliness
#   df <- df[c('privacy', 'cleanliness', 'expected')]
#   df$privacy
# })

# ddply(expected.posture.df, c('participantId'

# # One row to many doesn't look easy in SQL.
# .posture.network <- function(df){
#   # df has six rows for privacy * cleanliness
#   row.names(df) <- paste(df$privacy, df$cleanliness)
#   df <- df[c('expected')]
#   df['private unspecified','
# });
# ddply(expected.posture.df, c('sex', 'participantId', 'task'), .posture.network)


# sqldf("SELECT sum(expected) AS 'count.expected', count(expected) AS 'count.total' FROM \"expected.posture.df\" GROUP BY participantId")
