# Subset for testing
# video.posture.df <- video.posture.df[1:120,]

plot.no.cleanliness <- function(.sex) {
  pdf(paste('graphs/no.cleanliness', .sex, 'pdf', sep = '.'))
  d_ply(subset(video.posture.df, sex == .sex), 'participantId', function(df){
    p <- ggplot(subset(df, cleanliness == 'unspecified')) +
      aes(y = Level, x = posture, fill = expected) +
      scale_y_discrete('Scenario', drop = T) + 
      scale_x_discrete('Posture', drop = F) + geom_tile() +
      scale_fill_discrete('Was the expected posture used?', drop = F) +
      labs(title = paste('Participant ', df[1,'participantId'], ' (', df[1,'sex'], ')', sep = ''))

    print(p)
  }, .progress = 'text')
  dev.off()
}

plot.cleanliness <- function(.sex) {
  pdf(paste('graphs/cleanliness', .sex, 'pdf', sep = '.'))
  d_ply(subset(video.posture.df, sex == .sex), 'participantId', function(df){
    p <- ggplot(subset(df, cleanliness != 'unspecified')) +
      aes(y = Level, x = posture, fill = expected) +
      scale_y_discrete('Scenario', drop = T) + 
      scale_x_discrete('Posture', drop = F) + geom_tile() +
      scale_fill_discrete('Was the expected posture used?', drop = F) +
      labs(title = paste('Participant ', df[1,'participantId'], ' (', df[1,'sex'], ')', sep = ''))

    print(p)
  }, .progress = 'text')
  dev.off()
}

plot.extremes <- function(.sex) {
  pdf(paste('graphs/extremes', .sex, 'pdf', sep = '.'))
  d_ply(subset(video.posture.df, sex == .sex), 'participantId', function(df){
    p <- ggplot(subset(df, (cleanliness == 'clean' & privacy == 'private') | (cleanliness == 'dirty' & privacy == 'public'))) +
      aes(y = Level, x = posture, fill = expected) +
      scale_y_discrete('Scenario', drop = T) + 
      scale_x_discrete('Posture', drop = F) + geom_tile() +
      scale_fill_discrete('Was the expected posture used?', drop = F) +
      labs(title = paste('Participant ', df[1,'participantId'], ' (', df[1,'sex'], ')', sep = ''))

    print(p)
  }, .progress = 'text')
  dev.off()
}

plot.participants <- function(.sex) {
  pdf(paste('graphs/table.video', .sex, 'pdf', sep = '.'))
  d_ply(subset(video.posture.df, sex == .sex), 'participantId', function(df){
    p <- ggplot(df) +
      aes(y = Level, x = posture, fill = expected) +
      scale_y_discrete('Scenario') + 
      scale_x_discrete('Posture', drop = F) + geom_tile() +
      scale_fill_discrete('Was the expected posture used?', drop = F) +
      labs(title = paste('Participant ', df[1,'participantId'], ' (', df[1,'sex'], ')', sep = ''))

    print(p)
  }, .progress = 'text')
  dev.off()
}

plot.no.cleanliness('male')
plot.no.cleanliness('female')
#plot.cleanliness('male')
#plot.cleanliness('female')
#plot.extremes('male')
#plot.extremes('female')
#plot.participants('male')
#plot.participants('female')
