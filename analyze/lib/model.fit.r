fake.ranef <- function(model) {
  sample(model@ranef, size = 1, replace = T)[1]
}
fake.sex  <- function() {
  if (runif(1)[1] < (79 / (79 + 110)) ) {'male'} else {'female'}
}

crosstabs.fake.fn <- function(model, fake.participant.func) {
  df <- adply(1:179, 1, function(i) {
    fake.participant.func(model, i)
  })
  crosstabs <- dcast(df, sex + task + privacy + cleanliness ~ as.numeric(y.simulated), value.var = 'y.simulated', fun.aggregate = length)
  crosstabs$odds <- crosstabs[,'1'] / crosstabs[,'0']
  crosstabs
}

fake.layout <- function(participantId) {
  data.frame(
    participantId = round(participantId),
    sex = fake.sex(),
    privacy = factor(rep(c('private', 'public'), each = 6), levels = c('private', 'public')),
    cleanliness = factor(rep(c('unspecified', 'clean', 'dirty'), each = 2), levels =  c('unspecified', 'clean', 'dirty')),
    task = factor(c('urinate', 'defecate'), levels = c('urinate', 'defecate'))
  )
}

model.1.fake.participant <- function(model.1, participantId) {
  loglikelihood.participant <- fake.ranef(model.1)
  fake.data <- fake.layout(participantId)
  s <- summary(model.1)

  # Main effects
  fake.data$y.loglikelihood <- loglikelihood.participant + 
    rnorm(1, s@coefs['(Intercept)','Estimate'], s@coefs['(Intercept)','Std. Error'])
  fake.data$y.loglikelihood[fake.data$cleanliness == 'clean'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'clean'] +
    rnorm(1, s@coefs['cleanlinessclean','Estimate'], s@coefs['cleanlinessclean','Std. Error'])
  fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty'] +
    rnorm(1, s@coefs['cleanlinessdirty','Estimate'], s@coefs['cleanlinessdirty','Std. Error'])
  fake.data$y.loglikelihood[fake.data$privacy == 'public'] <-
    fake.data$y.loglikelihood[fake.data$privacy == 'public'] +
    rnorm(1, s@coefs['privacypublic','Estimate'], s@coefs['privacypublic','Std. Error'])
  fake.data$y.loglikelihood[fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$sex == 'female'] +
    rnorm(1, s@coefs['sexfemale','Estimate'], s@coefs['sexfemale','Std. Error'])

  # Second-order
  fake.data$y.loglikelihood[fake.data$cleanliness == 'clean' & fake.data$privacy == 'public'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'clean' & fake.data$privacy == 'public'] + 
    rnorm(1, s@coefs['cleanlinessclean:privacypublic','Estimate'], s@coefs['cleanlinessclean:privacypublic','Std. Error'])
  fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty' & fake.data$privacy == 'public'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty' & fake.data$privacy == 'public'] + 
    rnorm(1, s@coefs['cleanlinessdirty:privacypublic','Estimate'], s@coefs['cleanlinessdirty:privacypublic','Std. Error'])
  fake.data$y.loglikelihood[fake.data$cleanliness == 'clean' & fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'clean' & fake.data$sex == 'female'] + 
    rnorm(1, s@coefs['cleanlinessclean:sexfemale','Estimate'], s@coefs['cleanlinessclean:sexfemale','Std. Error'])
  fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty' & fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty' & fake.data$sex == 'female'] + 
    rnorm(1, s@coefs['cleanlinessdirty:sexfemale','Estimate'], s@coefs['cleanlinessdirty:sexfemale','Std. Error'])
  fake.data$y.loglikelihood[fake.data$privacy == 'public' & fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$privacy == 'public' & fake.data$sex == 'female'] + 
    rnorm(1, s@coefs['privacypublic:sexfemale','Estimate'], s@coefs['privacypublic','Std. Error'])

  # Third-order
  fake.data$y.loglikelihood[fake.data$cleanliness == 'clean' & fake.data$privacy == 'public' & fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'clean' & fake.data$privacy == 'public' & fake.data$sex == 'female'] + 
    rnorm(1, s@coefs['cleanlinessclean:privacypublic:sexfemale','Estimate'], s@coefs['cleanlinessclean:privacypublic:sexfemale','Std. Error'])
  fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty' & fake.data$privacy == 'public' & fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$cleanliness == 'dirty' & fake.data$privacy == 'public' & fake.data$sex == 'female'] + 
    rnorm(1, s@coefs['cleanlinessdirty:privacypublic:sexfemale','Estimate'], s@coefs['cleanlinessdirty:privacypublic:sexfemale','Std. Error'])

  # Is the expected posture used?
  # Randomly guess based on the log likelihood
  fake.data$y.likelihood <- exp(fake.data$y.loglikelihood)
  fake.data$y.simulated <- apply(fake.data, 1, function(vec){
    likelihood <- as.numeric(vec['y.likelihood'])
    p <- likelihood / (1 + likelihood)
    sd <- sqrt( p * (1 - p) / (s@ngrps) )
    rnorm(1, p - 0.5, sd)[1] > 0
  })

  fake.data
}

model.2.fake.participant <- function(model.2, participantId) {
  loglikelihood.participant <- fake.ranef(model.2)
  fake.data <- fake.layout(participantId)
  s <- summary(model.2)

  # Main effects
  fake.data$y.loglikelihood <- loglikelihood.participant + 
    rnorm(1, s@coefs['(Intercept)','Estimate'], s@coefs['(Intercept)','Std. Error'])
  fake.data$y.loglikelihood[fake.data$sex == 'female'] <-
    fake.data$y.loglikelihood[fake.data$sex == 'female'] +
    rnorm(1, s@coefs['sexfemale','Estimate'], s@coefs['sexfemale','Std. Error'])
  fake.data$y.loglikelihood[fake.data$task == 'urinate'] <-
    fake.data$y.loglikelihood[fake.data$task == 'urinate'] +
    rnorm(1, s@coefs['taskurinate','Estimate'], s@coefs['taskurinate','Std. Error'])
  fake.data$y.loglikelihood[fake.data$unacceptable == TRUE] <-
    fake.data$y.loglikelihood[fake.data$unacceptable == TRUE] +
    rnorm(1, s@coefs['unacceptableTRUE','Estimate'], s@coefs['unacceptableTRUE','Std. Error'])

  # Second-order
  fake.data$y.loglikelihood[fake.data$sex == 'female' & fake.data$task == 'urinate'] <-
    fake.data$y.loglikelihood[fake.data$sex == 'female' & fake.data$task == 'urinate'] + 
    rnorm(1, s@coefs['sexfemale:taskurinate','Estimate'], s@coefs['sexfemale:taskurinate','Std. Error'])
  fake.data$y.loglikelihood[fake.data$sex == 'female' & fake.data$unacceptable == TRUE] <-
    fake.data$y.loglikelihood[fake.data$sex == 'female' & fake.data$unacceptable == TRUE] + 
    rnorm(1, s@coefs['sexfemale:unacceptableTRUE','Estimate'], s@coefs['sexfemale:unacceptableTRUE','Std. Error'])
  fake.data$y.loglikelihood[fake.data$task == 'urinate' & fake.data$unacceptable == TRUE] <-
    fake.data$y.loglikelihood[fake.data$task == 'urinate' & fake.data$unacceptable == TRUE] + 
    rnorm(1, s@coefs['taskurinate:unacceptableTRUE','Estimate'], s@coefs['taskurinate:unacceptableTRUE','Std. Error'])

  # Third-order
  fake.data$y.loglikelihood[fake.data$sex == 'female' & fake.data$task == 'urinate' & fake.data$unacceptable == TRUE] <-
    fake.data$y.loglikelihood[fake.data$sex == 'female' & fake.data$task == 'urinate' & fake.data$unacceptable == TRUE] + 
    rnorm(1, s@coefs['sexfemale:taskurinate:unacceptableTRUE','Estimate'], s@coefs['sexfemale:taskurinate:unacceptableTRUE','Std. Error'])

  # Is the expected posture used?
  # Randomly guess based on the log likelihood
  fake.data$y.likelihood <- exp(fake.data$y.loglikelihood)
  fake.data$y.simulated <- apply(fake.data, 1, function(vec){
    likelihood <- as.numeric(vec['y.likelihood'])
    p <- likelihood / (1 + likelihood)
    sd <- sqrt( p * (1 - p) / (s@ngrps) )
    rnorm(1, p - 0.5, sd)[1] > 0
  })

  fake.data
}
