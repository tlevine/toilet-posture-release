# Model fit, from the MCMCglmm vignette
# 5.3.1 Posterior predictive checks
# These are predictions based on the observed (training) data.

fake.data <- function(m) {
    n.rows <- nrow(m$X)
    posterior.prediction <- matrix(NA, n.rows, nrow(m$Sol))
    for (i in 1:nrow(m$Sol)) {
        .loglikelihood <- rnorm(n.rows, (m$X %*% m$Sol[i,])@x, sqrt(m$VCV[i]))
        .likelihood <- exp(.loglikelihood)
        .p <- (.likelihood) / (1 + .likelihood)
        .p[.likelihood == Inf] <- 1
        .sd <- sqrt( .p * (1 - .p) / n.rows )
        posterior.prediction[,i] <- rnorm(n.rows, .p, .sd) > 0.5
    }
    posterior.prediction
}

# Categorize postures as clean or dirty.
posture.df$dirtyposture <- 0
posture.df$dirtyposture[posture.df$posture %in% c('hover', 'other')] <- 1
posture.df$dirtyposture[posture.df$posture == 'stand' & posture.df$sex == 'female'] <- 1

# Order factor levels from most ordinary to least ordinary.
posture.df$public <- as.numeric(posture.df$privacy == 'public')
posture.df$cleanliness <- factor(posture.df$cleanliness, levels = c('unspecified', 'clean', 'dirty'))
posture.df$dirty <- as.numeric(posture.df$cleanliness == 'dirty')
posture.df$defecate <- as.numeric(posture.df$task == 'defecate')

# Consider females to be more ordinary.
posture.df$male <- as.numeric(posture.df$sex == 'male')

# Cleanliness dummy variables
posture.df$clean <- as.numeric(posture.df$cleanliness == 'clean')
posture.df$cleanlinessdirty <- posture.df$dirty
posture.df$cleanlinessclean <- posture.df$clean

library(MCMCglmm)

nitt <- 150000
burnin <- 100000
quick <- FALSE 
if (quick) {
    nitt <- nitt / 100
    burnin <- burnin / 100
}

set.seed(9001)
posturetype.all.model <- MCMCglmm(
    dirtyposture ~ 1 + cleanliness * defecate * male * public,
    random = ~participantId, family= "categorical",
    nitt = nitt, burnin = burnin,
    data = posture.df
)

posturetype.cleanliness.data <- subset(posture.df,
    cleanliness != 'unspecified' & privacy == 'private')
set.seed(6003)
posturetype.cleanliness.model <- MCMCglmm(
    dirtyposture ~ 1 + dirty * defecate * male,
    random = ~participantId, family= "categorical",
    nitt = nitt, burnin = burnin,
    data = posturetype.cleanliness.data
)

posturetype.privacy.data <- subset(posture.df, cleanliness == 'unspecified')
set.seed(8284)
posturetype.privacy.model <- MCMCglmm(
    dirtyposture ~ 1 + public * defecate * male,
    random = ~participantId, family= "categorical",
    nitt = nitt, burnin = burnin,
    data = posturetype.privacy.data
)

posturetype.clean.data <- subset(posture.df, cleanliness == 'clean')
set.seed(9382)
posturetype.clean.model <- MCMCglmm(
    dirtyposture ~ 1 + public * defecate * male,
    random = ~participantId, family= "categorical",
    nitt = nitt, burnin = burnin,
    data = posturetype.privacy.data
)

# Generate fake data.
set.seed(69723)
posturetype.cleanliness.fake <- fake.data(posturetype.cleanliness.model)
posturetype.privacy.fake <- fake.data(posturetype.privacy.model)
posturetype.clean.fake <- fake.data(posturetype.clean.model)
posturetype.all.fake <- fake.data(posturetype.all.model)

# Check how many times females said they use dirty postures when using
# toilets of unspecified cleanliness in public for both urinating and defecating
posture.df.multivariate <-  dcast(posture.df[c('participantId', 'sex', 'cleanliness', 'task', 'privacy', 'dirtyposture')], participantId + sex ~ cleanliness + task + privacy, value.var = 'dirtyposture')
joint.rate.fit.statistic.real <- sum((posture.df.multivariate$sex == 'female') & posture.df.multivariate$dirty_defecate_public &
    posture.df.multivariate$unspecified_defecate_public & posture.df.multivariate$unspecified_urinate_private) / sum(posture.df.multivariate$sex == 'female')

joint.rate.fit.statistic.fake <- apply(posturetype.all.fake, 2, function(dirtyposture) {
    posture.df.fake <- posture.df
    posture.df.fake$dirtyposture <- dirtyposture
    posture.df.multivariate.fake <- dcast(posture.df.fake[c('participantId', 'sex', 'cleanliness', 'task', 'privacy', 'dirtyposture')],
        participantId + sex ~ cleanliness + task + privacy, value.var = 'dirtyposture')
    fit.statistic.fake <- sum((posture.df.multivariate.fake$sex == 'female') & posture.df.multivariate.fake$dirty_defecate_public &
        posture.df.multivariate.fake$unspecified_defecate_public & posture.df.multivariate.fake$unspecified_urinate_private) /
        sum(posture.df.multivariate.fake$sex == 'female')
    fit.statistic.fake
})


# This checks whether each matrix column is equal to the row
posturetype.cleanliness.accuracy <-
    posturetype.cleanliness.fake == posturetype.cleanliness.data$dirtyposture
posturetype.privacy.accuracy <-
    posturetype.privacy.fake == posturetype.privacy.data$dirtyposture
posturetype.clean.accuracy <-
    posturetype.clean.fake == posturetype.clean.data$dirtyposture

# How often do those match?
pdf('graphs/posturetype-accuracy.pdf', width = 11, height = 8.5)
hist(colMeans(posturetype.cleanliness.accuracy),
  xlab = 'Proportion of model predictions correct',
  ylab = 'Number of simulations')
hist(colMeans(posturetype.privacy.accuracy),
  xlab = 'Proportion of model predictions correct',
  ylab = 'Number of simulations')
hist(colMeans(posturetype.clean.accuracy),
  xlab = 'Proportion of model predictions correct',
  ylab = 'Number of simulations')
dev.off()

# What is the rate of dirtyposture for one of the conditions?
posturetype.cleanliness.rates <- ddply(posturetype.cleanliness.data, c('defecate', 'dirty', 'male'), function(df) {
    .subset <- posturetype.cleanliness.data$defecate == df[1,'defecate'] &
        posturetype.cleanliness.data$dirty == df[1,'dirty'] & 
        posturetype.cleanliness.data$male == df[1,'male']
    c(
        real = mean(posturetype.cleanliness.data[.subset, 'dirtyposture']),
        fake = colMeans(posturetype.cleanliness.fake[.subset,])
    )
})

posturetype.privacy.rates <- ddply(posturetype.privacy.data, c('defecate', 'public', 'male'), function(df) {
    .subset <- posturetype.privacy.data$defecate == df[1,'defecate'] &
        posturetype.privacy.data$public == df[1,'public'] & 
        posturetype.privacy.data$male == df[1,'male']
    c(
        real = mean(posturetype.privacy.data[.subset, 'dirtyposture']),
        fake = colMeans(posturetype.privacy.fake[.subset,])
    )
})

posturetype.clean.rates <- ddply(posturetype.clean.data, c('defecate', 'public', 'male'), function(df) {
    .subset <- posturetype.clean.data$defecate == df[1,'defecate'] &
        posturetype.clean.data$public == df[1,'public'] & 
        posturetype.clean.data$male == df[1,'male']
    c(
        real = mean(posturetype.clean.data[.subset, 'dirtyposture']),
        fake = colMeans(posturetype.clean.fake[.subset,])
    )
})

posturetype.all.data <- posture.df
posturetype.all.rates <- ddply(posturetype.all.data, c('cleanliness', 'defecate', 'public', 'male'), function(df) {
    .subset <- posturetype.all.data$cleanliness == df[1,'cleanliness'] &
        posturetype.all.data$defecate == df[1,'defecate'] &
        posturetype.all.data$public == df[1,'public'] & 
        posturetype.all.data$male == df[1,'male']
    c(
        real = mean(posturetype.all.data[.subset, 'dirtyposture']),
        fake = colMeans(posturetype.all.fake[.subset,])
    )
})

# Plot those rates. They look good.
pdf('graphs/posturetype-fake.v.predicted.pdf', width = 11, height = 8.5)
for (i in 1:nrow(posturetype.all.rates)) {
    hist(t(posturetype.all.rates[i,-(1:5)]), xlim = 0:1, sub = 'All data')
    abline(v = posturetype.all.rates[i, 'real'], col = 2)
}
for (i in 1:nrow(posturetype.cleanliness.rates)) {
    hist(t(posturetype.cleanliness.rates[i,-(1:4)]), xlim = 0:1)
    abline(v = posturetype.cleanliness.rates[i, 'real'], col = 2)
}

for (i in 1:nrow(posturetype.privacy.rates)) {
    hist(t(posturetype.privacy.rates[i,-(1:4)]), xlim = 0:1)
    abline(v = posturetype.privacy.rates[i, 'real'], col = 2)
}

for (i in 1:nrow(posturetype.clean.rates)) {
    hist(t(posturetype.clean.rates[i,-(1:4)]), xlim = 0:1)
    abline(v = posturetype.clean.rates[i, 'real'], col = 2)
}
dev.off()



# -----------
# Get probabilities for each of the eight combinations

# These are within-person probabilities for the average person.
pdf('graphs/posturetype-probabilities.pdf')
models = list(
    cleanliness = posturetype.cleanliness.model,
    privacy = posturetype.privacy.model,
    clean = posturetype.clean.model
)
for (sub in names(models)) {
    df <- combine.relative.loglikelihood.3(models[sub][[1]])
    sapply(1:8, function(i){
      hist(df[,i],
        main = paste('Posture choice in the the', colnames(df)[i], 'condition'),
        xlab = 'Probability that an average person will choose a dirty posture',
        ylab = 'Number of simulations',
        xlim = c(0, 1), sub = paste('For the "', sub, '" model', sep = ''))
    })
}

# Full model
#
# Something might wrong with this one because the answers are
# very polarized.
df <- combine.relative.loglikelihood(posturetype.all.model)
sapply(1:24, function(i){
  hist(df[,i],
    main = paste('Posture choice in the the', colnames(df)[i], 'condition'),
    xlab = 'Probability that an average person will choose a dirty posture',
    ylab = 'Number of simulations', sub = 'Full model (all scenarios)', xlim= 0:1)
})
dev.off()

# So let's do it with fake data.

# These are averages across person
between.cleanliness <- dcast(posturetype.cleanliness.data,
    defecate + male + dirty ~ dirtyposture,
    value.var = 'dirtyposture', fun.aggregate = length
)
between.cleanliness$p <- between.cleanliness[,'1'] / rowSums(between.cleanliness[,c('0', '1')])

between.privacy <- dcast(posturetype.privacy.data,
    defecate + male + public ~ dirtyposture,
    value.var = 'dirtyposture', fun.aggregate = length
)
between.privacy$p <- between.privacy[,'1'] / rowSums(between.privacy[,c('0', '1')])

between.clean <- dcast(posturetype.clean.data,
    defecate + male + public ~ dirtyposture,
    value.var = 'dirtyposture', fun.aggregate = length
)
between.clean$p <- between.clean[,'1'] / rowSums(between.clean[,c('0', '1')])


# Does privacy have an effect independent of cleanliness?
scenarios <- combine.relative.loglikelihood(posturetype.all.model)
scenarios.logodds <- combine.relative.loglikelihood.logodds(posturetype.all.model)

#  1 (Intercept)
#  2 unspecified
#  3 dirty
#  4 defecate
#  5 male
#  6 public
#  7 unspecified:defecate
#  8 dirty:defecate
#  9 unspecified:male
# 10 dirty:male
# 11 defecate:male
# 12 unspecified:public
# 13 dirty:public
# 14 defecate:public
# 15 male:public
# 16 unspecified:defecate:male
# 17 dirty:defecate:male
# 18 unspecified:defecate:public
# 19 dirty:defecate:public
# 20 unspecified:male:public
# 21 dirty:male:public
# 22 defecate:male:public
# 23 unspecified:defecate:male:public
# 24 dirty:defecate:male:public

# Public minus private
public.minus.private <- data.frame(
  scenarios[, 6] - scenarios[, 1],
  scenarios[,12] - scenarios[, 2],
  scenarios[,13] - scenarios[, 3],
  scenarios[,14] - scenarios[, 4],
  scenarios[,15] - scenarios[, 5],
  scenarios[,18] - scenarios[, 7],
  scenarios[,19] - scenarios[, 8],
  scenarios[,20] - scenarios[, 9],
  scenarios[,21] - scenarios[,10],
  scenarios[,22] - scenarios[,11],
  scenarios[,23] - scenarios[,16],
  scenarios[,24] - scenarios[,17]
)
public.minus.private.logodds <- data.frame(
  scenarios.logodds[, 6] - scenarios.logodds[, 1],
  scenarios.logodds[,12] - scenarios.logodds[, 2],
  scenarios.logodds[,13] - scenarios.logodds[, 3],
  scenarios.logodds[,14] - scenarios.logodds[, 4],
  scenarios.logodds[,15] - scenarios.logodds[, 5],
  scenarios.logodds[,18] - scenarios.logodds[, 7],
  scenarios.logodds[,19] - scenarios.logodds[, 8],
  scenarios.logodds[,20] - scenarios.logodds[, 9],
  scenarios.logodds[,21] - scenarios.logodds[,10],
  scenarios.logodds[,22] - scenarios.logodds[,11],
  scenarios.logodds[,23] - scenarios.logodds[,16],
  scenarios.logodds[,24] - scenarios.logodds[,17]
)
.d <- posturetype.all.rates
public.minus.private.fake.prob <- data.frame(
  t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 0 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 0 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 1 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 1 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 1 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 1 & public == 0)[,-(1:5)]),

  t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 0 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 0 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 1 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 1 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 1 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 1 & public == 0)[,-(1:5)]),

  t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 0 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 0 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 1 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 1 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 1 & public == 1)[,-(1:5)]) -
    t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 1 & public == 0)[,-(1:5)])
)

public.minus.private.fake.odds <- data.frame(
  t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 0 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 0 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 1 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'unspecified' & defecate == 0 & male == 1 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 1 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'unspecified' & defecate == 1 & male == 1 & public == 0)[,-(1:5)]),

  t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 0 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 0 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 1 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'clean' & defecate == 0 & male == 1 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 1 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'clean' & defecate == 1 & male == 1 & public == 0)[,-(1:5)]),

  t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 0 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 0 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 0 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 1 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'dirty' & defecate == 0 & male == 1 & public == 0)[,-(1:5)]),
  t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 1 & public == 1)[,-(1:5)]) /
    t(subset(.d,cleanliness == 'dirty' & defecate == 1 & male == 1 & public == 0)[,-(1:5)])
)
names(public.minus.private.fake.prob) <- names(public.minus.private.fake.odds) <- c(
  "(Intercept)",
  "defecate",
  "male",
  "defecate:male",
  "cleanlinessclean:",
  "cleanlinessclean:defecate",
  "cleanlinessclean:male",
  "cleanlinessclean:defecate:male",
  "cleanlinessdirty:",
  "cleanlinessdirty:defecate",
  "cleanlinessdirty:male",
  "cleanlinessdirty:defecate:male"
)

coef.private = c(1:5, 7:11, 16:17)
colnames(public.minus.private) <- colnames(scenarios)[coef.private]
colnames(public.minus.private.fake.prob) <- colnames(scenarios)[coef.private]
colnames(public.minus.private.fake.odds) <- colnames(scenarios)[coef.private]
colnames(public.minus.private.logodds) <- colnames(scenarios.logodds)[coef.private]

pdf('graphs/effect-of-privacy.pdf', width = 11, height = 8.5)
sapply(names(public.minus.private.logodds), function(n){
  hist(public.minus.private.logodds[,n],
    main = n,
    sub = paste('Posterior distribution of the logodds between public and private for the', n, 'scenario'),
    xlab = 'Relative likelihood (log(3) means public is thrice as likely to use a dirty posture, and log(1/3) means private is thrice as likely to use a dirty posture)',
    ylab = 'Frequency'
  )
})
dev.off()

pdf('graphs/effect-of-privacy.fake.prob.pdf', width = 11, height = 8.5)
# Probability differences
sapply(names(public.minus.private.fake.prob), function(n){
  hist(public.minus.private.fake.prob[,n],
    main = n,
    sub = paste('Fake data simulation of the difference in probability of using a dirty posture for the', n, 'scenario'),
    xlab = 'Difference in probability of using a dirty posture (public minus private)',
    ylab = 'Frequency'
  )
})
dev.off()

.d <- public.minus.private.fake.odds
# Remove zeroes
.d[.d < 1e-7] <- 1e-7
public.minus.private.fake.logodds <- log(.d)

# Probability differences
pdf('graphs/effect-of-privacy.fake.odds.pdf', width = 11, height = 8.5)
sapply(names(public.minus.private.fake.logodds), function(n){
  hist(public.minus.private.fake.logodds[,n],
    main = n,
    sub = paste('Fake data simulation of the log-odds-ratio of using a dirty posture for the', n, 'scenario'),
    xlab = 'log(3) means dirty is thrice as likely for public, log(1/3) means dirty is thrice as likely for private',
    ylab = 'Frequency'
  )
})
dev.off()


# A simpler model to check this difference by publicness
public.minus.private.simple.molten <- melt(posture.df, c('participantId', 'male', 'defecate', 'cleanliness', 'public'),
    'dirtyposture')
public.minus.private.simple.cast <- dcast(public.minus.private.simple.molten,
    male + defecate + cleanliness + participantId ~ public, fun.aggregate = mean)
names(public.minus.private.simple.cast)[5:6] <- c('private', 'public')
public.minus.private.simple.cast$eq <- public.minus.private.simple.cast$private == public.minus.private.simple.cast$public

set.seed(2401)
public.minus.private.simple.model <- MCMCglmm(
#   I(public == private) ~ male * defecate * cleanliness,
    eq ~ male * defecate * cleanliness,
    random = ~participantId, family= "categorical",
    nitt = nitt, burnin = burnin,
    data = public.minus.private.simple.cast
)
public.minus.private.simple.post_prob_same <- combine.relative.loglikelihood(public.minus.private.simple.model)
public.minus.private.simple.post_prob_diff <- 1 - public.minus.private.simple.post_prob_same

pdf('graphs/effect-of-privacy.simple.pdf', width = 11, height = 8.5)
sapply(colnames(public.minus.private.simple.post_prob_diff), function(n){
  hist(public.minus.private.simple.post_prob_diff[,n],
    main = n,
    xlab = paste('Probability that posture changes between public and private', n, 'scenarios'),
    sub = 'Computed from the posterior distribution of the I(public == private) ~ male * defecate * cleanliness model',
    ylab = 'Frequency', xlim = 0:1
  )
})
dev.off()

# Even simpler

public.minus.private.simple.crosstabs <- dcast(public.minus.private.simple.cast, male + defecate + cleanliness ~ eq, fun.aggregate = length, value.var = 'eq')
public.minus.private.simple.crosstabs$p.different <- public.minus.private.simple.crosstabs[,'FALSE'] / ( public.minus.private.simple.crosstabs[,'TRUE'] +public.minus.private.simple.crosstabs[,'FALSE'])

# hhhhhh

between.all <- dcast(posture.df,
    cleanliness + defecate + male + public ~ dirtyposture,
    value.var = 'dirtyposture', fun.aggregate = length
)
between.all$p <- between.all[,'1'] / rowSums(between.all[,c('0', '1')])

# Hmm but that model doesn't fit that well; compare
# between.all$p to colMeans(scenarios). On the other hand,
# that might be rounding error.
