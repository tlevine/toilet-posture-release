# source('src/final.models.r')

#
# Real and fake cross-tabulations by model
#

# How many replications is the simulation?
n <- 2
n <- 100

set.seed(202)

crosstabs.real.1 <- dcast(expected.posture.df, sex + task + privacy + cleanliness ~ as.numeric(expected), value.var = 'sex', fun.aggregate = length) 
crosstabs.real.1$odds <- crosstabs.real.1[,'1'] / crosstabs.real.1[,'0']
crosstabs.fake.1 <- crosstabs.fake.fn(model.1, model.1.fake.participant)
for (i in 2:n) {
  crosstabs.fake.1 <- rbind(crosstabs.fake.1, crosstabs.fake.fn(model.1, model.1.fake.participant))
}

crosstabs.real.2 <- dcast(posture.df, sex + task + privacy + cleanliness ~ as.numeric(posture == 'hover'), value.var = 'sex', fun.aggregate = length)
crosstabs.real.2$odds <- crosstabs.real.2[,'1'] / crosstabs.real.2[,'0']
crosstabs.fake.2 <- crosstabs.fake.fn(model.2, model.2.fake.participant)
for (i in 2:n) {
  crosstabs.fake.2 <- rbind(crosstabs.fake.2, crosstabs.fake.fn(model.2, model.2.fake.participant))
}

# Plot, and relate the real to the fake
print('Plotting model 1')
pdf('graphs/simulations.1.pdf')
id.vars <- c('sex', 'task', 'privacy', 'cleanliness')
d_ply(crosstabs.fake.1, id.vars, function(crosstabs.fake.slice) {
  print(nrow(crosstabs.fake.slice))
  print(plot.crosstabs(crosstabs.real.1, crosstabs.fake.slice))
})
dev.off()

print('Plotting model 2')
pdf('graphs/simulations.2.pdf')
id.vars <- c('sex', 'task', 'privacy', 'cleanliness')
d_ply(crosstabs.fake.2, id.vars, function(crosstabs.fake.slice) {
  print(plot.crosstabs(crosstabs.real.2, crosstabs.fake.slice))
})
dev.off()

