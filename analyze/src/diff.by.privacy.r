library(scales)

# A simpler model to check this difference by privacyness
scenario <- dcast(
    posture.df,
    sex + task + cleanliness + participantId ~ privacy,
    value.var = 'dirtyposture'
)
scenario$eq <- scenario$public == scenario$private

person <- dcast(scenario, participantId + sex ~ cleanliness + task, value.var = 'eq')
privacy.manova <- manova(as.matrix(person[-(1:2)]) ~ person$sex)

privacy.crosstabs <- ddply(person, 'sex', function(df) { colMeans(df[-(1:2)]) })

privacy.p <- ggplot(melt(privacy.crosstabs)) +
    aes(x = variable, y = value, color = sex, group = sex) + geom_line() +
    aes(ymin = value - 0.08, ymax = sapply(value + 0.12, function(i){ min(1, i)})) +
    geom_errorbar(width = 0.1) +
    scale_x_discrete('Scenario') +
    scale_y_continuous('Percentage where public and private are the same', labels = percent, limits = 0:1) +
    labs(title = 'Do people use different postures for public and private toilets?')

privacy.person.df <- person
