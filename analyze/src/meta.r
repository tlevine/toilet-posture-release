meta.df <- ddply(expected.posture.df, 'participantId', function(df) {
    df$expected.count <- sum(df$expected)
    df
})

meta.aggregated.df <- sqldf('select sex, expected_count AS "n.expected" from [meta.df] group by participantId')

meta.model <- glm(formula = as.numeric(sex == "female") ~ n.expected, data = meta.aggregated.df)

meta.plot <- ggplot(meta.aggregated.df) + aes(x = n.expected, y = sex) + geom_jitter()
