posture.reliability.df$same <- factor(
    posture.reliability.df$test_posture == posture.reliability.df$retest_posture,
    levels = c(TRUE, FALSE)
)
levels(posture.reliability.df$same) <- c('Same', 'Different')

reliability.cast <- dcast(
    posture.reliability.df, task + cleanliness + privacy ~ same,
    value.var = 'same', fun.aggregate = length
)
