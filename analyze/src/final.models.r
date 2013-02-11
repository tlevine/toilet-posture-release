model.1 <- glmer(
    as.numeric(expected) ~ cleanliness * privacy * sex + (1|participantId),
    data = expected.posture.df, family = 'binomial')

model.2 <- glmer(
    as.numeric(posture == 'hover') ~ sex * task * unacceptable  + (1| participantId),
    data = posture.df, family = 'binomial')
