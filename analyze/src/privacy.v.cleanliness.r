library(lme4)

model.1 <- glmer(expected ~ cleanliness * privacy + (1|participantId), data = expected.posture.df, family = 'binomial')

# privacy.cleanliness.model <- glm(expected ~ privacy * cleanliness, data = expected.posture.df, family = 'binomial')

# print('The baseline is a private toilet of unspecified cleanliness, and the response variable is whether the societally expected posture will be used.')
# print('')
# print('When the toilet switches to a clean private toilet or a clean public toilet, the the likelihood of using the expected posture does not change.')
# print('We might thus see clean private toilets, typical private toilets and clean public toilets as similar. Let\'s call these sorts of toilets "acceptable toilets"')
# print('')
# print('Compared to an acceptable toilet, a toilet that is more dirty or more public is more likely to yeild a societally unexpected posture.')
# print(privacy.cleanliness.model)
# plot(privacy.cleanliness.model)


# sex.privacy.cleanliness.model <- glm(expected ~ sex * privacy * cleanliness, data = expected.posture.df, family = 'binomial')


# sex.task.privacy.cleanliness.model <- glm(expected ~ sex * task * privacy * cleanliness, data = expected.posture.df, family = 'binomial')
# print('1. If a person is defecating at any toilet, it\'s a decent guess that he/she/ze is sitting.')
# print('1.1. The exception is if the toilet is especially dirty; in that case, he/she/ze is likely to hover or to avoid using the toilet at all costs.')
# print('2. If the person is urinating at the private toilet, he/she/ze is a bit less likely to be doing as we expect; men might sit instead of stand, and women might hover instead of sit.')
# print('2.. If the person')
# print('')
# print('')
