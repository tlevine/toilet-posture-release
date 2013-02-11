privacy.cleanliness.model <- glmer(expected ~ privacy * cleanliness + (1|participantId), data = expected.posture.df, family = 'binomial')

print('The baseline is a private toilet of unspecified cleanliness, and the response variable is whether the societally expected posture will be used.')
print('')
print('When the toilet switches to a clean private toilet or a clean public toilet, the the likelihood of using the expected posture does not change.')
print('We might thus see clean private toilets, typical private toilets and clean public toilets as similar. Let\'s call these sorts of toilets "acceptable toilets"')
print('')
print('Compared to an acceptable toilet, a toilet that is more dirty or more public is more likely to yeild a societally unexpected posture.')
print(privacy.cleanliness.model)
# plot(privacy.cleanliness.model)


sex.privacy.cleanliness.model <- glm(expected ~ sex * privacy * cleanliness, data = expected.posture.df, family = 'binomial')


sex.task.privacy.cleanliness.model <- glm(expected ~ sex * task * privacy * cleanliness, data = expected.posture.df, family = 'binomial')
print('1. If a person is defecating at any toilet, it\'s a decent guess that he/she/ze is sitting.')
print('1.1. The exception is if the toilet is especially dirty; in that case, he/she/ze is likely to hover or to avoid using the toilet at all costs.')
print('2. If the person is urinating at the private toilet, he/she/ze is a bit less likely to be doing as we expect; men might sit instead of stand, and women might hover instead of sit.')
print('2.. If the person')
print('')
print('')




library('ProjectTemplate')
load.project()
expected.posture.df$unacceptable <- expected.posture.df$cleanliness == 'dirty' | ( expected.posture.df$privacy == 'public' & expected.posture.df$cleanliness != 'clean')
expected.posture.model.1 <- glm(expected ~ sex * task * unacceptable, data = expected.posture.df, family = 'binomial')
expected.posture.model.2 <- glm(expected ~ sex * task * unacceptable + participantId, data = expected.posture.df, family = 'binomial')

print('If a male is defecating at an acceptable (This term is defined elsewhere.) toilet, he is quite likely to be doing what we expect (sitting).')
print('  If the toilet is unacceptable, there\'s about a 50-50 chance that he\'ll be sitting.')
print('If a female is defecating at any toilet, she is likely to be doing as we expect (sitting), but not as strongly as is the case for the male who is defecating at an acceptable toilet.')
print('')
print('If anyone is urinating at an acceptable toilet, he/she/ze is a bit less likely to be doing what we expect (standing for men, sitting for women) than if he/she/ze was defecating.')
print('  If the person is male and the toilet is instead unacceptable, he\'s more likely to do what we expect (standing); he might sit instead.')
print('  If the person is female and the toilet is unacceptable, she\'s much less likely to do what we expect (sitting); she might hover instead')
print(summary(expected.posture.model.2))
print('Results are similarly significant regardless of whether we include participantId indicators in the model, except for the sex indicator. Maybe that\'s because participantId explains it?')

posture.df$hover <- posture.df$posture == 'hover'
posture.df$unacceptable <- posture.df$cleanliness == 'dirty' | ( posture.df$privacy == 'public' & posture.df$cleanliness != 'clean')
hover.model.1 <- glm(hover ~ sex * task * unacceptable, data = posture.df, family = 'binomial')
hover.model.2 <- glm(hover ~ sex * task * unacceptable + participantId, data = posture.df, family = 'binomial')
print('Results are similarly significant regardless of whether we include participantId indicators in the model.')

library(lme4)
# Conclusions are the same for the multilevel model.
hover.model.3 <- glmer(hover ~ sex * task * unacceptable  + (1| participantId), data = posture.df, family = 'binomial') 
