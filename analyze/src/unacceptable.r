expected.posture.model.1 <- glm(expected ~ sex * task * unacceptable, data = expected.posture.df, family = 'binomial')
expected.posture.model.2 <- glm(expected ~ sex * task * unacceptable + participantId, data = expected.posture.df, family = 'binomial')

# print('If a male is defecating at an acceptable (This term is defined elsewhere.) toilet, he is quite likely to be doing what we expect (sitting).')
# print('  If the toilet is unacceptable, there\'s about a 50-50 chance that he\'ll be sitting.')
# print('If a female is defecating at any toilet, she is likely to be doing as we expect (sitting), but not as strongly as is the case for the male who is defecating at an acceptable toilet.')
# print('')
# print('If anyone is urinating at an acceptable toilet, he/she/ze is a bit less likely to be doing what we expect (standing for men, sitting for women) than if he/she/ze was defecating.')
# print('  If the person is male and the toilet is instead unacceptable, he\'s more likely to do what we expect (standing); he might sit instead.')
# print('  If the person is female and the toilet is unacceptable, she\'s much less likely to do what we expect (sitting); she might hover instead')
# print(summary(expected.posture.model.2))
# print('Results are similarly significant regardless of whether we include participantId indicators in the model, except for the sex indicator. Maybe that\'s because participantId explains it?')

posture.df$hover <- posture.df$posture == 'hover'
posture.df$unacceptable <- posture.df$cleanliness == 'dirty' | ( posture.df$privacy == 'public' & posture.df$cleanliness != 'clean')
model.1 <- glmer(hover ~ sex * task * unacceptable  + (1| participantId), data = posture.df, family = 'binomial') 

# print('Results are similarly significant regardless of whether we include participantId indicators in the model.')

