# glmer( posture ~ Level + (1|participantId), data = subset(video.posture.df, sex == 'male'))

model.level.male.privacy <- glmer( expected ~ task * privacy + (1|participantId), data = subset(video.posture.df, sex == 'male' & cleanliness == 'unspecified'))
model.level.male.cleanliness <- glmer( expected ~ task * cleanliness + (1|participantId), data = subset(video.posture.df, sex == 'male' & privacy == 'public'))
model.level.female.privacy <- glmer( expected ~ task * privacy + (1|participantId), data = subset(video.posture.df, sex == 'female' & cleanliness == 'unspecified'))
model.level.female.cleanliness <- glmer( expected ~ task * cleanliness + (1|participantId), data = subset(video.posture.df, sex == 'female' & privacy == 'public'))
