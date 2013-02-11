posture.reliability.df <- sqldf("select a.participantId, a.task, a.privacy, a.cleanliness, a.posture as 'test_posture', b.posture as 'retest_posture' from 'thought.experiment' as 'a' join 'thought.experiment' as 'b' where a.questionnaire_round = 'retest' and b.questionnaire_round = 'test' and a.participantId = b.participantId and a.task = b.task and a.privacy = b.privacy and a.cleanliness = b.cleanliness;", dbname = DB)

