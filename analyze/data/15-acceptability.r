expected.posture.df$unacceptable <- expected.posture.df$cleanliness == 'dirty' | ( expected.posture.df$privacy == 'public' & expected.posture.df$cleanliness != 'clean')

posture.df$unacceptable <- posture.df$cleanliness == 'dirty' |
    ( posture.df$privacy == 'public' & posture.df$cleanliness != 'clean' )
posture.df$acceptable <- !posture.df$unacceptable
