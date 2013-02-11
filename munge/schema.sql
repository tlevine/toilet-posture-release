CREATE TABLE demographic (
  participantId INTEGER NOT NULL,
  questionnaire_round TEXT NOT NULL,
  questionnaire_start DATETIME NOT NULL,
  questionnaire_end DATETIME NOT NULL,
  school TEXT NOT NULL,
  consent INTEGER NOT NULL,
  tompkins_county INTEGER NOT NULL,
  building TEXT NOT NULL,
  has_mobility_impairment INTEGER NOT NULL,
  country_of_birth TEXT NOT NULL,
  height INTEGER NOT NULL,
  age INTEGER NOT NULL,
  sex TEXT NOT NULL,
  UNIQUE(participantId, questionnaire_round)
);

-- Concept of public vs private
CREATE TABLE publicness (
  participantId INTEGER NOT NULL,
  questionnaire_round TEXT NOT NULL,
  location TEXT NOT NULL,
  rating TEXT NOT NULL,
  UNIQUE (participantId, questionnaire_round, location),
  FOREIGN KEY (participantId, questionnaire_round)
    REFERENCES demographic(participantId, questionnaire_round)
);

-- Thought experiment
CREATE TABLE [thought.experiment] (
  participantId INTEGER NOT NULL,
  questionnaire_round TEXT NOT NULL,

  -- I specified task, privacy and cleanliness
  task TEXT NOT NULL,
  privacy TEXT NOT NULL,

  -- 'unspecified' if not asked
  cleanliness TEXT NOT NULL,

  -- Response variables
  posture TEXT,
  position TEXT,
  cover_before INTEGER,
  wipe_before INTEGER,
  wipe_after INTEGER,

  -- Thought interventions
  UNIQUE (participantId, questionnaire_round, task, privacy, cleanliness),

  -- Relate
  FOREIGN KEY (participantId, questionnaire_round)
    REFERENCES demographic(participantId, questionnaire_round)
);




CREATE TABLE codebook (
  -- Questionnaire questions mapped to tables
  table_name TEXT NOT NULL,
  column_name TEXT NOT NULL,

  -- Question text if the variable was a question on the questionnaire
  question TEXT NOT NULL,

  -- Additional information
  description TEXT NOT NULL
);

-- Not questions
INSERT INTO codebook VALUES ("demographic", "participantId", "", "Identification number for the participant");
INSERT INTO codebook VALUES ("demographic", "questionnaire_round", "", "Which round of questionnaire-sending? The main test or the retest?");
INSERT INTO codebook VALUES ("demographic", "questionnaire_start", "", "Date and time when the participant started completing the questionnaire, according to Qualtrics.");
INSERT INTO codebook VALUES ("demographic", "questionnaire_end", "", "Date and time when the participant ended completing the questionnaire, according to Qualtrics.");
INSERT INTO codebook VALUES ("demographic", "school", "", "Two-letter code for the school within the university with which the participant was affiliated");
INSERT INTO codebook VALUES ("demographic", "consent", "", "Whether the participant provided consent");

-- Questions in the demographic table
INSERT INTO codebook VALUES ("demographic", "tompkins_county", "Did you live in Tompkins County during the last three months (the spring 2011 semester)?", "true (1) or false(0)");
INSERT INTO codebook VALUES ("demographic", "building", "What sort of building did you live in during the last three months (the spring 2011 semester)?", "this, this or that");
INSERT INTO codebook VALUES ("demographic", "has_mobility_impairment", "Do you currently have any diagnosed mobility impairments?", "true (1) or false (0), but everyone answered false");
INSERT INTO codebook VALUES ("demographic", "country_of_birth", "In what country were you born?", "");
INSERT INTO codebook VALUES ("demographic", "height", "What is your height?", "in inches");
INSERT INTO codebook VALUES ("demographic", "age", "What is your age?", "in years");
INSERT INTO codebook VALUES ("demographic", "sex", "What is your sex? Choose the one that best applies.", "male or female, no other options");

-- Questions in other tables
INSERT INTO codebook VALUES ("publicness", "location", "", "Location can be dorm, house, restaurant or campus. This is four questions combined, one for each location.");
INSERT INTO codebook VALUES ("posture", "task", "", "urinate or defecate");
INSERT INTO codebook VALUES ("posture", "privacy", "", "private or public");
INSERT INTO codebook VALUES ("posture", "cleanliness", "", "clean, dirty or unspecified");


INSERT INTO codebook VALUES ("cleaning", "task", "", "urinate or defecate");
INSERT INTO codebook VALUES ("cleaning", "privacy", "", "private or public");
INSERT INTO codebook VALUES ("cleaning", "position", "", "up or down");
INSERT INTO codebook VALUES ("cleaning", "cover_before", "", "true or false");
INSERT INTO codebook VALUES ("cleaning", "wipe_before", "", "true or false");
INSERT INTO codebook VALUES ("cleaning", "wipe_after", "", "true or false");

