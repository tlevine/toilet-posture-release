DROP TABLE IF EXISTS "expected.posture";
CREATE TABLE "expected.posture" (
  sex TEXT NOT NULL,
  task TEXT NOT NULL,
  expected TEXT NOT NULL,
  UNIQUE(sex, task)
);
INSERT INTO "expected.posture" VALUES ('male',   'defecate', 'sit'  );
INSERT INTO "expected.posture" VALUES ('female', 'defecate', 'sit'  );
INSERT INTO "expected.posture" VALUES ('male',   'urinate',  'stand');
INSERT INTO "expected.posture" VALUES ('female', 'urinate',  'sit'  );

-- Write the queries here so I don't need to do it in R.
DROP VIEW IF EXISTS "expected.posture.df";
CREATE VIEW "expected.posture.df" AS
  SELECT
    "thought.experiment".participantId,
    "thought.experiment".task,
    "thought.experiment".privacy,
    "thought.experiment".cleanliness,
    "demographic".sex,
    "thought.experiment".posture = "expected.posture".expected AS 'expected'
  FROM "thought.experiment"
  JOIN demographic ON "thought.experiment".participantId = demographic.participantId
  JOIN "expected.posture"
    ON "thought.experiment".task = "expected.posture".task
    AND demographic.sex = "expected.posture".sex
  WHERE "thought.experiment".questionnaire_round = 'test';
