Postures of toilet use
===

This is a portion of the secret repository that I use for analysis of
toilet stuff. In order to make sure that I don't bury private in a
public git repository, I reviewed this section and created a new
repository for it.

The only vaguely potentially identifying variables are the school and
country of birth variables. These are probably fine, actually, but they
might be identifying for the smaller schools and countries. So I've
removed these variables from the data.

## Loading the data
First, convert the spreadsheet from Qualtrics into a SQLite database.

    python2 munge/qualtrics_to_sqlite.py munge/raw.csv toilet.db

Next, add the `expected.posture` table. This says things like "Society expects
that men sit while defecating."

    sqlite3 toilet.db < expected.posture.sql

## Analyzing the data
Once the `toilet.db` is prepared, you can run the analysis scripts in the
`analyze` directory. They use [ProjectTemplate](http://projecttemplate.net).
