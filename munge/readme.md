This converts the spreadsheet from Qualtrics into a SQLite database with
the schema in `schema.sql`. Run it like so.

    python2 qualtrics_to_sqlite.py [input spreadsheet] [output database]

For example,

    python2 qualtrics_to_sqlite.py raw.csv /tmp/foo.db

