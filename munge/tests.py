#!/usr/bin/env python
# -*- encoding: utf-8 -*-

from collections import OrderedDict
import os
import datetime
import unittest
import dumptruck
from qualtrics_to_sqlite import qualtrics_to_sqlite

# Put everything in RAM for fast test-running
os.system('cp raw.csv schema.sql /tmp')

# Subset the data for even faster test-running
os.system('sed -n -e 1p -e 2p -e /156/p raw.csv > /tmp/raw-participant-156.csv')

RAW = '/tmp/raw.csv'
RAW_156 = '/tmp/raw-participant-156.csv'
SCHEMA = '/tmp/schema.sql'
DBNAME = '/tmp/test_toilet_posture_munge.db'

class TestDb(unittest.TestCase):
    def setUp(self):
        # Delete the database
        self._delete_db()

        # Create the database
        qualtrics_to_sqlite(self.raw, DBNAME)

        # Open a DumpTruck connection.
        self.dt = dumptruck.DumpTruck(dbname = DBNAME)

    def _delete_db(self):
        "Delete the database"
        try:
            os.remove(DBNAME)
        except OSError:
            # File already doesn't exist.
            pass

    def tearDown(self):
        # Delete the database
        self._delete_db()

class TestParticipant156(TestDb):
    raw = RAW_156
    """
   Here's the row from the CSV file from Qualtrics

R_ePu0aK8kDrPqfsg,Survey Phase,2011-05-05 16:35:29,2011-05-05 16:37:40,1,0,0,0,156,AG,,y,1,1,1,2,private,3,3,public,public,1,2,2,2,1,2,2,2,,1,,,,1,,,,1,,,,1,,,no,1,46,75,4,male,1,1,1,5,5,5,5,5,,,,,,,
    """
    def _cells(self, sql, expected_values):
        """Pass sql that returns one row with one cell and the value that it
        should match.
        """
        observed = self.dt.execute(sql)
        self.assertListEqual(observed, expected_values)

    def _cell(self, table, column, value):
        "Pass a table and column that wind up getting one cell and the value that it should have."

        sql = 'SELECT [%(column)s] FROM [%(table)s] WHERE participantId = 156 AND questionnaire_round = "test"'
        observed = self.dt.execute(sql % {'column': column, 'table': table})[0].items()
        expected = [(column, value)]
        self.assertListEqual(observed, expected)

    def test_participantId(self):
        self._cell('demographic', 'participantId', 156)

    def test_questionnaire_round(self):
        self._cell('demographic', 'questionnaire_round', 'test')

    def test_questionnaire_start(self):
        self._cell('demographic', 'questionnaire_start', datetime.datetime.strptime('2011-05-05 16:35:29', '%Y-%m-%d %H:%M:%S'))

    def test_questionnaire_end(self):
        self._cell('demographic', 'questionnaire_end', datetime.datetime.strptime('2011-05-05 16:37:40', '%Y-%m-%d %H:%M:%S'))

    def test_school(self):
        self._cell('demographic', 'school', 'cals')

    def test_consent(self):
        self._cell('demographic', 'consent', 1)

    def test_tompkins_county(self):
        "Participant 156 lived in Tompkins County."
        self._cell('demographic', 'tompkins_county', 1)

    def test_building(self):
        self._cell('demographic', 'building', 'house_apartment')

    def test_has_mobility_impairment(self):
        self._cell('demographic', 'has_mobility_impairment', 0)

    def test_country_of_birth(self):
        self._cell('demographic', 'country_of_birth', 'China')

    def test_height(self):
        self._cell('demographic', 'height', 75)

    def test_age(self):
        # 4? Something's weird. Maybe people lied,
        # or maybe the questionnaire was wrong.
        self._cell('demographic', 'age', 21)

    def test_sex(self):
        self._cell('demographic', 'sex', 'male')

    def test_publicness(self):
        sql = 'SELECT location, rating from publicness WHERE participantId = 156 AND questionnaire_round = "test" ORDER BY location;'
        expectation = [
            {'location': 'campus', 'rating': 'public'},
            {'location': 'dorm', 'rating': 'neither'},
            {'location': 'house', 'rating': 'neither'},
            {'location': 'restaurant', 'rating': 'public'},
            {'location': 'your_residence', 'rating': 'private'},
        ]
        self._cells(sql, expectation)

    def _thought_experiment_row(self, expectation, task, privacy, cleanliness='unspecified'):
        sql = '''
SELECT
  posture,
  position,
  cover_before,
  wipe_before,
  wipe_after
FROM [thought.experiment]
WHERE
  participantId = 156 AND
  questionnaire_round = "test" AND
  task = "%(task)s" AND
  privacy = "%(privacy)s" AND
  cleanliness = "%(cleanliness)s"
;''' % {'task': task, 'privacy': privacy, 'cleanliness': cleanliness}
        self._cells(sql, [expectation])

    def test_thought_experiment_defecate_private_clean(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'sit'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'defecate', 'private', 'clean',
        )

    def test_thought_experiment_defecate_private_dirty(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'sit'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'defecate', 'private', 'dirty',
        )
    def test_thought_experiment_defecate_private_unspecified(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', u'sit'),
                (u'position', u'down'),
                (u'cover_before', 0),
                (u'wipe_before', 1),
                (u'wipe_after', 0)
            ]),
            'defecate', 'private', 'unspecified',
        )

    def test_thought_experiment_defecate_public_clean(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'sit'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'defecate', 'public', 'clean',
        )
    def test_thought_experiment_defecate_public_dirty(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'sit'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'defecate', 'public', 'dirty',
        )
    def test_thought_experiment_defecate_public_unspecified(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', u'sit'),
                (u'position', u'down'),
                (u'cover_before', 0),
                (u'wipe_before', 1),
                (u'wipe_after', 0)
            ]),
            'defecate', 'public', 'unspecified',
        )

    def test_thought_experiment_urinate_private_clean(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'sit'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'urinate', 'private', 'clean',
        )
    def test_thought_experiment_urinate_private_dirty(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'stand'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'urinate', 'private', 'dirty',
        )
    def test_thought_experiment_urinate_private_unspecified(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', u'sit'),
                (u'position', u'down'),
                (u'cover_before', 0),
                (u'wipe_before', 1),
                (u'wipe_after', 0)
            ]),
            'urinate', 'private', 'unspecified',
        )

    def test_thought_experiment_urinate_public_clean(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', 'stand'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'urinate', 'public', 'clean',
        )
    def test_thought_experiment_urinate_public_dirty(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', u'stand'),
                (u'position', None),
                (u'cover_before', None),
                (u'wipe_before', None),
                (u'wipe_after', None),
            ]),
            'urinate', 'public', 'dirty',
        )
    def test_thought_experiment_urinate_public_unspecified(self):
        self._thought_experiment_row(
            OrderedDict([
                (u'posture', u'stand'),
                (u'position', u'up'),
                (u'cover_before', 0),
                (u'wipe_before', 1),
                (u'wipe_after', 0)
            ]),
            'urinate', 'public', 'unspecified',
        )


TEST_RESPONSES = 173
RETEST_RESPONSES = 16

class TestRowCounts(TestDb):
    "Tables and subsets should have the appropriate numbers of rows."
    raw = RAW
    def _count(self, sql, count):
        data = self.dt.execute(sql)
        self.assertEqual(len(data), count)

    # Demographics table
    def test_number_of_retests(self):
        self._count('SELECT * FROM demographic WHERE questionnaire_round = "retest"', RETEST_RESPONSES)

    def test_number_of_tests(self):
        self._count('SELECT * FROM demographic WHERE questionnaire_round = "test"', TEST_RESPONSES)

    def test_number_of_questionnaires(self):
        self._count('SELECT * FROM demographic', TEST_RESPONSES + RETEST_RESPONSES)

    # Other tables
    def test_thought_experiment(self):
        self._count('SELECT * FROM [thought.experiment] WHERE cleanliness = "unspecified"', (TEST_RESPONSES + RETEST_RESPONSES) * 4)
        self._count('SELECT * FROM [thought.experiment]', (TEST_RESPONSES + RETEST_RESPONSES) * 12)

    def test_publicness(self):
        self._count('SELECT * FROM [publicness]', (TEST_RESPONSES + RETEST_RESPONSES)* 5)

class TestBalance(TestDb):
    "Factors should be balanced per the experiment design."
    raw = RAW
    def _balance(self, sql):
        # Number of replicates by level
        counts = set(self.dt.execute(sql)[0])
 
        # All of these counts should be the same
        self.assertEqual(len(counts), 1)

    def test_thought_experiment(self):
        self._balance(
            'SELECT count(*) FROM [thought.experiment]'
            'GROUP BY task || cleanliness || privacy'
        )

    def test_publicness(self):
        self._balance('SELECT count(*) from publicness GROUP BY location')

class TestFactorLevels(TestDb):
    "Each factor should contain only the appropriate levels."

    BOOLEAN = {0, 1}
    raw = RAW

    def _factor(self, table, column, expected_levels, allow_null = False):
        p = (column, table)
        data = self.dt.execute('SELECT DISTINCT [%s] from [%s]' % p)
        observed_levels = {row.values()[0] for row in data}
        observed_subset = {l: None for l in observed_levels}
        expected = {l: None for l in expected_levels}

        if allow_null and None in observed_subset:
            del(observed_subset[None])

        self.assertDictContainsSubset(observed_subset, expected)

    def test_questionnaire_round(self):
        levels = {'test', 'retest'}
        for table in self.dt.tables():
            if table != 'codebook':
                self._factor(table, 'questionnaire_round', levels)

    # Demographics table factors
    def test_school(self):
        levels = [
            'cals',
            'aap',
            'arts and sciences',
            'weill',
            'engineering',
            'management',
            'graduate school',
            'hotel',
            'human ecology',
            'ilr',
            'law',
            'vet',
        ]
        self._factor('demographic', 'school', levels, allow_null = True)

    def test_consent(self):
        self._factor('demographic', 'consent', self.BOOLEAN)

    def test_tompkins_county(self):
        self._factor('demographic', 'tompkins_county', self.BOOLEAN)

    def test_has_mobility_impairment(self):
        self._factor('demographic', 'has_mobility_impairment', self.BOOLEAN)

    def test_sex(self):
        self._factor('demographic', 'sex', { 'male', 'female' })

    def test_country_of_birth(self):
        options = [
            'United States of America',
            'Afghanistan',
            'Åland Islands',
            'Albania',
            'Algeria',
            'American Samoa',
            'Andorra',
            'Angola',
            'Anguilla',
            'Antigua and Barbuda',
            'Argentina',
            'Armenia',
            'Aruba',
            'Australia',
            'Austria',
            'Azerbaijan',
            'Bahamas',
            'Bahrain',
            'Bangladesh',
            'Barbados',
            'Belarus',
            'Belgium',
            'Belize',
            'Benin',
            'Bermuda',
            'Bhutan',
            'Bolivia (Plurinational State of)',
            'Bonaire Saint Eustatius and Saba,BES',
            'Bosnia and Herzegovina',
            'Botswana',
            'Brazil',
            'British Virgin Islands',
            'Brunei Darussalam',
            'Bulgaria',
            'Burkina Faso',
            'Burundi',
            'Cambodia',
            'Cameroon',
            'Canada',
            'Cape Verde',
            'Cayman Islands',
            'Central African Republic',
            'Chad',
            'Channel Islands 	',
            'Chile',
            'China',
            'China Hong Kong Special Administrative Region,HKG',
            'China Macao Special Administrative Region,MAC',
            'Colombia',
            'Comoros',
            'Congo',
            'Cook Islands',
            'Costa Rica',
            'Côte d\'Ivoire',
            'Croatia',
            'Cuba',
            'Curaçao',
            'Cyprus',
            'Czech Republic',
            'Democratic People\'s Republic of Korea',
            'Democratic Republic of the Congo',
            'Denmark',
            'Djibouti',
            'Dominica',
            'Dominican Republic',
            'Ecuador',
            'Egypt',
            'El Salvador',
            'Equatorial Guinea',
            'Eritrea',
            'Estonia',
            'Ethiopia',
            'Faeroe Islands',
            'Falkland Islands (Malvinas)',
            'Fiji',
            'Finland',
            'France',
            'French Guiana',
            'French Polynesia',
            'Gabon',
            'Gambia',
            'Georgia',
            'Germany',
            'Ghana',
            'Gibraltar',
            'Greece',
            'Greenland',
            'Grenada',
            'Guadeloupe',
            'Guam',
            'Guatemala',
            'Guernsey',
            'Guinea',
            'Guinea-Bissau',
            'Guyana',
            'Haiti',
            'Holy See',
            'Honduras',
            'Hungary',
            'Iceland',
            'India',
            'Indonesia',
            'Iran (Islamic Republic of)',
            'Iraq',
            'Ireland',
            'Isle of Man',
            'Israel',
            'Italy',
            'Jamaica',
            'Japan',
            'Jersey',
            'Jordan',
            'Kazakhstan',
            'Kenya',
            'Kiribati',
            'Kuwait',
            'Kyrgyzstan',
            'Lao People\'s Democratic Republic',
            'Latvia',
            'Lebanon',
            'Lesotho',
            'Liberia',
            'Libyan Arab Jamahiriya',
            'Liechtenstein',
            'Lithuania',
            'Luxembourg',
            'Madagascar',
            'Malawi',
            'Malaysia',
            'Maldives',
            'Mali',
            'Malta',
            'Marshall Islands',
            'Martinique',
            'Mauritania',
            'Mauritius',
            'Mayotte 	MYT',
            'Mexico',
            'Micronesia (Federated States of)',
            'Monaco',
            'Mongolia',
            'Montenegro',
            'Montserrat',
            'Morocco',
            'Mozambique',
            'Myanmar',
            'Namibia',
            'Nauru',
            'Nepal',
            'Netherlands',
            'New Caledonia',
            'New Zealand',
            'Nicaragua',
            'Niger',
            'Nigeria',
            'Niue',
            'Norfolk Island',
            'Northern Mariana Islands',
            'Norway',
            'Occupied Palestinian Territory',
            'Oman',
            'Pakistan',
            'Palau',
            'Panama',
            'Papua New Guinea',
            'Paraguay',
            'Peru',
            'Philippines',
            'Pitcairn',
            'Poland',
            'Portugal',
            'Puerto Rico',
            'Qatar',
            'Republic of Korea',
            'Republic of Moldova',
            'Réunion',
            'Romania',
            'Russian Federation',
            'Rwanda',
            'Saint-Barthélemy',
            'Saint Helena',
            'Saint Kitts and Nevis',
            'Saint Lucia',
            'Saint-Martin (French part',
            'Saint Pierre and Miquelon',
            'Saint Vincent and the Grenadines',
            'Samoa',
            'San Marino',
            'Sao Tome and Principe',
            'Saudi Arabia',
            'Senegal',
            'Serbia',
            'Seychelles',
            'Sierra Leone',
            'Singapore',
            'Sint Maarten (Dutch part',
            'Slovakia',
            'Slovenia',
            'Solomon Islands',
            'Somalia',
            'South Africa',
            'Spain',
            'Sri Lanka',
            'Sudan',
            'Suriname',
            'Svalbard and Jan Mayen Islands',
            'Swaziland',
            'Sweden',
            'Switzerland',
            'Syrian Arab Republic',
            'Tajikistan',
            'Thailand',
            'The former Yugoslav Republic of Macedonia',
            'Timor-Leste',
            'Togo',
            'Tokelau',
            'Tonga',
            'Trinidad and Tobago',
            'Tunisia',
            'Turkey',
            'Turkmenistan',
            'Turks and Caicos Islands',
            'Tuvalu',
            'Uganda',
            'Ukraine',
            'United Arab Emirates',
            'United Kingdom of Great Britain and Northern Ireland',
            'United Republic of Tanzania',
            'United States Virgin Islands',
            'Uruguay',
            'Uzbekistan',
            'Vanuatu',
            'Venezuela (Bolivarian Republic of)',
            'Viet Nam',
            'Wallis and Futuna Islands',
            'Western Sahara',
            'Yemen',
            'Zambia',
            'Zimbabwe ',
        ]

    # Publicness table
    def test_location(self):
        levels = { 'dorm', 'house_apartment', 'other' }
        self._factor('publicness', 'location', levels)

    def test_rating(self):
        levels = { 'public', 'private', 'neither' }
        self._factor('publicness', 'rating', levels)

    # Thought experiment
    def test_task(self):
        self._factor('thought.experiment', 'task', {'urinate', 'defecate'})

    def test_location(self):
        levels = {'public', 'private'}
        self._factor('thought.experiment', 'privacy', levels)

    def test_cleanliness(self):
        levels = {'clean', 'dirty', 'unspecified'}
        self._factor('thought.experiment', 'cleanliness', levels)

    def test_posture(self):
        levels = { 'hover', 'other', 'sit', 'squat', 'stand', None }
        self._factor('thought.experiment', 'posture', levels)

    def test_position(self):
        levels = { 'up', 'down' }
    #   levels = { 'up', 'down', None }
        self._factor('thought.experiment', 'position', levels, allow_null = True)

    def test_cover_before(self):
        self._factor('thought.experiment', 'cover_before', self.BOOLEAN, allow_null = True)

    def test_wipe_before(self):
        self._factor('thought.experiment', 'wipe_before', self.BOOLEAN, allow_null = True)

    def test_wipe_after(self):
        self._factor('thought.experiment', 'wipe_after', self.BOOLEAN, allow_null = True)

if __name__ == '__main__':
    unittest.main()
