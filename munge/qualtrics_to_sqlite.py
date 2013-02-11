#!/usr/bin/env python
# -*- encoding: utf-8 -*-

import os
import datetime
import csv
import sqlite3
import dumptruck

TMPDB = '/tmp/toilets.db'

def _schema(dbname):
    try:
        os.remove(dbname)
    except OSError:
        # No database to remove
        pass

    con = sqlite3.connect(dbname)
    cur = con.cursor()
    schema = open('schema.sql').read()
    cur.executescript(schema)
    con.commit()

def _consent(row):
    "Did the participant consent?"
    return row['Q36'] == 'y'

def _index(row):
    "Get the participantId and the questionnaire round from a row."
    rowId = row['id'].split('_')
    rowId[0] = int(rowId[0])

    if len(rowId) == 1:
        participantId = rowId[0]
        questionnaire_round = 'test'
    elif len(rowId) == 2:
        participantId, questionnaire_round = rowId
    else:
        raise ValueError('"id" has the wrong number of underscores.')

    return {
        'participantId': participantId,
        'questionnaire_round': questionnaire_round,
    }

SCHOOL_MAPPING = {
    'AG': 'cals',
    'AR': 'aap',
    'AS': 'arts and sciences',
    'RE': 'weill',
    'EN': 'engineering',
    'GM': 'management',
    'GR': 'graduate school',
    'HA': 'hotel',
    'HE': 'human ecology',
    'IL': 'ilr',
    'LA': 'law',
    'VM': 'vet',
}
CHECKBOX_MAPPING = {
  '': 0, '1': 1,
}
YES_NO_MAPPING = {
    'yes': 1, 'no': 0,
    'y': 1, 'n': 0,
}
_countries = [
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
COUNTRY_MAPPING=dict(enumerate(_countries))
YOUR_BUILDING_MAPPING = {
    '1': 'dorm',
    '2': 'house_apartment',
    '3': 'other',
}
ONE_TWO_MAPPING = {
    '1': 0,
    '2': 1,
}

def _demographics(dt, row):
    "Extract demographic info from a row and save to the demographics table."
    dem = _index(row)
    dem.update({
        'questionnaire_start': row['V3'],
        'questionnaire_end': row['V4'],
        'school': SCHOOL_MAPPING[row['school']],
        'consent': YES_NO_MAPPING[row['Q36']],
        'tompkins_county': ONE_TWO_MAPPING[row['Q42']],
        'building': YOUR_BUILDING_MAPPING[row['Q43']],
        'has_mobility_impairment': YES_NO_MAPPING[row['Q8']],

        # Match country of birth by line number.
        # Python starts at zero, but the questionnaire started at one.
        'country_of_birth': COUNTRY_MAPPING[int(row['Q40'])-1],

        'height': row['Q5'],
        'age': 17 + int(row['Q51']),
        'sex': row['Q3'],
    })
    dt.insert(dem, 'demographic')

LOCATION_MAPPING = {
    'public': 'public',
    'private': 'private',
    '3': 'neither',
}

def _publicness(dt, row):
    "Extract publicness info from a row and save to the demographics table."
    for location in ['dorm', 'house', 'restaurant', 'campus', 'your_residence']:
        pub = _index(row)
        pub.update({
            'location': location,
            'rating': LOCATION_MAPPING[row[location]],
        })
        dt.insert(pub, 'publicness')

TASK_MAPPING = {'urinate': 'urine', 'defecate': 'defecate'}
RESPONSE_MAPPING = {}
POSITION_MAPPING = {
    '1': 'up',
    '2': 'down',
    '':  None, # Dunno what this means
}

CLEANLINESS_POSTURE_QUESTION_MAPPING = {
    u'urinate': {
        u'public': {
            u'dirty': u'Q44_16',
            u'clean': u'Q44_17',
        },
        u'private': {
            u'dirty': u'Q44_18',
            u'clean': u'Q44_19',
        },
    },
    u'defecate': {
        u'public': {
            u'dirty': u'Q44_20',
            u'clean': u'Q44_21',
        },
        u'private': {
            u'dirty': u'Q44_22',
            u'clean': u'Q44_23',
        },
    },
}
POSTURE_MAPPING_UNSPECIFIED_CLEANLINESS = {
    '1': 'stand',
    '2': 'sit',
    '3': 'hover',
    '4': 'squat',
    '5': 'other',
    '':  None,
}
POSTURE_MAPPING_SPECIFIED_CLEANLINESS = {
    '1': 'stand',
    '3': 'hover',
    '4': 'squat',
    '5': 'sit',
    '6': 'other',
    '':  None,
}

def _thought_experiment(dt, row):
    "Extract thought experiment info from a row and save to the demographics table."
    
    for task in ['urinate', 'defecate']:
        for privacy in ['private', 'public']:

           # Unspecified cleanliness
            prefix = TASK_MAPPING[task] + '_' + privacy
            e = _index(row)
            e.update({
                'task': task,
                'privacy': privacy,
                'cleanliness': 'unspecified',
                'posture': POSTURE_MAPPING_UNSPECIFIED_CLEANLINESS[row[prefix + ': posture']],
                'position': POSITION_MAPPING[row[prefix + ': cleaning']],
                'cover_before': CHECKBOX_MAPPING[row[prefix + '_1']],
                'wipe_before': CHECKBOX_MAPPING[row[prefix + '_2']],
                'wipe_after': CHECKBOX_MAPPING[row[prefix + '_3']],
            })
            dt.insert(e, 'thought.experiment')

            # Specified cleanliness
            for cleanliness in ['clean', 'dirty']:
                e = _index(row)
                e.update({
                    'task': task,
                    'privacy': privacy,
                    'cleanliness': cleanliness,
                    'posture': POSTURE_MAPPING_SPECIFIED_CLEANLINESS[row[CLEANLINESS_POSTURE_QUESTION_MAPPING[task][privacy][cleanliness]]],
                    'position': None,
                    'cover_before': None,
                    'wipe_before': None,
                    'wipe_after': None,
                })
                dt.insert(e, 'thought.experiment')
# Headers
# ['\xef\xbb\xbfV1', 'V2', 'V3', 'V4', 'V5', 'SC0_0', 'SC0_1', 'SC0_2', 'id', 'school', 'school2', 'Q36', 'Q37', 'Q28', 'Q42', 'Q43', 'your_residence', 'dorm', 'house', 'restaurant', 'campus', 'urine_public: posture', 'urine_private: posture', 'defecate_public: posture', 'defecate_private: posture', 'urine_public: cleaning', 'urine_private: cleaning', 'defecate_public: cleaning', 'defecate_private: cleaning', 'urine_public_1', 'urine_public_2', 'urine_public_3', 'urine_public_4', 'urine_private_1', 'urine_private_2', 'urine_private_3', 'urine_private_4', 'defecate_public_1', 'defecate_public_2', 'defecate_public_3', 'defecate_public_4', 'defecate_private_1', 'defecate_private_2', 'defecate_private_3', 'defecate_private_4', 'Q8', 'Q25', 'Q40', 'Q5', 'Q51', 'Q3', 'Q44_16', 'Q44_17', 'Q44_18', 'Q44_19', 'Q44_20', 'Q44_21', 'Q44_22', 'Q44_23', 'Q12', 'Q11', 'Q21', 'Q19', 'Q20', 'Q22', '']


def qualtrics_to_sqlite(filename, dbname):
    # Create tables
    _schema(TMPDB)

    # Read the first two rows of the Qualtrics CSV file
    file = open(filename)
    reader = csv.reader(file)
    headers = reader.next()
    questions = reader.next()
    file.close()

    # Now use the header to open a dict reader
    file = open(filename)
    reader = csv.DictReader(file, fieldnames = headers)

    # Skip the two header rows.
    reader.next()
    reader.next()

    # Access via DumpTruck
    dt = dumptruck.DumpTruck(dbname = TMPDB, auto_commit = False)

    # Parse each row
    for row in reader:

        # Skip if the participant did not consent
        if not _consent(row):
            continue

        _demographics(dt, row)
        _publicness(dt, row)
        _thought_experiment(dt, row)

    # Commit
    dt.commit()

    file.close()

    # Copy to disk
    os.rename(TMPDB, dbname)

if __name__ == '__main__':
    import sys
    qualtrics_to_sqlite(*sys.argv[1:])
