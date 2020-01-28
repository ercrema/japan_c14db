# encounter_c14db
This repository contains scripts and taw data for cleaning and translating [Rekihaku's 14C database](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd/db_param) for the [Encounter Project](https://www.encounterproject.info/) database.

The translated version will contain a subset of the information available from the original database. These are:

| Field               | Notes                                                                                                             | Language | Status                  |
|---------------------|-------------------------------------------------------------------------------------------------------------------|----------|-------------------------|
| LAB Number          | Unique Identifier                                                                                                 | NA       | NA                      |
| Prefecture          | Code and Text                                                                                                     | English  | Translation completed   |
| Address             | Text                                                                                                              | Japanese | No translation planned  |
| Latitude            | In decimal degrees, WGS84                                                                                         | NA       | Requires sanity check   |
| Longitude           | In decimal degrees, WGS84                                                                                         | NA       | Requires sanity check   |
| Site Name           | Text; In kanji, a script should be used to process site names from the Nabunken database to allow transliteration | Japanese | Transliteration planned |
| Sampling Location   | Text (indicate location of the sample in the site)                                                                | Japanese | No translation planned  |
| MaterialType        | Text and Code                                                                                                     | English  | Translation completed   |
| MaterialTypeDetails | Text and Code                                                                                                     | English  | Translation completed   |
| Period              | Text and Code                                                                                                     | English  | Translation completed   |
| PeriodDetails       | Text                                                                                                              | English  | Translation planned     |
| ArchaeologicalPhase | Text                                                                                                              | English  | Translation planned     |
| DatingMethod        | Code                                                                                                              | English  | Translation completed   |
| MeasuredAge         | Number                                                                                                            | NA       | NA                      |
| MeasuredAgeError    | Number                                                                                                            | NA       | NA                      |
| NormalisedAge       | Number                                                                                                            | NA       | NA                      |
| NormalisedAgeError  | Number                                                                                                            | NA       | NA                      |
| Delta13C            | Number                                                                                                            | NA       | NA                      |
| Delta13CError       | Number                                                                                                            | NA       | NA                      |
| Reference           | Japanese                                                                                                          | Japanese | No translation planned  |
|                     |                                                                                                                   |          |                         |
