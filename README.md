# Encounter/Rekihaku Radiocarbon DataBase
This repository contains scripts and raw data for cleaning and translating [Rekihaku's 14C database](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd/db_param) for the [Encounter Project](https://www.encounterproject.info/) database.

The main workflow for the data cleaning and translation is recorded on the markdown file `main.Rmd` and its main output is in the R image file `japanc14db_v02(200130).RData`, which contains the the rekihaku database on its January 30th 2020 version. The image file contains the data.frame `c14db` which includes the following fields:
  - PrefectureCode ... Code indicating the prefecture where the site is located
  - Prefecture ... Prefecture where the site is located (in Japanese)
  - SiteName ... Site name (in Japanese)
  - SiteLocation  ... Site Location Address (in Japanese)
  - SamplingLocation  ... Sampling location within the site (in Japanese)
  - MaterialType  ... Details on the material dated (in Japanese)
  - MaterialCode1  ... Material dated (O=Other, T=Terrestrial, M=Marine)
  - MaterialCode2  ... Material dated further details
  - Period  ... Archaeological Period (in Japanese)
  - Phase  ... Archaeological Phase (in Japanese)
  - PeriodCode ... Archaeological Period Code
  - LabCode ... Lab Code
  - Method  ... Dating Method (in Japanese)
  - CRA ... Conventional Radiocarbon Age 
  - CRAError ... Conventional Radiocarbon Age Error
  - CorrectedC14Age ... Corrected C14 Age
  - CorrectedC14Error ...  Corrected C14 Age Error
  - Delta13C ... Delta13C (using AMS based)
  - Delta13CError ... Delta13C Error
  - Delta13CIRMS ... Delta13C (using IR-MS based)
  - AnalysedBy ... Research group who dated the sample
  - Laboratory ... Laboratory where the dating was processed
  - PubblicationYear ... Pubblication year of the date
  - ReportTitle ... Title of Report (in Japanese)
  - Page ... Page number
  - Remarks ... Additional Notes (in Japanese)
  - Reference ... Full Reference (in Japanese)
  - Publisher ... Publisher (in Japanese)
  - Latitude ... Latitude (in degree decimal)
  - Longitude ... Longitude (in degree decimal)
  - ReferenceID ... Unique identifier for the reference
  - retain ... Boolean on whether the sample can be retained for analysis
  - originalRow ... Original Row number
  - PrefectureNameEn ...  Prefecture where the site is located (in English)
  - Region ... Region where the site is located (in English)
  - coord.check ... If TRUE the coordinates are within the attributed prefecture.
  - Method_En  ... Dating Method (in English)
  - Material  ... Material either Other, Terrestrial, or Marine
  - Material_Details ... Additional details on the materials dated (expanded form of MaterialCode2)
  - PeriodEN ... Archaeological Period (in English)
  - PhaseEN ... Archaeological Phase (in English)
  - Furigana ... Site name in Furigana (in Japanese)
  - ToCheck ... Whether a manual check of the site name is necessary.
  - Alternative ... Suggested correction for sitename
  - SiteType  ... Site type (in English)
  - Romanised ... RTomanised SiteName
  
## Repository Structure

The directory *raw* contains the original excel files provided by Y.Kudo, the *lookup* directory contains several lookup table for translation and reverse coding, the *reports* directory cotains tables listing issues such as duplicates and potential errors to be fixed, the *gis* directory contains shapefile with the Japanese administrative regions, and the *utility* directory contains bespoke functions for checks, geocoding, and romanisation. 



