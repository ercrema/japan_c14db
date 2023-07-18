# Archaeological Radiocarbon Database of Japan
This repository contains scripts and pipelines for the processing and the translation of the radiocarbon database of Japan hosted by the National Musuem of Japanese History (
[遺跡発掘調査報告書放射性炭素年代測定データベース](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd/db_param)). The original database was created for the purpose of collating information from site reports, and as such it includes duplicates and inconsistencies from the original source material (i.e. site reports). The R scripts contained in this repository is designed to eliminate those issues and provide at the same time an English translation of the some of the fields. More details can be found on the following dedicated publication:

Kudo, Y., Sakamoto, M., Hakozaki, M., Stevens, C.J., Crema, E.R (Submitted). An archaeological radiocarbon database of Japan. _Journal of Open Archaeology Data_.

Copies of the english version of the database can be found in this repository (`output/c14db_1.1.0.csv`), on a [dedicated webpage of the National Musuem of Japanese History website](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd_en/db_param
)

## Repository Structure
 * `/gis` ... Folder containing a polygon shapefile (`polbnda_jpn.*`) obtained from [Global Map Japan v.2.1 Boundary layer of the Geospatial Information Authority of Japan](https://www.gsi.go.jp/kankyochiri/gm_japan_e.html)
 * `/joad_figures` ... contains R scripts for generating figure and summary statistics for the Journal of Open Archaeology manuscript.
 * `/lookup_tables` ... contains lookup CSV tables to aid translation and romanisation
 * `/output` ... contains core output (flat table with all dates) generated using the R script `log.R`. The CSV table `c14db_1.1.0.csv` contains the translated and cleaned version of the database, whilst the `c14raw_1.1.0.csv` includes the full dataset (including problematic entries removed for `c14db_1.1.0.csv`).
 * `/raw` ... contains the expanded version of the original japanese database. The expanded version includes lat-long coordinates which are not available for download (but visible) in the National Musuem of Japanese History database.
 * `/utility` ... contains custom utility functions
 * `log.R` ... main R script for cleaning and translation.


## Funding
 * Research Funds for Database Development of the National Museum of Japanese History 国立歴史民俗博物館データベース開発経費 （工藤雄一郎・坂本稔・箱﨑真隆）
 * JSPS KAKENHI Grant Number 22H00743, 18H00757, 15K02995.
 * ERC-Stg grant “Demography, Cultural Change, and the Diffusion of Rice and Millets during the Jomon-Yayoi transition in prehistoric Japan (ENCOUNTER)”, Project N. 801953






