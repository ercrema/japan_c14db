# Archaeological Radiocarbon Database of Japan
This repository contains scripts and pipelines for the processing and the translation of the radiocarbon database of Japan hosted by the National Musuem of Japanese History (
[遺跡発掘調査報告書放射性炭素年代測定データベース](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd/db_param)). The original database was created for the purpose of collating information from site reports, and as such it includes duplicates and inconsistencies from the original source material (i.e. site reports). The R scripts contained in this repository is designed to eliminate those issues and provide at the same time an English translation of the some of the fields. More details can be found on the following dedicated publication:

Kudo, Y., Sakamoto, M., Hakozaki, M., Stevens, C.J., Crema, E.R (Submitted). An archaeological radiocarbon database of Japan. _Journal of Open Archaeology Data_.

Copies of the english version of the database can be found in this repository (`output/c14db_1.0.0.csv`), on a [dedicated webpage of the National Musuem of Japanese History website](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd_en/db_param
)

## Repository Structure
The core script for containing the main pipeline for translation and cleaning is `log.R`. 





