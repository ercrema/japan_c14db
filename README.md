[![DOI](https://zenodo.org/badge/182769755.svg)](https://zenodo.org/badge/latestdoi/182769755)

# Archaeological Radiocarbon Database of Japan
This repository contains scripts and pipelines for the processing and the translation of the radiocarbon database of Japan hosted by the National Museum of Japanese History (
[遺跡発掘調査報告書放射性炭素年代測定データベース](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd/db_param)). The original database was created for the purpose of collating information from site reports, and as such it includes duplicates and inconsistencies from the original source material (i.e. site reports). The R scripts contained in this repository is designed to eliminate those issues and provide at the same time an English translation of the some of the fields. More details can be found on the following dedicated publication:

Kudo, Y., Sakamoto, M., Hakozaki, M., Stevens, C.J., Crema, E.R (2023). An archaeological radiocarbon database of Japan. _Journal of Open Archaeology Data_. DOI:10.5334/joad.115

Copies of the English curated version of the database can be found in this repository (`output/c14db_1.1.0.csv`), on a [dedicated webpage of the National Musuem of Japanese History website](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd_en/db_param
)

## Repository Structure
 * `/gis` ... Folder containing a polygon shapefile (`polbnda_jpn.*`) obtained from [Global Map Japan v.2.1 Boundary layer of the Geospatial Information Authority of Japan](https://www.gsi.go.jp/kankyochiri/gm_japan_e.html)
 * `/joad_figures` ... contains R scripts for generating figure and summary statistics for the Journal of Open Archaeology manuscript.
 * `/lookup_tables` ... contains lookup CSV tables to aid translation and romanisation
 * `/output` ... contains core output (flat table with all dates) generated using the R script `log.R`. The CSV table `c14db_1.1.0.csv` contains the translated and cleaned version of the database, whilst the `c14raw_1.1.0.csv` includes the full dataset (including problematic entries removed for `c14db_1.1.0.csv`).
 * `/raw` ... contains the expanded version of the original Japanese database. The expanded version includes lat-long coordinates which are not available for download (but visible) in the National Museum of Japanese History database.
 * `/utility` ... contains custom utility functions
 * `log.R` ... main R script for cleaning and translation.

## R Session Info
```
attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] revgeo_0.15        here_1.0.1         Nippon_0.7.1       stringr_1.5.0     
[5] sf_1.0-13          sp_1.6-1           measurements_1.5.1 readxl_1.4.2      
[9] dplyr_1.1.2       

loaded via a namespace (and not attached):
 [1] compiler_4.3.1     tidyselect_1.2.0   Rcpp_1.0.11        bitops_1.0-7      
 [5] lattice_0.21-8     R6_2.5.1           generics_0.1.3     classInt_0.4-9    
 [9] tibble_3.2.1       units_0.8-2        rprojroot_2.0.3    DBI_1.1.3         
[13] pillar_1.9.0       rlang_1.1.1        utf8_1.2.3         stringi_1.7.12    
[17] RJSONIO_1.3-1.8    cli_3.6.1          magrittr_2.0.3     class_7.3-22      
[21] grid_4.3.1         rstudioapi_0.14    lifecycle_1.0.3    vctrs_0.6.3       
[25] KernSmooth_2.23-21 proxy_0.4-27       glue_1.6.2         cellranger_1.1.0  
[29] RCurl_1.98-1.12    fansi_1.0.4        e1071_1.7-13       tools_4.3.1       
[33] pkgconfig_2.0.3 
```

## Funding
 * Research Funds for Database Development of the National Museum of Japanese History 国立歴史民俗博物館データベース開発経費 （工藤雄一郎・坂本稔・箱﨑真隆）
 * JSPS KAKENHI Grant Number 22H00743, 18H00757, 15K02995.
 * ERC-Stg grant “Demography, Cultural Change, and the Diffusion of Rice and Millets during the Jomon-Yayoi transition in prehistoric Japan (ENCOUNTER)”, Project N. 801953

## Licence
CC-BY 4.0
