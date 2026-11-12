[![DOI](https://zenodo.org/badge/182769755.svg)](https://zenodo.org/badge/latestdoi/182769755)
![GitHub Release](https://img.shields.io/github/v/release/ercrema/japan_c14db)

# Archaeological Radiocarbon Database of Japan
This repository contains scripts and pipelines for the processing and translation of the radiocarbon database of Japan hosted by the National Museum of Japanese History (
[遺跡発掘調査報告書放射性炭素年代測定データベース](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd/db_param)). The original database was created for the purpose of collating information from site reports, and as such it includes duplicates and inconsistencies from the original source material (i.e. site reports). The R scripts in this repository are designed to eliminate those issues while also providing English translations for some of the fields. More details can be found in the following dedicated publication:

Kudo, Y., Sakamoto, M., Hakozaki, M., Stevens, C.J., Crema, E.R (2023). An archaeological radiocarbon database of Japan. _Journal of Open Archaeology Data_. DOI:10.5334/joad.115

Copies of the English curated version of the database can be found in this repository (`output/c14db_1.3.0.csv`), on a [dedicated webpage of the National Museum of Japanese History website](https://www.rekihaku.ac.jp/up-cgi/login.pl?p=param/esrd_en/db_param
)

## Repository Structure
 * `/gis` ... Folder containing a polygon shapefile (`polbnda_jpn.*`) obtained from [Global Map Japan v.2.1 Boundary layer of the Geospatial Information Authority of Japan](https://www.gsi.go.jp/kankyochiri/gm_japan_e.html)
 * `/joad_figures` ... contains R scripts for generating figures and summary statistics for the Journal of Open Archaeology manuscript.
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
[1] here_1.0.1         audubon_0.6.3      stringr_1.6.0      sf_1.0-21         
[5] sp_2.2-0           measurements_1.5.1 readxl_1.4.5       dplyr_1.1.4       

loaded via a namespace (and not attached):
 [1] vctrs_0.7.3        cli_3.6.6          rlang_1.3.0        stringi_1.8.9     
 [5] DBI_1.2.3          KernSmooth_2.23-26 generics_0.1.4     jsonlite_2.0.0    
 [9] glue_1.8.1         rprojroot_2.1.1    V8_6.0.2           e1071_1.7-16      
[13] grid_4.5.0         cellranger_1.1.0   classInt_0.4-11    tibble_3.3.1      
[17] lifecycle_1.0.5    compiler_4.5.0     Rcpp_1.1.0         pkgconfig_2.0.3   
[21] lattice_0.22-7     R6_2.6.1           class_7.3-23       tidyselect_1.2.1  
[25] curl_7.0.0         pillar_1.11.1      magrittr_2.0.5     tools_4.5.0       
[29] proxy_0.4-27       units_0.8-7 
```

## Funding
 * Research Funds for Database Development of the National Museum of Japanese History 国立歴史民俗博物館データベース開発経費 （工藤雄一郎・坂本稔・箱﨑真隆）
 * JSPS KAKENHI Grant Number 22H00743, 18H00757, 15K02995.
 * ERC-Stg grant “Demography, Cultural Change, and the Diffusion of Rice and Millets during the Jomon-Yayoi transition in prehistoric Japan (ENCOUNTER)”, Project N. 801953

## Licence
CC-BY 4.0
