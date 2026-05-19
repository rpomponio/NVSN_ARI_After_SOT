# File Structure
```
./
│   .gitignore
│   SOT Analysis.Rproj
│   ...
├───archive
│   ...
└───data
|   │   ARI Transplant 0117.xlsx
|   ├───CDC ShareFile Download 1-30-2026
|   │       PITT_RUBY_IMMUNE_JAN26.csv
|   │       pitt_ruby_immune_jan26.sas7bdat
|   └───NVSN ARI Repeaters
|           NVSN_ARI_Repeaters_2016-2023.csv
└───derived
│   ...
```

# Notes

Useful code for spot checking number of repeat episodes:

```
inc[is_sot==1 & !is.na(individ), .(sum(duplicated(individ)), .N)]
```