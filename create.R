# create analysis dataset as of Feb 2026

# setup/packages
library(data.table)
library(readxl)
library(haven)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

# read in analytical dataset with LATEST updates
df.sas <- read_sas(file.path("data/CDC ShareFile Download 1-30-2026",
                             "pitt_ruby_immune_jan26.sas7bdat"))

# read in repeaters dataset as of feb-2025
repeaters <- fread(file.path("data/NVSN ARI Repeaters",
                             "NVSN_ARI_Repeaters_2016-2023.csv"),
                   na.strings="#N/A")

# merge on case id
dat <- merge(data.table(df.sas), repeaters, all.x=TRUE, by.x="Caseid", by.y="caseid")

# clean-up specific cases according to ARI Data Team instructions
dat[Caseid=="EC1R00655" & scradmitdate==as.Date("2017-10-05"), exclude:=TRUE]
dat[Caseid=="ER4R00030" & scradmitdate==as.Date("2017-12-04"), Caseid:="ER4R00330"]
dat[Caseid=="EP2R00381" & scradmitdate==as.Date("2021-06-07"), Caseid:="EP2R03811"]
dat[Caseid=="ER2R0349", Caseid:="ER2R03409"]
dat[Caseid=="EP2R-0363", Caseid:="EP2R03637"]

# detect duplicate case IDs used, determine whether to drop or keep first enrollment
dup.caseids <- dat$Caseid[dat[is.na(exclude), duplicated(Caseid)]]
dups <- dat[Caseid %in% dup.caseids, .(
  date1=min(scradmitdate), date2=max(scradmitdate),
  individ1=first(individ), individ2=last(individ)), by=Caseid]
dat[Caseid %in% dups$Caseid, exclude:=TRUE]
cln <- dat[is.na(exclude) | exclude==FALSE, ]
message(paste("Removed", sum(dat$exclude, na.rm=T), "cases due to duplicate IDs!"))

# tabulate total cases by site and year
# out <- cln[, .(Total=.N, Identified=sum(!is.na(individ))), by=.(studysite, c_ariyear)]
# out1 <- dcast(out[order(studysite, c_ariyear)], studysite ~ c_ariyear, value.var="Total")
# out2 <- dcast(out[order(studysite, c_ariyear)], studysite ~ c_ariyear, value.var="Identified")
# fwrite(out1, "C:/Users/pomponiord/Downloads/total_cases_by_site-season.csv")
# fwrite(out2, "C:/Users/pomponiord/Downloads/identified_cases_by_site-season.csv")

##### Process Transplant Spreadsheet rec'd from sites #####

TPX.SITES <- c("Nashville", "Pittsburgh", "Seattle", "KC", "Rochester", "Cincinnati", "Houston")
TPX.TYPES <- c("BMT", "SOT")
df.tpx <- data.frame(caseid=character(), tpx_type=character(),
                      tpx_organ=character(), tpx_over1yr=logical(),
                      tpx_notes=character())
for (tpx.site in TPX.SITES){
  if (tpx.site=="Pittsburgh"){
    for (tpx.type in TPX.TYPES){
      if (tpx.type=="SOT"){
        ### note: by far the most complicated sheet due to multiple tpx dts!!!
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A1:H25",
          trim_ws=TRUE,
          col_types="text") %>%
          filter(str_detect(`Case ID`, "EP")) %>%
          mutate(
            caseid=str_remove(`Case ID`, "\\s+\\(repeat patient\\)"),
            tpx_type=tpx.type,
            tpx_organ=`Organ Transplanted`,
            enroll_dt=as.Date(
              as.numeric(`Date of Enrollment`), origin="1899-12-30"),
            tpx_notes=NA) %>%
          rowwise() %>%
          mutate(tpx_dts=case_when(
            str_detect(`Date of Transplant`, "\\d{5}") ~ as.Date(
              as.numeric(`Date of Transplant`), origin="1899-12-30"),
            str_detect(`Date of Transplant`, "\\d+/\\d+/\\d{4}") ~
              max(mdy(str_extract_all(
                `Date of Transplant`, "\\d+/\\d+/\\d{4}")[[1]])),
            TRUE ~ mdy(`Date of Transplant`))) %>%
          ungroup() %>%
          mutate(tpx_over1yr=interval(tpx_dts, enroll_dt) / years(1) > 1)
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      } else if (tpx.type=="BMT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A27:I31",
          trim_ws=TRUE,
          col_names=FALSE,
          col_types="text") %>%
          mutate(
            caseid=str_remove(`...1`, " (repeat patient)"),
            tpx_type=tpx.type,
            tpx_organ=`...4`,
            enroll_dt=as.Date(
              as.numeric(`...3`), origin="1899-12-30"),
            tpx_dts=as.Date(
              as.numeric(`...2`), origin="1899-12-30"),
            tpx_notes=NA) %>%
          mutate(tpx_over1yr=interval(tpx_dts, enroll_dt) / years(1) > 1)
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      }
    }
  }
  if (tpx.site=="Nashville"){
    for (tpx.type in TPX.TYPES){
      if (tpx.type=="SOT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A2:G20",
          trim_ws=TRUE,
          col_types="text") %>%
          filter(str_detect(`Case ID`, "EN")) %>%
          mutate(
            caseid=`Case ID`,
            tpx_type=tpx.type,
            tpx_organ=`Organ Transplanted`,
            tpx_dt=as.Date(
              as.numeric(`Date of Transplant`), origin="1899-12-30"),
            enroll_dt=as.Date(
              as.numeric(`Enrollment Date`), origin="1899-12-30"),
            tpx_notes=`...7`) %>%
          mutate(tpx_over1yr=interval(tpx_dt, enroll_dt) / years(1) > 1)
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      } else if (tpx.type=="BMT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A22:F25",
          trim_ws=TRUE,
          col_names=FALSE,
          col_types="text") %>%
          mutate(
            caseid=`...1`,
            tpx_type=tpx.type,
            tpx_organ=`...4`,
            tpx_dt=as.Date(
              as.numeric(`...3`), origin="1899-12-30"),
            enroll_dt=as.Date(
              as.numeric(`...2`), origin="1899-12-30"),
            tpx_notes=NA) %>%
          mutate(tpx_over1yr=interval(tpx_dt, enroll_dt) / years(1) > 1)
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      }
    }
  }
  if (tpx.site=="Seattle"){
    dat.tmp <- read_excel(
      file.path("data/ARI Transplant 0117.xlsx"),
      sheet=tpx.site,
      range="A1:E19",
      trim_ws=TRUE,
      col_types="text") %>%
      mutate(
        caseid=str_remove(`CASE ID:`, "\\s*\\(Repeat patient\\)"),
        tpx_type=case_when(
          str_detect(`Type of organ transplanted`, "Bone Marrow") ~ "BMT",
          str_detect(`Type of organ transplanted`, "Blood Stem Cell") ~ "BMT",
          TRUE ~ "SOT"),
        tpx_organ=`Type of organ transplanted`,
        tpx_over1yr=str_detect(
          `Timing of transplant relative to respiratory illness onset (>12 or <12 months)`,
          ">12"),
        tpx_notes=Notes)
    df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
  }
  if (tpx.site=="KC"){
    dat.tmp <- read_excel(
      file.path("data/ARI Transplant 0117.xlsx"),
      sheet=tpx.site,
      range="A1:H2",
      trim_ws=TRUE,
      col_types="text") %>%
      mutate(
        caseid=`Case ID`,
        tpx_type="SOT",
        tpx_organ=`Organ Transplanted`,
        tpx_over1yr=NA,
        tpx_notes=paste0(
          "Date of Transplant: ",
          as.Date(as.numeric(`Date of Transplant`), origin="1899-12-30")))
    df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
  }
  if (tpx.site=="Rochester"){
    for (tpx.type in TPX.TYPES){
      if (tpx.type=="SOT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A1:I4",
          trim_ws=TRUE,
          col_types="text") %>%
          filter(str_detect(`Case ID`, "ER")) %>%
          mutate(
            caseid=`Case ID`,
            tpx_type=tpx.type,
            tpx_organ=`Organ Transplanted`,
            tpx_over1yr=NA,
            tpx_notes=paste0(
              "Date of Transplant: ",
              as.Date(as.numeric(`Date of Transplant`), origin="1899-12-30")))
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      } else if (tpx.type=="BMT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A6:F6",
          trim_ws=TRUE,
          col_names=FALSE,
          col_types="text") %>%
          mutate(
            caseid=`...1`,
            tpx_type=tpx.type,
            tpx_organ=`...3`,
            tpx_over1yr=NA,
            tpx_notes=paste0(
              "Date of Transplant: ",
              as.Date(as.numeric(`...2`), origin="1899-12-30")))
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      }
    }
  }
  if (tpx.site=="Houston"){
    for (tpx.type in TPX.TYPES){
      if (tpx.type=="SOT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A1:H119",
          trim_ws=TRUE,
          col_types="text") %>%
          filter(str_detect(`Case ID`, "EH")) %>%
          # pivot_longer(cols=contains("ID"), values_to="caseid", values_drop_na=T) %>%
          mutate(caseid=`Case ID`) %>%
          mutate(
            tpx_type=tpx.type,
            tpx_organ=`Organ Transplanted`,
            tpx_over1yr=str_detect(`>1 year since transplant`, "Y"),
            tpx_notes=NA)
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      } else if (tpx.type=="BMT"){
        dat.tmp <- read_excel(
          file.path("data/ARI Transplant 0117.xlsx"),
          sheet=tpx.site,
          range="A121:H163",
          trim_ws=TRUE,
          col_names=FALSE,
          col_types="text") %>%
          filter(str_detect(`...1`, "EH")) %>%
          # pivot_longer(cols=c(`...1`:`...6`), values_to="caseid", values_drop_na=T) %>%
          mutate(caseid=`...1`) %>%
          mutate(
            tpx_type=tpx.type,
            tpx_organ=`...8`,
            tpx_over1yr=str_detect(`...7`, "Y"),
            tpx_notes=NA)
        df.tpx <- bind_rows(df.tpx, select(dat.tmp, colnames(df.tpx)))
      }
    }
  }
}

##### Cleanup and merge transplant information #####

# merge on caseid
tpx <- data.table(df.tpx)
mrg <- merge(cln, tpx, all.x=TRUE, by.x="Caseid", by.y="caseid") 
message(paste("Merged transplant info for", sum(!is.na(mrg$tpx_type)), "cases."))

# create consolidated transplant variables
mrg[Caseid=="EP1R05148", tpx_notes:="Date of Transplant: 2021-08-31"]
mrg[Caseid=="EP1R05124", tpx_notes:="Date of Transplant: 2012-01-31"]
mrg[, tpx_type:=fcase(
  c_utrans_yn==1 & c_utrans_so==1, "SOT",
  c_utrans_yn==1 & c_utrans_bl==1, "BMT",
  default=tpx_type)]
mrg[, tpx_organ:=fcase(
  utranssp___1==1, "Heart",
  utranssp___2==1, "Lung",
  utranssp___3==1, "Kidney",
  utranssp___4==1, "Liver",
  utranssp___5==1, "Small bowel/multivisceral",
  default=tpx_organ)]
mrg[, tpx_over1yr:=fcase(
  grepl("Date of Transplant", tpx_notes),
  as.IDate(scradmitdate) - as.IDate(gsub("[^0-9.-]", "", tpx_notes)) > 365,
  default=tpx_over1yr)]

# final dataset covering 2016 - 2023
if (any(duplicated(mrg$Caseid))) stop("Duplicate Case IDs found!")
fwrite(mrg[c_ariyear %in% 2:8], "derived/ARI_Analytical_Dataset_2016-2023.csv")

# for analyses including 2024: ignore repeaters
fwrite(mrg[c_ariyear %in% 2:9], "derived/ARI_Analytical_Dataset_2016-2024.csv")
