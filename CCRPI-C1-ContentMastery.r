#' ---
#' title: "CCRPI 2018 Estimates using 2018 GMAS Data - Content Mastery Component"
#' date: "Last updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#' \newpage
#'
#+ setup, include=FALSE
# SETUP -------------------------------------------------------------------

source("SETUP.R") ## SETS MY OWN VARIOUS GLOBAL R OPTIONS AND DEFINES A FEW UTILITY FUNCTIONS FOR GENERATING REPORTS ##
library(kableExtra); library(dplyr)
kable <- function(x, format = "pandoc", ...) {
  knitr:::kable(x, format = format, ...)
}


knitr::opts_chunk$set(echo = FALSE,
                        results = 'asis',
                        dev = c("pdf", "png", "svg"))

library(ccrpi)

fcs_schools <- Rrdcsv("data/2017-ComponentScoring.csv", asDT = TRUE)[
  system.id == 660, .(system.id, system.name, school.id, school.name,
                      grade.configuration, grade.cluster)]
fcs_schools[, school.id := as.integer(school.id)]
fcs_schools <- fcs_schools[school.id != 307] ## EXCLUDE GA BAPTIST ##
#'
#+ data_gmas_eog, results='hide'

# GMAS - EOG DATA  -----------------------------------------------------------
files.eog <- list.files(path = "data/", pattern = "EOG.*?\\.csv$", full.names = TRUE)
ldat.eog0 <- lapply(files.eog, function(x) Rrdcsv(x, asDT = TRUE))

## SAVE LIST OF COLUMN NAMES IN CASE ANY NEED TO BE ADDED BELOW ##
.orignames.eog <- lapply(ldat.eog0, names)

ldat.eog01 <- lapply(ldat.eog0, function(x) {
  ## MAKE A TEMPORARY COPY (NOT VERY EFFICIENT, BUT NONTHELESS SAFER IN TERMS OF POSSIBLE DATA LOSS) ##
  y <- x
  names(y) <- gsub("_rpt", "", names(y))
  y <- y[, c("gtid", "syscode", "sysname", "schcode", "schname", ## STU, SYSTEM, & SCHOOL IDs ##
             "testadmin", "testdate", "testedgrade", ## TEST ADMIN (TERM & YEAR) & TESTED GRADE LEVEL ##
             "stugender", "ethnicityrace", "swdflag", "eip", ## SUB-GROUP VARS ##
             "achlevel_ela", ## ELA ACHIEVEMENT LEVEL ##
             "achlevel_math", ## MATH ACHIEVEMENT LEVEL ##
             "achlevel_sci", ## SCIENCE ACHIEVEMENT LEVEL ##
             "achlevel_soc")] ## SOCIAL STUDIES ACHIEVEMENT LEVEL ##
  ## CREATE 'SCHOOLYEARNUMBERSPRING' BASED ON 'TESTADMIN' STRING VALUES (FOR BACK-COMPATABILITY) ##
  ## &  REMOVE ANY ERRANT LEADING OR TRAILING WHITE SPACE IN 'TESTADMIN' STRING VALUES ##
  y$testadmin <- stringi::stri_trim_both(y$testadmin)

  y$school.year <- as.integer(gsub("^Spring (20\\d{2})$", "\\1", y$testadmin),
                              perl = TRUE) ## USE REGEX TO GET CORRECT YEAR VALUE ##
  ## RECODE 'ETHNICITYRACE' VALUES (SEE 'ZMISC/GA MAIN EOG 2018 DATA FILE LAYOUT XLSX.XLSX' - PROVIDED BY GADOE (RE: JOE BLESSING) ON 20180628) ##
  y$ethnicityrace_rec <- car::recode(y$ethnicityrace,
                                     c("1='Asian / Pacific Islander';
                                       2='Black (Non-Hispanic)';
                                       3='Hispanic';
                                       4='Native American / Alaskan Native';
                                       5='White (Non-Hispanic)';
                                       6='Multi-Racial';
                                       else = NA"))
  y$ethnicity_rec_short <- car::recode(y$ethnicityrace, c("1='as'; 2='bl';
                                                         3='hp'; 4='ai';
                                                         5='wh'; 6='mr';
                                                         else = NA"))
  ## RECODE SWD FLAG ##
  y$swdflag_rec <- ifelse(y$swdflag == 1, "SWD", "Not SWD")
  return(y[!is.na(y$gtid), ])
})

## COMBINE (NOT MERGE) BOTH YEARS' EOG DATA (RESULTS IS A 'DATA.TABLE' CLASSED OBJ.) ##
dat.eog <- unique(rbindlist(ldat.eog01, fill = TRUE))
datl.eog <- melt(dat.eog,
                 id.vars = setdiff(names(dat.eog),
                                   grep("achlevel_", names(dat.eog),
                                        value = TRUE)),
                 variable.name = "subject", value.name = "achlevel")
datl.eog[, achlevel_rec := car::recode(achlevel,
                                       c("1='BEG'; 2='DEV'; 3='ADV'; 4='DIS'"))]
setnames(datl.eog,
         c("schcode", "schname"),
         c("school.id", "school.name"))
datl.eog[, school.year := as.integer(gsub("^Spring (20\\d{2})$", "\\1", testadmin),
                                     perl = TRUE)]
#'
#'
#+ eoc_data, results='hide'
## STATE GA MILESTONES EOC DATA ==============================================
files.eoc <- list.files(path = "data/", pattern = "GMAS-FCS-EOC.*?\\.csv$", full.names = TRUE)
ldat.eoc0 <- lapply(files.eoc, function(x) Rrdcsv(x, asDT = TRUE))

.orignames.eoc <- lapply(ldat.eoc0, names) ## SAVE LIST OF COLUMN NAMES IN CASE ANY NEED TO BE ADDED BELOW ##

## SUBSET EACH EOC DATAFRAME TO INCLUDE ONLY PERTINENT COLUMNS ##
ldat.eoc01 <- lapply(ldat.eoc0, function(x) {
  ## MAKE A TEMPORARY COPY (NOT VERY EFFICIENT, BUT NONTHELESS SAFER IN TERMS OF POSSIBLE DATA LOSS) ##
  y <- x
  names(y) <- gsub("_rpt", "", names(y))
  y <- y[, c("gtid", "syscode", "sysname", "schcode", "schname", ## IDs ##
             "testadmin", "testdate", ## TEST ADMIN & TEST GRADE LEVEL INFO ##
             "stugender", "ethnicityrace", "swdflag", "eip", ## DEMOGRAPHICS ##
             "contentarea", "contentareacode", "lexile", "readingstatus", ## LEXILE SCORES ##
             "ss", "achlevel", "condsem")] ## ELA SCORES ##
  ## CREATE 'SCHOOLYEARNUMBERSPRING' BASED ON 'TESTADMIN' STRING VALUES ##
  y$testadmin <- stringi::stri_trim_both(gsub(" MM ", " ", y$testadmin)) ## REMOVE ANY ERRANT LEADING OR TRAILING WHITE SPACE IN 'TESTADMIN' STRING VALUES ##
  y$schoolyearnumberspring <- as.integer(gsub("^SPRING (20\\d{2})$", "\\1", y$testadmin, perl = TRUE)) ## USE REGEX TO GET CORRECT YEAR VALUE ##
  print(table(y$testadmin, y$schoolyearnumberspring, deparse.level = 2)) ## IN-VIVO QC ##

  ## RECODE 'ETHNICITYRACE' VALUES (SEE 'DATA/GA MAIN EOC 2018 DATA FILE LAYOUT XLSX.XLSX' - PROVIDED BY GADOE (JOE BLESSING) ON 20180628) ##
  y$ethnicityrace_rec <- car::recode(y$ethnicityrace, c("1='Asian / Pacific Islander'; 2='Black (Non-Hispanic)';
                                                        3='Hispanic'; 4='Native American / Alaskan Native';
                                                        5='White (Non-Hispanic)'; 6='Multi-Racial';
                                                        else = NA"))
  y$ethnicity_rec_short <- car::recode(y$ethnicityrace, c("1='as'; 2='bl';
                                                        3='hp'; 4='ai';
                                                        5='wh'; 6='mr';
                                                        else = NA"))
  print(table(y$ethnicityrace_rec, y$ethnicityrace, y$schoolyearnumberspring, deparse.level = 2)) ## IN-VIVO QC ##
  print(table(y$ethnicityrace, y$ethnicity_rec_short, y$schoolyearnumberspring, deparse.level = 2)) ## IN-VIVO QC ##

  ## RECODE SWD FLAG ##
  y$swdflag_rec <- ifelse(y$swdflag == 1, "SWD", "Not SWD")
  print(table(y$swdflag_rec, y$swdflag, y$schoolyearnumberspring, deparse.level = 2)) ## IN-VIVO QC ##

  ## FIX CONTENT AREA LABELS TO PREVENT LATEX ERRORS WITH '&' CHAR. INSIDE TABULAR ENV. & SHORTEN EXTRA LONG LABELS ##
  y$contentarea <- gsub("Literature & Composition", "Literature", y$contentarea)
  ### AND SHORTED SOME OF THE CONTENT AREA LABELS ###
  y$contentarea <- gsub("Ninth", "9th", y$contentarea)
  y$contentarea <- gsub("Economics/Business/Free Enterprise", "Economics", y$contentarea)

  print(table(y$contentarea))

  cat("New Dimensions: "); print(names(y)) ## IN-VIVO QC ##

  return(y[!is.na(y$gtid), ]) 
})

## COMBINE (NOT MERGE) BOTH YEARS' EOC DATA (RESULTS IS A 'DATA.TABLE' CLASSED OBJ.) ##
dat.eoc <- rbindlist(ldat.eoc01); #class(dat.eoc) ## CHECK THAT THE RESULT IS IN FACT A DATA.TABLE (JUST IN CASE SOMETHING SPOOKY HAPPENS) ##
cat("Combined Data Dimensions:\n"); paste(dim(dat.eoc)[1], "rows, ", dim(dat.eoc)[2], "columns") ## QC ##
cat("Subsetted Dataframes' Separate Dimensions:\n"); pander(lapply(ldat.eoc01, function(x) paste0(dim(x)[1], " rows, ", dim(x)[2], " columns"))) ## QC ##

str(dat.eoc)
## RECODE CONTENT AREAS AS ELA/MATH/SCI/SOC
dat.eoc$subject <- car::recode(dat.eoc$contentareacode,
                               c("c(1, 2)='achlevel_ela';
                                 c(3, 4, 9, 10)='achlevel_math';
                                 c(5, 6)='achlevel_sci';
                                 c(7, 8)='achlevel_soc';
                                 else = NA"))
with(dat.eoc, {
  table(contentarea, subject) ## QC ##
})

dat.eoc[, achlevel_rec := car::recode(achlevel, c("1='BEG'; 2='DEV'; 3='ADV'; 4='DIS'"))]
setnames(dat.eoc,
         c("schcode", "schname"),
         c("school.id", "school.name"))
dat.eoc[, school.year := as.integer(gsub("^SPRING (20\\d{2})$", "\\1", testadmin), perl = TRUE)]
dat.eoc[, testedgrade := ifelse(school.id %in% datl.eog$school.id, 8, 9)]

dat.eoc$subject_rec <- car::recode(dat.eoc$subject, c("'ELA' = 'achlevel_ela';
                                                      'Math' = 'achlevel_math';
                                                      'Science' = 'achlevel_sci';
                                                      'Social Studies' = 'achlevel_soc'"))

datl.eog <- merge(dat.eoc[, .(school.id, gtid, subject, achlevel_rec)], datl.eog, all.x = FALSE, all.y = TRUE)
datl.eog[, testedgrade := ifelse(is.na(testedgrade) & gtid %in% datl.eog$gtid, 8, testedgrade)]
#'
#' # Elementary Schools (ES) End-of-Grade (EOG) Scores
#'
#' -----
#'
#' ## ES _English Language Arts (ELA)_ Subject-Area Indicator Scores
#'
#+ ach_ela_es
## ACH.ELA.ES ==============================================================

ach.ela.es.cy0 <- Rach(x = datl.eog[school.year == 2018],
                       gradeband = "ES",
                       grade_var = "testedgrade",
                       subject = "ELA",
                       subject_var = "subject",
                       subject_code = "achlevel_ela",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_ela",
                       fay_var = "subject",
                       fay_code = "achlevel_ela",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.ela.es.cy <- ach.ela.es.cy0$cm.subj

ptvars.ela <- grep("Pts", names(ach.ela.es.cy), value = TRUE)
ach.ela.es.cy[, (ptvars.ela) := lapply(.SD, function(x)
  ifelse(N_Students.ELA < 15, NA, x)),
  .SD = ptvars.ela]

ach.ela.es.cym <- merge(ach.ela.es.cy, unique(datl.eog[school.year == 2018,
                                                       .(school.id, school.name)]))
setcolorder(ach.ela.es.cym, c("school.id", "school.name"))
setnames(ach.ela.es.cym, c(grep("\\.ELA", names(ach.ela.es.cym), value = TRUE)),
         c(paste0(grep("\\.ELA", names(ach.ela.es.cym), value = TRUE), ".cy")))

ach.ela.es.py0 <- Rach(x = datl.eog[school.year == 2017],
                       gradeband = "ES",
                       grade_var = "testedgrade",
                       subject = "ELA",
                       subject_var = "subject",
                       subject_code = "achlevel_ela",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_ela",
                       fay_var = "subject",
                       fay_code = "achlevel_ela",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.ela.es.py <- ach.ela.es.py0$cm.subj
ach.ela.es.py[, (ptvars.ela) := lapply(.SD, function(x)
  ifelse(N_Students.ELA < 15, NA, x)),
  .SD = ptvars.ela]


ach.ela.es.pym <- merge(ach.ela.es.py, unique(datl.eog[school.year == 2017,
                                                       .(school.id, school.name)]))
setcolorder(ach.ela.es.pym, c("school.id", "school.name"))
setnames(ach.ela.es.pym, c(grep("\\.ELA", names(ach.ela.es.pym), value = TRUE)),
         c(paste0(grep("\\.ELA", names(ach.ela.es.pym), value = TRUE), ".py")))

ach.ela.esm <- merge(ach.ela.es.pym, ach.ela.es.cym, all.x = FALSE, all.y = TRUE,
                     by = c("school.id", "school.name"))

setcolorder(ach.ela.esm, c("school.id", "school.name",
                           grep("N_Students", names(ach.ela.esm), value = TRUE),
                           grep("SumPts\\.ELA", names(ach.ela.esm), value = TRUE),
                           grep("AchPts\\.ELA", names(ach.ela.esm), value = TRUE),
                           grep("AchPts_Cpd\\.ELA", names(ach.ela.esm), value = TRUE),
                           grep("AchPts_Wgtd\\.ELA", names(ach.ela.esm), value = TRUE)))

ach.ela.esm[, AchPts_Wgtd.ELA.diff := AchPts_Wgtd.ELA.cy - AchPts_Wgtd.ELA.py]

#'
#+ ach_ela_es_output, echo=FALSE, results='asis'
setkey(ach.ela.esm, "school.name")
ach.ela.esm[, .(school.name,
                N_Students.ELA.cy,
                AchPts_Wgtd.ELA.cy = format(round(AchPts_Wgtd.ELA.cy*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.ELA.py = format(round(AchPts_Wgtd.ELA.py*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.ELA.diff = format(round(AchPts_Wgtd.ELA.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.ELA.diff =
           cell_spec(AchPts_Wgtd.ELA.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.ELA.cy =
           cell_spec(AchPts_Wgtd.ELA.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "ES \\textit{Weighted ELA} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{ELA} Pts.", "2017 \\textit{ELA} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## ES _Mathematics_ Subject-Area Indicator Scores
#'
#+ ach_math_es
## ACH.MATH.ES ==============================================================

ach.math.es.cy0 <- Rach(x = datl.eog[school.year == 2018],
                        gradeband = "ES",
                        grade_var = "testedgrade",
                        subject = "MATH",
                        subject_var = "subject",
                        subject_code = "achlevel_math",
                        assessment_type_var = "subject",
                        assessment_type_codes = "achlevel_math",
                        fay_var = "subject",
                        fay_code = "achlevel_math",
                        performance_code_var = "achlevel_rec",
                        keynames = c("school.id", "school.year", "gtid"))
ach.math.es.cy <- ach.math.es.cy0$cm.subj
ptvars.math <- grep("Pts", names(ach.math.es.cy), value = TRUE)
ach.math.es.cy[, (ptvars.math) := lapply(.SD, function(x)
  ifelse(N_Students.MATH < 15, NA, x)),
  .SD = ptvars.math]

ach.math.es.cym <- merge(ach.math.es.cy, unique(datl.eog[school.year == 2018,
                                                         .(school.id, school.name)]))
setcolorder(ach.math.es.cym, c("school.id", "school.name"))
setnames(ach.math.es.cym, c(grep("\\.MATH", names(ach.math.es.cym), value = TRUE)),
         c(paste0(grep("\\.MATH", names(ach.math.es.cym), value = TRUE), ".cy")))

ach.math.es.py0 <- Rach(x = datl.eog[school.year == 2017],
                        gradeband = "ES",
                        grade_var = "testedgrade",
                        subject = "MATH",
                        subject_var = "subject",
                        subject_code = "achlevel_math",
                        assessment_type_var = "subject",
                        assessment_type_codes = "achlevel_math",
                        fay_var = "subject",
                        fay_code = "achlevel_math",
                        performance_code_var = "achlevel_rec",
                        keynames = c("school.id", "school.year", "gtid"))
ach.math.es.py <- ach.math.es.py0$cm.subj
ptvars.math <- grep("Pts", names(ach.math.es.cy), value = TRUE)
ach.math.es.py[, (ptvars.math) := lapply(.SD, function(x)
  ifelse(N_Students.MATH < 15, NA, x)),
  .SD = ptvars.math]
ach.math.es.pym <- merge(ach.math.es.py, unique(datl.eog[school.year == 2017,
                                                         .(school.id, school.name)]))
setcolorder(ach.math.es.pym, c("school.id", "school.name"))
setnames(ach.math.es.pym, c(grep("\\.MATH", names(ach.math.es.pym), value = TRUE)),
         c(paste0(grep("\\.MATH", names(ach.math.es.pym), value = TRUE), ".py")))

ach.math.esm <- merge(ach.math.es.pym, ach.math.es.cym, all.x = FALSE, all.y = TRUE,
                      by = c("school.id", "school.name"))

setcolorder(ach.math.esm, c("school.id", "school.name",
                            grep("N_Students", names(ach.math.esm), value = TRUE),
                            grep("SumPts\\.MATH", names(ach.math.esm), value = TRUE),
                            grep("AchPts\\.MATH", names(ach.math.esm), value = TRUE),
                            grep("AchPts_Cpd\\.MATH", names(ach.math.esm), value = TRUE),
                            grep("AchPts_Wgtd\\.MATH", names(ach.math.esm), value = TRUE)))

ach.math.esm[, AchPts_Wgtd.MATH.diff := AchPts_Wgtd.MATH.cy - AchPts_Wgtd.MATH.py]

#'
#+ ach_math_es_output, echo=FALSE, results='asis'

setkey(ach.math.esm, "school.name")
library(kableExtra); library(dplyr)
ach.math.esm[, .(school.name,
                 N_Students.MATH.cy,
                 AchPts_Wgtd.MATH.cy = format(round(AchPts_Wgtd.MATH.cy*100, digits = 2), nsmall = 2),
                 AchPts_Wgtd.MATH.py = format(round(AchPts_Wgtd.MATH.py*100, digits = 2), nsmall = 2),
                 AchPts_Wgtd.MATH.diff = format(round(AchPts_Wgtd.MATH.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.MATH.diff =
           cell_spec(AchPts_Wgtd.MATH.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.MATH.cy =
           cell_spec(AchPts_Wgtd.MATH.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "ES \\textit{Weighted Mathematics} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Math} Pts.", "2017 \\textit{Math} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## ES _Science_ Subject-Area Indicator Scores
#'
#+ ach_sci_es
## ACH.SCI.ES ==============================================================

ach.sci.es.cy0 <- Rach(x = datl.eog[school.year == 2018],
                       gradeband = "ES",
                       grade_var = "testedgrade",
                       subject = "SCI",
                       subject_var = "subject",
                       subject_code = "achlevel_sci",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_sci",
                       fay_var = "subject",
                       fay_code = "achlevel_sci",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.sci.es.cy <- ach.sci.es.cy0$cm.subj
ptvars.sci <- grep("Pts", names(ach.sci.es.cy), value = TRUE)
ach.sci.es.cy[, (ptvars.sci) := lapply(.SD, function(x)
  ifelse(N_Students.SCI < 15, NA, x)),
  .SD = ptvars.sci]

ach.sci.es.cym <- merge(ach.sci.es.cy, unique(datl.eog[school.year == 2018,
                                                       .(school.id, school.name)]))
setcolorder(ach.sci.es.cym, c("school.id", "school.name"))
setnames(ach.sci.es.cym, c(grep("\\.SCI", names(ach.sci.es.cym), value = TRUE)),
         c(paste0(grep("\\.SCI", names(ach.sci.es.cym), value = TRUE), ".cy")))

ach.sci.es.py0 <- Rach(x = datl.eog[school.year == 2017],
                       gradeband = "ES",
                       grade_var = "testedgrade",
                       subject = "SCI",
                       subject_var = "subject",
                       subject_code = "achlevel_sci",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_sci",
                       fay_var = "subject",
                       fay_code = "achlevel_sci",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.sci.es.py <- ach.sci.es.py0$cm.subj
ach.sci.es.py[, (ptvars.sci) := lapply(.SD, function(x)
  ifelse(N_Students.SCI < 15, NA, x)),
  .SD = ptvars.sci]


ach.sci.es.pym <- merge(ach.sci.es.py, unique(datl.eog[school.year == 2017,
                                                       .(school.id, school.name)]))
setcolorder(ach.sci.es.pym, c("school.id", "school.name"))
setnames(ach.sci.es.pym, c(grep("\\.SCI", names(ach.sci.es.pym), value = TRUE)),
         c(paste0(grep("\\.SCI", names(ach.sci.es.pym), value = TRUE), ".py")))

ach.sci.esm <- merge(ach.sci.es.pym, ach.sci.es.cym, all.x = FALSE, all.y = TRUE,
                     by = c("school.id", "school.name"))

setcolorder(ach.sci.esm, c("school.id", "school.name",
                           grep("N_Students", names(ach.sci.esm), value = TRUE),
                           grep("SumPts\\.SCI", names(ach.sci.esm), value = TRUE),
                           grep("AchPts\\.SCI", names(ach.sci.esm), value = TRUE),
                           grep("AchPts_Cpd\\.SCI", names(ach.sci.esm), value = TRUE),
                           grep("AchPts_Wgtd\\.SCI", names(ach.sci.esm), value = TRUE)))

ach.sci.esm[, AchPts_Wgtd.SCI.diff := AchPts_Wgtd.SCI.cy - AchPts_Wgtd.SCI.py]

#'
#+ ach_sci_es_output, echo=FALSE, results='asis'
setkey(ach.sci.esm, "school.name")
ach.sci.esm[, .(school.name,
                N_Students.SCI.cy,
                AchPts_Wgtd.SCI.cy = format(round(AchPts_Wgtd.SCI.cy*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.SCI.py = format(round(AchPts_Wgtd.SCI.py*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.SCI.diff = format(round(AchPts_Wgtd.SCI.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.SCI.diff =
           cell_spec(AchPts_Wgtd.SCI.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.SCI.cy =
           cell_spec(AchPts_Wgtd.SCI.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "ES \\textit{Weighted Science} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Science} Pts.", "2017 \\textit{Science} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## ES _Social Studies (SS)_ Subject-Area Indicator Scores
#'
#+ ach_ss_es
## ACH.SS.ES ==============================================================

ach.ss.es.cy0 <- Rach(x = datl.eog[school.year == 2018],
                      gradeband = "ES",
                      grade_var = "testedgrade",
                      subject = "SS",
                      subject_var = "subject",
                      subject_code = "achlevel_soc",
                      assessment_type_var = "subject",
                      assessment_type_codes = "achlevel_soc",
                      fay_var = "subject",
                      fay_code = "achlevel_soc",
                      performance_code_var = "achlevel_rec",
                      keynames = c("school.id", "school.year", "gtid"))
ach.ss.es.cy <- ach.ss.es.cy0$cm.subj

ptvars.ss <- grep("Pts", names(ach.ss.es.cy), value = TRUE)
ach.ss.es.cy[, (ptvars.ss) := lapply(.SD, function(x)
  ifelse(N_Students.SS < 15, NA, x)),
  .SD = ptvars.ss]

ach.ss.es.cym <- merge(ach.ss.es.cy, unique(datl.eog[school.year == 2018,
                                                     .(school.id, school.name)]))
setcolorder(ach.ss.es.cym, c("school.id", "school.name"))
setnames(ach.ss.es.cym, c(grep("\\.SS", names(ach.ss.es.cym), value = TRUE)),
         c(paste0(grep("\\.SS", names(ach.ss.es.cym), value = TRUE), ".cy")))

ach.ss.es.py0 <- Rach(x = datl.eog[school.year == 2017],
                      gradeband = "ES",
                      grade_var = "testedgrade",
                      subject = "SS",
                      subject_var = "subject",
                      subject_code = "achlevel_soc",
                      assessment_type_var = "subject",
                      assessment_type_codes = "achlevel_soc",
                      fay_var = "subject",
                      fay_code = "achlevel_soc",
                      performance_code_var = "achlevel_rec",
                      keynames = c("school.id", "school.year", "gtid"))
ach.ss.es.py <- ach.ss.es.py0$cm.subj

ach.ss.es.py[, (ptvars.ss) := lapply(.SD, function(x)
  ifelse(N_Students.SS < 15, NA, x)),
  .SD = ptvars.ss]

ach.ss.es.pym <- merge(ach.ss.es.py, unique(datl.eog[school.year == 2017,
                                                     .(school.id, school.name)]))
setcolorder(ach.ss.es.pym, c("school.id", "school.name"))
setnames(ach.ss.es.pym, c(grep("\\.SS", names(ach.ss.es.pym), value = TRUE)),
         c(paste0(grep("\\.SS", names(ach.ss.es.pym), value = TRUE), ".py")))

ach.ss.esm <- merge(ach.ss.es.pym, ach.ss.es.cym, all.x = FALSE, all.y = TRUE, by =
                      c("school.id", "school.name"))

setcolorder(ach.ss.esm, c("school.id", "school.name",
                          grep("N_Students", names(ach.ss.esm), value = TRUE),
                          grep("SumPts\\.SS", names(ach.ss.esm), value = TRUE),
                          grep("AchPts\\.SS", names(ach.ss.esm), value = TRUE),
                          grep("AchPts_Cpd\\.SS", names(ach.ss.esm), value = TRUE),
                          grep("AchPts_Wgtd\\.SS", names(ach.ss.esm), value = TRUE)))

ach.ss.esm[, AchPts_Wgtd.SS.diff := AchPts_Wgtd.SS.cy - AchPts_Wgtd.SS.py]

#'
#+ ach_ss_es_output, echo=FALSE, results='asis'
setkey(ach.ss.esm, "school.name")
ach.ss.esm[, .(school.name,
               N_Students.SS.cy,
               AchPts_Wgtd.SS.cy = format(round(AchPts_Wgtd.SS.cy*100, digits = 2), nsmall = 2),
               AchPts_Wgtd.SS.py = format(round(AchPts_Wgtd.SS.py*100, digits = 2), nsmall = 2),
               AchPts_Wgtd.SS.diff = format(round(AchPts_Wgtd.SS.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.SS.diff =
           cell_spec(AchPts_Wgtd.SS.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.SS.cy =
           cell_spec(AchPts_Wgtd.SS.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "ES \\textit{Weighted SS} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Science} Pts.", "2017 \\textit{Science} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## Elementary Schools' Content Mastery Component _Composite Scores_
#'
#+ es_cm_composite, results='asis'
# ES CONTENT MASTERY COMPOSITE SCORE --------------------------------------

ach.esm <- lapply(list(ach.ela.esm, ach.math.esm, ach.sci.esm, ach.ss.esm),
                  function(x) setkey(x, "school.id"))
names(ach.esm) <- c("ELA", "MATH", "SCI", "SS")
ach.esm <- lapply(ach.esm, function(x)
    x[, .SD, .SDcols = c("school.id", "school.name",
                         grep("N_Students", names(x), value = TRUE),
                         grep("AchPts_Wgtd", names(x), value = TRUE))])

cm.esm <- Reduce(function(x, y)
  merge(x, y, by = c("school.id", "school.name"), all = TRUE),
  ach.esm)

cm.esm[, cm.cy := 100*sum(.SD, na.rm = TRUE), by = "school.id",
       .SDcols = grep("AchPts_Wgtd\\.\\w+\\.cy", names(cm.esm), value = TRUE)]
cm.esm[, cm.py := 100*sum(.SD, na.rm = TRUE), by = "school.id",
       .SDcols = grep("AchPts_Wgtd\\.\\w+\\.py", names(cm.esm), value = TRUE)]
cm.esm[, cm.diff := cm.cy - cm.py, by = "school.id"]

#'
#+ cm_es_output, echo=FALSE, results='asis'
setkey(cm.esm, "school.name")
cm.esm[, .(school.name,
               cm.cy = format(round(cm.cy, digits = 2), nsmall = 2),
               cm.py = format(round(cm.py, digits = 2), nsmall = 2),
               cm.diff = format(round(cm.diff, digits = 2), nsmall = 2))] %>%
  mutate(cm.diff = cell_spec(cm.diff, format = "latex",
                             italic = ifelse(Rna(cm.cy) < Rna(cm.py),
                                             TRUE, FALSE),
                             color = ifelse(Rna(cm.cy) < Rna(cm.py),
                                            "darkred", "black"),
                             escape = FALSE),
         cm.cy = cell_spec(cm.cy, format = "latex",
                           bold = TRUE,
                           color = ifelse(Rna(cm.cy) < Rna(cm.py),
                                          "darkred", "black"),
                           escape = FALSE)) %>%
  kable(caption = "ES \\textit{Composite} Content Mastery Points",
        col.names = c("School",
                      "\\textbf{2018 \\textit{Composite} Pts.}", "2017 \\textit{Composite} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))
#'
#' \newpage
#' # Middle Schools (MS) Scores
#'
#' -----
#'
#' ## MS _English Language Arts (ELA)_ Subject-Area Indicator Scores
#'
#+ ach_ela_ms
## ACH.ELA.MS ==============================================================

ach.ela.ms.cy0 <- Rach(x = datl.eog[school.year == 2018],
                       gradeband = "MS",
                       grade_var = "testedgrade",
                       subject = "ELA",
                       subject_var = "subject",
                       subject_code = "achlevel_ela",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_ela",
                       fay_var = "subject",
                       fay_code = "achlevel_ela",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.ela.ms.cy <- ach.ela.ms.cy0$cm.subj

ptvars.ela <- grep("Pts", names(ach.ela.ms.cy), value = TRUE)
ach.ela.ms.cy[, (ptvars.ela) := lapply(.SD, function(x)
  ifelse(N_Students.ELA < 15, NA, x)),
  .SD = ptvars.ela]

ach.ela.ms.cym <- merge(ach.ela.ms.cy, unique(datl.eog[school.year == 2018,
                                                       .(school.id, school.name)]))
setcolorder(ach.ela.ms.cym, c("school.id", "school.name"))
setnames(ach.ela.ms.cym, c(grep("\\.ELA", names(ach.ela.ms.cym), value = TRUE)),
         c(paste0(grep("\\.ELA", names(ach.ela.ms.cym), value = TRUE), ".cy")))

ach.ela.ms.py0 <- Rach(x = datl.eog[school.year == 2017],
                       gradeband = "MS",
                       grade_var = "testedgrade",
                       subject = "ELA",
                       subject_var = "subject",
                       subject_code = "achlevel_ela",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_ela",
                       fay_var = "subject",
                       fay_code = "achlevel_ela",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.ela.ms.py <- ach.ela.ms.py0$cm.subj
ach.ela.ms.py[, (ptvars.ela) := lapply(.SD, function(x)
  ifelse(N_Students.ELA < 15, NA, x)),
  .SD = ptvars.ela]


ach.ela.ms.pym <- merge(ach.ela.ms.py, unique(datl.eog[school.year == 2017,
                                                       .(school.id, school.name)]))
setcolorder(ach.ela.ms.pym, c("school.id", "school.name"))
setnames(ach.ela.ms.pym, c(grep("\\.ELA", names(ach.ela.ms.pym), value = TRUE)),
         c(paste0(grep("\\.ELA", names(ach.ela.ms.pym), value = TRUE), ".py")))

ach.ela.msm <- merge(ach.ela.ms.pym, ach.ela.ms.cym, all.x = FALSE, all.y = TRUE,
                     by = c("school.id", "school.name"))

setcolorder(ach.ela.msm, c("school.id", "school.name",
                           grep("N_Students", names(ach.ela.msm), value = TRUE),
                           grep("SumPts\\.ELA", names(ach.ela.msm), value = TRUE),
                           grep("AchPts\\.ELA", names(ach.ela.msm), value = TRUE),
                           grep("AchPts_Cpd\\.ELA", names(ach.ela.msm), value = TRUE),
                           grep("AchPts_Wgtd\\.ELA", names(ach.ela.msm), value = TRUE)))

ach.ela.msm[, AchPts_Wgtd.ELA.diff := AchPts_Wgtd.ELA.cy - AchPts_Wgtd.ELA.py]

#'
#+ ach_ela_ms_output, echo=FALSE, results='asis'
setkey(ach.ela.msm, "school.name")
ach.ela.msm[, .(school.name,
                N_Students.ELA.cy,
                AchPts_Wgtd.ELA.cy = format(round(AchPts_Wgtd.ELA.cy*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.ELA.py = format(round(AchPts_Wgtd.ELA.py*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.ELA.diff = format(round(AchPts_Wgtd.ELA.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.ELA.diff =
           cell_spec(AchPts_Wgtd.ELA.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.ELA.cy =
           cell_spec(AchPts_Wgtd.ELA.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "MS \\textit{Weighted ELA} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{ELA} Pts.", "2017 \\textit{ELA} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## MS _Mathematics_ Subject-Area Indicator Scores
#'
#+ ach_math_ms
## ACH.MATH.MS ==============================================================

ach.math.ms.cy0 <- Rach(x = datl.eog[school.year == 2018],
                        gradeband = "MS",
                        grade_var = "testedgrade",
                        subject = "MATH",
                        subject_var = "subject",
                        subject_code = "achlevel_math",
                        assessment_type_var = "subject",
                        assessment_type_codes = "achlevel_math",
                        fay_var = "subject",
                        fay_code = "achlevel_math",
                        performance_code_var = "achlevel_rec",
                        keynames = c("school.id", "school.year", "gtid"))
ach.math.ms.cy <- ach.math.ms.cy0$cm.subj
ptvars.math <- grep("Pts", names(ach.math.ms.cy), value = TRUE)
ach.math.ms.cy[, (ptvars.math) := lapply(.SD, function(x)
  ifelse(N_Students.MATH < 15, NA, x)),
  .SD = ptvars.math]

ach.math.ms.cym <- merge(ach.math.ms.cy, unique(datl.eog[school.year == 2018,
                                                         .(school.id, school.name)]))
setcolorder(ach.math.ms.cym, c("school.id", "school.name"))
setnames(ach.math.ms.cym, c(grep("\\.MATH", names(ach.math.ms.cym), value = TRUE)),
         c(paste0(grep("\\.MATH", names(ach.math.ms.cym), value = TRUE), ".cy")))

ach.math.ms.py0 <- Rach(x = datl.eog[school.year == 2017],
                        gradeband = "MS",
                        grade_var = "testedgrade",
                        subject = "MATH",
                        subject_var = "subject",
                        subject_code = "achlevel_math",
                        assessment_type_var = "subject",
                        assessment_type_codes = "achlevel_math",
                        fay_var = "subject",
                        fay_code = "achlevel_math",
                        performance_code_var = "achlevel_rec",
                        keynames = c("school.id", "school.year", "gtid"))
ach.math.ms.py <- ach.math.ms.py0$cm.subj
ptvars.math <- grep("Pts", names(ach.math.ms.cy), value = TRUE)
ach.math.ms.py[, (ptvars.math) := lapply(.SD, function(x)
  ifelse(N_Students.MATH < 15, NA, x)),
  .SD = ptvars.math]
ach.math.ms.pym <- merge(ach.math.ms.py, unique(datl.eog[school.year == 2017,
                                                         .(school.id, school.name)]))
setcolorder(ach.math.ms.pym, c("school.id", "school.name"))
setnames(ach.math.ms.pym, c(grep("\\.MATH", names(ach.math.ms.pym), value = TRUE)),
         c(paste0(grep("\\.MATH", names(ach.math.ms.pym), value = TRUE), ".py")))

ach.math.msm <- merge(ach.math.ms.pym, ach.math.ms.cym, all.x = FALSE, all.y = TRUE,
                      by = c("school.id", "school.name"))

setcolorder(ach.math.msm, c("school.id", "school.name",
                            grep("N_Students", names(ach.math.msm), value = TRUE),
                            grep("SumPts\\.MATH", names(ach.math.msm), value = TRUE),
                            grep("AchPts\\.MATH", names(ach.math.msm), value = TRUE),
                            grep("AchPts_Cpd\\.MATH", names(ach.math.msm), value = TRUE),
                            grep("AchPts_Wgtd\\.MATH", names(ach.math.msm), value = TRUE)))

ach.math.msm[, AchPts_Wgtd.MATH.diff := AchPts_Wgtd.MATH.cy - AchPts_Wgtd.MATH.py]

#'
#+ ach_math_ms_output, echo=FALSE, results='asis'

setkey(ach.math.msm, "school.name")
library(kableExtra); library(dplyr)
ach.math.msm[, .(school.name,
                 N_Students.MATH.cy,
                 AchPts_Wgtd.MATH.cy = format(round(AchPts_Wgtd.MATH.cy*100, digits = 2), nsmall = 2),
                 AchPts_Wgtd.MATH.py = format(round(AchPts_Wgtd.MATH.py*100, digits = 2), nsmall = 2),
                 AchPts_Wgtd.MATH.diff = format(round(AchPts_Wgtd.MATH.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.MATH.diff =
           cell_spec(AchPts_Wgtd.MATH.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.MATH.cy =
           cell_spec(AchPts_Wgtd.MATH.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "MS \\textit{Weighted Mathematics} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Math} Pts.", "2017 \\textit{Math} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## MS _Science_ Subject-Area Indicator Scores
#'
#+ ach_sci_ms
## ACH.SCI.MS ==============================================================

ach.sci.ms.cy0 <- Rach(x = datl.eog[school.year == 2018],
                       gradeband = "MS",
                       grade_var = "testedgrade",
                       subject = "SCI",
                       subject_var = "subject",
                       subject_code = "achlevel_sci",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_sci",
                       fay_var = "subject",
                       fay_code = "achlevel_sci",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.sci.ms.cy <- ach.sci.ms.cy0$cm.subj
ptvars.sci <- grep("Pts", names(ach.sci.ms.cy), value = TRUE)
ach.sci.ms.cy[, (ptvars.sci) := lapply(.SD, function(x)
  ifelse(N_Students.SCI < 15, NA, x)),
  .SD = ptvars.sci]

ach.sci.ms.cym <- merge(ach.sci.ms.cy, unique(datl.eog[school.year == 2018,
                                                       .(school.id, school.name)]))
setcolorder(ach.sci.ms.cym, c("school.id", "school.name"))
setnames(ach.sci.ms.cym, c(grep("\\.SCI", names(ach.sci.ms.cym), value = TRUE)),
         c(paste0(grep("\\.SCI", names(ach.sci.ms.cym), value = TRUE), ".cy")))

ach.sci.ms.py0 <- Rach(x = datl.eog[school.year == 2017],
                       gradeband = "MS",
                       grade_var = "testedgrade",
                       subject = "SCI",
                       subject_var = "subject",
                       subject_code = "achlevel_sci",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_sci",
                       fay_var = "subject",
                       fay_code = "achlevel_sci",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.sci.ms.py <- ach.sci.ms.py0$cm.subj
ach.sci.ms.py[, (ptvars.sci) := lapply(.SD, function(x)
  ifelse(N_Students.SCI < 15, NA, x)),
  .SD = ptvars.sci]


ach.sci.ms.pym <- merge(ach.sci.ms.py, unique(datl.eog[school.year == 2017,
                                                       .(school.id, school.name)]))
setcolorder(ach.sci.ms.pym, c("school.id", "school.name"))
setnames(ach.sci.ms.pym, c(grep("\\.SCI", names(ach.sci.ms.pym), value = TRUE)),
         c(paste0(grep("\\.SCI", names(ach.sci.ms.pym), value = TRUE), ".py")))

ach.sci.msm <- merge(ach.sci.ms.pym, ach.sci.ms.cym, all.x = FALSE, all.y = TRUE,
                     by = c("school.id", "school.name"))

setcolorder(ach.sci.msm, c("school.id", "school.name",
                           grep("N_Students", names(ach.sci.msm), value = TRUE),
                           grep("SumPts\\.SCI", names(ach.sci.msm), value = TRUE),
                           grep("AchPts\\.SCI", names(ach.sci.msm), value = TRUE),
                           grep("AchPts_Cpd\\.SCI", names(ach.sci.msm), value = TRUE),
                           grep("AchPts_Wgtd\\.SCI", names(ach.sci.msm), value = TRUE)))

ach.sci.msm[, AchPts_Wgtd.SCI.diff := AchPts_Wgtd.SCI.cy - AchPts_Wgtd.SCI.py]

#'
#+ ach_sci_ms_output, echo=FALSE, results='asis'
setkey(ach.sci.msm, "school.name")
ach.sci.msm[, .(school.name,
                N_Students.SCI.cy,
                AchPts_Wgtd.SCI.cy = format(round(AchPts_Wgtd.SCI.cy*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.SCI.py = format(round(AchPts_Wgtd.SCI.py*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.SCI.diff = format(round(AchPts_Wgtd.SCI.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.SCI.diff =
           cell_spec(AchPts_Wgtd.SCI.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.SCI.cy =
           cell_spec(AchPts_Wgtd.SCI.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "MS \\textit{Weighted Science} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Science} Pts.", "2017 \\textit{Science} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## MS _Social Studies (SS)_ Subject-Area Indicator Scores
#'
#+ ach_ss_ms
## ACH.SS.MS ==============================================================

ach.ss.ms.cy0 <- Rach(x = datl.eog[school.year == 2018],
                      gradeband = "MS",
                      grade_var = "testedgrade",
                      subject = "SS",
                      subject_var = "subject",
                      subject_code = "achlevel_soc",
                      assessment_type_var = "subject",
                      assessment_type_codes = "achlevel_soc",
                      fay_var = "subject",
                      fay_code = "achlevel_soc",
                      performance_code_var = "achlevel_rec",
                      keynames = c("school.id", "school.year", "gtid"))
ach.ss.ms.cy <- ach.ss.ms.cy0$cm.subj

ptvars.ss <- grep("Pts", names(ach.ss.ms.cy), value = TRUE)
ach.ss.ms.cy[, (ptvars.ss) := lapply(.SD, function(x)
  ifelse(N_Students.SS < 15, NA, x)),
  .SD = ptvars.ss]

ach.ss.ms.cym <- merge(ach.ss.ms.cy, unique(datl.eog[school.year == 2018,
                                                     .(school.id, school.name)]))
setcolorder(ach.ss.ms.cym, c("school.id", "school.name"))
setnames(ach.ss.ms.cym, c(grep("\\.SS", names(ach.ss.ms.cym), value = TRUE)),
         c(paste0(grep("\\.SS", names(ach.ss.ms.cym), value = TRUE), ".cy")))

ach.ss.ms.py0 <- Rach(x = datl.eog[school.year == 2017],
                      gradeband = "MS",
                      grade_var = "testedgrade",
                      subject = "SS",
                      subject_var = "subject",
                      subject_code = "achlevel_soc",
                      assessment_type_var = "subject",
                      assessment_type_codes = "achlevel_soc",
                      fay_var = "subject",
                      fay_code = "achlevel_soc",
                      performance_code_var = "achlevel_rec",
                      keynames = c("school.id", "school.year", "gtid"))
ach.ss.ms.py <- ach.ss.ms.py0$cm.subj

ach.ss.ms.py[, (ptvars.ss) := lapply(.SD, function(x)
  ifelse(N_Students.SS < 15, NA, x)),
  .SD = ptvars.ss]

ach.ss.ms.pym <- merge(ach.ss.ms.py, unique(datl.eog[school.year == 2017,
                                                     .(school.id, school.name)]))
setcolorder(ach.ss.ms.pym, c("school.id", "school.name"))
setnames(ach.ss.ms.pym, c(grep("\\.SS", names(ach.ss.ms.pym), value = TRUE)),
         c(paste0(grep("\\.SS", names(ach.ss.ms.pym), value = TRUE), ".py")))

ach.ss.msm <- merge(ach.ss.ms.pym, ach.ss.ms.cym, all.x = FALSE, all.y = TRUE, by =
                      c("school.id", "school.name"))

setcolorder(ach.ss.msm, c("school.id", "school.name",
                          grep("N_Students", names(ach.ss.msm), value = TRUE),
                          grep("SumPts\\.SS", names(ach.ss.msm), value = TRUE),
                          grep("AchPts\\.SS", names(ach.ss.msm), value = TRUE),
                          grep("AchPts_Cpd\\.SS", names(ach.ss.msm), value = TRUE),
                          grep("AchPts_Wgtd\\.SS", names(ach.ss.msm), value = TRUE)))

ach.ss.msm[, AchPts_Wgtd.SS.diff := AchPts_Wgtd.SS.cy - AchPts_Wgtd.SS.py]

#'
#+ ach_ss_ms_output, echo=FALSE, results='asis'
setkey(ach.ss.msm, "school.name")
ach.ss.msm[, .(school.name,
               N_Students.SS.cy,
               AchPts_Wgtd.SS.cy = format(round(AchPts_Wgtd.SS.cy*100, digits = 2), nsmall = 2),
               AchPts_Wgtd.SS.py = format(round(AchPts_Wgtd.SS.py*100, digits = 2), nsmall = 2),
               AchPts_Wgtd.SS.diff = format(round(AchPts_Wgtd.SS.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.SS.diff =
           cell_spec(AchPts_Wgtd.SS.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.SS.cy =
           cell_spec(AchPts_Wgtd.SS.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "MS \\textit{Weighted SS} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Science} Pts.", "2017 \\textit{Science} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## Middle Schools' EOG Content Mastery Component _Composite Scores_
#'
#+ ms_cm_composite, results='asis'
# ES CONTENT MASTERY COMPOSITE SCORE --------------------------------------

ach.msm <- lapply(list(ach.ela.msm, ach.math.msm, ach.sci.msm, ach.ss.msm),
                  function(x) setkey(x, "school.id"))
names(ach.msm) <- c("ELA", "MATH", "SCI", "SS")
ach.msm <- lapply(ach.msm, function(x)
  x[, .SD, .SDcols = c("school.id", "school.name",
                       grep("N_Students", names(x), value = TRUE),
                       grep("AchPts_Wgtd", names(x), value = TRUE))])

cm.msm <- Reduce(function(x, y)
  merge(x, y, by = c("school.id", "school.name"), all = TRUE),
  ach.msm)

cm.msm[, cm.cy := 100*sum(.SD, na.rm = TRUE), by = "school.id",
       .SDcols = grep("AchPts_Wgtd\\.\\w+\\.cy", names(cm.msm), value = TRUE)]
cm.msm[, cm.py := 100*sum(.SD, na.rm = TRUE), by = "school.id",
       .SDcols = grep("AchPts_Wgtd\\.\\w+\\.py", names(cm.msm), value = TRUE)]
cm.msm[, cm.diff := cm.cy - cm.py, by = "school.id"]

#'
#+ cm_ms_output, echo=FALSE, results='asis'
setkey(cm.msm, "school.name")
cm.msm[, .(school.name,
           cm.cy = format(round(cm.cy, digits = 2), nsmall = 2),
           cm.py = format(round(cm.py, digits = 2), nsmall = 2),
           cm.diff = format(round(cm.diff, digits = 2), nsmall = 2))] %>%
  mutate(cm.diff = cell_spec(cm.diff, format = "latex",
                             italic = ifelse(Rna(cm.cy) < Rna(cm.py),
                                             TRUE, FALSE),
                             color = ifelse(Rna(cm.cy) < Rna(cm.py),
                                            "darkred", "black"),
                             escape = FALSE),
         cm.cy = cell_spec(cm.cy, format = "latex",
                           bold = TRUE,
                           color = ifelse(Rna(cm.cy) < Rna(cm.py),
                                          "darkred", "black"),
                           escape = FALSE)) %>%
  kable(caption = "MS \\textit{Composite} Content Mastery Points",
        col.names = c("School",
                      "\\textbf{2018 \\textit{Composite} Pts.}", "2017 \\textit{Composite} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))
#'
#' \newpage
#' # High Schools (HS) Scores

#'
#' -----
#'
#' ## HS _English Language Arts (ELA)_ Subject-Area Indicator Scores
#'
#+ ach_ela_hs
## ACH.ELA.HS ==============================================================

ach.ela.hs.cy0 <- Rach(x = dat.eoc[school.year == 2018],
                       gradeband = "HS",
                       grade_var = "testedgrade",
                       subject = "ELA",
                       subject_var = "subject",
                       subject_code = "achlevel_ela",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_ela",
                       fay_var = "subject",
                       fay_code = "achlevel_ela",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.ela.hs.cy <- ach.ela.hs.cy0$cm.subj

ptvars.ela <- grep("Pts", names(ach.ela.hs.cy), value = TRUE)
ach.ela.hs.cy[, (ptvars.ela) := lapply(.SD, function(x)
  ifelse(N_Students.ELA < 15, NA, x)),
  .SD = ptvars.ela]

ach.ela.hs.cym <- merge(ach.ela.hs.cy, unique(dat.eoc[school.year == 2018,
                                                      .(school.id, school.name)]))
setcolorder(ach.ela.hs.cym, c("school.id", "school.name"))
setnames(ach.ela.hs.cym, c(grep("\\.ELA", names(ach.ela.hs.cym), value = TRUE)),
         c(paste0(grep("\\.ELA", names(ach.ela.hs.cym), value = TRUE), ".cy")))

ach.ela.hs.py0 <- Rach(x = dat.eoc[school.year == 2017],
                       gradeband = "HS",
                       grade_var = "testedgrade",
                       subject = "ELA",
                       subject_var = "subject",
                       subject_code = "achlevel_ela",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_ela",
                       fay_var = "subject",
                       fay_code = "achlevel_ela",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.ela.hs.py <- ach.ela.hs.py0$cm.subj
ach.ela.hs.py[, (ptvars.ela) := lapply(.SD, function(x)
  ifelse(N_Students.ELA < 15, NA, x)),
  .SD = ptvars.ela]


ach.ela.hs.pym <- merge(ach.ela.hs.py, unique(dat.eoc[school.year == 2017,
                                                      .(school.id, school.name)]))
setcolorder(ach.ela.hs.pym, c("school.id", "school.name"))
setnames(ach.ela.hs.pym, c(grep("\\.ELA", names(ach.ela.hs.pym), value = TRUE)),
         c(paste0(grep("\\.ELA", names(ach.ela.hs.pym), value = TRUE), ".py")))

ach.ela.hsm <- merge(ach.ela.hs.pym, ach.ela.hs.cym, all.x = FALSE, all.y = TRUE,
                     by = c("school.id", "school.name"))

setcolorder(ach.ela.hsm, c("school.id", "school.name",
                           grep("N_Students", names(ach.ela.hsm), value = TRUE),
                           grep("SumPts\\.ELA", names(ach.ela.hsm), value = TRUE),
                           grep("AchPts\\.ELA", names(ach.ela.hsm), value = TRUE),
                           grep("AchPts_Cpd\\.ELA", names(ach.ela.hsm), value = TRUE),
                           grep("AchPts_Wgtd\\.ELA", names(ach.ela.hsm), value = TRUE)))

ach.ela.hsm[, AchPts_Wgtd.ELA.diff := AchPts_Wgtd.ELA.cy - AchPts_Wgtd.ELA.py]

#'
#+ ach_ela_hs_output, echo=FALSE, results='asis'
setkey(ach.ela.hsm, "school.name")
ach.ela.hsm[, .(school.name,
                N_Students.ELA.cy,
                AchPts_Wgtd.ELA.cy = format(round(AchPts_Wgtd.ELA.cy*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.ELA.py = format(round(AchPts_Wgtd.ELA.py*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.ELA.diff = format(round(AchPts_Wgtd.ELA.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.ELA.diff =
           cell_spec(AchPts_Wgtd.ELA.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.ELA.cy =
           cell_spec(AchPts_Wgtd.ELA.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.ELA.cy) < Rna(AchPts_Wgtd.ELA.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "HS \\textit{Weighted ELA} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{ELA} Pts.", "2017 \\textit{ELA} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## HS _Mathematics_ Subject-Area Indicator Scores
#'
#+ ach_math_hs
## ACH.MATH.HS ==============================================================

ach.math.hs.cy0 <- Rach(x = dat.eoc[school.year == 2018],
                        gradeband = "HS",
                        grade_var = "testedgrade",
                        subject = "MATH",
                        subject_var = "subject",
                        subject_code = "achlevel_math",
                        assessment_type_var = "subject",
                        assessment_type_codes = "achlevel_math",
                        fay_var = "subject",
                        fay_code = "achlevel_math",
                        performance_code_var = "achlevel_rec",
                        keynames = c("school.id", "school.year", "gtid"))
ach.math.hs.cy <- ach.math.hs.cy0$cm.subj
ptvars.math <- grep("Pts", names(ach.math.hs.cy), value = TRUE)
ach.math.hs.cy[, (ptvars.math) := lapply(.SD, function(x)
  ifelse(N_Students.MATH < 15, NA, x)),
  .SD = ptvars.math]

ach.math.hs.cym <- merge(ach.math.hs.cy, unique(dat.eoc[school.year == 2018,
                                                        .(school.id, school.name)]))
setcolorder(ach.math.hs.cym, c("school.id", "school.name"))
setnames(ach.math.hs.cym, c(grep("\\.MATH", names(ach.math.hs.cym), value = TRUE)),
         c(paste0(grep("\\.MATH", names(ach.math.hs.cym), value = TRUE), ".cy")))

ach.math.hs.py0 <- Rach(x = dat.eoc[school.year == 2017],
                        gradeband = "HS",
                        grade_var = "testedgrade",
                        subject = "MATH",
                        subject_var = "subject",
                        subject_code = "achlevel_math",
                        assessment_type_var = "subject",
                        assessment_type_codes = "achlevel_math",
                        fay_var = "subject",
                        fay_code = "achlevel_math",
                        performance_code_var = "achlevel_rec",
                        keynames = c("school.id", "school.year", "gtid"))
ach.math.hs.py <- ach.math.hs.py0$cm.subj
ptvars.math <- grep("Pts", names(ach.math.hs.cy), value = TRUE)
ach.math.hs.py[, (ptvars.math) := lapply(.SD, function(x)
  ifelse(N_Students.MATH < 15, NA, x)),
  .SD = ptvars.math]
ach.math.hs.pym <- merge(ach.math.hs.py, unique(dat.eoc[school.year == 2017,
                                                        .(school.id, school.name)]))
setcolorder(ach.math.hs.pym, c("school.id", "school.name"))
setnames(ach.math.hs.pym, c(grep("\\.MATH", names(ach.math.hs.pym), value = TRUE)),
         c(paste0(grep("\\.MATH", names(ach.math.hs.pym), value = TRUE), ".py")))

ach.math.hsm <- merge(ach.math.hs.pym, ach.math.hs.cym, all.x = FALSE, all.y = TRUE,
                      by = c("school.id", "school.name"))

setcolorder(ach.math.hsm, c("school.id", "school.name",
                            grep("N_Students", names(ach.math.hsm), value = TRUE),
                            grep("SumPts\\.MATH", names(ach.math.hsm), value = TRUE),
                            grep("AchPts\\.MATH", names(ach.math.hsm), value = TRUE),
                            grep("AchPts_Cpd\\.MATH", names(ach.math.hsm), value = TRUE),
                            grep("AchPts_Wgtd\\.MATH", names(ach.math.hsm), value = TRUE)))

ach.math.hsm[, AchPts_Wgtd.MATH.diff := AchPts_Wgtd.MATH.cy - AchPts_Wgtd.MATH.py]

#'
#+ ach_math_hs_output, echo=FALSE, results='asis'

setkey(ach.math.hsm, "school.name")
library(kableExtra); library(dplyr)
ach.math.hsm[, .(school.name,
                 N_Students.MATH.cy,
                 AchPts_Wgtd.MATH.cy = format(round(AchPts_Wgtd.MATH.cy*100, digits = 2), nsmall = 2),
                 AchPts_Wgtd.MATH.py = format(round(AchPts_Wgtd.MATH.py*100, digits = 2), nsmall = 2),
                 AchPts_Wgtd.MATH.diff = format(round(AchPts_Wgtd.MATH.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.MATH.diff =
           cell_spec(AchPts_Wgtd.MATH.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.MATH.cy =
           cell_spec(AchPts_Wgtd.MATH.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.MATH.cy) < Rna(AchPts_Wgtd.MATH.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "HS \\textit{Weighted Mathematics} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Math} Pts.", "2017 \\textit{Math} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## HS _Science_ Subject-Area Indicator Scores
#'
#+ ach_sci_hs
## ACH.SCI.ES ==============================================================

ach.sci.hs.cy0 <- Rach(x = dat.eoc[school.year == 2018],
                       gradeband = "HS",
                       grade_var = "testedgrade",
                       subject = "SCI",
                       subject_var = "subject",
                       subject_code = "achlevel_sci",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_sci",
                       fay_var = "subject",
                       fay_code = "achlevel_sci",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.sci.hs.cy <- ach.sci.hs.cy0$cm.subj
ptvars.sci <- grep("Pts", names(ach.sci.hs.cy), value = TRUE)
ach.sci.hs.cy[, (ptvars.sci) := lapply(.SD, function(x)
  ifelse(N_Students.SCI < 15, NA, x)),
  .SD = ptvars.sci]

ach.sci.hs.cym <- merge(ach.sci.hs.cy, unique(dat.eoc[school.year == 2018,
                                                      .(school.id, school.name)]))
setcolorder(ach.sci.hs.cym, c("school.id", "school.name"))
setnames(ach.sci.hs.cym, c(grep("\\.SCI", names(ach.sci.hs.cym), value = TRUE)),
         c(paste0(grep("\\.SCI", names(ach.sci.hs.cym), value = TRUE), ".cy")))

ach.sci.hs.py0 <- Rach(x = dat.eoc[school.year == 2017],
                       gradeband = "HS",
                       grade_var = "testedgrade",
                       subject = "SCI",
                       subject_var = "subject",
                       subject_code = "achlevel_sci",
                       assessment_type_var = "subject",
                       assessment_type_codes = "achlevel_sci",
                       fay_var = "subject",
                       fay_code = "achlevel_sci",
                       performance_code_var = "achlevel_rec",
                       keynames = c("school.id", "school.year", "gtid"))
ach.sci.hs.py <- ach.sci.hs.py0$cm.subj
ach.sci.hs.py[, (ptvars.sci) := lapply(.SD, function(x)
  ifelse(N_Students.SCI < 15, NA, x)),
  .SD = ptvars.sci]


ach.sci.hs.pym <- merge(ach.sci.hs.py, unique(dat.eoc[school.year == 2017,
                                                      .(school.id, school.name)]))
setcolorder(ach.sci.hs.pym, c("school.id", "school.name"))
setnames(ach.sci.hs.pym, c(grep("\\.SCI", names(ach.sci.hs.pym), value = TRUE)),
         c(paste0(grep("\\.SCI", names(ach.sci.hs.pym), value = TRUE), ".py")))

ach.sci.hsm <- merge(ach.sci.hs.pym, ach.sci.hs.cym, all.x = FALSE, all.y = TRUE,
                     by = c("school.id", "school.name"))

setcolorder(ach.sci.hsm, c("school.id", "school.name",
                           grep("N_Students", names(ach.sci.hsm), value = TRUE),
                           grep("SumPts\\.SCI", names(ach.sci.hsm), value = TRUE),
                           grep("AchPts\\.SCI", names(ach.sci.hsm), value = TRUE),
                           grep("AchPts_Cpd\\.SCI", names(ach.sci.hsm), value = TRUE),
                           grep("AchPts_Wgtd\\.SCI", names(ach.sci.hsm), value = TRUE)))

ach.sci.hsm[, AchPts_Wgtd.SCI.diff := AchPts_Wgtd.SCI.cy - AchPts_Wgtd.SCI.py]

#'
#+ ach_sci_hs_output, echo=FALSE, results='asis'
setkey(ach.sci.hsm, "school.name")
ach.sci.hsm[, .(school.name,
                N_Students.SCI.cy,
                AchPts_Wgtd.SCI.cy = format(round(AchPts_Wgtd.SCI.cy*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.SCI.py = format(round(AchPts_Wgtd.SCI.py*100, digits = 2), nsmall = 2),
                AchPts_Wgtd.SCI.diff = format(round(AchPts_Wgtd.SCI.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.SCI.diff =
           cell_spec(AchPts_Wgtd.SCI.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.SCI.cy =
           cell_spec(AchPts_Wgtd.SCI.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.SCI.cy) < Rna(AchPts_Wgtd.SCI.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "HS \\textit{Weighted Science} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Science} Pts.", "2017 \\textit{Science} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## HS _Social Studies (SS)_ Subject-Area Indicator Scores
#'
#+ ach_ss_hs
## ACH.SS.HS ==============================================================

ach.ss.hs.cy0 <- Rach(x = dat.eoc[school.year == 2018],
                      gradeband = "HS",
                      grade_var = "testedgrade",
                      subject = "SS",
                      subject_var = "subject",
                      subject_code = "achlevel_soc",
                      assessment_type_var = "subject",
                      assessment_type_codes = "achlevel_soc",
                      fay_var = "subject",
                      fay_code = "achlevel_soc",
                      performance_code_var = "achlevel_rec",
                      keynames = c("school.id", "school.year", "gtid"))
ach.ss.hs.cy <- ach.ss.hs.cy0$cm.subj

ptvars.ss <- grep("Pts", names(ach.ss.hs.cy), value = TRUE)
ach.ss.hs.cy[, (ptvars.ss) := lapply(.SD, function(x)
  ifelse(N_Students.SS < 15, NA, x)),
  .SD = ptvars.ss]

ach.ss.hs.cym <- merge(ach.ss.hs.cy, unique(dat.eoc[school.year == 2018,
                                                    .(school.id, school.name)]))
setcolorder(ach.ss.hs.cym, c("school.id", "school.name"))
setnames(ach.ss.hs.cym, c(grep("\\.SS", names(ach.ss.hs.cym), value = TRUE)),
         c(paste0(grep("\\.SS", names(ach.ss.hs.cym), value = TRUE), ".cy")))

ach.ss.hs.py0 <- Rach(x = dat.eoc[school.year == 2017],
                      gradeband = "HS",
                      grade_var = "testedgrade",
                      subject = "SS",
                      subject_var = "subject",
                      subject_code = "achlevel_soc",
                      assessment_type_var = "subject",
                      assessment_type_codes = "achlevel_soc",
                      fay_var = "subject",
                      fay_code = "achlevel_soc",
                      performance_code_var = "achlevel_rec",
                      keynames = c("school.id", "school.year", "gtid"))
ach.ss.hs.py <- ach.ss.hs.py0$cm.subj

ach.ss.hs.py[, (ptvars.ss) := lapply(.SD, function(x)
  ifelse(N_Students.SS < 15, NA, x)),
  .SD = ptvars.ss]

ach.ss.hs.pym <- merge(ach.ss.hs.py, unique(dat.eoc[school.year == 2017,
                                                    .(school.id, school.name)]))
setcolorder(ach.ss.hs.pym, c("school.id", "school.name"))
setnames(ach.ss.hs.pym, c(grep("\\.SS", names(ach.ss.hs.pym), value = TRUE)),
         c(paste0(grep("\\.SS", names(ach.ss.hs.pym), value = TRUE), ".py")))

ach.ss.hsm <- merge(ach.ss.hs.pym, ach.ss.hs.cym, all.x = FALSE, all.y = TRUE, by =
                      c("school.id", "school.name"))

setcolorder(ach.ss.hsm, c("school.id", "school.name",
                          grep("N_Students", names(ach.ss.hsm), value = TRUE),
                          grep("SumPts\\.SS", names(ach.ss.hsm), value = TRUE),
                          grep("AchPts\\.SS", names(ach.ss.hsm), value = TRUE),
                          grep("AchPts_Cpd\\.SS", names(ach.ss.hsm), value = TRUE),
                          grep("AchPts_Wgtd\\.SS", names(ach.ss.hsm), value = TRUE)))

ach.ss.hsm[, AchPts_Wgtd.SS.diff := AchPts_Wgtd.SS.cy - AchPts_Wgtd.SS.py]

#'
#+ ach_ss_hs_output, echo=FALSE, results='asis'
setkey(ach.ss.hsm, "school.name")
ach.ss.hsm[, .(school.name,
               N_Students.SS.cy,
               AchPts_Wgtd.SS.cy = format(round(AchPts_Wgtd.SS.cy*100, digits = 2), nsmall = 2),
               AchPts_Wgtd.SS.py = format(round(AchPts_Wgtd.SS.py*100, digits = 2), nsmall = 2),
               AchPts_Wgtd.SS.diff = format(round(AchPts_Wgtd.SS.diff*100, digits = 2), nsmall = 2))] %>%
  mutate(AchPts_Wgtd.SS.diff =
           cell_spec(AchPts_Wgtd.SS.diff, format = "latex",
                     italic = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                     TRUE, FALSE),
                     color = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                    "darkred", "black"),
                     escape = FALSE),
         AchPts_Wgtd.SS.cy =
           cell_spec(AchPts_Wgtd.SS.cy, format = "latex",
                     bold = TRUE,
                     color = ifelse(Rna(AchPts_Wgtd.SS.cy) < Rna(AchPts_Wgtd.SS.py),
                                    "darkred", "black"),
                     escape = FALSE)) %>%
  kable(caption = "HS \\textit{Weighted SS} Achievement Points",
        col.names = c("School", "$N_{students}$ (2018)",
                      "2018 \\textit{Science} Pts.", "2017 \\textit{Science} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))

#'
#' \newpage
#' ## High Schools' EOC Content Mastery Component _Composite Scores_
#'
#+ hs_cm_composite, results='asis'
# HS CONTENT MASTERY COMPOSITE SCORE --------------------------------------

ach.hsm <- lapply(list(ach.ela.hsm, ach.math.hsm, ach.sci.hsm, ach.ss.hsm),
                  function(x) setkey(x, "school.id"))
names(ach.hsm) <- c("ELA", "MATH", "SCI", "SS")
ach.hsm <- lapply(ach.hsm, function(x)
  x[, .SD, .SDcols = c("school.id", "school.name",
                       grep("N_Students", names(x), value = TRUE),
                       grep("AchPts_Wgtd", names(x), value = TRUE))])

cm.hsm <- Reduce(function(x, y)
  merge(x, y, by = c("school.id", "school.name"), all = TRUE),
  ach.hsm)

cm.hsm[, cm.cy := 100*sum(.SD, na.rm = TRUE), by = "school.id",
       .SDcols = grep("AchPts_Wgtd\\.\\w+\\.cy", names(cm.hsm), value = TRUE)]
cm.hsm[, cm.py := 100*sum(.SD, na.rm = TRUE), by = "school.id",
       .SDcols = grep("AchPts_Wgtd\\.\\w+\\.py", names(cm.hsm), value = TRUE)]
cm.hsm[, cm.diff := cm.cy - cm.py, by = "school.id"]

#'
#+ cm_hs_output, echo=FALSE, results='asis'
setkey(cm.hsm, "school.name")
cm.hsm[, .(school.name,
           cm.cy = format(round(cm.cy, digits = 2), nsmall = 2),
           cm.py = format(round(cm.py, digits = 2), nsmall = 2),
           cm.diff = format(round(cm.diff, digits = 2), nsmall = 2))] %>%
  mutate(cm.diff = cell_spec(cm.diff, format = "latex",
                             italic = ifelse(Rna(cm.cy) < Rna(cm.py),
                                             TRUE, FALSE),
                             color = ifelse(Rna(cm.cy) < Rna(cm.py),
                                            "darkred", "black"),
                             escape = FALSE),
         cm.cy = cell_spec(cm.cy, format = "latex",
                           bold = TRUE,
                           color = ifelse(Rna(cm.cy) < Rna(cm.py),
                                          "darkred", "black"),
                           escape = FALSE)) %>%
  kable(caption = "HS \\textit{Composite} Content Mastery Points",
        col.names = c("School",
                      "\\textbf{2018 \\textit{Composite} Pts.}", "2017 \\textit{Composite} Pts.",
                      "YoY Change"), align = c("l", rep("r", 4)),
        longtable = TRUE, format = "latex", booktabs = TRUE, escape = FALSE) %>%
  kable_styling(latex_options = c("repeat_header"))
#'
#'
