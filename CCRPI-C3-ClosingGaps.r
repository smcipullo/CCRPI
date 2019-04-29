#' ---
#' titla: "CCRPI 2018 Estimates using 2018 GMAS Data - Content Mastery Component"
#' date: "Last updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ----
#'
#' \newpage
#'
#+ setup, include=FALSE
# SETUP -------------------------------------------------------------------

source("CCRPI-C1-ContentMastery.R")
kable <- function(x, format = "pandoc", ...) {
  knitr::kable(x, format = "pandoc", ...)
}
lgrpvars <- list(all = "all",
                ai = "ethnicity_rec_short",
                as = "ethnicity_rec_short",
                bl = "ethnicity_rec_short",
                hp = "ethnicity_rec_short",
                mr = "ethnicity_rec_short",
                wh = "ethnicity_rec_short",
                swd = "swdflag",
                ed = "edflag",
                el = "ellflag")
lgrps <- list(all = "all",
             ai = "ai",
             as = "as",
             bl = "bl",
             hp = "hp",
             mr = "mr",
             wh = "wh",
             swd = 1,
             ed = "Y",
             el = "Y")
lbsg <- list(ball = "all",
             bai = "ai",
             bas = "as",
             bbl = "bl",
             bhp = "hp",
             bmr = "mr",
             bwh = "wh",
             bswd = 1,
             bed = "Y",
             bel = "Y")
## FOR GETTING SUBGROUP BASELINES IN 'CMBL.*SW' ##

#'
#+ metadems, include=FALSE
# D.METADEMS --------------------------------------------------------------

library(feather)
d.metadems <- read_feather(path = "data/metadems_backups/metadems_backup-20180626.feather")

library(Riley); library(data.table)
setDT(d.metadems)
setnames(d.metadems, c("schcode", "schoolyearnumberspring"), c("school.id", "school.year"))
setkeyv(d.metadems, c("school.id", "gtid", "school.year"))

d.metadems[, edflag := ifelse(freereducedmealcode %in% c("F", "R"), "Y", "N")]
with(d.metadems, {table(freereducedmealcode, edflag, school.year, deparse.level = 2) }) ## QC ##

d.metadems[, ellflag := ifelse(toupper(ellcode) == "Y", "Y", "N")]
with(d.metadems, {table(ellflag, ellcode, school.year, deparse.level = 2) }) ## QC ##

d.metadems <- d.metadems[, .SD, .SDcols = intersect(names(d.metadems), unlist(c(key(d.metadems), Runname(lgrpvars))))]


## ENSURE SCHOOL ARE STORED INTEGERS ##
datl.eog[, gtid := as.double(gtid)]
datl.eog[, school.id := as.double(school.id)]
d.metadems[, gtid := as.double(gtid)]
d.metadems[, school.id := as.double(school.id)]
d.metadems <- d.metadems[!is.na(gtid) & !is.na(school.id)]
setkeyv(datl.eog, c("school.id", "gtid", "school.year")) ## SET KEYS TO MERGE ON ##

## FIRST REDUCE D.METADEMS+EOG/D.METADEMS_EOC TO ONLY INCLUDE STUDENTS IN DATL.EOG/DAT.EOC (TO IMPROVE PERFORMANCE IN MERGE CALL ##


d.metadems_eog <- d.metadems[gtid %in% datl.eog$gtid] ## AND REMOVE DUPLICATES ##
d.metadems_eoc <- d.metadems[gtid %in% dat.eoc$gtid] ## AND REMOVE DUPLICATES ##

d.metadems_eog <- unique(d.metadems_eog) ## REMOVE DUPLICATED ROWS ##

datl.eogm <- merge(unique(datl.eog), d.metadems_eog, all.x = TRUE, all.y = FALSE)

datl.eogm <- unique(datl.eogm)
datl.eogm$all <- "all"

d.metadems_eoc <- unique(d.metadems_eoc) ## REMOVE DUPLICATED ROWS ##

dat.eocm <- merge(unique(dat.eoc), d.metadems_eoc, all.x = TRUE, all.y = FALSE)
dat.eocm <- unique(dat.eocm)

dat.eocm$all <- "all"
#'
#' \newpage
#' # Closing Gaps Baseline Data
#'
#+ cmbl_es

# CMBL - ES ---------------------------------------------------------

bl.ela.es <- datl.eogm[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_ela"]
bl.math.es <- datl.eogm[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_math"]
bl.sci.es <- datl.eogm[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_sci"]
bl.ss.es <- datl.eogm[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_soc"]

## RUN ES STUDENT-LEVEL DATA THROUGH RSUBJCMBL() (DEFINED ABOVE) ##
cmbl.ela.es <- Rcmbl(bl.ela.es,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.math.es <- Rcmbl(bl.math.es,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.sci.es <- Rcmbl(bl.sci.es,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.ss.es <- Rcmbl(bl.ss.es,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                    lbsg = lbsg,
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


# COMBINE ES CMBL.SUBJ DATA -------------------------------------------------

cmbl.es <- rbind(cmbl.ela.es, cmbl.math.es, cmbl.sci.es, cmbl.ss.es)

# SAVE BACKUPS OF CMBL RESULTS -------------------------------------------------
## THESE ONLY NEED TO BE BE RUN WHEN MAJOR CHANGES ARE MADE TO THE RESULTING DFs ##
# write.csv(cmbl.ela.es, "data/cmbl_backups/cmbl_ela_es.csv", row.names = FALSE)
# write.csv(cmbl.math.es, "data/cmbl_backups/cmbl_math_es.csv", row.names = FALSE)
# write.csv(cmbl.sci.es, "data/cmbl_backups/cmbl_sci_es.csv", row.names = FALSE)
# write.csv(cmbl.ss.es, "data/cmbl_backups/cmbl_ss_es.csv", row.names = FALSE)
# write.csv(cmbl.es, "data/cmbl_backups/cmbl_es.csv", row.names = FALSE)

#+ echo=FALSE, results='asis'
kable(Rmsmm(cmbl.es), caption = "Summary Statistics for Elementary Schools' Baseline Data",
       col.names = c("Mean", "SD", "Min", "Max", "$N_{unique values}$", "$N_{missing}$"))
#'
#' \newpage
#+ cmbl_ms

# CMBL - MS ---------------------------------------------------------

bl.ela.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2017 & subject == "achlevel_ela"]
bl.math.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2017 & subject == "achlevel_math"]
bl.sci.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2017 & subject == "achlevel_sci"]
bl.ss.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2017 & subject == "achlevel_soc"]

cmbl.ela.ms <- Rcmbl(bl.ela.ms,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.math.ms <- Rcmbl(bl.math.ms,
                      lgrpvars = lgrpvars,
                      lgrps = lgrps,
                      lbsg = lbsg,
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

cmbl.sci.ms <- Rcmbl(bl.sci.ms,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.ss.ms <- Rcmbl(bl.ss.ms,
                    lgrpvars = lgrpvars,
                    lgrps = lgrps,
                    lbsg = lbsg,
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

# COMBINE MS CMBL.SUBJ DATA -------------------------------------------------

cmbl.ms <- rbind(cmbl.ela.ms, cmbl.math.ms, cmbl.sci.ms, cmbl.ss.ms)


# SAVE BACKUPS OF CMBL RESULTS -------------------------------------------------
## THESE ONLY NEED TO BE BE RUN WHEN MAJOR CHANGES ARE MADE TO THE RESULTING DFs ##
# write.csv(cmbl.ela.ms, "data/cmbl_backups/cmbl_ela_ms.csv", row.names = FALSE)
# write.csv(cmbl.math.ms, "data/cmbl_backups/cmbl_math_ms.csv", row.names = FALSE)
# write.csv(cmbl.sci.ms, "data/cmbl_backups/cmbl_sci_ms.csv", row.names = FALSE)
# write.csv(cmbl.ss.ms, "data/cmbl_backups/cmbl_ss_ms.csv", row.names = FALSE)
# write.csv(cmbl.ms, "data/cmbl_backups/cmbl_ms.csv", row.names = FALSE)
#'
#+ echo=FALSE, results='asis'
kable(Rmsmm(cmbl.ms), caption = "Summary Statistics for Middle Schools' Baseline Data",
       col.names = c("Mean", "SD", "Min", "Max", "$N_{unique values}$", "$N_{missing}$"))
#'
#'
#+ cmbl_hs

# CMBL - HS ---------------------------------------------------------

bl.ela.hs <- dat.eocm[testedgrade >= 9 & school.year == 2017 & subject == "achlevel_ela"]
bl.math.hs <- dat.eocm[testedgrade >= 9 & school.year == 2017 & subject == "achlevel_math"]
bl.sci.hs <- dat.eocm[testedgrade >= 9 & school.year == 2017 & subject == "achlevel_sci"]
bl.ss.hs <- dat.eocm[testedgrade >= 9 & school.year == 2017 & subject == "achlevel_soc"]


cmbl.ela.hs <- Rcmbl(bl.ela.hs,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.math.hs <- Rcmbl(bl.math.hs,
                      lgrpvars = lgrpvars,
                      lgrps = lgrps,
                      lbsg = lbsg,
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

cmbl.sci.hs <- Rcmbl(bl.sci.hs,
                     lgrpvars = lgrpvars,
                     lgrps = lgrps,
                     lbsg = lbsg,
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

cmbl.ss.hs <- Rcmbl(bl.ss.hs,
                    lgrpvars = lgrpvars,
                    lgrps = lgrps,
                    lbsg = lbsg,
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

# COMBINE HS CMBL.SUBJ DATA -------------------------------------------------


cmbl.hs <- rbind(cmbl.ela.hs, cmbl.math.hs, cmbl.sci.hs, cmbl.ss.hs)

# SAVE BACKUPS OF CMBL RESULTS -------------------------------------------------
## THESE ONLY NEED TO BE BE RUN WHEN MAJOR CHANGES ARE MADE TO THE RESULTING DFs ##
# write.csv(cmbl.ela.hs, "data/cmbl_backups/cmbl_ela_hs.csv", row.names = FALSE)
# write.csv(cmbl.math.hs, "data/cmbl_backups/cmbl_math_hs.csv", row.names = FALSE)
# write.csv(cmbl.sci.hs, "data/cmbl_backups/cmbl_sci_hs.csv", row.names = FALSE)
# write.csv(cmbl.ss.hs, "data/cmbl_backups/cmbl_ss_hs.csv", row.names = FALSE)
# write.csv(cmbl.hs, "data/cmbl_backups/cmbl_hs.csv", row.names = FALSE)
#'
#+ echo=FALSE, results='asis'
kable(Rmsmm(cmbl.hs), caption = "Summary Statistics for High Schools' Baseline Data",
       col.names = c("Mean", "SD", "Min", "Max", "$N_{unique values}$", "$N_{missing}$"))
#'
#' \newpage
#'
#+ cgpts_sgt

# CGPTS - SGT -------------------------------------------------------------

vars.cmbl <- c("school.id", "reporting.category", "assessment.subject", "baseline")
cmbl.esw <- reshape(cmbl.es, v.names = c("baseline"),
                    idvar = c("school.id", "reporting.category"),
                    timevar = "assessment.subject", direction = "wide")
cmbl.msw <- reshape(cmbl.ms, v.names = c("baseline"),
                    idvar = c("school.id", "reporting.category"),
                    timevar = "assessment.subject", direction = "wide")
cmbl.hsw <- reshape(cmbl.hs, v.names = c("baseline"),
                    idvar = c("school.id", "reporting.category"),
                    timevar = "assessment.subject", direction = "wide")

## RUN EACH GRADEBAND'S SUBGROUPS' BASELINE DATA THROUGH 'Rcgpts_sgt()' ##

es.sgt <- Rcgpts_sgt(xw = cmbl.esw, group_labs = unlist(Runname(lgrps)))
ms.sgt <- Rcgpts_sgt(xw = cmbl.msw, group_labs = unlist(Runname(lgrps)))
hs.sgt <- Rcgpts_sgt(xw = cmbl.hsw, group_labs = unlist(Runname(lgrps)))


# RCGPTS - ES ---------------------------------------

ela.es <- as.data.frame(datl.eogm[testedgrade <= 5 & school.year == 2018 & subject == "achlevel_ela"])
math.es <- as.data.frame(datl.eogm[testedgrade <= 5 & school.year == 2018 & subject == "achlevel_math"])
sci.es <- as.data.frame(datl.eogm[testedgrade <= 5 & school.year == 2018 & subject == "achlevel_sci"])
ss.es <- as.data.frame(datl.eogm[testedgrade <= 5 & school.year == 2018 & subject == "achlevel_soc"])

### [XELA.ES - GMA ACHIEVEMENT SCORES BY SUBGROUP] ###
xela.es <- lapply(1:length(lgrps), function(x)
  Rach(ela.es,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xmath.es <- lapply(1:length(lgrps), function(x)
  Rach(math.es,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xsci.es <- lapply(1:length(lgrps), function(x)
  Rach(sci.es,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xss.es <- lapply(1:length(lgrps), function(x)
  Rach(ss.es,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

# RCGPTS - MS -------------------------------------------------------------

ela.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2018 & subject == "achlevel_ela"]
math.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2018 & subject == "achlevel_math"]
sci.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2018 & subject == "achlevel_sci"]
ss.ms <- datl.eogm[testedgrade >= 6 & testedgrade <= 8 & school.year == 2018 & subject == "achlevel_soc"]

xela.ms <- lapply(1:length(lgrps), function(x)
  Rach(ela.ms,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xmath.ms <- lapply(1:length(lgrps), function(x)
  Rach(math.ms,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)


xsci.ms <- lapply(1:length(lgrps), function(x)
  Rach(sci.ms,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xss.ms <- lapply(1:length(lgrps), function(x)
  Rach(ss.ms,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)


# RCGPTS - HS -------------------------------------------------------------

ela.hs <- dat.eocm[testedgrade >= 9 & school.year == 2018 & subject == "achlevel_ela"] %>% as.data.frame
math.hs <- dat.eocm[testedgrade >= 9 & school.year == 2018 & subject == "achlevel_math"] %>% as.data.frame
sci.hs <- dat.eocm[testedgrade >= 9 & school.year == 2018 & subject == "achlevel_sci"] %>% as.data.frame
ss.hs <- dat.eocm[testedgrade >= 9 & school.year == 2018 & subject == "achlevel_soc"] %>% as.data.frame

xela.hs <- lapply(1:length(lgrps), function(x)
  Rach(ela.hs,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xmath.hs <- lapply(1:length(lgrps), function(x)
  Rach(math.hs,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)


xsci.hs <- lapply(1:length(lgrps), function(x)
  Rach(sci.hs,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

xss.hs <- lapply(1:length(lgrps), function(x)
  Rach(ss.hs,
       group_var = lgrpvars[[x]],
       group = lgrps[[x]],
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
)

#'
#' \newpage
#' # Elementary Schools Closing Gaps Subject-Area Indicator Points
#'
#' -----
#'
#' ## ES ELA Closing Gaps Indicator Points
#'
#+ cgm_ela_es, results='asis'

### (XELA.ES.NEWX & XELA.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xela.es.newx <- lapply(xela.es, function(x) x[["new_x"]])
xela.es.cm0 <- lapply(xela.es, function(x) x[["cm.subj"]])

### (XELA.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xela.es.newx) <- names(lgrpvars)
names(xela.es.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xela.es.cm <- lapply(xela.es.cm0, function(x) {
    y <- x
    y[, c("AchPts.ELA", "AchPts_Cpd.ELA", "AchPts_Wgtd.ELA")] <- apply(y[, c("AchPts.ELA", "AchPts_Cpd.ELA", "AchPts_Wgtd.ELA")], 2, function(x) x*100)
    return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.ela.es01 <- lapply(c(1:10), function(x) {
    Rcgpts(xela.es.cm[[x]][, c("school.id", "N_Students.ELA", "AchPts_Cpd.ELA")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
           df.baseline = es.sgt$x.sgt.bl[[x]], df.target = es.sgt$x.sgt.tg[[x]], subject = "ELA")
})
cgm.ela.esx <- lapply(cgm.ela.es01, function(x) x$xx)
cgm.ela.es0 <- lapply(cgm.ela.es01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.ela.es <- Rcgpts_post(x = cgm.ela.es0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.ela.es <- cgm.ela.es$cnts.subj
names(cnts.ela.es) <- c("school.id", "denom.ela")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.ela.es <- cgm.ela.es$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.ela.es$CG_ELA <- apply(cgm.ela.es[, -1], 1, sum, na.rm = TRUE)

# xela.es.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.ela.es0[[x]][, c("school.id", "cat")], xela.es.cm[[x]])
# })
cgm.ela.esc0 <- lapply(cgm.ela.es0, as.data.table)
cgm.ela.esc <- rbindlist(cgm.ela.esc0)

setkey(cgm.ela.esc, "school.id")

kable(cgm.ela.es, caption = "Elementary Schools' ELA CG Points by Subgroup")
#'
#' \newpage
#' ## ES Mathematics Closing Gaps Indicator Points
#'
#+ cgm_math_es, results='asis'

### (xmath.ES.NEWX & xmath.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xmath.es.newx <- lapply(xmath.es, function(x) x[["new_x"]])
xmath.es.cm0 <- lapply(xmath.es, function(x) x[["cm.subj"]])

### (xmath.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xmath.es.newx) <- names(lgrpvars)
names(xmath.es.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xmath.es.cm <- lapply(xmath.es.cm0, function(x) {
  y <- x
  y[, c("AchPts.MATH", "AchPts_Cpd.MATH", "AchPts_Wgtd.MATH")] <- apply(y[, c("AchPts.MATH", "AchPts_Cpd.MATH", "AchPts_Wgtd.MATH")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.math.es01 <- lapply(c(1:10), function(x) {
  Rcgpts(xmath.es.cm[[x]][, c("school.id", "N_Students.MATH", "AchPts_Cpd.MATH")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = es.sgt$x.sgt.bl[[x]], df.target = es.sgt$x.sgt.tg[[x]], subject = "MATH")
})
cgm.math.esx <- lapply(cgm.math.es01, function(x) x$xx)
cgm.math.es0 <- lapply(cgm.math.es01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.math.es <- Rcgpts_post(x = cgm.math.es0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.math.es <- cgm.math.es$cnts.subj
names(cnts.math.es) <- c("school.id", "denom.math")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.math.es <- cgm.math.es$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.math.es$CG_MATH <- apply(cgm.math.es[, -1], 1, sum, na.rm = TRUE)

# xmath.es.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.math.es0[[x]][, c("school.id", "cat")], xmath.es.cm[[x]])
# })
cgm.math.esc0 <- lapply(cgm.math.es0, as.data.table)
cgm.math.esc <- rbindlist(cgm.math.esc0)

setkey(cgm.math.esc, "school.id")

kable(cgm.math.es, caption = "Elementary Schools' Mathematics CG Points by Subgroup")

#'
#' \newpage
#' ## ES Science Closing Gaps Indicator Points
#'
#+ cgm_sci_es, results='asis'

### (xsci.ES.NEWX & xsci.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xsci.es.newx <- lapply(xsci.es, function(x) x[["new_x"]])
xsci.es.cm0 <- lapply(xsci.es, function(x) x[["cm.subj"]])

### (xsci.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xsci.es.newx) <- names(lgrpvars)
names(xsci.es.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xsci.es.cm <- lapply(xsci.es.cm0, function(x) {
  y <- x
  y[, c("AchPts.SCI", "AchPts_Cpd.SCI", "AchPts_Wgtd.SCI")] <- apply(y[, c("AchPts.SCI", "AchPts_Cpd.SCI", "AchPts_Wgtd.SCI")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.sci.es01 <- lapply(c(1:10), function(x) {
  Rcgpts(xsci.es.cm[[x]][, c("school.id", "N_Students.SCI", "AchPts_Cpd.SCI")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = es.sgt$x.sgt.bl[[x]], df.target = es.sgt$x.sgt.tg[[x]], subject = "SCI")
})
cgm.sci.esx <- lapply(cgm.sci.es01, function(x) x$xx)
cgm.sci.es0 <- lapply(cgm.sci.es01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.sci.es <- Rcgpts_post(x = cgm.sci.es0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.sci.es <- cgm.sci.es$cnts.subj
names(cnts.sci.es) <- c("school.id", "denom.sci")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.sci.es <- cgm.sci.es$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.sci.es$CG_SCI <- apply(cgm.sci.es[, -1], 1, sum, na.rm = TRUE)

# xsci.es.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.sci.es0[[x]][, c("school.id", "cat")], xsci.es.cm[[x]])
# })
cgm.sci.esc0 <- lapply(cgm.sci.es0, as.data.table)
cgm.sci.esc <- rbindlist(cgm.sci.esc0)

setkey(cgm.sci.esc, "school.id")

kable(cgm.sci.es, caption = "Elementary Schools' Science CG Points by Subgroup")

#'
#' \newpage
#' ## ES Social Studies Closing Gaps Indicator Points
#'
#+ cgm_ss_es, results='asis'

### (xss.ES.NEWX & xss.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xss.es.newx <- lapply(xss.es, function(x) x[["new_x"]])
xss.es.cm0 <- lapply(xss.es, function(x) x[["cm.subj"]])

### (xss.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xss.es.newx) <- names(lgrpvars)
names(xss.es.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xss.es.cm <- lapply(xss.es.cm0, function(x) {
  y <- x
  y[, c("AchPts.SS", "AchPts_Cpd.SS", "AchPts_Wgtd.SS")] <- apply(y[, c("AchPts.SS", "AchPts_Cpd.SS", "AchPts_Wgtd.SS")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.ss.es01 <- lapply(c(1:10), function(x) {
  Rcgpts(xss.es.cm[[x]][, c("school.id", "N_Students.SS", "AchPts_Cpd.SS")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = es.sgt$x.sgt.bl[[x]], df.target = es.sgt$x.sgt.tg[[x]], subject = "SS")
})
cgm.ss.esx <- lapply(cgm.ss.es01, function(x) x$xx)
cgm.ss.es0 <- lapply(cgm.ss.es01, function(x) x$xpts)

## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.ss.es <- Rcgpts_post(x = cgm.ss.es0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.ss.es <- cgm.ss.es$cnts.subj
names(cnts.ss.es) <- c("school.id", "denom.ss")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.ss.es <- cgm.ss.es$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.ss.es$CG_SS <- apply(cgm.ss.es[, -1], 1, sum, na.rm = TRUE)

# xss.es.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.ss.es0[[x]][, c("school.id", "cat")], xss.es.cm[[x]])
# })
cgm.ss.esc0 <- lapply(cgm.ss.es0, as.data.table)
cgm.ss.esc <- rbindlist(cgm.ss.esc0)

setkey(cgm.ss.esc, "school.id")

kable(cgm.ss.es, caption = "Elementary Schools' Social Studies CG Points by Subgroup")

#'
#' ## Elementary Schools' Closing Gaps - Composite
#'
# CG - ES - COMPOSITE -----------------------------------------------------

## CG.ES TOTAL POINTS ===========================================

## [MERGE CGM.SUBJ.ES POINTS DATAFRAMES] ##
cgc.es0 <- Reduce(function(x, y)
  merge(x, y, by = "school.id", all = TRUE),
  list(cgm.ela.es[, c("school.id", "CG_ELA")],
       cgm.math.es[ , c("school.id", "CG_MATH")],
       cgm.sci.es[ , c("school.id", "CG_SCI")],
       cgm.ss.es[ , c("school.id", "CG_SS")]))
# cgc.es0 <- merge(cgc.es1, cgc.es2, all = TRUE) ## DEPRECATED ON 2018-06-02 (KEPT HERE TEMPORARILY FOR REFERENCE) ##

## [COMPUTE EACH SCHOOL'S TOTAL CG POINTS (FOR CG COMPOSITE SCORE NUMERATOR)] ##
cgc.es0$sum_pts <- apply(cgc.es0[, c("CG_ELA", "CG_MATH", "CG_SCI", "CG_SS")],
                         1, sum, na.rm = TRUE)

## CG.ES TOTAL FLAGS =========================================

## [MERGE CNTS.SUBJ.ES DATAFRAMES] ##
cnts.es0 <- Reduce(function(x, y)
  merge(x, y, by = "school.id", all = TRUE),
  list(cnts.ela.es,
       cnts.math.es,
       cnts.sci.es,
       cnts.ss.es))
# cnts.es0 <- merge(cnts.es1, cnts.es2, all = TRUE) ## DEPRECATED ON 2018-06-02 (KEPT HERE TEMPORARILY FOR REFERENCE) ##

## COMPUTE EACH SCHOOOL'S TOTAL FLAG COUNT (FOR COMPOSITE CG SCORE DENOMINATOR)] ##
cnts.es <- cnts.es0[!duplicated(cnts.es0), ]
cnts.es$n_flags <- apply(cnts.es[, c("denom.ela", "denom.math", "denom.sci", "denom.ss")],
                         1, sum, na.rm = TRUE)

## CG.ES COMPOSITE SCORE ======================================

## [MERGE N-FLAGS DF WITH TOTAL POINTS DF] ##
cgc.es01 <- merge(cnts.es, cgc.es0)

## [COMPUTE CG.ES COMPOSITE SCORE (BY SCHOOL)] ##
cgc.es01$CG.SCORE <- 100*(cgc.es01$sum_pts/cgc.es01$n_flags) ## COMPONENT SCORE ROUNDED TO 1 DECIMAL POINT, PER 2018 CALCULATION GUIDE [IMPLEMENTED ON 2018-06-02; DEPRECATED FOR QC PURPOSES ON 20180612] ##

### (MAKE NAMES SLIGHTLY PRETTIER) ###
cgc.es02 <- cgc.es01[, c("school.id", "CG_ELA", "denom.ela", "CG_MATH", "denom.math", "CG_SCI", "denom.sci", "CG_SS", "denom.ss", "sum_pts", "n_flags", "CG.SCORE")]
cgc.es <- plyr::rename(cgc.es02, c("n_flags" = "N.Flags.Total", "sum_pts" = "N.Points.Total"))
names(cgc.es) <- gsub("denom\\.", "N.Flags.", names(cgc.es))
names(cgc.es) <- gsub("ela", "ELA", names(cgc.es))
names(cgc.es) <- gsub("math", "Math", names(cgc.es))
names(cgc.es) <- gsub("sci", "Science", names(cgc.es), ignore.case = TRUE)
names(cgc.es) <- gsub("ss", "Soc.Studies", names(cgc.es), ignore.case = TRUE)

# CGC.ES (FINAL CG COMPONENT DATAFRAME) -------------------------------------------------------
## NOTE: CGC.ES IS SPECIFICALLY SETUP TO ALIGN WITH OTHER ES COMPONENTS' FINAL DFs ... ##
### (... FOR MERGING WITH THE OTHER COMPONENTS' DFs TO COMPUTING ES' CCRPI SINGLE SCORES) ###
cgc.es <- merge(fcs_schools, cgc.es[!duplicated(cgc.es), ])

## DEPRECATED ROUND FOR QC PURPOSES ON 20180612 ##
## ROUND COMPONENT SCORE TO 1 DECIMAL (PER 2018 CALCULATION GUIDE) ##
# cgc.es$CG.SCORE <- round(cgc.es$CG.SCORE, 2) ## [IMPLEMENTED ON 2018-06-02] ##
kable(cgc.es[, c("school.name", "N.Points.Total", "N.Flags.Total", "CG.SCORE")])

## CGM.ES (VALIDATION/QC FOR CG COMPONENT) ==========================================

lcgm.es <- list(ELA = cgm.ela.es, MATH = cgm.math.es, SCI = cgm.sci.es, SS = cgm.ss.es)

## RESHAPE CGM.SUBJ.ES DFs TO LONG FORMAT (SETTING UP TO BE MERGE THESE) ##
lcgm.long.es <- lapply(names(lcgm.es),
                       function(x) {
                         y <- lcgm.es[[x]]
                         y.long <- reshape(y,
                                           idvar = "school.id",
                                           varying = setdiff(names(y),
                                                             c("school.id", paste0("CG_", x))),
                                           times = setdiff(names(y),
                                                           c("school.id", paste0("CG_", x))),
                                           direction = "long",
                                           timevar = "subgroup")
                         names(y.long) <- c("school.id",
                                            paste0("cgpts.total.", tolower(x)),
                                            "subgroup",
                                            paste0("cgpts.subgroup.", tolower(x)))
                         return(y.long)
                       })
names(lcgm.long.es) <- names(lcgm.es)

## MERGE RESHAPED CGM.SUBJ.ES DFs ##
cgm.es <- Reduce(function(x, y)
  merge(x, y, all = TRUE, by = c("school.id", "subgroup")),
  lcgm.long.es)

setDT(cgm.es, key = c("school.id", "subgroup")) ## RECLASS CGM.ES AS data.table (FOR CONVENIENCE ON NEXT LINE) ##

cols.totcgpts <- c("cgpts.total.ela", "cgpts.total.math", "cgpts.total.sci", "cgpts.total.ss")
cgm.es[, CGPTS.TOTAL := sum(.SD), by = "school.id", .SDcols = cols.totcgpts]

# WRITE CGC.ES TABLES ---------------------------------------------------------------------

xlsx::write.xlsx(cgc.es, "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "ClosingGaps_ES", row.names = FALSE)
xlsx::write.xlsx(cgm.ela.es, "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CG_Subgroups_ELA_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.math.es, "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CG_Subgroups_MATH_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.sci.es, "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CG_Subgroups_SCIENCE_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.ss.es, "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CG_Subgroups_SS_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xela.es.cm[[2]][, -ncol(xela.es.cm[[2]])], "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CGPoints_ELA_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xmath.es.cm[[2]][, -ncol(xmath.es.cm[[2]])], "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CGPoints_MATH_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xsci.es.cm[[2]][, -ncol(xsci.es.cm[[2]])], "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CGPoints_SCI_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xss.es.cm[[2]][, -ncol(xss.es.cm[[2]])], "reports/CCRPI-ClosingGaps-ES.xlsx", sheetName = "CGPoints_SS_ES", row.names = FALSE, append = TRUE)

#' \newpage
#' # Middle Schools' Closing Gaps Subject-Area Indicator Points
#'
#' -----
#'
#' ## MS ELA Closing Gaps Indicator Points
#'
#+ cgm_ela_ms, results='asis'


### (XELA.ES.NEWX & XELA.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xela.ms.newx <- lapply(xela.ms, function(x) x[["new_x"]])
xela.ms.cm0 <- lapply(xela.ms, function(x) x[["cm.subj"]])

### (XELA.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xela.ms.newx) <- names(lgrpvars)
names(xela.ms.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xela.ms.cm <- lapply(xela.ms.cm0, function(x) {
  y <- x
  y[, c("AchPts.ELA", "AchPts_Cpd.ELA", "AchPts_Wgtd.ELA")] <- apply(y[, c("AchPts.ELA", "AchPts_Cpd.ELA", "AchPts_Wgtd.ELA")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.ela.ms01 <- lapply(c(1:10), function(x) {
  Rcgpts(xela.ms.cm[[x]][, c("school.id", "N_Students.ELA", "AchPts_Cpd.ELA")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = ms.sgt$x.sgt.bl[[x]], df.target = ms.sgt$x.sgt.tg[[x]], subject = "ELA")
})
cgm.ela.msx <- lapply(cgm.ela.ms01, function(x) x$xx)
cgm.ela.ms0 <- lapply(cgm.ela.ms01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.ela.ms <- Rcgpts_post(x = cgm.ela.ms0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.ela.ms <- cgm.ela.ms$cnts.subj
names(cnts.ela.ms) <- c("school.id", "denom.ela")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.ela.ms <- cgm.ela.ms$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.ela.ms$CG_ELA <- apply(cgm.ela.ms[, -1], 1, sum, na.rm = TRUE)

# xela.ms.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.ela.ms0[[x]][, c("school.id", "cat")], xela.ms.cm[[x]])
# })
cgm.ela.msc0 <- lapply(cgm.ela.ms0, as.data.table)
cgm.ela.msc <- rbindlist(cgm.ela.msc0)

setkey(cgm.ela.msc, "school.id")

kable(cgm.ela.ms, caption = "Middle Schools' ELA CG Points by Subgroup")


#'
#' ## MS Mathematics Closing Gaps Indicator Points
#'
#+ cgm_math_ms, results='asis'
### (xmath.ES.NEWX & xmath.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xmath.ms.newx <- lapply(xmath.ms, function(x) x[["new_x"]])
xmath.ms.cm0 <- lapply(xmath.ms, function(x) x[["cm.subj"]])

### (xmath.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xmath.ms.newx) <- names(lgrpvars)
names(xmath.ms.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xmath.ms.cm <- lapply(xmath.ms.cm0, function(x) {
  y <- x
  y[, c("AchPts.MATH", "AchPts_Cpd.MATH", "AchPts_Wgtd.MATH")] <- apply(y[, c("AchPts.MATH", "AchPts_Cpd.MATH", "AchPts_Wgtd.MATH")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.math.ms01 <- lapply(c(1:10), function(x) {
  Rcgpts(xmath.ms.cm[[x]][, c("school.id", "N_Students.MATH", "AchPts_Cpd.MATH")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = ms.sgt$x.sgt.bl[[x]], df.target = ms.sgt$x.sgt.tg[[x]], subject = "MATH")
})
cgm.math.msx <- lapply(cgm.math.ms01, function(x) x$xx)
cgm.math.ms0 <- lapply(cgm.math.ms01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.math.ms <- Rcgpts_post(x = cgm.math.ms0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.math.ms <- cgm.math.ms$cnts.subj
names(cnts.math.ms) <- c("school.id", "denom.math")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.math.ms <- cgm.math.ms$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.math.ms$CG_MATH <- apply(cgm.math.ms[, -1], 1, sum, na.rm = TRUE)

# xmath.ms.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.math.ms0[[x]][, c("school.id", "cat")], xmath.ms.cm[[x]])
# })
cgm.math.msc0 <- lapply(cgm.math.ms0, as.data.table)
cgm.math.msc <- rbindlist(cgm.math.msc0)

setkey(cgm.math.msc, "school.id")

kable(cgm.math.ms, caption = "Middle Schools' Mathematics CG Points by Subgroup")

#'
#' \newpage
#' ## MS Science Closing Gaps Indicator Points
#'
#+ cgm_sci_ms, results='asis'

### (xsci.ES.NEWX & xsci.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xsci.ms.newx <- lapply(xsci.ms, function(x) x[["new_x"]])
xsci.ms.cm0 <- lapply(xsci.ms, function(x) x[["cm.subj"]])

### (xsci.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xsci.ms.newx) <- names(lgrpvars)
names(xsci.ms.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xsci.ms.cm <- lapply(xsci.ms.cm0, function(x) {
  y <- x
  y[, c("AchPts.SCI", "AchPts_Cpd.SCI", "AchPts_Wgtd.SCI")] <- apply(y[, c("AchPts.SCI", "AchPts_Cpd.SCI", "AchPts_Wgtd.SCI")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.sci.ms01 <- lapply(c(1:10), function(x) {
  Rcgpts(xsci.ms.cm[[x]][, c("school.id", "N_Students.SCI", "AchPts_Cpd.SCI")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = ms.sgt$x.sgt.bl[[x]], df.target = ms.sgt$x.sgt.tg[[x]], subject = "SCI")
})
cgm.sci.msx <- lapply(cgm.sci.ms01, function(x) x$xx)
cgm.sci.ms0 <- lapply(cgm.sci.ms01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.sci.ms <- Rcgpts_post(x = cgm.sci.ms0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.sci.ms <- cgm.sci.ms$cnts.subj
names(cnts.sci.ms) <- c("school.id", "denom.sci")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.sci.ms <- cgm.sci.ms$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.sci.ms$CG_SCI <- apply(cgm.sci.ms[, -1], 1, sum, na.rm = TRUE)

# xsci.ms.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.sci.ms0[[x]][, c("school.id", "cat")], xsci.ms.cm[[x]])
# })
cgm.sci.msc0 <- lapply(cgm.sci.ms0, as.data.table)
cgm.sci.msc <- rbindlist(cgm.sci.msc0)

setkey(cgm.sci.msc, "school.id")

kable(cgm.sci.ms, caption = "Middle Schools' Science CG Points by Subgroup")

#'
#' ## MS Social Studies Closing Gaps Indicator Points
#'
#+ cgm_ss_ms, results='asis'


### (xss.ES.NEWX & xss.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xss.ms.newx <- lapply(xss.ms, function(x) x[["new_x"]])
xss.ms.cm0 <- lapply(xss.ms, function(x) x[["cm.subj"]])

### (xss.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xss.ms.newx) <- names(lgrpvars)
names(xss.ms.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xss.ms.cm <- lapply(xss.ms.cm0, function(x) {
  y <- x
  y[, c("AchPts.SS", "AchPts_Cpd.SS", "AchPts_Wgtd.SS")] <- apply(y[, c("AchPts.SS", "AchPts_Cpd.SS", "AchPts_Wgtd.SS")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.ss.ms01 <- lapply(c(1:10), function(x) {
  Rcgpts(xss.ms.cm[[x]][, c("school.id", "N_Students.SS", "AchPts_Cpd.SS")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = ms.sgt$x.sgt.bl[[x]], df.target = ms.sgt$x.sgt.tg[[x]], subject = "SS")
})
cgm.ss.msx <- lapply(cgm.ss.ms01, function(x) x$xx)
cgm.ss.ms0 <- lapply(cgm.ss.ms01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.ss.ms <- Rcgpts_post(x = cgm.ss.ms0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.ss.ms <- cgm.ss.ms$cnts.subj
names(cnts.ss.ms) <- c("school.id", "denom.ss")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.ss.ms <- cgm.ss.ms$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.ss.ms$CG_SS <- apply(cgm.ss.ms[, -1], 1, sum, na.rm = TRUE)

# xss.ms.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.ss.ms0[[x]][, c("school.id", "cat")], xss.ms.cm[[x]])
# })
cgm.ss.msc0 <- lapply(cgm.ss.ms0, as.data.table)
cgm.ss.msc <- rbindlist(cgm.ss.msc0)

setkey(cgm.ss.msc, "school.id")

kable(cgm.ss.ms, caption = "Middle Schools' Social Studies CG Points by Subgroup")


#'
#' \newpage
#' ## Middle Schools' Closing Gaps - Composite
#'
# CG - MS - COMPOSITE -----------------------------------------------------

## CG.MS TOTAL POINTS ===========================================

## [MERGE CGM.SUBJ.MS POINTS DATAFRAMES] ##
cgc.ms0 <- Reduce(function(x, y)
  merge(x, y, by = "school.id", all = TRUE),
  list(cgm.ela.ms[, c("school.id", "CG_ELA")],
       cgm.math.ms[ , c("school.id", "CG_MATH")],
       cgm.sci.ms[ , c("school.id", "CG_SCI")],
       cgm.ss.ms[ , c("school.id", "CG_SS")]))
# cgc.ms0 <- merge(cgc.ms1, cgc.ms2, all = TRUE) ## DEPRECATED ON 2018-06-02 (KEPT HERE TEMPORARILY FOR REFERENCE) ##

## [COMPUTE EACH SCHOOL'S TOTAL CG POINTS (FOR CG COMPOSITE SCORE NUMERATOR)] ##
cgc.ms0$sum_pts <- apply(cgc.ms0[, c("CG_ELA", "CG_MATH", "CG_SCI", "CG_SS")],
                         1, sum, na.rm = TRUE)

## CG.MS TOTAL FLAGS =========================================

## [MERGE CNTS.SUBJ.MS DATAFRAMES] ##
cnts.ms0 <- Reduce(function(x, y)
  merge(x, y, by = "school.id", all = TRUE),
  list(cnts.ela.ms,
       cnts.math.ms,
       cnts.sci.ms,
       cnts.ss.ms))
# cnts.ms0 <- merge(cnts.ms1, cnts.ms2, all = TRUE) ## DEPRECATED ON 2018-06-02 (KEPT HERE TEMPORARILY FOR REFERENCE) ##

## COMPUTE EACH SCHOOOL'S TOTAL FLAG COUNT (FOR COMPOSITE CG SCORE DENOMINATOR)] ##
cnts.ms <- cnts.ms0[!duplicated(cnts.ms0), ]
cnts.ms$n_flags <- apply(cnts.ms[, c("denom.ela", "denom.math", "denom.sci", "denom.ss")],
                         1, sum, na.rm = TRUE)

## CG.MS COMPOSITE SCORE ======================================

## [MERGE N-FLAGS DF WITH TOTAL POINTS DF] ##
cgc.ms01 <- merge(cnts.ms, cgc.ms0)

## [COMPUTE CG.MS COMPOSITE SCORE (BY SCHOOL)] ##
cgc.ms01$CG.SCORE <- 100*(cgc.ms01$sum_pts/cgc.ms01$n_flags) ## COMPONENT SCORE ROUNDED TO 1 DECIMAL POINT, PER 2018 CALCULATION GUIDE [IMPLEMENTED ON 2018-06-02; DEPRECATED FOR QC PURPOSES ON 20180612] ##

### (MAKE NAMES SLIGHTLY PRETTIER) ###
cgc.ms02 <- cgc.ms01[, c("school.id", "CG_ELA", "denom.ela", "CG_MATH", "denom.math", "CG_SCI", "denom.sci", "CG_SS", "denom.ss", "sum_pts", "n_flags", "CG.SCORE")]
cgc.ms <- plyr::rename(cgc.ms02, c("n_flags" = "N.Flags.Total", "sum_pts" = "N.Points.Total"))
names(cgc.ms) <- gsub("denom\\.", "N.Flags.", names(cgc.ms))
names(cgc.ms) <- gsub("ela", "ELA", names(cgc.ms))
names(cgc.ms) <- gsub("math", "Math", names(cgc.ms))
names(cgc.ms) <- gsub("sci", "Science", names(cgc.ms), ignore.case = TRUE)
names(cgc.ms) <- gsub("ss", "Soc.Studies", names(cgc.ms), ignore.case = TRUE)

# CGC.MS (FINAL CG COMPONENT DATAFRAME) -------------------------------------------------------
## NOTE: CGC.MS IS SPECIFICALLY SETUP TO ALIGN WITH OTHER MS COMPONENTS' FINAL DFs ... ##
### (... FOR MERGING WITH THE OTHER COMPONENTS' DFs TO COMPUTING MS' CCRPI SINGLE SCORES) ###
cgc.ms <- merge(fcs_schools, cgc.ms[!duplicated(cgc.ms), ])

## DEPRECATED ROUND FOR QC PURPOSES ON 20180612 ##
## ROUND COMPONENT SCORE TO 1 DECIMAL (PER 2018 CALCULATION GUIDE) ##
# cgc.ms$CG.SCORE <- round(cgc.ms$CG.SCORE, 2) ## [IMPLEMENTED ON 2018-06-02] ##
kable(cgc.ms[, c("school.name", "N.Points.Total", "N.Flags.Total", "CG.SCORE")])

## CGM.MS (VALIDATION/QC FOR CG COMPONENT) ==========================================

lcgm.ms <- list(ELA = cgm.ela.ms, MATH = cgm.math.ms, SCI = cgm.sci.ms, SS = cgm.ss.ms)

## RESHAPE CGM.SUBJ.MS DFs TO LONG FORMAT (SETTING UP TO BE MERGE THESE) ##
lcgm.long.ms <- lapply(names(lcgm.ms),
                       function(x) {
                         y <- lcgm.ms[[x]]
                         y.long <- reshape(y,
                                           idvar = "school.id",
                                           varying = setdiff(names(y),
                                                             c("school.id", paste0("CG_", x))),
                                           times = setdiff(names(y),
                                                           c("school.id", paste0("CG_", x))),
                                           direction = "long",
                                           timevar = "subgroup")
                         names(y.long) <- c("school.id",
                                            paste0("cgpts.total.", tolower(x)),
                                            "subgroup",
                                            paste0("cgpts.subgroup.", tolower(x)))
                         return(y.long)
                       })
names(lcgm.long.ms) <- names(lcgm.ms)

## MERGE RESHAPED CGM.SUBJ.MS DFs ##
cgm.ms <- Reduce(function(x, y)
  merge(x, y, all = TRUE, by = c("school.id", "subgroup")),
  lcgm.long.ms)

setDT(cgm.ms, key = c("school.id", "subgroup")) ## RECLASS CGM.MS AS data.table (FOR CONVENIENCE ON NEXT LINE) ##

cols.totcgpts <- c("cgpts.total.ela", "cgpts.total.math", "cgpts.total.sci", "cgpts.total.ss")
cgm.ms[, CGPTS.TOTAL := sum(.SD), by = "school.id", .SDcols = cols.totcgpts]

# WRITE CGC.MS TABLES ---------------------------------------------------------------------

xlsx::write.xlsx(cgc.ms, "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "ClosingGaps_ES", row.names = FALSE)
xlsx::write.xlsx(cgm.ela.ms, "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CG_Subgroups_ELA_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.math.ms, "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CG_Subgroups_MATH_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.sci.ms, "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CG_Subgroups_SCIENCE_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.ss.ms, "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CG_Subgroups_SS_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xela.ms.cm[[2]][, -ncol(xela.ms.cm[[2]])], "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CGPoints_ELA_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xmath.ms.cm[[2]][, -ncol(xmath.ms.cm[[2]])], "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CGPoints_MATH_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xsci.ms.cm[[2]][, -ncol(xsci.ms.cm[[2]])], "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CGPoints_SCI_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xss.ms.cm[[2]][, -ncol(xss.ms.cm[[2]])], "reports/CCRPI-ClosingGaps-MS.xlsx", sheetName = "CGPoints_SS_ES", row.names = FALSE, append = TRUE)

#' \newpage
#' # High Schools' Closing Gaps Subject-Area Indicator Points
#'
#' -----
#'
#' ## HS ELA Closing Gaps Indicator Points
#'
#+ cgm_ela_ms, results='asis'


### (XELA.ES.NEWX & XELA.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xela.hs.newx <- lapply(xela.hs, function(x) x[["new_x"]])
xela.hs.cm0 <- lapply(xela.hs, function(x) x[["cm.subj"]])

### (XELA.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xela.hs.newx) <- names(lgrpvars)
names(xela.hs.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xela.hs.cm <- lapply(xela.hs.cm0, function(x) {
  y <- x
  y[, c("AchPts.ELA", "AchPts_Cpd.ELA", "AchPts_Wgtd.ELA")] <- apply(y[, c("AchPts.ELA", "AchPts_Cpd.ELA", "AchPts_Wgtd.ELA")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.ela.hs01 <- lapply(c(1:10), function(x) {
  Rcgpts(xela.hs.cm[[x]][, c("school.id", "N_Students.ELA", "AchPts_Cpd.ELA")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = hs.sgt$x.sgt.bl[[x]], df.target = hs.sgt$x.sgt.tg[[x]], subject = "ELA")
})
cgm.ela.hsx <- lapply(cgm.ela.hs01, function(x) x$xx)
cgm.ela.hs0 <- lapply(cgm.ela.hs01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.ela.hs <- Rcgpts_post(x = cgm.ela.hs0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.ela.hs <- cgm.ela.hs$cnts.subj
names(cnts.ela.hs) <- c("school.id", "denom.ela")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.ela.hs <- cgm.ela.hs$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.ela.hs$CG_ELA <- apply(cgm.ela.hs[, -1], 1, sum, na.rm = TRUE)

# xela.hs.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.ela.hs0[[x]][, c("school.id", "cat")], xela.hs.cm[[x]])
# })
cgm.ela.hsc0 <- lapply(cgm.ela.hs0, as.data.table)
cgm.ela.hsc <- rbindlist(cgm.ela.hsc0)

setkey(cgm.ela.hsc, "school.id")

kable(cgm.ela.hs, caption = "High Schools' ELA CG Points by Subgroup")


#'
#' ## HS Mathematics Closing Gaps Indicator Points
#'
#+ cgm_math_ms, results='asis'
### (xmath.ES.NEWX & xmath.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xmath.hs.newx <- lapply(xmath.hs, function(x) x[["new_x"]])
xmath.hs.cm0 <- lapply(xmath.hs, function(x) x[["cm.subj"]])

### (xmath.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xmath.hs.newx) <- names(lgrpvars)
names(xmath.hs.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xmath.hs.cm <- lapply(xmath.hs.cm0, function(x) {
  y <- x
  y[, c("AchPts.MATH", "AchPts_Cpd.MATH", "AchPts_Wgtd.MATH")] <- apply(y[, c("AchPts.MATH", "AchPts_Cpd.MATH", "AchPts_Wgtd.MATH")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.math.hs01 <- lapply(c(1:10), function(x) {
  Rcgpts(xmath.hs.cm[[x]][, c("school.id", "N_Students.MATH", "AchPts_Cpd.MATH")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = hs.sgt$x.sgt.bl[[x]], df.target = hs.sgt$x.sgt.tg[[x]], subject = "MATH")
})
cgm.math.hsx <- lapply(cgm.math.hs01, function(x) x$xx)
cgm.math.hs0 <- lapply(cgm.math.hs01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.math.hs <- Rcgpts_post(x = cgm.math.hs0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.math.hs <- cgm.math.hs$cnts.subj
names(cnts.math.hs) <- c("school.id", "denom.math")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.math.hs <- cgm.math.hs$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.math.hs$CG_MATH <- apply(cgm.math.hs[, -1], 1, sum, na.rm = TRUE)

# xmath.hs.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.math.hs0[[x]][, c("school.id", "cat")], xmath.hs.cm[[x]])
# })
cgm.math.hsc0 <- lapply(cgm.math.hs0, as.data.table)
cgm.math.hsc <- rbindlist(cgm.math.hsc0)

setkey(cgm.math.hsc, "school.id")

kable(cgm.math.hs, caption = "High Schools' Mathematics CG Points by Subgroup")

#'
#' \newpage
#' ## HS Science Closing Gaps Indicator Points
#'
#+ cgm_sci_ms, results='asis'

### (xsci.ES.NEWX & xsci.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xsci.hs.newx <- lapply(xsci.hs, function(x) x[["new_x"]])
xsci.hs.cm0 <- lapply(xsci.hs, function(x) x[["cm.subj"]])

### (xsci.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xsci.hs.newx) <- names(lgrpvars)
names(xsci.hs.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xsci.hs.cm <- lapply(xsci.hs.cm0, function(x) {
  y <- x
  y[, c("AchPts.SCI", "AchPts_Cpd.SCI", "AchPts_Wgtd.SCI")] <- apply(y[, c("AchPts.SCI", "AchPts_Cpd.SCI", "AchPts_Wgtd.SCI")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.sci.hs01 <- lapply(c(1:10), function(x) {
  Rcgpts(xsci.hs.cm[[x]][, c("school.id", "N_Students.SCI", "AchPts_Cpd.SCI")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = hs.sgt$x.sgt.bl[[x]], df.target = hs.sgt$x.sgt.tg[[x]], subject = "SCI")
})
cgm.sci.hsx <- lapply(cgm.sci.hs01, function(x) x$xx)
cgm.sci.hs0 <- lapply(cgm.sci.hs01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.sci.hs <- Rcgpts_post(x = cgm.sci.hs0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.sci.hs <- cgm.sci.hs$cnts.subj
names(cnts.sci.hs) <- c("school.id", "denom.sci")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.sci.hs <- cgm.sci.hs$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.sci.hs$CG_SCI <- apply(cgm.sci.hs[, -1], 1, sum, na.rm = TRUE)

# xsci.hs.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.sci.hs0[[x]][, c("school.id", "cat")], xsci.hs.cm[[x]])
# })
cgm.sci.hsc0 <- lapply(cgm.sci.hs0, as.data.table)
cgm.sci.hsc <- rbindlist(cgm.sci.hsc0)

setkey(cgm.sci.hsc, "school.id")

kable(cgm.sci.hs, caption = "High Schools' Science CG Points by Subgroup")

#'
#' ## HS Social Studies Closing Gaps Indicator Points
#'
#+ cgm_ss_ms, results='asis'


### (xss.ES.NEWX & xss.ES.CM* - SEPARATE Rach() OUTPUTS) ###
xss.hs.newx <- lapply(xss.hs, function(x) x[["new_x"]])
xss.hs.cm0 <- lapply(xss.hs, function(x) x[["cm.subj"]])

### (xss.ES.*'s ELEMENT NAMES <==> SUBGROUPS' LABELS UNDER CM16$REPORTING.CATEGORY) ###
names(xss.hs.newx) <- names(lgrpvars)
names(xss.hs.cm0) <- names(lgrpvars) ## LIST ELEMENT NAMES CORRESPOND TO SUBGROUPS ##

### (CONVERT CM ACH SCORES FROM DECIMAL TO PERCENTAGE) ###
xss.hs.cm <- lapply(xss.hs.cm0, function(x) {
  y <- x
  y[, c("AchPts.SS", "AchPts_Cpd.SS", "AchPts_Wgtd.SS")] <- apply(y[, c("AchPts.SS", "AchPts_Cpd.SS", "AchPts_Wgtd.SS")], 2, function(x) x*100)
  return(y)
})

## APPLY Rcgpts() TO EACH SUBGROUP FOR THE CURRENT SUBJ. AT THE CURRENT GRADEBAND ##
cgm.ss.hs01 <- lapply(c(1:10), function(x) {
  Rcgpts(xss.hs.cm[[x]][, c("school.id", "N_Students.SS", "AchPts_Cpd.SS")], ## USE *UNWEIGHTED* CONTENT MASTERY ACHIEVEMENT POINTS FOR THE CURRENT ASSESSMENT.SUBJECT ##
         df.baseline = hs.sgt$x.sgt.bl[[x]], df.target = hs.sgt$x.sgt.tg[[x]], subject = "SS")
})
cgm.ss.hsx <- lapply(cgm.ss.hs01, function(x) x$xx)
cgm.ss.hs0 <- lapply(cgm.ss.hs01, function(x) x$xpts)
## (TO BE COMBINED WITH OTHER SUBJECT-SPECIFIC CG DATAFRAMES FOR THE CURRENT GRADEBAND) ##

cgm.ss.hs <- Rcgpts_post(x = cgm.ss.hs0, lgrps = lgrps, lgrpvars = lgrpvars, lbsg = lbsg)

## [SUBSET CNTS.SUBJ DATA (FOR CONVENIENCE)] ##
cnts.ss.hs <- cgm.ss.hs$cnts.subj
names(cnts.ss.hs) <- c("school.id", "denom.ss")

## [SUBSET CGM.SUBJ DATA (FOR CONVENIENCE)] ##
cgm.ss.hs <- cgm.ss.hs$cgm.subj

## [COMPUTE TOTAL NUMBER OF CG POINTS EARNED ACROSS ALL SUBGROUPS] ##
cgm.ss.hs$CG_SS <- apply(cgm.ss.hs[, -1], 1, sum, na.rm = TRUE)

# xss.hs.cmc0 <- lapply(c(1:10), function(x) {
#     y <- merge(cgm.ss.hs0[[x]][, c("school.id", "cat")], xss.hs.cm[[x]])
# })
cgm.ss.hsc0 <- lapply(cgm.ss.hs0, as.data.table)
cgm.ss.hsc <- rbindlist(cgm.ss.hsc0)

setkey(cgm.ss.hsc, "school.id")

kable(cgm.ss.hs, caption = "High Schools' Social Studies CG Points by Subgroup")


#'
#' \newpage
#' ## High Schools' Closing Gaps - Composite
#'
# CG - HS - COMPOSITE -----------------------------------------------------

## CG.HS TOTAL POINTS ===========================================

## [MERGE CGM.SUBJ.HS POINTS DATAFRAMES] ##
cgc.hs0 <- Reduce(function(x, y)
  merge(x, y, by = "school.id", all = TRUE),
  list(cgm.ela.hs[, c("school.id", "CG_ELA")],
       cgm.math.hs[ , c("school.id", "CG_MATH")],
       cgm.sci.hs[ , c("school.id", "CG_SCI")],
       cgm.ss.hs[ , c("school.id", "CG_SS")]))
# cgc.hs0 <- merge(cgc.hs1, cgc.hs2, all = TRUE) ## DEPRECATED ON 2018-06-02 (KEPT HERE TEMPORARILY FOR REFERENCE) ##

## [COMPUTE EACH SCHOOL'S TOTAL CG POINTS (FOR CG COMPOSITE SCORE NUMERATOR)] ##
cgc.hs0$sum_pts <- apply(cgc.hs0[, c("CG_ELA", "CG_MATH", "CG_SCI", "CG_SS")],
                         1, sum, na.rm = TRUE)

## CG.HS TOTAL FLAGS =========================================

## [MERGE CNTS.SUBJ.HS DATAFRAMES] ##
cnts.hs0 <- Reduce(function(x, y)
  merge(x, y, by = "school.id", all = TRUE),
  list(cnts.ela.hs,
       cnts.math.hs,
       cnts.sci.hs,
       cnts.ss.hs))
# cnts.hs0 <- merge(cnts.hs1, cnts.hs2, all = TRUE) ## DEPRECATED ON 2018-06-02 (KEPT HERE TEMPORARILY FOR REFERENCE) ##

## COMPUTE EACH SCHOOOL'S TOTAL FLAG COUNT (FOR COMPOSITE CG SCORE DENOMINATOR)] ##
cnts.hs <- cnts.hs0[!duplicated(cnts.hs0), ]
cnts.hs$n_flags <- apply(cnts.hs[, c("denom.ela", "denom.math", "denom.sci", "denom.ss")],
                         1, sum, na.rm = TRUE)

## CG.HS COMPOSITE SCORE ======================================

## [MERGE N-FLAGS DF WITH TOTAL POINTS DF] ##
cgc.hs01 <- merge(cnts.hs, cgc.hs0)

## [COMPUTE CG.HS COMPOSITE SCORE (BY SCHOOL)] ##
cgc.hs01$CG.SCORE <- 100*(cgc.hs01$sum_pts/cgc.hs01$n_flags) ## COMPONENT SCORE ROUNDED TO 1 DECIMAL POINT, PER 2018 CALCULATION GUIDE [IMPLEMENTED ON 2018-06-02; DEPRECATED FOR QC PURPOSES ON 20180612] ##

### (MAKE NAMES SLIGHTLY PRETTIER) ###
cgc.hs02 <- cgc.hs01[, c("school.id", "CG_ELA", "denom.ela", "CG_MATH", "denom.math", "CG_SCI", "denom.sci", "CG_SS", "denom.ss", "sum_pts", "n_flags", "CG.SCORE")]
cgc.hs <- plyr::rename(cgc.hs02, c("n_flags" = "N.Flags.Total", "sum_pts" = "N.Points.Total"))
names(cgc.hs) <- gsub("denom\\.", "N.Flags.", names(cgc.hs))
names(cgc.hs) <- gsub("ela", "ELA", names(cgc.hs))
names(cgc.hs) <- gsub("math", "Math", names(cgc.hs))
names(cgc.hs) <- gsub("sci", "Science", names(cgc.hs), ignore.case = TRUE)
names(cgc.hs) <- gsub("ss", "Soc.Studies", names(cgc.hs), ignore.case = TRUE)

# CGC.HS (FINAL CG COMPONENT DATAFRAME) -------------------------------------------------------
## NOTE: CGC.HS IS SPECIFICALLY SETUP TO ALIGN WITH OTHER HS COMPONENTS' FINAL DFs ... ##
### (... FOR MERGING WITH THE OTHER COMPONENTS' DFs TO COMPUTING HS' CCRPI SINGLE SCORES) ###
cgc.hs <- merge(fcs_schools, cgc.hs[!duplicated(cgc.hs), ])

## DEPRECATED ROUND FOR QC PURPOSES ON 20180612 ##
## ROUND COMPONENT SCORE TO 1 DECIMAL (PER 2018 CALCULATION GUIDE) ##
# cgc.hs$CG.SCORE <- round(cgc.hs$CG.SCORE, 2) ## [IMPLEMENTED ON 2018-06-02] ##
kable(cgc.hs[, c("school.name", "N.Points.Total", "N.Flags.Total", "CG.SCORE")])

## CGM.HS (VALIDATION/QC FOR CG COMPONENT) ==========================================

lcgm.hs <- list(ELA = cgm.ela.hs, MATH = cgm.math.hs, SCI = cgm.sci.hs, SS = cgm.ss.hs)

## RESHAPE CGM.SUBJ.HS DFs TO LONG FORMAT (SETTING UP TO BE MERGE THESE) ##
lcgm.long.hs <- lapply(names(lcgm.hs),
                       function(x) {
                         y <- lcgm.hs[[x]]
                         y.long <- reshape(y,
                                           idvar = "school.id",
                                           varying = setdiff(names(y),
                                                             c("school.id", paste0("CG_", x))),
                                           times = setdiff(names(y),
                                                           c("school.id", paste0("CG_", x))),
                                           direction = "long",
                                           timevar = "subgroup")
                         names(y.long) <- c("school.id",
                                            paste0("cgpts.total.", tolower(x)),
                                            "subgroup",
                                            paste0("cgpts.subgroup.", tolower(x)))
                         return(y.long)
                       })
names(lcgm.long.hs) <- names(lcgm.hs)

## MERGE RESHAPED CGM.SUBJ.HS DFs ##
cgm.hs <- Reduce(function(x, y)
  merge(x, y, all = TRUE, by = c("school.id", "subgroup")),
  lcgm.long.hs)

setDT(cgm.hs, key = c("school.id", "subgroup")) ## RECLASS CGM.HS AS data.table (FOR CONVENIENCE ON NEXT LINE) ##

cols.totcgpts <- c("cgpts.total.ela", "cgpts.total.math", "cgpts.total.sci", "cgpts.total.ss")
cgm.hs[, CGPTS.TOTAL := sum(.SD), by = "school.id", .SDcols = cols.totcgpts]

# WRITE CGC.HS TABLES ---------------------------------------------------------------------

xlsx::write.xlsx(cgc.hs, "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "ClosingGaps_ES", row.names = FALSE)
xlsx::write.xlsx(cgm.ela.hs, "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CG_Subgroups_ELA_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.math.hs, "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CG_Subgroups_MATH_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.sci.hs, "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CG_Subgroups_SCIENCE_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(cgm.ss.hs, "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CG_Subgroups_SS_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xela.hs.cm[[2]][, -ncol(xela.hs.cm[[2]])], "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CGPoints_ELA_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xmath.hs.cm[[2]][, -ncol(xmath.hs.cm[[2]])], "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CGPoints_MATH_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xsci.hs.cm[[2]][, -ncol(xsci.hs.cm[[2]])], "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CGPoints_SCI_ES", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(xss.hs.cm[[2]][, -ncol(xss.hs.cm[[2]])], "reports/CCRPI-ClosingGaps-HS.xlsx", sheetName = "CGPoints_SS_ES", row.names = FALSE, append = TRUE)
