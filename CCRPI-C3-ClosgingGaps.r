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

#'
#'
#+ cmbl_es

bl.ela.es <- datl.eog[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_ela"]
bl.math.es <- datl.eog[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_math"]
bl.sci.es <- datl.eog[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_sci"]
bl.ss.es <- datl.eog[testedgrade <= 5 & school.year == 2017 & subject == "achlevel_ss"]

# CMBL - ES - ELA ---------------------------------------------------------

## RUN ES STUDENT-LEVEL DATA THROUGH RSUBJCMBL() (DEFINED ABOVE) ##
cmbl.ela.es <- Rcmbl(bl.ela.es,
                     gradecluster = "ES",
                     gradevar = "testedgrade",
                     subject = "ELA",
                     subject_var = "subject",
                     subject_code = "achlevel_ela",
                     performance_code_var = "achlevel_rec",
                     lgrpvars = list(all = "all",
                                     ai = "ethnicity_rec_short",
                                     as = "ethnicity_rec_short",
                                     bl = "ethnicity_rec_short",
                                     hp = "ethnicity_rec_short",
                                     mr = "ethnicity_rec_short",
                                     wh = "ethnicity_rec_short",
                                     swd = "swdflag",
                                     ed = "ed",
                                     el = "el"),
                     lgrps = list(all = "all",
                                  ai = "ai",
                                  as = "as",
                                  bl = "bl",
                                  hp = "hp",
                                  mr = "mr",
                                  wh = "wh",
                                  swd = 1,
                                  ed = "Y",
                                  el = "Y"))


# cmbl.math.es <- Rcmbl(bl.math.es, gradecluster = "ES", gradevar = "testedgrade", subject = "MATH", subject_var = "subject", subject_code = "achlevel_math", performance_code_var = "achlevel_rec")
# cmbl.sci.es <- Rcmbl(bl.sci.es, gradecluster = "ES", gradevar = "testedgrade", subject = "SCI", subject_var = "subject", subject_code = "achlevel_sci", performance_code_var = "achlevel_rec")
# cmbl.ss.es <- Rcmbl(bl.ss.es, gradecluster = "ES", gradevar = "testedgrade", subject = "SS", subject_var = "subject", subject_code = "achlevel_ss", performance_code_var = "achlevel_rec")

## COMBINE ES CMBL.SUBJ DATA ##
