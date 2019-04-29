#' ---
#' title: "CCRPI 2018 Estimates using 2018 GMAS Data - Readiness Component"
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
lkable <- function(x, format = "latex", booktabs = TRUE, escape = FALSE, ...) {
  knitr:::kable(x, format = format, booktabs = booktabs, escape = escape, ...)
}

knitr::opts_chunk$set(echo = FALSE,
                      results = 'asis',
                      dev = c("pdf", "png", "svg"))

# library(ccrpi)

fcs_schools <- Rrdcsv("data/fcs_schools.csv", asDT = TRUE) ## CREATED USING GaDOE 2017 CCRPI COMPONENT SCORING DATAFILE ##
setkeyv(fcs_schools, c("school.id"))
fcs_schools <- as.data.frame(fcs_schools)
fcs_schools <- fcs_schools[fcs_schools$system.id == 660, c("school.id", "school.name"), drop = FALSE] %>% droplevels()

#'
#' \newpage
#'
#' # Readiness Component Indicator Scores
#'
#'
#' `r tufte::newthought("Note:")` The _Literacy Indicator_ for the _Readiness Component_ is calculated below using FCS students' **actual 2018** _GA Milestones Lexile Scores_. Importantly, however, students' **2018** _Lexile Scores_ are compared against the avaialble **2017** _Lexile Midpoints_ until GaDOE releases the current year's actual midpoints to be used in determining each student's actual contribution to each school's _Literacy Indicator_ points. All other _Readiness Component Indicators_, as well _High School Graduation Rates_ are calculated using the available **2017** data.
#'
#'
#' -----
#'
#' # Elementary Schools' Readiness Component Literacy Indicator Points
#'
#+

# DAT.EOG -----------------------------------------------------------------


dat.eog0 <- rbind(Rrdcsv("data/GMAS-FCS-EOG-SP18.csv"), Rrdcsv("data/GMAS-FCS-EOG-SU18.csv"))
names(dat.eog0) <- gsub("_rpt", "", names(dat.eog0))

dat.eog <- unique(dat.eog0[, c("gtid", "syscode", "sysname", "schcode", "schname", ## STU, SYSTEM, & SCHOOL IDs ##
           "testadmin", "testdate", "testedgrade", ## TEST ADMIN (TERM & YEAR) & TESTED GRADE LEVEL ##
           "lexile" ## STUDENT LEXILE SCALE SCORE ##
           )])

dat.eog$testadmin <- stringi::stri_trim_both(dat.eog$testadmin)
dat.eog$school.year <- as.integer(gsub("^Spring (20\\d{2})$", "\\1", dat.eog$testadmin),
                            perl = TRUE) ## USE REGEX TO GET CORRECT YEAR VALUE ##


# DAT.EOC -----------------------------------------------------------------

dat.eoc0 <- rbind(Rrdcsv("data/GMAS-FCS-EOC-SP18.csv"), Rrdcsv("data/GMAS-FCS-EOC-SU18.csv"))
names(dat.eoc0) <- gsub("_rpt", "", names(dat.eoc0))

dat.eoc <- unique(dat.eoc0[, c("gtid", "syscode", "sysname", "schcode", "schname", ## STU, SYSTEM, & SCHOOL IDs ##
                               "testadmin", "testdate", "stugrade", ## TEST ADMIN (TERM & YEAR) & TESTED GRADE LEVEL ##
                               "lexile" ## STUDENT LEXILE SCALE SCORE ##
)])

dat.eoc$testadmin <- stringi::stri_trim_both(dat.eoc$testadmin)
dat.eoc$school.year <- as.integer(gsub("^SPRING (20\\d{2})$", "\\1", dat.eoc$testadmin),
                                  perl = TRUE) ## USE REGEX TO GET CORRECT YEAR VALUE ##


# Rlxmid() -------------------------------------------------------------------


## GRADE-SPECIFIC PARAMS FOR COMPUTING LEXILE READINESS INDICATOR POINTS ##
lxmidpoints <- list(es = c(670, 840, 920),
                    ms = c(997, 1045, 1097),
                    hs = c(1155, 1285))
lxgrades <- list(es = c(3, 4, 5),
                 ms = c(6, 7, 8),
                 hs = c(9:12))
lxgradvars <- list(es = "testedgrade",
                   ms = "testedgrade",
                   hs = "stugrade")

Rlxmid <- function(x, grades, gradevar = "testedgrade",
                   midpoints) { ## NOTE: 'MIDPOINTS' & 'GRADES' SHOULD BE VECTORS OF THE SAME LENGTH ##
  ## RESTRICT TO SPECIFIED GRADES ##
  xnew <- x[x[, gradevar] %in% c(grades), , drop = FALSE] %>% droplevels()

  ## ADD COLUMN CONTAINING GRADE-SPECIFIC LEXILE MIDPOINTS (FOR REFERENCE) ##
  rec.midpoints <- paste0("'", grades, "'='", midpoints, "'", collapse = "; ")
  xnew$lexile.midpoint <- car::recode(xnew[, gradevar], rec.midpoints)

  ## IMPLEMENT BIZ RULES FOR LEXILE POINTS ##
  ## ASSIGN VALUE OF '1' TO STUDENTS WITH LEXILE SCORE GREATER THAN OR EQUAL TO SPECIFIED MIDPOINT FOR THEIR GRADEBAND,
  ## OTHERWISE, ASSIGN VALUE OF '0' ##
  xnew$lexile.readiness.point <- ifelse(xnew$lexile >= xnew$lexile.midpoint, 1, 0)

  ## RETURN NEW DF CONTAINING ONLY THE ESSENTIALS FOR LEXILE READINESS INDICATOR ##
  return(xnew[, c("schcode", "gtid", gradevar, "lexile", "lexile.midpoint", "lexile.readiness.point")])
}

# RD - ES - LIT -----------------------------------------------------------

# ela.es0 <- Rrdcsv("data/CM_ES_ELA_2017.csv", asDT = FALSE)

lxes0 <- Rlxmid(x = dat.eog0, grades = lxgrades$es, gradevar = "testedgrade",
                midpoints = lxmidpoints$es)

lxes.n0 <- table(lxes0$schcode, lxes0$lexile.readiness.point)
lxes.n1 <- apply(lxes.n0, 1, sum, na.rm = TRUE)
lxes.p <- data.frame(school.id = as.integer(names(lxes.n1)), pts.literacy = (lxes.n0[, "1"]/lxes.n1)*100)
lxes <- merge(fcs_schools, lxes.p)
lxes <- lxes[!duplicated(lxes), ] %>% droplevels()
lxes$pts.literacy <- round(lxes$pts.literacy, 2)
kable(unique(lxes[order(lxes$school.name), c("school.name", "pts.literacy")]), caption = "Literacy Readiness Points", col.names = c("School", "Literacy Pts."), row.names = FALSE)
#'
#' \newpage
#' ## Attendance, Beyond the Core, & Composite Readiness Scores
#'
#'
# RD - ES - ATT -----------------------------------------------------------------

rdy_esms0 <- Rrdcsv("data/RDY-ESMS-2018.csv")
rdy_esms <- merge(fcs_schools[, c("school.id", "school.name")],
                   rdy_esms0[, c("school.id", "attendance", "beyond.the.core")],
                  by = c("school.id"), all.x = FALSE, all.y = TRUE)
rdy_esms <- rdy_esms[, c("school.id", "school.name", "attendance", "beyond.the.core")]
names(rdy_esms) <- c("school.id", "school.name", "pts.att", "pts.btc")
rdy_esms[, c("pts.att", "pts.btc")] <- apply(rdy_esms[, c("pts.att", "pts.btc")], 2, function(x) x*100)

# RD - ES - COMPOSITE -----------------------------------------------------------

wgt.em <- 0.33
wgt.hs <- 0.20
lxes$school.id <- as.character(lxes$school.id)
es.c1 <- merge(lxes, rdy_esms, all.x = TRUE, all.y = FALSE)
es.c1 <- as.data.frame(es.c1)

vars.rdpts <- c("pts.literacy", "pts.att", "pts.btc")
es.c1 <- es.c1[, c("school.id", "school.name", vars.rdpts)]

es.c1[, vars.rdpts] <- apply(es.c1[, vars.rdpts], 2, function(x) ifelse(x > 100, 100, x))
es.c1[, vars.rdpts] <- apply(es.c1[, vars.rdpts], 2, function(x) x*wgt.em)
es.c1$sum_pts <- apply(es.c1[, vars.rdpts], 1, sum, na.rm = TRUE)
lkable(es.c1[order(es.c1$school.name), c("school.name", "pts.literacy", "pts.att", "pts.btc", "sum_pts")],
      caption = "Elementary Schools' Weighted Readiness Component Points \\& Composite Readiness Scores[note]",
      col.names = c("School",
                    "Literacy",
                    "Attendance",
                    "BTC",
                    "Readiness Score"),
      row.names = FALSE, format = 'latex', booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "repeat_header")) %>%
  add_footnote(c("ES Readiness Weights (all indicators) = 0.333"))

#'
#' \newpage
#'
#' # Middle Schools' Readiness
#'
#' -----
#'
#' ## Literacy
#'
# RD - MS - LIT -----------------------------------------------------------

lxms0 <- Rlxmid(x = dat.eog0, grades = lxgrades$ms, gradevar = "testedgrade",
                midpoints = lxmidpoints$es)

lxms.n0 <- table(lxms0$schcode, lxms0$lexile.readiness.point)
lxms.n1 <- apply(lxms.n0, 1, sum, na.rm = TRUE)
lxms.p <- data.frame(school.id = as.integer(names(lxms.n1)), pts.literacy = (lxms.n0[, "1"]/lxms.n1)*100)
lxms <- merge(fcs_schools, lxms.p)
lxms <- lxms[!duplicated(lxms), ] %>% droplevels()
lxms$pts.literacy <- round(lxms$pts.literacy, 2)
kable(lxms[order(lxms$school.name), c("school.name", "pts.literacy")],
      caption = "Literacy Readiness Points",
      col.names = c("School", "Literacy Pts."), row.names = FALSE)


#'
#' \newpage
#' ## Attendance, Beyond the Core, & Composite Readiness Scores
#'
#'

# RD - MS - COMPOSITE -----------------------------------------------------------

wgt.em <- 0.33
lxms$school.id <- as.character(lxms$school.id)
ms.c1 <- merge(lxms, rdy_esms[, c("school.id", "pts.att", "pts.btc")], all.x = TRUE, all.y = FALSE)
ms.c1 <- as.data.frame(ms.c1)

vars.rdpts <- c("pts.literacy", "pts.att", "pts.btc")
ms.c1 <- ms.c1[, c("school.id", "school.name", vars.rdpts)]

ms.c1[, vars.rdpts] <- apply(ms.c1[, vars.rdpts], 2, function(x) ifelse(x > 100, 100, x))
ms.c1[, vars.rdpts] <- apply(ms.c1[, vars.rdpts], 2, function(x) x*wgt.em)
ms.c1$sum_pts <- apply(ms.c1[, vars.rdpts], 1, sum, na.rm = TRUE)

lkable(ms.c1[order(ms.c1$school.name), c("school.name", "pts.literacy", "pts.att", "pts.btc", "sum_pts")],
       caption = "Middle Schools' Weighted Readiness Component Points \\& Composite Readiness Scores[note]",
       col.names = c("School",
                     "Literacy",
                     "Attendance",
                     "BTC",
                     "Readiness Score"),
       row.names = FALSE, format = 'latex', booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "repeat_header")) %>%
  add_footnote(c("MS Readiness Weights (all indicators) = 0.333"))
#'
#' \newpage
#' # High Schools' Readiness
#'
#' ------
#'
#' ## Literacy
#'
#+ LX-HS

# RD - HS - LIT -----------------------------------------------------------

lxhs0 <- Rlxmid(x = dat.eoc, grades = lxgrades$hs, gradevar = "stugrade", midpoints = lxmidpoints$hs)
lxhs.n0 <- table(lxhs0$schcode, lxhs0$lexile.readiness.point)
lxhs.n1 <- apply(lxhs.n0, 1, sum, na.rm = TRUE)
lxhs.p <- data.frame(school.id = names(lxhs.n1), pts.literacy = (lxhs.n0[, "1"]/lxhs.n1)*100)
lxhs <- merge(fcs_schools[, c("school.id", "school.name")], lxhs.p)
lxhs <- lxhs[!duplicated(lxhs), ] %>% droplevels()
kable(lxhs[order(lxhs$school.name), c("school.name", "pts.literacy")],
      caption = "Literacy Readiness Points",
      col.names = c("School", "Literacy Pts."), row.names = FALSE)
#'
#' \newpage
#' ## College & Career Readiness (CCR) - Using 2017 Data
#'
#+

# RD - HS - CCR -----------------------------------------------------------------

Rindicators <- function(x, idvar, v.names, timevar, benchmark = 1){
    x[, v.names] <- factor(x[, v.names])
    xpw <- reshape(x, v.names = v.names, idvar = idvar, timevar = timevar, direction = "wide")
    xp_n0 <- sapply(xpw[, -1], table) %>% t() %>% as.data.frame()
    xp_n0$n <- apply(xp_n0, 1, sum)
    xp_n1 <- data.frame(school.id = gsub(paste0(v.names, "\\."), "", rownames(xp_n0)), xp_n0)
    # xp_n2 <- merge(fcs_schools, xp_n1)
    xp_n <- xp_n1[!duplicated(xp_n1), ]

    ## COMPUTE INDICATOR PERFORMANCE POINT POINTS ##
    pts.indicator <- round((xp_n[, "X1"]/xp_n$n)*100, 0)

    ## ADDED BENCHMARK OPTION AND IMPLEMENTATION ON 20180607 ##
    xp_p <- data.frame(school.id = xp_n[, timevar], N.met.indicator = xp_n[, "X1"], N.students.total = xp_n$n,
                       pts = pts.indicator, benchmark = benchmark, pts.adj = pts.indicator/benchmark)
    return(xp_p)
}

ccr <- Rrdcsv("data/zRDY-HS-CCR-2017.csv")[, c("school.id", "gtid", "meets.indicator")]
ccrp <- Rindicators(ccr, idvar = "gtid", timevar = "school.id", v.names = "meets.indicator")
ccrp <- merge(fcs_schools, ccrp, by = "school.id")
kable(ccrp[order(ccrp$school.name), c("school.id", "school.name",
                                      "N.met.indicator", "N.students.total", "pts.adj")],
      caption = "High Schools' College & Career Readiness (CCR) Indicator Points",
        col.names = c("School ID", "School Name", "N Met Indicator", "N Students Total", "CCR Pts."),
      row.names = FALSE)

ccrp <- subset(ccrp, select = -c(school.name))
names(ccrp) <- c("school.id", "N.met.indicator", "N.students.total",
                 "pts.ccr", "benchmark", "pts.adj")
#'
#' \newpage
#' ## Attendance, Pathway Completion, Accellerated Enrollment, & Composite Readiness Scores
#'
#'

rdy_hs0 <- Rrdcsv("data/RDY-HS-2018.csv")
rdy_hs <- rdy_hs0[, c("school.id", "attendance", "pathway.completers", "accelerated.enrollment")]
names(rdy_hs) <- c("school.id", "pts.att", "pts.pc", "pts.ae")
rdy_hs[, c("pts.att", "pts.pc", "pts.ae")] <-
  apply(rdy_hs[, c("pts.att", "pts.pc", "pts.ae")], 2, function(x) x*100)

# RD - HS - COMPOSITE -----------------------------------------------------------

wgt.hs <- 0.20

lxhs$school.id <- as.character(lxhs$school.id)

hs.c1 <- Reduce(function(x, y) merge(x, y, all = TRUE),
                list(lxhs,
                     rdy_hs[, c("school.id", "pts.att", "pts.ae", "pts.pc")],
                     ccrp[, c("school.id", "pts.ccr")]
                ))
hs.c1 <- as.data.frame(hs.c1)

vars.rdpts <- c("pts.literacy", "pts.att", "pts.ae", "pts.pc", "pts.ccr")
hs.c1 <- hs.c1[, c("school.id", "school.name", vars.rdpts)]

hs.c1[, vars.rdpts] <- apply(hs.c1[, vars.rdpts], 2, function(x) ifelse(x > 100, 100, x))
hs.c1[, vars.rdpts] <- apply(hs.c1[, vars.rdpts], 2, function(x) x*wgt.em)
hs.c1$sum_pts <- apply(hs.c1[, vars.rdpts], 1, sum, na.rm = TRUE)

lkable(hs.c1[order(hs.c1$school.name), c("school.name", "pts.literacy", "pts.att", "pts.ae", "pts.pc", "pts.ccr", "sum_pts")],
       caption = "High Schools' Weighted Readiness Component Points \\& Composite Readiness Scores",
       col.names = c("School",
                     "Literacy",
                     "Attendance",
                     "Acc. Enrollment",
                     "Pathway Completion",
                     "CCR",
                     "Readiness Score"),
       row.names = FALSE, format = 'latex', booktabs = TRUE, longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "repeat_header")) %>%
  add_footnote(c("HS Readiness Weights (all indicators) = 0.20"))
#'
#' \newpage
#'
#' # Graduation Rates (Grade-12 Only)
#'
# HS - GRAD RATE ---------------------------------------------------------------

grd <- Rrdcsv("data/GRAD-HS-2018.csv")[, c("school.id", "school.name", "x2018.estimated.grad.rate")]
names(grd) <- c("school.id", "school.name", "pts.grad")
grd$pts.grad <- grd$pts.grad*100

lkable(grd[order(grd$school.name), ], caption = "High School Graduation Rates") %>%
  kable_styling(latex_options = c("scale_down"))


# WRITE READINESS & GRAD RATES ---------------------------------------------------------

xlsx::write.xlsx(es.c1, "reports/Readiness_All.xlsx", sheetName = "readiness_ES", append = FALSE, row.names = FALSE)
xlsx::write.xlsx(ms.c1, "reports/Readiness_All.xlsx", sheetName = "readiness_MS", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(hs.c1, "reports/Readiness_All.xlsx", sheetName = "readiness_HS", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(grd, "reports/GraduationRate_HS.xlsx", sheetName = "GraduationRate_HS", append = FALSE, row.names = FALSE)
#'
#' \newpage
#'
