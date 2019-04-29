#' ---
#' title: "Fulton County Schools CCRPI 2017 Replicated using 2018 Scoring Model - Progress Component (using 2017 data as a temporary placeholder)"
#' date: "Last Updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#' \newpage
#'
#+ setup, include=FALSE
# SETUP -------------------------------------------------------------------
.date_suffix <- format(Sys.Date(), '%Y%m%d')

source("SETUP.R") ## SETS MY OWN VARIOUS GLOBAL R OPTIONS AND DEFINES A FEW UTILITY FUNCTIONS FOR GENERATING REPORTS ##
library(kableExtra); library(dplyr)
kable <- function(x, format = "pandoc", ...) {
  knitr:::kable(x, format = format, ...)
}

knitr::opts_chunk$set(echo = FALSE,
                      results = 'asis',
                      dev = c("pdf", "png", "svg"))

# library(ccrpi)

### ACTUAL 2017 CCRPI SCORING BY COMPONENT ####
dat17 <- Rrdcsv("data/2017-ComponentScoring.csv", asDT = FALSE)
dat17 <- dat17[dat17$system.id == 660, ]
dat17$system.id <- as.character(car::recode(dat17$system.id, c("'ALL' = 0; 'RTC' = 1")))
dat17$school.id <- as.character(car::recode(dat17$school.id, c("'ALL' = 0;")))

## FORCE ACTUAL SINGLE SCORE FOR CHARTER SCHOOLS WITH NON-STANDARD GRADEBANDS (IMPLEMENTED ON 20180618) ##
dat17$single.score.orig <- dat17$single.score ## KEEP ORIGINAL SINGLE.SCORE VALUE IN A SEPARATE COLUMN, JUST IN CASE ##
dat17$single.score <- ifelse(dat17$school.id %in% c("204", "116", "1319"), dat17$ccrpi.score, dat17$single.score) ## KIPP (204); MAIN ST. ACA. (116), & FAST (1319)
# dat17$single.score <- dat17$challenge.points + dat17$ccrpi.score

fcs17 <- dat17[dat17$system.id == 660, ] %>% droplevels()
fcs17$school.name <- gsub("'", "", as.character(fcs17$school.name), perl = TRUE)

### FCS_SCHOOLS ####
fcs_schools <- fcs17[, c("school.id", "school.name")]
fcs_schools <- fcs_schools[!fcs_schools$school.id %in% c("307", "1321", "1312", "0"), ] %>% droplevels()
ids <- c("school.id", "gtid")

# PROGRESS COMPONENT FUNCTIONS --------------------------------------------

Rprgpts <- function(x, subjectarea, fayfilter = TRUE, assessmenttype = c("EOG", "EOC", "GAA"), ids = c("school.id", "gtid"), ptsvar = "x2018.points") {
  ## FILTER ON SPECIFIED SUBJECT AREA ##
  x0 <- x[x$assessmentsubjectareacode == subjectarea, ] %>% droplevels()
  ## INCLUDE ONLY VALID ASSESSMENT TYPES ##
  x0 <- x0[x0$assessmenttypecode %in% assessmenttype, , drop = FALSE] %>% droplevels()
  # x0 <- x0[x0$gradecluster == gradecluster, ] %>% droplevels()
  x0 <- merge(fcs_schools, x0) %>% droplevels()
  xnew <- x0[!duplicated(x0), ]

  ## FILTER ON FAY.PARTICIPANT IF APPLICABLE ##
  if (fayfilter == TRUE) {
    xnew <- xnew[xnew$fayparticipant == "Y", , drop = FALSE] %>% droplevels()
  }

  ## SUBSET TO INCLUDE ID COLUMNS AND SPECIFIED POINTS COLUMN ##
  xpts <- xnew[, c(ids, "gradecluster", "assessmentsubjectcode", ptsvar)]
  names(xpts) <- c(ids, "gradecluster", "assessmentsubjectcode", "pts")
  xpts$pts <- factor(xpts$pts)
  xpts <- xpts[!duplicated(xpts), ]
  xptsw <- reshape(xpts, v.names = c("pts"), idvar = c("gtid", "gradecluster", "assessmentsubjectcode"), timevar = "school.id", direction = "wide")

  xpts_n0 <- sapply(xptsw[, -1:-3], table) %>% t()

  xpts_n1 <- data.frame(school.id = gsub("pts.", "", rownames(xpts_n0)), xpts_n0)
  xpts_n2 <- merge(fcs_schools, xpts_n1)
  xpts_n <- xpts_n2[!duplicated(xpts_n2), ]
  xpts_n$n <- apply(xpts_n0, 1, sum)
  xpts_p <- data.frame(xpts_n[, 1:2], round((xpts_n0/xpts_n$n)*100, 2)) ## MODIFIED TO ROUND TO 2 DEC. PLACES, PER 2018 CALC. GUIDE, ON 2018-06-03 ##

  xpts_p1 <- data.frame(xpts_n[, 1:2],
                        apply(format(round((xpts_n0/xpts_n$n)*100, 2), nsmall = 2),
                              2, Ras.percent))
  return(list(xpts_n = xpts_n, xpts_p = xpts_p, xpts_p1 = xpts_p1))
}

Rprg <- function(xpts_n, xpts_p) {
  xpts_ptr0 <- t(xpts_p[, c("school.id", grep("[XL]\\d", names(xpts_p), value = TRUE))])
  colnames(xpts_ptr0) <- xpts_ptr0[1, ]
  xpts_ptr01 <- xpts_ptr0[-1, ]
  xpts_ptr <- apply(xpts_ptr01, 2, as.numeric)
  wgts <- c(0, 0.5, 1, 1.5)
  xpts_ptr1 <- xpts_ptr*wgts
  xpts_pts <- t(xpts_ptr1)
  prg.subj0 <- data.frame(school.id = rownames(xpts_pts), prg.subj = apply(xpts_pts, 1, sum))
  # prg.subj1 <- merge(xpts_n[, c("school.id", "n")], prg.subj0)
  prg.subj1 <- merge(fcs_schools, prg.subj0)
  prg.subj <- prg.subj1[!duplicated(prg.subj1), ]
  prg.subj$school.id <- as.numeric(as.character(prg.subj$school.id))
  return(list(xpts_pts = xpts_pts, prg.subj = prg.subj))
}


Relp <- function(x, ids = c(school = "school.id", student = "gtid", group = "student.grade.level")) {
  colnames(x) <- tolower(colnames(x)) ## PREEMPTIVELY FORCE ALL COLNAMES TO LOWER CASE ##
  colnames(x) <- gsub("[\\.]{2,}", ".", colnames(x), perl = TRUE) ## PREEMPTIVELY CLEAN EXCESS CONSECUTIVE "."s (CAUSED BY EXCESS CONSECUTIVE WHITESPACES) IN x'S COLNAMES ##
  ## NOTE: DEF OF 'elpcols' BELOW ASSUMES THE FOLLOWING COLUMNS EXIST IN 'x': 'cscore.prior' = 'Prior Year Composite Score' ('Y'); 2. 'Prior Year Performance Band' ('Z'); 3. 'Current Year Composite Score' ('AA'); 4. 'Current Year Performance Band' ('AB') ##
  ## ALSO NOTE: THE BELOW-DEFINED 'elpcols' VAR IS PURELY 'COSMETIC', BUT THE ADDED CODE-READABILITY WILL BE USEFUL FOR ANY DEBUGGING NEEDED FOR THIS FUNCTION IN THE FUTURE ##
  ## ALSO ALSO NOTE: THE BELOW CODE CREATES A COPY OF THE INPUT DF, 'x', WHICH ALSO SERVES A PRIMARILY DEBUGGING-SPECIFIC PURPOSE ##
  xnew <- plyr::rename(x, c("prior.year.composite.score" = "cscore.prior",
                            "prior.year.performance.band" = "pband.prior",
                            "current.year.composite.score" = "cscore.current",
                            "current.year.performance.band" = "pband.current"))

  ## IMPLEMENT ELP BUSINESS RULES (IT SHOULD BE CLEAR AT THIS POINT WHAT'S GOING ON IN THE NEXT CODE-CHUNK, IF NOT, THEN YOU HAVE NO BUSINESS EVALUATING/PROOFING THIS, OR ANY OTHER, CODE (IN ANY PROGRAMMING/SCRIPTING LANG)) ##
  xnew <- within(xnew, {
    ELP <- ifelse((pband.current <= pband.prior) & (cscore.current <= cscore.prior),
                  "L1", NA)
    ELP <- ifelse((pband.current == pband.prior) & (cscore.current > cscore.prior),
                  "L2", ELP)
    ELP <- ifelse((pband.current - pband.prior) == 1,
                  "L3", ELP)
    ELP <- ifelse((pband.current - pband.prior) > 1,
                  "L4", ELP)
  })

  ## THE REST IS ESSENTIALLY REPETITIVE OF PROCEDURES DONE VIA 'Rprgpts()', EXCEPT WITH NAMING CONVENTIONS SPECIFIC TO ELP, FOR NOW (TOOD: COMBINE Relp() & Rprgpts()) ##

  # ## REMOVE ANY RESULTING DUPLICATES ##
  xnew <- xnew[!duplicated(xnew), ]

  ## CREATE SUBSET DF ('xpts') OF 'xnew' CONTAINING ONLY ID VARS (SEE DEFAULT VALUES FOR THE 'ids' ARG FUN CALL LINE ABOVE) AND ELP POINT VAR (CREATED IN PREVIOUS CODE-CHUNK) ##
  xpts <- xnew[, c(Runname(ids), "ELP")]

  ## SET 'ELP' POINTS VAR AS A FACTOR, TO RESTRICT TO ONLY VALID VALUES (ALL OTHERS WILL BE COERCED TO 'NA's, WHICH WILL RESULT IN A WARNING THAT CAN BE USED FOR DEBUGGING AS NEEDED, WHICH IS, IMO, THE ONLY REAL REASON FOR SETTING A VAR TO A FACTOR, SO LONG AS YOU HAVE A PREDETERMINED SET OF VALID VALUES, FYI) ##
  xpts$ELP <- factor(xpts$ELP, levels = c(paste0("L", 1:4))) ## AGAIN, SEE 'ELP' VAR DEF ABOVE FOR REFERENCE ##

  ## RESHAPE 'xpts' TO WIDE FORMAT, WITH EACH COLUMN CONTAINING DATA FOR A UNIQUE SCHOOL (VIA 'school.id' BY DEFAULT), AND EACH ROW REPRESENTING A UNIQUE STUDENT'S (VIA 'gtid' BY DEFAULT) ELP POINT CONTRIBUTION (VIA 'ELP') PER GRADELEVEL/GROUP (VIA 'student.grade.level' BY DEFAULT) WITHIN A SCHOOL (AGAIN VIA 'school.id' BY DEFAULT)
  xptsw <- reshape(xpts, v.names = c("ELP"), idvar = Runname(c(ids[c("student", "group")])), timevar = "school.id", direction = "wide")
  ## FYI [TLDR: 'Runname()' IS A UTILITY FUN IN MY 'Riley' R-PKG ... RUN '?is.atomic' FOR MORE)]: 'Runname() IS A UTILITY FUN FROM MY 'Riley' R-PKG THAT STRIPS THE NAMES ATTRIBUTE FROM A GIVEN OBJECT (WHICH PARTICULARLY IS HELPFUL IN SITUATIONS LIKE THIS WHERE YOU WANT TO BE ABLE TO CALL, INLINE/EN-VIVO, AN 'ATOMIC' OBJECT'S ELEMENTS BY NAME, WHILE MAINTAINING THE OBJECT'S 'ATOMIC' IMPLEMENTATION IN THE CONTEXT OF THE THE SURROUNDING CALL. RUN '?is.atomic' IN THE R CONSOLE FOR MORE) ##

  ## COMPUTE NUMERATOR FOR EACH SCHOOL'S FINAL ELP POINTS, I.E., TABULATED COUNTS OF UNIQUE 'ELP' VALUES (SEE PREVIOUS DEF OF 'ELP' COLUMN FOR REFERENCE) WITHIN EACH COLUMN (I.E., SCHOOL), EXCEPT FOR THE STUDENT & GROUP ID VARS (AGAIN, SEE ABOVE) ##
  xpts_n0 <- sapply(xptsw[, -1:-2], table) %>%
    t() ## TRANSPOSE THE RESULTING TABULATIONS DF SO THAT STUDENT-GRADE COMBOSE ARE NOW ON COLUMNS, AND SCHOOLS ON ROWS ##

  ## CREATE NEW DF CONTAINING (1) SCHOOL.IDs (BY STIPPING R'S DEFAULT ROWNAMES PREFIX RESULTING FROM THE ABOVE TWO PROCEDURES), & (2) THE TABULATIONS RESULTING FROM THE IMMEDIATELY-ABOVE CODE-CHUNK ##
  xpts_n1 <- data.frame(school.id = gsub("ELP\\.", "", rownames(xpts_n0)), xpts_n0)

  xpts_n1$n <- apply(xpts_n0, 1, sum) ## COMPUTE DENOMINATOR FOR EACH SCHOOL'S FINAL ELP POINTS, I.E., TOTAL NUMBER OF STUDENTS WITH VALID ELP POINTS PER SCHOOL ##

  ## COMPUTE EACH SCHOOL'S FINAL ELP POINTS USING THE ABOVE COMPUTED NUMERFATOR AND DENOMINATOR PER SCHOOL ##
  xpts_p <- data.frame(school.id = xpts_n1[, 1], round((xpts_n0/xpts_n1$n)*100, 2))

  ## CLEAN UP FOR PRESENTATION ##
  xpts_p1 <- data.frame(school.id = xpts_n1[, 1], apply(format(round((xpts_n0/xpts_n1$n)*100, 2), nsmall = 2), 2, Ras.percent)) ## RECODE EACH SCHOOL'S COMPUTED ELP POINTS AS A PRECENTAGE ##
  # xpts_p1[, 3:6] <- apply(xpts_p1[, c], 2, function(x) ifelse(x == "NaN%", NA, x))

  ## MERGE THE RESULTING DF WITH THE 'fcs_schools' DF (SEE 'zmeta.R'), TO BRING IN SCHOOL NAMES ##
  xpts_n <- merge(fcs_schools, xpts_n1, all.y = TRUE)
  xpts_n <- xpts_n[!is.na(xpts_n$school.name), ] ## REMOVE ROWS WITH BLANK SCHOOL NAMES (CAUSED BY MERGE WITH FCS_SCHOOLS) ##
  xpts_p <- merge(fcs_schools, xpts_p, all.y = TRUE)
  xpts_p <- xpts_p[!is.na(xpts_p$school.name), ] ## REMOVE ROWS WITH BLANK SCHOOL NAMES (CAUSED BY MERGE WITH FCS_SCHOOLS) ##
  xpts_p1 <- merge(fcs_schools, xpts_p1, all.y = TRUE)
  xpts_p1 <- xpts_p[!is.na(xpts_p$school.name), ] ## REMOVE ROWS WITH BLANK SCHOOL NAMES (CAUSED BY MERGE WITH FCS_SCHOOLS) ##

  return(list(xpts_n = xpts_n, xpts_p = xpts_p, xpts_p1 = xpts_p1))
} ## THE RETURNED RESULTS MUST BE RUN THROUGH THE 'Rprg()', DEFINED BELOW FUN TO IMPLEMENT ACTUAL POINT WEIGHTS FOR ACTUAL FINAL PRG.ELP INDICATOR CALCULATION ##

cols.prg <- c("school.id", "school.name", "prg.ela", "prg.elaw", "prg.math", "prg.mathw", "prg.elp", "prg.elpw", "n_elp", "prg.score") ## COLUMN ORDER FOR FINAL PRG.ES/PRG.MS/PRG.HS OUTPUT TABLES ##

rec.prgnames <- c("'school.id' = 'School ID'; 'school.name' = 'School Name'; 'gradecluster' = 'Grade Cluster'; 'prg.ela' = 'Raw ELA Points'; 'prg.elaw' = 'Weighted ELA Points'; 'prg.math' = 'Raw Math Points'; 'prg.mathw' = 'Weighted Math Points'; 'prg.elp' = 'Raw ELP Points'; 'prg.elpw' = 'Weighted ELP Points'; 'n_elp' = 'N ELP Results'; 'prg.score' = 'Progress Component Score'") ## FOR PRG.ES/PRG.MS/PRG.HS PRESENTATION OUTPUT TABLE ##

#'
#' # Student Growth Points (SGPs) - All Grade Levels
#'
#' -----
#'
#' ## ELA SGPs
#+
sgp <- Riley::Rrdcsv("data/PRG-ALL-SGP.csv")
names(sgp) <- gsub("schoolid", "school.id", names(sgp))

ptsela <- Rprgpts(x = sgp, subjectarea = "E", fayfilter = TRUE)
prgela <- Rprg(xpts_n = ptsela$xpts_n, xpts_p = ptsela$xpts_p)

prgela$prg.subj <- merge(unique(sgp[, c("school.id", "gradecluster")]), prgela$prg.subj, all = TRUE)
prgela$prg.subj <- prgela$prg.subj[!is.na(prgela$prg.subj$school.name), ]
prgela$prg.subj$gradecluster <- factor(prgela$prg.subj$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
prgela$prg.subj <- prgela$prg.subj[order(prgela$prg.subj$gradecluster), ]

ptsela$xpts_n <- merge(unique(sgp[, c("school.id", "gradecluster")]), ptsela$xpts_n, all = TRUE)
ptsela$xpts_n <- ptsela$xpts_n[!is.na(ptsela$xpts_n$school.name), ]
ptsela$xpts_n$gradecluster <- factor(ptsela$xpts_n$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
ptsela$xpts_n <- ptsela$xpts_n[order(ptsela$xpts_n$gradecluster), ]
# prgela$prg.subj <- prgela$pg.subj[!is.na(prgela$prg.subj$school.name), ]
# prgela$prg.subj <- prgela$pg.subj[order(prgela$prg.subj$gradecluster), ]

ptsela$xpts_p <- merge(unique(sgp[, c("school.id", "gradecluster")]), ptsela$xpts_p, all = TRUE)
ptsela$xpts_p <- ptsela$xpts_p[!is.na(ptsela$xpts_p$school.name), ]
ptsela$xpts_p$gradecluster <- factor(ptsela$xpts_p$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
ptsela$xpts_p <- ptsela$xpts_p[order(ptsela$xpts_p$gradecluster), ]

ptsela$xpts_p1 <- merge(unique(sgp[, c("school.id", "gradecluster")]),ptsela$xpts_p1, all = TRUE)
ptsela$xpts_p1 <- ptsela$xpts_p1[!is.na(ptsela$xpts_p1$school.name), ]
ptsela$xpts_p1$gradecluster <- factor(ptsela$xpts_p1$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
ptsela$xpts_p1 <- ptsela$xpts_p1[order(ptsela$xpts_p1$gradecluster), ]

kable(ptsela$xpts_n[, -1], caption = "All Schools' ELA SGP Counts by School", col.names = c("Grade Cluster", "School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"), align = c("l", "l", rep("r", 5)))

kable(ptsela$xpts_p1[, -1], caption = "All Schools' ELA SGP %'s by School", col.names = c("Grade Cluster", "School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"), align = c("l", "l", rep("r", 4)))

kable(prgela$prg.subj[, -1], caption = "All Schools' ELA Student Growth", col.names = c("Grade Cluster", "School", "ELA SGPs"), align = c("l", "l", "r"))
names(prgela$prg.subj) <- gsub("subj", "ela", names(prgela$prg.ela))

#'
#' \newpage
#' ## Mathematics SGPs
#+
# SGP - MATH --------------------------------------------------------------
ptsmath <- Rprgpts(x = sgp, subjectarea = "M", fayfilter = TRUE)
prgmath <- Rprg(xpts_n = ptsmath$xpts_n, xpts_p = ptsmath$xpts_p)

prgmath$prg.subj <- merge(unique(sgp[, c("school.id", "gradecluster")]), prgmath$prg.subj, all = TRUE)
prgmath$prg.subj <- prgmath$prg.subj[!is.na(prgmath$prg.subj$school.name), ]
prgmath$prg.subj$gradecluster <- factor(prgmath$prg.subj$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
prgmath$prg.subj <- prgmath$prg.subj[order(prgmath$prg.subj$gradecluster), ]

ptsmath$xpts_n <- merge(unique(sgp[, c("school.id", "gradecluster")]), ptsmath$xpts_n, all = TRUE)
ptsmath$xpts_n <- ptsmath$xpts_n[!is.na(ptsmath$xpts_n$school.name), ]
ptsmath$xpts_n$gradecluster <- factor(ptsmath$xpts_n$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
ptsmath$xpts_n <- ptsmath$xpts_n[order(ptsmath$xpts_n$gradecluster), ]
# prgmath$prg.subj <- prgmath$pg.subj[!is.na(prgmath$prg.subj$school.name), ]
# prgmath$prg.subj <- prgmath$pg.subj[order(prgmath$prg.subj$gradecluster), ]

ptsmath$xpts_p <- merge(unique(sgp[, c("school.id", "gradecluster")]), ptsmath$xpts_p, all = TRUE)
ptsmath$xpts_p <- ptsmath$xpts_p[!is.na(ptsmath$xpts_p$school.name), ]
ptsmath$xpts_p$gradecluster <- factor(ptsmath$xpts_p$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
ptsmath$xpts_p <- ptsmath$xpts_p[order(ptsmath$xpts_p$gradecluster), ]

ptsmath$xpts_p1 <- merge(unique(sgp[, c("school.id", "gradecluster")]),ptsmath$xpts_p1, all = TRUE)
ptsmath$xpts_p1 <- ptsmath$xpts_p1[!is.na(ptsmath$xpts_p1$school.name), ]
ptsmath$xpts_p1$gradecluster <- factor(ptsmath$xpts_p1$gradecluster, levels = c("E", "M", "H"), ordered = TRUE)
ptsmath$xpts_p1 <- ptsmath$xpts_p1[order(ptsmath$xpts_p1$gradecluster), ]

kable(ptsmath$xpts_n[, -1], caption = "All Schools' Math SGP Counts by School", col.names = c("Grade Cluster", "School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"), align = c("l", "l", rep("r", 5)))

kable(ptsmath$xpts_p1[, -1], caption = "All Schools' Math SGP %'s by School", col.names = c("Grade Cluster", "School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"), align = c("l", "l", rep("r", 4)))

kable(prgmath$prg.subj[, -1], caption = "All Schools' Math Student Growth", col.names = c("Grade Cluster", "School", "Math SGPs"), align = c("l", "l", "r"))
names(prgmath$prg.subj) <- gsub("subj", "math", names(prgmath$prg.math))

#'
#' -----
#'
#' # Progress Toward ELP (by Grade Cluster)
#'
#' -----
#'
#' ## Elementary Schools' Progress toward ELP
#'
#+
# ELP - ES --------------------------------------------------------------------------
elp.es <- Rrdcsv("data/PRG-ES-ELP.csv")
elppts.es <- Relp(x = elp.es)
prgelp.es <- Rprg(xpts_n = elppts.es$xpts_n, xpts_p = elppts.es$xpts_p)
prg.elp.es <- merge(prgelp.es$prg.subj, elppts.es$xpts_n[, c("school.id", "n")], all = TRUE)
names(prg.elp.es) <- c("school.id", "school.name", "prg.elp", "n_elp")

kable(elppts.es$xpts_n[, -1], caption = "ELP Band Counts by School", col.names = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"))
kable(elppts.es$xpts_p1[, -1], caption = "ELP Band Percentages by School", col.names = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"))
kable(prg.elp.es[, -1], caption = "Elementary Schools' Progress toward ELP Indicator Points", col.names = c("School", "ELP Indicator Points", "N ELP Results"))

#'
#' -----
#'
#' ## Middle Schools' Progress Toward ELP
#'
#+

# ELP - MS --------------------------------------------------------------------------
elp.ms <- Rrdcsv("data/PRG-MS-ELP.csv")
elppts.ms <- Relp(x = elp.ms)
prgelp.ms <- Rprg(xpts_n = elppts.ms$xpts_n, xpts_p = elppts.ms$xpts_p)
prg.elp.ms <- merge(prgelp.ms$prg.subj, elppts.ms$xpts_n[, c("school.id", "n")], all = TRUE)
names(prg.elp.ms) <- c("school.id", "school.name", "prg.elp", "n_elp")

kable(elppts.ms$xpts_n[, -1], caption = "ELP Band Counts by School", col.names = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"))
kable(elppts.ms$xpts_p1[, -1], caption = "ELP Band Percentages by School", col.names = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"))
kable(prg.elp.ms[, -1], caption = "Middle Schools' Progress toward ELP Indicator Points", col.names = c("School", "ELP Indicator Points", "N ELP Results"))
#' -----
#'
#'
#' # High Schools' Progress toward ELP
#'
#'
#+

# ELP - HS --------------------------------------------------------------------------

elp.hs <- Rrdcsv("data/PRG-HS-ELP.csv")
elppts.hs <- Relp(x = elp.hs)
prgelp.hs <- Rprg(xpts_n = elppts.hs$xpts_n, xpts_p = elppts.hs$xpts_p)
prg.elp.hs <- merge(prgelp.hs$prg.subj, elppts.hs$xpts_n[, c("school.id", "n")], all = TRUE)
names(prg.elp.hs) <- c("school.id", "school.name", "prg.elp", "n_elp")

kable(elppts.hs$xpts_n[, -1], caption = "ELP Band Counts by School", col.names = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band", "N Total"))
kable(elppts.hs$xpts_p1[, -1], caption = "ELP Band Percentages by School", col.names = c("School", "No Mvmt.", "< 1 Band", "1 Band", "> 1 Band"))
kable(prg.elp.hs[, -1], caption = "High Schools' Progress toward ELP Indicator Points", col.names = c("School", "ELP Indicator Points", "N ELP Results"))

#'
#' \newpage
#'
#' -----
#'
#' # Composite Progress Scores (by Grade Cluster)
#'
#' -----
#'
#' ## Elementary Schools Comoposite Progress Scores
#'
#+
# ES-PRG COMPOSITE ----------------------------------------------------------------

colnames(prgela$prg.subj) <- c("school.id", "gradecluster", "school.name", "prg.ela")
colnames(prgmath$prg.subj) <- c("school.id", "gradecluster", "school.name", "prg.math")

prg.es1 <- merge(prgela$prg.subj[prgela$prg.subj$gradecluster == "E", ], prgmath$prg.subj[prgmath$prg.subj$gradecluster == "E", ], all = TRUE)
prg.es2 <- merge(prg.es1, prg.elp.es, all = TRUE)

prg.es <- within(prg.es2, {
    ## MODIFIED TO INCLUDE NA'S IN IFELSE LOGIC RULES ON 2018-06-03 ##
    n_elp <- Rna(n_elp)
    prg.elp <- Rna(prg.elp)
    prg.elaw <- ifelse(n_elp < 15, prg.ela*.50, prg.ela*.45)
    prg.mathw <- ifelse(n_elp < 15, prg.math*.50, prg.math*.45)
    prg.elpw <- ifelse(n_elp < 15, 0, prg.elp*.10)
})

prg.es[, c("prg.elaw", "prg.mathw", "prg.elpw")] <- apply(
    prg.es[, c("prg.elaw", "prg.mathw", "prg.elpw")], 2,
    function(x) ifelse(x >= 100, 100, x))
prg.es$prg.score <- apply(prg.es[, c("prg.elaw", "prg.mathw", "prg.elpw")], 1, sum)
prg.es$prg.score <- round(prg.es$prg.score, 1) ## ROUND COMPONENT SCORE TO 1 DEC. PT., PER 2018 CALC. GUIDE [IMPLEMENTED ON 2018-06-03] ##
prg.es$prg.score <- ifelse(prg.es$prg.score >= 100, 100, prg.es$prg.score)

prg.es <- prg.es[, c(cols.prg)]
Rdt(prg.es, caption = "Progress Points by Area Composite Progress Scores", colnames = car::recode(names(prg.es), rec.prgnames)) ## 'rec.prgnames' DEFINED IN 'zmeta.R' ##

#'
#' -----
#'
#' ## Middle Schools Progress Composite Scores
#'
#+
# MS-PRG COMPOSITE ----------------------------------------------------------------
prg.ms1 <- merge(prgela$prg.subj[prgela$prg.subj$gradecluster == "M", ], prgmath$prg.subj[prgmath$prg.subj$gradecluster == "M", ], all = TRUE)
prg.ms2 <- merge(prg.ms1, prg.elp.ms, all = TRUE)

prg.ms <- within(prg.ms2, {
    ## MODIFIED TO INCLUDE NA'S IN IFELSE LOGIC RULES ON 2018-06-03 ##
    n_elp <- Rna(n_elp)
    prg.elp <- Rna(prg.elp)
    prg.elaw <- ifelse(n_elp < 15, prg.ela*.50, prg.ela*.45)
    prg.mathw <- ifelse(n_elp < 15, prg.math*.50, prg.math*.45)
    prg.elpw <- ifelse(n_elp < 15, 0, prg.elp*.10)
})

prg.ms[, c("prg.elaw", "prg.mathw", "prg.elpw")] <- apply(
    prg.ms[, c("prg.elaw", "prg.mathw", "prg.elpw")], 2,
    function(x) ifelse(x >= 100, 100, x))
prg.ms$prg.score <- apply(prg.ms[, c("prg.elaw", "prg.mathw", "prg.elpw")], 1, sum)
prg.ms$prg.score <- round(prg.ms$prg.score, 1) ## ROUND COMPONENT SCORE TO 1 DEC. PT., PER 2018 CALC. GUIDE [IMPLEMENTED ON 2018-06-03] ##
prg.ms$prg.score <- ifelse(prg.ms$prg.score >= 100, 100, prg.ms$prg.score)

prg.ms <- prg.ms[, c(cols.prg)]
kable(prg.ms, caption = "Progress Points by Area Composite Progress Scores", col.names = car::recode(names(prg.ms), rec.prgnames)) ## 'rec.prgnames' DEFINED IN 'zmeta.R' ##
#'
#' ------
#'
#' ## High Schools Progress Composite Scores
#'
#+
# HS-PRG COMPOSITE ----------------------------------------------------------------
prg.hs1 <- merge(prgela$prg.subj[prgela$prg.subj$gradecluster == "H", ], prgmath$prg.subj[prgmath$prg.subj$gradecluster == "H", ])
prg.hs2 <- merge(prg.hs1, prg.elp.hs, all = TRUE)

prg.hs <- within(prg.hs2, {
    ## MODIFIED TO INCLUDE NA'S IN IFELSE LOGIC RULES ON 2018-06-03 ##
    n_elp <- Rna(n_elp)
    prg.elp <- Rna(prg.elp)
    prg.elaw <- ifelse(n_elp < 15, prg.ela*.50, prg.ela*.45)
    prg.mathw <- ifelse(n_elp < 15, prg.math*.50, prg.math*.45)
    prg.elpw <- ifelse(n_elp < 15, 0, prg.elp*.10)
})

prg.hs[, c("prg.elaw", "prg.mathw", "prg.elpw")] <- apply(prg.hs[, c("prg.elaw", "prg.mathw", "prg.elpw")], 2,
                                                          function(x) ifelse(x >= 100, 100, x))
prg.hs$prg.score <- apply(prg.hs[, c("prg.elaw", "prg.mathw", "prg.elpw")], 1, sum)
prg.hs$prg.score <- round(prg.hs$prg.score, 1) ## ROUND COMPONENT SCORE TO 1 DEC. PT., PER 2018 CALC. GUIDE [IMPLEMENTED ON 2018-06-03] ##
prg.hs$prg.score <- ifelse(prg.hs$prg.score >= 100, 100, prg.hs$prg.score)

prg.hs <- prg.hs[, c(cols.prg)]
kable(prg.hs, caption = "High Schools' Progress Points by Area Composite Progress Scores", col.names = car::recode(names(prg.hs), rec.prgnames)) ## 'rec.prgnames' DEFINED IN 'zmeta.R' ##



## ***WRITE PRG - ALL*** ====================================================

prg.es$school.id <- as.integer(as.character(prg.es$school.id))
prg.ms$school.id <- as.integer(as.character(prg.ms$school.id))
prg.hs$school.id <- as.integer(as.character(prg.hs$school.id))
prgela$prg.subj$school.id <- as.integer(as.character(prgela$prg.subj$school.id))
prgmath$prg.subj$school.id <- as.integer(as.character(prgmath$prg.subj$school.id))
prg.elp.es$school.id <- as.integer(prg.elp.es$school.id)
prg.elp.ms$school.id <- as.integer(prg.elp.ms$school.id)
prg.elp.hs$school.id <- as.integer(prg.elp.hs$school.id)

xlsx::write.xlsx(prg.es, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "Progress_ES", append = FALSE, row.names = FALSE)
xlsx::write.xlsx(prg.ms, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "Progress_MS", append = TRUE, row.names = FALSE)
xlsx::write.xlsx(prg.hs, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "Progress_HS", append = TRUE, row.names = FALSE)
# xlsx::write.xlsx(prgela$prg.subj, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "sgp_ela_all", append = TRUE)
# xlsx::write.xlsx(prgmath$prg.subj, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "sgp_math_all", append = TRUE)
# xlsx::write.xlsx(prg.elp.es, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "elp_es", append = TRUE)
# xlsx::write.xlsx(prg.elp.ms, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "elp_ms", append = TRUE)
# xlsx::write.xlsx(prg.elp.hs, paste0("Progress_ALL-", .date_suffix, ".xlsx"), sheetName = "elp_hs", append = TRUE)
## ***END WRITE PRG - ALL*** ====================================================
