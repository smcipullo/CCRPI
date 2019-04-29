#' ---
#' title: "Fulton County Schools CCRPI 2017 Replicated using 2018 Scoring Model - CCRPI Single Score"
#' date: "Last Updated: `r format(Sys.Date(), '%d %b %Y')`"
#' ---
#'
#' \newpage
#'
#+ setup, include=FALSE
.START <- Sys.time() ## FOR R-CODE PERFORMANCE TRACKING PURPOSES ##


system.time(source("CCRPI-C2-Progress-2017.R"))[3]
system.time(source("CCRPI-C4-Readiness.R"))[3]
system.time(source("CCRPI-C1-ContentMastery.R"))[3]
system.time(source("CCRPI-C3-ClosingGaps.R"))[3]

.ls <- as.data.table(ls())
knitr::opts_chunk$set(echo = FALSE, tidy = TRUE, tidy.opts = list(width.cutoff = 60))

library(kableExtra)
kable <- function(x, format = "latex", booktabs = TRUE, escape = FALSE, longtable = TRUE, ...) {
  knitr:::kable(x, format = format, booktabs = booktabs, escape = escape, longtable = longtable, ...)
}
#'
#' # Elementary Schools' Component & Indicator Scores
#'
#' -----
#'
#' ## Content Mastery (CM)
#'
#+ echo=FALSE
vars.cm <- c("school.id", "school.name", "AchPts_Wgtd.ELA.cy", "AchPts_Wgtd.MATH.cy", "AchPts_Wgtd.SCI.cy", "AchPts_Wgtd.SS.cy", "cm.cy")
kable(cm.esm[, .SD, .SDcols = c(vars.cm)],
      col.names = c("School ID", "School Name",
                    "ELA",
                    "Math",
                    "Science",
                    "Social Studies",
                    "CM Component Score"), longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
#'
#' \newpage
#' ## Progress (PRG)
#'
#+ echo=FALSE
kable(prg.es[, c("school.id", "school.name", "prg.elaw", "prg.mathw", "prg.elpw", "prg.score")],
     col.names = c("School ID", "School Name", "ELA Points",
                   "Math", "ELP", "Progress Component Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

#'
#' \newpage
#' ## Closing Gaps (CG)
#'
#+ echo=FALSE
vars.cgc1 <- c("school.name",
              "CG_ELA", "N.Flags.ELA",
              "CG_MATH", "N.Flags.Math",
              "CG_Science", "N.Flags.Science",
              "CG_Soc.Studies", "N.Flags.Soc.Studies")
vars.cgc2 <- c("school.name", "N.Points.Total", "N.Flags.Total", "CG.SCORE")
kable(cgc.es[, .SD, .SDcols = c(vars.cgc1)],
      col.names = c("School",
                    "ELA Pts", "ELA Flags",
                    "Math Pts", "Math Flags",
                    "Sci. Pts", "Sci. Flags",
                    "Soc. Pts", "Soc. Flags")) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
kable(cgc.es[, .SD, .SDcols = c(vars.cgc2)],
      col.names = c("School", "Tot. Pts", "Tot. Flags", "CG Component Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

#'
#' \newpage
#' ## Readiness (RDY)
#'
#+ echo=FALSE
kable(es.c1[, c("school.name", "pts.literacy", "pts.att", "pts.btc", "sum_pts")],
      col.names = c("School",
                    "Literacy",
                    "Attendance",
                    "BTC",
                    "Readiness Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))
#'
#' \newpage
#'
#' ## Elementary Schools' Composite CCRPI Single Scores
#'
# GCOMP - ES ------------------------------------------------------
wgts.esms <- c(CM = 0.30, PRG = 0.35, CG = 0.15, RDY = 0.20) ## ES & MS ONLY ##
wgts.hs <- c(CM = 0.30, PRG = 0.30, CG = 0.10, RDY = 0.15, GRAD = 0.15) ## HS ONLY ##


es.c1$school.id <- as.integer(es.c1$school.id)
gcomp.es0 <- Reduce(function(x, y) merge(x, y, all = TRUE),
                    list(cm.esm[, c("school.id", "cm.cy")],
                         prg.es[, c("school.id", "prg.score")],
                         cgc.es[, c("school.id", "CG.SCORE")],
                         es.c1[, c("school.id", "sum_pts")]))
names(gcomp.es0) <- c("school.id", names(wgts.esms))
gcomp.es0 <- as.data.frame(gcomp.es0)

## ROUND ALL COMPONENTS' SCORES TO 1 DEC. PT. (PER 2018 CALC. GUIDES [IMPLEMENTED ON 2018-06-04]) ##
gcomp.es0[, names(wgts.esms)] <- apply(gcomp.es0[, names(wgts.esms)], 2, round, digits = 1)

## CAP ALL (UNWEIGHTED) COMPONENTS' SCORES AT 100PTS ##
gcomp.es0[, names(wgts.esms)] <- apply(
    gcomp.es0[, names(wgts.esms)], 2, function(x) ifelse(x > 100, 100, x))

## APPLY CCRPI COMPONENTS' WEIGHTS TO CAPPED COMPONENT SCORES BY SCHOOL ##
kable(wgts.esms, col.names = "ES Components' Weights")

A <- matrix(rep(1, nrow(gcomp.es0)*length(wgts.esms)), ncol = length(wgts.esms))
Awgts <- t(t(A)*wgts.esms) ## VERIFY THAT THE WEIGHTS ARE CORRECTLY APPLIED TO EACH COMPONENT (***NOTE: 'wgts.esms' DEFINED IN 'zmeta.R'***) ##
colnames(Awgts) <- names(wgts.esms)
# kable(Rmsmm(Awgts)[, c("Min", "Max", "N_Unique")], caption = "QC - Schema for Final Weight Application to Component Scores")

gcomp.est0 <- t(gcomp.es0[, names(wgts.esms)])
# gcomp.est0
gcomp.est1 <- gcomp.est0*Runname(wgts.esms)
# gcomp.est1
gcomp.est <- t(gcomp.est1)
# gcomp.est
colnames(gcomp.est) <- paste0(names(gcomp.es0[, names(wgts.esms)]), ".wgt")
# gcomp.est
gcomp.es <- data.frame(gcomp.es0, gcomp.est)

ccrpicols.es <- paste0(names(wgts.esms), ".wgt")
gcomp.es[, ccrpicols.es] <- apply(gcomp.es[, ccrpicols.es], 2, Rna) %>% as.data.frame()
gcomp.es$CCRPI <- apply(gcomp.es[, ccrpicols.es], 1, sum)
gcomp.es$CCRPI <- ifelse(gcomp.es$CG.wgt == 0, round(gcomp.es$CCRPI/0.85, 1), gcomp.es$CCRPI)
gcomp.es$CCRPI <- round(gcomp.es$CCRPI, 1) ## ROUND CCRPI SINGLE SCORE TO 1 DEC. PT., PER 2018 CALC. GUIDE [IMPLEMENTED ON 2018-06-03] ##
gcomp.es$CCRPI <- ifelse(gcomp.es$CCRPI > 100, 100, gcomp.es$CCRPI) ## CAP CCRPI SINGLE SCORE AT 100 ##
gcomp.es$CCRPI <- ifelse(gcomp.es$school.id %in% c("1319"), NA, gcomp.es$CCRPI) ## SET FAST ES FINAL CCRPI SCORE EST. TO NA DUE TO NON-STANDARD GRADEBAND STRUCTURE (IMPLEMENTED ON 20180618) ##

names(gcomp.es[, ccrpicols.es]) <- c("ContentMastery", "Progress", "ClosingGaps", "Readiness")
gcomp.es <- merge(fcs_schools[, .(school.id, school.name)], gcomp.es)
gcomp.es <- gcomp.es[!duplicated(gcomp.es), ]
gcomp.es <- gcomp.es[order(gcomp.es$school.name), ]

kable(gcomp.es, caption = "Weighted Component Scores \\& Estimated CCRPI Single Score",
      row.names = FALSE, format.args = list(digits = 1, nsmall = 1)) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
#'
#' \newpage
#' ### 2017 Actual vs. 2018 CCRPI Single Score Estimate
#'
# names(wgts.esms) <- c("Content Mastery", "Progress", "Closing Gaps", "Readiness")
gcomp.es$school.id <- as.character(gcomp.es$school.id)
comp.es <- merge(gcomp.es, dat17[, c("school.id", "single.score")],
                 all.x = TRUE, all.y = FALSE)

comp.es$school.name <- ifelse(comp.es$school.id == "406", "Amana Charter Elementary School", as.character(comp.es$school.name))
comp.es$school.name <- ifelse(comp.es$school.id == "215", "Chattahoochee Hills Elementary School", as.character(comp.es$school.name))
comp.es$school.name <- ifelse(comp.es$school.id == "1319", "FAST Academy Elementary School", as.character(comp.es$school.name))
comp.es$school.name <- ifelse(comp.es$school.id == "204", "KIPP South Fulton Academy Elementary School", as.character(comp.es$school.name))
comp.es <- comp.es[!duplicated(comp.es$school.id), ] %>% droplevels()

comp.es <- plyr::rename(comp.es, c("school.name" = "school", "CCRPI" = "ccrpi.new", "single.score" = "ccrpi.old")) #, "challenge.points" = "chpt")) ## DEPRECATED CHALLENGE POINTS ON 20180607 ##
comp.es <- comp.es[order(comp.es$school), ]
comp.es$diff <- comp.es$ccrpi.new - comp.es$ccrpi.old

# comp.es <- na.omit(comp.es)
kable(comp.es[, c("school", "ccrpi.new", "ccrpi.old", "diff")], caption = "Elementary Schools CCRPI Single Scores: 2018 Simulated vs. 2017 Actual", row.names = FALSE, format.args = list(digits = 1, nsmall = 1)) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

xlsx::write.xlsx(comp.es, "CCRPI_ES-20180911-01.xlsx", row.names = FALSE)
#'
#' \newpage-
#'
#' # Middle Schools' Component & Indicator Scores
#'
#' -----
#'
#' ## Content Mastery (CM)
#'
#+ echo=FALSE
vars.cm <- c("school.id", "school.name", "AchPts_Wgtd.ELA.cy", "AchPts_Wgtd.MATH.cy", "AchPts_Wgtd.SCI.cy", "AchPts_Wgtd.SS.cy", "cm.cy")
kable(cm.msm[, .SD, .SDcols = c(vars.cm)],
      col.names = c("School ID", "School Name",
                    "ELA",
                    "Math",
                    "Science",
                    "Social Studies",
                    "CM Component Score"), longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
#'
#' \newpage
#' ## Progress (PRG)
#'
#+ echo=FALSE
kable(prg.ms[, c("school.id", "school.name", "prg.elaw", "prg.mathw", "prg.elpw", "prg.score")],
      col.names = c("School ID", "School Name", "ELA Points",
                    "Math", "ELP", "Progress Component Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

#'
#' \newpage
#' ## Closing Gaps (CG)
#'
#+ echo=FALSE
vars.cgc1 <- c("school.name",
               "CG_ELA", "N.Flags.ELA",
               "CG_MATH", "N.Flags.Math",
               "CG_Science", "N.Flags.Science",
               "CG_Soc.Studies", "N.Flags.Soc.Studies")
vars.cgc2 <- c("school.name", "N.Points.Total", "N.Flags.Total", "CG.SCORE")
kable(cgc.ms[, .SD, .SDcols = c(vars.cgc1)],
      col.names = c("School",
                    "ELA Pts", "ELA Flags",
                    "Math Pts", "Math Flags",
                    "Sci. Pts", "Sci. Flags",
                    "Soc. Pts", "Soc. Flags")) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
kable(cgc.ms[, .SD, .SDcols = c(vars.cgc2)],
      col.names = c("School", "Tot. Pts", "Tot. Flags", "CG Component Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

#'
#' \newpage
#' ## Readiness (RDY)
#'
#+ echo=FALSE
kable(ms.c1[, c("school.name", "pts.literacy", "pts.att", "pts.btc", "sum_pts")],
      col.names = c("School",
                    "Literacy",
                    "Attendance",
                    "BTC",
                    "Readiness Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))
#'
#' \newpage
#' ## Middle Schools' Composite CCRPI Single Scores
#'
# GCOMP - MS ------------------------------------------------------
wgts.msms <- c(CM = 0.30, PRG = 0.35, CG = 0.15, RDY = 0.20) ## ES & MS ONLY ##
wgts.hs <- c(CM = 0.30, PRG = 0.30, CG = 0.10, RDY = 0.15, GRAD = 0.15) ## HS ONLY ##


ms.c1$school.id <- as.integer(ms.c1$school.id)
ms.c1 <- ms.c1[!duplicated(ms.c1$school.id), ]
gcomp.ms0 <- Reduce(function(x, y) merge(x, y, all = TRUE),
                    list(cm.msm[, c("school.id", "cm.cy")],
                         prg.ms[, c("school.id", "prg.score")],
                         cgc.ms[, c("school.id", "CG.SCORE")],
                         ms.c1[, c("school.id", "sum_pts")]))
names(gcomp.ms0) <- c("school.id", names(wgts.msms))
gcomp.ms0 <- as.data.frame(gcomp.ms0)

## ROUND ALL COMPONENTS' SCORES TO 1 DEC. PT. (PER 2018 CALC. GUIDES [IMPLEMENTED ON 2018-06-04]) ##
gcomp.ms0[, names(wgts.msms)] <- apply(gcomp.ms0[, names(wgts.msms)], 2, round, digits = 1)

## CAP ALL (UNWEIGHTED) COMPONENTS' SCORES AT 100PTS ##
gcomp.ms0[, names(wgts.msms)] <- apply(
  gcomp.ms0[, names(wgts.msms)], 2, function(x) ifelse(x > 100, 100, x))

## APPLY CCRPI COMPONENTS' WEIGHTS TO CAPPED COMPONENT SCORES BY SCHOOL ##
kable(wgts.msms, col.names = "MS Components' Weights")

A <- matrix(rep(1, nrow(gcomp.ms0)*length(wgts.esms)), ncol = length(wgts.esms))
Awgts <- t(t(A)*wgts.msms) ## VERIFY THAT THE WEIGHTS ARE CORRECTLY APPLIED TO EACH COMPONENT (***NOTE: 'wgts.msms' DEFINED IN 'zmeta.R'***) ##
colnames(Awgts) <- names(wgts.msms)
# kable(Rmsmm(Awgts)[, c("Min", "Max", "N_Unique")], caption = "QC - Schema for Final Weight Application to Component Scores")

gcomp.mst0 <- t(gcomp.ms0[, names(wgts.msms)])
# gcomp.mst0
gcomp.mst1 <- gcomp.mst0*Runname(wgts.msms)
# gcomp.mst1
gcomp.mst <- t(gcomp.mst1)
# gcomp.mst
colnames(gcomp.mst) <- paste0(names(gcomp.ms0[, names(wgts.msms)]), ".wgt")
# gcomp.mst
gcomp.ms <- data.frame(gcomp.ms0, gcomp.mst)

ccrpicols.ms <- paste0(names(wgts.msms), ".wgt")
gcomp.ms[, ccrpicols.ms] <- apply(gcomp.ms[, ccrpicols.ms], 2, Rna) %>% as.data.frame()
gcomp.ms$CCRPI <- apply(gcomp.ms[, ccrpicols.ms], 1, sum)
gcomp.ms$CCRPI <- ifelse(gcomp.ms$CG.wgt == 0, round(gcomp.ms$CCRPI/0.85, 1), gcomp.ms$CCRPI)
gcomp.ms$CCRPI <- round(gcomp.ms$CCRPI, 1) ## ROUND CCRPI SINGLE SCORE TO 1 DEC. PT., PER 2018 CALC. GUIDE [IMPLEMENTED ON 2018-06-03] ##
gcomp.ms$CCRPI <- ifelse(gcomp.ms$CCRPI > 100, 100, gcomp.ms$CCRPI) ## CAP CCRPI SINGLE SCORE AT 100 ##
gcomp.ms$CCRPI <- ifelse(gcomp.ms$school.id %in% c("1319"), NA, gcomp.ms$CCRPI) ## SET FAST ES FINAL CCRPI SCORE EST. TO NA DUE TO NON-STANDARD GRADEBAND STRUCTURE (IMPLEMENTED ON 20180618) ##

names(gcomp.ms[, ccrpicols.ms]) <- c("ContentMastery", "Progress", "ClosingGaps", "Readiness")
gcomp.ms <- merge(fcs_schools[, .(school.id, school.name)], gcomp.ms)
gcomp.ms <- gcomp.ms[!duplicated(gcomp.ms), ]
gcomp.ms <- gcomp.ms[order(gcomp.ms$school.name), ]

kable(gcomp.ms, caption = "Weighted Component Scores \\& Estimated CCRPI Single Score",
      row.names = FALSE, format.args = list(digits = 1, nsmall = 1)) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
#'
#' \newpage
#' ### 2017 Actual vs. New CCRPI Single Score Estimate
#'
# names(wgts.msms) <- c("Content Mastery", "Progress", "Closing Gaps", "Readiness")
gcomp.ms$school.id <- as.character(gcomp.ms$school.id)
comp.ms <- merge(gcomp.ms, dat17[, c("school.id", "single.score")],
                 all.x = TRUE, all.y = FALSE)

comp.ms$school.name <- ifelse(comp.ms$school.id == "406", "Amana Charter Middle School", as.character(comp.ms$school.name))
comp.ms$school.name <- ifelse(comp.ms$school.id == "215", "Chattahoochee Hills Middle School", as.character(comp.ms$school.name))
comp.ms$school.name <- ifelse(comp.ms$school.id == "1319", "FAST Academy Middle School", as.character(comp.ms$school.name))
comp.ms$school.name <- ifelse(comp.ms$school.id == "204", "KIPP South Fulton Academy Middle School", as.character(comp.ms$school.name))
comp.ms <- comp.ms[!duplicated(comp.ms$school.id), ] %>% droplevels()

comp.ms <- plyr::rename(comp.ms, c("school.name" = "school", "CCRPI" = "ccrpi.new", "single.score" = "ccrpi.old")) #, "challenge.points" = "chpt")) ## DEPRECATED CHALLENGE POINTS ON 20180607 ##
comp.ms <- comp.ms[order(comp.ms$school), ]
comp.ms$diff <- comp.ms$ccrpi.new - comp.ms$ccrpi.old

# comp.ms <- na.omit(comp.ms)
kable(comp.ms[, c("school", "ccrpi.new", "ccrpi.old", "diff")],
      caption = "Middle Schools CCRPI Single Scores: 2018 Simulated vs. 2017 Actual",
      row.names = FALSE, format.args = list(digits = 1, nsmall = 1)) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

xlsx::write.xlsx(comp.ms, "CCRPI_MS-20180911-01.xlsx", row.names = FALSE)


#'
#' \newpage
#'
#' # High Schools' Components & Indicators Scores
#'
#' -----
#'
#' ## Content Mastery (CM)
#'
#+ echo=FALSE
vars.cm <- c("school.id", "school.name", "AchPts_Wgtd.ELA.cy", "AchPts_Wgtd.MATH.cy", "AchPts_Wgtd.SCI.cy", "AchPts_Wgtd.SS.cy", "cm.cy")
kable(cm.hsm[, .SD, .SDcols = c(vars.cm)],
      col.names = c("School ID", "School Name",
                    "ELA",
                    "Math",
                    "Science",
                    "Social Studies",
                    "CM Component Score"), longtable = TRUE) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
#'
#' \newpage
#' ## Progress (PRG)
#'
#+ echo=FALSE
kable(prg.hs[, c("school.id", "school.name", "prg.elaw", "prg.mathw", "prg.elpw", "prg.score")],
      col.names = c("School ID", "School Name", "ELA Points",
                    "Math", "ELP", "Progress Component Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

#'
#' \newpage
#' ## Closing Gaps (CG)
#'
#+ echo=FALSE
vars.cgc1 <- c("school.name",
               "CG_ELA", "N.Flags.ELA",
               "CG_MATH", "N.Flags.Math",
               "CG_Science", "N.Flags.Science",
               "CG_Soc.Studies", "N.Flags.Soc.Studies")
vars.cgc2 <- c("school.name", "N.Points.Total", "N.Flags.Total", "CG.SCORE")
kable(cgc.hs[, .SD, .SDcols = c(vars.cgc1)],
      col.names = c("School",
                    "ELA Pts", "ELA Flags",
                    "Math Pts", "Math Flags",
                    "Sci. Pts", "Sci. Flags",
                    "Soc. Pts", "Soc. Flags")) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
kable(cgc.hs[, .SD, .SDcols = c(vars.cgc2)],
      col.names = c("School", "Tot. Pts", "Tot. Flags", "CG Component Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))
#'
#' \newpage
#' ## Readiness (RDY)
#'
#+ echo=FALSE
kable(hs.c1[, c("school.name", "pts.literacy", "pts.att", "pts.ae", "pts.pc", "pts.ccr", "sum_pts")],
      col.names = c("School",
                    "Literacy",
                    "Attendance",
                    "AE",
                    "PC",
                    "CCR",
                    "Readiness Score")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))
#'
#' \newpage
#' ## Graduation Rate
#'
#+ echo=FALSE
kable(grd[, c("school.name", "pts.grad")],
      col.names = c("School Name", "Graduation Rate Points")) %>%
  kable_styling(latex_options = c("scale_down", "striped"))
#'
#' \newpage
#'
#' ## High Schools' Composite CCRPI Single Scores
#'
# GCOMP - HS ------------------------------------------------------

wgts.hs <- c(CM = 0.30, PRG = 0.30, CG = 0.10, RDY = 0.15, GRAD = 0.15) ## HS ONLY ##

hs.c1$school.id <- as.integer(hs.c1$school.id)
gcomp.hs0 <- Reduce(function(x, y) merge(x, y, all = TRUE),
                    list(cm.hsm[, c("school.id", "cm.cy")],
                         prg.hs[, c("school.id", "prg.score")],
                         cgc.hs[, c("school.id", "CG.SCORE")],
                         hs.c1[, c("school.id", "sum_pts")],
                         grd[, c("school.id", "pts.grad")]
                         ))
names(gcomp.hs0) <- c("school.id", names(wgts.hs))
gcomp.hs0 <- as.data.frame(gcomp.hs0)

## ROUND ALL COMPONENTS' SCORES TO 1 DEC. PT. (PER 2018 CALC. GUIDES [IMPLEMENTED ON 2018-06-04]) ##
gcomp.hs0[, names(wgts.hs)] <- apply(gcomp.hs0[, names(wgts.hs)], 2, round, digits = 1)

## CAP ALL (UNWEIGHTED) COMPONENTS' SCORES AT 100PTS ##
gcomp.hs0[, names(wgts.hs)] <- apply(
  gcomp.hs0[, names(wgts.hs)], 2, function(x) ifelse(x > 100, 100, x))

## APPLY CCRPI COMPONENTS' WEIGHTS TO CAPPED COMPONENT SCORES BY SCHOOL ##
kable(wgts.hs, col.names = "HS Components' Weights") %>%
  kable_styling(latex_options = c("scale_down", "striped"))

A <- matrix(rep(1, nrow(gcomp.hs0)*length(wgts.hs)), ncol = length(wgts.hs))
Awgts <- t(t(A)*wgts.hs) ## VERIFY THAT THE WEIGHTS ARE CORRECTLY APPLIED TO EACH COMPONENT (***NOTE: 'wgts.hs' DEFINED IN 'zmeta.R'***) ##
colnames(Awgts) <- names(wgts.hs)
# kable(Rmsmm(Awgts)[, c("Min", "Max", "N_Unique")], caption = "QC - Schema for Final Weight Application to Component Scores")

gcomp.hst0 <- t(gcomp.hs0[, names(wgts.hs)])
# gcomp.hst0
gcomp.hst1 <- gcomp.hst0*Runname(wgts.hs)
# gcomp.hst1
gcomp.hst <- t(gcomp.hst1)
# gcomp.hst
colnames(gcomp.hst) <- paste0(names(gcomp.hs0[, names(wgts.hs)]), ".wgt")
# gcomp.hst
gcomp.hs <- data.frame(gcomp.hs0, gcomp.hst)

ccrpicols.hs <- paste0(names(wgts.hs), ".wgt")
gcomp.hs[, ccrpicols.hs] <- apply(gcomp.hs[, ccrpicols.hs], 2, Rna) %>% as.data.frame()
gcomp.hs$CCRPI <- apply(gcomp.hs[, ccrpicols.hs], 1, sum)
gcomp.hs$CCRPI <- ifelse(gcomp.hs$CG.wgt == 0, round(gcomp.hs$CCRPI/0.85, 1), gcomp.hs$CCRPI)
gcomp.hs$CCRPI <- round(gcomp.hs$CCRPI, 1) ## ROUND CCRPI SINGLE SCORE TO 1 DEC. PT., PER 2018 CALC. GUIDE [IMPLEMENTED ON 2018-06-03] ##
gcomp.hs$CCRPI <- ifelse(gcomp.hs$CCRPI > 100, 100, gcomp.hs$CCRPI) ## CAP CCRPI SINGLE SCORE AT 100 ##
gcomp.hs$CCRPI <- ifelse(gcomp.hs$school.id %in% c("1319"), NA, gcomp.hs$CCRPI) ## SET FAST ES FINAL CCRPI SCORE EST. TO NA DUE TO NON-STANDARD GRADEBAND STRUCTURE (IMPLEMENTED ON 20180618) ##

names(gcomp.hs[, ccrpicols.hs]) <- c("ContentMastery", "Progress", "ClosingGaps", "Readiness")
gcomp.hs <- merge(fcs_schools[, .(school.id, school.name)], gcomp.hs)
gcomp.hs <- gcomp.hs[!duplicated(gcomp.hs), ]
gcomp.hs <- gcomp.hs[order(gcomp.hs$school.name), ]

kable(gcomp.hs, caption = "Weighted Component Scores \\& Estimated CCRPI Single Score",
      row.names = FALSE, format.args = list(digits = 1, nsmall = 1)) %>%
  kable_styling(latex_options = c("scale_down", "striped")) %>%
  landscape()
#'
#'
#' \newpage
#' ### 2017 Actual vs. New CCRPI Single Score Estimate
#'
#'
# names(wgts.hs) <- c("Content Mastery", "Progress", "Closing Gaps", "Readiness")
gcomp.hs$school.id <- as.character(gcomp.hs$school.id)
comp.hs <- merge(gcomp.hs, dat17[, c("school.id", "single.score")],
                 all.x = TRUE, all.y = FALSE)

comp.hs$school.name <- ifelse(comp.hs$school.id == "406", "Amana Charter High School", as.character(comp.hs$school.name))
comp.hs$school.name <- ifelse(comp.hs$school.id == "215", "Chattahoochee Hills High School", as.character(comp.hs$school.name))
comp.hs$school.name <- ifelse(comp.hs$school.id == "1319", "FAST Academy High School", as.character(comp.hs$school.name))
comp.hs$school.name <- ifelse(comp.hs$school.id == "204", "KIPP South Fulton Academy High School", as.character(comp.hs$school.name))
comp.hs <- comp.hs[!duplicated(comp.hs$school.id), ] %>% droplevels()

comp.hs <- plyr::rename(comp.hs, c("school.name" = "school", "CCRPI" = "ccrpi.new", "single.score" = "ccrpi.old")) #, "challenge.points" = "chpt")) ## DEPRECATED CHALLENGE POINTS ON 20180607 ##
comp.hs <- comp.hs[order(comp.hs$school), ]
comp.hs$diff <- comp.hs$ccrpi.new - comp.hs$ccrpi.old

# comp.hs <- na.omit(comp.hs)
kable(comp.hs[, c("school", "ccrpi.new", "ccrpi.old", "diff")], caption = "High Schools CCRPI Single Scores: 2018 Simulated vs. 2017 Actual", row.names = FALSE, format.args = list(digits = 1, nsmall = 1)) %>%
  kable_styling(latex_options = c("scale_down", "striped"))

xlsx::write.xlsx(comp.hs, "CCRPI_HS-20180911-01.xlsx", row.names = FALSE)
#'
#' \newpage
#'
#' <!-- # All FCS Schools' CCRPI Indicators, Components, and Single Score Estimates-->
#'
#+ gcomp_all

gcomp.esm <- merge(gcomp.es, comp.es[, c("school.id", "ccrpi.old", "diff")],
                   all.x = TRUE, all.y = FALSE)
gcomp.esm <- gcomp.esm[sort(gcomp.esm$school.name),
                     c("school.id", "school.name", "CM", "PRG", "CG", "RDY",
                       "CM.wgt", "PRG.wgt", "CG.wgt", "RDY.wgt", "CCRPI",
                       "ccrpi.old", "diff")]

gcomp.msm <- merge(gcomp.ms, comp.ms[, c("school.id",
                                        "ccrpi.old", "diff")],
                   all.x = TRUE, all.y = FALSE)
gcomp.msm <- gcomp.msm[sort(gcomp.msm$school.name),
         c("school.id", "school.name", "CM", "PRG", "CG", "RDY",
           "CM.wgt", "PRG.wgt", "CG.wgt", "RDY.wgt", "CCRPI",
           "ccrpi.old", "diff")]
gcomp.hsm <- merge(gcomp.hs, comp.hs[, c("school.id", "ccrpi.old", "diff")],
                   all.x = TRUE, all.y = FALSE)
gcomp.hsm <- gcomp.hsm[sort(gcomp.hsm$school.name),
                     c("school.id", "school.name", "CM", "PRG", "CG", "RDY",
                       "CM.wgt", "PRG.wgt", "CG.wgt", "RDY.wgt", "CCRPI",
                       "ccrpi.old", "diff",
                       "GRAD", "GRAD.wgt")]

gcomp.all <- rbindlist(list(gcomp.esm, gcomp.msm, gcomp.hsm), fill = TRUE)
setcolorder(gcomp.all, c("school.id", "school.name",
                         "CM", "PRG", "CG", "RDY",
                         "CM.wgt", "PRG.wgt", "CG.wgt", "RDY.wgt",
                         "GRAD", "GRAD.wgt",
                         "CCRPI", "ccrpi.old", "diff"))
setnames(gcomp.all, c("CCRPI", "ccrpi.old", "diff"),
         c("2018 CCRPI Est.", "2017 CCRPI Actual", "CCRPI Score Change"))

xlsx::write.xlsx(gcomp.all, "CCRPI_ALL-20180912.xlsx",
                 sheetName = "CCRPI-2018-EST", row.names = FALSE)


#'
#' # _**`R-Code`**_ Performance Tracking
#'
#+ code_performance
.END <- Sys.time()
.TOTTIME <- .END - .START
format(.START, "Start time (total) = %H:%M:%S")
format(.END, "End time (total) = %H:%M:%S")
ceiling(.TOTTIME)

