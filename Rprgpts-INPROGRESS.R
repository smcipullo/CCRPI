#' Compute Student Growth Points (SGPs) for one subject area and across all grade levels at once (\code{DRAFT RD}).
#'
#' @export
Rprgpts <- function(x,
                    subject_var = "assessmentsubjectareacode",
                    subject,
                    fay_filter = TRUE,
                    fay_var = "fayparticipant",
                    fay_code = "Y",
                    assessment_type_var = "assessmenttypecode",
                    assessment_type_code = c("EOG", "EOC", "GAA"),
                    grade_cluster_var = "gradecluster",
                    grade_cluster_code = c("E", "M", "H"),
                    school_id_var = "school.id",
                    stu_id_var = "gtid",
                    pts_var = "x2018.points") {
  ## FILTER ON SPECIFIED SUBJECT AREA ##
  x0 <- x[x[[subject_var]] == subject, ] %>% droplevels()
  ## INCLUDE ONLY VALID ASSESSMENT TYPES ##
  x0 <- x0[x0[[assessment_type_var]] %in% assessment_type_code, , drop = FALSE] %>% droplevels()
  # x0 <- x0[x0[[grade_cluster_var]] == grade_cluster_code, ] %>% droplevels()
  # x0 <- merge(fcs_schools, x0) %>% droplevels()
  xnew <- x0[!duplicated(x0), ]

  ## FILTER ON FAY.PARTICIPANT IF APPLICABLE ##
  if (fay_filter == TRUE) {
    xnew <- xnew[xnew[[fay_var]] == fay_code, , drop = FALSE] %>% droplevels()
  }
  ## SUBSET TO INCLUDE ID COLUMNS AND SPECIFIED POINTS COLUMN ##
  ids <- c(school_id_var, stu_id_var)
  xpts <- xnew[, c(ids, grade_cluster_var, subject_var, pts_var)]
  names(xpts) <- c(ids, grade_cluster_var, subject_var, "pts")
  xpts$pts <- factor(xpts$pts, levels = sort(unique(x[[pts_var]])), ordered = TRUE)
  xpts <- xpts[!duplicated(xpts), ]
  xptsw <- reshape(xpts, v.names = c("pts"), idvar = c(stu_id_var, grade_cluster_var, subject_var), timevar = school_id_var, direction = "wide")

  xpts_n0 <- sapply(xptsw[, -1:-3], table) %>% t()

  xpts_n1 <- data.frame(school_id_var = gsub("pts.", "", rownames(xpts_n0)), xpts_n0)
  names(xpts_n1)[1] <- school_id_var
  # xpts_n1 <- merge(fcs_schools, xpts_n1)
  xpts_n <- xpts_n1[!duplicated(xpts_n1), ]
  xpts_n$n <- apply(xpts_n0, 1, sum)
  xpts_p <- data.frame(xpts_n[, 1:2],
                       X0 = round((xpts_n$X0/xpts_n$n)*100, 2),
                       X0.5 = round((xpts_n$X0.5/xpts_n$n)*100, 2),
                       X1 = round((xpts_n$X1/xpts_n$n)*100, 2),
                       X1.5 = round((xpts_n$X1.5/xpts_n$n)*100, 2)
                       )
  xpts_p1 <- data.frame(xpts_n[, 1:2],
                        apply(format(round((xpts_n0/xpts_n$n)*100, 2), nsmall = 2),
                              2, Ras.percent))
  return(list(xpts_n = xpts_n, xpts_p = xpts_p, xpts_p1 = xpts_p1))
}

#' @export
Rprg <- function(xpts_n,
                 xpts_p,
                 school_id_var = "school.id",
                 wgts = c(0, 0.5, 1, 1.5)) {
  xpts_ptr0 <- t(xpts_p[, c(school_id_var, grep("[XL]\\d", names(xpts_p), value = TRUE))])
  colnames(xpts_ptr0) <- xpts_ptr0[1, ]
  xpts_ptr01 <- xpts_ptr0[-1, ]
  xpts_ptr <- apply(xpts_ptr01, 2, as.numeric)
  xpts_ptr1 <- xpts_ptr*wgts
  xpts_pts <- t(xpts_ptr1)
  prg.subj0 <- data.frame(school_id_var = rownames(xpts_pts), prg.subj = apply(xpts_pts, 1, sum))
  names(prg.subj0)[1] <- school_id_var
  # prg.subj1 <- merge(xpts_n[, c(school_id_var, "n")], prg.subj0)
  # prg.subj1 <- merge(fcs_schools, prg.subj0)
  prg.subj <- prg.subj0[!duplicated(prg.subj0), ]
  prg.subj[[school_id_var]] <- as.numeric(as.character(prg.subj[[school_id_var]]))
  return(list(xpts_pts = xpts_pts, prg.subj = prg.subj))
}
