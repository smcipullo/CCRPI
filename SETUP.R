
# BASE OPTIONS -----------------------------------------------------------------

options(
    stringsAsFactors = FALSE,
    prompt = "> ",
    continue = "... ",
    width = 75,
    scipen = 10,
    digits = 3,
    zero.print = ".",
    str = list(strict.width = "cut", digits.d = 2, vec.len = 3),
    citation_format = "pandoc",
    formatR.arrow = TRUE,
    formatR.brace.newline = TRUE,
    knitr.table.format = "pandoc",
    knitr.kable.NA = "-",
    xtable.floating = FALSE,
    xtable.comment = FALSE,
    xtable.booktabs = TRUE,
    xtable.caption.placement = "top",
    xtable.caption.width = "\\linewidth",
    xtable.table.placement = "hb",
    xtable.floating = FALSE,
    DT.fillContainer = FALSE,
    DT.options = list(pageLength = 200,
                      language = list(search = "Filter:"))
)


# APIs -------------------------------------------------------------------------

.apis <- list(
    googlemaps = "AIzaSyAWPniuPT5oOBw6ZrP7AHOX0ePXVQooTqs", ## REGISTERED UNDER 'FCS-Geocoding' GOOGLE API PROGJECT ##
    plotly = c("eccriley", "Dh4IpRKIxmt9qtwYR4zj"),
    mapbox = "sk.eyJ1IjoiZWNjcmlsZXkiLCJhIjoiY2pnbTkwazRoMWV6NjJ3bjBucjBvNXI3biJ9.COEENQmYZ9jntH37S2Zl0Q"
)


## SET API ENVIRONMENTS ===============================================

Sys.setenv("plotly_username"=.apis$plotly[1])
Sys.setenv("plotly_api_key"=.apis$plotly[2])
Sys.setenv('MAPBOX_TOKEN' = .apis$mapbox)
# .api_googlemaps <- readLines("C:/Users/plluiou/cipullosm/rstudio/auxDocs/api-google-geocoding.txt")


# LOAD *MY* DEFAULT PACKAGES ---------------------------------------------------

.auto.loads <- c("Riley", "devtools", "data.table", "rmarkdown", "knitr", "tufte", "formatR", "pander", "scales", "magrittr")
.sshhh <- function(a.package) {
    suppressWarnings(suppressPackageStartupMessages(
        library(a.package, character.only = TRUE)))
}
invisible(sapply(.auto.loads, .sshhh))
.unpkg <- function() {
    loaded <- paste0("package:", (.packages()))
    sapply(loaded, detach, character.only = TRUE)
}

## RECREATE PIPE() FUN FROM THE 'MAGRITTR' (TIDYVERSE) PKG TO AVOID LOADING MAGRITTR ET AL ##
# `%>%` <- magrittr:::pipe()


# PANDER OPTIONS ---------------------------------------------------------------

panderOptions("digits", 3)
panderOptions("round", 3)
panderOptions("missing", "-")
panderOptions("keep.line.breaks", TRUE)
panderOptions("table.style", "multiline")
panderOptions("table.alignment.default", "left")
panderOptions("table.split.table", 160)
panderOptions("table.split.cells", 50)
panderOptions("table.continues", "")
panderOptions("p.copula", ", and ")


# MISC DATE-RELATED VARS -------------------------------------------------------

.today <- format(Sys.Date(), "%d %B %Y")
.origin.xls <- as.Date("1899-12-30")
.origin.r <- as.Date("1970-01-01")

# R'S DEFAULT PLOT MARGINS (FOR REFERENCE) -------------------------------------

.mar <- (c(5, 4, 4, 2) + 0.1) ## R'S DEFAULT 'mar' PLOT PARAMETER VALUE ##


# KNITR OPTIONS ----------------------------------------------------------------

knitr::opts_chunk$set(warning = FALSE, message = FALSE, comment = "#>",
                      highlight = TRUE, background = NA, size = "footnotesize",
                      cache = FALSE, autodep = TRUE,
                      fig.path = "graphics/rplot-", fig.width = 7,
                      fig.height = 7, fig.align = "center") # fig.showtext = TRUE,
# KNITR CHUNK HOOKS ------------------------------------------------------------

knitr::knit_hooks$set(Rplot = function(before, options, envir)
{
    if (before)
    {
        palette(pal_rye)
        par(bg = "transparent", font.main = 3, family = "serif", font.main = 3)
    }
})
knitr::knit_hooks$set(Rrule = function(before, options, envir)
{
    if (knitr:::is_latex_output())
    {
        if (before)
        {
            if (!is.null(options$Rrule))
            {
                "\\Rrule\n\n"
            }
        }
        else
        {
            if (!is.null(options$Rerule))
            {
                "\n\n\\Rerule"
            }
        }
    }
})
knitr::opts_hooks$set(echoRule = function(options)
{
    if (options$echo == FALSE)
    {
        options$Rrule <- NULL
        options$Rerule <- NULL
    }
    else
    {
        options$Rrule <- TRUE
        options$Rerule <- TRUE
    }
    options
})

knitr::knit_hooks$set(Rplot_whbg = function(before, options, envir)
{
    if (before)
    {
        palette(pal_rye)
        par(bg = "#ffffff", font.main = 3, family = "Serif")
    }
})

knitr::opts_chunk$set(Rplot = TRUE, Rplot_whbg = NULL, echoRule = TRUE)
knitr::knit_hooks$set(Rruleb = function(before, options, envir)
{
    if (knitr:::is_latex_output())
    {
        if (before)
        {
            "\\Rruleb\n\n"
        }
        else
        {
            "\n\n\\Reruleb"
        }
    }
})
knitr::opts_hooks$set(echoRuleb = function(options)
{
    if (options$echo == FALSE)
    {
        options$Rruleb <- NULL
    }
    else
    {
        options$Rruleb <- TRUE
    }
    options
})
knitr::opts_chunk$set(echoRuleb = NULL)

knitr::knit_hooks$set(fignos = function(options) {
    if (!is.null(options$fig.lab) & !knitr:::is_latex_output()) {
        knitr::knit_hooks$set(plot = function(x, options) {
            paste('![', options$fig.cap, '](',
                  opts_knit$get('base.url'), paste(x, collapse = '.'),
                  '){#fig:', options$fig.lab, "}",
                  sep = '')
        })
    }
})

