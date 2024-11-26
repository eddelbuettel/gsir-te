## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, 
                      eval = TRUE,
                      cache = TRUE,
                      fig.pos = 'H') #, knitr.table.format = 'latex')
library(data.table)
library(ggplot2)
library(knitr)
library(tinytable)
options(tinytable_theme_placement_latex_float = "H")
options(width=50)

# Making some aesthetic changes for this document
theme_set(theme_gray(base_size = 9))
update_geom_defaults("point", list(size = 0.5))
update_geom_defaults("boxplot", list(outlier.size = 0.5))

# Temporarily resetting the print limit
op <- options()
options(datatable.print.topn=3, width=50)

## ----RStudioScreenshot, out.width="3.4in", fig.show='hold', fig.cap="\\label{fig:rstudio}RStudio Screenshot with Console on the left and  Help tab in the bottom right", echo=FALSE----
include_graphics("figures/RStudio-Screenshot.png")

## ----helpttest, echo=TRUE, eval=FALSE------------------------------------
## help(mean)
## ?mean

## ----aproposShow, echo=TRUE, eval=TRUE-----------------------------------
apropos("mean")

## ----egttest, echo=2-----------------------------------------------------
options(prompt="> ")
example(mean)
options(prompt="R> ")

## ----continuation, echo=TRUE, eval=FALSE---------------------------------
## +

## ----createObjs----------------------------------------------------------
height <- 173
height

## ----caseSens1-----------------------------------------------------------
age <- 10
AgE <- 50

## ----caseSens2-----------------------------------------------------------
age
AgE

## ----semicolon-----------------------------------------------------------
Name <- "Leo"; Age <- 25; City <- "Lisbon"
Name; Age; City

## ----comments------------------------------------------------------------
# This comment line will be ignored when run.
City     # Text after "#" is ignored.

## ----calcBasic-----------------------------------------------------------
2 + 3           
(5*11)/4 - 7     
# ^ = "to the power of"
7^3 

## ----calcFunctions-------------------------------------------------------
# Square root
sqrt(2)          
# Round down to nearest integer
floor(8.6178)
# Round to 2 decimal places
round(8.6178, 2)

## ----assignBasic---------------------------------------------------------
roomLength <- 7.8
roomWidth <- 6.4
roomArea <- roomLength * roomWidth
roomArea

## ----assignText----------------------------------------------------------
Greeting <- "Hello World!"
Greeting

## ----cVector-------------------------------------------------------------
# A "numeric" vector
x1 <- c(26, 10, 4, 7, 41, 19)
x1
# A "character" vector of country names
x2 <- c("Peru", "Italy", "Cuba", "Ghana")  
x2

## ----repseq--------------------------------------------------------------
# Repeat vector (2, 6, 7, 4) three times
r1 <- rep(c(2, 6, 7, 4), times=3)
r1
# Vector from -2 to 3 incremented by half
s1 <- seq(from=-2, to=3, by=0.5)
s1

## ----operationsVecs------------------------------------------------------
x1 * 2
round(sqrt(x1*2.6), 2)

## ----MissingValues-------------------------------------------------------
x2 <- c(3, -7, NA, 5, 1, 1) 
x2
x3 <- c("Rat", NA, "Mouse", "Hamster")
x3

## ----lsrm----------------------------------------------------------------
ls()
rm(x, x1, x2, x3, xm, r1, s1, AgE, age)
ls()

## ----functionUsage-------------------------------------------------------
seq(from = 5, to = 8, by = 0.4)

## ----functionAlt---------------------------------------------------------
seq(from = 5, to = 8, length.out = 16)

## ----installation, eval=FALSE--------------------------------------------
## install.packages(c("data.table", "ggplot2"))

## ----helpCWdata, echo=TRUE, eval=FALSE-----------------------------------
## help("ChickWeight")

## ----writeCW, echo=FALSE, message=FALSE, warning=FALSE, eval=TRUE--------
## data.table already loaded above
CW <- data.table(ChickWeight)
if (!file.exists("ChickWeight.csv"))
    fwrite(CW[, .(Chick, Diet, Time, weight)],
           file="ChickWeight.csv")

## ----cwdt----------------------------------------------------------------
suppressMessages(library(data.table))  # tinyverse
cw <- fread("ChickWeight.csv")

## ----printcw-------------------------------------------------------------
cw

## ----cwsummaryActual, echo=3:4, eval=TRUE, result="asis"-----------------
str <- function(object, ...) {
    if (!is.data.frame(object)) {
        warning("str.data.frame() called with non-data.frame -- coercing to one.")
        object <- data.frame(object)
    }
    cl <- oldClass(object)
    cl <- cl[cl != "data.frame"]
    if (0 < length(cl)) 
        cat("Classes", paste(sQuote(cl), collapse = ", "), "and ")
    cat("'data.frame':\n  ", nrow(object), " obs. of  ", (p <- length(object)), 
        " variable", if (p != 1) "s", if (p > 0) ":", "\n", sep = "")
  #if (length(l <- list(...)) && any("give.length" == names(l))) 
  #  invisible(NextMethod("str", ...))
  #else invisible(NextMethod("str", give.length = FALSE, ...))
  utils:::str.default(object, vec.len=2, strict.width="wrap", ...)
}
options(digits=5,width=45)
str(cw)
summary(cw)
options(digits=6,width=50)

## ----emptyPlot, fig.width=1.74, fig.height=1.74, fig.show='hold', fig.align='center'----
# (Silently) load the plotting package
suppressMessages(library(ggplot2))
# An empty plot (the plot on the left)
ggplot(cw, aes(Time, weight))  
# With data (the plot on the right)
ggplot(cw, aes(Time, weight)) + geom_point() 

## ----addColourPlot, fig.height=2.0---------------------------------------
# Adding colour for diet
ggplot(cw, aes(Time,weight,colour=factor(Diet))) +
  geom_point() 

## ----cwfactor, echo=2:4--------------------------------------------------
options(digits=2)
cw[, Diet := factor(Diet)]  
cw[, Time := factor(Time)]
summary(cw) # notice the difference ?
options(digits=6)

## ----jitterPlot----------------------------------------------------------
# Adding jitter to the points
ggplot(cw, aes(Time, weight, colour=Diet)) +
  geom_point() +
  facet_wrap(~ Diet) +
  theme(legend.position = "bottom")

## ----meanlinesPlot, fig.height=2.0---------------------------------------
ggplot(cw, aes(Time, weight, 
               group=Diet, colour=Diet)) +
  stat_summary(fun.y="mean", geom="line") 

## ----boxPlot-------------------------------------------------------------
ggplot(cw, aes(Time, weight, colour=Diet)) +
  facet_wrap(~ Diet) +
  geom_boxplot() +
  theme(legend.position = "none") +
  ggtitle("Chick Weight over Time by Diet")

## ----finalPlot-----------------------------------------------------------
ggplot(cw, aes(Time, weight, group=Diet, 
                             colour=Diet)) +
  facet_wrap(~ Diet) +
  geom_jitter() +
  stat_summary(fun.y="mean", geom="line",
               colour="black") +
  theme(legend.position = "none") +
  ggtitle("Chick Weight over Time by Diet") + 
  xlab("Time (days)") +
  ylab("Weight (grams)")


## ----dtmutate------------------------------------------------------------
cw[, weightKg := weight/1000 ]    # add a column
cw
cw[, Diet := paste0("Diet_", Diet)] # mod col.
cw

## ----dtselect------------------------------------------------------------
# Keep variables Time, Diet and weightKg
cw[, .(Chick, Time, Diet, weightKg)]

## ----condmean------------------------------------------------------------
cw[, .(Mean=mean(weight),SDev=sd(weight)),by=Diet]

## ----dtrename------------------------------------------------------------
setnames(cw, c("Diet", "weight"),
         c("Group", "Weight"))
cw

## ----dtfilter------------------------------------------------------------
cw[ Time == 21 & Weight > 300, ]

## ----dtarrange-----------------------------------------------------------
cw[order(Weight), ]       # on the fly
setkey(cw, Chick, Time)   # setting a key
cw

## ----resetW, echo=FALSE--------------------------------------------------
#cw[, `:=`(weight=Weight, Diet=Group) ]        
cw <- fread("ChickWeight.csv")

## ----cwchained-----------------------------------------------------------
cw[Time %in% c(0,21),][           # i: select rows
  , Weight := weight][            # j: mutate
  , Group := factor(paste0("Diet_", Diet))][
  , .(Chick,Group,Time,Weight)][  # j: arrange
   order(Chick,Time)][            # i: order
  1:5,]                           # i: subset

## ----ctstats-------------------------------------------------------------
cw[, .(N    = .N,            # .N is nb per group
       Mean = mean(weight)), # compute mean
   by=.(Diet, Time)][        # group by Diet + Time
   1:5, ]                    # display rows 1 to 5

## ----dtSum, echo=2:3-----------------------------------------------------
options(digits=3)  	# tighter display here
cws <- cw[Time %in% c(0,21),
          .(N      = .N, 
            Mean   = mean(weight),
            SDev   = sd(weight),
            Median = median(weight),
            Min    = min(weight),
            Max    = max(weight)  ),
          by=.(Diet, Time)]
cws

## ----dtpretty------------------------------------------------------------
cws[, Mean_SD := paste0(format(Mean,digits=1),
                        " (",
                        format(SDev,digits=2),
                        ")")]
cws[, Range := paste(Min, "-", Max)]
prettySum <- cws[ , .(Diet, Time, N, Mean_SD, 
                      Median, Range)][
                 order(Diet, Time)]
prettySum

## ----dtprettytable-------------------------------------------------------
# library(tinytable)
prettySum |>
  tt(theme = "striped") |>
  style_tt(i = 0, bold = TRUE) |>
  format_tt(escape = TRUE)

## ----reset, include=FALSE------------------------------------------------
options(op)

