### R code from vignette source 'latex-bpca.Rnw'

###################################################
### code chunk number 1: latex-bpca.Rnw:72-81
###################################################
library(bpca)
library(xtable)

bp <- bpca(iris[-5])

## The simplest possible
xtable(bp)

print(xtable(bp))


###################################################
### code chunk number 2: latex-bpca.Rnw:89-94
###################################################
## With caption and label
## It will use the methods print.xtable.bpca provided by the bpca package
xtable(bpca(iris[-5]),
       caption='Biplot of iris data (packages:datasets).',
       label='tbl_iris')


###################################################
### code chunk number 3: latex-bpca.Rnw:98-102
###################################################
## With caption and label
xtable(bpca(gabriel1971),
       caption='Biplot of gabriel1971 data (package:datasets).',
       label='tbl_gabriel')


###################################################
### code chunk number 4: latex-bpca.Rnw:110-123
###################################################
## With bold in the columns
bp_rock_x <- xtable(bpca(rock),
                    caption='Biplot of rock data (package:dtasets).',
                    label='tbl_rock')

bold <- function(x){
  paste('\\textbf{',
        x,
        '}')
}

print(bp_rock_x,
      sanitize.colnames.function = bold)


###################################################
### code chunk number 5: latex-bpca.Rnw:129-143
###################################################
## With italic in the rows
bp_USA_x <- xtable(bpca(USArrests),
                   caption='Biplot of USArrests data (package:datasets).',
                   label='tbl_USArrests')

italic <- function(x){
  paste('\\textit{',
        x,
        '}',
        sep='')
}

print(bp_USA_x,
      sanitize.rownames.function = italic)


###################################################
### code chunk number 6: latex-bpca.Rnw:150-175
###################################################
## Principal labels in portuguese
tbl_rock_x <- xtable(bpca(rock),
                     caption='Biplot of rock data (package:datasets).',
                     label='tbl_rock_2')

rownames(tbl_rock_x) <- gsub('Eigenvalues',
                             'Autovalores',
                             rownames(tbl_rock_x))

rownames(tbl_rock_x) <- gsub('Eigenvectors',
                             'Autovetores',
                             rownames(tbl_rock_x))

rownames(tbl_rock_x) <- gsub('Variance retained',
                             'Variância retida',
                             rownames(tbl_rock_x))

rownames(tbl_rock_x) <- gsub('Variance accumulated',
                             'Variância acumulada',
                             rownames(tbl_rock_x))

colnames(tbl_rock_x) <- c('CP1',
                          'CP2')

print(tbl_rock_x)


###################################################
### code chunk number 7: latex-bpca.Rnw:182-200
###################################################
## If you don't want to use the bpca formatting standard (method print.xtable.bpca),
## you can directly call the print.xtable function and format the table as you wish.
italic <- function(x){
  paste('\\textit{',
        x,
        '}',
        sep='')
}

print.xtable(xtable(bpca(rock),
             caption='Call directly the print.xtable function',
             label='tbl_directly'),
             sanitize.colnames.function=bold,
             sanitize.rownames.function=italic)

## To others formatations see:
## - ?xtable
## - ?print.xtable


