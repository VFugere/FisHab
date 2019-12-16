# script to load a microsoft access database,
# unlist tables as individual data frames, and 
# export as an excel file with multiple sheets

# for this to work on a mac, you need the mdbtools program, 
# which can be installed with homebrew:
# http://macappstore.org/mdbtools/

library(Hmisc)
library(writexl)

p <- '~/Desktop/'
accdbfilename <- 'poissons.accdb'

#####

accdbconcat <- paste0(p, accdbfilename)
accdbexportp <- paste0(p, accdbfilename, '.xlsx')
accdb <- mdb.get(accdbconcat)
accdbnames <- data.frame(mdb.get(accdbconcat, tables = TRUE))
list2env(accdb, .GlobalEnv)
write_xlsx(accdb, accdbexportp)