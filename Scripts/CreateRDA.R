librarian::shelf(readxl)

file = '/Users/maxm1/pCloudSync/Packages/Pplot/inst/TablesFixes.xlsx'

Essences        <- read_excel(file, sheet = 'Essences')
Cat             <- read_excel(file, sheet = 'Cat')
CodeEcologie    <- read_excel(file, sheet = 'Essences')
CodeDurete      <- read_excel(file, sheet = 'Essences')
CodeEcorce      <- read_excel(file, sheet = 'Essences')
CodeTypoArbres  <- read_excel(file, sheet = 'Essences')

usethis::use_data(Essences, overwrite = T)
