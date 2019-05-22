#Devtools workflow

library(devtools)
devtools::load_all()
devtools::document()
devtools::check_man()
devtools::test()
devtools::build_vignette()
devtools::build()
devtools::install()
