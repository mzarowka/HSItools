
core <- run_core()
#400, 465, 532, 550,570,590,615,630,650,659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670,671,690,730,790,845,900, 950, 1000
core <- terra::readRDS("/Volumes/Macintosh HD/Users/nicholas/Downloads/OLOF_OLOF_ROTOR4_LC1U_2B_2025-07-15_02-07-25/HSItools_core.rds")
standard_workflow(core)
