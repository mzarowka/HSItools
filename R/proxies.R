# A list of proxies and their spectral properties
# Where b == band (wavelength, nm); rabd == RABD, raba == RABA, ratio == band ratio, diff == band difference, deriv = derivative.

proxies <- list(
  # Relative absorption band depth = RABD
  rabd_b510 = list(proxy_type = "RABD", proxy_name = "RABD510", edges = c(590, 730), trough = 510, reference = "NA", interpretation = "Carotenoids-HSI"),
  rabd_b616 = list(proxy_type = "RABD", proxy_name = "RABD615", edges = c(590, 730), trough = 615, reference = "NA", interpretation = "Phycocyanin-HSI"),
  rabd_b640b655 = list(proxy_type = "RABD", proxy_name = "RABD640:655", edges = c(590, 730), trough = 647.5, reference = "NA", interpretation = "Tchl-aHSI"),
  rabd_b660 = list(proxy_type = "RABD", proxy_name = "RABD660", edges = c(590, 730), trough = 660, reference = "NA", interpretation = "Tchl-aHSI"),
  rabd_b660b670 = list(proxy_type = "RABD", proxy_name = "RABD660:670", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  rabd_b845 = list(proxy_type = "RABD", proxy_name = "RABD640:655", edges = c(790, 900), trough = 845, reference = "NA", interpretation = "Bphe-aHSI"),
  rabd_b845 = list(proxy_type = "RABD", proxy_name = "RABD640:655", edges = c(790, 900), trough = 845, reference = "NA", interpretation = "Bphe-aHSI"),
  rabd_b1660b1690_b1670 = list(proxy_type = "RABD", proxy_name = "RABD1660:1690/1670", edges = c(790, 900), trough = 845, reference = "NA", interpretation = "Terrestrial aromatic matter"),
  # Relative absorption band area = RABA
  raba_b650b700 = list(proxy_type = "RABA", proxy_name = "RABA660:670", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b600b760 = list(proxy_type = "RABA", proxy_name = "RABA660:670", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b590b730 = list(proxy_type = "RABA", proxy_name = "RABA660:670", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b650b750 = list(proxy_type = "RABA", proxy_name = "RABA660:670", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b650b700rmean = list(proxy_type = "RABA/Rmean", proxy_name = "RABA660:670/Rmean", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b600b760rmean = list(proxy_type = "RABA/Rmean", proxy_name = "RABA660:670/Rmean", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b590b730rmean = list(proxy_type = "RABA/Rmean", proxy_name = "RABA660:670/Rmean", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  raba_b650b750rmean = list(proxy_type = "RABA/Rmean", proxy_name = "RABA660:670/Rmean", edges = c(590, 730), trough = 665, reference = "NA", interpretation = "Tchl-aHSI"),
  # Band ratios
  ratio_b570b630 = list(proxy_type = "Band ratio", proxy_name = "R570/R630", edges = c(570, 630), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b590r690 = list(proxy_type = "Band ratio", proxy_name = "R590/R690", edges = c(590, 690), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b850b900 = list(proxy_type = "Band ratio", proxy_name = "R850/R900", edges = c(850, 900), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b950b970 = list(proxy_type = "Band ratio", proxy_name = "R950/R970", edges = c(950, 970), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b675b750 = list(proxy_type = "Band ratio", proxy_name = "R675/R750", edges = c(675, 750), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b645b675 = list(proxy_type = "Band ratio", proxy_name = "R645/R675", edges = c(645, 675), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b650b675 = list(proxy_type = "Band ratio", proxy_name = "R650/R675", edges = c(650, 675), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_b660b670 = list(proxy_type = "Band ratio", proxy_name = "R660/R670", edges = c(660, 670), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  ratio_blr750lr673 = list(proxy_type = "Band ratio", proxy_name = "log(R750)/log(R673)", edges = c(750, 673), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  # Band differences
  diff_b675b750 = list(proxy_type = "Band difference", proxy_name = "R570/R630", edges = c(675, 750), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  diff_b650b675 = list(proxy_type = "Band difference", proxy_name = "R570/R630", edges = c(650, 675), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  diff_b660b690 = list(proxy_type = "Band difference", proxy_name = "R570/R630", edges = c(660, 690), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  # Derivatives
  lambdaremp = list(proxy_type = "Derivative", proxy_name = "λREMP", edges = c(570, 630), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  deriv_b675 = list(proxy_type = "Derivative", proxy_name = "d675", edges = c(570, 630), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  deriv_b690 = list(proxy_type = "Derivative", proxy_name = "d690", edges = c(570, 630), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  deriv_b660b690 = list(proxy_type = "Derivative", proxy_name = "d660/d690", edges = c(570, 630), trough = NA, reference = "NA", interpretation = "Clay minerals, dust"),
  # Wavelengths and generals
  rmean = list(proxy_type = "general", proxy_name = "Rmean", edges = c(570, 630), trough = NA, reference = "NA", interpretation = "Clay minerals, dust")
)
