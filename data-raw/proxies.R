## code to prepare `proxies` dataset goes here

# a list of proxies and their spectral properties
# where b == band (wavelength, nm); rabd == rabd, raba == raba, ratio == band ratio, diff == band difference, deriv = derivative.

proxies <- list(

# Relative absorption band depth = RABD
  rabd_b510 = list(proxy_type = "rabd", proxy_name = "rabd510", edges = c(590, 730), trough = 510, reference = "na", interpretation = "carotenoids-hsi"),
  rabd_b616 = list(proxy_type = "rabd", proxy_name = "rabd615", edges = c(590, 730), trough = 615, reference = "na", interpretation = "phycocyanin-hsi"),
  rabd_b620 = list(proxy_type = "rabd", proxy_name = "rabd610620", edges = c(590, 640), trough = 615, reference = "von Gunten, L., Grosjean, M., Kamenik, C., Fujak, M., Urrutia, R., 2012. Calibrating biogeochemical and physical climate proxies from non-varved lake sediments with meteorological data: methods and case studies. J Paleolimnol 47, 583–600. https://doi.org/10.1007/s10933-012-9582-9
", interpretation = "albite"),
  rabd_b640b655 = list(proxy_type = "rabd", proxy_name = "rabd640655", edges = c(590, 730), trough = 647.5, reference = "na", interpretation = "tchl-ahsi"),
  rabd_b660 = list(proxy_type = "rabd", proxy_name = "rabd660", edges = c(590, 730), trough = 660, reference = "na", interpretation = "tchl-ahsi"),
  rabd_b660b670 = list(proxy_type = "rabd", proxy_name = "rabd660670", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-ahsi"),
  rabd_b845 = list(proxy_type = "rabd", proxy_name = "rabd640655", edges = c(790, 900), trough = 845, reference = "na", interpretation = "bphe-ahsi"),
  rabd_b1660b1690_b1670 = list(proxy_type = "rabd", proxy_name = "rabd16601690_1670", edges = c(790, 900), trough = 845, reference = "na", interpretation = "terrestrial aromatic matter"),

# Relative absorption band area = RABA
  raba_b650b700 = list(proxy_type = "raba", proxy_name = "raba660670", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b600b760 = list(proxy_type = "raba", proxy_name = "raba660670", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b590b730 = list(proxy_type = "raba", proxy_name = "raba660670", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b650b750 = list(proxy_type = "raba", proxy_name = "raba660670", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b650b700rmean = list(proxy_type = "raba/rmean", proxy_name = "raba660670_rmean", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b600b760rmean = list(proxy_type = "raba/rmean", proxy_name = "raba660670_rmean", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b590b730rmean = list(proxy_type = "raba/rmean", proxy_name = "raba660670_rmean", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),
  raba_b650b750rmean = list(proxy_type = "raba/rmean", proxy_name = "raba660670_rmean", edges = c(590, 730), trough = 665, reference = "na", interpretation = "tchl-a"),

# Band ratios
  ratio_b570b630 = list(proxy_type = "band ratio", proxy_name = "r570r630", edges = c(570, 630), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_b590r690 = list(proxy_type = "band ratio", proxy_name = "r590r690", edges = c(590, 690), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_b590r640 = list(proxy_type = "band ratio", proxy_name = "r590r640", edges = c(590, 640), trough = NA, reference = "von Gunten, L., Grosjean, M., Kamenik, C., Fujak, M., Urrutia, R., 2012. Calibrating biogeochemical and physical climate proxies from non-varved lake sediments with meteorological data: methods and case studies. J Paleolimnol 47, 583–600. https://doi.org/10.1007/s10933-012-9582-9", interpretation = "lithogenic content, speciﬁcally illite, chlorite and mica"),
  ratio_b645b675 = list(proxy_type = "band ratio", proxy_name = "r645r675", edges = c(645, 675), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_b660b670 = list(proxy_type = "band ratio", proxy_name = "r660r670", edges = c(660, 670), trough = NA, reference = "von Gunten, L., Grosjean, M., Kamenik, C., Fujak, M., Urrutia, R., 2012. Calibrating biogeochemical and physical climate proxies from non-varved lake sediments with meteorological data: methods and case studies. J Paleolimnol 47, 583–600. https://doi.org/10.1007/s10933-012-9582-9", interpretation = "degree of photopigment diagenesis"),
  ratio_b675b750 = list(proxy_type = "band ratio", proxy_name = "r675r750", edges = c(675, 750), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_b650b675 = list(proxy_type = "band ratio", proxy_name = "r650r675", edges = c(650, 675), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_b850b900 = list(proxy_type = "band ratio", proxy_name = "r850r900", edges = c(850, 900), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_b950b970 = list(proxy_type = "band ratio", proxy_name = "r950r970", edges = c(950, 970), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  ratio_blr750lr673 = list(proxy_type = "band ratio", proxy_name = "log(r750)_log(r673)", edges = c(750, 673), trough = NA, reference = "na", interpretation = "clay minerals, dust"),

# Band differences
  diff_b675b750 = list(proxy_type = "band difference", proxy_name = "d675d750", edges = c(675, 750), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  diff_b650b675 = list(proxy_type = "band difference", proxy_name = "d650d675", edges = c(650, 675), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  diff_b660b690 = list(proxy_type = "band difference", proxy_name = "d660d690", edges = c(660, 690), trough = NA, reference = "na", interpretation = "clay minerals, dust"),

# Derivatives
  lambdaremp = list(proxy_type = "derivative", proxy_name = "λremp", edges = c(570, 630), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  deriv_b675 = list(proxy_type = "derivative", proxy_name = "d675", edges = c(570, 630), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  deriv_b690 = list(proxy_type = "derivative", proxy_name = "d690", edges = c(570, 630), trough = NA, reference = "na", interpretation = "clay minerals, dust"),
  deriv_b660b690 = list(proxy_type = "derivative", proxy_name = "d660d690", edges = c(570, 630), trough = NA, reference = "na", interpretation = "clay minerals, dust"),

# Wavelengths and generals
  rmean = list(proxy_type = "general", proxy_name = "rmean", edges = c(570, 630), trough = NA, reference = "na", interpretation = "mean reflectance")
)

usethis::use_data(proxies, overwrite = TRUE)
