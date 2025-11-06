
xfun::pkg_attach2("osfr")

yousif_download_info <- ODPHelper::download_yousif()
dir.create("made")
saveRDS(object = yousif_download_info, file = "made/yousif_download_info.Rdata")
