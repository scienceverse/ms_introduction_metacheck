
hilgard_download_info <- ODPHelper::download_hilgard()
dir.create("made")
saveRDS(object = hilgard_download_info, file = "made/hilgard_download_info.Rdata")
