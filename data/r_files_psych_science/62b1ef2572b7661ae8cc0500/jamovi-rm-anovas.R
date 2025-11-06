jmv::anovaRM(
  data = data,
  rm = list(
    list(
      label="Laterality",
      levels=c("Contra", "Ipsi")),
    list(
      label="Task",
      levels=c("WM", "Att")),
    list(
      label="Set size",
      levels=c("Two", "Four"))),
  rmCells = list(
    list(
      measure="E1E2E3E4_contra_wm2",
      cell=c("Contra", "WM", "Two")),
    list(
      measure="E1E2E3E4_contra_wm4",
      cell=c("Contra", "WM", "Four")),
    list(
      measure="E1E2E3E4_contra_att2",
      cell=c("Contra", "Att", "Two")),
    list(
      measure="E1E2E3E4_contra_att4",
      cell=c("Contra", "Att", "Four")),
    list(
      measure="E1E2E3E4_ipsi_wm2",
      cell=c("Ipsi", "WM", "Two")),
    list(
      measure="E1E2E3E4_ipsi_wm4",
      cell=c("Ipsi", "WM", "Four")),
    list(
      measure="E1E2E3E4_ipsi_att2",
      cell=c("Ipsi", "Att", "Two")),
    list(
      measure="E1E2E3E4_ipsi_att4",
      cell=c("Ipsi", "Att", "Four"))),
  bs = Experiment_1_2_3_4,
  effectSize = "partEta",
  rmTerms = ~ Laterality + Task + `Set size` + Laterality:Task + Laterality:`Set size` + Task:`Set size` + Laterality:Task:`Set size`,
  bsTerms = ~ Experiment_1_2_3_4)