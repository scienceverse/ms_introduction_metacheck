# dataset with no exclusions
multisite_dataset_nonfiltered <- read_csv("dataset/multisite_complete_dataset_nonfiltered.csv")

# dataset with no task data excluded, but demog/attention exclusion criteria applied
multisite_dataset_filtered <- filter(multisite_dataset_nonfiltered, failed_checks < 2, hearing_diagnosis != "Yes")

# tbl of demographics data
multisite_dataset_demog <- multisite_dataset_filtered %>% 
  select(participant_identifier, site, gender, age, country, handedness, ses,
         alcohol_use, cigarette_use, cannabis_use, diagnosis, relative_diagnosis, hearing_diagnosis)

# tbl of dichotic listening data
multisite_dataset_dl <- multisite_dataset_filtered %>% 
  select(participant_identifier, dl_LE_sum, dl_RE_sum, dl_laterality_index,
         dl_LE_forcedleft_sum, dl_RE_forcedleft_sum, dl_forced_left_index,
         dl_LE_forcedright_sum, dl_RE_forcedright_sum, dl_forced_right_index,
         dl_nonforced_homonym, dl_forcedright_homonym_acc, dl_forcedleft_homonym_acc,
         dl_nf_corr_resp, dl_fl_corr_resp, dl_fr_corr_resp) %>% 
  filter(dl_nf_corr_resp >= 20, dl_fl_corr_resp >= 20, dl_fr_corr_resp >=20) %>% 
  filter(dl_nonforced_homonym >= 3, dl_forcedright_homonym_acc >=3, dl_forcedleft_homonym_acc >=3) %>% 
  filter(dl_laterality_index != 1, dl_laterality_index != -1,
         dl_forced_left_index != 1, dl_forced_left_index != -1,
         dl_forced_right_index != 1, dl_forced_right_index != -1)

# tbl of matrix reasoning data
multisite_dataset_mr <- multisite_dataset_filtered %>% 
  select(participant_identifier, mr_mean_rt, mr_too_quick, mr_too_slow,
         corr_too_quick, mr_score) %>% 
  filter(mr_too_quick < 4, mr_mean_rt >= 8000)

# tbl of source memory data
multisite_dataset_smt <- multisite_dataset_filtered %>% 
  select(participant_identifier, smt_hear_hear, smt_hear_imagine, smt_hear_new,
         smt_imagine_imagine, smt_imagine_hear, smt_imagine_new, smt_new_new,
         smt_new_hear, smt_new_imagine, smt_rm_acc, smt_ext_errors, smt_int_errors,
         smt_misattribution_bias, smt_oldnew_acc, smt_oldnew_sensitivity, smt_overall_acc) %>% 
  filter(smt_overall_acc != 100, smt_overall_acc > 33.33, smt_oldnew_acc > 50)

# tbl of digit span data
multisite_dataset_ds <- multisite_dataset_filtered %>% 
  select(participant_identifier, ds_mean_span, max_length) %>% 
  filter(ds_mean_span > 3, ds_mean_span < 12)

# tbl of signal detection data
multisite_dataset_sdt <- multisite_dataset_filtered %>% 
  select(participant_identifier, sdt_hits, sdt_fa, sdt_d, sdt_beta, sdt_c) %>% 
  filter(sdt_d > 0, sdt_hits > 10, sdt_hits - sdt_fa != 100)

# tbls for each questionnaire
multisite_dataset_LSHS <- multisite_dataset_filtered %>% 
  select(participant_identifier, LSHS_aud_vis, LSHS_daydream, LSHS_intrusive, LSHS_multisensory,
         LSHS_total_score, LSHS_voices, LSHS_missing, LSHS_mean_rt) %>% 
  filter(LSHS_mean_rt >= 1.5, LSHS_missing < 6)
multisite_dataset_CAPS <- multisite_dataset_filtered %>% 
  select(participant_identifier, CAPS_total, CAPS_distress, CAPS_distract,
         CAPS_frequency, CAPS_auditory, CAPS_missing, CAPS_mean_rt) %>% 
  filter(CAPS_mean_rt >= 1.5, CAPS_missing < 11)
multisite_dataset_SPQ <- multisite_dataset_filtered %>% 
  select(participant_identifier, SPQ_cp, SPQ_do, SPQ_ip, SPQ_total_score,
         SPQ_missing, SPQ_mean_rt) %>% 
  filter(SPQ_mean_rt >= 1.5, SPQ_missing < 11)
multisite_dataset_DASS <- multisite_dataset_filtered %>% 
  select(participant_identifier, DASS_anxiety, DASS_depression, DASS_stress,
         DASS_mean_rt, DASS_missing) %>% 
  filter(DASS_mean_rt >= 1.5, DASS_missing < 7)
multisite_dataset_ACES <- multisite_dataset_filtered %>% 
  select(participant_identifier, ACES_total_score, ACES_mean_rt, ACES_missing) %>% 
  filter(ACES_mean_rt >= 1.5, ACES_missing < 6)
multisite_dataset_attchecks <- multisite_dataset_filtered %>% 
  select(participant_identifier, att_check_CAPS, att_check_sport, att_check_memory,
         failed_checks, origin)

# join all tbls together, now with tasks individually filtered         
multisite_dataset <- multisite_dataset_demog %>% 
  full_join(multisite_dataset_dl, by = "participant_identifier") %>% 
  full_join(multisite_dataset_mr, by = "participant_identifier") %>% 
  full_join(multisite_dataset_smt, by = "participant_identifier") %>% 
  full_join(multisite_dataset_ds, by = "participant_identifier") %>% 
  full_join(multisite_dataset_sdt, by = "participant_identifier") %>% 
  full_join(multisite_dataset_LSHS, by = "participant_identifier") %>% 
  full_join(multisite_dataset_SPQ, by = "participant_identifier") %>% 
  full_join(multisite_dataset_CAPS, by = "participant_identifier") %>% 
  full_join(multisite_dataset_DASS, by = "participant_identifier") %>% 
  full_join(multisite_dataset_ACES, by = "participant_identifier") %>% 
  full_join(multisite_dataset_attchecks, by = "participant_identifier")

# replace non-answers to demographics with NA values
multisite_dataset <- multisite_dataset %>% 
  mutate(gender_bin = gender, handedness_bin = handedness, diagnosis_bin = diagnosis,
         relative_diagnosis_bin = relative_diagnosis, ses_tidy = ses)
multisite_dataset$gender_bin[multisite_dataset$gender_bin == "Prefer Not to Say"
                             | multisite_dataset$gender_bin == "Other"] <- NA
multisite_dataset$handedness_bin[multisite_dataset$handedness_bin == "Both"] <- "Left"
multisite_dataset$diagnosis_bin[multisite_dataset$diagnosis_bin == "Prefer not to say"] <- NA               
multisite_dataset$relative_diagnosis_bin[multisite_dataset$relative_diagnosis_bin == "Prefer not to say"] <- NA
multisite_dataset$ses_tidy[multisite_dataset$ses_tidy == 0] <- NA                   
