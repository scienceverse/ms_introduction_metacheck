source("utils.R")
library("corrplot")

liwc_filename = "../processed_data/liwc2015_scores.csv"
metadata_filename = "../processed_data/metadata_2024.csv"
varname_map_filename = "../processed_data/relabeling.csv"

#metadata
full_data = get_meta(metadata_filename)

#LIWC outputs
liwc_fulltext = get_liwc(liwc_filename, "fulltext")[,c('paper_id','log_WC')]
colnames(liwc_fulltext)<-c('paper_id','log_WC_fulltext')

liwc_study = get_liwc(liwc_filename, "study")[,c('study_id','log_WC')]
colnames(liwc_study)<-c('study_id','log_WC_study')

liwc_abstract = get_liwc(liwc_filename, "abstract")[,c('paper_id','log_WC')]
colnames(liwc_abstract)<-c('paper_id','log_WC_abstract')

full_data = full_data[liwc_study, on = "study_id"]
full_data = full_data[liwc_fulltext, on = "paper_id"]
full_data = merge(full_data, liwc_abstract, by = 'paper_id', all.x = TRUE)

#####Table S5a Summary Stats (continuous variables)

cont_columns <- c("power.o",
                  "p_value.o",
                  "effect_size.o",
                  "n.o",
                  "n_authors.o",
                  "authors_male.o",
                  "num_refs",
                  "Full Prof Ratio",
                  "num_tables",
                  "num_figures",
                  "num_studies",
                  "pub_year",
                  "citations",
                  "endprice",
                  "log_WC_study",
                  "log_WC_abstract",
                  "log_WC_fulltext",
                  "replicated")

cont_controls <- full_data[,..cont_columns]

cont_controls %>%
  mutate(pub_year = I(pub_year-2000),
         log_WC_abstract = exp(log_WC_abstract),
         log_WC_study = exp(log_WC_study),
         log_WC_fulltext = exp(log_WC_fulltext))%>%
  ungroup()%>%
  pivot_longer(!replicated, names_to = "name", values_to = "value")%>%
  group_by(name)%>%
  mutate(Total=mean(value,na.rm=TRUE))%>%
  ungroup()%>%
  group_by(name,replicated,Total)%>%
  summarise(mean=mean(value,na.rm=TRUE),
            Total = mean(Total))%>%
  pivot_wider(names_from = replicated,id_cols = c(name,Total),values_from = mean)%>%
  arrange(name)%>%
  mutate_if(is.numeric, round, 3)%>%
  relocate(Total,.after = `1`)%>%
  write.csv("../results/SM_tableS5a.csv")

#####Table S5b (binary variables)

binary_df = full_data[,list(replicated,
                            discipline,
                            effect_type,
                            us_lab.o,
                            subjects.o,
                            supp_material,
                            project_collapsed,
                            author_involvement)]%>%as.data.frame()

binary_df = binary_df %>% mutate(discipline = as.factor(discipline),
                                              effect_type = as.factor(effect_type),
                                              project_collapsed = as.factor(project_collapsed),
                                              subjects.o = as.factor(subjects.o))
binary_controls = model.matrix(replicated ~.,
                            data = binary_df,
                            contrasts.arg = lapply(binary_df[, sapply(binary_df, is.factor), drop = FALSE],
                                                   contrasts, contrasts = FALSE))[,-1]%>%as.data.frame()

binary_controls = cbind(binary_controls,full_data[,'replicated'])
binary_controls%>%
  ungroup()%>%
  pivot_longer(!replicated, names_to = "name", values_to = "value")%>%
  group_by(name)%>%
  mutate(Total=mean(value,na.rm=TRUE))%>%
  ungroup()%>%
  group_by(name,replicated,Total)%>%
  summarise(mean=mean(value,na.rm=TRUE),
            Total = mean(Total))%>%
  pivot_wider(names_from = replicated,id_cols = c(name,Total),values_from = mean)%>%
  arrange(name)%>%
  mutate_if(is.numeric, round, 3)%>%
  relocate(Total,.after = `1`)%>%
  write.csv("../results/SM_tableS5b.csv")


#####Table S5a, part 2 (keywords)

controls_mat = prepare_controls(full_data)

keyword_mat = model.matrix(~ 0 + keyword, full_data)
colnames(keyword_mat)<-stringr::str_replace_all(colnames(keyword_mat),"keyword","")
keyword_mat = cbind(full_data[,'replicated'], keyword_mat)
keyword_controls = keyword_mat %>% as.data.frame()

keyword_controls%>%
  ungroup()%>%
  pivot_longer(!replicated, names_to = "name", values_to = "value")%>%
  group_by(name)%>%
  mutate(Total=mean(value,na.rm=TRUE))%>%
  ungroup()%>%
  group_by(name,replicated,Total)%>%
  summarise(mean=mean(value,na.rm=TRUE),
            Total = mean(Total))%>%
  pivot_wider(names_from = replicated,id_cols = c(name,Total),values_from = mean)%>%
  arrange(name)%>%
  mutate_if(is.numeric, round, 3)%>%
  relocate(Total,.after = `1`)%>%
  write.csv("../results/SM_tableS5c.csv")


#####Table S6 Controls Correlations
controls_mat = prepare_controls(full_data)
varname_map = fread(varname_map_filename)
old_names <- controls_mat %>% cor(use = "pairwise.complete.obs") %>% round(3) %>% rownames()
new_names <- merge(data.table(old_names),varname_map,by.x = 'old_names',by.y='original',sort=FALSE)$new
corr_dat <- controls_mat %>% cor(use = "pairwise.complete.obs") %>%round(3)
rownames(corr_dat) <- new_names
colnames(corr_dat) <- new_names
corr_dat < - corr_dat[order(rownames(corr_dat)),]


pdf("../results/controls_correlations.pdf", width = 10, height = 10)
corr_dat%>%corrplot(method = 'color', is.corr = TRUE, type = 'lower', diag = FALSE,tl.pos = 'ld',cl.pos = 'n',tl.col = 'red',tl.cex = 0.7,number.font = 1,number.cex = 0.7,addCoef.col = 'black')
dev.off()

keywords_mat = prepare_keywords(full_data)
controls_mat2 = cbind(controls_mat,keywords_mat)
corr_dat2 <- controls_mat2%>%cor(use = "pairwise.complete.obs")%>%round(3)

pdf("../results/controls_correlations_with_keywords.pdf", width = 21, height = 21)
corr_dat2[,colnames(controls_mat)]%>%corrplot(method = 'color', is.corr = TRUE, type = 'lower', diag = FALSE,tl.pos = 'ld',cl.pos = 'n',tl.col = 'red',tl.cex = 0.7,number.font = 1,number.cex = 0.7,addCoef.col = 'black')
dev.off()




