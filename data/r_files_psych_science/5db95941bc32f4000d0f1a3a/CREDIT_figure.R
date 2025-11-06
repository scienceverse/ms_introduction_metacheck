library(tidyverse)
library(stringr)

credit <- rio::import("PSA001_CREDIT.xlsx")
colnames(credit) <- c("timestamp", "email", "first_name", "middle_initial", "last_name",  
                      "credit_conceptualization", "credit_data", "credit_analysis", "credit_funding",
                      "credit_investigation", "credit_methodology", "credit_administration", "credit_resources",
                      "credit_software", "credit_supervision", "credit_validation", "credit_visualization", "credit_writinginit", 
                      "credit_writingedit")
credit$order <- 1:nrow(credit)
credit$email <- tolower(credit$email)

# Investigate duplicate responses in the credit data
tab <- table(credit$email)
dups <- names(tab[tab>1])

credit_corrections <- rio::import("PSA001_CREDIT_change_request_form.csv")
credit_corrections <- credit_corrections[-19, ] # This person filled out the correction form twice
colnames(credit_corrections) <- c("timestamp", "email", "first_name", "middle_initial", "last_name", 
                                  "credit_conceptualization", "credit_data", "credit_analysis", "credit_funding",
                                  "credit_investigation", "credit_methodology", "credit_administration", "credit_resources",
                                  "credit_software", "credit_supervision", "credit_validation", "credit_visualization", 
                                  "credit_writinginit", "credit_writingedit")
credit_corrections$email <- tolower(credit_corrections$email)
credit_corrections <- inner_join(credit_corrections, credit[, c("email", "order")])
credit_names <- colnames(credit)[grep("credit_", colnames(credit))]


credit <- credit %>%
  anti_join(select(credit_corrections, email)) %>%
  bind_rows(credit_corrections) %>%
  arrange(order)

credit$middle_initial = ifelse(is.na(credit$middle_initial), "", credit$middle_initial)
credit[credit$first_name=="ANABEL", c("first_name", "last_name")] <- c("Anabel", "de la Rosa-Gómez")
credit[credit$middle_initial=="Kumar.", "middle_initial"] <- "K."
credit[credit$middle_initial=="W.A.", "middle_initial"] <- "W. A."
credit[credit$middle_initial=="K", "middle_initial"] <- "K."
credit[credit$middle_initial=="A", "middle_initial"] <- "A."
credit[credit$middle_initial=="C", "middle_initial"] <- "C."
credit[credit$middle_initial=="L", "middle_initial"] <- "L."
credit[credit$middle_initial=="R", "middle_initial"] <- "R."
credit <- mutate(credit,
                 name = paste(first_name, middle_initial, last_name),
                 name = str_squish(str_trim(name)),
                 name = factor(name, levels=rev(name)),
                 first_initial = ifelse(str_detect(first_name, "-"), 
                                        paste0(str_sub(first_name, end=1),  
                                               str_sub(first_name, start=str_locate(first_name, "-")[,1], 
                                                       end=str_locate(first_name, "-")[,1]+1), "."),
                                        paste0(str_sub(first_name, end=1), ".")
                                        ),
                 last_initial = ifelse(str_detect(last_name, "-"), 
                                        paste0(str_sub(last_name, end=1),  
                                               str_sub(last_name, start=str_locate(last_name, "-")[,1], 
                                                       end=str_locate(last_name, "-")[,1]+1), "."),
                                        paste0(str_sub(last_name, end=1), ".")
                                       ),
                 initials = paste0(first_initial, middle_initial, last_initial),
                 initials = str_replace_all(initials, " ", ""),
                 role = case_when(order %in% 1:3 ~ "admin",
                                  order %in% (-2:0 + nrow(credit)) ~ "lead",
                                  TRUE ~ "lab"))
credit <- anti_join(credit, data.frame(name=c("Julia Sterm", "Tara Marshall")))

# check for duplicate initials
tab <- table(credit$initials)
dups <- tab[which(tab > 1)]
# replace duplicates
credit <- mutate(credit,
                 initials = ifelse(initials %in% names(dups), paste(first_initial, last_name), initials))

# manually change some errors
credit[credit$initials == "K.T.", "initials"] <- "K.W.T."
credit[credit$initials == "L.V.", "initials"] <- "L.A.V."
credit[credit$initials == "D.O.", "initials"] <- "D.W.O."
credit[credit$initials == "N.V.", "initials"] <- "N.V.d.L."
credit[credit$initials == "E.M.O.", "initials"] <- "E.M.O.K."
credit[credit$initials == "C.J.J.v.", "initials"] <- "C.J.J.v.Z."
credit[credit$initials == "K.L.", "initials"] <- "K.M.L."
credit[credit$initials == "A.D.", "initials"] <- "A.D.R."
credit[credit$initials == "A.F.", "initials"] <- "A.M.F."
credit[credit$initials == "E.M.", "initials"] <- "E.M.G."
credit[credit$initials == "M.L.", "initials"] <- "M.T.L."

credit[, credit_names] <- lapply(credit[, credit_names], function(v) case_when(v == "I did not contribute to this task" ~ "none",
                                                                               v == "I did not contribute to this task;I played a supporting role in this task" ~ "none",
                                                                               v == "I played a supporting role in this task" ~ "support",
                                                                               v == "I played a supporting role in this task;I played a leading role in this task" ~ "support",
                                                                               v == "I played a leading role in this task" ~ "lead"))
credit[, credit_names] <- lapply(credit[, credit_names], function(v) factor(v, levels = c("none", "support", "lead")))

plot_dat <- credit[, c("name", credit_names)] %>%
  pivot_longer(cols=starts_with("credit_"), names_to="credit_category", values_to="credit_level") %>%
  arrange(name, credit_category) %>%
  mutate(credit_category = factor(credit_category, 
                                  levels=c("credit_conceptualization", "credit_data", "credit_analysis", "credit_funding",
                                           "credit_investigation", "credit_methodology", "credit_administration", "credit_resources",
                                           "credit_software", "credit_supervision", "credit_validation", "credit_visualization", "credit_writinginit", "credit_writingedit"),
                                  labels=c("Conceptualization", "Data curation", "Formal analysis", "Funding acquisition", 
                                           "Investigation", "Methodology", "Project administration", "Resources", 
                                           "Software", "Supervision", "Validation", "Visualization", "Writing (original draft)", "Writing (review & editing)")))

plot_dat %>%
  ggplot() +
  geom_tile(aes(x=credit_category, y=name, fill=credit_level), color="lightgrey") + 
  scale_fill_manual(values=c("white", "lightblue", "darkblue"), na.translate=FALSE) +
  scale_x_discrete(position="top") + 
  theme_minimal(base_size=16) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid=element_blank(),
        legend.position="none", axis.text.y=element_text(vjust=.5, size=5.5, 
                                                         face=case_when(credit$role == "lead" ~ "bold", 
                                                                        credit$role == "admin" ~ "bold.italic",
                                                                        TRUE ~ "plain")), 
        axis.text.x=element_text(angle=-90, hjust=1, vjust=0))
ggsave("CREDIT.pdf", height=21, width=7)

credit_written <- plot_dat %>%
  arrange(desc(name)) %>%
  left_join(select(credit, name, initials)) %>%
  select(-name) %>%
  group_by(credit_category) %>%
  filter(credit_level != "none") %>%
  summarize(initial_list = paste(initials, collapse=", ")) %>%
  mutate(credit_statement = paste0(credit_category, ": ", initial_list)) %>%
  select(credit_statement) %>%
  write_csv("credit_written.csv")
