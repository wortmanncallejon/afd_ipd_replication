############################################################
#
#   REPLICATION CODE FOR:
#
#   What Drives the Intra-Party Democracy of the “Alternative for Germany”: Populist Ideology, Low Institutionalisation or Lacking Party Unity?
#   
#   CODE AUTHOR: Felix Wortmann Callejón
#
#   DATE: JUNE, 12th 2023
#
############################################################

# SETUP ----
rm(list = ls())

library(MASS)
library(dplyr)
library(haven)
library(ggplot2)
library(here)

source(here("clean.R"))

a <- get_data()

raw <- read_sav(here("Import", "Data.sav")) %>%
  filter(Filter_Sample1 == 1)

VBD <- read_sav(here("Import", "VBD.sav"))

dpi = 600
width = 16
height = 9

# INTRODUCTION ----

## Page 2 ----

# We compare their attitudes with those of the other six parties
# elected to the Bundestag in 2017 (NAll = 7,923, NAfD = 1,100)
# to contextualise our empirical observations on the AfD. 

raw %>% 
  group_by(as_factor(Partei)) %>% 
  count() %>%  ungroup() %>% 
  mutate(N = sum(n))


# THESES ON WHAT DRIVES IPD WITHIN THE AFD: IDEOLOGY, INSTITUTIONALISATION, PARTY UNITY ----

## Figure 1 -----
events <- VBD %>% 
  filter(id08 == 1 & id09 == 1 & id11 == 1) %>% 
  mutate(party = as_factor(id02),
         party = factor(case_when(party == "BÜNDNIS 90/DIE GRÜNEN" ~ "A'90/Greens",
                                  party == "DIE LINKE" ~ "The Left",
                                  party == "CSU" ~ "CDU/CSU",
                                  party == "CDU" ~ "CDU/CSU",
                                  T ~ as.character(party))),
         id = id01,
         effective_inclusion = fa04,
         CIS = fa14,
         RCI = fa09,
         ALP = fa15,
         time = Besetztseit,
         contri_lp = fa16,
         contri_a = fa17) %>% 
  select(party:contri_a)

fig1_dat <- raw %>% 
  mutate(party = as_factor(Partei),
         party = factor(case_when(party == "CSU" ~ "CDU/CSU",
                                  party == "CDU" ~ "CDU/CSU",
                                  party == "Bündnis 90/Die Grünen" ~ "A'90/Greens",
                                  party == "Die Linke" ~ "The Left",
                                  T ~ as.character(party))),
         DV = factor(case_when(q16 == 1 ~ "Primary",
                               q16 == 2 ~ "General meetings",
                               q16 == 3 ~ "Delegate meetings",
                               q16 == 4 ~ "Party board"),
                     levels = rev(c("Primary", "General meetings", "Delegate meetings", "Party board")))) %>%
  rename(id = point_nr) %>% 
  select(party, id, DV) %>% 
  group_by(party,id,DV) %>% 
  count() %>% 
  filter(!is.na(DV)) %>% 
  group_by(party,id) %>%
  mutate(N = sum(n),
         p = n/N,
         afd = factor(ifelse(party == "AfD", 1, 0))) %>% 
  filter(DV %in% c("General meetings", "Delegate meetings")) %>% 
  inner_join(select(events, id, party, effective_inclusion), by = c("id", "party")) %>% 
  mutate(party = factor(party, levels = c("AfD", "A'90/Greens", "CDU/CSU", "The Left","FDP", "SPD")))

fig1_dat %>% 
  ggplot(aes(effective_inclusion,p, color = party, size = afd)) +
  scale_color_grey("Party") +
  scale_x_continuous("Effective inclusion", labels = scales::label_percent(1)) +
  scale_y_continuous("% preferred mode of candidate selection", labels = scales::label_percent(1)) +
  scale_size_manual(values = c(1,2)) +
  geom_smooth(color = "#000000", linetype = "dashed", size = 1) +
  geom_point() +
  theme_light() +
  guides(size = "none") +
  theme(legend.position = "bottom") +
  facet_grid(rows = vars(DV))


rm(events,fig1_dat)


# DATA AND DESCRIPTIVE FINDINGS ----

## Page 9 ----

# At the state level, 48 list selection were sampled. Since the German
# federal state consists of 16 states, the dataset includes eight of 
# 16 list nominations of each party.

VBD %>% 
  filter(id08 == 1 & id09 == 1 & id10 == 1 & id07 == 2) %>%
  group_by(as_factor(id02)) %>% 
  count() %>% ungroup() %>%
  mutate(N = sum(n))

# At the district level, 90 candidate selections were chosen across all
# parties, of which 14 (out of 15 per party) could be realised for the AfD. 

VBD %>% 
  filter(id08 == 1 & id09 == 1 & id10 == 1 & id07 == 1) %>%
  group_by(as_factor(id02)) %>% 
  count() %>% ungroup() %>%
  mutate(N = sum(n))

# + 1 meeting at the Pirate party that was excluded from the dataset.

# 20 of the AfD nomination gatherings were general meetings, while two at
# the state level were delegate assemblies.

VBD %>% 
  filter(id08 == 1 & id09 == 1 & id10 == 1 & id02 == 7) %>% 
  group_by(as_factor(fa01)) %>% 
  count()

## Page 10 ----

# A total of 2,804 AfD members were surveyed. 

VBD %>% 
  filter(id08 == 1 & id09 == 1 & id10 == 1) %>%
  filter(id02 == 7) %>% 
  group_by(as_factor(id02)) %>% 
  summarise(N = sum(fa02))

# 1,100 returned the questionnaire, 153 of them in district associations 
# and 947 in state level organisations. 

raw %>%
  filter(Filter_Sample1 == 1 & as_factor(Partei) == "AfD") %>% 
  group_by(as_factor(EBENE)) %>% 
  count() %>% ungroup() %>% 
  mutate(N = sum(n))

# The response rate for AfD members was a remarkable 39.2% 

round((1100/2804)*100,1)

# However, their approval of party-external primary is only about four
# percentage points higher than among members of all parties (4.5%
# approval in district elections). 

raw %>% 
  select(Partei, q16) %>% 
  mutate(comp_afd = factor(ifelse(as_factor(Partei) == "AfD","AfD","Others"))) %>% 
  filter(!is.na(q16)) %>% 
  group_by(comp_afd, as_factor(q16)) %>%
  count() %>% 
  group_by(comp_afd) %>% 
  mutate(p = n/sum(n),
         primary = factor(ifelse(`as_factor(q16)` == "die wahlberechtigten Bürgerinnen und Bürger",1,0))) %>% 
  filter(primary == 1) %>% 
  select(-n) %>% 
  tidyr::pivot_wider(values_from = p,names_from = comp_afd) %>% 
  mutate(across(AfD:Others, ~ round(.x*100,1)),
         d = AfD-Others) 

# Remarkably, about 70.6% of AfD respondents support general meetings in list selections.
# This is the highest value for this option, followed by a gap of 19.6%P to the second
# highest value measured for the Greens. 

raw %>% 
  select(Partei, q16) %>% 
  mutate(Partei = as_factor(Partei),
         q16 = factor(case_when(q16 == 1 ~ "Primary",
                                q16 == 2 ~ "General meetings",
                                q16 == 3 ~ "Delegate meetings",
                                q16 == 4 ~ "Party board",
                                T ~ NA_character_),
                      levels = c("Primary", "General meetings", "Delegate meetings", "Party board"))) %>% 
  filter(!is.na(q16)) %>% 
  group_by(Partei, q16) %>% count() %>% 
  group_by(Partei) %>% mutate(p = round(n/sum(n)*100,2)) %>%
  ungroup() %>% mutate(comp_afd = factor(ifelse(Partei == "AfD",1,0))) %>% 
  filter(q16 == "General meetings") %>% 
  group_by(comp_afd) %>% mutate(max_p = max(p)) %>% 
  ungroup() %>% 
  filter(p == max_p) %>% 
  select(Partei, p) %>% 
  tidyr::pivot_wider(names_from = Partei, values_from = p) %>% 
  mutate(d = AfD - `Bündnis 90/Die Grünen`)


# Moreover, fewer AfD members support an inclusive procedure in the
# more centralised arena of the state level (69.1%) than in the
# decentralised arena of the district level (80.8%) – presumably for
# practical reasons.

raw %>% 
  filter(as_factor(Partei) == "AfD") %>% 
  select(q16, EBENE) %>% 
  mutate(q16 = factor(case_when(q16 == 1 ~ "Primary",
                                q16 == 2 ~ "General meetings",
                                q16 == 3 ~ "Delegate meetings",
                                q16 == 4 ~ "Party board",
                                T ~ NA_character_),
                      levels = c("Primary", "General meetings", "Delegate meetings", "Party board"))) %>% 
  filter(!is.na(q16)) %>% 
  group_by(EBENE, q16) %>% count() %>% 
  group_by(EBENE) %>% mutate(p = round(n/sum(n)*100,2)) %>% 
  filter(q16 == "General meetings")

## Figure 2 ----

raw %>% 
  select(Partei, q16) %>% 
  mutate(party = as_factor(Partei),
         party = factor(case_when(party == "Bündnis 90/Die Grünen" ~ "A'90/Greens",
                                  party == "Die Linke" ~ "The Left",
                                  party == "CSU" ~ "CDU/CSU",
                                  party == "CDU" ~ "CDU/CSU",
                                  T ~ as.character(party)),
                        levels = c("AfD", "A'90/Greens", "CDU/CSU", "FDP", "The Left", "SPD")),
         DV = factor(case_when(q16 == 1 ~ "Primary",
                               q16 == 2 ~ "General meetings",
                               q16 == 3 ~ "Delegate meetings",
                               q16 == 4 ~ "Party board"),
                     levels = c("Primary", "General meetings", "Delegate meetings", "Party board"))) %>% 
  filter(!is.na(DV)) %>% 
  group_by(party, DV) %>%
  count() %>% 
  group_by(party) %>% 
  mutate(p = n/sum(n),
         lab = paste0(as.character(round(p*100, 0)), "%")) %>% 
  ggplot(aes(DV, p, fill = party, label = lab)) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0,1), "Share of respondents") +
  scale_fill_grey() +
  scale_x_discrete(NULL) +
  geom_col(position = position_dodge(0.9)) +
  geom_text(position = position_dodge(0.9), vjust = -0.5) +
  facet_wrap(~party) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1),
        panel.grid.major.x = element_blank())

## Figure 3 ----

sup <- tibble(x = c(0.4,3.6),
              y = c(0,0),
              plot = factor(c("Difference","Difference"), levels = c("Placements", "Difference")),
              lower = rep(0,2),
              upper = rep(0,2))

raw %>% 
  filter(as_factor(Partei) == "AfD" & !is.na(q16)) %>% 
  select(q16, q6a, q6b) %>% 
  mutate(q16 = as_factor(q16),
         q16 = factor(case_when(q16 == "die wahlberechtigten Bürgerinnen und Bürger" ~ "Primary",
                                q16 == "alle Mitglieder meiner Partei auf einer Mitgliederversammlung" ~ "General meeting",
                                q16 == "die gewählten Mitglieder meiner Partei auf einer Delegiertenversammlung" ~ "Delegate assembly",
                                q16 == "den jeweils zuständigen Parteivorstand" ~ NA_character_),
                      levels = rev(c("Primary", "General meeting", "Delegate assembly"))),
         diff = q6a - q6b) %>% 
  tidyr::pivot_longer(-q16) %>% 
  group_by(q16,name) %>% 
  summarise(est = t.test(value)[["estimate"]],
            lower = t.test(value)[["conf.int"]][1],
            upper = t.test(value)[["conf.int"]][2],
            n = n()) %>% 
  filter(!is.na(q16)) %>% 
  mutate(name = factor(case_when(name == "q6a" ~ "Self-placement",
                                 name == "q6b" ~ "Party-placement",
                                 TRUE ~ "Difference"),
                       levels = c("Self-placement", "Party-placement", "Difference")),
         plot = factor(ifelse(name == "Difference", "Difference", "Placements"), levels = c("Placements", "Difference")),
         q16 = stringr::str_replace_all(q16, " ", "\n")) %>% 
  ggplot(aes(q16,est, ymin = lower, ymax = upper, color = name)) +
  scale_x_discrete(NULL) +
  scale_y_continuous(NULL) +
  scale_color_grey(NULL, end = 0.6) +
  geom_line(data = sup, aes(x,y), color = "#000000", linetype = "dashed") +
  geom_pointrange(position = position_dodge(0.3)) +
  theme_light() +
  theme(legend.position = "bottom") +
  facet_wrap(~plot, scale = "free_y")

rm(sup)


# MULTIVARIATE RESULTS ----

## Figure 4 ----

ests <- tibble(term = character(0),
               estimate = numeric(0),
               std.error = numeric(0),
               statistic = numeric(0),
               df = numeric(0),
               party = character(0),
               conf.low = numeric(0),
               conf.high = numeric(0),
               p.value = numeric(0),
               p.stars = character(0),
               p.label = character(0))

for(p in list("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "Die Linke", "FDP", "AfD"))  {
  
  d <- filter(a, party == p)
  
  h1 <- polr(DV ~ trust_demo + dist_mahal + party_membership + gender + educ + politiknah, data = d, method = "logistic", Hess = T)
  h2 <- polr(DV ~ satis_parl + dist_mahal + party_membership + gender + educ + politiknah, data = d, method = "logistic", Hess = T)
  h3 <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal, data = d, method = "logistic", Hess = T)
  h4 <- polr(DV ~ party_work       + educ + gender + trust_demo + dist_mahal, data = d, method = "logistic", Hess = T)
  h5 <- polr(DV ~ q6a_minus_b + trust_demo + party_membership + gender + educ, data = d, method = "logistic", Hess = T)
  h6 <- polr(DV ~ dist_mahal  + trust_demo + party_membership + gender + educ, data = d, method = "logistic", Hess = T)
  
  
  
  ests <- filter(mutate(broom::tidy(h1), df = h1[["df.residual"]]), term == "trust_demo") %>% 
    rbind(filter(mutate(broom::tidy(h2), df = h2[["df.residual"]]), term == "satis_parl")) %>% 
    rbind(filter(mutate(broom::tidy(h3), df = h3[["df.residual"]]), term == "party_membership")) %>% 
    rbind(filter(mutate(broom::tidy(h4), df = h4[["df.residual"]]), term == "party_work")) %>% 
    rbind(filter(mutate(broom::tidy(h5), df = h5[["df.residual"]]), term == "q6a_minus_b")) %>%
    rbind(filter(mutate(broom::tidy(h6), df = h6[["df.residual"]]), term == "dist_mahal")) %>%
    mutate(party = p,
           conf.low = exp(estimate - qt(0.025, df) * std.error),
           conf.high = exp(estimate + qt(0.025, df) * std.error),
           statistic = estimate/std.error,
           p.value = (1 - pt(abs(statistic),df))*2,
           p.stars = case_when(p.value < 0.001 ~ "***",
                               p.value < 0.01 ~ "**",
                               p.value < 0.05 ~ "*",
                               T ~ ""),
           estimate = exp(estimate),
           p.label = paste0(format(round(estimate, 2), nsmall = 2),p.stars)) %>% 
    select(-coef.type) %>% 
    rbind(ests)
}

rm(h1,h2,h3,h4,h5,h6,p)

ests <- ests %>% 
  mutate(term = factor(case_when(term == "trust_demo" ~ "Trust in democracy",
                                 term == "satis_parl" ~ "Satisfaction with parliament",
                                 term == "party_membership" ~ "Duration party membership",
                                 term == "party_work" ~ "Monthly hours of party work",
                                 term == "dist_mahal" ~ "Multivariate distance from party", 
                                 TRUE ~ "Perceived distance from party"),
                       levels = c("Trust in democracy","Satisfaction with parliament", "Duration party membership", "Monthly hours of party work", "Perceived distance from party", "Multivariate distance from party")),
         party = factor(case_when(party == "Bündnis 90/Die Grünen" ~ "A'90/Greens",
                                  party == "Die Linke" ~ "The Left",
                                  T ~ as.character(party)),
                        levels = rev(c("AfD", "A'90/Greens", "CDU/CSU", "FDP", "The Left", "SPD"))),
         afd = factor(ifelse(party == "AfD",1,0)))

ests %>% 
  filter(term %in% c("Trust in democracy", "Satisfaction with parliament", "Duration party membership")) %>% 
  ggplot(aes(estimate, party, xmin = conf.low, xmax = conf.high, label = p.label, color = party, size = afd, linewidth = as.numeric(afd))) +
  scale_color_grey(start = 0.8, end = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_size_manual(values = c(0.5,0.8)) +
  scale_linewidth(range = c(0.5,1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(NULL) +
  scale_x_continuous("Estimated marginal effect on nomination preference in odds ratios") +
  geom_pointrange() +
  geom_text(vjust = -1, size = 3) +
  theme_light() +
  theme(legend.position = "none") +
  facet_wrap(~term, scales = "free_x")

## Figure 5 ----

ests %>%
  filter(term %in% c("Monthly hours of party work", "Multivariate distance from party", "Perceived distance from party")) %>% 
  ggplot(aes(estimate, party, xmin = conf.low, xmax = conf.high, label = p.label, color = party, size = afd, linewidth = as.numeric(afd))) +
  scale_color_grey(start = 0.8, end = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_size_manual(values = c(0.5,0.8)) +
  scale_linewidth(range = c(0.5,1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete(NULL) +
  scale_x_continuous("Estimated marginal effect on nomination preference in odds ratios") +
  geom_pointrange() +
  geom_text(vjust = -1, size = 3) +
  theme_light() +
  theme(legend.position = "none") +
  facet_wrap(~term, scales = "free_x")



# APPENDIX ----

## Table A ----

a %>% 
  select(id, party,
         trust_demo, satis_parl,
         q6a_minus_b, dist_mahal,
         party_work, party_membership,
         gender, politiknah) %>% 
  mutate(gender = ifelse(gender == "männlich",0,1),
         politiknah = ifelse(politiknah == "keine Tätigkeit im politische Bereich", 0,1)) %>% 
  tidyr::pivot_longer(-c(id, party)) %>% 
  mutate(count = ifelse(is.na(value),0,1)) %>% 
  group_by(name, party) %>% 
  summarise(mean = mean(value, na.rm = T),
            sd = sd(value, na.rm = T),
            N = sum(count)) %>% 
  filter(party == "AfD")  %>% 
  rename(Variable = name,
         Mean = mean,
         SD = sd) %>% 
  mutate(Variable = factor(case_when(Variable == "trust_demo" ~ "Trust in democracy",
                                     Variable == "satis_parl" ~ "Satisfaction with parliament",
                                     Variable == "party_membership" ~ "Duration of party membership",
                                     Variable == "party_work" ~ "Regular hours of party work per month",
                                     Variable == "q6a_minus_b" ~ "Difference between party- and self placement",
                                     Variable == "gender" ~ "% female",
                                     Variable == "politiknah" ~ "% occupied in politics",
                                     Variable == "dist_mahal" ~ "Ideological distance"),
                           levels = c("Trust in democracy", "Satisfaction with parliament", 
                                      "Duration of party membership", "Regular hours of party work per month",
                                      "Difference between party- and self placement", "Ideological distance",
                                      "% female", "% occupied in politics"))) %>% 
  ungroup() %>% 
  select(-party) %>% 
  mutate(across(is.numeric, round, 2)) %>% 
  arrange(Variable) %>%
  xtable::xtable()

## Figure A ----

VBD <- read_sav(here("Import", "VBD.sav"))


states <- c("Baden-\nWürttemberg", "Bavaria", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hesse", "Lower Saxony", "Mecklenburg-\nVorpommern", "North Rhine-\nWestphalia", "Rhineland-\nPalatinate", "Saarland", "Saxony", "Saxony-\nAnhalt", "Schleswig-\nHolstein", "Thuringia") 

VBD %>% 
  filter(id08 == 1 & id09 == 1 & id10 == 1) %>%
  mutate(party = factor(case_when(as_factor(id02) %in% c("CDU","CSU") ~ "CDU/CSU",
                                  as_factor(id02) == "BÜNDNIS 90/DIE GRÜNEN" ~ "A'90/Greens",
                                  as_factor(id02) == "DIE LINKE" ~ "The Left",
                                  T ~ as.character(as_factor(id02))),
                        levels = c("AfD", "A'90/Greens", "CDU/CSU", "FDP", "The Left", "SPD")),
         state = as_factor(id06),
         level = factor(ifelse(as_factor(id07) == "Wahlkreisebene", "District Level", "State Level"))) %>% 
  group_by(party, state, level) %>% 
  count() %>% 
  mutate(party_num = case_when(party == "AfD" ~ 1,
                               party == "A'90/Greens" ~ 2,
                               party == "CDU/CSU" ~ 3,
                               party == "FDP" ~ 4,
                               party == "The Left" ~ 5,
                               party == "SPD" ~ 6),
         party_num = ifelse(level == "District Level", party_num - 0.1, party_num + 0.1),
         state = case_when(state %in% c("Saarland SL", "Baden-Württemberg BW", "Bremen HB", "Brandenburg BB", "Berlin BE", "Mecklenburg-Vorpommern MV", "Schleswig-Holstein SH", "Hamburg HH") ~ stringr::str_sub(state, 1, -4),
                           state == "Bayern BY" ~ "Bavaria",
                           state == "Rheinland-Pfalz RP" ~ "Rhineland-Palatinate",
                           state %in% c("Sachsen SN", "Sachsen-Anhalt ST") ~ stringr::str_sub(stringr::str_replace(state, "Sachsen", "Saxony"),1,-4),
                           state == "Thüringen TH" ~ "Thuringia",
                           state == "Nordrhein-Westfalen NW" ~ "North Rhine-Westphalia",
                           state == "Hessen HE" ~ "Hesse",
                           state == "Niedersachsen NI" ~ "Lower Saxony"),
         state = factor(stringr::str_replace(state, "-", "-\n"),
                        levels = rev(states)),
         lab = ifelse(level == "District Level", n, NA),
         si = ifelse(level == "District Level", 5, 4)) %>% 
  ggplot(aes(party_num, state, color = level, size = si, shape = level)) +
  scale_x_continuous(NULL, breaks = 1:6, labels = c("AfD", "A'90/Greens", "CDU/CSU", "FDP", "The Left", "SPD")) +
  scale_y_discrete(NULL) +
  scale_shape_manual("Number of selected nomination meetings", values = c(16,18)) +
  scale_color_grey("Number of selected nomination meetings", start = 0.9, end = 0.3) +
  scale_size_area() +
  geom_point() +
  geom_text(aes(label = lab), color = "black", size = 4, fontface = "bold") +
  guides(size = "none",
         color = guide_legend(override.aes = list(size=4))) +
  theme_bw() +
  theme(legend.position = "bottom")


## Figure B  -----

raw %>% 
  select(Partei, EBENE, q16) %>% 
  na.omit() %>% 
  mutate(across(Partei:q16, as_factor)) %>%
  filter(Partei == "AfD") %>% 
  mutate(q16 = factor(case_when(q16 == "die wahlberechtigten Bürgerinnen und Bürger" ~ "Primary",
                                q16 == "alle Mitglieder meiner Partei auf einer Mitgliederversammlung" ~ "General meeting",
                                q16 == "die gewählten Mitglieder meiner Partei auf einer Delegiertenversammlung" ~ "Delegate assembly",
                                q16 == "den jeweils zuständigen Parteivorstand" ~ "Party board",
                                T ~ NA_character_),
                      levels = c("Primary", "General meeting", "Delegate assembly", "Party board")),
         EBENE = factor(ifelse(EBENE == "Wahlkreisebene", "District level", "State level"))) %>% 
  group_by(EBENE,q16) %>% 
  count() %>% 
  group_by(EBENE) %>% 
  mutate(N = sum(n),
         p = n/N,
         lab = ifelse(p >= 0.005, paste0(round(p*100, 0),"%"), "")) %>% 
  ggplot(aes(q16,p, fill = EBENE, label = lab)) +
  scale_y_continuous(limits = c(0,1), labels = scales::label_percent(1), "Share of respondents") +
  scale_fill_grey(NULL, start = 0.5, end = 0.8) +
  scale_x_discrete(NULL) +
  geom_col(position = position_dodge(0.9)) +
  geom_text(position = position_dodge(0.9), family = "CMSS", size = 4, vjust = -0.3) +
  theme_light() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())


## Figure C ----


raw %>% 
  select(Partei, q7, q8, q16) %>% 
  mutate(across(c(Partei, q16), \(x) as_factor(x)),
         q16 = factor(case_when(q16 == "die wahlberechtigten Bürgerinnen und Bürger" ~ "Primary",
                                q16 == "alle Mitglieder meiner Partei auf einer Mitgliederversammlung" ~ "General meeting",
                                q16 == "die gewählten Mitglieder meiner Partei auf einer Delegiertenversammlung" ~ "Delegate assembly",
                                q16 == "den jeweils zuständigen Parteivorstand" ~ NA_character_),
                      levels = rev(c("Primary", "General meeting", "Delegate assembly"))),
         Partei = factor(case_when(Partei %in% c("CDU", "CSU") ~ "CDU/CSU",
                                   Partei == "Bündnis 90/Die Grünen" ~ "A'90/Greens",
                                   Partei == "Die Linke" ~ "The Left",
                                   T ~ Partei),
                         levels = c("AfD", "A'90/Greens", "CDU/CSU", "FDP", "The Left", "SPD")),
         across(q7:q8, \(x) as.numeric(x))) %>% 
  tidyr::pivot_longer(c(q7,q8)) %>% 
  group_by(Partei, q16, name) %>% 
  na.omit() %>% 
  summarise(est = t.test(value)[["estimate"]],
            lwr = t.test(value)[["conf.int"]][1],
            upr = t.test(value)[["conf.int"]][2],
            n = n()) %>% 
  mutate(name = factor(ifelse(name == "q7", "Satisfaction with parliament", "Trust in democracy"),
                       levels = c("Trust in democracy", "Satisfaction with parliament"))) %>% 
  ggplot(aes(q16,est, ymin = lwr, ymax = upr, colour = Partei)) +
  scale_y_continuous(NULL, limits = c(1,4)) +
  scale_color_manual(values = c("#005EA4", "#0E8C1D", "#000000", "#FFC000", "#CC0066", "#C00000"), "Party") +
  scale_x_discrete(NULL) +
  geom_pointrange(position = position_dodge(0.3)) +
  facet_wrap(~name) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())



## Figure D ----


p1 <- raw %>% 
  filter(as_factor(Partei) == "AfD" & !is.na(q16)) %>%
  select(q1_Dauer, q2, q16) %>% 
  mutate(q16 = as_factor(q16),
         q16 = factor(case_when(q16 == "die wahlberechtigten Bürgerinnen und Bürger" ~ "Primary",
                                q16 == "alle Mitglieder meiner Partei auf einer Mitgliederversammlung" ~ "General meeting",
                                q16 == "die gewählten Mitglieder meiner Partei auf einer Delegiertenversammlung" ~ "Delegate assembly",
                                q16 == "den jeweils zuständigen Parteivorstand" ~ NA_character_),
                      levels = rev(c("Primary", "General meeting", "Delegate assembly")))) %>% 
  tidyr::pivot_longer(-q16) %>% 
  group_by(q16,name) %>% 
  summarise(est = t.test(value)[["estimate"]],
            lower = t.test(value)[["conf.int"]][1],
            upper = t.test(value)[["conf.int"]][2]) %>% 
  filter(!is.na(q16)) %>% 
  mutate(name = ifelse(name == "q2", "Monthly hours of party work", "Duration of party membership")) %>% 
  filter(name == "Duration of party membership") %>% 
  ggplot(aes(q16,est, ymin = lower, ymax = upper)) +
  scale_x_discrete(NULL) +
  scale_y_continuous(NULL, limits = c(1,3)) +
  geom_pointrange() +
  theme_light() +
  facet_wrap(~name)

p2 <- raw %>% 
  filter(as_factor(Partei) == "AfD" & !is.na(q16)) %>%
  select(q1_Dauer, q2, q16) %>% 
  mutate(q16 = as_factor(q16),
         q16 = factor(case_when(q16 == "die wahlberechtigten Bürgerinnen und Bürger" ~ "Primary",
                                q16 == "alle Mitglieder meiner Partei auf einer Mitgliederversammlung" ~ "General meeting",
                                q16 == "die gewählten Mitglieder meiner Partei auf einer Delegiertenversammlung" ~ "Delegate assembly",
                                q16 == "den jeweils zuständigen Parteivorstand" ~ NA_character_),
                      levels = rev(c("Primary", "General meeting", "Delegate assembly")))) %>% 
  tidyr::pivot_longer(-q16) %>% 
  group_by(q16,name) %>% 
  summarise(est = t.test(value)[["estimate"]],
            lower = t.test(value)[["conf.int"]][1],
            upper = t.test(value)[["conf.int"]][2]) %>% 
  filter(!is.na(q16)) %>% 
  mutate(name = ifelse(name == "q2", "Monthly hours of party work", "Duration of party membership")) %>% 
  filter(name == "Monthly hours of party work") %>% 
  ggplot(aes(q16,est, ymin = lower, ymax = upper)) +
  scale_x_discrete(NULL) +
  scale_y_continuous(NULL, limits = c(10,50)) +
  geom_pointrange() +
  theme_light() +
  facet_wrap(~name)

library(patchwork)

p1 + p2

rm(p1,p2)

## Figure E ----

dist <- tibble(fbnr = character(0),
               dist_mahal = numeric(0))

events <- raw %>% 
  select(fbnr, Partei, q6a, q501, q502) %>% 
  mutate(Partei = as_factor(Partei)) %>% 
  split(.$Partei)


for(event in events[1:7]) {
  
  means <- event %>% 
    tidyr::pivot_longer(-c(fbnr, Partei)) %>% 
    group_by(name) %>% 
    summarise(value = mean(value, na.rm = T)) %>%  .$value
  
  dist <- tibble(fbnr = event$fbnr,
                 dist_mahal = mahalanobis(event[3:5], means, cov(event[3:5], use = "pairwise.complete.obs"))) %>% 
    rbind(dist)
}

raw %>% 
  filter(as_factor(Partei) == "AfD") %>%
  select(fbnr, q16) %>% 
  inner_join(dist, by = "fbnr") %>% 
  mutate(q16 = as_factor(q16),
         q16 = factor(case_when(q16 == "die wahlberechtigten Bürgerinnen und Bürger" ~ "Primary",
                                q16 == "alle Mitglieder meiner Partei auf einer Mitgliederversammlung" ~ "General meeting",
                                q16 == "die gewählten Mitglieder meiner Partei auf einer Delegiertenversammlung" ~ "Delegate assembly",
                                q16 == "den jeweils zuständigen Parteivorstand" ~ NA_character_),
                      levels = c("Primary", "General meeting", "Delegate assembly"))) %>% 
  na.omit() %>%
  ggplot(aes(dist_mahal)) + 
  scale_x_continuous("Multivariate distance") +
  labs(y = "Density") +
  geom_density(alpha = 0.8, fill = "#BFBFBF") +
  theme_light() +
  theme(legend.position = "none") +
  facet_grid(rows = vars(q16))


rm(dist, events, event, means)

## Table B ----

h1 <- polr(DV ~ trust_demo + dist_mahal + party_membership + gender + educ + politiknah, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h2 <- polr(DV ~ satis_parl + dist_mahal + party_membership + gender + educ + politiknah, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h3 <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h4 <- polr(DV ~ party_work       + educ + gender + trust_demo + dist_mahal, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h5 <- polr(DV ~ q6a_minus_b + trust_demo + party_membership + gender + educ, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h6 <- polr(DV ~ dist_mahal  + trust_demo + party_membership + gender + educ, data = filter(a, party == "AfD"), method = "logistic", Hess = T)

h1 <- broom::tidy(h1) %>% 
  mutate(df = h1[["df.residual"]],
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         label = paste0(format(round(exp(estimate), 2), nsmall = 2),p.stars),
         conf.int = paste0("[",round(exp(estimate + qt(0.025, df) * std.error),2)," - ", round(exp(estimate - qt(0.025, df) * std.error),2), "]")) %>% 
  select(term,label, conf.int) %>% 
  tidyr::pivot_longer(-term) %>%
  rename(`(1)` = value) 

h2 <- broom::tidy(h2) %>% 
  mutate(df = h2[["df.residual"]],
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         label = paste0(format(round(exp(estimate), 2), nsmall = 2),p.stars),
         conf.int = paste0("[",round(exp(estimate + qt(0.025, df) * std.error),2)," - ", round(exp(estimate - qt(0.025, df) * std.error),2), "]")) %>% 
  select(term,label, conf.int) %>% 
  tidyr::pivot_longer(-term) %>%
  rename(`(2)` = value) 

h3 <- broom::tidy(h3) %>% 
  mutate(df = h3[["df.residual"]],
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         label = paste0(format(round(exp(estimate), 2), nsmall = 2),p.stars),
         conf.int = paste0("[",round(exp(estimate + qt(0.025, df) * std.error),2)," - ", round(exp(estimate - qt(0.025, df) * std.error),2), "]")) %>% 
  select(term,label, conf.int) %>% 
  tidyr::pivot_longer(-term) %>%
  rename(`(3)` = value) 

h4 <- broom::tidy(h4) %>% 
  mutate(df = h4[["df.residual"]],
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         label = paste0(format(round(exp(estimate), 2), nsmall = 2),p.stars),
         conf.int = paste0("[",round(exp(estimate + qt(0.025, df) * std.error),2)," - ", round(exp(estimate - qt(0.025, df) * std.error),2), "]")) %>% 
  select(term,label, conf.int) %>% 
  tidyr::pivot_longer(-term) %>%
  rename(`(4)` = value) 

h5 <- broom::tidy(h5) %>% 
  mutate(df = h5[["df.residual"]],
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         label = paste0(format(round(exp(estimate), 2), nsmall = 2),p.stars),
         conf.int = paste0("[",round(exp(estimate + qt(0.025, df) * std.error),2)," - ", round(exp(estimate - qt(0.025, df) * std.error),2), "]")) %>% 
  select(term,label, conf.int) %>% 
  tidyr::pivot_longer(-term) %>%
  rename(`(5)` = value) 

h6 <- broom::tidy(h6) %>% 
  mutate(df = h6[["df.residual"]],
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         label = paste0(format(round(exp(estimate), 2), nsmall = 2),p.stars),
         conf.int = paste0("[",round(exp(estimate + qt(0.025, df) * std.error),2)," - ", round(exp(estimate - qt(0.025, df) * std.error),2), "]")) %>% 
  select(term,label, conf.int) %>% 
  tidyr::pivot_longer(-term) %>%
  rename(`(6)` = value) 


full_join(h1, h2, by = c("term", "name")) %>% 
  full_join(h3, by = c("term", "name")) %>% 
  full_join(h4, by = c("term", "name")) %>% 
  full_join(h5, by = c("term", "name")) %>% 
  full_join(h6, by = c("term", "name")) %>%
  mutate(term = factor(case_when(term == "trust_demo" ~ "Trust in democracy",
                                 term == "satis_parl" ~ "Satisfaction with parliament",
                                 term == "q6a_minus_b" ~ "Perceived distance from party",
                                 term == "dist_mahal" ~ "Multivar. distance from party",
                                 term == "party_membership" ~ "Duration of party membership",
                                 term == "party_work" ~ "Monthly hours of party work",
                                 term == "genderweiblich" ~ "Gender: female",
                                 term == "educHaupt- oder Volksschulabschluss" ~ "Education: school leaving certificate",
                                 term == "educMittlere Reife oder Abschluss der polytechnischen Oberschule" ~ "Education: middle school completion or polytechnic high school graduation",
                                 term == "educAbitur, Fachhochschulreife (Gymnasium oder erweiterte Oberschule EOS)" ~ "Education: university preparatory school graduation",
                                 term == "educUniversitäts-, Hochschul- bzw. Fachhochschulabschluss" ~ "Education: university, technical college or college degree",
                                 term == "politiknahkeine Tätigkeit im politische Bereich" ~ "Occupied in politics",
                                 term == "Delegiertenversammlung|Mitgliederversammlung" ~ "Delegate Assembly → General Meeting",
                                 term == "Mitgliederversammlung|Primaries" ~ "General Meeting → Primaries"),
                       levels = c("Delegate Assembly → General Meeting",
                                  "General Meeting → Primaries",
                                  "Trust in democracy",
                                  "Satisfaction with parliament",
                                  "Duration of party membership",
                                  "Monthly hours of party work",
                                  "Perceived distance from party",
                                  "Multivar. distance from party",
                                  "Gender: female",
                                  "Education: school leaving certificate",
                                  "Education: middle school completion or polytechnic high school graduation",
                                  "Education: university preparatory school graduation",
                                  "Education: university, technical college or college degree",
                                  "Occupied in politics"))) %>% 
  arrange(term) %>%
  mutate(term = ifelse(name == "label", as.character(term), NA_character_)) %>% 
  rename(Predictors = term) %>% select(-name) %>% xtable::xtable()



## Figure F -----

h1 <- polr(DV ~ trust_demo + dist_mahal + party_membership + gender + educ + politiknah, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h2 <- polr(DV ~ satis_parl + dist_mahal + party_membership + gender + educ + politiknah, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h3 <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h4 <- polr(DV ~ party_work       + educ + gender + trust_demo + dist_mahal, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h5 <- polr(DV ~ q6a_minus_b + trust_demo + party_membership + gender + educ, data = filter(a, party == "AfD"), method = "logistic", Hess = T)
h6 <- polr(DV ~ dist_mahal  + trust_demo + party_membership + gender + educ, data = filter(a, party == "AfD"), method = "logistic", Hess = T)

h1_m <- multinom(DV ~ trust_demo + dist_mahal + party_membership + gender + educ + politiknah, data = filter(a, party == "AfD"))
h2_m <- multinom(DV ~ satis_parl + dist_mahal + party_membership + gender + educ + politiknah, data = filter(a, party == "AfD"))
h3_m <- multinom(DV ~ party_membership + educ + gender + trust_demo + dist_mahal, data = filter(a, party == "AfD"))
h4_m <- multinom(DV ~ party_work       + educ + gender + trust_demo + dist_mahal, data = filter(a, party == "AfD"))
h5_m <- multinom(DV ~ q6a_minus_b + trust_demo + party_membership + gender + educ, data = filter(a, party == "AfD"))
h6_m <- multinom(DV ~ dist_mahal  + trust_demo + party_membership + gender + educ, data = filter(a, party == "AfD"))


h1 <- data.frame(h1[["fitted.values"]]) %>% 
  mutate(model = "Ordinal",
         id = 1:length(.$Primaries)) %>% 
  rbind(mutate(data.frame(h1_m[["fitted.values"]]), model = "Multinomial", id = 1:length(.$Primaries))) %>% 
  tidyr::pivot_longer(-c(model, id)) %>% 
  tidyr::pivot_wider(values_from = "value", names_from = "model") %>% 
  mutate(Hypothesis = "Trust in democracy")


h2 <- data.frame(h2[["fitted.values"]]) %>% 
  mutate(model = "Ordinal",
         id = 1:length(.$Primaries)) %>% 
  rbind(mutate(data.frame(h2_m[["fitted.values"]]), model = "Multinomial", id = 1:length(.$Primaries))) %>% 
  tidyr::pivot_longer(-c(model, id)) %>% 
  tidyr::pivot_wider(values_from = "value", names_from = "model") %>% 
  mutate(Hypothesis = "Satisfaction with parliament")

h4 <- data.frame(h4[["fitted.values"]]) %>% 
  mutate(model = "Ordinal",
         id = 1:length(.$Primaries)) %>% 
  rbind(mutate(data.frame(h4_m[["fitted.values"]]), model = "Multinomial", id = 1:length(.$Primaries))) %>% 
  tidyr::pivot_longer(-c(model, id)) %>% 
  tidyr::pivot_wider(values_from = "value", names_from = "model") %>% 
  mutate(Hypothesis = "Monthly hours of party work")

h3 <- data.frame(h3[["fitted.values"]]) %>% 
  mutate(model = "Ordinal",
         id = 1:length(.$Primaries)) %>% 
  rbind(mutate(data.frame(h3_m[["fitted.values"]]), model = "Multinomial", id = 1:length(.$Primaries))) %>% 
  tidyr::pivot_longer(-c(model, id)) %>% 
  tidyr::pivot_wider(values_from = "value", names_from = "model") %>% 
  mutate(Hypothesis = "Duration of party membership")

h5 <- data.frame(h5[["fitted.values"]]) %>% 
  mutate(model = "Ordinal",
         id = 1:length(.$Primaries)) %>% 
  rbind(mutate(data.frame(h5_m[["fitted.values"]]), model = "Multinomial", id = 1:length(.$Primaries))) %>% 
  tidyr::pivot_longer(-c(model, id)) %>% 
  tidyr::pivot_wider(values_from = "value", names_from = "model") %>% 
  mutate(Hypothesis = "Perceived distance from party")

h6 <- data.frame(h6[["fitted.values"]]) %>% 
  mutate(model = "Ordinal",
         id = 1:length(.$Primaries)) %>% 
  rbind(mutate(data.frame(h6_m[["fitted.values"]]), model = "Multinomial", id = 1:length(.$Primaries))) %>% 
  tidyr::pivot_longer(-c(model, id)) %>% 
  tidyr::pivot_wider(values_from = "value", names_from = "model") %>% 
  mutate(Hypothesis = "Multivariate distance from party")

rm(h1_m, h2_m, h3_m, h4_m, h5_m, h6_m)

rbind(h1,h2,h4,h5,h6,h7) %>% 
  mutate(name = factor(case_when(name == "Delegiertenversammlung" ~ "Delegate assembly",
                                 name == "Mitgliederversammlung" ~ "General meeting",
                                 T ~ name),
                       levels = c("Delegate assembly", "General meeting","Primaries")),
         Hypothesis = factor(Hypothesis,
                             levels = c("Trust in democracy", "Satisfaction with parliament", "Monthly hours of party work",
                                        "Duration of party membership", "Perceived distance from party", "Multivariate distance from party"))) %>% 
  ggplot(aes(Ordinal,Multinomial,color = name, label = id)) +
  scale_x_continuous("Probability fitted by ordinal model", limits =c(0,0.85), labels = scales::label_percent(1)) +
  scale_y_continuous("Probability fitted by multinomial model", limits =c(0,0.85), labels = scales::label_percent(1)) +
  scale_color_grey(NULL) +
  geom_point(shape = 1, alpha = 0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_light() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme(legend.position = "bottom") +
  facet_wrap(~Hypothesis)


## Figure G ----

predicted_probs <- tibble(party = character(0),
                          indep_var = character(0),
                          indep_var_vals = numeric(0),
                          class = character(0),
                          prob = numeric(0))

sum_stats <- filter(a, party == "AfD") %>%
  select(id,party_work, party_membership, dist_mahal, satis_parl, trust_demo, q6a_minus_b) %>% 
  tidyr::pivot_longer(-id) %>% 
  group_by(name) %>% 
  summarise(min = floor(min(value, na.rm = T)),
            max = ceiling(max(value, na.rm = T)),
            sd = floor(sd(value, na.rm = T)))

var_df <- tibble(dist_mahal = seq(sum_stats[1,]$min, sum_stats[1,]$max, length.out = 10),
                 party_membership = seq(sum_stats[2,]$min, sum_stats[2,]$max, length.out = 10),
                 party_work = seq(sum_stats[3,]$min, 100, length.out = 10),
                 q6a_minus_b = seq(sum_stats[4,]$min, sum_stats[4,]$max, length.out = 10),
                 satis_parl = seq(sum_stats[5,]$min, sum_stats[5,]$max, length.out = 10),
                 trust_demo = seq(sum_stats[6,]$min, sum_stats[6,]$max, length.out = 10)) 

for(p in list("CDU/CSU", "SPD", "Bündnis 90/Die Grünen", "Die Linke", "FDP", "AfD"))  {
  
  d <- filter(a, party == p)
  
  h1 <- polr(DV ~ trust_demo + dist_mahal + party_membership + gender + educ + politiknah, data = d, method = "logistic", Hess = T)
  h2 <- polr(DV ~ satis_parl + dist_mahal + party_membership + gender + educ + politiknah, data = d, method = "logistic", Hess = T)
  h3 <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal, data = d, method = "logistic", Hess = T)
  h4 <- polr(DV ~ party_work       + educ + gender + trust_demo + dist_mahal, data = d, method = "logistic", Hess = T)
  h5 <- polr(DV ~ q6a_minus_b + trust_demo + party_membership + gender + educ, data = d, method = "logistic", Hess = T)
  h6 <- polr(DV ~ dist_mahal  + trust_demo + party_membership + gender + educ, data = d, method = "logistic", Hess = T)
  
  
  means <- d %>%
    select(gender,educ,politiknah,age,lire_self,party_work, party_membership, dist_mahal, satis_parl, trust_demo, q6a_minus_b) %>% 
    summarise(across(gender:politiknah, factor, ordered = T),
              across(gender:politiknah, quantile, na.rm = T, probs = 0.5, type = 1),
              across(age:q6a_minus_b, mean, na.rm = T)) %>% 
    distinct() %>% 
    mutate(gender = factor(as.character(gender), levels = levels(d$gender)[-3], ordered = F),
           educ = factor(as.character(educ), levels = c("ohne Schulabschluss", stringr::str_sub(names(h1$coefficients)[stringr::str_detect(names(h1$coefficients), "educ")], 5,-1)), ordered = F),
           politiknah = factor(as.character(politiknah), levels = levels(d$politiknah)[-3], ordered = F))
  
  
  for (i in 1:6) {
    
    ivs = c("trust_demo", "satis_parl", "party_membership", "party_work", "q6a_minus_b", "dist_mahal")[i]
    
    hypo = list(h1,h2,h3,h4,h5,h6)[[i]]
    
    grid <- means[rep(1, 10), ]  
    grid[[ivs]] <- var_df[[ivs]]
    
    predicted_probs <- tibble(data.frame(pred.POLR(hypo, newdata = grid, type = "probs"))) %>%
      mutate(party = p,
             indep_var = ivs,
             indep_var_vals = grid[[ivs]]) %>% 
      tidyr::pivot_longer(cols = c("Delegiertenversammlung", "Mitgliederversammlung", "Primaries")) %>% 
      rename(prob = value,
             class = name) %>% 
      rbind(predicted_probs)
    
    rm(ivs,hypo, grid)
  }
  rm(means,d,h1,h2,h3,h4,h5,h6)
}
rm(p,i)

predicted_probs %>% 
  mutate(indep_var = factor(case_when(indep_var == "trust_demo" ~ "Trust in democracy",
                                      indep_var == "satis_parl" ~ "Satisfaction with parliament",
                                      indep_var == "party_membership" ~ "Duration party membership",
                                      indep_var == "party_work" ~ "Monthly hours of party work",
                                      indep_var == "dist_mahal" ~ "Multivariate distance from party", 
                                      TRUE ~ "Perceived distance from party"),
                            levels = c("Trust in democracy","Satisfaction with parliament", "Duration party membership", "Monthly hours of party work", "Perceived distance from party", "Multivariate distance from party")),
         class = factor(case_when(class == "Delegiertenversammlung" ~ "Delegate assembly",
                                  class == "Primaries" ~ "Primary",
                                  T ~ "General meeting"),
                        levels = c("Delegate assembly", "General meeting", "Primary")),
         party = factor(case_when(party == "Bündnis 90/Die Grünen" ~ "A'90/Greens",
                                  party == "Die Linke" ~ "The Left",
                                  T ~ as.character(party)),
                        levels = rev(c("AfD", "A'90/Greens", "CDU/CSU", "FDP", "The Left", "SPD"))),
         afd = factor(ifelse(party == "AfD",1,0))) %>% 
  ggplot(aes(indep_var_vals, prob, color = party, shape = class, alpha = afd, linetype = class)) +
  scale_y_continuous(labels = scales::label_percent(1), limits = c(0,0.8), "Predicted Probabilities") +
  scale_color_manual(values = rev(c("#005EA4", "#0E8C1D", "#000000", "#FFC000", "#CC0066", "#C00000")), "Party") +
  scale_alpha_manual(values = c(0.2,1)) +
  scale_x_continuous(NULL) +
  scale_linetype(NULL) +
  scale_shape(NULL) +
  geom_point() +
  geom_line() +
  guides(alpha = "none", color = "none") +
  facet_wrap(~indep_var,
             scales = "free") +
  theme_light() +
  theme(text = element_text(family = "CMSS"))



rm(predicted_probs,sum_stats,var_df)

## Figure H ----

d <- filter(a, party == "AfD") %>% 
  mutate(transition = factor(case_when(as.numeric(party_leader) <= 2014 ~ "Lucke-Adam-Petry",
                                       as.numeric(party_leader) == 2015 ~ "Transition",
                                       as.numeric(party_leader) > 2015 ~ "Meuthen-Petry"),
                             levels = c("Transition", "Lucke-Adam-Petry", "Meuthen-Petry")),
         lucke = factor(case_when(as.numeric(party_leader) <= 2015 ~ "Lucke-Adam-Petry",
                                  as.numeric(party_leader) > 2015 ~ "Meuthen-Petry"),
                        levels = c("Lucke-Adam-Petry", "Meuthen-Petry")),
         meuthen = factor(case_when(as.numeric(party_leader) <= 2014 ~ "Lucke-Adam-Petry",
                                    as.numeric(party_leader) > 2014 ~ "Meuthen-Petry"),
                          levels = c("Lucke-Adam-Petry", "Meuthen-Petry")))

original_spec <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal, data = d, method = "logistic", Hess = T)
transition_spec <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal + transition, data = d, method = "logistic", Hess = T)
Lucke_spec <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal + lucke, data = d, method = "logistic", Hess = T)
Meuthen_spec <- polr(DV ~ party_membership + educ + gender + trust_demo + dist_mahal + meuthen, data = d, method = "logistic", Hess = T)



ests <- filter(mutate(broom::tidy(original_spec), df = original_spec[["df.residual"]], spec = "Original"), term == "party_membership") %>% 
  rbind(filter(mutate(broom::tidy(transition_spec), df = transition_spec[["df.residual"]], spec = "Transition"), term == "party_membership")) %>% 
  rbind(filter(mutate(broom::tidy(Lucke_spec), df = Lucke_spec[["df.residual"]], spec = "Lucke"), term == "party_membership")) %>%
  rbind(filter(mutate(broom::tidy(Meuthen_spec), df = Meuthen_spec[["df.residual"]], spec = "Meuthen"), term == "party_membership")) %>%
  mutate(conf.low = exp(estimate - qt(0.025, df) * std.error),
         conf.high = exp(estimate + qt(0.025, df) * std.error),
         statistic = estimate/std.error,
         p.value = (1 - pt(abs(statistic),df))*2,
         p.stars = case_when(p.value < 0.001 ~ "***",
                             p.value < 0.01 ~ "**",
                             p.value < 0.05 ~ "*",
                             T ~ ""),
         estimate = exp(estimate),
         p.label = paste0(format(round(estimate, 2), nsmall = 2),p.stars)) %>% 
  select(-coef.type)

ests %>% 
  mutate(term = factor("Duartion party membership"),
         spec = factor(case_when(spec == "Lucke" ~ "Coded with\npre-2015 members",
                                 spec == "Meuthen" ~ "Coded with\npost-2015 members",
                                 spec == "Transition" ~ "Coded as\nown category",
                                 spec == "Original" ~ "Original Specification\n without dummies"),
                       levels = rev(c("Original Specification\n without dummies", "Coded as\nown category", "Coded with\npre-2015 members", "Coded with\npost-2015 members")))) %>%
  ggplot(aes(estimate, spec, xmin = conf.low, xmax = conf.high, label = p.label)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_y_discrete("Coding of members that entered 2015") +
  scale_x_continuous("Est. marginal effect of party membership duration on IPD preference in ORs") +
  geom_pointrange() +
  geom_text(vjust = -1, family = "CMSS", size = 3) +
  theme_light() +
  theme(text = element_text(family = "CMSS"))

showtext::showtext_opts(dpi = dpi)
ggsave(here("Export", "Paper", "Appendix", "figH.pdf"), device = "pdf", width = width, height = height, units = "cm", dpi = dpi)
showtext::showtext_opts(dpi = 96)

rm(original_spec, Lucke_spec, Meuthen_spec, transition_spec)



