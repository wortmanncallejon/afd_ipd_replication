rm(list = ls())

library(MASS)
library(dplyr)
library(haven)
library(here)

get_data <- function() {
  
  a <- read_sav(here("Import", "Data.sav")) %>% 
    mutate(DV = factor(recode(as.numeric(DV),
                              `0` = "Delegiertenversammlung",
                              `1` = "Mitgliederversammlung",
                              `2` = "Primaries",
                              .default = NA_character_))) %>%
    filter(!is.na(DV) & Filter_Sample1 == 1) %>% 
    select(DV, point_nr, fbnr, q16,
           q301_T, q302_T, q303_T,
           q501, q502,
           q20_r, q21_r, Partei, 
           q24_PN, ALTER, ALTER_KAT, ALTER_KAT_2,
           q6a, q6b, q2, q1_Dauer, q4_T, q7, q8, q1020, q1705, q1) %>% 
    rename(id = fbnr,
           event_id = point_nr,
           gender = q20_r,
           age = ALTER,
           age_5 = ALTER_KAT,
           age_10 = ALTER_KAT_2,
           party_membership = q1_Dauer,
           nomination_exp = q4_T,
           lire_econ = q501,
           gal_tan = q502,
           lire_self = q6a,
           lire_party = q6b,
           politiknah = q24_PN,
           educ = q21_r,
           trust_demo = q7,
           satis_parl = q8,
           buergernaehe = q1020,
           party = Partei,
           board = q301_T,
           legis = q302_T,
           exec = q303_T,
           party_work = q2,
           DV_num = q16,
           party_leader = q1) %>% 
    mutate(across(buergernaehe:q1705, ~  .x * -1 + 5),
           across(gender:politiknah, as_factor),
           nomination_exp = relevel(factor(ifelse(is.na(nomination_exp), "Nein", "Ja")), ref = "Nein"),
           across(board:exec, ~ factor(case_when(.x == 1 ~ "Lokal-, Kreis-, Bezirk- oder Unterbezirk",
                                                 .x == 2 ~ "Land",
                                                 .x == 3 ~ "Bund & Europa",
                                                 is.na(.x) ~ "Keins",
                                                 TRUE ~ "Keins"),
                                       levels = c("Keins", "Lokal-, Kreis-, Bezirk- oder Unterbezirk", "Land", "Bund & Europa"))),
           q6a_minus_b = lire_self - lire_party,
           DV_num = as.numeric(DV_num),
           abs_q6a_minus_b = abs(lire_self - lire_party),
           party = factor(case_when(party == "CDU" ~ "CDU/CSU",
                                    party == "CSU" ~ "CDU/CSU",
                                    T ~ as.character(party))))
  
  dist <- tibble(id = character(0),
                 dist_mahal = numeric(0))
  
  events <- a %>% 
    select(party, id, lire_self, lire_econ, gal_tan) %>% 
    split(.$party)
  
  
  for(event in events[1:6]) {
    
    means <- event %>% 
      tidyr::pivot_longer(-c(id, party)) %>% 
      group_by(name) %>% 
      summarise(value = mean(value, na.rm = T)) %>%  .$value
    
    dist <- tibble(id = event$id,
                   dist_mahal = mahalanobis(event[3:5], means, cov(event[3:5], use = "pairwise.complete.obs"))) %>% 
      rbind(dist)
  }
  
  a <- a %>% inner_join(dist, by = "id")
  
  rm(dist, events, event, means)
  
  
  return(a)
}

pred.POLR <-  function (object, newdata, type = c("class", "probs"), ...) 
{
  newdata <- as.data.frame(newdata)
  Terms <- delete.response(object$terms)
  m <- model.frame(Terms, newdata)
  if (!is.null(cl <- attr(Terms, "dataClasses"))) .checkMFClasses(cl, m)
  X <- model.matrix(Terms, m, contrasts = object$contrasts)
  xint <- match("(Intercept)", colnames(X), nomatch = 0L)
  if (xint > 0L) X <- X[, -xint, drop = FALSE]
  n <- nrow(X)
  q <- length(object$zeta)
  eta <- drop(X %*% object$coefficients)
  pfun <- switch(object$method, logistic = plogis, probit = pnorm, 
                 loglog = pgumbel, cloglog = pGumbel, cauchit = pcauchy)
  cumpr <- matrix(pfun(matrix(object$zeta, n, q, byrow = TRUE) - 
                         eta), , q)
  Y <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
  dimnames(Y) <- list(rownames(X), object$lev)
  
  if (type == "class") factor(max.col(Y), levels = seq_along(object$lev), labels = object$lev)
  else drop(Y)
}
