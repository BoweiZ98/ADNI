# Import ADNI Dataset
install.packages("./ADNIMERGE_0.0.1.tar.gz", repos = NULL, type = "source")
library(ADNIMERGE)
data(adnimerge)

# Survival Dataset
library(tidyverse)
target_id = adnimerge %>%
  filter(M <= 60) %>% # use only first 5 years
  filter(DX.bl != "AD") %>% # exclude patients having AD at baseline
  select(RID) %>%
  unique() %>%
  pull(RID)

adni_survival = adnimerge %>%
  filter(RID %in% target_id & M <= 60) %>% # use only the first 5 years
  mutate(M = as.numeric(M)) %>%
  group_by(RID) %>%
  arrange(RID, M) %>%
  mutate(first_dementia = cumsum(DX == "Dementia" & !is.na(DX))) %>%
  filter(first_dementia <= 1) %>%
  ungroup() %>%
  select(-first_dementia) %>%
  mutate(status = case_when(
    DX == "Dementia" ~ 1,
    TRUE ~ 0
  )) %>%
  select(
    # ID and time
    RID, M, status,
    # Sociodemographic 
    PTGENDER, AGE, PTEDUCAT, PTMARRY,
    # subtypes of MCI 
    DX.bl, DX,
    # clinical scales
    CDRSB, CDRSB, FAQ,
    # Neurosychological tests
    MMSE, ADAS11, ADAS13,ADASQ4, 
    RAVLT.immediate, RAVLT.learning, RAVLT.forgetting,
    RAVLT.perc.forgetting, LDELTOTAL, TRABSCOR,
  ) 

write.csv(adni_survival, 
          file = "adni_survival.csv",
          row.names = F)

# Longitudinal Dataset

adni_long = adnimerge %>%
  filter(RID %in% target_id & M <= 60) %>%
  select(
    # ID and time
    RID, M,
    # subtypes of MCI 
    DX.bl, DX,
    # Sociodemographic 
    PTGENDER, AGE, PTEDUCAT, PTMARRY,
    # clinical scales
    CDRSB, CDRSB, FAQ,
    # Neurosychological tests
    MMSE, ADAS11, ADAS13,ADASQ4, 
    RAVLT.immediate, RAVLT.learning, RAVLT.forgetting,
    RAVLT.perc.forgetting, LDELTOTAL, TRABSCOR,
  ) 

write.csv(adni_long, 
          file = "adni_long.csv",
          row.names = F)  

# Baseline

converter_id = adnimerge %>%
  filter(DX.bl != "AD" & DX == "Dementia") %>%
  select(RID) %>%
  unique() %>%
  pull(RID)

non_converter_id = setdiff(target_id, converter_id)

adni_baseline = adnimerge %>%
  filter(RID %in% target_id & M <= 60 & VISCODE == "bl") %>% # use only the first 5 years
  mutate(converter = case_when(
    RID %in% converter_id ~ 1,
    RID %in% non_converter_id ~ 0
  ),
    Month = as.numeric(Month),
    DX) %>%
  select(
    # ID
    RID,
    # outcome
    converter,
    # Sociodemographic 
    PTGENDER, AGE, PTEDUCAT, PTMARRY,
    # subtypes of MCI at baseline
    DX.bl,
    # clinical scales
    CDRSB, CDRSB, FAQ,
    # Neurosychological tests
    MMSE, ADAS11, ADAS13,ADASQ4, 
    RAVLT.immediate, RAVLT.learning, RAVLT.forgetting,
    RAVLT.perc.forgetting, LDELTOTAL, TRABSCOR,
  )

gtsummary::tbl_summary(adni_baseline, b = converter) 

write.csv(adni_baseline, 
          file = "adni_baseline.csv",
          row.names = F)  
