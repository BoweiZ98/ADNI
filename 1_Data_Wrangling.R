# Import ADNI Dataset
install.packages("./ADNIMERGE_0.0.1.tar.gz", repos = NULL, type = "source")
library(ADNIMERGE)
data(adnimerge)

# Data Manipulation 
library(tidyverse)
mci_id = adnimerge %>%
  filter(DX.bl %in% c("EMCI", "LMCI")) %>%
  select(RID) %>%
  unique() %>%
  pull(RID)


converter_id = adnimerge %>%
  filter(DX.bl %in% c("EMCI", "LMCI") & DX == "Dementia") %>%
  select(RID) %>%
  unique() %>%
  pull(RID)

non_converter_id = setdiff(mci_id, converter_id)

adni_mci = adnimerge %>%
  filter(RID %in% mci_id & VISCODE == "bl") %>% # baseline visit
  mutate(converter = case_when(
    RID %in% converter_id ~ 1,
    RID %in% non_converter_id ~ 0
  )) %>%
  select(
    # Sociodemographic 
    PTGENDER, AGE, PTEDUCAT, PTMARRY,
    # subtypes of MCI
    DX.bl,
    # clinical scales
    CDRSB.bl, CDRSB.bl, FAQ.bl,
    # Neurosychological tests
    MMSE.bl, ADAS11.bl, ADAS13.bl,ADASQ4.bl, 
    RAVLT.immediate.bl, RAVLT.learning.bl, RAVLT.forgetting.bl,
    RAVLT.perc.forgetting.bl, LDELTOTAL.bl, TRABSCOR.bl,
    # outcome
    converter
  )

gtsummary::tbl_summary(adni_mci, b = converter)   
