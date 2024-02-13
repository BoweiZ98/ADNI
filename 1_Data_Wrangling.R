# Import ADNI Dataset
install.packages("./ADNIMERGE_0.0.1.tar.gz", repos = NULL, type = "source")
library(ADNIMERGE)
data(adnimerge)

# Baseline
library(tidyverse)

target_id = adnimerge %>%
  filter(M <= 60) %>% # use only first 5 years
  filter(DX.bl %in% c("EMCI", "LMCI")) %>% # only include MCI patients at baseline
  select(RID) %>%
  unique() %>%
  pull(RID)

converter_id = adnimerge %>%
  filter(M <= 60) %>%
  filter(DX.bl %in% c("EMCI", "LMCI") & DX == "Dementia") %>%
  select(RID) %>%
  unique() %>%
  pull(RID)

non_converter_id = setdiff(target_id, converter_id)

adni_baseline = adnimerge %>%
  filter(RID %in% target_id & M <= 60 & VISCODE == "bl") %>% # use only the first 5 years
  mutate(converter = case_when(
    RID %in% converter_id ~ 1,
    RID %in% non_converter_id ~ 0),
    Month = as.numeric(Month),
    PTMARRY = as.factor(PTMARRY),
    PTMARRY = na_if(PTMARRY, "Unknown")
  ) %>%
  select(
    # outcome
    converter,
    # Sociodemographic 
    PTGENDER, AGE, PTEDUCAT, PTMARRY,
    # subtypes of MCI at baseline
    DX.bl,
    # clinical scales
    CDRSB, FAQ,
    # Neurosychological tests
    MMSE, ADAS11, ADAS13,ADASQ4, 
    RAVLT.immediate, RAVLT.learning, RAVLT.forgetting,
    RAVLT.perc.forgetting, LDELTOTAL, TRABSCOR,
  )

# Missing Value Imputation
library(mice)
imputation = mice(
  adni_baseline,
  m = 10,
  method = "pmm",
  maxit = 10,
  seed = 1
)
adni_imputed = complete(imputation,1)

# Train/Test Split
set.seed(2024)
n = nrow(adni_imputed)
indices = sample(n, 0.7*n)
train = adni_imputed[indices,]
test = adni_imputed[-indices,]

# Write Out CSV
write.csv(adni_imputed, 
          file = "adni_baseline.csv",
          row.names = F)  
write.csv(train, 
          file = "train.csv",
          row.names = F)  
write.csv(test, 
          file = "test.csv",
          row.names = F)  

# completer

completer_id = adnimerge %>%
  filter(M == 60) %>% # use only first 5 years
  filter(DX.bl %in% c("EMCI", "LMCI")) %>%
  select(RID) %>%
  unique() %>%
  pull(RID)

adni_completer = adnimerge %>%
  filter(RID %in% completer_id & M <= 60 & VISCODE == "bl") %>% # use only the first 5 years
  mutate(converter = case_when(
    RID %in% converter_id ~ 1,
    RID %in% non_converter_id ~ 0),
    Month = as.numeric(Month),
    PTMARRY = as.factor(PTMARRY),
    PTMARRY = na_if(PTMARRY, "Unknown")
  ) %>%
  select(
    # outcome
    converter,
    # Sociodemographic 
    PTGENDER, AGE, PTEDUCAT, PTMARRY,
    # subtypes of MCI at baseline
    DX.bl,
    # clinical scales
    CDRSB, FAQ,
    # Neurosychological tests
    MMSE, ADAS11, ADAS13,ADASQ4, 
    RAVLT.immediate, RAVLT.learning, RAVLT.forgetting,
    RAVLT.perc.forgetting, LDELTOTAL, TRABSCOR,
  )

# missing

imputation = mice(
  adni_completer,
  m = 10,
  method = "pmm",
  maxit = 10,
  seed = 1
)
adni_completer = complete(imputation,1)

# Train/Test Split
set.seed(2024)
n = nrow(adni_completer)
indices = sample(n, 0.7*n)
train = adni_completer[indices,]
test = adni_completer[-indices,]

# Write Out CSV
write.csv(adni_completer, 
          file = "adni_completer.csv",
          row.names = F)  
write.csv(train, 
          file = "train_completer.csv",
          row.names = F)  
write.csv(test, 
          file = "test_completer.csv",
          row.names = F)  
