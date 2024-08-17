# VFN - Psychologie - parovani dat z iMed
# ------------------------------------------
# Jiri Drahota / jiri.drahota@vfn.cz (hlavni autor)
# Jiri Motyl / jiri.motyl@vfn.cz (rozsireni scriptu o dalsi polozky)
# 
# Vstupy: 
#    1. Data z RedCap se sloupci record_id (= iMed ID), 
#       psycho_date (= datum psychologickeho vysetreni)
#    2. iMed - Identification, Visits, Treatments, Relapses
# Vystup = doplneni zaznamu psychologickeho vysetreni o :
#    A. EDSS (nejblizsi hodnota vzhledem k psycho_date) (-je pred psycho_date, +po psycho_date)
#    B. Lecba (aktivni nebo posledni lecba)
#    C. Pocet relapsu v poslednich 12 mesicich, prumerna severita (1 = MILD, 2 = MODERATE, 3 = SEVERE)
#    D. Relaps do 30 dnu pred psycho_date - TRUE / FALSE, prumerna severita (1 = MILD, 2 = MODERATE, 3 = SEVERE)
#    E. Relaps do 30 dnu po psycho_date - TRUE / FALSE, prumerna severita (1 = MILD, 2 = MODERATE, 3 = SEVERE)
#    F. Relaps do 90 dnu pred psycho_date - TRUE / FALSE, prumerna severita (1 = MILD, 2 = MODERATE, 3 = SEVERE)
#    G. Relaps do 90 dnu po psycho_date - TRUE / FALSE, prumerna severita (1 = MILD, 2 = MODERATE, 3 = SEVERE)
#    H. Disease Duration (v letech) . délka trvání nemoci od začátku onemocnění po psychologické vyšetření + Date of onset (datum počátku nemoci)


#  / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / / /

library(data.table)
library(lubridate)

# ---- 1/ Settings ----
  iMed_prefix <- "export_imed_230314_"
  neuropsych_data_file <- "scg_export.csv"
  med_file <- "Medication_list.csv"
  loc_rawDataExportCutOffDate <- as.Date("2023-03-14")
  
# ---- 2/ Data loading ----
  # Neuropsychology file load
      psych_visits <- as.data.table(read.csv(paste0("data/",neuropsych_data_file),encoding = "UTF-8"))
      str(psych_visits)
      setnames(psych_visits, 1, "record_id")
  
  # iMed LOAD 
      Visits <- as.data.table(read.delim(paste0("data/", iMed_prefix,"VI.txt"), header = TRUE))
      Treatments <- as.data.table(read.delim(paste0("data/", iMed_prefix,"TR.txt"), header = TRUE))
      Relapses <- as.data.table(read.delim(paste0("data/", iMed_prefix,"RE.txt"), header = TRUE))
      Identification <- as.data.table(read.delim(paste0("data/", iMed_prefix,"ID.txt"), header = TRUE))
      # Medication list
      Medication <- as.data.table(read.csv2(med_file, fileEncoding="UTF-8-BOM"))
      
  # Promazani dat z iMedu a ponechani pouze pacientu s psychologickym vysetrenim
      used_iMed_IDs <- unique(psych_visits$record_id)
      Identification <- Identification[Patient.ID %in% used_iMed_IDs,]
      Treatments <- Treatments[Patient.ID %in% used_iMed_IDs,]
      Relapses <- Relapses[Patient.ID %in% used_iMed_IDs,]
      Visits <- Visits[Patient.ID %in% used_iMed_IDs,]
      
# ---- 3/ Data adjustment ----
  # Seznam tabulek
      tbl_list <- c("Identification", "Relapses", "Treatments", "Visits", "psych_visits")
      
  # Nastaveni datumovych sloupcu na format datum
      psych_visits$psycho_date <- as.Date(psych_visits$psycho_date, "%Y-%m-%d")
      Relapses$Relapse.Date <- as.Date(Relapses$Relapse.Date, "%d.%m.%Y")
      Visits$Visit.Date <- as.Date(Visits$Visit.Date, "%d.%m.%Y")
      Treatments$Start.Date <- as.Date(Treatments$Start.Date, "%d.%m.%Y")
      Treatments$End.Date <- as.Date(Treatments$End.Date, "%d.%m.%Y")
      Identification$Date.of.onset <- as.Date(Identification$Date.of.onset, "%d.%m.%Y")
      
  # EDSS jako numericka hodnota
      # Visits$EDSS <-  as.numeric(sub(",", ".", Visits$EDSS, fixed = TRUE))
      Visits$EDSS <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-9]+),([0-9]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$EDSS
      ))
  # Funkční subskóry jako numerická hodnota  
      #Pyramidal
      # Visits$Score.Pyramidal <-  as.numeric(sub(",", ".", Visits$Score.Pyramidal, fixed = TRUE))
      Visits$Score.Pyramidal <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-6]+),([0-6]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Pyramidal
      ))
      
      #Cerebellar
      # Visits$Score.Cerebellar <-  as.numeric(sub(",", ".", Visits$Score.Cerebellar, fixed = TRUE))
      Visits$Score.Cerebellar <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-5]+),([0-5]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Cerebellar
      ))
      
      #BrainStem
      # Visits$Score.BrainStem <-  as.numeric(sub(",", ".", Visits$Score.BrainStem, fixed = TRUE))
      Visits$Score.BrainStem <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-5]+),([0-5]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.BrainStem
      ))
      
      #Sensory
      # Visits$Score.Sensory <-  as.numeric(sub(",", ".", Visits$Score.Sensory, fixed = TRUE))
      Visits$Score.Sensory <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-6]+),([0-6]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Sensory
      ))
      
      #Bowel.Bladder
      # Visits$Score.Bowel.Bladder <-  as.numeric(sub(",", ".", Visits$Score.Bowel.Bladder, fixed = TRUE))
      Visits$Score.Bowel.Bladder <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-6]+),([0-6]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Bowel.Bladder
      ))
      
      #Visual
      # Visits$Score.Visual <-  as.numeric(sub(",", ".", Visits$Score.Visual, fixed = TRUE))
      Visits$Score.Visual <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-6]+),([0-6]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Visual
      ))
      
      #Mental
      # Visits$Score.Mental <-  as.numeric(sub(",", ".", Visits$Score.Mental, fixed = TRUE))
      Visits$Score.Mental <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-5]+),([0-5]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Mental
      ))
      
      #Ambulation
      # Visits$Score.Ambulation <-  as.numeric(sub(",", ".", Visits$Score.Ambulation, fixed = TRUE))
      Visits$Score.Ambulation <- as.numeric(gsub(
        # ONLY for strings containing numerics, comma, numerics
        "^([0-12]+),([0-12]+)$", 
        # Substitute by the first part, dot, second part
        "\\1.\\2", 
        Visits$Score.Ambulation
      ))
      
  # Propojeni DMD lecby do kontinualniho formatu
    
      
# ---- 4/ Calculations ----
      
    # ---- 4A) EDSS ----
      psych_visits$EDSS_closest <- as.numeric(NA)
      psych_visits$EDSS_days <- as.numeric(NA)
      psych_visits$FS_Pyramidal <- as.numeric(NA)
      psych_visits$FS_Cerebellar <- as.numeric(NA)
      psych_visits$FS_BrainStem <- as.numeric(NA)
      psych_visits$FS_Sensory <- as.numeric(NA)
      psych_visits$FS_Bowel <- as.numeric(NA)
      psych_visits$FS_Visual <- as.numeric(NA)
      psych_visits$FS_Mental <- as.numeric(NA)
      psych_visits$FS_Ambulation <- as.numeric(NA)
            
      pb = txtProgressBar(min = 1, max = nrow(psych_visits), initial = 1, style=3) 
      for (i in 1:nrow(psych_visits)) {
        setTxtProgressBar(pb,i)
        if (!is.na(psych_visits[i]$psycho_date)) {
          tmp_patient_ID <- psych_visits[i]$record_id
          tmp_psycho_date <- psych_visits[i]$psycho_date
          
          tmp_Visits <- Visits[Patient.ID == tmp_patient_ID,]
          tmp_Visits$Psycho_date <- tmp_psycho_date
          tmp_Visits$tmp_time_dist <- as.numeric(difftime(tmp_Visits$Visit.Date, tmp_Visits$Psycho_date, units = "days"))
          tmp_Visits$tmp_time_dist_abs <- abs(tmp_Visits$tmp_time_dist)        
          tmp_Visits <- tmp_Visits[order(tmp_time_dist_abs)]
          if (nrow(tmp_Visits)>0) {
            psych_visits[i]$EDSS_closest <- tmp_Visits[1]$EDSS
            psych_visits[i]$EDSS_days <- tmp_Visits[1]$tmp_time_dist
            psych_visits[i]$FS_Pyramidal <- tmp_Visits[1]$Score.Pyramidal
            psych_visits[i]$FS_Cerebellar <- tmp_Visits[1]$Score.Cerebellar
            psych_visits[i]$FS_BrainStem <- tmp_Visits[1]$Score.BrainStem
            psych_visits[i]$FS_Sensory <- tmp_Visits[1]$Score.Sensory
            psych_visits[i]$FS_Bowel<- tmp_Visits[1]$Score.Bowel.Bladder
            psych_visits[i]$FS_Visual <- tmp_Visits[1]$Score.Visual
            psych_visits[i]$FS_Mental <- tmp_Visits[1]$Score.Mental
            psych_visits[i]$FS_Ambulation <- tmp_Visits[1]$Score.Ambulation
            }
        }
      }

    # ---- 4B) LECBA DMD ----
      psych_visits$DMD <- as.character(NA)
      psych_visits$DMD_active <- as.logical(NA)
      
      source("DMD_treatments_connector.R")
      
      for (i in 1:nrow(psych_visits)) {
        setTxtProgressBar(pb,i)
        if (!is.na(psych_visits[i]$psycho_date)) {
          tmp_patient_ID <- psych_visits[i]$record_id
          tmp_psycho_date <- psych_visits[i]$psycho_date
          
          tmp_Treatments_connected_DMD <- Treatments_connected_DMD[Patient.ID == tmp_patient_ID,]
          tmp_Treatments_connected_DMD$Psycho_date <- tmp_psycho_date
          tmp_Treatments_connected_DMD <- tmp_Treatments_connected_DMD[Start.Date <= Psycho_date, ][order(-Start.Date)]
          if (nrow(tmp_Treatments_connected_DMD)>0) {
            psych_visits[i]$DMD <- tmp_Treatments_connected_DMD[1]$Medication
            psych_visits[i]$DMD_active <- ifelse(tmp_psycho_date < tmp_Treatments_connected_DMD[1]$End.Date, TRUE, FALSE)
          }
        }
      }
      
    # ---- 4C,D,E,F,G) RELAPSY -12M, +/-30D, +/-90D,  ----
      psych_visits$Rel_365bef_count <- as.numeric(NA)
      psych_visits$Rel_365bef_avg_sev <- as.numeric(NA)
      psych_visits$Rel_30bef_count <- as.numeric(NA)
      psych_visits$Rel_30bef_avg_sev <- as.numeric(NA)
      psych_visits$Rel_30aft_count <- as.numeric(NA)
      psych_visits$Rel_30aft_avg_sev <- as.numeric(NA)
      psych_visits$Rel_90bef_count <- as.numeric(NA)
      psych_visits$Rel_90bef_avg_sev <- as.numeric(NA)
      psych_visits$Rel_90aft_count <- as.numeric(NA)
      psych_visits$Rel_90aft_avg_sev <- as.numeric(NA)
      
      for (i in 1:nrow(psych_visits)) {
        setTxtProgressBar(pb,i)
        if (!is.na(psych_visits[i]$psycho_date)) {
          tmp_patient_ID <- psych_visits[i]$record_id
          tmp_psycho_date <- psych_visits[i]$psycho_date
          
          tmp_Relapses <- Relapses[Patient.ID == tmp_patient_ID,]
          tmp_Relapses$Psycho_date <- tmp_psycho_date
          tmp_Relapses$tmp_time_dist <- as.numeric(difftime(tmp_Relapses$Relapse.Date, tmp_Relapses$Psycho_date, units = "days"))
          tmp_Relapses$Severity_num <- as.numeric(NA)
          tmp_Relapses$Severity_num <- ifelse(tmp_Relapses$Severity == "Mild", 1,
                                              ifelse(tmp_Relapses$Severity == "Moderate", 2, 
                                                     ifelse(tmp_Relapses$Severity == "Severe", 3, NA)))
          
      # Relapsy 365 dni pred psychologickym pohovorem (vcetne)
          tmp_Relapses_sorted <- tmp_Relapses[tmp_time_dist>=-365 & tmp_time_dist<=0,]
          if (nrow(tmp_Relapses_sorted)>0) {
            psych_visits[i]$Rel_365bef_count <- nrow(tmp_Relapses_sorted)
            psych_visits[i]$Rel_365bef_avg_sev <- mean(tmp_Relapses_sorted$Severity_num)
          }
      # Relapsy 30 dni pred psychologickym pohovorem (vcetne)
          tmp_Relapses_sorted <- tmp_Relapses[tmp_time_dist>=-30 & tmp_time_dist<=0,]
          if (nrow(tmp_Relapses_sorted)>0) {
            psych_visits[i]$Rel_30bef_count <- nrow(tmp_Relapses_sorted)
            psych_visits[i]$Rel_30bef_avg_sev <- mean(tmp_Relapses_sorted$Severity_num)
          }
          
      # Relapsy 30 dni po psychologickym pohovorem (vcetne)
          tmp_Relapses_sorted <- tmp_Relapses[tmp_time_dist<=30 & tmp_time_dist>0,]
          if (nrow(tmp_Relapses_sorted)>0) {
            psych_visits[i]$Rel_30aft_count <- nrow(tmp_Relapses_sorted)
            psych_visits[i]$Rel_30aft_avg_sev <- mean(tmp_Relapses_sorted$Severity_num)
          }

          # Relapsy 90 dni pred psychologickym pohovorem (vcetne)
          tmp_Relapses_sorted <- tmp_Relapses[tmp_time_dist>=-90 & tmp_time_dist<=0,]
          if (nrow(tmp_Relapses_sorted)>0) {
            psych_visits[i]$Rel_90bef_count <- nrow(tmp_Relapses_sorted)
            psych_visits[i]$Rel_90bef_avg_sev <- mean(tmp_Relapses_sorted$Severity_num)
          }
          
          # Relapsy 90 dni po psychologickym pohovorem (vcetne)
          tmp_Relapses_sorted <- tmp_Relapses[tmp_time_dist<=90 & tmp_time_dist>0,]
          if (nrow(tmp_Relapses_sorted)>0) {
            psych_visits[i]$Rel_90aft_count <- nrow(tmp_Relapses_sorted)
            psych_visits[i]$Rel_90aft_avg_sev <- mean(tmp_Relapses_sorted$Severity_num)
          }                    
          
        }
      }
      
      # ---- 4H) Disease duration ----
      psych_visits$disease_duration <- as.numeric(NA)
      psych_visits$date_of_onset <- as.Date(as.character(NA),format="%Y%m%d")
    
      
      pb = txtProgressBar(min = 1, max = nrow(psych_visits), initial = 1, style=3) 
      for (i in 1:nrow(psych_visits)) {
        setTxtProgressBar(pb,i)
        if (!is.na(psych_visits[i]$psycho_date)) {
          tmp_patient_ID <- psych_visits[i]$record_id
          tmp_psycho_date <- psych_visits[i]$psycho_date
          
          tmp_Identification <- Identification[Patient.ID == tmp_patient_ID,]
          tmp_Identification$Psycho_date <- tmp_psycho_date
          tmp_Identification$tmp_time_dist <- as.numeric(lubridate::time_length(difftime(tmp_Identification$Psycho_date, tmp_Identification$Date.of.onset, units = "days"),"years"))
          if (nrow(tmp_Identification)>0) {
            psych_visits[i]$disease_duration <- tmp_Identification[1]$tmp_time_dist
            psych_visits[i]$date_of_onset <- tmp_Identification[1]$Date.of.onset
            
          }
        }
      }
      
      rm( tmp_Relapses, tmp_Relapses_sorted, tmp_Identification)
      rm(pb, tmp_Visits, tmp_patient_ID, tmp_psycho_date)
      
  write.csv(psych_visits, file = paste0(neuropsych_data_file, "_filled.csv"))      
  
  library(haven)
  write_sav(psych_visits, path = "spss_data_file.sav", compress = c("byte", "none", "zsav"), adjust_tz = TRUE)
  