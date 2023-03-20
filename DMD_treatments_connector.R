### DMD_treatments_connector.R  | ReMuS z dat iMed - v2019-12
### Jiri Drahota, 2020 | jiridrahota@multiplesclerosis.cz
### Popis: Provede spojeni relevantnich zaznamu o lecbe dle pravidel definice DMD/IVIG (vytvořeno na datech iMed - v2019-12)

  # Pravidla pro určení expozice DMD/IVIG:
  #     Mavenclad, Lemtrada                  - neomezeně, pokud je posledním nasazeným DMD
  #     Ocrevus, Mabthera, generika Mabthery - 2 roky od posledního nasazení
  #     ostatní LP                           - ukončeny dle "End Date"
  
library(lubridate)

    Treatments_connected <- Treatments
    
    print(paste("Nacteno ", nrow(Treatments_connected), "zaznamu o lecbe ke zpracovani a spojeni..."))
    
    Treatments_connected <- Treatments_connected[is.na(Treatments_connected$End.Date) | 
                                                   as.numeric(Treatments_connected$End.Date)>=as.numeric(as.Date("2013-01-01")) |
                                                   Treatments_connected$Treatment.Name %like% "Lemtrada"
                                                 ,]
    
    tmp_N_records_to_process <- nrow(Treatments_connected)
    print(paste("Po aplikaci casoveho omezeni bude zpracovano", tmp_N_records_to_process, "zaznamu o lecbe."))
    
    Treatments_connected$Medication <- Medication$Medication[match(Treatments_connected$Treatment.Name, Medication$Treatment.name, nomatch = NA)] 
    Treatments_connected$Category <- Medication$Category[match(Treatments_connected$Treatment.Name, Medication$Treatment.name, nomatch = NA)]

    # ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! 
    # ! ! ! !         TEST SOUBĚHU LÉČBY        ! ! ! ! 
    
    # Proces zkontroluje, zda nektery z pacientu nema dve neukoncene DMD lecby - delalo by to problem pri urcovani aktivni linie lecby
        
        tmp_Treatments_TEST_SOUBEHU <- Treatments_connected[Treatments_connected$Category %in% c("DMD")]
        setorderv(tmp_Treatments_TEST_SOUBEHU, 'Start.Date', order=1L, na.last=FALSE)
        tmp_Treatments_TEST_SOUBEHU$Unfinished <- ifelse(is.na(tmp_Treatments_TEST_SOUBEHU$End.Date), 1, 0)
        
        tmp_unfinished <- aggregate(x=tmp_Treatments_TEST_SOUBEHU$Unfinished, by=list(tmp_Treatments_TEST_SOUBEHU$Patient.ID), FUN="sum")
        print("Pacienti s dvema a vice neukoncenymi lecbami v registru - NUTNO ZKONTROLOVAT MANUALNE a pred reportem opravit")
        
        print("Pokud tato tabulka obsahuje 0 radku, neni treba nic upravovat.")  
        print(tmp_unfinished[tmp_unfinished$x>1,])
        rm(tmp_unfinished, tmp_Treatments_TEST_SOUBEHU)
    
    

Treatments_connected_IVIG <- Treatments_connected[Treatments_connected$Category %in% c("IVIG")]
Treatments_connected_no_interest <- Treatments_connected[Treatments_connected$Category %in% c("Not Interested")]
Treatments_connected <- Treatments_connected[Treatments_connected$Category %in% c("DMD")]

Treatments_connected <- Treatments_connected[!Treatments_connected$Treatment.Name %in% c("Ocrevus_doba", "Tysabri_infuze", "Mabthera_doba")] #Odstranění redundantních zápisů
Treatments_connected <- Treatments_connected[!(Treatments_connected$Treatment.Name %in% c("Mavenclad_doba") & is.na(Treatments_connected$End.Date))] #Odstranění redundantních zápisů

#  * * * * * * odsud zopakovat totéž pro IVIG  * * * * * *

setorder(Treatments_connected, "Patient.ID", "Start.Date")

Treatments_connected$Connect_to_next_row <- FALSE

# označí ty řádky s léčbou, které se mají spojit s řádkem následujícím
print(paste("=== DMD: Oznaceni zaznamu urcenych ke spojeni ==============="))

for (i in 1:(nrow(Treatments_connected)-1)) {
  if (i %% 5000 == 0 | i ==1 | i==(nrow(Treatments_connected)-1)) 
    print(paste("...zpracovavam zaznam c. ", i, "prubeh:", round(i/(nrow(Treatments_connected)-1)*100,0), "%"))
  
  if (Treatments_connected$Medication[i] %in% c("Lemtrada", "Mavenclad")) {
    Treatments_connected$Connect_to_next_row[i] <- 
      ifelse((Treatments_connected$Medication[i]==Treatments_connected$Medication[i+1]) &                                       #Kontrola, že na dalším řádku je stejný lék
               Treatments_connected$"Patient.ID"[i]==Treatments_connected$"Patient.ID"[i+1],TRUE,FALSE)                                         #Kontrola, že na dalším řádku je stejný pacient
  }  else if (Treatments_connected$Medication[i] %in% c("Ocrevus", "Mabthera", "generika Mabthery")) {
    Treatments_connected$Connect_to_next_row[i] <- 
      ifelse((Treatments_connected$Medication[i]==Treatments_connected$Medication[i+1]) &                                       #Kontrola, že na dalším řádku je stejný lék
               (difftime(Treatments_connected$Start.Date[i+1], Treatments_connected$End.Date[i], unit="days")<(2*365.25) |  #Kontrola, že další podání nebylo déle než po 2 letech
                  difftime(Treatments_connected$Start.Date[i+1], Treatments_connected$Start.Date[i], unit="days")<(2*365.25)) &   
               Treatments_connected$"Patient.ID"[i]==Treatments_connected$"Patient.ID"[i+1],TRUE,FALSE)                                         #Kontrola, že na dalším řádku je stejný pacient
  } else {
    ifelse((Treatments_connected$Medication[i]==Treatments_connected$Medication[i+1]) &                                       #Kontrola, že na dalším řádku je stejný lék
             difftime(Treatments_connected$Start.Date[i+1], Treatments_connected$End.Date[i], unit="days")<=1 &           #Kontrola, že další podání nebylo déle než po 1 dnu
             Treatments_connected$"Patient.ID"[i]==Treatments_connected$"Patient.ID"[i+1],TRUE,FALSE)                                         #Kontrola, že na dalším řádku je stejný pacient
  }
}


# Do označených řádků přenese z následujícího řádku "End Date" a "Reason for Discontinuation"; parametry Dosage, Unit, Frequency nastaví na NA

Treatments_connected$"Delete row" <- FALSE

print(paste("=== DMD: Spojovani lecby a odstraneni redundantnich zaznamu ==============="))

for (i in (nrow(Treatments_connected)-1):1) {
  if (((i-(nrow(Treatments_connected)-1)) %% 5000)==0 | 
          i == (nrow(Treatments_connected)-1) |
          i==1) 
    print(paste("...zpracovavam zaznam c. ", (nrow(Treatments_connected)-1)-i, "prubeh:", round(
      ((nrow(Treatments_connected)-1)-i)/(nrow(Treatments_connected)-1)*100,0), "%"))
  
  if (Treatments_connected$Connect_to_next_row[i]) {
    Treatments_connected$End.Date[i] <- Treatments_connected$End.Date[i+1]
    Treatments_connected$Reason.for.Discontinuation[i] <- Treatments_connected$Reason.for.Discontinuation[i+1]
    Treatments_connected$Dosage[i] <- NA
    Treatments_connected$Unit[i] <- NA
    Treatments_connected$Frequency[i] <- NA
    Treatments_connected$"Delete row"[i+1] <- TRUE
    
  } 
}

# Odstraní redundantní záznamy o léčbě
Treatments_connected <- Treatments_connected[!Treatments_connected$`Delete row`]
Treatments_connected$"Delete row" <- NULL
Treatments_connected$Connect_to_next_row <- NULL

# Prodlouží trvání pusních léků podle definice DMD - Mavenclad a Lemtrada do CutOffDate + den a Ocrevus, Mabthera a její generika o 2 roky (pokud nenásledoval jiný DMD)

print(paste("=== DMD: Prodluzovani ucinnosti pulsni lecby dle definice DMD v. 2019-01 ==============="))

for (i in 1:(nrow(Treatments_connected)-1)) {
  if (i %% 5000 == 0 | i ==1 | i==(nrow(Treatments_connected)-1)) 
    print(paste("...zpracovavam zaznam c. ", i, "prubeh:", round(i/(nrow(Treatments_connected)-1)*100,0), "%"))
  
  if (Treatments_connected$Medication[i] %in% c("Lemtrada", "Mavenclad")) {
    Treatments_connected$End.Date[i] <- as.Date(ifelse(Treatments_connected$"Patient.ID"[i]!=Treatments_connected$"Patient.ID"[i+1], #Pokud je na dalším řádku jiný pacient, prodlouží do CutOff+1D; pokud je pacient totožný, prodlouží těsně před začátek další léčby
                                                         loc_rawDataExportCutOffDate %m+% days(1), Treatments_connected$Start.Date[i+1] %m+% days(-1)),  origin = "1970-01-01")
  }  else if (Treatments_connected$Medication[i] %in% c("Ocrevus", "Mabthera", "generika Mabthery")) {
    Treatments_connected$End.Date[i] <- as.Date(ifelse(Treatments_connected$"Patient.ID"[i]!=Treatments_connected$"Patient.ID"[i+1], #Pokud je na dalším řádku jiný pacient, prodlouží do EndDate+2R; pokud je pacient totožný, prodlouží těsně před začátek další léčby
                                                         Treatments_connected$End.Date[i] %m+% years(2) %m+% days(-1), Treatments_connected$Start.Date[i+1] %m+% days(-1)),  origin = "1970-01-01")                                        #Kontrola, že na dalším řádku je stejný pacient
    Treatments_connected$End.Date[i] <- as.Date(ifelse(is.na(Treatments_connected$End.Date[i]) & Treatments_connected$"Patient.ID"[i]!=Treatments_connected$"Patient.ID"[i+1], #Pokud je na dalším řádku jiný pacient, prodlouží do EndDate+2R; pokud je pacient totožný, prodlouží těsně před začátek další léčby
                                                         loc_rawDataExportCutOffDate %m+% days(1), Treatments_connected$End.Date[i]),  origin = "1970-01-01")                                        #Kontrola, že na dalším řádku je stejný pacient
  } else if (is.na(Treatments_connected$End.Date[i])) {
    Treatments_connected$End.Date[i] <- as.Date(ifelse(Treatments_connected$"Patient.ID"[i]!=Treatments_connected$"Patient.ID"[i+1], #Pokud je na dalším řádku jiný pacient a léčba (i) není ukončená, ukončí ji k CutOff+1D; pokud je pacient totožný, prodlouží těsně před začátek další léčby
                                                         loc_rawDataExportCutOffDate %m+% days(1), Treatments_connected$Start.Date[i+1] %m+% days(-1)),  origin = "1970-01-01")                 }
}

# manuálně se musí doplnit prodloužení pro poslední řádek!!
i=nrow(Treatments_connected)
if (Treatments_connected$Medication[i] %in% c("Lemtrada", "Mavenclad")) {
  Treatments_connected$End.Date[i] <- as.Date(loc_rawDataExportCutOffDate %m+% days(1),  origin = "1970-01-01") #prodlouží do CutOff+1D
} else if (Treatments_connected$Medication[i] %in% c("Ocrevus", "Mabthera", "generika Mabthery")) {
  Treatments_connected$End.Date[i] <- as.Date(Treatments_connected$End.Date[i] %m+% years(2) %m+% days(-1),  origin = "1970-01-01") #prodlouží do EndDate+2R
  Treatments_connected$End.Date[i] <- as.Date(ifelse(is.na(Treatments_connected$End.Date[i]), #Pokud je puls neukončený, ukončí ho k CutOff+1D 
                                                       loc_rawDataExportCutOffDate %m+% days(1), Treatments_connected$End.Date[i]),  origin = "1970-01-01")                                        #Kontrola, že na dalším řádku je stejný pacient
} else if (is.na(Treatments_connected$End.Date[i])) {
  Treatments_connected$End.Date[i] <- as.Date(loc_rawDataExportCutOffDate %m+% days(1),  origin = "1970-01-01")
  # ukončí neukončenou léčbu k CutOff+1D
}              


# ============================================= IVIG START =========================================

setorder(Treatments_connected_IVIG, "Patient.ID", "Start.Date")

Treatments_connected_IVIG$Connect_to_next_row <- FALSE

# označí ty řádky s léčbou, které se mají spojit s řádkem následujícím
print(paste("=== IVIG: Oznaceni zaznamu urcenych ke spojeni ==============="))

for (i in 1:(nrow(Treatments_connected_IVIG)-1)) {
  if (i %% 100 == 0 | i ==1) 
    print(paste("...zpracovavam zaznam c. ", i, "prubeh:", round(i/(nrow(Treatments_connected_IVIG)-1)*100,0), "%"))
  
  ifelse((Treatments_connected_IVIG$Medication[i]==Treatments_connected_IVIG$Medication[i+1]) &                                       #Kontrola, že na dalším řádku je stejný lék
           difftime(Treatments_connected_IVIG$Start.Date[i+1], Treatments_connected_IVIG$End.Date[i], unit="days")<=1 &           #Kontrola, že další podání nebylo déle než po 1 dnu
           Treatments_connected_IVIG$"Patient.ID"[i]==Treatments_connected_IVIG$"Patient.ID"[i+1],TRUE,FALSE)                                         #Kontrola, že na dalším řádku je stejný pacient
}

# Do označených řádků přenese z následujícího řádku "End Date" a "Reason for Discontinuation"; parametry Dosage, Unit, Frequency nastaví na NA

Treatments_connected_IVIG$"Delete row" <- FALSE
print(paste("=== IVIG: Spojovani lecby a odstraneni redundantnich zaznamu ==============="))

for (i in (nrow(Treatments_connected_IVIG)-1):1) {
  if (((i-(nrow(Treatments_connected_IVIG)-1)) %% 100)==0 | 
      i == (nrow(Treatments_connected_IVIG)-1) |
      i==1) 
    print(paste("...zpracovavam zaznam c. ", (nrow(Treatments_connected_IVIG)-1)-i, "prubeh:", round(
      ((nrow(Treatments_connected_IVIG)-1)-i)/(nrow(Treatments_connected_IVIG)-1)*100,0), "%"))
  
  if (Treatments_connected_IVIG$Connect_to_next_row[i]) {
    Treatments_connected_IVIG$End.Date[i] <- Treatments_connected_IVIG$End.Date[i+1]
    Treatments_connected_IVIG$`Reason for Discontinuation`[i] <- Treatments_connected_IVIG$`Reason for Discontinuation`[i+1]
    Treatments_connected_IVIG$Dosage[i] <- NA
    Treatments_connected_IVIG$Unit[i] <- NA
    Treatments_connected_IVIG$Frequency[i] <- NA
    Treatments_connected_IVIG$"Delete row"[i+1] <- TRUE
    
  } 
}

# Odstraní redundantní záznamy o léčbě
Treatments_connected_IVIG <- Treatments_connected_IVIG[!Treatments_connected_IVIG$`Delete row`]
Treatments_connected_IVIG$"Delete row" <- NULL
Treatments_connected_IVIG$Connect_to_next_row <- NULL

# Neukončenou léčbu ukončí buď ke dni předcházejícímu další léčbě nebo do CutOffDate + 1D

for (i in 1:(nrow(Treatments_connected_IVIG)-1)) {
  if (is.na(Treatments_connected_IVIG$End.Date[i])) {
    Treatments_connected_IVIG$End.Date[i] <- as.Date(ifelse(Treatments_connected_IVIG$"Patient.ID"[i]!=Treatments_connected_IVIG$"Patient.ID"[i+1], #Pokud je na dalším řádku jiný pacient a léčba (i) není ukončená, ukončí ji k CutOff+1D; pokud je pacient totožný, prodlouží těsně před začátek další léčby
                                                              loc_rawDataExportCutOffDate %m+% days(1), Treatments_connected_IVIG$Start.Date[i+1] %m+% days(-1)),  origin = "1970-01-01")                 
  }
}

# manuálně se musí doplnit prodloužení pro poslední řádek!!
i=nrow(Treatments_connected_IVIG)
if (is.na(Treatments_connected_IVIG$End.Date[i])) {
  Treatments_connected_IVIG$End.Date[i] <- as.Date(loc_rawDataExportCutOffDate %m+% days(1), origin = "1970-01-01")                 
}              


# ============================================= IVIG END =========================================


Treatments_connected_DMD <- Treatments_connected
Treatments_connected_ALL <- rbind(rbind(Treatments_connected_DMD, Treatments_connected_IVIG), Treatments_connected_no_interest)

print(paste("Spojenim vychozich", tmp_N_records_to_process, "zaznamu o lecbe vzniklo celkem", nrow(Treatments_connected_ALL), "unikatnich zaznamu o jedinecnych lecbach.", tmp_N_records_to_process-nrow(Treatments_connected_ALL), "zaznamu bylo odebrano a zohledneno ve zbylych zaznamech."))


rm(i, Treatments_connected, tmp_N_records_to_process)

