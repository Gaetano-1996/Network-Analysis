################################################################################
#                                                                              #
#   INDAGINE SULLE RELAZIONI TRA I CORSI DI LAUREA DELL’UNIVERSITÀ DI PADOVA:  #
#                  UN’ APPLICAZIONE ALLE SCUOLE STEM                           #
#                                                                              #
################################################################################
#                                                                              #
#  Il presente script presenta step by step il codice utilizzato per svolgere  # 
#                              l' analisi.                                     #
#  Prima dell'esecuzione del seguente codice consigliamo la letture del file   #
# school_net_lib.R dove viene fornita una spiegazione esaustiva delle funzioni #
#                               utilizzate.                                    #
#                                                                              #
################################################################################


# Impostiamo la working directory dove abbiamo il file school_net_lin.R
setwd("...")
# Carichiamo le funzioni e le librerie utili all'analisi
source("school_net_lib.R")


# Estrazione dati---------------------------------------------------------------
# Decommentare per usare il crawler
#dati = extract_from_school("ingegneria")
# # rimozione dei dati superflui creati dallo scraper
#rm(INDEX)
#rm(DATA)

# Alternativamente si possono usare i file csv che abbiamo gia' inserito nella
# cartella report.
# Per la lettura da file csv decommentare il codice qui sotto.
#dati = read.csv(file.choose())


# Pulizia del dato--------------------------------------------------------------
dati = dati %>% cleanings()

# Definizione degli archi-------------------------------------------------------
archi = dati %>% make_edges(target = "Prerequisiti", source = "Corso")

# Creazione della rete----------------------------------------------------------
rete = archi %>% make_network(dati, save = T)

# Analisi descrittiva della rete------------------------------------------------
net_analytics(rete,
              percentile = 99,
              save = T,
              order_by = "grado_out")

# Community Detection-----------------------------------------------------------
cluster = community_detection(rete, save = T, percentile = 99)
