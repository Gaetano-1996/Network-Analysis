################################################################################
#                                                                              #
#   INDAGINE SULLE RELAZIONI TRA I CORSI DI LAUREA DELL’UNIVERSITÀ DI PADOVA:  #
#                  UN’ APPLICAZIONE ALLE SCUOLE STEM                           #
#                                                                              #
################################################################################
#                                                                              #
#      Progetto per il corso di "Metodi Statistici per Big Data" a cura di:    #
#                        Brunello Irene, 2002694.                              #
#                      Gastaldello Matteo, 2012098;                            #
#                        Tedesco Gaetano, 2006924.                             #
#                                                                              #
################################################################################

library(tidyverse)
library(Rcrawler)
library(tidytext)
library(igraph)

#Funzione estrazione -----------------------------------------------------------
extract_from_school = function(scuola = "scienze", URL = F) {
  # Funzione di estrazione dei dati dal sito didattica.unipd.it
  # Estrae i dati della suola selezionata li restituisce in formato tibble
  # e scrive un file in formato csv per farne il backup.
  #@param:
  # scuola(char): una stringa contenente "scienze" o "ingegneria"
  #               usare l'url della scuola per altre scuole e impostare URL = T;
  # URL(bool): se TRUE considera l'argomento passato a scuola come l'url della
  #            scuola di cui vogliamo fare lo scraping.
  #@return:
  # data_final(tibble): dati estratti.
  
  # controlliamo se in scuola abbiamo il link effettivo o solo il nome
  # 'scienze' o 'ingegneria'
  
  ####SETTING
  if (URL == F) {
    # variabili per scraping della scuola di scienze
    if (scuola == "scienze") {
      url = "https://didattica.unipd.it/off/2022/LT/SC"
      filtro = "https://didattica.unipd.it/off/[0-3]{4}/LT/SC/"
      sigla = "SC"
    }
    # variabili per scraping della scuola di ingegneria
    else{
      url = "https://didattica.unipd.it/off/2022/LT/IN"
      filtro = "https://didattica.unipd.it/off/[0-3]{4}/LT/IN/"
      sigla = "IN"
    }
  }
  # variabili per scraping di altre scuole da link fornito
  else{
    url = scuola
    path = scuola %>% str_split_1(pattern = "/") %>% tail(n = 2) %>%
      paste(collapse = "/") %>% paste("/", sep = "")
    filtro = "https://didattica.unipd.it/off/[0-3]{4}/" %>% paste(path, sep =
                                                                    "")
    sigla = path %>% str_split_1(pattern = '/') %>% .[2]
  }
  ####ESTRAZIONE
  # Funzione di crawling vero e proprio, esploriamo la rete del sito
  # a varie profondita' (fino ad una profondita' massima).
  # Raggiunta la profondita' massima cerchiamo nella pagina le informazioni
  # targetizzate e le estraiamo
  Rcrawler(
    Website = url,
    # link della scuola
    no_cores = 4,
    # numero di processi in parallelo 4 puo' bastare
    # aumentare se il processo e' troppo lento
    no_conn = 4,
    # numero di connessioni che si inviano al server
    # modificare (come sopra) con cura per evitare ban
    MaxDepth = 2,
    # profondita massima
    # sostanzialmente abbiamo la visione di internet come un grafo
    # possiamo impostare il numero di pagine max che apriamo per ogni link
    # allontanandoci dall radice che sarebbe il link di partenza.
    
    crawlUrlfilter = filtro,
    # usiamo un'espessione regolare per dire come sono fatti i link dei corsi
    # di ogni scuola.
    
    # le coordinate XPath degli elementi che ci interessano, in ordine:
    #1. tabella "Syllabus"
    #2. tabella "Settore Scientifico-Disciplinare"
    #3. titolo: contiene nome e codice del corso
    #4. tabella iniziale: estraiamo il nome del corso di laurea
    #5. periodo di erogazione del corso
    ExtractXpathPat = c(
      "//*/div[@class='contenuto tabella_syllabus']//td[@class='aleft vtop']",
      "//*/div[@class='grouped_info always_opened sel_crediti']//table[@class='dettaglio_righe']",
      "//*/div[@class='titolopagina']",
      "//*/table[@class='off_nav w100']//tr[@class='list_cds']",
      "//*/div[@class='grouped_info always_opened sel_erogazione']"
    ),
    # piu' espressione per singola XPath
    ManyPerPattern = T,
    saveOnDisk = F # non vogliamo salvare l'intero sito di cui facciamo scraping
  )
  
  ####CONVERSIONE DEI DATI IN FORMATO TABULARE
  data1 = data.frame(do.call("rbind", DATA))
  # Costruzione del dataframe---------------------------------------------------
  
  # Filtri per eliminare i dati indesiderati
  # sostanzialmente in data1 abbiamo dei link che sono stati scaricati
  # ma non contengono le info che vogliamo, sono inutili.
  # Notiamo che in queste righe inutili alcuni elementi V2 e V3 sono liste di
  # lunghezza 0, usiamo la lunghezza delle liste per creare una variabile
  # binaria:
  # 0 se la lunghezza della lista e' 0 (riga inutile),
  # 1 lunghezza non zero (riga utile)
  # usiamo queste due variabili per filtrare il dataset ed eliminare le righe
  # inutili.
  
  # estraiamo la colonna dove abbiamo messo il testo della tabella Syllabus
  contenuti = data1 %>% pull(V2) # lista di liste
  # vogliamo entrare nell'elemnto della lista con [[]]
  # e poi siccome tale elemento e'a sua volta una lista vogliamo usarne
  # la lunghezza come discriminante.
  
  ##Filtro per contenuti
  #preallocazione per dummy
  info_utili_c = rep(NA, length(contenuti))
  # generazione della dummy per syllabus
  for (i in seq_along(contenuti)) {
    if (length(contenuti[[i]]) == 0) {
      info_utili_c[i] = 0
    } else{
      info_utili_c[i] = 1
    }
  }
  
  ##Filtro per ambito disciplinare
  # estraiamo la colonna dove abbiamo messo il testo della tabella
  # Ambito disciplinare
  ambiti = data1 %>% pull(V3) # lista di liste
  #preallocazione per dummy
  info_utili_a = rep(NA, length(ambiti))
  # generazione dummy per ambito
  for (i in seq_along(ambiti)) {
    if (length(ambiti[[i]]) == 0) {
      info_utili_a[i] = 0
    } else{
      info_utili_a[i] = 1
    }
  }
  
  ##Filtro del dataframe
  # aggiungiamo le dummy al df
  data1$d1 = info_utili_c
  data1$d2 = info_utili_a
  # filtriamo il df
  data2 = data1 %>%
    filter(d1 == 1 & d2 == 1)
  
  ##Estrazione info utili
  
  ##Periodo di erogazione del corso
  periodo = rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V6)) {
    periodo[i] = data2$V6[i] %>%
      as.character() %>%
      str_split_1("\n") %>%
      .[c(6, 8)] %>%
      gsub(pattern = "  ", replacement = "") %>%
      str_split_i(pattern = " ", i = 1) %>%
      paste(collapse = "-")
  }
  ##Prerequisiti
  preReq = rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V2)) {
    preReq[i] = data2$V2[[i]][1]
  }
  ##Contenuti del corso
  syllabus = rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V2)) {
    syllabus[i] = data2$V2[[i]][5]
  }
  ##Ambito disciplinare
  ssd = rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V3)) {
    ssd[i] = data2$V3[[i]] %>%
      str_split_1(pattern = "\n") %>%
      .[7] %>%
      gsub(pattern = "  ", replacement = "")
  }
  ##Nome del corso
  nomi = rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V4)) {
    nomi[i] = data2$V4[[i]] %>%
      str_split_1(pattern = "\n") %>%
      .[3]
  }
  ##Codice del corso
  codici =  rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V4)) {
    codici[i] = data2$V4[[i]] %>%
      str_split_1(pattern = "\n") %>%
      .[4] %>%
      str_split_1(pattern = ",") %>%
      .[1]
  }
  ##Corso di laurea
  degree = rep(NA, nrow(data2))
  
  for (i in seq_along(data2$V5)) {
    degree[i] = data2$V5[[i]] %>%
      str_split_1(pattern = "\n") %>%
      .[2] %>%
      gsub(pattern = "  ", replacement = "")
  }
  ##Link alla pagina per controllo
  Ids = unlist(lapply(data2$PageID, `[[`, 1))
  data2$Id = as.character(Ids)
  
  data3 = left_join(data2, INDEX, by = c("Id" = "Id"))
  
  links = data3$Url
  # formazione del dataset
  data_final = tibble(
    Codice = codici,
    Corso = nomi,
    Laurea = degree,
    Settore = ssd,
    Prerequisiti = preReq,
    Contenuto = syllabus,
    Link = links,
    Periodo = periodo,
    Scuola = rep(sigla, nrow(data2))
  )
  ##Eliminiamo i corsi senza contenuto (inglese, prova finale, tirocinio, ecc)
  data_final = data_final %>% filter(!is.na(Contenuto))
  # salvataggio dei dati in un csv per backup
  write_csv(data_final, paste(sigla, as.character(today()), ".csv", sep =
                                ""))
  return(data_final)
}

# Manipolazione dati grezzi ----------------------------------------------------

##Normalizza settore
normalize_sector = function(tbbl, input, output = input) {
  # prende solo le lettere del codice ambito disciplinare
  # es: "FIS/02" --> "FIS"
  #@param:
  # tbbl (data.frame o tibble): dataset su cui vogliamo applicare la modifica
  # input (character): nome della colonna a cui applicare
  # output (character): nome della colonna in cui voglio salvare il risultato
  #@return:
  # tbbl (dataframe o tibble): dataset modificato
  temp = tbbl %>% pull(input) %>% as.character()
  for (i in seq_along(temp))
    if (temp[i] == "--")
      temp[i] = NA
  temp = temp %>% str_split_i(pattern = "/", 1)
  tbbl[, output] = temp
  tbbl
}

##Formatta nomi corsi
format_names = function(tbbl, input, output = input) {
  # Formattiamo i nomi dei corsi in modo utile alla ricerca di questi nei
  # prerequisiti
  # es:"ALGEBRA LINEARE (Ult.numero di matricola dispari)" --> "algebra lineare"
  #@param
  # tbbl (dataframe o tibble): dataset
  # input (character): nome della colonna da normalizzare
  # output (character): nome della nuova colonna (uguale a input di default)
  #@return
  # tbbl: il dataframe con la colonna normalizzata
  for (i in 1:nrow(tbbl)) {
    # iteriamo su tutta la colonna
    tbbl[i, output] = tbbl[i, input] %>% # singola (riga)
      # rimuovo le parentesi
      tolower() %>% # converto il testo in minuscolo
      # normalizzazione del nome
      
      #RIMOZIONE DI INTERE FRASI
      # nomi scuola ingegneria
      gsub(pattern = ' (a)',
           replacement = '',
           fixed = T) %>%
      gsub(pattern = ' (b)',
           replacement = '',
           fixed = T) %>%
      gsub(pattern = ' (c)',
           replacement = '',
           fixed = T) %>%
      gsub(pattern = ' (d)',
           replacement = '',
           fixed = T) %>%
      gsub(pattern = ' (e)',
           replacement = '',
           fixed = T) %>%
      
      gsub(pattern = "[()]", replacement = "") %>% # rimozione parentesi
      gsub(pattern = " ult. due numeri di matricola da 00 a 24",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. due numeri di matricola da 25 a 49",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. due numeri di matricola da 50 a 74",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. due numeri di matricola da 75 a 99",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. 2 num. matr. da 00 a 49",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. 2 num. matr. da 50 a 99",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. numero di matricola da  0 a 4",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. numero di matricola da  5 a 9",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " canale a",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " canale b",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " iniziali cognome a-l",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " iniziali cognome m-z",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " numerosita' canale 1",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " numerosita' canale 2",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " numerosita' canale 3",
           replacement = "",
           fixed = T) %>%
      
      # nomi scuola scienze
      gsub(pattern = " ult. numero di matricola dispari",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " ult. numero di matricola pari",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " mod. a",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " mod. b",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " mod.a",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " mod.b",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = " da a a e",
           replacement = '',
           fixed = T) %>%
      gsub(pattern = " da f a o",
           replacement = '',
           fixed = T) %>%
      gsub(pattern = " da p a z",
           replacement = '',
           fixed = T) %>%
      
      #RIMOZIONE DI PREPOSIZIONE E STOPWORD
      gsub(pattern = " alla ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " all'",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " negli ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " per l' ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " ed ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' e ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' di ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = "dell'",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = ' degli ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' della ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' dello ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' del ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " delle ",
           replacement = ' ',
           fixed = T) %>%
      gsub(pattern = ' per ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' dei ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' con ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' alle ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' al ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' in ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " la ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " le ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " gli ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " il ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " lo ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " nell' ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' ai ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' nei ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' nelle ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' uno ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = "-",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = "l'",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = ":",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = "à",
           replacement = "a",
           fixed = T) %>%
      gsub(pattern = "d'",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ",",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ".",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = "'",
           replacement = "",
           fixed = T) %>%
      
      #MODIFICA DI INTERI NOMI
      gsub(pattern = "analisi matematica", replacement = "analisi") %>%
      
      #ELIMINAZIONE SPAZI
      gsub(pattern = "  ", replacement = " ") %>%
      trimws("right")
  }
  # assegnamo e restituiamo il dataframe modificato
  final = tbbl
  return(final)
}

##Numeri romani in decimali
format_numbers = function(tbbl, input) {
  # Trasforma i numeri dal formato romano a quello decimale
  # es: "I" --> "1"
  #@param
  # tbbl (dataframe o tibble): dataset
  # input (character): nome della colonna da normalizzare
  # output (character): nome della nuova colonna (uguale a input di default)
  #@return: tbbl
  conversione = function (x) {
    # funzione di conversione vera e propria
    #@param:
    # x: vettore con le parole del singolo prerequisito
    #@return: none
    for (i in seq_along(x)) {
      if (x[i] == "I" | x[i] == "I,")
        x[i] = 1
      else if (x[i] == "II" | x[i] == "II,")
        x[i] = 2
      else if (x[i] == "III" | x[i] == "III,")
        x[i] = 3
      else
        i = i # altrimenti restituisco la parola normale
    }
    x
  }
  for (i in 1:nrow(tbbl)) {
    if (!is.na(tbbl[i, input]))
      tbbl[i, input] = tbbl[i, input] %>% as.character() %>%
        str_split_1(pattern = " ") %>%
        conversione() %>% paste(collapse = " ")
  }
  tbbl
}

##Formatta campo prerequisiti
format_requires = function(tbbl, input, output = input) {
  # Funzione per la formattazione del campo prerequisiti, in particolare
  # ci occupiamo di formattare il testo per facilitare il match
  # tra il nome del corso e il prerequisito alla base della creazione dell'arco
  #@param
  # tbbl: dataframe
  # input: nome della colonna su cui fare la normalizzazione del testo
  # output: nome della nuova colonna
  #@return
  # final: un dataframe con la nuova colonna con testo normalizzato
  
  for (i in 1:nrow(tbbl)) {
    # iteriamo su tutta la colonna
    tbbl[i, output] = tbbl[i, input] %>% # singolo nome del corso (riga)
      gsub(pattern = "[()]", replacement = "") %>% # rimuovo le parentesi
      tolower() %>%
      gsub(pattern = " alla ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " all'",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " negli ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " per l' ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " ed ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' e ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' di ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = "dell'",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = ' degli ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' della ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' dello ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' del ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " delle ",
           replacement = ' ',
           fixed = T) %>%
      gsub(pattern = ' per ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' dei ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' con ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' alle ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' al ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' in ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " la ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " le ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " gli ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " il ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " lo ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = " nell' ",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' ai ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' nei ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' nelle ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ' uno ',
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = "-",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = "l'",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = "'",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ":",
           replacement = "",
           fixed = T) %>%
      gsub(pattern = "à",
           replacement = "a",
           fixed = T) %>%
      gsub(pattern = ",",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = ".",
           replacement = " ",
           fixed = T) %>%
      gsub(pattern = '"', replacement = "") %>%
      # interi nomi
      gsub(pattern = "analisi matematica", replacement = "analisi") %>%
      gsub(pattern = "  ",
           replacement = " ",
           fixed = T)
  }
  final = tbbl
  final
}

# Pulizia del dataset-----------------------------------------------------------
cleanings = function(tbbl) {
  # Applica tutte le funzioni di pulizia del dataset in sequenza per ricreare
  # le esatte condizioni dei dati su cui e' stata svolta l'analisi
  # DISCLAIMER: funziona con i nomi delle colonne usate durante l'analisi
  #             generate durante l'estrazione dei dati.
  #@param:
  # tbbl(dataframe,tibble): dati grezzi
  #@return
  # tbbl(dataframe,tibble): dati puliti
  tbbl = tbbl %>%
    normalize_sector("Settore") %>% # settore disciplinare
    format_names(input = "Corso") %>% # nome del corso
    format_numbers("Prerequisiti") %>% # prerequisiti
    format_requires(input = "Prerequisiti") %>%
    distinct(Codice, .keep_all = T) %>%
    distinct(Corso, Laurea, .keep_all = T) %>%
    filter(!is.na(Contenuto))
  return(tbbl)
}

# Conversione tabellare-rete----------------------------------------------------
edge = function(tbbl, target, source) {
  # Funzione che restituisce le associazioni prerequisito-corso per singolo nome
  # di corso. Andiamo a cercare il `target` nei prerequisiti di tutti i corsi.
  # CREA ARCHI DIRETTI
  #@param:
  # tbbl(dataframe/tibble): dataset
  # source (char): nome del corso da valutare come prerequisito
  # target (char): nome della colonna dove cercare i corsi
  #@return:
  # tabella(tibble): dataset ridotto di due colonne, nella prima colonna
  #                  abbiamo il prerequistito e nella seconda tutti i corsi
  #                  di cui e' prerequisito.
  
  risultato <- tbbl %>%
    filter(str_detect(tbbl %>% pull(target), source))
  #no match
  if (length(unique(risultato$Corso)) == 0)
    corsi = NA
  # match
  else
    corsi = unique(risultato$Corso)
  # definiamo il tibble
  tabella = tibble(prerequisito = source, corsi = corsi)
  # restituiamo un tibble
  return(tabella)
}

make_edges = function(tbbl, target, source) {
  # Funzione che costruisce gli archi della rete
  # DEFINIZIONE ARCO tra due corsi:
  # "Il nome del corso e' presente tra i prerequisiti di un altro corso"
  # UTILIZZA ARCHI DIRETTI
  # DISCLAIMER: nelle covariate dove abbiamo piu di un informazione per nodo
  #             troviamo NA.
  #@param:
  # tbbl(dataframe/tibble): dataset della scuola
  # target(char): nome della colonna dove cercare i corsi
  # source(char): nome della colonna dove prendere i corsi da cercare
  #@return:
  # archi (tibble): lista archi
  #                 Source = prerequisito
  #                 Target = corso di cui e' prerequisito `Source`
  
  # tibble vuoto per imagazzinare la lista archi
  archi = tibble()
  # lista dei singoli nomi dei corsi
  lista_corsi <- unlist(as.vector(tbbl[, source]))
  # per ogni singolo nome corso estraiamo tutti gli archi che partono da questo
  for (i in seq_along(lista_corsi)) {
    arco = edge(tbbl = tbbl, target = target, lista_corsi[i])
    temp = tibble(arco[1], arco[2])
    archi <- bind_rows(archi, temp)
  }
  # cambiamo il nome delle colonne
  colnames(archi) = c("Source", "Target")
  # eliminiamo gli archi
  archi = archi %>%
    filter(!is.na(Target)) %>%
    filter(Source != Target)
  # restituiamo la lista archi
  return(archi)
}

make_network = function(edges,
                        raw_data,
                        cov = c("Laurea", "Settore", "Scuola"),
                        plot = T,
                        save = T) {
  # Funzione che da la lista degli archi costruisce la rete.
  # Inoltre inserisce nella rete le covariate di nodo utitli alla successiva
  # analisi.
  # La funzione puo' generare due file uno con i nodi e le covariate di nodo
  # e l'altro con gli archi e salvarli in csv.
  # LA FUNZIONE E' DISEGNATA PER LAVORARE CON ARCHI DIRETTI E GENERARE RETI
  # DIRETTE.
  #@param:
  # edges(dataframe/tibble): lista degli archi (IN PAROLE);
  # raw_data(dataframe/tibble): dataframe da cui estrarre le covariate;
  # cov(char): lista (di lunghezza arbitraria) con il nome delle colonne
  #            dove si trovano le covariate da inserire nella rete
  #            in raw_data;
  # save(bool): se TRUE genera due csv di backup uno per gli archi e l'altro per
  #             i nodi;
  #@return
  # network(igraph): oggetto di tipo igraph.
  
  #Costruiamo la prima rete --> l'id dei nodi corrisponde al nome del corso
  net0 = graph_from_edgelist(as.matrix(edges), directed = T)
  # estraiamo gli indici dei nodi
  IDs = 1:length(V(net0))
  # estraiamo le etichette
  etichette = names(V(net0))
  # creiamo un dataframe che contiene gli indici e i rispettivi nomi per ogni
  # nodo
  nodi = data.frame(id = IDs,
                    label = etichette)
  # estrazioni covariate
  # popolamento delle covariate
  for (i in 1:nrow(raw_data)) {
    for (j in 1:nrow(nodi)) {
      if (nodi[j, "label"] == raw_data[i, 'Corso']) {
        nodi[j, cov] = raw_data[i, cov]
      }
    }
  }
  # preallocazione
  archi_num = matrix(NA, nrow = nrow(edges), ncol = 2)
  colnames(archi_num) = c("Source", "Target")
  archi_num = as_tibble(archi_num)
  # Popoliamo le colonna `Source`
  for (i in IDs) {
    for (j in 1:nrow(edges)) {
      if (nodi[i, 2] == edges [j, 1])
        archi_num[j, 1] = nodi[i, 1]
    }
  }
  # Popoliamo la colonna `Target`
  for (i in IDs) {
    for (j in 1:nrow(edges)) {
      if (nodi[i, 2] == edges [j, 2])
        archi_num[j, 2] = nodi[i, 1]
    }
  }
  # Creiamo la rete con gli indici
  net1 = graph_from_edgelist(as.matrix(archi_num), directed = T)
  # inseriamo gli attributi ai nodi: label + covariate
  for (i in 2:ncol(nodi)) {
    net1 = set_vertex_attr(net1,
                           colnames(nodi)[i],
                           value = nodi[, i])
  }
  # remuove cicli e archi multipli/paralleli
  net2 = simplify(net1)
  # grafico della rete
  if (plot) {
    nodesize = log(1 + degree(net2, mode = "out"))
    nodelayout = layout.fruchterman.reingold(net2,
                                             niter = 1000,
                                             weights = NULL)
    plot(
      net2,
      layout = nodelayout,
      main = "Rete",
      vertex.label = NA,
      vertex.size = 2 * nodesize + 2,
      edge.arrow.size = degree(net2, mode = "out") /
        (8 * max(degree(net2, mode = "out"))),
      vertex.frame.color = "darkorange",
      vertex.color = "orange",
      edge.width = .3,
      edge.curved = .5
    )
  }
  
  # salvataggio
  if (save) {
    write_csv(nodi,
              paste(
                "NodiSemplici_",
                nodi$Scuola[1],
                "_",
                as.character(today()),
                ".csv",
                sep = ""
              ))
    write_csv(archi_num,
              paste(
                "Archi_",
                nodi$Scuola[1],
                "_",
                as.character(today()),
                ".csv",
                sep = ""
              ))
  }
  return(net2)
}


# Analisi della rete------------------------------------------------------------
net_analytics = function(rete,
                         cicli = F,
                         diretto = T,
                         save = T,
                         verbose = T,
                         order_by = c(
                           "punteggio_hub",
                           "eccentricita",
                           "btwn",
                           "grado_tot",
                           "grado_in",
                           "grado_out",
                           "autorita",
                           "vicinanza"
                         ),
                         plot.network = T,
                         plot.stat = T,
                         percentile = 95) {
  # Funzione che produce le statistiche descrittive di rete per lo studio
  # degli Hub e altre caratteristiche di rete.
  # Restituisce tutte le informazioni a video, fornisce un grafico della rete
  # evidenziando gli Hub e salva (se richiesto) un csv con le statistiche di
  # nodo.
  #@param:
  # rete(igraph): rete da analizzare;
  # cicli(bool): Se sono ammessi cicli o meno;
  # diretto(bool): specifica se considerare la rete come diretta, nel nostro
  #                caso e'sempre diretta (default=TRUE);
  # save(bool): se TRUE salva le descrittive di rete/nodo su un dataframe;
  # verbose(bool): se importato TRUE stampa l'input a video (default=TRUE);
  # order_by(char): esplicita l'ordinamento da utilizzare per i nodi e in base a
  #                 quale criterio visualizzare le etichette;
  # plot.network(bool): se TRUE stampa il grafico della rete basando le
  #                     caratteristiche estetiche sulle statistiche descrittive
  #                     calcolate;
  # plot.stat(bool): se TRUE stampa il grafico della statistica descrittiva
  #                  usata come rete;
  # percentile(num): il decile della distribuzione dell'indice di ordinamento
  #              espresso in order_by che vogliamo come treshold per filtrare
  #              le etichette del grafico della rete (default pari a 10 ossia
  #              il 90mo percentile).
  
  criterio = match.arg(order_by)
  ##statistiche di rete
  ordine = gorder(rete) # numero di nodi
  dimensione = gsize(rete) # numero di archi
  sp_massimo = diameter(rete, directed = diretto) # diametro della rete
  densita = edge_density(rete, loops = cicli) # densita' della rete
  sp_medio = mean_distance(rete, directed = diretto)  # shortest path medio
  
  ##statistiche di nodo
  # creiamo un dataframe per gestire le informazioni per singolo nodo
  # in ordine
  nodi_stat = tibble(
    id = 1:gorder(rete),
    label = V(rete)$label,
    Laurea = V(rete)$Laurea,
    Settore = V(rete)$Settore,
    Scuola = V(rete)$Scuola,
    autorita = authority_score(rete)$vector,
    eccentricita = eccentricity(rete, mode = "out"),
    vicinanza = closeness(rete, vids = V(rete), mode = "all"),
    grado_tot = degree(rete, mode = "total"),
    grado_in = degree(rete, mode = "in"),
    grado_out = degree(rete, mode = "out"),
    btwn = betweenness(rete, directed = T),
    punteggio_hub = hub_score(rete)$vector
  )
  
  ##Aggiorniamo i dati dei nodi nell'oggetto igraph
  for (i in 6:ncol(nodi_stat)) {
    rete = set_vertex_attr(rete,
                           colnames(nodi_stat)[i],
                           value = nodi_stat[, i])
  }
  
  ## Troviamo gli hub
  hubs = nodi_stat %>%
    filter(punteggio_hub >=
             quantile(round(punteggio_hub, 4),
                      probs = seq(0, 1, by = .01)[percentile])) %>%
    pull(label)
  
  ##Output a video
  if (verbose) {
    cat(
      "\nAnalisi Descrittiva di Rete",
      "\n\n----------------- Statistiche di Rete ------------------",
      "\n\nOrdine della rete:",
      ordine,
      "\nDimensione della Rete:",
      dimensione,
      "\nDiametro della Rete:",
      sp_massimo,
      "\nDensità della Rete:",
      round(densita, 4),
      "\nCammino minimo medio:",
      round(sp_medio, 3),
      "\n\n----------------- Statistiche di Nodo ------------------",
      "\n\nGrado medio totale:",
      round(mean(nodi_stat$grado_tot), 3),
      "\nGrado medio entrante:",
      round(mean(nodi_stat$grado_in), 3),
      "\nGrado medio uscente:",
      round(mean(nodi_stat$grado_out), 3)
    )
    
    cat("\n\n-------------------- Hub della Rete --------------------\n")
    for (i in seq_along(hubs)) {
      cat("\n", hubs[i])
    }
  }
  
  ##Backup delle analisi su csv
  if (save) {
    nodi = nodi_stat %>%
      arrange(criterio)
    write_csv(
      nodi,
      paste(
        "NodiDescrittiva_",
        nodi$Scuola[1],
        "_",
        as.character(today()),
        ".csv",
        sep = ""
      )
    )
  }
  
  ##Grafico della rete con etichette sugli HUB
  if (plot.network) {
    nodesize = log(1 + degree(rete, mode = "out"))
    nodelayout = layout.fruchterman.reingold(rete,
                                             niter = 1000,
                                             weights = NULL)
    etichette = V(rete)$label
    treshold = nodi_stat[, criterio] %>%
      pull() %>%
      quantile(probs = seq(0, 1, by = .01)) %>%
      .[percentile]
    etichette[nodi_stat[, criterio] <= treshold] = NA
    plot(
      rete,
      layout = nodelayout,
      main =  'Statistiche di Rete',
      vertex.label.family = "helvetica",
      vertex.label = etichette,
      vertex.size = 2 * nodesize + 2,
      vertex.label.cex = nodesize / 3,
      vertex.label.font = 1,
      vertex.label.color = "black",
      vertex.frame.color = "darkorange",
      vertex.color = "orange",
      edge.arrow.size = degree(rete, mode = "out") /
        (max(degree(rete, mode = "out"))),
      edge.width = .3,
      edge.curved = .5
    )
  }
  ##Grafico della distribuzione della statistica scelta
  if (plot.stat) {
    par(mfrow = c(1, 1))
    tibble(
      # stiamo definendo le colonne
      nodo = nodi_stat$label,
      variabile = criterio,
      valore = nodi_stat[, criterio] %>% pull()
    ) %>%
      ggplot(aes(x = valore,
                 fill = variabile)) +
      geom_histogram(
        color = "darkorange",
        fill = "orange",
        bins = 20
      ) +
      labs(title = criterio, x = criterio) +
      theme_minimal() +
      theme(legend.position = "none")
  }
}


community_detection = function(rete,
                               passi = 4,
                               verbose = T,
                               save = T,
                               plot = T,
                               order_by = c(
                                 "punteggio_hub",
                                 "eccentricita",
                                 "btwn",
                                 "grado_tot",
                                 "grado_in",
                                 "grado_out",
                                 "autorita",
                                 "vicinanza"
                               ),
                               percentile = 95,
                               mark.cluster = F) {
  # Funzione per lo studio dei cluster nella rete.
  # La funzione utilizza l'algorimo walktrap basato sui concetti di
  # trasformazione Naive della rete e random walk su di essa (vedi report
  # per approfondimento).
  #@param
  # rete(igraph): oggetto rete (diretta o indiretta);
  # passi(num): indica il numero di passi che impostiamo per la camminata
  #             casuale
  # verbose(bool): se TRUE stampa i risultati a video
  # save(bool): se TRUE salva i nodi aggiornato con l'informazione sulle
  #             comunita' su file csv
  # plot(bool): se TRUE manda a video il grafico della rete con le nuove
  #             comunita'
  # order_by(char): esplicita l'ordinamento da utilizzare per i nodi e in base a
  #                 quale criterio visualizzare le etichette;
  # percentile(num): il decile della distribuzione dell'indice di ordinamento
  #              espresso in order_by che vogliamo come treshold per filtrare
  #              le etichette del grafico della rete (default pari a 10 ossia
  #              il 90mo percentile).
  # mark.cluster(bool): se TRUE colora l'area dei geodischi per ogni comunita'
  #@return
  # comunita(list): lista contente per ogni comunita' l'id dei nodi che vi
  # appartengono.
  
  criterio = match.arg(order_by)
  cm = cluster_walktrap(rete, steps = passi)
  # creiamo un dataframe per gestire le informazioni per singolo nodo
  # in ordine
  nodi_cluster = tibble(
    id = 1:gorder(rete),
    label = V(rete)$label,
    Laurea = V(rete)$Laurea,
    Settore = V(rete)$Settore,
    Scuola = V(rete)$Scuola,
    autorita = authority_score(rete)$vector,
    eccentricita = eccentricity(rete, mode = "out"),
    vicinanza = closeness(rete, vids = V(rete), mode = "all"),
    grado_tot = degree(rete, mode = "total"),
    grado_in = degree(rete, mode = "in"),
    grado_out = degree(rete, mode = "out"),
    btwn = betweenness(rete, directed = T),
    punteggio_hub = hub_score(rete)$vector,
    cluster = cm$membership # aggiungiamo l'informazione sulla community
  )
  ##Stampa a video
  if (verbose) {
    cat(
      "\nCommunity Detection",
      "\n\nAlgoritmo utilizzato:",
      cm$algorithm,
      "\nNumero di passi della Random Walk:",
      passi,
      "\nModularità stimata:",
      modularity(cm),
      "\nNumero di comunità stimate:",
      length(cm),
      "\n\n-------------------- Comunità: -----------------------\n\n",
      "(Ordinate per grandezza)\n\n"
    )
    comunita = communities(cm)
    # ordiniamo le comunita' per dimensione:
    len = sapply(comunita, length)
    comunita = comunita[order(len, decreasing = T)]
    
    for (i in 1:length(comunita)) {
      count = length(comunita[[i]])
      prop = count / vcount(rete)
      perc = prop * 100
      cat("Comunità",
          i,
          "rappresenta:",
          round(perc, 2),
          "% dei nodi\n")
    }
  }
  ##Backup della rete con la nuova informazione sull'appartenenza alle community
  # su csv
  if (save) {
    nodi = nodi_cluster %>%
      arrange(criterio)
    write_csv(nodi,
              paste(
                "NodiComunita_",
                nodi$Scuola[1],
                "_",
                as.character(today()),
                ".csv",
                sep = ""
              ))
  }
  ##Grafico della rete
  if (plot) {
    # assegnamo i colori in base all'appartenenza alle comunita'
    V(rete)$"color" = cm$membership
    nodesize = log(1 + degree(rete, mode = "out"))
    nodelayout = layout.fruchterman.reingold(rete,
                                             niter = 1000,
                                             weights = NULL)
    etichette = V(rete)$label
    treshold = nodi[, criterio] %>%
      pull() %>%
      quantile(probs = seq(0, 1, by = .01)) %>%
      .[percentile]
    etichette[nodi[, criterio] <= treshold] = NA
    #Evidenziaamo i geodischi delle comunita'
    if (mark.cluster) {
      plot(
        rete,
        layout = nodelayout,
        main =  'Comunità di Rete con Walktrap',
        vertex.label.family = "helvetica",
        vertex.label = etichette,
        vertex.size = 2 * nodesize + 3,
        vertex.label.cex = nodesize / 3,
        vertex.label.font = 1,
        vertex.label.color = "black",
        vertex.frame.color = "gray",
        vertex.color = V(rete)$cluster,
        edge.arrow.size = degree(rete, mode = "out") /
          (max(degree(rete, mode = "out"))),
        edge.width = .3,
        edge.curved = .5,
        edge.color = "darkgray",
        mark.groups = comunita
      )
    }
    # Non grafichiamo i geodischi
    else{
      plot(
        rete,
        layout = nodelayout,
        main =  'Comunità di Rete con Walktrap',
        vertex.label.family = "helevetica",
        vertex.label = etichette,
        vertex.size = 2 * nodesize + 3,
        vertex.label.cex = nodesize / 3,
        vertex.label.font = 1,
        vertex.label.color = "black",
        vertex.frame.color = "white",
        vertex.color = V(rete)$cluster,
        edge.arrow.size = degree(rete, mode = "out") /
          (max(degree(rete, mode = "out"))),
        edge.width = .3,
        edge.curved = .5,
        edge.color = "gray"
      )
    }
  }
  # restutiamo le comunita' per ulteriori analisi
  invisible(communities(cm))
}