get_training_data <-
  function(api_key = "",
           start_day = lubridate::date("1945-01-01"),
           country_vec = c(""),
           reliability = 0.8,
           seed=42) {
    # Args:
    #  api_key:: str
    #       Der API-Key um die ManifestoR-API zu benutzen.
    #  start_day:: lubridate::date()
    #       Ab welchem Datum sollen Manifestos ausgewählt werden?
    #  country_vec:: c("")
    #       Character-Vector mit den Namen der Länder, aus denen man die Manifestos bekommen will.
    #  reliability:: float
    #       Die mininale Coder-Reliabilität, um Manifestos zurück zu erhalten. Zusätzlich immer Coder=199 --> Group-Mitglieder
    #  seed:: int
    #       Integer um den Zufallsprozess zu steuern, der die Zeilenfolge durchmischt. Default bei 42.
    #
    # Returns:
    #   df:: DataFrame
    #       Das DataFrame besteht aus folgenden Spalten: text (str), cmp_code (str), corpus_code (str), label (int), left_right (int).
    ### Packages laden
    require("dplyr")
    require("tidyr")
    require("stringr")
    require("lubridate")
    require("purrr")
    require("manifestoR")
    require("tm")
    
    ### import function, Anwendung in get_data
    import <- function(my_corpus, corpus = "") {
      # Args:
      #  my_corpus::
      #  corpus:: str
      # Returns:
      #  df: DataFrame
      if (length(as.data.frame(my_corpus[[corpus]])) == 0) {
        df <- data.frame(matrix(ncol = 4, nrow = 0))
        x <- c("text", "cmp_code", "corpus_code", "text_processed")
        colnames(df) <- x
        df$text <- as.character()
        df$cmp_code <- as.character()
        df$corpus_code <- as.character()
        df$text_processed <- as.character()
        return(df)
      } else {
        df <- as.data.frame(my_corpus[[corpus]]) %>%
          dplyr::select(text, cmp_code) %>%
          tidyr::drop_na() %>%
          dplyr::mutate(corpus_code = corpus) %>%
          dplyr::mutate(text_processed = stringr::str_trim(text)) %>%
          dplyr::mutate(text_processed = stringr::str_squish(text_processed)) %>% 
          dplyr::mutate(text_processed = stringr::str_to_lower(text_processed))
        return(df)
      }
    }

    ### get_corpus_code function
    get_corpus_code <- function(api_key, start_day, country_vec,
                                reliability) {
    # Args:
    #   api_key:: str
    #     Der API-Key um die ManifestoR-API zu benutzen.
    #   start_day:: lubridate::date()
    #       Ab welchem Datum sollen Manifestos ausgewählt werden?
    #   country_vec:: c("")
    #       Character-Vector mit den Namen der Länder, aus denen man die Manifestos bekommen will.
    #   reliability:: float
    #       Die mininale Coder-Reliabilität, um Manifestos zurück zu erhalten. Zusätzlich immer Coder=199 --> Group-Mitglieder
    # Returns:
    #   corpus_code:: c("")
    #       Ein Character-Vector mit den ID-Codes von Wahlprogrammen, welche den Anfordungen der Eingaben entsprechen.
      corpus_list <- manifestoR::mp_maindataset(version = "current",
                                                south_america = FALSE,
                                                apikey = api_key) %>%
        dplyr::filter(countryname %in% country_vec) %>%
        dplyr::mutate(corpus_code = paste(party, date, sep = "_")) %>%
        dplyr::select(countryname, party, edate, date, corpus_code, partyname,testresult, coderid) %>%
        dplyr::mutate(edate = lubridate::ymd(edate)) %>%
        dplyr::mutate(year = lubridate::year(edate)) %>%
        dplyr::filter(year >= lubridate::year(start_day)) %>% 
        dplyr::filter(testresult >= reliability | coderid <= 199)
      
      corpus_code <- as.vector(corpus_list$corpus_code)
      
      return(corpus_code)
    }

    ### Funktion zum Rekodieren des CMP-Codes in Links/Rechts-Einteilung. Wird in get_data() aufgerufen.
    recode_sentiment <- function(df){
      # Args:
      #     df:: DataFrame
      #       Ein DataFrame, dass in get_data() erstellt wird.
      # Returns:
      #     df:: DataFrame
      df <- df %>% 
        mutate(left_right=case_when(
          cmp_code == '103' ~  1,
          cmp_code == '103.1' ~  1,
          cmp_code == '103.2' ~  1,
          cmp_code == '105' ~  1,
          cmp_code == '106' ~  1,
          cmp_code == '107' ~  1,
          cmp_code == '202' ~  1,
          cmp_code == '202.1' ~  1,
          cmp_code == '202.2' ~  1,
          cmp_code == '202.3' ~  1,
          cmp_code == '202.4' ~  1,
          cmp_code == '403' ~  1,
          cmp_code == '404' ~  1,
          cmp_code == '406' ~  1,
          cmp_code == '412' ~  1,
          cmp_code == '413' ~  1,
          cmp_code == '504' ~  1,
          cmp_code == '506' ~  1,
          cmp_code == '701' ~  1, # Ende der linken Labels
          cmp_code == '104' ~  2,
          cmp_code == '201' ~  2,
          cmp_code == '203' ~  2,
          cmp_code == '305' ~  2,
          cmp_code == '401' ~  2,
          cmp_code == '402' ~  2,
          cmp_code == '407' ~  2,
          cmp_code == '414' ~  2,
          cmp_code == '505' ~  2,
          cmp_code == '601' ~  2,
          cmp_code == '603' ~  2,
          cmp_code == '605' ~  2,
          cmp_code == '606' ~  2,
          cmp_code == '201.1' ~  2,
          cmp_code == '201.2' ~  2,
          cmp_code == '305.1' ~  2,
          cmp_code == '305.2' ~  2,
          cmp_code == '305.3' ~  2,
          cmp_code == '305.4' ~  2,
          cmp_code == '305.5' ~  2,
          cmp_code == '305.6' ~  2,
          cmp_code == '601.1' ~  2,
          cmp_code == '601.2' ~  2,
          cmp_code == '605.1' ~  2,
          cmp_code == '605.2' ~  2,
          cmp_code == '606.1' ~  2,
          cmp_code == '606.2' ~  2, # Ende der rechten Labels
          cmp_code == 'H' ~  NA_real_, # Überschriften
          TRUE ~ 0 # Neutrale Labels
        )) %>% 
        drop_na()
      return(df)
    }

    # Funktion zum Einteilen in die Policy-Domänen. Wird in get_data() angewendet.
    recode_policy <- function(df){
      df <- df %>%
         mutate(policy=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  1,
            cmp_code == '102' ~  1,
            cmp_code == '103' ~  1,
            cmp_code == '104' ~  1,
            cmp_code == '105' ~  1,
            cmp_code == '106' ~  1,
            cmp_code == '107' ~  1,
            cmp_code == '108' ~  1,
            cmp_code == '109' ~  1,
            cmp_code == '110' ~  1,
            cmp_code == '201' ~  2,
            cmp_code == '202' ~  2,
            cmp_code == '203' ~  2,
            cmp_code == '204' ~  2,
            cmp_code == '301' ~  3,
            cmp_code == '302' ~  3,
            cmp_code == '303' ~  3,
            cmp_code == '304' ~  3,
            cmp_code == '305' ~  3,
            cmp_code == '401' ~  4,
            cmp_code == '402' ~  4,
            cmp_code == '403' ~  4,
            cmp_code == '404' ~  4,
            cmp_code == '405' ~  4,
            cmp_code == '406' ~  4,
            cmp_code == '407' ~  4,
            cmp_code == '408' ~  4,
            cmp_code == '409' ~  4,
            cmp_code == '410' ~  4,
            cmp_code == '411' ~  4,
            cmp_code == '412' ~  4,
            cmp_code == '413' ~  4,
            cmp_code == '414' ~  4,
            cmp_code == '415' ~  4,
            cmp_code == '416' ~  4,
            cmp_code == '501' ~  5,
            cmp_code == '502' ~  5,
            cmp_code == '503' ~  5,
            cmp_code == '504' ~  5,
            cmp_code == '505' ~  5,
            cmp_code == '506' ~  5,
            cmp_code == '507' ~  5,
            cmp_code == '601' ~  6,
            cmp_code == '602' ~  6,
            cmp_code == '603' ~  6,
            cmp_code == '604' ~  6,
            cmp_code == '605' ~  6,
            cmp_code == '606' ~  6,
            cmp_code == '607' ~  6,
            cmp_code == '608' ~  6,
            cmp_code == '701' ~  7,
            cmp_code == '702' ~  7,
            cmp_code == '703' ~  7,
            cmp_code == '704' ~  7,
            cmp_code == '705' ~  7,
            cmp_code == '706' ~  7,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  1,
            cmp_code == '103.2' ~  1,
            cmp_code == '201.1' ~  2,
            cmp_code == '201.2' ~  2,
            cmp_code == '202.1' ~  2,
            cmp_code == '202.2' ~  2,
            cmp_code == '202.3' ~  2,
            cmp_code == '202.4' ~  2,
            cmp_code == '305.1' ~  3,
            cmp_code == '305.2' ~  3,
            cmp_code == '305.3' ~  3,
            cmp_code == '305.4' ~  3,
            cmp_code == '305.5' ~  3,
            cmp_code == '305.6' ~  3,
            cmp_code == '416.1' ~  4,
            cmp_code == '416.2' ~  4,
            cmp_code == '601.1' ~  6,
            cmp_code == '601.2' ~  6,
            cmp_code == '602.1' ~  6,
            cmp_code == '602.2' ~  6,
            cmp_code == '605.1' ~  6,
            cmp_code == '605.2' ~  6,
            cmp_code == '606.1' ~  6,
            cmp_code == '606.2' ~  6,
            cmp_code == '607.1' ~  6,
            cmp_code == '607.2' ~  6,
            cmp_code == '607.3' ~  6,
            cmp_code == '608.1' ~  6,
            cmp_code == '608.2' ~  6,
            cmp_code == '608.3' ~  6,
            cmp_code == '703.1' ~  7,
            cmp_code == '703.2' ~  7,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Foreign Policy recode. Wird in get_data() angewendet.
    recode_foreign_policy <- function(df){
      df <- df %>%
         mutate(foreign_policy=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  1,
            cmp_code == '102' ~  1,
            cmp_code == '103' ~  2,
            cmp_code == '104' ~  3,
            cmp_code == '105' ~  3,
            cmp_code == '106' ~  4,
            cmp_code == '107' ~  5,
            cmp_code == '108' ~  6,
            cmp_code == '109' ~  5,
            cmp_code == '110' ~  6,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  2,
            cmp_code == '103.2' ~  2,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Foreign Relationships recode. Wird in get_data() angewendet.
    recode_foreign_relationships <- function(df){
      df <- df %>%
         mutate(foreign_relationship=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  1,
            cmp_code == '102' ~  2,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Military recode. Wird in get_data() angewendet.
    recode_military <- function(df){
      df <- df %>%
         mutate(foreign_relationship=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  1,
            cmp_code == '105' ~  2,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Internationalism recode. Wird in get_data() angewendet.
    recode_internationalism <- function(df){
      df <- df %>%
         mutate(internationalism=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  1,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  2,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # EU recode. Wird in get_data() angewendet.
    recode_eu <- function(df){
      df <- df %>%
         mutate(state_unions=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  1,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  2,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Freedom recode. Wird in get_data() angewendet.
    recode_freedom <- function(df){
      df <- df %>%
         mutate(freedom=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  1,
            cmp_code == '202' ~  2,
            cmp_code == '203' ~  3,
            cmp_code == '204' ~  3,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  1,
            cmp_code == '201.2' ~  1,
            cmp_code == '202.1' ~  2,
            cmp_code == '202.2' ~  2,
            cmp_code == '202.3' ~  2,
            cmp_code == '202.4' ~  2,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Constitutionalism recode. Wird in get_data() angewendet.
    recode_constitutionalism <- function(df){
      df <- df %>%
         mutate(constitutionalism=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  1,
            cmp_code == '204' ~  2,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Political System recode. Wird in get_data() angewendet.
    recode_political_system <- function(df){
      df <- df %>%
         mutate(political_system=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  1,
            cmp_code == '302' ~  1,
            cmp_code == '303' ~  2,
            cmp_code == '304' ~  3,
            cmp_code == '305' ~  4,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  4,
            cmp_code == '305.2' ~  4,
            cmp_code == '305.3' ~  4,
            cmp_code == '305.4' ~  4,
            cmp_code == '305.5' ~  4,
            cmp_code == '305.6' ~  4,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Centralisation recode. Wird in get_data() angewendet.
    recode_centralisation <- function(df){
      df <- df %>%
         mutate(centralisation=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  1,
            cmp_code == '302' ~  2,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  0,
            cmp_code == '402' ~  0,
            cmp_code == '403' ~  0,
            cmp_code == '404' ~  0,
            cmp_code == '405' ~  0,
            cmp_code == '406' ~  0,
            cmp_code == '407' ~  0,
            cmp_code == '408' ~  0,
            cmp_code == '409' ~  0,
            cmp_code == '410' ~  0,
            cmp_code == '411' ~  0,
            cmp_code == '412' ~  0,
            cmp_code == '413' ~  0,
            cmp_code == '414' ~  0,
            cmp_code == '415' ~  0,
            cmp_code == '416' ~  0,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  0,
            cmp_code == '416.2' ~  0,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }

    # Economy recode. Wird in get_data() angewendet.
    recode_economy <- function(df){
      df <- df %>%
         mutate(economy=case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  0,
            cmp_code == '102' ~  0,
            cmp_code == '103' ~  0,
            cmp_code == '104' ~  0,
            cmp_code == '105' ~  0,
            cmp_code == '106' ~  0,
            cmp_code == '107' ~  0,
            cmp_code == '108' ~  0,
            cmp_code == '109' ~  0,
            cmp_code == '110' ~  0,
            cmp_code == '201' ~  0,
            cmp_code == '202' ~  0,
            cmp_code == '203' ~  0,
            cmp_code == '204' ~  0,
            cmp_code == '301' ~  0,
            cmp_code == '302' ~  0,
            cmp_code == '303' ~  0,
            cmp_code == '304' ~  0,
            cmp_code == '305' ~  0,
            cmp_code == '401' ~  1,
            cmp_code == '402' ~  2,
            cmp_code == '403' ~  3,
            cmp_code == '404' ~  4,
            cmp_code == '405' ~  5,
            cmp_code == '406' ~  6,
            cmp_code == '407' ~  6,
            cmp_code == '408' ~  7,
            cmp_code == '409' ~  8,
            cmp_code == '410' ~  9,
            cmp_code == '411' ~  10,
            cmp_code == '412' ~  11,
            cmp_code == '413' ~  12,
            cmp_code == '414' ~  13,
            cmp_code == '415' ~  14,
            cmp_code == '416' ~  15,
            cmp_code == '501' ~  0,
            cmp_code == '502' ~  0,
            cmp_code == '503' ~  0,
            cmp_code == '504' ~  0,
            cmp_code == '505' ~  0,
            cmp_code == '506' ~  0,
            cmp_code == '507' ~  0,
            cmp_code == '601' ~  0,
            cmp_code == '602' ~  0,
            cmp_code == '603' ~  0,
            cmp_code == '604' ~  0,
            cmp_code == '605' ~  0,
            cmp_code == '606' ~  0,
            cmp_code == '607' ~  0,
            cmp_code == '608' ~  0,
            cmp_code == '701' ~  0,
            cmp_code == '702' ~  0,
            cmp_code == '703' ~  0,
            cmp_code == '704' ~  0,
            cmp_code == '705' ~  0,
            cmp_code == '706' ~  0,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            # Zusammenfassung der Subkategorien
            cmp_code == '103.1' ~  0,
            cmp_code == '103.2' ~  0,
            cmp_code == '201.1' ~  0,
            cmp_code == '201.2' ~  0,
            cmp_code == '202.1' ~  0,
            cmp_code == '202.2' ~  0,
            cmp_code == '202.3' ~  0,
            cmp_code == '202.4' ~  0,
            cmp_code == '305.1' ~  0,
            cmp_code == '305.2' ~  0,
            cmp_code == '305.3' ~  0,
            cmp_code == '305.4' ~  0,
            cmp_code == '305.5' ~  0,
            cmp_code == '305.6' ~  0,
            cmp_code == '416.1' ~  15,
            cmp_code == '416.2' ~  15,
            cmp_code == '601.1' ~  0,
            cmp_code == '601.2' ~  0,
            cmp_code == '602.1' ~  0,
            cmp_code == '602.2' ~  0,
            cmp_code == '605.1' ~  0,
            cmp_code == '605.2' ~  0,
            cmp_code == '606.1' ~  0,
            cmp_code == '606.2' ~  0,
            cmp_code == '607.1' ~  0,
            cmp_code == '607.2' ~  0,
            cmp_code == '607.3' ~  0,
            cmp_code == '608.1' ~  0,
            cmp_code == '608.2' ~  0,
            cmp_code == '608.3' ~  0,
            cmp_code == '703.1' ~  0,
            cmp_code == '703.2' ~  0,
            TRUE ~ NA_real_
        )) %>% 
        drop_na()
      return(df)
    }


    ### get_data_function
    get_data <- function(corpus_code=c(""), my_corpus) {
      # Args:
      #   corpus_code:: c("")
      #       Ein Character-Vector mit den ID-Codes von Wahlprogrammen, welche den Anfordungen der Eingaben entsprechen. Aus get_corpus_code()
      #   my_corpus:: ManifestoCorpus
      #       Object aus manifestoR::mp_corpus(). Enthält das komplette Dataset
      # Returns:
      #    df:: DataFrame

      # Leeres DF erstellen
      df <-  data.frame(matrix(ncol = 4, nrow = 0))
      x <- c("text", "cmp_code", "corpus_code", "text_processed")
      colnames(df) <- x
      df$text <- as.character()
      df$cmp_code <- as.character()
      df$corpus_code <- as.character()
      df$text_processed <- as.character()
      
      # df zeilenweise durch my_corpus & corpus_code auffüllen.
      df <- purrr::map_dfr(corpus_code, function(i = .x) {
        rbind(df, import(my_corpus, corpus = i)) # Anwendung der Import-Funktion. Loopt zeilenweise über my_corpus.
      }) 

      # Links/Rechts
      df <- recode_sentiment(df) # Anwendung der Funktion zur Links/Rechts Einteilung

      # Policies
      df <- recode_policy(df) # Anwendung der Funktion für Policy-Domänen

      # Foreign Policy
      df <- recode_foreign_policy(df) # Anwendung der Funktion für AUßenpolitik

      # Foreign Relationship
      df <- recode_foreign_relationships(df) # Anwendung der Funktion für spezielle Beziehungen

      # Military
      df <- recode_military(df) # Anwendung der Funktion für Militär.

      # Internationalism
      df <- recode_internationalism(df) # Anwendung der Funktion für Internationalismus.

      # EU
      df <- recode_eu(df) # Anwendung der Funktion für internationale Staatengemeinschaften.

      # Freedom
      df <- recode_freedom(df) # Anwendung der Funktion für Freiheit.

      # Constitutionalism
      df <- recode_constitutionalism(df) # Anwendung der Funktion für Konstitutionalismus.

      # Political System
      df <- recode_political_system(df) # Anwendung der Funktion für das Politische System.

      # Centralisation
      df <- recode_centralisation(df) # Anwendung der Funktion für Zentralisierung.

      # Economy
      df <- recode_economy(df) # Anwendung der Funktion für die Wirtschaft.
      
      # Bisher sind die Zeilen des DataFrames nach der Position im jeweiligen Wahlprogramm sortiert.
      # Um einen Einfluss auf das Training zu verhindern, wird nun die Reihenfolge der Zeilen per Zufall geändert.
      # Aus Gründen der Reproduzierbarkeit wird der Seed der Zufallsauswahl auf 42 festgesetzt. 
      set.seed(seed)
      df <- df %>% 
        slice_sample(prop=1)

      # Text_processed in Text ändern
      df <- df %>%
        dplyr::mutate(text=text_processed) %>%
        dplyr::select(-text_processed)

      return(df)
    }

    #### Manifesto-API-Key
    manifestoR::mp_setapikey(api_key)
    
    ### load Manifesto-Corpus
    my_corpus <- manifestoR::mp_corpus(edate >= start_day)
    
    ### load corpus_code
    corpus_code <- get_corpus_code(api_key, start_day, country_vec, reliability)
    
    ### get_data
    df <- get_data(corpus_code = corpus_code, my_corpus = my_corpus)
    
    return(df)
  }
