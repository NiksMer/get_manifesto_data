get_training_data <-
  function(api_key = "",
           start_day = lubridate::date("1945-01-01"),
           country_vec = c(""),
           reliability = 0.8) {
    ### Packages laden
    require("dplyr")
    require("tidyr")
    require("stringr")
    require("lubridate")
    require("purrr")
    require("manifestoR")
    require("tm")
    
    ### import function
    import <- function(my_corpus, corpus = "") {
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
    
    #### Manifesto-API-Key
    manifestoR::mp_setapikey(api_key)
    
    ### load Manifesto-Corpus
    my_corpus <- manifestoR::mp_corpus(edate >= start_day)
    
    ### get_corpus_code function
    get_corpus_code <- function(api_key, start_day, country_vec,
                                reliability) {
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
    
    ### load corpus_code
    corpus_code <- get_corpus_code(api_key, start_day, country_vec, reliability)
    
    ### Left or Right
    recode_sentiment <- function(df){
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
    
    ### get_data_function
    get_data <- function(corpus_code, my_corpus) {
      df <-  data.frame(matrix(ncol = 4, nrow = 0))
      x <- c("text", "cmp_code", "corpus_code", "text_processed")
      colnames(df) <- x
      df$text <- as.character()
      df$cmp_code <- as.character()
      df$corpus_code <- as.character()
      df$text_processed <- as.character()
      df <- purrr::map_dfr(corpus_code, function(i = .x) {
        rbind(df, import(my_corpus, corpus = i))
      }) %>%
        dplyr::mutate(
          label = dplyr::case_when(
            cmp_code == '000' ~  0,
            cmp_code == '101' ~  1,
            cmp_code == '102' ~  2,
            cmp_code == '103' ~  3,
            cmp_code == '104' ~  4,
            cmp_code == '105' ~  5,
            cmp_code == '106' ~  6,
            cmp_code == '107' ~  7,
            cmp_code == '108' ~  8,
            cmp_code == '109' ~  9,
            cmp_code == '110' ~  10,
            cmp_code == '201' ~  11,
            cmp_code == '202' ~  12,
            cmp_code == '203' ~  13,
            cmp_code == '204' ~  14,
            cmp_code == '301' ~  15,
            cmp_code == '302' ~  16,
            cmp_code == '303' ~  17,
            cmp_code == '304' ~  18,
            cmp_code == '305' ~  19,
            cmp_code == '401' ~  20,
            cmp_code == '402' ~  21,
            cmp_code == '403' ~  22,
            cmp_code == '404' ~  23,
            cmp_code == '405' ~  24,
            cmp_code == '406' ~  25,
            cmp_code == '407' ~  26,
            cmp_code == '408' ~  27,
            cmp_code == '409' ~  28,
            cmp_code == '410' ~  29,
            cmp_code == '411' ~  30,
            cmp_code == '412' ~  31,
            cmp_code == '413' ~  32,
            cmp_code == '414' ~  33,
            cmp_code == '415' ~  34,
            cmp_code == '416' ~  35,
            cmp_code == '501' ~  36,
            cmp_code == '502' ~  37,
            cmp_code == '503' ~  38,
            cmp_code == '504' ~  39,
            cmp_code == '505' ~  40,
            cmp_code == '506' ~  41,
            cmp_code == '507' ~  42,
            cmp_code == '601' ~  43,
            cmp_code == '602' ~  44,
            cmp_code == '603' ~  45,
            cmp_code == '604' ~  46,
            cmp_code == '605' ~  47,
            cmp_code == '606' ~  48,
            cmp_code == '607' ~  49,
            cmp_code == '608' ~  50,
            cmp_code == '701' ~  51,
            cmp_code == '702' ~  52,
            cmp_code == '703' ~  53,
            cmp_code == '704' ~  54,
            cmp_code == '705' ~  55,
            cmp_code == '706' ~  56,
            cmp_code == 'H' ~  NA_real_, # Überschriften raus, wegen Trennschärfe
            cmp_code == '103.1' ~  3,
            cmp_code == '103.2' ~  3,
            cmp_code == '201.1' ~  11,
            cmp_code == '201.2' ~  11,
            cmp_code == '202.1' ~  12,
            cmp_code == '202.2' ~  12,
            cmp_code == '202.3' ~  12,
            cmp_code == '202.4' ~  12,
            cmp_code == '305.1' ~  19,
            cmp_code == '305.2' ~  19,
            cmp_code == '305.3' ~  19,
            cmp_code == '305.6' ~  19,
            cmp_code == '416.1' ~  35,
            cmp_code == '416.2' ~  35,
            cmp_code == '601.1' ~  43,
            cmp_code == '601.2' ~  43,
            cmp_code == '602.1' ~  44,
            cmp_code == '602.2' ~  44,
            cmp_code == '605.1' ~  47,
            cmp_code == '605.2' ~  47,
            cmp_code == '606.1' ~  48,
            cmp_code == '606.2' ~  48,
            cmp_code == '607.1' ~  49,
            cmp_code == '607.2' ~  49,
            cmp_code == '607.3' ~  49,
            cmp_code == '608.1' ~  50,
            cmp_code == '608.2' ~  50,
            cmp_code == '608.3' ~  50,
            cmp_code == '703.1' ~  53,
            cmp_code == '703.2' ~  53,
            TRUE ~ NA_real_
          )
        ) %>% 
        drop_na() # Leere löschen
      
      df <- recode_sentiment(df)
      
      set.seed(42)
      df <- df %>% 
        slice_sample(prop=1)
      return(df)
    }
    
    ### get_data
    df <- get_data(corpus_code = corpus_code, my_corpus = my_corpus)
    
    return(df)
  }
