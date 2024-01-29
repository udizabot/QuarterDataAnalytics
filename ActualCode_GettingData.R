###########################################################################
# Getting Data From Market ------------------------------------------------
###########################################################################

# Load Libraries ----------------------------------------------------------
pacman::p_load(tidyverse, rvest, janitor, googledrive)

# Set Current Date --------------------------------------------------------
CUR_DATE <- Sys.Date()


###########################################################################
# Fleck Imobiliária -------------------------------------------------------

CODE <- "IM231S"

fleck.locac <- paste0("https://imobiliariafleck.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    fleck.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .}
        )) ) %>%
    map_dfr( . , bind_rows) 

IM231S.L <- overview %>%
    mutate( . , 
            CTGY2 = str_squish(CTGY) %>% str_remove_all( . , "R\\$ ") %>% parse_character()) %>%
    separate(col = CTGY2, into = LETTERS[1:4], sep = " ") %>%
    transform( . , 
               PRICE2 = case_when(A == "LOCAÇÃO" ~ B, C == "VENDA" ~ D) ) %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_remove_all(PRICE, "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "R\\$|[.,]|[:digit:]|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM231S.L
rm(overview, fleck.locac)


fleck.venda <- paste0("https://imobiliariafleck.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    fleck.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 


IM231S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM231S.V
rm(overview, fleck.venda)

IM231S <- full_join(IM231S.L, IM231S.V)
rm(IM231S.L, IM231S.V, CODE)


###########################################################################
# Realizare Imobiliária ---------------------------------------------------

CODE <- "IM221S"

realizare.locac <- paste0("https://realizareimobiliaria.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    realizare.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM221S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM221S.L
rm(overview, realizare.locac)


realizare.venda <- paste0("https://realizareimobiliaria.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    realizare.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 


IM221S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM221S.V
rm(overview, realizare.venda)

IM221S <- full_join(IM221S.L, IM221S.V)
rm(IM221S.L, IM221S.V, CODE)


###########################################################################
# Sinop Imóveis -----------------------------------------------------------

CODE <- "IM211S"

sinop.locac <- paste0("https://sinopimoveis.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    sinop.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM211S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM211S.L
rm(overview, sinop.locac)

sinop.venda <- paste0("https://sinopimoveis.com.br/vendas/?&pagina=", 1:30, "&ordem=data")

overview <- map(
    sinop.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM211S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM211S.V
rm(overview, sinop.venda)

IM211S <- full_join(IM211S.L, IM211S.V)
rm(IM211S.L, IM211S.V, CODE)


###########################################################################
# Imobiliária Flamboyants -------------------------------------------------

CODE <- "IM291S"

flambo.locac <- paste0("https://imobiliariaflamboyants.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    flambo.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM291S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM291S.L
rm(overview, flambo.locac)

flambo.venda <- paste0("https://imobiliariaflamboyants.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    flambo.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM291S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM291S.V
rm(overview, flambo.venda)

IM291S <- full_join(IM291S.L, IM291S.V)
rm(IM291S.L, IM291S.V, CODE)


###########################################################################
# Imobiliária Habitar -----------------------------------------------------

CODE <- "IM321S"

habitar.locac <- paste0("https://habitarsinop.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    habitar.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM321S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM321S.L
rm(overview, habitar.locac)


habitar.venda <- paste0("https://habitarsinop.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    habitar.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM321S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM321S.V
rm(overview, habitar.venda)

IM321S <- full_join(IM321S.L, IM321S.V)
rm(IM321S.L, IM321S.V, CODE)


###########################################################################
# Imobiliária Centro Oeste ------------------------------------------------

CODE <- "IM341S"

coeste.locac <- paste0("https://imobiliariacentrooeste.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    coeste.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM341S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM341S.L
rm(overview, coeste.locac)

coeste.venda <- paste0("https://imobiliariacentrooeste.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    coeste.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 


IM341S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM341S.V
rm(overview, coeste.venda)

IM341S <- full_join(IM341S.L, IM341S.V)
rm(IM341S.L, IM341S.V, CODE)


###########################################################################
# Glaciosa Imobiliária ----------------------------------------------------

CODE <- "IM421S"

glaciosa.locac <- paste0("https://glaciosaimobiliaria.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    glaciosa.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM421S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM421S.L
rm(overview, glaciosa.locac)

glaciosa.venda <- paste0("https://glaciosaimobiliaria.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    glaciosa.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM421S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM421S.V
rm(overview, glaciosa.venda)

IM421S <- full_join(IM421S.L, IM421S.V)
rm(IM421S.L, IM421S.V, CODE)


###########################################################################
# Visão Imobiliária -------------------------------------------------------

CODE <- "IM481S"

visao.locac <- paste0("https://visaoempreendimento.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    visao.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM481S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM481S.L
rm(overview, visao.locac)

visao.venda <- paste0("https://visaoempreendimento.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    visao.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM481S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM481S.V
rm(overview, visao.venda)

IM481S <- full_join(IM481S.L, IM481S.V)
rm(IM481S.L, IM481S.V, CODE)


###########################################################################
# Amortec Imobiliária -----------------------------------------------------

CODE <- "IM511S"

amortec.locac <- paste0("https://amortecimobiliaria.com.br/locacao/?&pagina=", 1:15, "&ordem=data")

overview <- map(
    amortec.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .})) ) %>%
    map_dfr( . , bind_rows) 

IM511S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|VENDA|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM511S.L
rm(overview, amortec.locac)

amortec.venda <- paste0("https://amortecimobiliaria.com.br/vendas/?&pagina=", 1:50, "&ordem=data")

overview <- map(
    amortec.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM511S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM511S.V
rm(overview, amortec.venda)

IM511S <- full_join(IM511S.L, IM511S.V)
rm(IM511S.L, IM511S.V, CODE)


###########################################################################
# Victorum Imobiliária ----------------------------------------------------

CODE <- "IM551S"

victorum.venda <- paste0("https://www.victorumimoveis.com.br/imoveis/a-venda?pagina=", 1:50)

overview <- map(
    victorum.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#content .bgWhite") %>%
        map_df(~list(description = .x %>% html_nodes('.text-capitalize a') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     SIZE = .x %>% html_nodes('ref+ ref strong') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.textSecondary') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     PROPTY = .x %>% html_nodes("p") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     CTGY = .x %>% html_nodes(".priceWrap") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     OTHER = .x %>% html_nodes(".list-unstyled") %>% html_text() %>% {if(length(.) == 0) NA else .} )) ) %>%
    map_dfr( . , bind_rows) 

IM551S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE,
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_remove_all(SIZE, "m²") %>% parse_number(),
            CTGY = str_remove_all(CTGY, "\r\n|R\\$|[.,]|[:digit:]|LOCAÇÃO|CONSULTE") %>% str_squish(.),
            PROPTY = str_remove_all(PROPTY, "TIPO: |REF: |\\.|m²") %>% str_replace_all( . , "[:digit:]", "") %>% str_squish(.), 
            NBHD = str_remove_all(description, "PARA|NO |CASA|LOCAÇÃO|APARTAMENTO|SALA COMERCIAL|-|TÉRREO|TERRENO|QUITINETE") %>% str_squish(.), 
            OTHER = str_squish(OTHER) %>% str_remove_all( . , "\r\n") %>% parse_character() ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% separate(col = OTHER, into = letters[1:16], sep = " ") %>%
    transform( . , 
               DORMS = case_when(a == "Dorm." ~ b, c == "Dorm." ~ d, e == "Dorm." ~ f, g == "Dorm." ~ h, 
                                 i == "Dorm." ~ j, k == "Dorm." ~ l, m == "Dorm." ~ n, o == "Dorm." ~ p) %>% parse_number(),
               SUITE = case_when(a == "Suítes" ~ b, c == "Suítes" ~ d, e == "Suítes" ~ f, g == "Suítes" ~ h, 
                                 i == "Suítes" ~ j, k == "Suítes" ~ l, m == "Suítes" ~ n, o == "Suítes" ~ p) %>% parse_number(),
               BANHE = case_when(a == "Ban." ~ b, c == "Ban." ~ d, e == "Ban." ~ f, g == "Ban." ~ h, 
                                 i == "Ban." ~ j, k == "Ban." ~ l, m == "Ban." ~ n, o == "Ban." ~ p) %>% parse_number(),
               VAGAS = case_when(a == "Vagas" ~ b, c == "Vagas" ~ d, e == "Vagas" ~ f, g == "Vagas" ~ h, 
                                 i == "Vagas" ~ j, k == "Vagas" ~ l, m == "Vagas" ~ n, o == "Vagas" ~ p) %>% parse_number() ) %>%
    as_tibble() %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    group_by(PROPTY, CTGY, NBHD, SIZE, PRICE) %>%
    slice( ., ifelse(all(is.na(DORMS)), 1 , which(!is.na(DORMS)))) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM551S.V
rm(overview, victorum.venda)

IM551S <- IM551S.V
rm(IM551S.V, CODE)


###########################################################################
# Tozi Imobiliária --------------------------------------------------------
CODE <- "IM251S"

tozi.locac <- paste0("https://www.toziimoveis.com.br/imoveis?pg=", 1:15, "&busca=aluguel")

overview <- map(
    tozi.locac, 
    . %>% 
        read_html() %>%
        html_nodes(".col-md-6") %>%
        map_df(~list(description = .x %>% html_nodes('.sobre , .desc') %>% html_text(), 
                     NBHD = .x %>% html_nodes('.text-trucate , strong') %>% html_text(), #%>% {if(length(.) == 0) NA else .}
                     #SIZE = .x %>% html_nodes('.flex-fill') %>% html_text() #%>% {if(length(.) == 0) NA else .}, 
                     PRICE = .x %>% html_nodes('.valor') %>% html_text(), #%>% {if(length(.) == 0) NA else .}
                     PROPTY = .x %>% html_nodes(".cat .flex-fill:nth-child(1)") %>% html_text(), #%>% {if(length(.) == 0) NA else .}
                     #OTHER = .x %>% html_nodes(".sobre , .infos .flex-fill") %>% html_text() %>% as.character() #{if(length(.) == 0) NA else .} 
                     CDGO = .x %>% html_nodes(".fixed-top-left") %>% html_text() #%>% {if(length(.) == 0) NA else .}, 
        ))) %>%
    map_dfr( . , bind_rows) 


IM251S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_replace_all(description, " ", "" ) %>% str_remove_all( . , "talde+\\d{1,5}\\.\\d{1,5}+M²|tal+\\d{1,5}\\.\\d{1,5}[,]\\d{1,2}+M²") %>% 
                str_extract( . , "\\d{1,5}\\.\\d{1,5}+M²|\\d{1,5}\\[,]\\d{1,5}+M²|\\d{1,5}+M²|\\d{1,5}\\.\\d{1,5}[,]\\d{1,2}+M²|\\d{1,5}[,]\\d{1,2}+M²|
                         \\d{1,5}\\.\\d{1,5}+m²|\\d{1,5}\\[,]\\d{1,5}+m²|\\d{1,5}+m²|\\d{1,5}\\.\\d{1,5}[,]\\d{1,2}+m²|\\d{1,5}[,]\\d{1,2}+m²") %>% parse_number(),
            DORMS = str_extract(description, "\\d{1,2}+ Q|\\d{1,2}+ q"),
            SUITE = str_extract(description, "\\d{1,2}+ Su|\\d{1,2}+ SU"),
            BANHE = str_extract(description, "\\d{1,2}+ B|\\d{1,2}+ b"),
            VAGAS = str_extract(description, "\\d{1,2}+ G|Garagem"),
            CTGY = str_extract_all(description, "LOCAÇ|locaç|Locaç|LCAÇ|VENDA|Venda|venda") %>% str_squish(.),
            NBHD = str_remove_all(NBHD, " Sinop") %>% str_squish(.), 
            CODE = CODE) %>%
    mutate( . , 
            SIZE = str_remove_all(SIZE, "M²|m²|\\.") %>% str_replace( . , ",", ".") %>% parse_number(),
            DORMS = str_remove_all(DORMS, " Q| q|") %>% parse_number(), 
            SUITE = str_remove_all(SUITE, " Su| SU|") %>% parse_number(), 
            BANHE = str_remove_all(BANHE, " B| b|") %>% parse_number(), 
            VAGAS = str_remove_all(VAGAS, " G") %>% str_replace( . , "Garagem", "1") %>% parse_number(), 
            CTGY = str_replace_all(CTGY, "LOCAÇ|locaç|Locaç|LCAÇ", "LOCAÇÃO")) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% 
    distinct( . ,
              CTGY, NBHD, SIZE, PRICE, .keep_all = T) %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM251S.L
rm(overview, tozi.locac)

tozi.venda <- paste0("https://www.toziimoveis.com.br/imoveis?pg=", 1:50, "&busca=venda")

overview <- map(
    tozi.venda, 
    . %>% 
        read_html() %>%
        html_nodes(".col-md-6") %>%
        map_df(~list(description = .x %>% html_nodes('.sobre , .desc') %>% html_text(), 
                     NBHD = .x %>% html_nodes('.text-trucate , strong') %>% html_text(),  
                     PRICE = .x %>% html_nodes('.valor') %>% html_text(), 
                     PROPTY = .x %>% html_nodes(".cat .flex-fill:nth-child(1)") %>% html_text(), 
                     CDGO = .x %>% html_nodes(".fixed-top-left") %>% html_text()  
        ))) %>%
    map_dfr( . , bind_rows) 

IM251S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_replace_all(description, " ", "") %>% #str_remove_all( . , "(\\d{1,2})\\d{1,4}\\.\\d{1,4}") %>% 
                str_extract( . , "\\d{1,3}\\.\\d{1,2}+M²|\\d{1,3}\\,\\d{1,2}+M²|\\d{1,3}\\,\\d{1,2}+MÂ²|\\d{1,3}\\,\\d{1,2}+mÂ²|
                             \\d{1,3}+mÂ²|\\d{1,4}+m2|\\d{1,4}+m²|\\d{1,3}\\,\\d{1,2}+m²|\\d{1,3}\\,\\d{1,2}+m2|\\d{1,4}+m2|\\d{1,4}+M2|\\d{1,4}+M²|\\d{1,3}\\,\\d{1,2}+m"),
            DORMS = str_extract(description, "\\d{1,2}+ Q|\\d{1,2}+ q|\\d{1,2}+ d|\\d{1,2}+ D|\\d{1,2}+  q"),
            SUITE = str_extract(description, "\\d{1,2}+ Su|\\d{1,2}+ SU|\\d{1,2}+ su"),
            BANHE = str_extract(description, "\\d{1,2}+ B|\\d{1,2}+ b|Banh|banh|BANH"),
            VAGAS = str_extract(description, "\\d{1,2}+ G|Garagem|GARAG"),
            CTGY = str_extract_all(description, "LOCAÇ|locaç|Locaç|LCAÇ|VENDA|Venda|venda") %>% str_squish(.),
            NBHD = str_remove_all(NBHD, " Sinop") %>% str_squish(.), 
            CODE = CODE) %>% 
    mutate( . , 
            SIZE = str_remove_all(SIZE, "M²|m²|MÂ²|mÂ²|m2|M2|m") %>% str_replace( . , ",", ".") %>% parse_number(),
            DORMS = str_remove_all(DORMS, " Q| q| d| D|  q") %>% parse_number(), 
            SUITE = str_remove_all(SUITE, " Su| SU| su") %>% parse_number(), 
            BANHE = str_remove_all(BANHE, " B| b|") %>% str_replace( . , "Banh|banh|BANH", "1") %>% parse_number(), 
            VAGAS = str_remove_all(VAGAS, " G") %>% str_replace( . , "Garagem", "1") %>% parse_number(), 
            CTGY = "VENDA")  %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% 
    distinct( . ,
              CTGY, NBHD, SIZE, PRICE, .keep_all = T) %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

IM251S.V
rm(overview, tozi.venda)

IM251S <- full_join(IM251S.L, IM251S.V)
rm(IM251S.L, IM251S.V, CODE)


###########################################################################
# Celeste Imobiliária -----------------------------------------------------

CODE <- "IM261S"

celeste.venda <- paste0("https://www.imobiliariaceleste.com.br/buscar/", 0:15)

overview <- map(
    celeste.venda, 
    . %>% 
        read_html() %>%
        html_nodes(".mb-5") %>%
        map_df(~list(description = .x %>% html_nodes('h2') %>% html_text(), 
                     #NBHD = .x %>% html_nodes('.text-trucate , strong') %>% html_text(), #%>% {if(length(.) == 0) NA else .}
                     SIZET = .x %>% html_nodes('.w-100 .i-terreno') %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     SIZEC = .x %>% html_nodes(".i-construcao") %>% html_text() %>% {if(length(.) == 0) NA else .},
                     PRICE = .x %>% html_nodes('.item-imoveis-list b') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     PROPTY = .x %>% html_nodes(".tipo") %>% html_text() %>% {if(length(.) == 0) NA else .},
                     OTHER = .x %>% html_nodes(".w-100") %>% html_text() %>% {if(length(.) == 0) NA else .}, 
                     DORMS = .x %>% html_nodes('.i-dormitorio') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     VAGAS = .x %>% html_nodes('.i-vaga') %>% html_text() %>% {if(length(.) == 0) NA else .},
                     CDGO = .x %>% html_nodes(".i-terreno.w-100") %>% html_text() %>% {if(length(.) == 0) NA else .} 
        ))) %>%
    map_dfr( . , bind_rows) 

IM261S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            CODE = CODE, 
            SIZE = str_squish(SIZEC) %>% str_remove_all( . , "m²") %>% parse_number(), 
            PRICE = str_squish(PRICE) %>% str_remove_all( . , "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            PROPTY = str_replace_all(PROPTY, " ", "") %>% str_remove_all( . , "AltoPadrão"), 
            NBHD = description,
            DORMS = str_squish(DORMS) %>% parse_number(), 
            SUITE = 0,
            BANHE = 0,
            VAGAS = str_squish(VAGAS) %>% parse_number(),
            CTGY = "VENDA",
            CDGO = str_replace_all(CDGO, " ", "") %>% str_remove_all( . , "Ref.:")
    ) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% 
    distinct( . ,
              CDGO, .keep_all = T) %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

rm(overview, celeste.venda)

IM261S <- IM261S.V
rm(IM261S.V, CODE)


###########################################################################
# Canto a Canto Imóveis ---------------------------------------------------

CODE <- "IM441S"

canto.locac <- paste0("https://cantoacantoimoveis.com.br/alugar/pagina-", 1:15 ,"/")

overview <- map(
    canto.locac, 
    . %>% 
        read_html() %>%
        html_nodes("#conteudo_resultado") %>%
        map_df(~list(description = .x %>% html_nodes('.detalhe') %>% html_text(), 
                     NBHD = .x %>% html_nodes('h2+ h3') %>% html_text(),  
                     PRICE = .x %>% html_nodes('h4') %>% html_text(), 
                     PROPTY = .x %>% html_nodes("h2") %>% html_text(), 
                     OTHER = .x %>% html_nodes("h3+ h3") %>% html_text() %>% as.character(), 
                     CDGO = .x %>% html_nodes("span:nth-child(1) b") %>% html_text()
        ))) %>%
    map_dfr( . , bind_rows) 

IM441S.L <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            PRICE = str_remove_all(PRICE, "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_extract_all(description, "Área construída: \\d{1,5}") %>% str_remove_all(. , "Área construída:") %>% parse_number(),
            SIZET = str_extract_all(description, "Área total: \\d{1,5}", simplify = T) %>% str_remove_all(. , "Área total:") %>% parse_number(),
            CTGY = "LOCAÇÃO" %>% parse_character(),
            DORMS = 0, SUITE = 0, BANHE = 0, VAGAS = 0,
            CODE = CODE) %>%
    filter( . , 
            PRICE != (is.na(PRICE))) %>% 
    distinct( . ,
              CTGY, NBHD, SIZE, PRICE, .keep_all = T) %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

rm(overview, canto.locac)

canto.venda <- paste0("https://cantoacantoimoveis.com.br/comprar/pagina-", 1:50 ,"/")

overview <- map(
    canto.venda, 
    . %>% 
        read_html() %>%
        html_nodes("#conteudo_resultado") %>%
        map_df(~list(description = .x %>% html_nodes('.detalhe') %>% html_text(), 
                     NBHD = .x %>% html_nodes('h2+ h3') %>% html_text(), 
                     PRICE = .x %>% html_nodes('h4') %>% html_text(), 
                     PROPTY = .x %>% html_nodes("h2") %>% html_text(), 
                     OTHER = .x %>% html_nodes("h3+ h3") %>% html_text() %>% as.character(), 
                     CDGO = .x %>% html_nodes("span:nth-child(1) b") %>% html_text() 
        ))) %>%
    map_dfr( . , bind_rows) 

IM441S.V <- overview %>%
    mutate( . , 
            DATE = CUR_DATE, 
            PRICE = str_remove_all(PRICE, "R\\$|\\.|\\,+\\d{1,2}") %>% parse_number(),
            SIZE = str_extract_all(description, "Área construída: \\d{1,5}") %>% 
                str_remove_all(. , "Área construída:") %>% parse_number(),
            SIZET = str_extract_all(description, "Área total: \\d{1,5}", simplify = T) %>% 
                str_remove_all(. , "Área total:") %>% parse_number(),
            CTGY = "VENDA" %>% parse_character(),
            DORMS = 0, SUITE = 0, BANHE = 0, VAGAS = 0,
            CODE = CODE) %>% 
    filter( . , 
            PRICE != (is.na(PRICE))) %>%
    transform( . , 
               SIZE = case_when(SIZE == 0 ~ SIZET, SIZE != 0 ~ SIZE)) %>%
    distinct( . ,
              CTGY, NBHD, SIZE, PRICE, .keep_all = T) %>%
    select( . , 
            DATE, CODE, PROPTY, CTGY, NBHD, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0)))

rm(overview, canto.venda)

IM441S <- full_join(IM441S.L, IM441S.V)
rm(IM441S.L, IM441S.V, CODE)






###########################################################################
# Data Adjustment ---------------------------------------------------------

REAL.ESTATE.BASE <- reduce(list(IM211S,
                                IM221S, IM231S, IM251S, 
                                IM261S, IM291S, 
                                IM321S, IM341S, 
                                #IM421S,
                                #IM441S, 
                                IM481S, 
                                IM511S #IM551S
), full_join)



PROPERTY <- tibble(PAT_PROP = c("APART|QUIT|KITN", "CASA|SOBRAD|CONDOM|", "LOTEAM|TERR", "COMERC|ESPA|PONT", "GALP|BARRAC", "TIO|CARA|FAZE"), 
                   COR_PROP = c("APARTAMENTO", "CASA", "TERRENO", "SALA COMERCIAL", "GALPÃO", "ÁREA RURAL"))

NEIGHBORHOOD <- tibble(PATTERN = c("NO LEIT", "CORES", "ALEXA", "ALTO DA GL", "ELA BRASIL", "DAS ARTES", "DOS POEMAS", "BONNE", "VILLAGE", "CAMPING", "CARPE", "CENTRO", "ADE ALTA",
                                   "ADE JARDIM", "RIVA", "ECOVIL", "FLORAIS", "NTE FELIZ", "EN VIL", "AMÉRICA|AMERICA", "ARAGUAIA", "ATENAS", "AZALÉIAS|AZALEIAS", "BARCELONA", 
                                   "BELLA SUÍÇA|LA SUIÇA", "BELO HORI", "BELVEDER", "BOA ESPERAN", "DIM BOT", "BOUGA", "CALIF", "CARAND", "CARIBE", "DIM CELESTE", "DIM COPACA", 
                                   "CURITIBA", "ACÁCIAS|ACACIAS", "M DAS ITAÚBAS|M DAS ITAUBAS", "NAÇÕES|NACOES|NAÇOES", "OLIVEIRAS", "ORQUÍDEAS|ORQUIDEAS", "PALMEIRAS", "PRIMAVERAS", 
                                   "ROSAS", "VIOLETAS", "CRAVOS", "DOS IPÊS|DOS IPES", "DUBAI", "EUROPA", "FLOREN", "GRAMADO", "IBIRAP", "IMPERIAL", "IPIRANGA", "IPORA|IPORÃ", 
                                   "ITÁLIA|ITALIA", "ITAPU", "JACARAND", "LONDRINA", "VINDILINA", "MARING|PLATTINUM", "MILÃO|MILAO", "MONET", "MONTE CARLO", "MORUMBI", "NÁPOLES|NAPOLES", 
                                   "NOVO ESTADO", "NOVO HORIZO", "ORIENTE", "PARAÍSO|PARAISO", "PAULISTA", "NA LONDRES", "PÉROLA|PEROLA", "PIENZA", "PORTINARI", "PORTO RICO", "ROMA", 
                                   "SAFIRA", "CECÍLIA|CECILIA", "MÔNICA|MONICA", "SÃO PAULO|SAO PAULO", "TARUMÂS|TARUMAS", "TERRA RICA", "TOLEDO", "TROPICAL", "TULIPAS", "UIRAPUR", 
                                   "UMUARAM", "VENEZA", "VIENA", "RÉGIA|REGIA", "NO JESUS", "MONDRIAN", "APARECIDA", "FÁTIMA|FATIMA", "ARARAS", "DO LAGO", "PLATINI", "PORTAL DA MATA", 
                                   "SERVIDOR", "PAMPULHA", "PÁSSAROS|PASSAROS", "NTO SUÍÇO|NTO SUI", "ERVA CELEST", "BELA MORADA", "BOURBON", "BRASÍLIA|BRASILIA", "BURITIS", 
                                   "CANADÁ|CANADA", "CANARINHO", "DELTA", "DEVILLE|DEVILE", "FLAMBOYA", "IGUATEMI", "IPANEMA", "JARAGU", "JEQUITIB", "MARIP", "MORIÁ|MORIA", "NOVO JARDIM", 
                                   "PANAMB", "PARIS", "SABRINA", "CATARINA", "FRANCISCO", "LOURENÇO|LOURENCO", "MARTINI", "LA VERDE", "VILA RICA", "ERA SUÍÇA|ERA SUIÇA", "SANTORINI", 
                                   "CRISTÓVÃO|CRISTOVAO|CRISTOVÃO|CRISTÓVAO", "DE MATOS", "TOR COMERC", "TOR INDUST", "TOR RESIDEN", "SONHALTO", "TOSCANA"), 
                       CORRECT = c("ADRIANO LEITÃO", "ALAMEDA DAS CORES", "ALEXANDRIA", "ALTO DA GLÓRIA", "AQUARELA BRASIL", "AQUARELA DAS ARTES", "AQUARELA DOS POEMAS", "BONNE VIE", 
                                   "BOSQUE VILLAGE", "CAMPING CLUB", "CARPE DIEM", "CENTRO", "CIDADE ALTA", "CIDADE JARDIM", "DAURY RIVA", "ECOVILEE RESIDENCE CLUB", 
                                   "FLORAIS DA AMAZÔNIA", "GENTE FELIZ", "GREEN VILLE", "JARDIM AMÉRICA", "JARDIM ARAGUAIA", "JARDIM ATENAS", "JARDIM DAS AZALÉIAS", 
                                   "JARDIM BARCELONA", "JARDIM BELLA SUÍÇA", "JARDIM BELO HORIZONTE", "JARDIM BELVEDERE", "JARDIM BOA ESPERANÇA", "JARDIM BOTÂNICO", 
                                   "JARDIM BOUGAINVILLE", "JARDIM CALIFÓRNIA", "JARDIM CARANDÁ", "JARDIM CARIBE", "JARDIM CELESTE", "JARDIM COPACABANA", "JARDIM CURITIBA", 
                                   "JARDIM DAS ACÁCIAS", "JARDIM DAS ITAÚBAS", "JARDIM DAS NAÇÕES", "JARDIM DAS OLIVEIRAS", "JARDIM DAS ORQUÍDEAS", "JARDIM DAS PALMEIRAS", 
                                   "JARDIM DAS PRIMAVERAS", "JARDIM DAS ROSAS", "JARDIM DAS VIOLETAS", "JARDIM DOS CRAVOS", "JARDIM DOS IPÊS", "JARDIM DUBAI", "JARDIM EUROPA", 
                                   "JARDIM FLORENÇA", "JARDIM GRAMADO", "JARDIM IBIRAPUERA", "JARDIM IMPERIAL", "JARDIM IPIRANGA", "JARDIM IPORÂ", "JARDIM ITÁLIA", 
                                   "JARDIM ITAPUÂ", "JARDIM JACARANDÁS", "JARDIM LONDRINA", "JARDIM MARIA VINDILINA", "JARDIM MARINGÁ", "JARDIM MILÃO", "JARDIM MONET", 
                                   "JARDIM MONTE CLARO", "JARDIM MORUMBI", "JARDIM NÁPOLES", "JARDIM NOVO ESTADO", "JARDIM NOVO HORIZONTE", "JARDIM ORIENTE", "JARDIM PARAÍSO", 
                                   "JARDIM PAULISTA", "JARDIM PEQUENA LONDRES", "JARDIM PÉROLA", "JARDIM PIENZA", "JARDIM PORTINARI", "JARDIM PORTO RICO", "JARDIM ROMA", 
                                   "JARDIM SAFIRA", "JARDIM SANTA CECÍLIA", "JARDIM SANTA MÔNICA", "JARDIM SÃO PAULO", "JARDIM TARUMÂS", "JARDIM TERRA RICA", "JARDIM TOLEDO", 
                                   "JARDIM TROPICAL", "JARDIM TULIPAS", "JARDIM UIRAPURU", "JARDIM UMUARAMA", "JARDIM VENEZA", "JARDIM VIENA", "JARDIM VITÓRIA RÉGIA", 
                                   "MENINO JESUS", "MONDRIAN", "NOSSA SENHORA APARECIDA", "NOSSA SENHORA DE FÁTIMA", "PARQUE DAS ARARAS", "PARQUE DO LAGO", "PLATINI URBAN PARK", 
                                   "PORTAL DA MATA", "PORTAL DO SERVIDOR", "QUINTA DA PAMPULHA", "RECANTO DOS PÁSSAROS", "RECANTO SUÍÇO", "RESERVA CELESTE", 
                                   "RESIDENCIAL BELA MORADA", "RESIDENCIAL BOURBON", "RESIDENCIAL BRASÍLIA", "RESIDENCIAL BURITIS", "RESIDENCIAL CANADÁ", "RESIDENCIAL CANARINHO",
                                   "RESIDENCIAL DELTA", "RESIDENCIAL DEVILLE", "RESIDENCIAL FLAMBOYANTS", "RESIDENCIAL IGUATEMI", "RESIDENCIAL IPANEMA", "RESIDENCIAL JARAGUÁ", 
                                   "RESIDENCIAL JEQUITIBÁS", "RESIDENCIAL MARIPÁ", "RESIDENCIAL MORIÁ", "RESIDENCIAL NOVO JARDIM", "RESIDENCIAL PANAMBY", 
                                   "RESIDENCIAL PARIS", "RESIDENCIAL SABRINA", "RESIDENCIAL SANTA CATARINA", "RESIDENCIAL SÃO FRANCISCO", "RESIDENCIAL SÃO LOURENÇO", 
                                   "RESIDENCIAL SAN MARTINI", "RESIDENCIAL VILLA VERDE", "RESIDENCIAL VILA RICA", "RIVIERA SUÍÇA", "SANTORINI RESIDENCE", "SÃO CRISTÓVÃO", 
                                   "SEBASTIÃO DE MATOS", "SETOR COMERCIAL", "SETOR INDUSTRIAL", "SETOR RESIDENCIAL", "SONHALTO VIDEIRA", "VILLA TOSCANA"))


REAL.ESTATE <- crossing(REAL.ESTATE.BASE, NEIGHBORHOOD) %>%
    transform( . ,
               NBHD2 = if_else(str_detect(NBHD, regex(PATTERN, ignore_case = T)), CORRECT, NBHD)) %>%
    filter( . , 
            NBHD2 == CORRECT) %>%
    crossing( . ,
              PROPERTY) %>%
    transform( . , 
               PROPTY2 = if_else(str_detect(PROPTY, regex(PAT_PROP, ignore_case = T)), COR_PROP, PROPTY)) %>%
    filter( . , 
            PROPTY2 == COR_PROP) %>%
    select( . , 
            DATE, CODE, PROPTY2, CTGY, NBHD2, SIZE, PRICE, DORMS, SUITE, BANHE, VAGAS) %>%
    as_tibble() %>%
    rename( . , 
            PROPTY = PROPTY2, NBHD = NBHD2) %>%
    transform( . , 
               SIZE = round(SIZE, 2), PRICE = round(PRICE, 2))




# Saving Data Set ---------------------------------------------------------

DataSetName <- paste0("REAL_ESTATE_SNP_", CUR_DATE, ".csv")

write_csv(REAL.ESTATE, DataSetName)

drive_upload(DataSetName, type = "spreadsheet", path = drive_get(as_id("1MAUtzMDEViT-jDJ9yrGM71HHcNL-O2yf")))

rm(NEIGHBORHOOD, PROPERTY, REAL.ESTATE.BASE)



