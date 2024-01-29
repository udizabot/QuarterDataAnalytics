###########################################################################
# Data Analysis -----------------------------------------------------------
###########################################################################

# Libraries ---------------------------------------------------------------
pacman::p_load(tidyverse, googledrive, googlesheets4, SciViews, zoo, IndexNumber)

# Getting Data From Google Drive ------------------------------------------
sheets <- drive_find(type = "spreadsheet", pattern = "REAL_ESTATE")  

RealEstate <- map_df(sheets$id, ~read_sheet(.)) %>% as_tibble()

# Organize DataSet --------------------------------------------------------
DataBase <- 
    RealEstate %>% arrange( . , DATE) %>%
    transform( . , 
               SIZE = unlist(RealEstate$SIZE) %>% parse_number() ) %>%
    filter( . ,
            PROPTY == "APARTAMENTO" | PROPTY == "CASA" | PROPTY == "TERRENO" | PROPTY == "SALA COMERCIAL") %>% as_tibble()

write_csv(DataBase, "Real2Estate2023.csv")

WEIGHT <- readRDS("WEIGHT.rds") %>% 
    as_tibble() %>% 
    mutate( . ,
            NBHD = str_to_upper(NBHD))




###########################################################################
# IVIS-T ------------------------------------------------------------------

IVIST1 <- filter(DataBase, 
                 CTGY == "VENDA" & PROPTY == "TERRENO") %>%
    filter( . , 
            SIZE >= 300 & SIZE <= 1000, DORMS == 0) %>%
    transform( . ,
               VMQ = round(PRICE / SIZE, 2), 
               YEAR = year(DATE),
               MONTH = month(DATE)) %>%
    filter( . , 
            VMQ >= 100 & VMQ <= 5000) %>% 
    group_by(YEAR, MONTH, NBHD) %>%
    filter( . , 
            VMQ > quantile(VMQ, probs = c(0.025)), 
            VMQ < quantile(VMQ, probs = c(0.975))) %>%
    mutate( . ,
            AVGP = mean(VMQ), 
            MEDP = median(VMQ)) %>%
    distinct( . , 
              YEAR, MONTH, NBHD, .keep_all = T)

IVIST2 <- left_join(IVIST1, WEIGHT) %>%
    select( . , 
            YEAR, MONTH, NBHD, AVGP, MEDP, TWGHT2) %>%
    group_by(NBHD) %>%
    mutate( . ,
            PRICE_T = rollmean(MEDP, 3, fill = NA, align = "right")) %>%
    mutate( . ,
            YIELD_T = log(PRICE_T) - log(lag(PRICE_T)))


IVIST3 <- IVIST2 %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0))) %>%
    transform( . , 
               NUMBER = PRICE_T*TWGHT2) %>%
    group_by(YEAR, MONTH) %>%
    mutate( . , 
            NUMBER_T = sum(NUMBER)) %>%
    distinct( . , 
              YEAR, MONTH, NUMBER_T)

IVIST4 <- IVIST3 %>% 
    transform( . , 
               IVIST = (NUMBER_T / lag(NUMBER_T))*100) 




###########################################################################
# IVIS-R ------------------------------------------------------------------

IVISR1 <- filter(DataBase, 
                 CTGY == "VENDA" & (PROPTY == "CASA" | PROPTY == "APARTAMENTO")) %>%
    filter( . , !(is.na(SIZE)),
            SIZE >= 50 & SIZE <= 500, DORMS != 0) %>%
    transform( . ,
               VMQ = round(PRICE / SIZE, 2), 
               YEAR = year(DATE),
               MONTH = month(DATE)) %>%
    filter( . , 
            VMQ >= 100 & VMQ <= 20000) %>% 
    group_by(YEAR, MONTH, NBHD) %>%
    filter( . , 
            VMQ > quantile(VMQ, probs = c(0.025)), 
            VMQ < quantile(VMQ, probs = c(0.975))) %>%
    mutate( . ,
            AVGP = mean(VMQ),
            MEDP = median(VMQ)) %>%
    distinct( . , 
              YEAR, MONTH, NBHD, .keep_all = T)

IVISR2 <- left_join(IVISR1, WEIGHT) %>%
    select( . , 
            YEAR, MONTH, NBHD, AVGP, MEDP, RWGHT2) %>%
    group_by(NBHD) %>%
    mutate( . ,
            PRICE_R = rollmean(MEDP, 3, fill = NA, align = "right"))

IVISR3 <- IVISR2 %>%
    mutate( . , 
            across(where(is.numeric), ~replace_na(.x, 0))) %>% 
    transform( . , 
               NUMBER = PRICE_R*RWGHT2) %>%
    group_by(YEAR, MONTH) %>%
    mutate( . , 
            NUMBER_R = sum(NUMBER)) %>%
    distinct( . , 
              YEAR, MONTH, NUMBER_R)



######################################################################################################################################################




