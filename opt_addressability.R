options(scipen = 999)
#source("sprklyRSpark.R")
library(readr)
library(ggplot2)
library(tictoc)


load_R_Native <- function()
{
  tic()
  print("Reading in export")
  raw_df <- read.table("GWCM_FPDS_28_AUGUST_EXTRACT.tsv", header = TRUE, comment.char = "", sep = "\t", quote = "", fill = TRUE, stringsAsFactors = FALSE)
  toc()
  print("Creating add_key for all transactions")
  raw_df$women_owned_flag <- replace(as.character(raw_df$women_owned_flag), raw_df$women_owned_flag == "", "NO")
  raw_df$veteran_owned_flag <- replace(as.character(raw_df$veteran_owned_flag), raw_df$veteran_owned_flag == "", "NO")
  raw_df$co_bus_size_determination_code <- replace(as.character(raw_df$co_bus_size_determination_code), raw_df$co_bus_size_determination_code == "", "O")
  raw_df$add_key <- paste0(raw_df$product_or_service_code,"_",raw_df$naics_code,"_", raw_df$co_bus_size_determination_code, "_", raw_df$women_owned_flag, "_", raw_df$veteran_owned_flag)
  #filter by date range to only have FY16
  tic()
  print("subsetting training transactions")
  training_transactions <<- raw_df %>% filter(as.Date(date_signed) >= as.Date("2014-10-01") & as.Date(date_signed) <= as.Date("2015-09-30"))
  toc()
  tic()
  print("subsetting testing transactions")
  testing_transactions <- raw_df %>% filter(as.Date(date_signed) >= as.Date("2015-10-01") & as.Date(date_signed) <= as.Date("2016-09-30"))
  toc()
}


load_spark_csv <- function(sc)
{##future dev note: file loading phase can be speeded up by using spark csv reader
  tic()
  print("Reading in export")
  raw_df <- spark_read_csv(sc, name = "sprkdf", "GWCM_FPDS_28_AUGUST_EXTRACT.tsv",delimiter = "\t", header = TRUE, overwrite = TRUE)
  toc( )
  print("Creating add_key for all transactions")
  raw_df$women_owned_flag <- replace(as.character(raw_df$women_owned_flag), raw_df$women_owned_flag == "", "NO")
  raw_df$veteran_owned_flag <- replace(as.character(raw_df$veteran_owned_flag), raw_df$veteran_owned_flag == "", "NO")
  raw_df$co_bus_size_determination_code <- replace(as.character(raw_df$co_bus_size_determination_code), raw_df$co_bus_size_determination_code == "", "O")
  raw_df$add_key <- paste0(raw_df$product_or_service_code,"_",raw_df$naics_code,"_", raw_df$co_bus_size_determination_code, "_", raw_df$women_owned_flag, "_", raw_df$veteran_owned_flag)
  #filter by date range to only have FY16
  tic()
  print("subsetting training transactions")
  training_transactions <<- raw_df %>% filter(as.Date(date_signed) >= as.Date("2014-10-01") & as.Date(date_signed) <= as.Date("2015-09-30"))
  toc()
  tic()
  print("subsetting testing transactions")
  testing_transactions <<- raw_df %>% filter(as.Date(date_signed) >= as.Date("2015-10-01") & as.Date(date_signed) <= as.Date("2016-09-30"))
  toc()
}

load_spark_parquet <- function()
{
  tic()
  print("Reading in export")
  raw_df <- spark_read_parquet(sc, name="raw_df", path = "August28FPDS.prq/")
  #filter by date range to only have FY16
  toc()
  print("Creating add_key for all transactions")
  raw_df$women_owned_flag <- replace(as.character(raw_df$women_owned_flag), raw_df$women_owned_flag == "", "NO")
  raw_df$veteran_owned_flag <- replace(as.character(raw_df$veteran_owned_flag), raw_df$veteran_owned_flag == "", "NO")
  raw_df$co_bus_size_determination_code <- replace(as.character(raw_df$co_bus_size_determination_code), raw_df$co_bus_size_determination_code == "", "O")
  raw_df$add_key <- paste0(raw_df$product_or_service_code,"_",raw_df$naics_code,"_", raw_df$co_bus_size_determination_code, "_", raw_df$women_owned_flag, "_", raw_df$veteran_owned_flag)
  tic()
  print("subsetting training transactions")
  training_transactions <<- raw_df %>% filter(as.Date(date_signed) >= as.Date("2014-10-01") & as.Date(date_signed) <= as.Date("2015-09-30"))
  toc()
  tic()
  print("subsetting testing transactions")
  testing_transactions <<- raw_df %>% filter(as.Date(date_signed) >= as.Date("2015-10-01") & as.Date(date_signed) <= as.Date("2016-09-30"))
  toc()
}


opt_get_contract_totals <- function(contract_label, transaction_df)
{
  contract_total_obligations <- transaction_df %>% 
    filter(contract_name == contract_label) %>% 
    select(dollars_obligated) %>% collect() %>% 
    sum()
  
  contract_total_obligations
}


## you will need to add the socio-economic logical variables here to produce the required add_key combination
## the case below only covers F,F,F,F
validated_generate_addressability_matrix_df <- function(contract_label, sb = "O", wo="NO", vo="NO")
{
  print("Generating addressability matrix from training archive")
  tic()
  #Subset transaction archive to only the columns needed.

    addressability_matrix_df <-  testing_transactions %>% filter(contract_name == contract_label) %>% 
      distinct(contract_name, product_or_service_code, naics_code) %>% 
      mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_", sb, "_", wo, "_", vo)) %>% select(add_key) %>% collect()

  #capture of the psc_naics keys
  add_key_vector <- addressability_matrix_df %>% .$add_key
  
  #summarizes total amount of obligations that map back through the psc_naics combinations. Should be equal to total contract obligations
  #optional: Uncomment to verify sums
  #addressability_summary <<- addressable_training_df %>% filter(psc_naics %in% psc_naics_combo_vector) %>%
  #  group_by(contract_name) %>%
  #    summarise(obligations = sum(dollars_obligated)) %>% 
  #    arrange(contract_name) %>% 
  #  collect
  toc()
  #matrix_df
  add_key_vector
}

validated_generate_addressable_obs<-function(addressability_matrix, transaction_df)
{
  testing_df <- transaction_df %>% 
    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, dollars_obligated)%>%
    mutate(psc_naics = paste0(product_or_service_code,"_",naics_code))
  
  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  addressability_obl
  #  group_by(contract_name) %>%
  #    summarise(obligations = sum(dollars_obligated)) 
}

opt_gen_addressable_obs <- function(addressability_matrix, transaction_df)
{
  
  #sb
  #sb, wo
  #sb, wo, vo
  #sb,wo, 8a
  #sb, wo, vo, 8a
  #sb, vo, 8a
  #sb, vo
  #sb, 8a
  
  #wo
  #vo
  
  addressability_obl <- transaction_df %>% filter(add_key %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  addressability_obl
  #default_case all variables == false -> provide only psc_naics combos
 # if(sb==FALSE & wo==FALSE & vo==FALSE & eighta==FALSE)
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(psc_naics = paste0(product_or_service_code,"_",naics_code))
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  
  #	sb
  #else if(sb == TRUE & wo==FALSE & vo==FALSE & eighta==FALSE) 
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  #	sb, wo
  #else if(sb == TRUE & wo==TRUE& vo==FALSE & eighta==FALSE)   
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  #	sb, wo, vo
  #else if(sb == TRUE & wo==TRUE & vo ==TRUE & eighta==FALSE )    
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  #sb, wo, 8a
  #else if(sb == TRUE & wo==TRUE & vo ==FALSE & eighta==TRUE ) 
  #{
  
  #testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  #sb, wo, vo, 8a
  #else if(sb == TRUE & wo==TRUE & vo ==TRUE & eighta==TRUE )
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  ##sb, vo, 8a
  #else if(sb == TRUE & wo==FALSE & vo ==TRUE & eighta==TRUE )    
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  ##sb, vo
  #else if(sb == TRUE & wo==FALSE & vo ==TRUE & eighta==FALSE )  
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  ##sb, 8a
  #else if(sb == TRUE & wo==FALSE & vo ==FALSE & eighta==TRUE )       
  #{
  #  testing_df <- transaction_df %>% 
  #    select(contract_name, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag, dollars_obligated)%>%
  #    mutate(add_key = paste0(product_or_service_code,"_",naics_code,"_",co_bus_size_determination_code)) 
  #  
  #  addressability_obl <- testing_df %>% filter(psc_naics %in% addressability_matrix) %>% select(dollars_obligated) %>% collect() %>% sum()
  #  addressability_obl
  #} 
  ##add wo
  ##add vo
  ##add 8a
  #else print(paste0("The variable combination sb==", sb, " wo==", wo, " vo==", vo, " eighta==", eighta, " is not covered"))
   
}

opt_produce_addressability_for_bic <- function(bic_name, addressability_matrix, sb = FALSE, wo=FALSE, vo=FALSE, eighta=FALSE)
{
  
  #sb
  #sb, wo
  #sb, wo, vo
  #sb,wo, 8a
  #sb, wo, vo, 8a
  #sb, vo, 8a
  #sb, vo
  #sb, 8a
  
  #default_case all variables == false -> provide only psc_naics combos
  if(sb==FALSE & wo==FALSE & vo==FALSE & eighta==FALSE)
    addressable_obligations <- addressability_matrix %>% 
      filter(official_bic_contract == bic_name ) %>% 
      group_by(official_bic_contract,product_or_service_code, naics_code) %>%
      summarise(obligations = sum(obligations))   
  
  #	sb
  else if(sb == TRUE & wo==FALSE & vo==FALSE & eighta==FALSE) 
    addressable_obligations <- addressability_matrix %>% 
      filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S" ) %>% 
      group_by(official_bic_contract,product_or_service_code, naics_code, co_bus_size_determination_code) %>%
      summarise(obligations = sum(obligations))
  
  #	sb, wo
  else if(sb == TRUE & wo==TRUE& vo==FALSE & eighta==FALSE)   
    addressable_obligations <- addressability_matrix %>% 
      filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S" & women_owned_flag == "YES") %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag) %>%
      summarise(obligations = sum(obligations))
  #	sb, wo, vo
  else if(sb == TRUE & wo==TRUE & vo ==TRUE & eighta==FALSE )    
    addressable_obligations <- addressability_matrix %>% 
      filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S" & women_owned_flag == "YES" & eight_a_flag =="YES") %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, eight_a_flag) %>%
      summarise(obligations = sum(obligations))  
  
  #sb, wo, 8a
  else if(sb == TRUE & wo==TRUE & vo ==FALSE & eighta==TRUE ) 
    addressable_obligations <- addressability_matrix %>% 
      filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S" & women_owned_flag == "YES" & eighta == "YES" ) %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, eighta) %>%
      summarise(obligations = sum(obligations))
  
  #sb, wo, vo, 8a
  else if(sb == TRUE & wo==TRUE & vo ==TRUE & eighta==TRUE )
    addressable_obligations <- addressability_matrix %>% 
      filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S"  & women_owned_flag == "YES" & veteran_owned_flag == "YES" & eight_a_flag =="YES" ) %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, women_owned_flag, veteran_owned_flag, eight_a_flag) %>%
      summarise(obligations = sum(obligations))
  #sb, vo, 8a
  else if(sb == TRUE & wo==FALSE & vo ==TRUE & eighta==TRUE )    
    addressable_obligations <- addressability_matrix %>% filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S"  & veteran_owned_flag == "YES", eighta == "YES" ) %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, veteran_owned_flag, eight_a_flag) %>%
      summarise(obligations = sum(obligations))
  
  #sb, vo
  else if(sb == TRUE & wo==FALSE & vo ==TRUE & eighta==FALSE )  
    addressable_obligations <- addressability_matrix %>% filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S"  & veteran_owned_flag =="YES") %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, veteran_owned_flag) %>%
      summarise(obligations = sum(obligations))      
  #sb, 8a
  else if(sb == TRUE & wo==FALSE & vo ==FALSE & eighta==TRUE )       
    addressable_obligations <- addressability_matrix %>% filter(official_bic_contract == bic_name & co_bus_size_determination_code == "S", eighta == "YES"  ) %>% 
      group_by(official_bic_contract, product_or_service_code, naics_code, co_bus_size_determination_code, eight_a_flag) %>%
      summarise(obligations = sum(obligations))     
  else print(paste0("The variable combination sb==", sb, " wo==", wo, " vo==", vo, " eighta==", eighta, " is not covered"))
  addressable_obligations
}
