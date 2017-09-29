#Sys.setenv("JAVA_HOME" = "/usr/java")
library(sparklyr)
library(dplyr)

sparkInit <- function()
{
  Sys.setenv("SPARK_MEM" = "30G")
  config <- spark_config()
  config$spark.driver.maxResultSize <- "4G"
  config$spark.driver.memory <- "2G"
  config$spark.executor.memory <- "2G"
  sc <- spark_connect(master = "local", config = config)
  sc
}

sparkLocalRemoteInit <- function()
{
  Sys.setenv("SPARK_HOME" = "/Users/destiny/Documents/development/spark-2.1.1-bin-hadoop2.7") 
  Sys.setenv("SPARK_MEM" = "30G")
  config <- spark_config()
  config$spark.driver.maxResultSize <- "4G"
  config$spark.driver.memory <- "2G"
  config$spark.executor.memory <- "2G"
  sc <- spark_connect(master = "spark://Event-Horizon.local:7077", config = config)
  sc 
}

sparkShutDown <- function(sc)
{
  spark_disconnect_all()
}

loadDfFrmCSV <- function(sc, filename)
{
 ##Spark's csv loader doesnt handle our csv file correctly. Defer to native R csv loader for accurate loading. Use function loadDf()
  sprkdf <- spark_read_csv(sc, name = "sprkdf", paste0("file://",filename),
                           delimiter = ",", header = TRUE, overwrite = TRUE)
  sprkdf
}
loadDfFrmTSV <- function(sc, filename)
{
  ##Spark's csv loader doesnt handle our csv file correctly. Defer to native R csv loader for accurate loading. Use function loadDf()
  sprkdf <- spark_read_csv(sc, name = "sprkdf", paste0(filename),
                           delimiter = "\t", quote = "" , header = TRUE, overwrite = TRUE)
  sprkdf
}

loadDfFrmParquet <- function(sc, filename)
{
  sprkdf <- spark_read_parquet(sc, name = "sprkdf", paste0("file://",filename), memory = TRUE, overwrite = TRUE )
  sprkdf
}

spark_detect_date_range <- function(spark_df)
{
  min_date <- collect(spark_df %>% summarize(min(as.Date(signeddate))))
  max_date <- collect(spark_df %>% summarize(max(as.Date(signeddate))))
  date_range <- c(min_date, max_date)
  #date_range
  print(paste0("Detected date range is ", date_range[1], " to ", date_range[2]))
}

spark_detect_psc_range <- function(spark_df)
{
  pscs <- collect(sparkdf %>% select(prod_or_serv_code) %>% distinct)   
  pscs
}


loadDf <- function(sc)
{
  #  sprkdf <- spark_read_csv(sc, name = "sprkdf", 
  #                           "file:///Users/destiny/Documents/development/Rspark/fy12_2.csv",
  #                           delimiter = ",", header = TRUE, overwrite = TRUE)
  #  sprkdf
  df <- read_csv("Jan16.csv")  
  index <- df$X1
  #index$index <- df$X1
  a_aid_acontid_piid <- data_frame(index, a_aid_acontid_piid = df$a_aid_acontid_piid)
  ag_name <- data_frame(index, ag_name = df$ag_name)
  bureau_name <- data_frame(index, bureau_name = df$bureau_name)
  cd_contactiontype <- data_frame(index, cd_contactiontype = df$cd_contactiontype)
  cd_descofcontreq <- data_frame(index, cd_descofcontreq = df$cd_descofcontreq)
  funding_agency_name <- data_frame(index, funding_agency_name = df$funding_agency_name)
  naics_code <- data_frame(index, naics_code = df$naics_code)
  obligatedamount <- data_frame(index, obligatedamount = df$obligatedamount)
  prod_or_serv_code <- data_frame(index, prod_or_serv_code = df$prod_or_serv_code)
  refidvid_piid <- data_frame(index, refidvid_piid = df$refidvid_piid)
  signeddate <- data_frame(index, signeddate = as.character(df$signeddate))
  vend_contoffbussizedeterm <- data_frame(index, vend_contoffbussizedeterm = df$vend_contoffbussizedeterm)
  vend_dunsnumber <- data_frame(index, vend_dunsnumber = df$vend_dunsnumber)
  
  
  s_a_aid_acontid_piid <- copy_to(sc, a_aid_acontid_piid)
  s_ag_name <- copy_to(sc, ag_name)
  s_bureau_name <- copy_to(sc, bureau_name)
  s_cd_contactiontype <- copy_to(sc, cd_contactiontype)
  s_cd_descofcontreq <- copy_to(sc, cd_descofcontreq)
  s_funding_agency_name <- copy_to(sc, funding_agency_name)
  s_naics_code <- copy_to(sc, naics_code)
  s_obligatedamount <- copy_to(sc, obligatedamount)
  s_prod_or_serv_code <- copy_to(sc, prod_or_serv_code)
  s_refidvid_piid <- copy_to(sc, refidvid_piid)
  s_signeddate <- copy_to(sc, signeddate)
  #s_signeddate <- mutate(signeddate, signeddate = as.Date(signeddate$signeddate))
  s_vend_contoffbussizedeterm <- copy_to(sc, vend_contoffbussizedeterm)
  s_vend_dunsnumber <- copy_to(sc, vend_dunsnumber)
  
  rm(list = c("df","index", "a_aid_acontid_piid", "ag_name", "bureau_name", "cd_contactiontype", 
              "cd_descofcontreq", "funding_agency_name", "naics_code", "obligatedamount", "prod_or_serv_code", 
              "refidvid_piid", "signeddate", "vend_contoffbussizedeterm", "vend_dunsnumber" ))
  
  result_set <- s_a_aid_acontid_piid %>% 
    left_join(s_obligatedamount) %>%  
    left_join(s_ag_name)%>%  
    left_join(s_bureau_name)%>%  
    left_join(s_cd_contactiontype)%>%  
    left_join(s_cd_descofcontreq)%>%  
    left_join(s_funding_agency_name)%>%  
    left_join(s_naics_code)%>%  
    left_join(s_obligatedamount)%>%  
    left_join(s_prod_or_serv_code)%>%  
    left_join(s_refidvid_piid)%>%  
    left_join(s_signeddate)%>%  
    left_join(s_vend_contoffbussizedeterm)%>%  
    left_join(s_vend_dunsnumber)
}





obsolete_prep_fiscal_years <- function(sparkdf)
{
  print("Creating fiscal year FY12 df")
  fy2012 <<- spark_get_date_range(sparkdf, "2011-10-01", "2012-09-30")  
  spark_detect_date_range(fy2012)
  ##2013
  print("Creating fiscal year FY13 df")
  fy2013 <<- spark_get_date_range(sparkdf, "2012-10-01", "2013-09-30") 
  spark_detect_date_range(fy2013)
  ##2014
  print("Creating fiscal year FY14 df")
  fy2014 <<- spark_get_date_range(sparkdf, "2013-10-01", "2014-09-30")   
  spark_detect_date_range(fy2014)
  ##2015
  print("Creating fiscal year FY15 df")
  fy2015 <<- spark_get_date_range(sparkdf, "2014-10-01", "2015-09-30")   
  spark_detect_date_range(fy2015)
  ##2016
  print("Creating fiscal year FY16 df")
  fy2016 <<- spark_get_date_range(sparkdf, "2015-10-01", "2016-09-30")   
  spark_detect_date_range(fy2016)
  ##2017  
  print("Creating fiscal year FY17 df")
  fy2017 <<- spark_get_date_range(sparkdf, "2016-10-01", "2017-09-30")
  spark_detect_date_range(fy2017)
}

Obsolete_spark_get_date_range <- function(spark_df, start_date, end_date)
{
  print(paste0("Assigning date range ", start_date, " to ", end_date))
  resultset <- filter(spark_df, as.Date(signeddate) >= as.Date(start_date) & as.Date(signeddate) <= as.Date(end_date))
  resultset
}

Obsolete_spark_subset_via_psc <- function(spark_df, psc_list)
{
  print(paste0("Retrieving transaction from pscs ", psc_list))
  resultset <- filter(spark_df, prod_or_serv_code %in% psc_list)
  resultset
}