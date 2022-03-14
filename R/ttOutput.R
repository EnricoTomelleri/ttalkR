#' @export


ttOutput <- function(site_name) {
  #library(RSQLite)

  #create a database and add our tables.
  filename <- paste0("./ttOutput_", site_name, ".db")
  myDB <- filename
  myConn <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = myDB)
  #dbListTables(myConn)



  #check existing tables
  #add the dataframes we just read in to the database


  if (exists("df_ttBattery") == F) {
    print("Warning! the dataframe <df_ttBattery> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttBattery", df_ttBattery, overwrite = T)
  }

  if (exists("df_ttGranier") == F) {
    print("Warning! the dataframe <df_ttGranier> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttGranier", df_ttGranier, overwrite = T)
  }

  if (exists("df_ttLight") == F) {
    print("Warning! the dataframe <df_ttLight> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttLight", df_ttLight, overwrite = T)
  }

  if (exists("df_ttRH") == F) {
    print("Warning! the dataframe <df_ttRH> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttRH", df_ttRH, overwrite = T)
  }

  if (exists("df_ttStability") == F) {
    print("Warning! the dataframe <df_ttStability> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttStability", df_ttStability, overwrite = T)
  }

  if (exists("df_ttStWC") == F) {
    print("Warning! the dataframe <df_ttStWC> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttStWC", df_ttStWC, overwrite = T)
  }

  if (exists("df_ttTair") == F) {
    print("Warning! the dataframe <df_ttTair> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttTair", df_ttTair, overwrite = T)
  }

  if (exists("df_ttGrowth") == F) {
    print("Warning! the dataframe <df_ttGrowth> is missing.")
  } else{
    RSQLite::dbWriteTable(myConn, "df_ttGrowth", df_ttGrowth, overwrite = T)
  }

  RSQLite::dbDisconnect()

}
