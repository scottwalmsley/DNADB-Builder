addSpectraToMZVaultDB <- function(dbname){
             
    dbname = 'test.db'
   con <- dbConnect(RSQLite::SQLite(), dbname)
   SQL = paste('INSERT INTO CompoundTable
            (Formula) 	
			VALUES (\'',form,'\')')
 con <- dbConnect(RSQLite::SQLite(), dbname)
dbExecute(con,SQL, overwrite= T)
dbDisconnect(con)

       

consensusSpectra$Formula


}