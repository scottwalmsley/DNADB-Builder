creatMZVaultDB <- function(dbname){

if(file.exists(dbname)){
   file.remove(dbname)
}

library(RSQLite)


SQL = 'CREATE TABLE 
       SpectrumTable 
       ([SpectrumId] INTEGER PRIMARY KEY, 
        [CompoundId] INTEGER REFERENCES 
        [CompoundTable] ([CompoundId]), 
        [mzCloudURL] TEXT, 
        [ScanFilter] TEXT, 
        [RetentionTime] DOUBLE, 
        [ScanNumber] INTEGER, 
        [PrecursorMass] DOUBLE, 
        [NeutralMass] DOUBLE,
        [CollisionEnergy] TEXT, 
        [Polarity] TEXT, 
        [FragmentationMode] TEXT,
        [IonizationMode] TEXT, 
        [MassAnalyzer] TEXT, 
        [InstrumentName] TEXT, 
        [InstrumentOperator] TEXT, 
        [RawFileURL] TEXT, 
        [blobMass] BLOB, 
        [blobIntensity] BLOB, 
        [blobAccuracy] BLOB, 
        [blobResolution] BLOB, 
        [blobFlags] BLOB, 
        [blobTopPeaks] BLOB, 
        [Version] INTEGER,
        [CreationDate] TEXT, 
        [Curator] TEXT, [CurationType], 
        [PrecursorIonType] TEXT, 
        [Accession] TEXT)'



con <- dbConnect(RSQLite::SQLite(), dbname)
dbExecute(con,SQL, overwrite= T)
dbDisconnect(con)




SQL = 'CREATE TABLE 
        CompoundTable
       ([CompoundId] INTEGER PRIMARY KEY, 
        [Formula] TEXT, 
        [Name] TEXT, 
        [Synonyms] BLOB_TEXT, 
        [Tag] TEXT,  
        [Sequence] TEXT, 
        [CASId] TEXT, 
        [ChemSpiderId] TEXT,  
        [HMDBId] TEXT, 
        [KEGGId] TEXT, 
        [PubChemId] TEXT,  
        [Structure] BLOB_TEXT, 
        [mzCloudId] INTEGER, 
        [CompoundClass] TEXT)'

con <- dbConnect(RSQLite::SQLite(), dbname)
dbExecute(con,SQL, overwrite= T)
dbDisconnect(con)

SQL = 'CREATE TABLE 
         HeaderTable
         (version INTEGER NOT NULL DEFAULT 0,  
          [CreationDate] TEXT,
          [LastModifiedDate] TEXT,
          [Description] TEXT,
          [Company] TEXT)'

con <- dbConnect(RSQLite::SQLite(),dbname)
dbExecute(con,SQL, overwrite= T)
dbDisconnect(con)



}