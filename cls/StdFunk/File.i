
/*------------------------------------------------------------------------
    File        : File.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Mar 11 10:45:44 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFile NO-UNDO
    FIELD iFileId          AS INTEGER
    FIELD FileName         AS CHARACTER 
    FIELD FullName         AS CHARACTER 
    FIELD FileDirectory    AS CHARACTER
    FIELD FileSize         AS INTEGER
    FIELD Creationtime     AS DATETIME 
    FIELD LastAccessTime   AS DATETIME 
    FIELD LastWriteTime    AS DATETIME 
    FIELD Extension        AS CHARACTER 
    FIELD DirectoryName    AS CHARACTER 
    FIELD IsReadOnly       AS LOGICAL 
    INDEX idxFile IS PRIMARY UNIQUE FullName
    .

DEFINE DATASET dsFile FOR ttFile.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
