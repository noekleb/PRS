
/*------------------------------------------------------------------------
    File        : Directory.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Mar 11 10:42:24 CET 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE ttDirectory NO-UNDO
    FIELD iDirectoryId       AS INT
    FIELD DirectoryName      AS CHAR
    FIELD DirectoryPath      AS CHAR
    FIELD iParentDirectoryId AS INT
    INDEX idxDirectory IS PRIMARY UNIQUE DirectoryPath DirectoryName
    .

DEFINE DATASET dsDirectory FOR ttDirectory.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
