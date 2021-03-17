 
 /*------------------------------------------------------------------------
    File        : Directory
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Geir Otto (goo@thinc.no)
    Created     : Mon Feb 26 12:19:26 CET 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/*BLOCK-LEVEL ON ERROR UNDO, THROW.*/

CLASS cls.Stdfunk.Directory:
  {cls\Stdfunk\Directory.i}  
    
  DEF STREAM inStr. 
  DEF PRIVATE VAR ix AS INT NO-UNDO.

  DEF PROPERTY DirectoryName       AS CHAR NO-UNDO GET. SET.
  DEF PROPERTY DirectoryPath       AS CHAR NO-UNDO GET. SET.
  DEF PROPERTY DirectoryRecursive  AS LOG  NO-UNDO GET. SET.
  
  DEF PROPERTY DirectoryCount AS INT  NO-UNDO 
  GET():
    RETURN ix. 
  END GET. PRIVATE SET.
  DEF PROPERTY DirectoryExist AS LOG NO-UNDO GET. SET.
  DEF PROPERTY thDirectory    AS HANDLE NO-UNDO 
  GET(): 
    RETURN TEMP-TABLE ttDirectory:HANDLE.
  END GET. PRIVATE SET.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC Directory (  ):
    SUPER ().
  END CONSTRUCTOR.


  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC Directory (INPUT ipDirectory AS CHAR):
    SUPER ().
    
    DirectoryName = ipDirectory.
    DirectoryExist = System.IO.Directory:Exists(DirectoryName).
    
    IF NOT DirectoryExist THEN RETURN.
    
    readDirectory(DirectoryName,?).
    
  END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID emptyTempTable(  ):

        EMPTY TEMP-TABLE ttDirectory.
        
        RETURN.

    END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL readDirectory(INPUT ipDirectory AS CHAR, INPUT ipParentId AS INT):
    DEFINE VARIABLE cDirectories AS "System.String[]" NO-UNDO.
    DEFINE VARIABLE cDirectory AS CHAR           NO-UNDO.
    DEFINE VARIABLE ixx        AS INTEGER           NO-UNDO.
    DEF VAR bExist AS LOG NO-UNDO.
    DEF VAR ParentId AS INT NO-UNDO.
    
    ix = ix + 1.
    bExist = System.IO.Directory:Exists(ipDirectory).
    IF NOT bExist THEN RETURN ?.

    CREATE ttDirectory.
    ASSIGN 
      ttDirectory.iDirectoryId     = ix
      ttDirectory.DirectoryName    = SUBSTRING(ipDirectory,R-INDEX(ipDirectory,'\') + 1)
      ttDirectory.DirectoryPath    = SUBSTRING(ipDirectory,1,R-INDEX(ipDirectory,'\'))
      ttDirectory.iParentDirectory = ipParentId 
      ParentId                     = ix
      .
    IF DirectoryRecursive THEN DO:  
      ASSIGN 
        cDirectories = System.IO.Directory:GetDirectories(ipDirectory)
        .
      cDirectory = cDirectories:GetValue(ixx) NO-ERROR.
      IF NOT System.IO.Directory:Exists(cDirectory) OR cDirectories:Length = 0 THEN RETURN FALSE. 
          
      DO ixx = 0 TO cDirectories:Length - 1:
        cDirectory = cDirectories:GetValue(ixx) NO-ERROR.
        readDirectory(cDirectory,ParentId).  
      END. 
      IF ix GT 5000 THEN RETURN FALSE. /*OM det skulle være mer enn 5000 kataloger, så avbryt*/
    END.
  END METHOD.  

END CLASS.