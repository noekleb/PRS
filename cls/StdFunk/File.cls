 
/*------------------------------------------------------------------------
   File        : File
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : goo_000
   Created     : Tue Feb 27 13:12:46 CET 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.IO.Compression.FileSystem.*.

/*BLOCK-LEVEL ON ERROR UNDO, THROW.*/

CLASS cls.StdFunk.File:
    {cls\Stdfunk\File.i}  
    
  DEFINE VARIABLE seqFile AS INTEGER NO-UNDO.

  DEFINE PROPERTY thFile    AS HANDLE NO-UNDO 
    GET(): 
      RETURN TEMP-TABLE ttFile:HANDLE.
    END GET. 
    PRIVATE SET.
  DEFINE PROPERTY ZipWhenSizeInMb AS DECIMAL NO-UNDO GET. SET.
     
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
    
  CONSTRUCTOR PUBLIC File (  ):
    SUPER ().
    
    ZipWhenSizeInMb = 50.
    CATCH e AS Progress.Lang.Error:

    END CATCH.

  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID addFile(INPUT ipFileDirectory AS CHARACTER, INPUT ipFilename AS CHARACTER):
    
    DEFINE VARIABLE oFileInfo AS CLASS System.IO.FileInfo.
    
    oFileInfo = NEW System.IO.FileInfo(ipFilename).
    
    IF oFileInfo:Exists THEN 
    DO:
      CREATE ttFile.
      ASSIGN 
        seqFile                 = seqFile + 1
        ttFile.iFileId          = seqFile
        ttFile.FileName         = oFileInfo:Name
        ttFile.FullName         = oFileInfo:FullName
        ttFile.FileSize         = oFileInfo:Length
        ttFile.FileDirectory    = ipFileDirectory
        ttFile.Creationtime     = oFileInfo:Creationtime 
        ttFile.LastAccessTime   = oFileInfo:LastAccessTime 
        ttFile.LastWriteTime    = oFileInfo:LastWriteTime 
        ttFile.Extension        = oFileInfo:Extension
        ttFile.DirectoryName    = oFileInfo:DirectoryName
        ttFile.IsReadOnly       = oFileInfo:IsReadOnly
        . 
    END.
  END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID emptyTempTable(  ):
        
        EMPTY TEMP-TABLE ttFile. 
        
        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    METHOD PUBLIC VOID getDataSetFile( OUTPUT DATASET dsFile ):
        
        RETURN.

    END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID readDirectory(INPUT ipDirectory AS CHARACTER, INPUT ipSearchPattern AS CHARACTER ):
    DEFINE VARIABLE bOk   AS LOG               NO-UNDO.
    DEFINE VARIABLE Files AS "System.String[]" NO-UNDO.
    DEFINE VARIABLE File  AS CHARACTER         NO-UNDO.
    DEFINE VARIABLE ix    AS INTEGER           NO-UNDO.
    
    bOk = System.IO.Directory:Exists(ipDirectory).
    IF bOk THEN 
    DO:
      Files = System.IO.Directory:GetFiles(ipDirectory).
      IF Files:Length GT 0 THEN 
      DO ix = 0 TO Files:length - 1:
        File = Files:GetValue(ix).
        addFile(ipDirectory, File).
      END.
    END.
  END METHOD. 

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:  TN 11/3-18 oFileStream:CopyTo(oZipStream). Gir kompileringsfeil i 10.2b.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID zipFile(INPUT ipFilename AS CHARACTER ):
    DEFINE VARIABLE oFileInfo             AS CLASS     System.IO.FileInfo.
    DEFINE VARIABLE oCompressedFileStream AS CLASS     System.IO.FileStream.
    DEFINE VARIABLE oZipStream            AS CLASS     System.IO.Compression.GZipStream.
    DEFINE VARIABLE oFileStream           AS CLASS     System.IO.FileStream.
    DEFINE VARIABLE MoveToFilename        AS CHARACTER NO-UNDO.
    
    oFileInfo = NEW System.IO.FileInfo(ipFilename).
    IF oFileInfo:Exists AND oFileInfo:Length GE (ZipWhenSizeInMb * (1024 * 1024)) THEN 
    DO:
/*      MoveToFilename = REPLACE(oFileInfo:FullName,oFileInfo:Extension,'_log_' + string(TODAY,'99999999') + oFileInfo:Extension).*/
      oFileStream = oFileInfo:OpenRead().
      oCompressedFileStream = System.IO.File:Create(oFileInfo:FullName + '.gz').
      oZipStream = NEW System.IO.Compression.GZipStream(oCompressedFileStream,System.IO.Compression.CompressionMode:Compress).
      /*oFileStream:CopyTo(oZipStream).*/
      oFileInfo:Delete().
    END.

  END METHOD. 

END CLASS.
