
/*------------------------------------------------------------------------
    File        : getserverfile.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : goo_000
    Created     : Tue Jun 12 15:50:36 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihDS        AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.
  
DEFINE VARIABLE cFilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE bhFile    AS HANDLE    NO-UNDO.

ASSIGN 
  cFilename = ENTRY(1,icParam,'|')
  bhFile    = ihDS:GET-BUFFER-HANDLE (1)
  .

FILE-INFO:FILE-NAME =  cFilename.
IF FILE-INFO:FILE-SIZE = ? THEN UNDO, THROW NEW Progress.Lang.AppError("Fant ikke filen " + quoter(cFilename),1).
bhFile:BUFFER-CREATE ().
COPY-LOB FROM FILE FILE-INFO:FULL-PATHNAME TO OBJECT bhFile::BLOBcontent NO-CONVERT.

CATCH eAny AS Progress.Lang.Error :
  ocReturn = PROGRAM-NAME(1) + eAny:GetMessage(1).
END CATCH.

FINALLY:
  obOk = ocReturn = ''.      
END FINALLY.
