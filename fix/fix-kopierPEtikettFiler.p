
/*------------------------------------------------------------------------
    File        : kopierFilerChainWebTest.p
    Purpose     : 

    Syntax      :

    Description : Oppstart av klassen som kopierer RIGAL filer fra produksjon 
                  til test server.

    Author(s)   : Tom Nøkleby
    Created     : Mon Apr 17 14:00:00 CET 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iX           AS INTEGER   NO-UNDO.       
DEFINE VARIABLE cTxt         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLogg        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalogLst  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInnFil      AS CHARACTER NO-UNDO.
DEFINE VARIABLE mInnFil      AS MEMPTR    NO-UNDO.
DEFINE VARIABLE bOk          AS LOG       NO-UNDO.
DEFINE VARIABLE cPrefixLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMaalKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop        AS INTEGER   NO-UNDO.
DEFINE VARIABLE dDato        AS DATE      NO-UNDO.
DEFINE VARIABLE cFilNavn     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUtFilNavn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRecord      AS CHAR      NO-UNDO.

DEF VAR pcFileName    AS CHAR FORMAT "x(80)" NO-UNDO.
DEF VAR pcFilePath    AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR pcFileAttrib  AS CHAR FORMAT "x(15)" NO-UNDO.

DEFINE STREAM InnFil.
DEFINE STREAM LesInn.
DEFINE STREAM LesUt.

CURRENT-WINDOW:WIDTH = 350.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN  
    cLogg        = 'fix-kopierPEtikettFiler' + REPLACE(STRING(TODAY,"99/99/9999"),'/','')
    cPrefixLst   = 'txt'
    cKatalogLst  = 'C:\home\lindbak\ankommet\petikett\bku'
    cMaalKatalog = 'C:\home\lindbak\ankommet\petikett'
    dDato        = 06/01/2017
    cUtFilNavn   = 'tmpPEtikett.txt'
    .

KATALOG_LOOP:
DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
    cKatalog = ENTRY(iLoop,cKatalogLst).
    
    INPUT STREAM InnFil FROM OS-DIR (cKatalog) NO-ECHO.
    FILEINPUTSTREAM:
    REPEAT:
        IMPORT STREAM InnFil
            pcFileName  
            pcFilePath  
            pcFileAttrib
            NO-ERROR.

        /* Bare filer skal behandles, og bare filer med riktig prefix. */
        IF LOOKUP("F",pcFileAttrib) <> 0 AND 
        CAN-DO(cPrefixLst,ENTRY(2,pcFileName,'.')) THEN
        FILBLOKK:
        DO:
            /* Åpner for filinformasjonen */
            ASSIGN
              FILE-INFO:FILE-NAME = pcFilePath
              . 

            /* Bare filer yngre enn dato skal behandles. */
            IF FILE-INFO:FILE-MOD-DATE <= dDato THEN 
              NEXT FILEINPUTSTREAM.

            /*
            DISPLAY 
                pcFileName FORMAT "x(40)"
                pcFilePath  FORMAT "x(80)"
                pcFileAttrib  FORMAT "x(4)"
                FILE-INFO:FILE-NAME 
            WITH WIDTH 350.
            */

            INPUT STREAM LesInn FROM VALUE(FILE-INFO:FILE-NAME).
            OUTPUT STREAM LesUt TO VALUE(cMaalKatalog + cUtFilNavn) APPEND.
            REPEAT:
                IMPORT STREAM LesInn UNFORMATTED 
                    cRecord.
                PUT STREAM LesUt UNFORMATTED 
                    cRecord
                SKIP.
            END.
            OUTPUT STREAM LesUt CLOSE.
            INPUT STREAM LesInn CLOSE.
                
         END. /* FILBLOKK */
    END. /* FILEINPUTSTREAM */

END. /* KATALOG_LOOP */


    
