/* importNyOutlet.p */

DEFINE VARIABLE cCmd                AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cCopyLog            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cFilNavn            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cStatus             AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bError              AS LOG                            NO-UNDO.
DEFINE VARIABLE cError              AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iLoop               AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cRecord             AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cTempFil            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE i2Loop              AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cCmdFil             AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cCmdBody            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cOrgFilMaske        AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cFil                AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cLogistikKatalog    AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cMalKatalog         AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE httImpFilLinje      AS HANDLE                         NO-UNDO.
DEFINE VARIABLE obOk                AS LOG                            NO-UNDO.
DEFINE VARIABLE ocReturn            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cFilLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

{ cls\StdFunk\dsttImpFil.i }

DEFINE STREAM Inn.
DEFINE STREAM Ut.

IF SEARCH('tnc.txt') <> ? THEN 
  ASSIGN
    cLogistikKatalog = 'C:\tmp\tn\Gant\'
    cMalKatalog      = 'C:\NSoft\polygon\PRS\konv'
    cOrgFilMaske     = 'NYOUTLET*.xlsx'
    cCopyLog         = 'C:\NSoft\polygon\PRS\log\nyoutlet.log'
    cCmdFil          = 'cmd\hentNyOutlet.bat'
    cCmdBody         = 'copy "' + cLogistikKatalog + cOrgFilMaske + '" ' + cMalKatalog + ' > ' + cCopyLog 
    .
ELSE 
  ASSIGN
    cLogistikKatalog = '\\gant0047\felles\LOGISTIKK DOKUMENTER\outlet\'
    cMalKatalog      = 'C:\appdir\se\konv'
    cOrgFilMaske     = 'NYOUTLET*.xlsx'
    cCopyLog         = 'C:\appdir\se\log\nyoutlet.log'
    cCmdFil          = 'cmd\hentNyOutlet.bat'
    cCmdBody         = 'copy "' + cLogistikKatalog + cOrgFilMaske + '" ' + cMalKatalog + ' > ' + cCopyLog 
    .
  
ASSIGN   
  cCmd     = 'cmd\hentnyOutlet.bat'
  cLogg    = 'importNyOutlet' + REPLACE(STRING(TODAY),'/','') 
  .

/* Oppretter bat fil */
OUTPUT STREAM Ut TO VALUE(cCmdFil).
PUT STREAM Ut UNFORMATTED
  cCmdBody 
  SKIP.
OUTPUT STREAM Ut CLOSE.  
  
  
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

/* T�mmer loggen fra forrige kj�ring */
OS-DELETE VALUE(cCopyLog).

/* Kj�rer bat filen som gj�r kopieringsjobben. */
OS-COMMAND SILENT VALUE(cCmd). 

INPUT STREAM Inn FROM VALUE(cCopyLog).
REPEAT:
  iLoop = iLoop + 1.
  IMPORT STREAM Inn UNFORMATTED
    cRecord.
  IF iLoop = 1 THEN
  DO:
    ASSIGN 
      cFilNavn = TRIM(cRecord)
      cFilNavn = cMalKatalog + '\' + ENTRY(NUM-ENTRIES(cFilNavn,'\'),cFilNavn,'\')
      .    
  END.
  ELSE IF iLoop = 2 THEN
    DO:
      ASSIGN 
        cStatus = TRIM(cRecord)
        .
    END.
    
  DO:
    bError = iLoop > 2.
    cError = cError + 
      (IF cError = '' THEN '' ELSE CHR(10)) + 
      TRIM(cRecord).
  END.
END.
INPUT STREAM Inn CLOSE.

IF bError = FALSE THEN 
DO:
  /* Konverterer filen i m�lkatalogen. */
  rStandardFunksjoner:konvExcel2csv( INPUT cFilNavn + '|otl',
                                     INPUT '',
                                     OUTPUT cTempFil,
                                     OUTPUT i2Loop ).
  cFilLst = cTempFil.
  FILLLOOP:
  DO iLoop = 1 TO NUM-ENTRIES(cFilLst):
    cTempFil = ENTRY(iLoop,cFilLst).

    IF SEARCH(cTempFil) <> ? THEN 
    DO:
      rStandardFunksjoner:importerImpFil( INPUT cTempFil,
                                          INPUT 1,
                                          INPUT-OUTPUT DATASET dsttImpFil ).
      
  /*    TEMP-TABLE ttImpFilLinje:WRITE-JSON('file', 'konv\ImpFillinje.json',TRUE).*/
       
      httImpFilLinje = BUFFER ttImpFilLinje:HANDLE.
   
      RUN pksdl_sendtimport.p ('2|' + cLogg, httImpFilLinje, '', OUTPUT ocReturn, OUTPUT obOk).
/*      IF obOk THEN                */
/*        OS-DELETE VALUE(cTempFil).*/
      
    END.
      
  END. /* FILLLOOP */
  
/*  OS-DELETE VALUE(cFilNavn).*/
  
END.


/* **********************  Internal Procedures  *********************** */