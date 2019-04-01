
/*------------------------------------------------------------------------
    File        : fix-timegrip_eksport_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter timegrip generering og eksport

    Author(s)   : tomn
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE obOk        AS LOG       NO-UNDO.
DEFINE VARIABLE ocMelding   AS CHAR      NO-UNDO.
DEFINE VARIABLE ButikkNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTid        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iReturn     AS INT       NO-UNDO.
DEFINE VARIABLE ocValue     AS CHAR      NO-UNDO.
DEFINE VARIABLE cStatusList AS CHAR      NO-UNDO.

DEFINE VARIABLE icParam     AS CHAR      NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE icSessionId AS CHAR      NO-UNDO.
DEFINE VARIABLE ocReturn    AS CHAR      NO-UNDO.
DEFINE VARIABLE dFraDato    AS DATE      NO-UNDO.
DEFINE VARIABLE dTilDato    AS DATE      NO-UNDO.
DEFINE VARIABLE iAntLest    AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAntPostert AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbLst      AS CHARACTER NO-UNDO.

DEF VAR dDato1 AS DATE.
DEF VAR dDato2 AS DATE.

DEFINE TEMP-TABLE bTGExport LIKE TGExport.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

ASSIGN
  cDbLst   = 'SkoTex,Data,Vpi'
  cLogg    = 'TIMEGRIP' + REPLACE(STRING(TODAY),'/','')
  iTid     = TIME
  dFraDato = TODAY - 6
  dTilDato = TODAY.

/* Er ikke databasene oppkoblet, avsluttes rutinen. */
IF rStandardFunksjoner:SjekkOmDbErOppkoblet(cDbLst, cLogg) = FALSE THEN 
    QUIT.
                                
{adecomm/appserv.i}

RUN initjukebox.p.


RUN bibl_logg.p (cLogg, STRING(TIME,"HH:MM:SS") + ' timegrip_eksport_run.p: Starter. ').

/* Prepper temp-tabell med ikke eksporterte datasett */
/* Allt som ikke er eksportert legges ut.            */
iAntLest = 0.
FOR EACH Butiker NO-LOCK WHERE 
  Butiker.ApningsDato <> ? AND 
  Butiker.NedlagtDato = ? AND 
  Butiker.HarButikkSystem = TRUE:
  
  /* De som ikke tidligere er eksportert. */
  FOR EACH TGExport EXCLUSIVE-LOCK WHERE 
    TGExport.TGStore_Id   = Butiker.Butik AND 
    TGExport.TGExportDate >= dFraDato:
    
    ASSIGN
        TGExport.TGExportDate = ?
        TGExport.TGExportTime = 0 
        TGExport.TGNote       = ''
        .

    CREATE bTGExport.
    BUFFER-COPY TGExport TO bTGExport NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DELETE bTGExport.
    iAntLest = iAntLest + 1.
  END. 
END.

ASSIGN
  ihBuffer = BUFFER bTGExport:HANDLE.  
IF VALID-HANDLE(ihBuffer) THEN  
DO:
  RUN bibl_logg.p (cLogg, STRING(TIME,"HH:MM:SS") + ' tgexport_eksporter.p: Starter. Antall å eksportere: ' + STRING(iAntLest)).
  RUN tgexport_eksporter.p (
                            icParam,
                            ihBuffer,
                            icSessionId,
                            OUTPUT ocReturn,
                            OUTPUT obOK  
                           ).
  RUN bibl_logg.p (cLogg, STRING(TIME,"HH:MM:SS") + ' tgexport_eksporter.p: Ferdig. Status: ' + STRING(obOk)).
  IF NOT obOK THEN
  DO:
    ocMelding = DYNAMIC-FUNCTION("getTransactionMessage").
    RUN bibl_logg.p (cLogg, STRING(TIME,"HH:MM:SS") + ' tgexport_eksporter.p: avsluttet med melding: ' + STRING(ocMelding)).
  END.
END.

RUN bibl_logg.p (cLogg, STRING(TIME,"HH:MM:SS") + ' timegrip_eksport_run.p: Ferdig. Tidsbruk: ' + string(TIME - iTid,"HH:MM:SS") + ' Status: ' + STRING(obOk) + CHR(10)).

QUIT.
