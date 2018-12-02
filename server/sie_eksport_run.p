
/*------------------------------------------------------------------------
    File        : sie_eksport_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter sie generering og eksport

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
DEF VAR dDato1 AS DATE.
DEF VAR dDato2 AS DATE.

DEFINE TEMP-TABLE bSIEEksport NO-UNDO LIKE SIEEksport.

{adecomm/appserv.i}
RUN initjukebox.p.

ASSIGN
  iTid     = TIME
  dFraDato = TODAY - 5
  dTilDato = TODAY.

RUN bibl_logg.p ('SIE', STRING(TIME,"HH:MM:SS") + ' sie_eksport_run.p: Starter. ').

/* Kjører generering av datasett for TimeGrip eksport. */
FOR EACH Butiker NO-LOCK WHERE 
  Butiker.ApningsDato <> ? AND 
  Butiker.NedlagtDato = ? AND 
  Butiker.HarButikkSystem = TRUE:
  
  RUN bibl_logg.p ('SIE', STRING(TIME,"HH:MM:SS") + ' generersieeksport.p: Butikk: ' + STRING(Butiker.Butik) + ' Dato fra/til: ' + STRING(dFraDato) + '-' + STRING(dTilDato)).
  RUN generersieeksport.p (dFraDato,
                                dTilDato,
                                STRING(Butiker.Butik),
                                INPUT-OUTPUT iAntLest,
                                INPUT-OUTPUT iAntPostert,
                                OUTPUT ocMelding).

  IF ocMelding <> '' THEN 
    RUN bibl_logg.p ('SIE', STRING(TIME,"HH:MM:SS") + ' generersieeksport.p: avsluttet med melding: ' + ocMelding).
END.
RUN bibl_logg.p ('SIE', STRING(TIME,"HH:MM:SS") + ' generersieeksport.p: Ferdig med alle butikker. Antall lest: ' + STRING(iAntLest) + ' Antall postert: ' + STRING(iAntPostert)).

/* ------------- TN 9/1-2012 Dette må skrives om til SIE eksport -------------
/* Prepper temp-tabell med ikke eksporterte datasett */
/* Allt som ikke er eksportert legges ut.            */
iAntLest = 0.
FOR EACH Butiker NO-LOCK WHERE 
  Butiker.ApningsDato <> ? AND 
  Butiker.NedlagtDato = ? AND 
  Butiker.HarButikkSystem = TRUE:
  
  /* De som ikke tidligere er eksportert. */
  FOR EACH SIEEksport NO-LOCK WHERE 
    SIEEksport.ButikkNr = Butiker.Butik AND 
    SIEEksport.EkspDato = ?:
    
    CREATE bSIEEksport.
    BUFFER-COPY SIEEksport TO bSIEEksport.
    iAntLest = iAntLest + 1.
  END. 
END.

ASSIGN
  ihBuffer = BUFFER bSIEEksport:HANDLE.  
IF VALID-HANDLE(ihBuffer) THEN  
DO:
  RUN bibl_logg.p ('TIMEGRIP', STRING(TIME,"HH:MM:SS") + ' sieeksport_eksporter.p: Starter. Antall å eksportere: ' + STRING(iAntLest)).
  RUN sieeksport_eksporter.p (
                            icParam,
                            ihBuffer,
                            icSessionId,
                            OUTPUT ocReturn,
                            OUTPUT obOK 
                           ).
  RUN bibl_logg.p ('TIMEGRIP', STRING(TIME,"HH:MM:SS") + ' sieeksport_eksporter.p: Ferdig. Status: ' + STRING(obOk)).
  IF NOT obOK THEN
  DO:
    ocMelding = DYNAMIC-FUNCTION("getTransactionMessage").
    RUN bibl_logg.p ('TIMEGRIP', STRING(TIME,"HH:MM:SS") + ' sieeksport_eksporter.p: avsluttet med melding: ' + STRING(ocMelding)).
  END.
END.

RUN bibl_logg.p ('TIMEGRIP', STRING(TIME,"HH:MM:SS") + ' timegrip_eksport_run.p: Ferdig. Tidsbruk: ' + string(TIME - iTid,"HH:MM:SS") + ' Status: ' + STRING(obOk) + CHR(10)).
QUIT.

------------------------ */

