/* kordrehode_skrivfaktura.p
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cPrinter        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE iFormat AS INTEGER NO-UNDO.
DEFINE VARIABLE cBruker AS CHARACTER NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

ASSIGN 
    iFormat = 1
    cBruker = ENTRY(1,icParam,'|')
    .
IF cBruker = '' THEN 
  cBruker = USERID('SkoTex').

FIND Bruker NO-LOCK WHERE 
  Bruker.BrukerId = cBruker NO-ERROR.
IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = Bruker.Butik NO-ERROR.
    IF AVAILABLE Butiker THEN 
      cPrinter = Butiker.RAPPrinter.    
END.
ELSE cPrinter = ''.
 
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

    FIND FIRST KordreHode WHERE 
        KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL KOrdreHode AND CAN-FIND(Kunde WHERE 
                                     Kunde.KundeNr = KOrdreHode.KundeNr) THEN
    BEHANDLE:
    DO:
        RUN skrivfaktura.p (STRING(KOrdreHode.Faktura_Id) + "|",TRUE,cPrinter,1,"",iFormat). 
        ASSIGN 
            obOk     = TRUE
            ocReturn = ''
            .
    END. /* BEHANDLE */
  IF AVAIL KOrdreHode THEN RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END.

