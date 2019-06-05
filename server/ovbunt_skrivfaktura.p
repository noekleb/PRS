/* ovbunt_skrivfaktura.p
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

DEF VAR hQuery       AS HANDLE NO-UNDO.

DEFINE BUFFER bufPkSdlHode FOR PkSdlHode.

ASSIGN 
    iFormat = 1
    .

FIND Bruker NO-LOCK WHERE 
  Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = Bruker.Butik NO-ERROR.
    IF AVAILABLE Butiker THEN 
/*      cPrinter = Butiker.RAPPrinter.*/
      cPrinter = Butiker.Fakturaskriver.  
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

    FIND FIRST Ovbunt WHERE 
        OvBunt.BuntNr = INT(ihBuffer:BUFFER-FIELD('Ovbunt'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL OvBunt AND OvBunt.Faktura_Id > 0 THEN
    BEHANDLE:
    DO:
        RUN skrivfaktura.p (STRING(OvBunt.Faktura_Id) + "|",TRUE,cPrinter,1,"",iFormat). 
        ASSIGN 
            obOk     = TRUE
            ocReturn = ''
            .
    END. /* BEHANDLE */
  IF AVAIL Ovbunt THEN RELEASE Ovbunt.
  hQuery:GET-NEXT().
END.

