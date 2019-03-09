/* pksdlhode_skrivfaktura.p
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
 
MESSAGE USERID("SkoTex") SKIP
Bruker.BrukerID SKIP
Bruker.ButikkNr SKIP
Butiker.RAPPrinter SKIP
cPrinter SKIP
VIEW-AS ALERT-BOX. 
 
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

    FIND FIRST PkSdlHode WHERE 
        PkSdlHode.PkSdlId = DEC(ihBuffer:BUFFER-FIELD('PksdlId'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL PkSdlHode AND CAN-FIND(FIRST FakturaHode WHERE 
                                     FakturaHode.FakturaNr = PkSdlHode.FakturaNr) THEN
    BEHANDLE:
    DO:
        FIND FIRST FakturaHode NO-LOCK WHERE
            FakturaHode.FakturaNr = PkSdlHode.FakturaNr NO-ERROR.
        RUN skrivfaktura.p (STRING(FakturaHode.Faktura_Id) + "|",TRUE,cPrinter,1,"",iFormat). 
        ASSIGN 
            obOk     = TRUE
            ocReturn = ''
            .
        DO TRANSACTION:
            FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
            ASSIGN 
                PkSdlHode.LeveringsDato = TODAY.
            FIND CURRENT PkSdlHode NO-LOCK.
        END.
    END. /* BEHANDLE */
  IF AVAIL PkSdlHode THEN RELEASE PkSdlHode.
  hQuery:GET-NEXT().
END.

