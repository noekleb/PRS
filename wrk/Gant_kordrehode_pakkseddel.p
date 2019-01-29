/* Registrer 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE bOpprettFaktura AS LOG NO-UNDO.
DEFINE VARIABLE cPrinter        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

/*iIntegrasjon     = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",*/
/*    "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 1",              */
/*    "Parameter1")).                                                */
    
DEFINE BUFFER bufKOrdrEHode FOR KOrdreHode.    
    
ASSIGN 
    cLogg = 'kordrehode_pakkseddel' + REPLACE(STRING(TODAY),'/','')
    .

FIND Bruker NO-LOCK WHERE 
  Bruker.BrukerId = USERID("SkoTex") NO-ERROR.
IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = Bruker.Butik NO-ERROR.
    IF AVAILABLE Butiker THEN 
      cPrinter = Butiker.RAPPrinter.    
END.
ELSE cPrinter = ''.
    
RUN bibl_loggDbFri.p (cLogg, 'skrivkundeordre.p: START' + 
                 ' Bruker: ' + string(Bruker.BrukerId) +
                 ' Butikk: ' + STRING(Bruker.Butik) + 
                 ' Skriver: ' + cPrinter).
        
bOpprettFaktura = IF DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 8","Parameter1") = '1'
    THEN TRUE
    ELSE FALSE.
    
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
        RUN skrivkundeordre.p (STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|FULL",
            YES,cPrinter,2,"",DYNAMIC-FUNCTION("getTransactionMessage")) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO: 
            DO iX = 1 TO ERROR-STATUS:NUM-MESSAGES:
                cTekst = ERROR-STATUS:GET-MESSAGE(iX).
                RUN bibl_loggDbFri.p (cLogg, cTekst).
            END.    
            hQuery:GET-NEXT(). 
        END.
        ELSE DO TRANSACTION:     
            FIND bufKORdreHode EXCLUSIVE-LOCK WHERE 
                bufKOrdreHode.KOrdre_Id = ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE NO-ERROR.
            IF AVAILABLE bufKOrdrEHode THEN 
            DO:
                ASSIGN 
                    bufKOrdreHode.AntApnet  = bufKOrdreHode.AntApnet + 1
                    .
                RELEASE bufKOrdreHode.      
            END.    

            RUN bibl_loggDbFri.p (cLogg, 'skrivkundeordre.p: START' + 
                ' KOrdre_Id: ' + STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) +
                ' Bruker: ' + string(Bruker.BrukerId) +
                ' Butikk: ' + STRING(Bruker.Butik) + 
                ' Skriver: ' + cPrinter + 
                ' Fil: ' + RETURN-VALUE            
                ).
        END.
            
        ASSIGN 
            obOk     = TRUE
            ocReturn = ''
            .
    END. /* BEHANDLE */
  IF AVAIL KOrdreHode THEN RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END.

