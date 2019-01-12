/* Registrer 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iIntegrasjon AS INT NO-UNDO.
DEFINE VARIABLE iStatusLst AS INTEGER NO-UNDO.
DEFINE VARIABLE cSkriver AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE bSTvang AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEFINE BUFFER bufKORdreHode FOR KOrdreHode.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

iIntegrasjon     = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 1",
    "Parameter1")).
    
cSkriver = DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 210 and SysGr = 100 and ParaNr = 7",
    "Parameter1").

/* Parameter gruppe hvor statuslisten skal hentes fra. */
{syspara.i 19 9 4 iStatusLst INT}
IF iStatusLst = 0 THEN 
    iStatusLst = 1.
ELSE 
    iStatusLst = 15.
/* Tvang på å følge odrestatus i ordrebehandling. */
IF iStatusLst = 15 THEN 
DO:
    {syspar2.i 19 9 4 cTekst}
    IF CAN-DO('1',cTekst) THEN 
        bSTvang = TRUE.
    ELSE 
        bSTvang = FALSE.
END.
ELSE bSTvang = FALSE.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

/* Er det tvang, og noen av postene ikke har fått skrevet ut pakkseddel, skal det varsles om det og avsluttes. */
IF bSTvang THEN 
DO:
    obOk = FALSE. 
    hQuery:GET-FIRST().
    SJEKKLOOP:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE:
        IF ihBuffer:BUFFER-FIELD('levStatus'):BUFFER-VALUE < '35' THEN 
        DO:
            obOk = TRUE.
            LEAVE SJEKKLOOP.
        END.
        hQuery:GET-NEXT().
    END. /* SJEKKLOOP */
    IF obOk = TRUE THEN 
    DO:
        obOk = FALSE.
        ocReturn = 'En eller flere av de valgte kundeordre mangler utskrift av pakkseddel.'.
        RETURN.    
    END.
END.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

/*  MESSAGE 'kordrehode_postpakke.p'                          */
/*      DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)  */
/*      DEC(ihBuffer:BUFFER-FIELD('EkstOrdreNr'):BUFFER-VALUE)*/
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK.                    */


    FIND FIRST KordreHode WHERE 
        KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL KOrdreHode THEN
    BEHANDLE:
    DO:
        FIND bufKORdreHode EXCLUSIVE-LOCK WHERE 
            bufKOrdreHode.KOrdre_Id = KordreHode.KOrdre_Id NO-ERROR.
        IF AVAILABLE bufKOrdrEHode THEN 
        DO:
            ASSIGN 
                bufKOrdrEHode.AntPPEti = bufKOrdreHode.AntPPEti + 1
                .
            rKundeordreBehandling:setStatusKundeordre( INPUT STRING(bufKOrdreHode.KOrdre_Id),
                                                       INPUT IF (bufKOrdreHode.LevStatus < '40' AND iStatusLst = 15) THEN 40 ELSE INT(bufKOrdreHode.LevStatus)).  
            RELEASE bufKOrdreHode.      
        END.    
        
        CASE iIntegrasjon:
            WHEN 1 THEN
                DO:
                    RUN ekspWinEDI.p(STRING(KordreHode.KOrdre_Id) + '|WinEDI' + '|' + cSkriver).
                END.
            WHEN 2 THEN
                DO:
                    RUN ekspUniFaun.p(STRING(KordreHode.KOrdre_Id) + '|UniFaun' + '|' + cSkriver).
                END.
            OTHERWISE
            DO:
                MESSAGE 'Ukjent integrasjonsoppsett for postpakke etikettskriver.'
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
            END.
        END CASE.
        obOk = NOT ERROR-STATUS:ERROR.
        IF NOT obOk THEN
        DO:
          ocReturn = ERROR-STATUS:GET-MESSAGE(1).
          LEAVE.
        END.
      
    END. /* BEHANDLE */


  IF AVAIL KOrdreHode THEN RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END.

