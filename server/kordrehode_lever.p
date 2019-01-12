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

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEFINE VARIABLE hJbApi AS HANDLE NO-UNDO.

DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDummy AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE cKOrdreValiderMsg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bSTvang AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusLst AS INTEGER NO-UNDO.

DEFINE VARIABLE rKundeordreBehandling AS cls.Kundeordre.KundeordreBehandling NO-UNDO.
rKundeordreBehandling  = NEW cls.Kundeordre.KundeordreBehandling( ) NO-ERROR.

 SUBSCRIBE "KOrdreValiderMsg" ANYWHERE.

/*iIntegrasjon     = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",*/
/*    "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 1",              */
/*    "Parameter1")).                                                */

/* Start av dette biblioteket må gjøres for å kunne kjøre AppServer funksjonene i denne rutinen. */
hJBApi = DYNAMIC-FUNCTION("startAsLib" IN SOURCE-PROCEDURE).

/* Dersom en .p kalt direkte herfra (f.eks  kordre_sjekkartnettbutikk.p) OGSÅ gjør slike kall, (f.ex getFieldList..) så må den også legge til biblioteket som SUPER til seg selv.
   Dermed må startAsLib være tilgjengelig HERFRA: */ 
       
FUNCTION startAsLib RETURNS HANDLE ():
   SOURCE-PROCEDURE:ADD-SUPER-PROCEDURE(hJbApi).
   RETURN hJbApi. 
END FUNCTION.           
 
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
    
/* Denne står til 0 normalt sett, da faktura utstedes i nettbutikk. */
/* Hos Gant er den satt til 0.                                      */    
bOpprettFaktura = IF DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 8","Parameter1") = '1'
    THEN TRUE
    ELSE FALSE.

ASSIGN
    bTest = TRUE 
    cLogg = 'KOrdreUtlever' + REPLACE(STRING(TODAY),'/','')
    .

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'Start kordrehode_lever.p').
    RUN Bibl_LoggDbFri.p(cLogg,'    bOpprettFaktura: ' + STRING(bOpprettFaktura) + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'    Buffer NAME: ' + ihBuffer:NAME + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'    icParam: ' + icParam + '.').
END.
    
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

/* Er det tvang, og noen av postene ikke har fått skrevet ut pakkseddel og postpakke etikett, skal det varsles om det og avsluttes. */
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
        ocReturn = 'En eller flere av de valgte kundeordre mangler utskrift av pakkseddel og postpakke etikett.'.
        RETURN.    
    END.
END.

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

    IF ihBuffer:AVAILABLE AND CAN-FIND(Kunde WHERE 
                                     Kunde.KundeNr = DEC(ihBuffer:BUFFER-FIELD('KundeNr'):BUFFER-VALUE)) THEN
    BEHANDLE:
    DO:
        IF NOT CAN-DO('30,35,40,45,47',STRING(ihBuffer:BUFFER-FIELD('LevStatus'):BUFFER-VALUE)) THEN  
            LEAVE BEHANDLE. 
        IF INT(ihBuffer:BUFFER-FIELD('Opphav'):BUFFER-VALUE) = 10 THEN 
            RUN kordre_sjekkartnettbutikk.p(DEC(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)).

        IF bTest THEN 
            RUN Bibl_LoggDbFri.p(cLogg,'    start kordre_levering.p').
        /* Validerer ordren. Er all informasjon om artikler korrekt, og kan ordren utleveres. */
        IF NOT DYNAMIC-FUNCTION("runproc","kordre_levering.p",
                                STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)
                                ,
                                ?
                                ) THEN
          MESSAGE  DYNAMIC-FUNCTION("getTransactionMessage")
          VIEW-AS ALERT-BOX.                     
        ELSE DO:
            IF bOpprettFaktura = FALSE THEN 
            DO:
                IF bTest THEN
                DO: 
                    RUN Bibl_LoggDbFri.p(cLogg,'    bOpprettFaktura = false').
                    RUN Bibl_LoggDbFri.p(cLogg,'    Opphav: ' + ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE).
                    IF INTEGER(ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE) = 10 THEN 
                        RUN Bibl_LoggDbFri.p(cLogg,'    start kordre_kontant.p').
                    ELSE 
                        RUN Bibl_LoggDbFri.p(cLogg,'    start kordre_fakturer.p').
                END.
                IF DYNAMIC-FUNCTION("runproc",
                                    IF INTEGER(ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE) = 10  
                                        THEN "kordre_kontant.p"    /* Nettbutikk har opphav 10. Det behandles da som et kontantsalg (Hos Gant). */
                                        ELSE "kordre_fakturer.p",  /* Ellers behandles det som kreditsalg.                                      */
                                    STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                                    ?
                                    ) THEN 
                    RUN skrivkundeordre.p (STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                                           YES,
                                           "",
                                           1,
                                           "",
                                           DYNAMIC-FUNCTION("getTransactionMessage")
                                           ) NO-ERROR.
                ELSE
/*                    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").*/
                    ocReturn = DYNAMIC-FUNCTION("getTransactionMessage").
                    
                IF bTest THEN
                DO: 
                    IF INTEGER(ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE) = 10 THEN 
                        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig kordre_kontant.p - Melding: ' + ocReturn).
                    ELSE 
                        RUN Bibl_LoggDbFri.p(cLogg,'    ferdig kordre_fakturer.p - Melding: ' + ocReturn).
                END.
            END.
            ELSE 
            DO:
                IF bTest THEN
                DO: 
                    RUN Bibl_LoggDbFri.p(cLogg,'    bOpprettFaktura = true').
                    RUN Bibl_LoggDbFri.p(cLogg,'    Opphav: ' + ihBuffer:BUFFER-FIELD("Opphav"):BUFFER-VALUE).
                    RUN Bibl_LoggDbFri.p(cLogg,'    start kordre_fakturer.p').
                END.
            
                IF DYNAMIC-FUNCTION("runproc",
                    "kordre_fakturer.p",
                    STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                    ?) THEN 
                    RUN skrivkundeordre.p (STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                        YES,"",1,"",DYNAMIC-FUNCTION("getTransactionMessage")) NO-ERROR.
                ELSE
/*                    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").*/
                    ocReturn = DYNAMIC-FUNCTION("getTransactionMessage").
                    
                IF bTest THEN
                DO: 
                    RUN Bibl_LoggDbFri.p(cLogg,'    ferdig kordre_fakturer.p - Melding: ' + ocReturn).
                END.
            END.
        END.
        obOk = NOT ERROR-STATUS:ERROR.
        IF NOT obOk THEN
        DO:
          ocReturn = ERROR-STATUS:GET-MESSAGE(1).
          LEAVE.
        END.
    END. /* BEHANDLE */
  hQuery:GET-NEXT().
END.

IF bTest THEN 
DO:
    RUN Bibl_LoggDbFri.p(cLogg,'    ocReturn: ' + ocReturn + '.').
    RUN Bibl_LoggDbFri.p(cLogg,'Slutt kordrehode_lever.p').
END.

ERROR-STATUS:ERROR = FALSE.

RETURN.

PROCEDURE KOrdreValiderMsg:
    DEFINE INPUT PARAMETER pcMsg AS CHARACTER NO-UNDO.
    ASSIGN 
        cKOrdreValiderMsg = pcMsg.
END.    