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
              
    
/* Denne står til 0 normalt sett, da faktura utstedes i nettbutikk. */
/* Hos Gant er den satt til 0.                                      */    
bOpprettFaktura = IF DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 8","Parameter1") = '1'
    THEN TRUE
    ELSE FALSE.
    
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
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
        IF NOT CAN-DO('30,40',KOrdreHode.LevStat) THEN  
            LEAVE BEHANDLE. 
        IF KOrdreHode.Opphav = 10 THEN 
            RUN kordre_sjekkartnettbutikk.p(DEC(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)).

        IF NOT DYNAMIC-FUNCTION("runproc","kordre_levering.p",
                                STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE)
                                ,
                                ?
                                ) 
            THEN DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
        ELSE 
        DO:
            IF bOpprettFaktura = FALSE THEN 
            DO:
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
                                           ).
                ELSE
                    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
            END.
            ELSE 
            DO:
                IF DYNAMIC-FUNCTION("runproc",
                    "kordre_fakturer.p",
                    STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),
                    ?) THEN 
                    RUN skrivkundeordre.p (STRING(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|utlev",
                        YES,"",1,"",DYNAMIC-FUNCTION("getTransactionMessage")).
                ELSE
                    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
            END.
        END.
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
