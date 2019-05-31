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
/*DEFINE VARIABLE hJbApi AS HANDLE NO-UNDO.*/

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

obOk = rKundeordreBehandling:sjekkTvang( OUTPUT iStatusLst, OUTPUT bSTvang ).  
 
/* Denne står til 0 normalt sett, da faktura utstedes i nettbutikk. */
/* Hos Gant er den satt til 1.                                      */    
{syspara.i  150 1 8 cTekst}
IF CAN-DO(cTekst,'1') THEN  
    bOpprettFaktura = TRUE.

ASSIGN
    bTest = TRUE 
    cLogg = 'KOrdreUtlever' + REPLACE(STRING(TODAY),'/','')
    .
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().
/* Er det tvang, og noen av postene ikke er bekreftet mottat fra leverandør, skal det varsles om det og avsluttes. */
IF bSTvang THEN 
DO:
    obOk = FALSE. 
    hQuery:GET-FIRST().
    SJEKKLOOP:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE:
        IF ihBuffer:BUFFER-FIELD('levStatus'):BUFFER-VALUE <> '55' AND ihBuffer:BUFFER-FIELD('levStatus'):BUFFER-VALUE < '40' THEN 
        DO:
            obOk = TRUE.
            LEAVE SJEKKLOOP.
        END.
        hQuery:GET-NEXT().
    END. /* SJEKKLOOP */
    IF obOk = TRUE THEN 
    DO:
        obOk = FALSE.
/*        ocReturn = 'En eller flere av de valgte kundeordre er ikke bekreftet mottat av speditør.'.*/
        ocReturn = 'En eller flere av de valgte kundeordre er utskrevet pakkseddel og/eller postpakke etikett.'.
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
        rKundeordreBehandling:LeverTilKunde(DEC(ihBuffer:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE),USERID('Skotex'),OUTPUT ocReturn,OUTPUT obOk).
        /* Flagger om det har gått bra. */
        IF NOT obOk THEN
        DO:
          LEAVE.
        END.
    END. /* BEHANDLE */
  hQuery:GET-NEXT().
END.

ERROR-STATUS:ERROR = FALSE.

RETURN.
   