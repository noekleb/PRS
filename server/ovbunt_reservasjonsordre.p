/* Registrer 
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
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStatusLst AS INTEGER NO-UNDO.
DEFINE VARIABLE cBruker AS CHARACTER NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

DEFINE BUFFER bufOvBunt FOR OvBunt.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    cLogg   = 'ovbunt_reservasjonsordre.p' + REPLACE(STRING(TODAY),'/','')
    cBruker = ENTRY(1,icParam,'|')
    .
cBruker = IF cBruker = '' THEN USERID("SkoTex") ELSE cBruker.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

RUN bibl_loggDbFri.p (cLogg, 'Start:' + 
                 ' Bruker: ' + cBruker).

/* Parameter gruppe hvor statuslisten skal hentes fra. */
{syspara.i 19 9 4 iStatusLst INT}
IF iStatusLst = 0 THEN 
    iStatusLst = 1.
ELSE 
    iStatusLst = 15.

FIND Bruker NO-LOCK WHERE 
  Bruker.BrukerId = cBruker NO-ERROR. 
IF AVAILABLE Bruker AND Bruker.Butik > 0 THEN 
DO:
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = Bruker.Butik NO-ERROR.
    IF AVAILABLE Butiker THEN
      cPrinter = Butiker.RAPPrinter.    

    RUN bibl_loggDbFri.p (cLogg, 'skrivkundeordre.p: ' + 
                     ' Bruker: ' + string(Bruker.BrukerId) +
                     ' Butikk: ' + STRING(Bruker.Butik) + 
                     ' Skriver: ' + cPrinter).
END.
ELSE DO: 
    ASSIGN 
      cPrinter = ''
      obOK     = FALSE 
      ocReturn = '** Ukjent bruker ' + cBruker + '.'
      .
END.
        
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, LEAVE: 
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE.

    FIND FIRST OvBunt WHERE 
        OvBunt.BuntNr = INT(ihBuffer:BUFFER-FIELD('BuntNr'):BUFFER-VALUE)
        NO-LOCK NO-ERROR.
    
    IF AVAIL OvBunt THEN
    BEHANDLE:
    DO:
        RUN skrivOverforing.p (STRING(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE), cBruker, cPrinter, TRUE)) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO: 
            DO iX = 1 TO ERROR-STATUS:NUM-MESSAGES:
                cTekst = ERROR-STATUS:GET-MESSAGE(iX).
                RUN bibl_loggDbFri.p (cLogg, cTekst).
            END.    
            hQuery:GET-NEXT(). 
        END.
        ELSE DO:
            RUN bibl_loggDbFri.p (cLogg, 'Utskrift: ' + 
                ' BuntNr: ' + STRING(ihBuffer:BUFFER-FIELD("BuntNr"):BUFFER-VALUE) +
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
  IF AVAIL OvBunt THEN RELEASE OvBunt.
  hQuery:GET-NEXT().
END.

RUN bibl_loggDbFri.p (cLogg, 'Slutt.').
