/* Kopierer pakkseddel
   Parameter:  <PkSdlId>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE rRowId AS ROWID NO-UNDO.
DEFINE VARIABLE iFrabutNr AS INTEGER NO-UNDO.

DEF VAR lPkSdlId LIKE PkSdlHode.PkSdlId.
DEF VAR lNyPkSdlId LIKE PkSdlHode.PkSdlId.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
                          
DEFINE BUFFER clButiker FOR Butiker.
DEF BUFFER bufPkSdlHode FOR PkSdlHode.
DEF BUFFER bufPkSdlLinje FOR PkSdlLinje.
DEF BUFFER bufPkSdlPris FOR PkSdlPris.

DEF VAR hQuery          AS HANDLE NO-UNDO.

{syspara.i 22 5 2 cOutletLst}
{syspara.i  5 1 1 iCl INT}
FIND clButiker WHERE 
    clButiker.butik = iCl NO-ERROR.

ASSIGN 
    ocReturn  = ""
    rRowId    = ?
    iFrabutNr = 0
    cLogg     = 'pksdl_kopier' + REPLACE(STRING(TODAY),'/','')
    .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
    /* Det skal bare komme en post i temp-tabellen. */
    iAnt = iAnt + 1.
    IF iAnt > 1 THEN 
        LEAVE BLOKKEN.
    
    /* Henter nytt ID */
    FIND LAST PkSdlHode NO-LOCK.
    IF AVAILABL PkSdlHode THEN
        lNyPkSdlId = PkSdlHode.PkSdlId + 1.
    ELSE 
        lNyPkSdlId = 1.
        
    /* Pakkseddel som skal kopieres */
    FIND LAST PkSdlHode NO-LOCK WHERE
      PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
      USE-INDEX SendtDato NO-ERROR.
    IF rRowId = ? AND AVAILABLE PkSdlHode THEN 
        ASSIGN 
            rRowId = ROWID(PkSdlHode)
            .
    IF AVAILABLE PkSdlHode THEN
    DO: 
        ASSIGN 
            lPkSdlId = PkSdlHode.PkSdlId
            .
          
        CREATE bufPkSdlHode.
        BUFFER-COPY pkSdlHode
            EXCEPT PkSdlId PksdlStatus
            TO bufPkSdlHode
            ASSIGN
                bufPkSdlHode.PkSdlId = lNyPkSdlId
                bufPkSdlHode.PkSdlStatus = 10
                .
        IF AVAILABLE bufPkSdlHode THEN 
        DO:
            FOR EACH PkSdlLinje OF PkSdlHode:
                CREATE bufPkSdlLinje.
                BUFFER-COPY PkSdlLinje
                    EXCEPT PkSdlId
                    TO bufPkSdlLinje
                    ASSIGN 
                        bufPkSdlLinje.PkSdlId   = bufPkSdlHode.PkSdlId
                        .
            END.
            FOR EACH PkSdlPris OF PkSdlHode:
                CREATE bufPkSdlPris.
                BUFFER-COPY PkSdlPris
                    EXCEPT PkSdlId
                    TO bufPkSdlPris
                    ASSIGN 
                        bufPkSdlPris.PkSdlId = bufPkSdlHode.PkSdlId.
            END.
        END.
          
        ASSIGN 
            obOK     = TRUE 
            ocReturn = STRING(ROWID(bufPkSdlHode)) + '|' + bufPkSdlHode.PksdlNr
            . 
              
        RUN bibl_loggDbFri.p (cLogg, 'Kopierer pakkseddel:' 
            + ' Fra PkSdlId: ' + STRING(lPkSdlId)
            + ' Til PkSdlId: ' + STRING(lNyPkSdlId)
            + ' PakkseddelNr: ' + STRING(ihBuffer:BUFFER-FIELD("PkSdlNr"):BUFFER-VALUE)
            ).
    END.
    
    hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

IF ocReturn = '' THEN
    ASSIGN 
        obOk     = FALSE  
        ocReturn = 'Fikk ikke kopiert pakkseddel.'.

