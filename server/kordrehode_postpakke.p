/* Registrer 
   Parameter:  
   Opprettet:             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iIntegrasjon AS INT NO-UNDO.
DEFINE VARIABLE cSkriver AS CHARACTER NO-UNDO.
DEF VAR hQuery       AS HANDLE NO-UNDO.

iIntegrasjon     = INT(DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 19 and SysGr = 9 and ParaNr = 1",
    "Parameter1")).
    
cSkriver = DYNAMIC-FUNCTION("getFieldValues","SysPara",
    "WHERE SysHId = 210 and SysGr = 100 and ParaNr = 7",
    "Parameter1").


/*
MESSAGE 'icParam' icParam
VIEW-AS ALERT-BOX.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN 
DO:
    CREATE TEMP-TABLE httTable.
    httTable:ADD-LIKE-FIELD("ArtikkelNr","ArtBas.ArtikkelNr").
    httTable:TEMP-TABLE-PREPARE("ttArtBas").
    ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
    IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
        FIND ArtBas WHERE ROWID(ArtBas) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN 
        DO:
            ihBuffer:BUFFER-CREATE().
            ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
        END.
    END.
    ELSE
    DO ix = 3 TO NUM-ENTRIES(icParam):
        FIND ArtBas WHERE ArtBas.ArtikkelNr = DEC(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
        IF AVAIL ArtBas THEN 
        DO:
            ihBuffer:BUFFER-CREATE().
            ihBuffer:BUFFER-COPY(BUFFER ArtBas:HANDLE).
        END.
    END.
END.
*/

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

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

