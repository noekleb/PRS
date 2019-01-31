/* kordre_sjekkmakuler.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lNekad          AS LOG NO-UNDO.
DEFINE VARIABLE lPs12           AS LOG NO-UNDO.

ASSIGN 
    obOk     = TRUE
    .

cNettButikkType = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                        "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20","Parameter1")).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    lNekad   = FALSE 
    lPs12    = FALSE
    ocReturn = ''
    .
    
  FIND FIRST KordreHode WHERE 
      KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)
      NO-LOCK NO-ERROR.
  IF AVAIL KOrdreHode THEN
  DO:    
    /* For JF */    
    IF KOrdreHode.Opphav = 10 AND cNettButikkType = "2" /* PRS nettbutikk */ THEN 
    DO:
        IF CAN-FIND(FIRST kordrelinje WHERE 
                    kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 2) THEN
            lNekad = TRUE.
        IF CAN-FIND(FIRST kordrelinje WHERE 
                    kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 0 AND 
                    KOrdrelinje.plockstatus < 3) THEN
            lPs12 = TRUE.
        IF lNekad AND lPs12 THEN 
        DO:
            ASSIGN 
                obOk = FALSE 
                ocReturn = "Behandling av order påbörjad. >> Manuell handtering krävs"
                .
            LEAVE.
        END.
    END.
  END.
  IF obOk THEN 
    LEAVE.

  IF AVAIL KOrdreHode THEN 
    RELEASE KOrdreHode.
  hQuery:GET-NEXT().
END.

