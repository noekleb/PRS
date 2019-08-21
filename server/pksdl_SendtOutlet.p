/* Hent merkelapper for pakkseddel
   Parameter:  <PkSdlId>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

ASSIGN 
    ocReturn  = ""
    .

IF icParam = '' /*OR NOT CAN-FIND(Butiker WHERE 
                                Butiker.butik = INT(icParam)
                                ) */ THEN 
  RETURN.                                

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND PkSdlHode EXCLUSIVE-LOCK WHERE
    PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
    
  IF AVAILABLE PkSdlHode THEN 
  DO:
    ASSIGN 
      PkSdlHode.SendtFraLagerTilOutlet = (IF INT(icParam) = 0 THEN ? ELSE NOW)
      PkSdlHode.SendtOutlet            = INT(icParam)
      .
    RELEASE PkSdlHode.
  END.

  hQuery:GET-NEXT().
END. /* BLOKKEN */


DELETE OBJECT hQuery NO-ERROR.

obOk = TRUE.
ocReturn = ''.
