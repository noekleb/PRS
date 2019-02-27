/* Setter antall plukket lik antall foreslått plukket.
   Parametere:  buffersandfields
                query 
   
   Opprettet: 21.04.08 av BHa 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR hBuffer      AS HANDLE NO-UNDO.
DEF VAR piBuntNr     AS INT    NO-UNDO.
DEF VAR iAntLinjer   AS INT    NO-UNDO.
DEF VAR iAntFeil     AS INT    NO-UNDO.
DEF VAR piLinjeNr    AS INT    NO-UNDO.
DEF VAR wEDB-System  AS CHAR   NO-UNDO.
DEF VAR wTabell      AS CHAR   NO-UNDO.
DEF VAR iOppdDirekte AS INT    NO-UNDO.
DEF VAR lantPlukket  AS DEC    NO-UNDO.

DEF VAR dPlListeId  LIKE PlListeHode.PlListeId NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

{syspara.i 11 5 3 iOppdDirekte INT}
{syspara.i 1 2 3 wEDB-System}
if wEDB-System = "" then
  wEDB-System = "OVERFOR-LOCK".

/* Tømmer */
FOR EACH TT_OvBuffer:
    DELETE TT_OvBuffer. /* ev metod empty-temp-table */
END.

ASSIGN 
    dPlListeId = DECI(ENTRY(1,icParam,"¤"))
    piLinjeNr  = 1
    .
    
FIND PlListeHode NO-LOCK WHERE
    PlListeHode.PlListeId = dPlListeId NO-ERROR.

DO TRANSACTION:
    CREATE QUERY hQuery.
    CREATE BUFFER hBuffer FOR TABLE "PlListeLinje".
    hQuery:SET-BUFFERS(hBuffer).
    hQuery:QUERY-PREPARE("For each PlListeLinje exclusive-lock where PlListeLinje.PlListeId = " + 
                          STRING(dPlListeId)).
    hQuery:QUERY-OPEN().

    hQuery:GET-FIRST().
    IF NOT hBuffer:AVAIL THEN
        RETURN.
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:

      /* Antall linjer lagt ut i filen */
      iAntLinjer = iAntLinjer + 1.

      /* Setter antall plukket lik antall foreslått plukket. */
      ASSIGN
          hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE = hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
          lantPlukket = lantPlukket + hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
          .

      hQuery:GET-NEXT().
    END.
END.

OUTPUT CLOSE.

DELETE OBJECT hBuffer NO-ERROR.
DELETE OBJECT hQuery  NO-ERROR.

IF iAntLinjer > 0 THEN
DO TRANSACTION:
    FIND CURRENT PlListeHode EXCLUSIVE-LOCK.
    ASSIGN
        PlListeHode.DatoPlukket   = TODAY
        PlListeHode.TidPlukket    = TIME
        PlListeHode.AntallPlukket = lantPlukket
        PlListeHode.PlMerknad     = "* Satt automatisk"
        .
    RELEASE PlListeHode.
END.

IF ocReturn = "" THEN obOk = TRUE.

