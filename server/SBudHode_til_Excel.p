/* Hent merkelapper for telleliste
   Parameter:  <TelleNr>
   Opprettet: 07.09.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery    AS HANDLE NO-UNDO.
DEF VAR iSBudId   AS INT    FORMAT ">>>>>9" NO-UNDO.
DEF VAR cocReturn AS CHAR   NO-UNDO.
DEF VAR pihBuffer AS HANDLE NO-UNDO.

ocReturn = "".

DEF TEMP-TABLE tmpSBudHode  
    FIELD SBudId LIKE SBudHode.SBudId
    .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE ").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN
    iSBudId = INT(ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE)
    .

  FIND SBudHode NO-LOCK WHERE SBudHode.SBudId = iSbudId NO-ERROR.
  IF AVAILABLE SBudHode THEN 
  DO:
      IF NOT AVAILABLE tmpSBudHode THEN
          CREATE tmpSBudHode.
      tmpSBudHode.SBudId = iSBudId.

      pihBuffer = BUFFER tmpSBudHode:HANDLE.
      RUN SBud_til_Excel.p ("",
                                pihBuffer,
                                ?,
                                OUTPUT cocReturn,
                                OUTPUT obOk
                               ).
  END.
  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

obOk     = YES.
