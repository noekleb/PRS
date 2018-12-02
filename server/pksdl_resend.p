/* Oppdater pakkseddelhoder slik at de blir distribuert (på nytt) fra HK
   Parameter:  
   Opprettet: 24.06.10 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

DO TRANSACTION:

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
    FIND FIRST PkSdlHode EXCLUSIVE-LOCK
         WHERE PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
         NO-WAIT NO-ERROR.
    IF AVAIL PkSdlHode THEN
      PkSdlHode.PkSdlStatus = 5.
    ELSE DO:
      ocReturn = "Pakkseddel ikke tilgjengelig for oppdatering. Program: " + PROGRAM-NAME(1).
      LEAVE.
    END.
    hQuery:GET-NEXT().
  END.

END.

hQuery:GET-FIRST().

DO TRANSACTION:

  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
    FIND FIRST PkSdlHode EXCLUSIVE-LOCK
         WHERE PkSdlHode.PkSdlId = DECIMAL(ihBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
         NO-WAIT NO-ERROR.
    IF AVAIL PkSdlHode THEN
      PkSdlHode.PkSdlStatus = 6.
    ELSE DO:
      ocReturn = "Pakkseddel ikke tilgjengelig for oppdatering. Program: " + PROGRAM-NAME(1).
      LEAVE.
    END.
    hQuery:GET-NEXT().
  END.

END.


DELETE OBJECT hQuery NO-ERROR.

obOK = ocReturn = "".

