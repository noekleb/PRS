/* Registrer Slett SIEEksport record
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
  .

  DO TRANSACTION:
    FIND FIRST FalckEksport WHERE FalckEksport.EksportId = DEC(ihBuffer:BUFFER-FIELD('EksportId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL FalckEksport THEN
    DO:
      FOR EACH Falck_Sykkelregister OF FalckEksport:
          ASSIGN
              Falck_Sykkelregister.EksportId      = 0
              Falck_Sykkelregister.SendtDato      = ?
              Falck_Sykkelregister.SendtTid       = 0
              Falck_Sykkelregister.EksportertDato = ?
              Falck_Sykkelregister.EksportertTid  = 0
              .
      END.

      DELETE FalckEksport NO-ERROR.
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END.
  END.
  IF AVAIL FalckEksport THEN RELEASE FalckEksport.
  hQuery:GET-NEXT().
END.

