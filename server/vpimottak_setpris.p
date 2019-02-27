/* Registrer innleveranse fra pakkseddel
   Parameter:  
   Opprettet: 25.11.2007             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.


DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fPris           AS DEC    NO-UNDO.

ASSIGN
  fPris        = DEC(ENTRY(1,icParam,';'))
.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

  /*Ved feil, skal alle feil samles, og sendes etter alle er lest. Gi da også stat på 
hvor mange som ble oppettet i artbas og vareboklinje*/
  DO TRANSACTION:
    FIND FIRST VPImottak WHERE VPImottak.VPImottakId = DEC(ihBuffer:BUFFER-FIELD('VPImottakId'):BUFFER-VALUE)
                         EXCLUSIVE-LOCK NO-ERROR.
    
    IF AVAIL VPImottak THEN
    DO:
      ASSIGN 
        VPImottak.pris      = fPris
        VPImottak.behStatus = 20
      .
    END.
  END.
  IF AVAIL VPImottak THEN RELEASE VPImottak.
  hQuery:GET-NEXT().
END.
ASSIGN 
  ocReturn = ''
  obOk     = TRUE
.
