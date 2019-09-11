/* kampanjehode_aktiver.p 
   Parameter:  
   Opprettet:               
-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE ihBufPkSdlLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutletLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE NO-UNDO.
DEFINE VARIABLE iLagereCom AS INTEGER NO-UNDO.
DEFINE VARIABLE hdKampanjeHode AS HANDLE NO-UNDO.

ASSIGN 
  cLogg     = 'Kampanjehode_aktiver' + REPLACE(STRING(TODAY),'/','')
  ocReturn  = ""
  iAnt      = 0
  .

RUN dKampanjeHode.w PERSISTENT SET hdKampanjeHode.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:

  RUN Aktiver IN hdKampanjeHode (INPUT INT(ihBuffer:BUFFER-FIELD("KampanjeId"):BUFFER-VALUE)).

  hQuery:GET-NEXT().
END. /* BLOKKEN */

IF VALID-HANDLE(hdKampanjeHode) THEN 
  DELETE PROCEDURE hdKampanjeHode.

DELETE OBJECT hQuery NO-ERROR.

ASSIGN 
  obOk     = TRUE 
  ocReturn = ''
  .
