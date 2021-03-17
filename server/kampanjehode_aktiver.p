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
DEFINE VARIABLE bIgnorerNOS AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBrukerId AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassord AS CHARACTER NO-UNDO.


ASSIGN
  cBrukerId = 'batch'
  cPassord  = 'batch'   
  cLogg     = 'Kampanjehode_aktiver' + REPLACE(STRING(TODAY),'/','')
  ocReturn  = ""
  iAnt      = 0
  .

/* TN 13/5-20 For overstyring av bruk som startet brokeren som AppServer benytter.   */
/* Ved oppkall via AppServer, settes batch som bruker hvis dette er angitt i kallet. */
IF TRIM(ENTRY(1,icParam,'|')) = '' AND 
  CAN-FIND(_User WHERE 
           _User._UserId = cBrukerid) THEN 
  SETUSERID(cBrukerId, cPassord, 'SkoTex') NO-ERROR.

RUN dKampanjeHode.w PERSISTENT SET hdKampanjeHode.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:

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
