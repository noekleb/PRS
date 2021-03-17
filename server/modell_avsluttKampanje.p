/* Hent merkelapper for pakkseddel
   Parameter:  
   Opprettet:               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bAvbestill AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttMailLogg 
  FIELD LinjeNr AS INTEGER 
  FIELD Tekst AS CHARACTER 
  .
  
DEF VAR hQuery          AS HANDLE NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

ASSIGN 
    ocReturn  = ""
    cLogg = 'modell_avsluttKampanje.p' + REPLACE(STRING(TODAY),'/','')
    .

IF icParam = '' /*OR NOT CAN-FIND(Butiker WHERE 
                                Butiker.butik = INT(icParam)
                                ) */ THEN 
  RETURN.                                
IF NUM-ENTRIES(icParam,'|') = 2 THEN 
DO:
  ASSIGN
    bAvbestill = TRUE 
    icParam    = ENTRY(1,icParam,'|')
    .
END. 

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

EMPTY TEMP-TABLE ttMailLogg.

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND ArtBas EXCLUSIVE-LOCK WHERE
    ArtBas.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) NO-ERROR.
    
  IF AVAILABLE ArtBas THEN 
  DO:
    
    RUN artbasPriskoTilbudDeAktiver.p (cLogg, ArtBas.ArtikkelNr, -1, INPUT-OUTPUT TABLE ttMailLogg).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Deaktivert kampanje: ' + STRING(ArtBas.ArtikkelNr) + ' ' + ArtBas.LevKod + ' ' + ArtBas.LEvFargKod 
      ).
    
    RELEASE ArtBas.
  END.

  hQuery:GET-NEXT().
END. /* BLOKKEN */

IF NOT CAN-FIND(FIRST ttMailLogg) THEN 
  ocReturn = '** Ingen kampanje aktiv på markerte artikler.'.

DELETE OBJECT hQuery NO-ERROR.
EMPTY TEMP-TABLE ttMailLogg.

obOk = TRIM(ocReturn) = ''.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

RETURN ocReturn.



/* **********************  Internal Procedures  *********************** */
