/* pksdl_lagerPksdl.p

DEFINE TEMP-TABLE ttLagerListe NO-UNDO
  FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"  
  FIELD Varetekst AS CHARACTER FORMAT "x(40)"
  FIELD LevKod AS CHARACTER FORMAT "x(20)"
  FIELD LevFargKod AS CHARACTER FORMAT "x(20)"
  FIELD MainGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD ArtGroup AS INTEGER FORMAT ">>>>>>>9"
  FIELD InnkjopsPris AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntPkSdl AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiPkSdl AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VVarekostL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntL10 AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiL10 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD VVarekostL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD AntL40 AS INTEGER FORMAT "->>>>>>>9"
  FIELD VerdiL40 AS DECIMAL FORMAT "->>>>>>>>>9.99"
  FIELD TotAnt AS INTEGER  FORMAT "->>>>>>>9"
  FIELD TotVerdi AS DECIMAL FORMAT "->>>>>>>>>9.99"
  INDEX idxArtikkelNr AS UNIQUE PRIMARY ArtikkelNr
  INDEX idxGant LevKod LevFargKod

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLagerListe AS CHARACTER NO-UNDO.

{ ttLagerliste.i }
   
ASSIGN 
  cLagerListe = ENTRY(1,icParam,'|')
  obOK        = YES
  ocReturn    = ""
  .


/* Henter lagerliste med pakkseddel info som er skrevet til fil fra pksdl_Lagerliste.p, legger den opp i tmp tabell og returnerer den. */
IF SEARCH(cLagerListe) <> ? THEN 
  TEMP-TABLE ttLagerListe:READ-JSON('file',cLagerListe,'EMPTY').

ihBuffer:COPY-TEMP-TABLE (BUFFER ttLagerliste:HANDLE,NO,NO,YES).

/* **********************  Internal Procedures  *********************** */
