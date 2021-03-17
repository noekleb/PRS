/* pksdl_artikkelPksdl.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cArtikkelListe AS CHARACTER NO-UNDO. 

{ ttArtikkelliste.i }
   
ASSIGN 
  cArtikkelListe = ENTRY(1,icParam,'|')
  obOK        = YES
  ocReturn    = ""
  . 

/* Henter lagerliste med pakkseddel info som er skrevet til fil fra pksdl_Lagerliste.p, legger den opp i tmp tabell og returnerer den. */
IF SEARCH(cArtikkelListe) <> ? THEN 
  TEMP-TABLE ttArtikkelListe:READ-JSON('file',cArtikkelListe,'EMPTY').

ihBuffer:COPY-TEMP-TABLE (BUFFER ttArtikkelliste:HANDLE,NO,NO,YES).

/* **********************  Internal Procedures  *********************** */
 