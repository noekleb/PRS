/* Gjør FIND på artbas der beskr er involvert.
   Dette må gjøres statis for å håndtere ' og ""
   
   Parameter: <feltnavn>|<verdi>¤<feltnavn>|<verdi>¤..
   
   Opprettet: 23.11.07 av BHa              
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR iLevNr        AS INT  NO-UNDO.
DEF VAR cLevKod       AS CHAR NO-UNDO.
DEF VAR cLevFargKod   AS CHAR NO-UNDO.
DEF VAR cBeskr        AS CHAR NO-UNDO.
DEF VAR iStrTypeId    AS INT  NO-UNDO.
DEF VAR iVg           AS INT  NO-UNDO.
DEF VAR ix            AS INT  NO-UNDO.
DEF VAR cField        AS CHAR NO-UNDO.
DEF VAR cValue        AS CHAR NO-UNDO.

DO ix = 1 TO NUM-ENTRIES(icParam,"¤"):
  ASSIGN cField = ENTRY(1,ENTRY(ix,icParam,"¤"),"|")
         cValue = ENTRY(2,ENTRY(ix,icParam,"¤"),"|").
  CASE cField:
    WHEN "Levnr"      THEN iLevNr      = INT(cValue).
    WHEN "LevKod"     THEN cLevKod     = cValue.
    WHEN "LevFargKod" THEN cLevFargKod = cValue.
    WHEN "Beskr"      THEN cBeskr      = cValue.
    WHEN "StrTypeId"  THEN iStrTypeId  = INT(cValue).
    WHEN "Vg"         THEN iVg         = INT(cValue).
  END CASE.
END.

FIND FIRST ArtBas NO-LOCK
     WHERE (IF iLevNr       NE 0  THEN ArtBas.LevNr       = iLevNr      ELSE TRUE)
       AND (IF cLevKod      NE "" THEN ArtBas.LevKod      = cLevKod     ELSE TRUE)
       AND (IF cLevFargKod  NE "" THEN ArtBas.LevFargKod  = cLevFargKod ELSE TRUE)
       AND (IF cBeskr       NE "" THEN ArtBas.Beskr       = cBeskr      ELSE TRUE)
       AND (IF iStrTypeId   NE 0  THEN ArtBas.StrTypeID   = iStrTypeId  ELSE TRUE)
       AND (IF iVg          NE 0  THEN ArtBas.Vg          = iVg         ELSE TRUE)
    NO-ERROR.

IF AVAIL ArtBas THEN
  ASSIGN ocReturn = STRING(ArtBas.ArtikkelNr)
         obOk     = YES.
ELSE ocReturn = ?.

