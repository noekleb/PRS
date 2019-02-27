DEF INPUT  PARAM icVgRowid    AS CHAR NO-UNDO.
DEF INPUT  PARAM icFields     AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues     AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR hVGbuffer AS HANDLE NO-UNDO.
DEF VAR bOK       AS LOG    NO-UNDO.
DEF VAR cVgBeskr  AS CHAR   NO-UNDO.
DEF VAR iVg       AS INT    NO-UNDO.
DEF VAR bTillatLokalePriser AS LOG NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

ASSIGN hVGbuffer = BUFFER VarGr:HANDLE
       cVgBeskr  = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"VgBeskr")
       iVg       = INT(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"Vg"))
       cTekst    = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"TillatLokalePriser")
       .

IF CAN-DO('yes,true',cTekst) THEN 
  bTillatLokalePriser = TRUE.
ELSE 
  bTillatLokalePriser = FALSE.
  
bOK = hVGbuffer:FIND-BY-ROWID(TO-ROWID(icVgRowid),NO-LOCK,NO-WAIT) NO-ERROR.

/*
IF bOk AND hVGbuffer:BUFFER-FIELD("VgBeskr"):BUFFER-VALUE NE cVgBeskr THEN DO:
  FOR EACH VareBokLinje EXCLUSIVE-LOCK 
      WHERE VareBokLinje.Vg = iVg:
    VareBokLinje.VgBeskr = cVgBeskr.
  END.
  FOR EACH VareBehLinje EXCLUSIVE-LOCK 
      WHERE VareBehLinje.Vg = iVg:
    VareBehLinje.VgBeskr = cVgBeskr.
  END.
END.
*/

FIND VarGr NO-LOCK WHERE
    VarGr.Vg = iVg NO-ERROR.
/* Oppdaterer alle artikler med endret HG. */
IF AVAILABLE VarGr THEN
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
    ArtBas.Vg = iVg:
    
    IF ArtBas.Hg <> VarGr.Hg THEN
        ASSIGN
        ArtBas.Hg = VarGr.hg
        .
    ASSIGN
        Artbas.LokPris = bTillatLokalePriser. 
END.

