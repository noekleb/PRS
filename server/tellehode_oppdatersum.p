USING Progress.Lang.*.

DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE rTelleListe AS cls.Telling.Telleliste NO-UNDO.

ASSIGN 
    cLogg = 'tellehode_oppdatersum'
    .

rTelleListe  = NEW cls.Telling.TelleListe( INPUT cLogg ).

FIND TelleHode NO-LOCK WHERE ROWID(TelleHode) = TO-ROWID(ENTRY(1,icParam,'|')) NO-ERROR.

IF AVAIL TelleHode THEN
DO:
    obOk = rTelleListe:summerTelling(TelleHode.TelleNr).
END.    
    
    

