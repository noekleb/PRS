/* kopierelogg.i */

DEF VAR cFeltLst AS CHAR NO-UNDO.
DEF VAR piLoop   AS INT  NO-UNDO.
DEF VAR iTst     AS INT  NO-UNDO.
DEFINE VARIABLE cWebLager AS CHARACTER NO-UNDO.

{syspara.i 150 1 3 cWebLager}

ASSIGN
    cFeltLst = "{1}". 

DEFINE BUFFER bElogg   FOR Elogg.
DEFINE BUFFER erpELogg FOR Elogg.

DO piLoop = 1 TO NUM-ENTRIES(cFeltLst): 

    /* NB: Her skal ALLTID stå WebBut, da det er det db triggerne bruker */
    LOOPEN:
    FOR EACH ELogg WHERE ELogg.TabellNavn     = entry(piLoop,cFeltLst) AND
        ELogg.EksterntSystem = "WEBBUT" /*AND 
                             Elogg.EndringsType   = 1      */ NO-LOCK:
        
        IF ENTRY(piLoop,cFeltLst) = 'Lager' THEN 
            DO:
                IF NUM-ENTRIES(ELogg.Verdier,CHR(1)) < 2 THEN 
                    NEXT LOOPEN.
                IF ENTRY(2,ELogg.Verdier,CHR(1)) <> cWebLager THEN
                DO: 
                    FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
                    IF AVAILABLE bELogg THEN DELETE bELogg.
                    NEXT LOOPEN.
                END.
            END. 
                                 
        FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
        IF NOT AVAIL bElogg THEN
            NEXT.
        BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
        DELETE bELogg.
        IF AVAILABLE TT_Elogg THEN
            RELEASE TT_ELogg.
    END. /* LOOPEN */
END.



