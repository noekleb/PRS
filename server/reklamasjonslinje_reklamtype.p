DEF INPUT  PARAM irReklamasjonslinje AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId         AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn            AS CHAR NO-UNDO.

FOR FIRST Reklamasjonslinje FIELDS() NO-LOCK 
    WHERE ROWID(Reklamasjonslinje) = irReklamasjonslinje:
    
    FIND FIRST SysPara NO-LOCK WHERE 
        SysPara.sysHId = 15 AND 
        sysPara.sysGr  = 12 AND
        sysPara.ParaNr = Reklamasjonslinje.TTId NO-ERROR.
    IF AVAILABLE SysPara THEN
        ocReturn = SysPara.Parameter1.
    ELSE
        ocReturn = "".
END.
