DEF INPUT  PARAM irReklamasjonslogg AS ROWID NO-UNDO.
DEF INPUT  PARAM icSessionId        AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn           AS CHAR NO-UNDO.

FOR FIRST Reklamasjonslogg FIELDS() NO-LOCK 
    WHERE ROWID(Reklamasjonslogg) = irReklamasjonslogg:
    
    FIND FIRST SysPara NO-LOCK WHERE 
        SysPara.sysHId = 15 AND 
        sysPara.sysGr  = 1 AND
        sysPara.ParaNr = Reklamasjonslogg.ReklamStatus NO-ERROR.
    IF AVAILABLE SysPara THEN
        ocReturn = SysPara.Parameter1.
    ELSE
        ocReturn = "".
END.
