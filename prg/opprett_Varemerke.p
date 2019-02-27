/* opprett_Varemerke.p */

DEF INPUT PARAMETER cVaremerke AS CHAR NO-UNDO.

DEF BUFFER bufVaremerke FOR Varemerke.

IF NOT CAN-FIND(FIRST Varemerke NO-LOCK WHERE 
  Varemerke.Beskrivelse = cVaremerke) THEN 
DO TRANSACTION:
    FIND LAST bufVaremerke NO-LOCK NO-ERROR.
    CREATE Varemerke.
    ASSIGN
        Varemerke.VmId        = IF AVAILABLE bufVaremerke THEN bufVaremerke.VmId + 1 ELSE 1
        Varemerke.Beskrivelse = cVaremerke.
END.
    
