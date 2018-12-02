DEF INPUT  PARAM hBuffer     AS HANDLE NO-UNDO.
DEF INPUT  PARAM icFields    AS CHAR NO-UNDO.
DEF INPUT  PARAM icValues    AS CHAR NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.

DEF VAR iLinjenr  AS INT NO-UNDO.

DEF BUFFER bKOrdreLinje FOR KOrdreLinje.
FOR EACH  bKOrdreLinje
    WHERE bKOrdreLinje.KOrdre_Id    = DEC(DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"KOrdre_Id"))
     AND bKOrdreLinje.KOrdreLinjeNr < 999
      BY bKOrdreLinje.KOrdreLinjeNr 
    DESC:
  iLinjenr = bKOrdreLinje.KOrdreLinjeNr.
  LEAVE.
END.

hBuffer:BUFFER-FIELD("KOrdreLinjeNr"):BUFFER-VALUE = iLinjenr + 1.

