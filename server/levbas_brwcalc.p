/* In order to speed up processing of calculated fields it can be crucial
   to place the procedures in a single persistent procedure that is loaded
   once before processing the records.
   To invoke:
   
   DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","orderline_browsecalc.p").
   
   Usage in browse (the name must not end on .p) :
   
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total|Total"
   
   (vs, when calling a .p each time - : 
   + ";+LineTotal|DECIMAL|->><>>><>>9.99|orderline_total.p|Total"
   )
   
   The CleanUp procedure will be executed (if it exists) before the this procedure is deleted.
------------------------------------------------------------------------*/   
DEF VAR hQuery AS HANDLE NO-UNDO.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER ArtBas:HANDLE).
hQuery:FORWARD-ONLY = YES.

PROCEDURE levbas_antart:
  DEF INPUT PARAM  iiLevNr      AS INT  NO-UNDO.
  DEF INPUT PARAM  icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

  DEF VAR ix AS INT NO-UNDO.

  hQuery:QUERY-CLOSE() NO-ERROR.

  hQuery:QUERY-PREPARE("FOR EACH ArtBas NO-LOCK WHERE LevNr = " + STRING(iiLevNr)).
  hQuery:QUERY-OPEN().
  REPEAT:
    hQuery:GET-NEXT().
    IF hQuery:QUERY-OFF-END THEN LEAVE.
    ELSE ix = ix + 1.
  END.

  ocReturn = STRING(ix).  

END PROCEDURE.

PROCEDURE CleanUp:
  DELETE OBJECT hQuery NO-ERROR.
END PROCEDURE.
