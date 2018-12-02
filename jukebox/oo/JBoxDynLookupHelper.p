/* Helper for jboxdynlookup.cls */

DEF INPUT PARAM ihSourceProc       AS HANDLE NO-UNDO.
DEF INPUT PARAM icBuffersAndFields AS CHAR NO-UNDO.
DEF INPUT PARAM icQueryJoin        AS CHAR NO-UNDO.
DEF INPUT PARAM icReturnFields     AS CHAR NO-UNDO.
DEF INPUT PARAM ibMultiSelect      AS LOG  NO-UNDO.

DEF VAR cReturnValues AS CHAR   NO-UNDO.
DEF VAR hBrowse       AS HANDLE NO-UNDO.
DEF VAR hToolbarRect  AS HANDLE NO-UNDO.
DEF VAR bOk           AS LOG    NO-UNDO.

ihSourceProc:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN JBoxLookup.w (THIS-PROCEDURE,200,
                  "Sak"
                  + ";cSaksnr"
                  + ";cSaksTittel"
                + ",Person"
                  + ";cNavn"
                 ,"WHERE false,FIRST Person WHERE Person.iPersonId = Sak.iPersSaksansvar"
                  ,IF ibMultiSelect THEN "multiple" ELSE ""                                                  
                  ,"cSaksnr",   /* <- return values for these fields */
                  OUTPUT cReturnValues,
                  OUTPUT bOK).
ihSourceProc:CURRENT-WINDOW:SENSITIVE = TRUE.

PROCEDURE setLookupAttributes:
  DEF INPUT PARAM ihBrowse      AS HANDLE NO-UNDO.
  DEF INPUT PARAM ihToolbarRect AS HANDLE NO-UNDO.
  DEF INPUT PARAM ihOther       AS HANDLE NO-UNDO.

  ASSIGN hBrowse      = ihBrowse
         hToolbarRect = ihToolbarRect
         .
END PROCEDURE.

PROCEDURE getLookupAttributes:
  DEF OUTPUT PARAM ohBrowse      AS HANDLE NO-UNDO.
  DEF OUTPUT PARAM ohToolbarRect AS HANDLE NO-UNDO.

  ASSIGN ohBrowse      = hBrowse
         ohToolbarRect = hToolbarRect
         .
END PROCEDURE.

FUNCTION getReturnValues RETURNS CHARACTER():
  RETURN cReturnValues.
END FUNCTION.

FUNCTION getIsSelectionMade RETURNS LOGICAL():
  RETURN bOk.
END FUNCTION.
