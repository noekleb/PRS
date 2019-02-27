/* To invoke: 
   DYNAMIC-FUNCTION("setAttribute",hFieldMap | hBrowse,"PostUpdateProc","<procedure>").
   If there's no fieldmap (viewer) set the attribute on the browse object

-- To get the fields used in current update (comma-separated list): --
DEF VAR cFields AS CHAR NO-UNDO.
cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).

-- To get the corrensponding list of values used in current update (pipe-separated list): --
DEF VAR cValues AS CHAR NO-UNDO.
cValues = DYNAMIC-FUNCTION("getCurrentValues" IN SOURCE-PROCEDURE).

-- To get a spesific value from current update: --
cValues = DYNAMIC-FUNCTION("getValue" IN SOURCE-PROCEDURE,"<field name>").

-----------------------------------------------------------------------------------------*/   
DEF INPUT  PARAM ihBuffer       AS HANDLE NO-UNDO.  /* Handle to current buffer */
DEF INPUT  PARAM icAction       AS CHAR   NO-UNDO.  /* Create or Update */
DEF INPUT  PARAM icSessionId    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocValue        AS CHAR   NO-UNDO.  /* Error message. If <> blank the transaction is backed out */


/* To get the fields used in current update (comma-separated list): */
DEF VAR cFields AS CHAR NO-UNDO.
cFields = DYNAMIC-FUNCTION("getCurrentValueFields" IN SOURCE-PROCEDURE).

FIND SBudDag EXCLUSIVE-LOCK WHERE
  SBudDag.SBudId    = ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE AND 
  SBudDag.AarMnd    = ihBuffer:BUFFER-FIELD("AarMnd"):BUFFER-VALUE AND  
  SBudDag.AarMndDag = ihBuffer:BUFFER-FIELD("AarMndDag"):BUFFER-VALUE  
  NO-ERROR.
FIND SBudManed EXCLUSIVE-LOCK WHERE
  SBudManed.SBudId = ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE AND 
  SBudManed.AArMnd = ihBuffer:BUFFER-FIELD("AarMnd"):BUFFER-VALUE NO-ERROR.
IF AVAILABLE SBudManed AND AVAILABLE SBudDag THEN 
DO:
    CASE cFields:
        WHEN 'SalgBudsjett' THEN 
        DO:
            ASSIGN 
            SBudDag.SalgProsent = ROUND(((SBudDag.SalgBudsjett / SBudManed.SalgBudsjett) * 100),2)
            SBudDag.DbBudsjett  = ROUND(((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100),2)
            SBudDag.DbProsent   = ROUND(((SBudDag.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0)) * 100),2)
            SBudDag.DbProsent   = IF SBudDag.DbProsent = ? THEN 0 ELSE SBudDag.DbProsent.
        END.
        WHEN 'SalgProsent' THEN 
        DO:
            ASSIGN 
              SBudDag.SalgBudsjett = ROUND(((SBudManed.SalgBudsjett * SBudDag.SalgProsent) / 100),2)
              SBudDag.DbBudsjett   = ROUND(((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100),2)
              SBudDag.DbProsent   = ROUND(((SBudDag.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0)) * 100),2).
              SBudDag.DbProsent   = IF SBudDag.DbProsent = ? THEN 0 ELSE SBudDag.DbProsent.
        END.
        WHEN 'DbBudsjett' THEN 
        DO:
            SBudDag.DbProsent = ROUND(((SBudDag.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0)) * 100),2).
        END.
        
        WHEN 'DbProsent' THEN 
        DO:
            SBudDag.DbBudsjett = ROUND(((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100),2).
        END.
    END CASE.
    
END.
