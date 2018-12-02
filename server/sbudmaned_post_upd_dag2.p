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

DEFINE VARIABLE lSum1 AS DECIMAL NO-UNDO.

FIND SBudHode WHERE 
  SBudHode.SBudId = ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE NO-LOCK NO-ERROR.
DO TRANSACTION:
  FIND SBudManed EXCLUSIVE-LOCK WHERE
    SBudManed.SBudId = ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE AND 
    SBudManed.AArMnd = ihBuffer:BUFFER-FIELD("AarMnd"):BUFFER-VALUE NO-ERROR.
  IF AVAILABLE SBudHode AND AVAILABLE SBudManed THEN
  DO: 
    RUN sjekkDag (OUTPUT lSum1).
    IF lSum1 = 0 THEN 
        RUN getbudDag.
    ELSE 
        RUN oppdaterDag. 
  END.

IF AVAILABLE SBudManed THEN 
  RELEASE SBudManed.
END. /* TRANSACTION */

/* **********************  Internal Procedures  *********************** */

PROCEDURE getbudDag:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH SBudDag EXCLUSIVE-LOCK WHERE 
        SBudDag.SBudId = SBudManed.SBudId AND 
        SBudDag.AarMnd = SBudManed.AarMnd
        BREAK BY SBudDag.SBudId
        BY SBudDag.AarMnd:
            
        FIND SBudMalDag NO-LOCK WHERE 
          SBudMalDag.MalId     = SBudHode.MalId AND 
          SBudMalDag.AarMnd    = SBudDag.AarMnd AND 
          SBudMalDag.AarMndDag = SBudDag.AarMndDag NO-ERROR.
              
        IF AVAILABLE SBudMalDag THEN 
        ASSIGN
            SBudDag.SalgBudsjett = ROUND((SBudManed.SalgBudsjett * SBudMalDag.Prosent) / 100,2)
            SBudDag.SalgProsent  = SBudMalDag.Prosent
            SBudDag.DbProsent    = (IF SBudDag.SalgBudsjett = 0 THEN 0 ELSE SBudManed.DbProsent)
            SBudDag.DbBudsjett   = ROUND((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100,2)
            . 
    END.


END PROCEDURE.

PROCEDURE oppdaterDag:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  
    FOR EACH SBudDag EXCLUSIVE-LOCK WHERE 
      SBudDag.SBudId = SBudManed.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd
      BREAK BY SBudDag.SBudId
            BY SBudDag.AarMnd:
      ASSIGN
          SBudDag.SalgBudsjett = ROUND((SBudManed.SalgBudsjett * SBudDag.SalgProsent) / 100,2)
          SBudDag.DbProsent    = (IF SBudDag.SalgBudsjett = 0 THEN 0 ELSE SBudManed.DbProsent)
          SBudDag.DbBudsjett   = ROUND((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100,2)
          . 
    END.

END PROCEDURE.

PROCEDURE sjekkDag:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER plSumDag AS DECIMAL NO-UNDO.
  
    FOR EACH SBudDag NO-LOCK WHERE 
        SBudDag.SBudId = SBudManed.SBudId AND 
        SBudDag.AarMnd = SBudManed.AarMnd
        BREAK BY SBudDag.SBudId
        BY SBudDag.AarMnd:
        ASSIGN
            plSumDag = plSumDag + SBudDag.SalgBudsjett
            . 
    END.

END PROCEDURE.
