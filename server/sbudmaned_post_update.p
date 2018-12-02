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

/*MESSAGE 'Program: sbudmaned_post_update.p'*/
/*VIEW-AS ALERT-BOX.                        */

FIND SBudHode WHERE 
  SBudHode.SBudId = ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE NO-LOCK NO-ERROR.
DO TRANSACTION:
FIND SBudManed EXCLUSIVE-LOCK WHERE
  SBudManed.SBudId = ihBuffer:BUFFER-FIELD("SBudId"):BUFFER-VALUE AND 
  SBudManed.AArMnd = ihBuffer:BUFFER-FIELD("AarMnd"):BUFFER-VALUE NO-ERROR.
IF AVAILABLE SBudHode AND AVAILABLE SBudManed THEN 
DO:
    CASE cFields:
        WHEN 'SalgBudsjett' THEN 
        DO:
            SBudManed.SalgProsent = ROUND(((SBudManed.SalgBudsjett / SBudHode.SalgBudsjett) * 100),2).
            SBudManed.DbBudsjett = ROUND(((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0) * SBudManed.DbProsent) / 100),2).
            /*RUN oppdaterDbKrManed.*/
        END.
        WHEN 'SalgProsent' THEN 
        DO:
            ASSIGN 
              SBudManed.SalgBudsjett = ROUND(((SBudHode.SalgBudsjett * SBudManed.SalgProsent) / 100),2)
              SBudManed.DbBudsjett   = ROUND(((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0) * SBudManed.DbProsent) / 100),2).
              /*RUN oppdaterDbKrManed.*/
              .
        END.
        WHEN 'DbBudsjett' THEN 
        DO:
            SBudManed.DbProsent = ROUND(((SBudManed.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0)) * 100),2).
/*             SBudManed.DbProsent = ROUND(((SBudManed.DbBudsjett / SBudManed.SalgBudsjett) * 100),2). Rad ovan ändrad 160229 - ken1 */
            /*RUN oppdaterDbKrManed.*/
        END.
        
        WHEN 'DbProsent' THEN 
        DO:
            SBudManed.DbBudsjett = ROUND(((DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0) * SBudManed.DbProsent) / 100),2).
            /*RUN oppdaterDb%Maned.*/
        END.
    END CASE.
END.

/*
IF NUM-ENTRIES(cFields) = 3 THEN 
    RUN oppdaterDbKrManed.
*/

IF AVAILABLE SBudManed THEN 
  RELEASE SBudManed.
END. /* TRANSACTION */

/* **********************  Internal Procedures  *********************** */
/*
PROCEDURE oppdaterDb%Maned:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    ASSIGN
      SBudManed.DbBudsjett = 0.
    FOR EACH SBudDag EXCLUSIVE-LOCK WHERE 
      SBudDag.SBudId = SBudManed.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd
      BREAK BY SBudDag.SBudId
            BY SBudDag.AarMnd:
      ASSIGN
          SBudDag.DbProsent    = SBudManed.DbProsent
          SBudDag.DbBudsjett   = (DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100
          SBudDag.DbBudsjett   = IF SBudDag.DbBudsjett = ? THEN 0 ELSE SBudDag.DbBudsjett
          SBudDag.DbProsent    = IF SBudDag.Salgbudsjett = 0 THEN 0 ELSE SbudDag.DbProsent
          SBudManed.DbBudsjett = SBudManed.DbBudsjett + SBudDag.DbBudsjett 
          . 
    END.

    ASSIGN
        SBudManed.DbProsent = ROUND((SBudManed.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0)) * 100,2) 
        SBudManed.DbProsent = IF SBudManed.DbProsent = ? THEN 0 ELSE SBudManed.DbProsent
        .
        
END PROCEDURE.

PROCEDURE oppdaterDbKrManed:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSumSalg AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lSumDb   AS DECIMAL NO-UNDO.
  
    ASSIGN
      SBudManed.DbBudsjett = 0.
    
    FOR EACH SBudDag EXCLUSIVE-LOCK WHERE 
      SBudDag.SBudId = SBudManed.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd
      BREAK BY SBudDag.SBudId
            BY SBudDag.AarMnd:
      ASSIGN
          SBudDag.DbProsent    = SBudManed.DbProsent
          SBudDag.DbBudsjett   = (DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100
          SBudDag.DbBudsjett   = IF SBudDag.DbBudsjett = ? THEN 0 ELSE SBudDag.DbBudsjett
          SBudDag.DbProsent    = IF SBudDag.Salgbudsjett = 0 THEN 0 ELSE SbudDag.DbProsent
          SBudManed.DbBudsjett = SBudManed.DbBudsjett + SBudDag.DbBudsjett 
          . 
      /* Kjøres oppdater måned, må oms% regnes om pr. dag. */
      IF NUM-ENTRIES(cFields) = 3 THEN 
          SBudDag.SalgProsent = ROUND(((SBudDag.SalgBudsjett / SBudManed.SalgBudsjett) * 100),2).
    END.

    ASSIGN
        SBudManed.DbProsent = ROUND((SBudManed.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0)) * 100,2) 
        SBudManed.DbProsent = IF SBudManed.DbProsent = ? THEN 0 ELSE SBudManed.DbProsent
        .
END PROCEDURE.
*/
