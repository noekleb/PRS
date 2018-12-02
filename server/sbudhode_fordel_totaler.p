&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sbudhode_fordel_totaler.p
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN    
    DEFINE VARIABLE piSBudId       AS INTEGER INIT 1    NO-UNDO.
    DEFINE VARIABLE piMalId        AS INTEGER INIT 1    NO-UNDO.
    DEFINE VARIABLE piButikkNr     AS INTEGER INIT 4    NO-UNDO.
    DEFINE VARIABLE piAar          AS INTEGER INIT 2012 NO-UNDO.
    DEFINE VARIABLE pdSalgBudsjett AS DECIMAL NO-UNDO.
    DEFINE VARIABLE pdDbBudsjett   AS DECIMAL NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER piSBudId       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piMalId        AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piButikkNr     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piAar          AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER pdSalgBudsjett AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER pdDbBudsjett   AS DECIMAL NO-UNDO.
&ENDIF 
   
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/*MESSAGE 'Program: sbudhode_fordel_totaler.p'*/
/*VIEW-AS ALERT-BOX.                          */

FIND SBudHode NO-LOCK WHERE SBudHode.SBudId = piSBudId NO-ERROR.
IF NOT AVAILABLE SBudHode THEN
  RETURN.

RUN OppdaterPoster.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OppdaterPoster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterPoster Procedure 
PROCEDURE OppdaterPoster :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

ASSIGN
  pdDbBudsjett = 0.

/* Månedsbudsjett */
FOR EACH SBudManed EXCLUSIVE-LOCK WHERE 
  SBudManed.SBudId = SBudHode.SBudId:
  ASSIGN
      SBudManed.SalgBudsjett = (SBudHode.SalgBudsjett / 100) * SBudManed.SalgProsent
      SBudManed.SalgBudsjett = IF SBudManed.SalgBudsjett = ? THEN 0 ELSE SBudManed.SalgBudsjett 
      SBudManed.DbBudsjett   = 0
      .
    /* Dagbudsjett for måned */  
    MANED:       
    FOR EACH SBudDag EXCLUSIVE-LOCK WHERE 
      SBudDag.SBudId = SBudHode.SBudId AND 
      SBudDag.AarMnd = SBudManed.AarMnd:
      ASSIGN
          SBudDag.SalgBudsjett = (SBudManed.SalgBudsjett / 100) * SBudDag.SalgProsent
          SBudDag.SalgBudsjett = IF SBudDag.SalgBudsjett = ? THEN 0 ELSE SBudDag.SalgBudsjett 
          
          SBudDag.DbBudsjett   = (DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudDag.DbProsent) / 100
/*           SBudDag.DbBudsjett   = (SBudDag.SalgBudsjett * SBudDag.DbProsent) / 100   Ändrad 160226 - ken1 */

          SBudDag.DbBudsjett   = IF SBudDag.DbBudsjett = ? THEN 0 ELSE SBudDag.DbBudsjett 
          SBudManed.DbBudsjett = SBudManed.DbBudsjett + SBudDag.DbBudsjett
          
          .    
    END. /* MANED */
    
    ASSIGN
        SBudManed.DbProsent = ROUND((SBudManed.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0)) * 100,2) 
/*       SBudManed.DbProsent = ROUND((SBudManed.DbBudsjett / SBudManed.SalgBudsjett) * 100,2)  Ändrad 160226 - ken1 */
      SBudManed.DbProsent = IF SBudManed.DbProsent = ? THEN 0 ELSE SBudManed.DbProsent
      pdDbBudsjett        = pdDbBudsjett + SBudManed.DbBudsjett      
    .
    
END. /* Månedsbudsjett */

DO TRANSACTION:
  FIND CURRENT SBudHode EXCLUSIVE-LOCK.
  ASSIGN
    SBudHode.DbBudsjett = pdDbBudsjett.
  FIND CURRENT SBudHode NO-LOCK.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

