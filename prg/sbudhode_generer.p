&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : SBudHode_generer.p
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
         HEIGHT             = 9.57
         WIDTH              = 41.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

FIND SBudHode NO-LOCK WHERE SBudHode.SBudId = piSBudId NO-ERROR.
IF AVAILABLE SBudHode THEN
  RUN NullBudsjett.
ELSE
  DO TRANSACTION:
      CREATE SBudHode.
      ASSIGN SBudHode.SBudId          = piSBudId
             SBudHode.MalID           = piMalId
             SBudHode.ButikkNr        = piButikkNr
             SBudHode.Aar             = piAar
             SBudHode.SBudBeskrivelse = "Test 1"
             SBudHode.SBudNotat       = "Test 1"
             SBudHode.SalgBudsjett    = pdSalgBudsjett
             SBudHode.DbBudsjett      = pdDbBudsjett
              .
      FIND CURRENT SBudHode NO-LOCK.
  END. /* TRANSACTION */

/*
MESSAGE 
'piSBudId' piSBudId SKIP
'piMalId' piMalId SKIP
'piButikkNr' piButikkNr SKIP
'piAar' piAar SKIP 
'pdSalgBudsjett' pdSalgBudsjett SKIP
'pdDbBudsjett' pdDbBudsjett SKIP
VIEW-AS ALERT-BOX.
*/
RUN OpprettPoster.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-NullBudsjett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullBudsjett Procedure 
PROCEDURE NullBudsjett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH SBudManed EXCLUSIVE-LOCK WHERE SBudManed.SBudId = piSBudId:
    ASSIGN
    SBudManed.SalgBudsjett = 0
    SBudManed.DbBudsjett   = 0
    SBudManed.SalgProsent  = 0.
    DELETE SBudManed.
END.
FOR EACH SBudDag EXCLUSIVE-LOCK WHERE SBudDag.SBudId = piSBudId:
    ASSIGN
    SBudDag.SalgBudsjett = 0
    SBudDag.DbBudsjett   = 0
    SBudDag.SalgProsent  = 0.
    DELETE SBudDag.
END.
DO TRANSACTION:
    FIND CURRENT SBudHode EXCLUSIVE-LOCK.
    ASSIGN
      SBudHode.MalId = piMalId.
    FIND CURRENT SBudHode NO-LOCK.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettPoster) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPoster Procedure 
PROCEDURE OpprettPoster :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

ASSIGN
  pdDbBudsjett = 0.

/* Månedsbudsjett */
FOR EACH SBudMalManed NO-LOCK WHERE 
  SBudMalManed.MalId = piMalId:
  FIND SBudManed WHERE 
    SBudManed.SBudId = piSBudId AND 
    SBudManed.AarMnd = SBudMalManed.AarMnd NO-ERROR.
  IF NOT AVAILABLE SBudManed THEN 
  DO:
      CREATE SBudManed.
      ASSIGN
          SBudManed.SBudId = piSBudId  
          SBudManed.AarMnd = INT(STRING(SBudHode.Aar,'9999') + SUBSTRING(STRING(SBudMalManed.AarMnd),5))
          .
  END.
  ASSIGN
      SBudManed.SalgBudsjett = (SBudHode.SalgBudsjett / 100) * SBudMalManed.Prosent
      SBudManed.SalgBudsjett = IF SBudManed.SalgBudsjett = ? THEN 0 ELSE SBudManed.SalgBudsjett 
      SBudManed.SalgProsent  = SBudMalManed.Prosent
      SBudManed.DbBudsjett   = 0
      .
    /* Dagbudsjett for måned */  
    MANED:       
    FOR EACH SBudMalDag NO-LOCK WHERE 
      SBudMalDag.MalId  = piMalId AND 
      SBudMalDag.AarMnd = SBudMalManed.AarMnd:
      FIND SBudDag WHERE 
        SBudDag.SBudId    = piSBudId AND 
        SBudDag.AarMnd    = INT(STRING(SBudHode.Aar,'9999') + SUBSTRING(STRING(SBudMalDag.AarMnd),5)) AND  
        SBudDag.AarMndDag = INT(STRING(SBudHode.Aar,'9999') + SUBSTRING(STRING(SBudMalDag.AarMndDag),5))  
        NO-ERROR.
      IF NOT AVAILABLE SBudDag THEN 
      DO:
          CREATE SBudDag.
          ASSIGN
              SBudDag.SBudId    = piSBudId  
              SBudDag.AarMnd    = INT(STRING(SBudHode.Aar,'9999') + SUBSTRING(STRING(SBudMalDag.AarMnd),5))
              SBudDag.AarMndDag = INT(STRING(SBudHode.Aar,'9999') + SUBSTRING(STRING(SBudMalDag.AarMndDag),5))
              .
      END.
      ASSIGN
          SBudDag.SalgBudsjett = (SBudManed.SalgBudsjett / 100) * SBudMalDag.Prosent
          SBudDag.SalgBudsjett = IF SBudDag.SalgBudsjett = ? THEN 0 ELSE SBudDag.SalgBudsjett 
          
          SBudDag.DbBudsjett   = (DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudDag.SalgBudsjett,0) * SBudMalDag.DbProsent) / 100
/*           SBudDag.DbBudsjett   = (SBudDag.SalgBudsjett * SBudMalDag.DbProsent) / 100 Ändra 160216 - ken1*/
          SBudDag.DbBudsjett   = IF SBudDag.DbBudsjett = ? THEN 0 ELSE SBudDag.DbBudsjett 
          SBudDag.SalgProsent  = SBudMalDag.Prosent
          SBudDag.DbProsent    = SBudMalDag.DbProsent
          SBudManed.DbBudsjett = SBudManed.DbBudsjett + SBudDag.DbBudsjett
          
          .    
    END. /* MANED */
    
    ASSIGN
        SBudManed.DbProsent = ROUND((SBudManed.DbBudsjett / DYNAMIC-FUNCTION("calkNettoFraBruttoOms",SBudManed.SalgBudsjett,0)) * 100,2) 
/*       SBudManed.DbProsent = ROUND((SBudManed.DbBudsjett / SBudManed.SalgBudsjett) * 100,2) Ändrad 160226 - ken1 */
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

