&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sbudmalhode_generer.p 
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
    DEFINE VARIABLE piMalId    AS INTEGER INIT 1    NO-UNDO.
    DEFINE VARIABLE piButikkNr AS INTEGER INIT 4    NO-UNDO.
    DEFINE VARIABLE piStatButikkNr AS INTEGER INIT 4 NO-UNDO.
    DEFINE VARIABLE piInAar    AS INTEGER INIT 2012 NO-UNDO.
    DEFINE VARIABLE piHAar     AS INTEGER INIT 2011 NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER piMalId     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piButikkNr  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piStatButikkNr  AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piInAar     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER piHAar      AS INTEGER NO-UNDO.
&ENDIF 
   
DEFINE VARIABLE cInAar     AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE dTotAr     AS DECIMAL FORMAT ">>>,>>>,>>9.99"           NO-UNDO.
DEFINE VARIABLE dTotMan    AS DECIMAL EXTENT 12 FORMAT ">>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dManKost   AS DECIMAL EXTENT 12 FORMAT ">>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dProc      AS DECIMAL EXTENT 12                         NO-UNDO.
DEFINE VARIABLE dDbProc    AS DECIMAL EXTENT 12                         NO-UNDO.
DEFINE VARIABLE dTotProc   AS DECIMAL                                   NO-UNDO.
DEFINE VARIABLE iX         AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE iX2        AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE iX3        AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE dTotAr2    AS DECIMAL FORMAT ">>>,>>>,>>9.99"           NO-UNDO.
DEFINE VARIABLE datberv    AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE datum1     AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE datum2     AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE antdag     AS DECIMAL                                   NO-UNDO.
DEFINE VARIABLE lSvar      AS LOGICAL                                   NO-UNDO.
DEFINE VARIABLE iSkottDag  AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE iSkottDag2 AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE cMtab      AS CHARACTER EXTENT 12                       NO-UNDO.
DEFINE VARIABLE iFrDag     AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE iToDag     AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE iAntDar    AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE dTotDag    AS DECIMAL EXTENT 31 FORMAT ">>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dKostDag   AS DECIMAL EXTENT 31 FORMAT ">>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE dProcDag   AS DECIMAL EXTENT 31 FORMAT "->>9.99"        NO-UNDO.
DEFINE VARIABLE dDbProcDag AS DECIMAL EXTENT 31 FORMAT "->>9.99"        NO-UNDO.
DEFINE VARIABLE iAarMnd    AS INTEGER FORMAT "999999"                   NO-UNDO.
DEFINE VARIABLE iAarMndDag AS INTEGER FORMAT "99999999"                 NO-UNDO.
DEFINE VARIABLE cAarMnd    AS CHARACTER                                 NO-UNDO.
DEFINE VARIABLE dTestProc  AS DECIMAL                                   NO-UNDO.
DEFINE VARIABLE DayNumIAar AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE DaynumHAar AS INTEGER                                   NO-UNDO.
DEFINE VARIABLE TestDate   AS DATE        NO-UNDO.

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

/*MESSAGE 
    'piMalId' piMalId SKIP     
    'piButikkNr' piButikkNr SKIP  
    'piInAar' piInAar SKIP    
    'piHAar' piHAar     
VIEW-AS ALERT-BOX.*/


ASSIGN cMtab[1] = "1:31"
       cMtab[2] = "32:59"
       cMtab[3] = "60:90"
       cMtab[4] = "91:120"
       cMtab[5] = "121:151"
       cMtab[6] = "152:181"
       cMtab[7] = "182:212"
       cMtab[8] = "213:243"
       cMtab[9] = "244:273"
       cMtab[10] = "274:304"
       cMtab[11] = "305:334"
       cMtab[12] = "335:365".


ASSIGN cInAar = STRING(piHAar).
ASSIGN datberv = 1.
ASSIGN datum1 = cInAar + "0229".

RUN datumber.p (datberv, datum1, OUTPUT datum2, antdag, OUTPUT lSvar).


IF lSvar = FALSE THEN
    ASSIGN iSkottDag = 0.
ELSE IF lSvar = TRUE THEN
    ASSIGN iSkottDag = 1.
ELSE
    ASSIGN iSkottDag = 0.

FIND SBudMalHode EXCLUSIVE-LOCK WHERE SBudMalHode.MalId = piMalId
                                  AND SBudMalHode.ButikkNr = piButikkNr
                                  AND SBudMalHode.Aar = piInAar NO-ERROR.
IF AVAILABLE SBudMalHode THEN
DO:
  RELEASE SBudMalHode.
  RUN DelMalManODag.
END.
ELSE
  DO:
      CREATE SBudMalHode.
      ASSIGN SBudMalHode.MalId = piMalId
             SBudMalHode.ButikkNr = piButikkNr
             SBudMalHode.Aar = piInAar
             SBudMalHode.MalBeskrivelse = "Test 1"
             SBudMalHode.MalNotat = "Test 1".
  END.

/* Henter total verdi solgt for året fra StLinje. */
RUN GetTotAar.

RUN GetTotMan.
RUN WriteMalMan.
RUN GetTotDag.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-DelMalManODag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelMalManODag Procedure 
PROCEDURE DelMalManODag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH SBudMalManed WHERE SBudMalManed.MalId = piMalId:
    DO:
      ASSIGN cAarMnd = STRING(SBudMalManed.AarMnd).
      ASSIGN cInAar = SUBSTRING(cAarMnd,1,4).
      IF INT(cInAar) = piInAar THEN
        DELETE sBudMalManed.
    END.                      
END.
FOR EACH SBudMalDag WHERE SBudMalDag.MalId = piMalId:
    DO:
      ASSIGN cAarMnd = STRING(SBudMalDag.AarMnd).
      ASSIGN cInAar = SUBSTRING(cAarMnd,1,4).
      IF INT(cInAar) = piInAar THEN
        DELETE sBudMalDag.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTotAar) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTotAar Procedure 
PROCEDURE GetTotAar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH StLinje WHERE 
    StLinje.Butik       = piStatButikkNr AND 
    StLinje.StTypeId    = "BUTSTAT" AND  
    StLinje.PerId       = "AAR" AND 
    StLinje.AarPerLinNr = INTEGER(STRING(piHAar,"9999") + '001')
    NO-LOCK:
    
    ASSIGN dTotAr = StLinje.VerdiSolgt.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTotDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTotDag Procedure 
PROCEDURE GetTotDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cTest   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iAddDar AS INTEGER FORMAT -999    NO-UNDO.
DEFINE VARIABLE iX4     AS INTEGER     NO-UNDO.
ASSIGN cInAar = STRING(piInAar).
ASSIGN datberv = 1.
ASSIGN datum1 = cInAar + "0229".


RUN datumber.p (datberv, datum1, OUTPUT datum2, antdag, OUTPUT lSvar).

IF lSvar = FALSE THEN
    ASSIGN iSkottDag2 = 0.
ELSE IF lSvar = TRUE THEN
    ASSIGN iSkottDag2 = 1.
ELSE
    ASSIGN iSkottDag2 = 0.
/*MESSAGE "SkottDag " iSkottDag SKIP "Skottdag2 " iSkottDag2
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
ASSIGN TestDate = DATE(1,1,piHAar)
       DayNumHAar = WEEKDAY(TestDate).
/*MESSAGE TestDate " - " DayNumHAar
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
ASSIGN TestDate = DATE(1,1,piInAar)
       DayNumIAar = WEEKDAY(TestDate).
/*MESSAGE TestDate " - " DayNumIAar
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

ASSIGN iAddDar = DayNumIAar - DayNumHAar.
IF iAddDar < 0 THEN
   ASSIGN iAddDar = iAddDar + 7.

/*MESSAGE "iAddDar " iAddDar
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

DO iX = 1 TO 12:
    DO iX3 = 1 TO 31:
      ASSIGN dTotDag[iX3] = 0
             dKostDag[iX3] = 0
             dProcDag[iX3] = 0
             dDbProcDag[iX3] = 0.
    END.
    ASSIGN iAarMnd = piInAar * 100 + iX.
    ASSIGN iFrDag = INTEGER(ENTRY(1,cMtab[iX],":"))
           iToDag = INTEGER(ENTRY(2,cMtab[iX],":")).
    IF iX = 2 THEN
        ASSIGN iToDag = iToDag + iSkottDag.
    ELSE IF iX > 2 THEN
        ASSIGN iFrdag = iFrdag + iSkottDag
               iToDag = iToDag + iSkottDag.
    ASSIGN iAntDar = iToDag - iFrDag + 1
           iX3 = 0.
    IF iX = 2 AND iSkottDag = 1 AND iSkottDag2 = 0 THEN
       ASSIGN iAntDar = iAntDar - 1.
    IF iX = 2 AND iSkottDag = 0 AND iSkottDag2 = 1 THEN
       ASSIGN iAntDar = iAntDar + 1.
    IF iX = 3 AND iSkottDag = 0 AND iSkottDag2 = 1 THEN
    DO:
      IF iAddDar = 6 THEN
        ASSIGN iAddDar = 0.
      ELSE
        ASSIGN iAddDar = iAddDar + 1.
    END.
    IF iX = 3 AND iSkottDag = 1 AND iSkottDag2 = 0 THEN
    DO:
      IF iAddDar = 0 THEN
        ASSIGN iAddDar = 6.
      ELSE
        ASSIGN iAddDar = iAddDar - 1.
    END.
    DO iX2 = iFrDag TO iToDag:
        ASSIGN iX4 = iX2 + iAddDar.
        FOR EACH StLinje WHERE 
            StLinje.Butik    = piStatButikkNr AND 
            StLinje.StTypeId = "BUTSTAT" AND 
            StLinje.PerId    = "DAG" AND 
            StLinje.AarPerLinNr = INTEGER(STRING(piHAar,"9999") + STRING(iX4,"999"))
            NO-LOCK:
            
            ASSIGN iX3 = iX3 + 1
                   dTotDag[iX3] = StLinje.VerdiSolgt
                   dKostDag[iX3] = StLinje.VVarekost.
        END.
    END.
    DO iX3 = 1 TO iAntDar:
        ASSIGN dProcDag[iX3] = (dTotDag[iX3] * 100) / dTotMan[iX].
        IF dProcDag[iX3] = ? THEN dProcDag[iX3] = 0.
        ASSIGN dDbProcDag[iX3] = ((dTotDag[iX3] - dKostDag[iX3]) * 100) / dTotDag[iX3].
        IF dDbProcDag[iX3] = ? THEN dDbProcDag[iX3] = 0.
    END.

    ASSIGN dTestProc = 0.
    DO iX3 = 1 TO iAntDar:
      ASSIGN dTestProc = dTestProc + dProcDag[iX3].
    END.
    IF dTestProc = 0 THEN
      DO iX3 = 1 TO iAntDar:
        ASSIGN dProcDag[iX3] = 100 / iAntDar.
        ASSIGN dTestProc = dTestProc + dProcDag[iX3].
      END.
    IF dTestProc = 100 THEN
      ASSIGN dTestProc = 0.
/*    ELSE
    DO:
      ASSIGN dProcDag[iAntDar] = dProcDag[iAntDar] + (dTestProc - 100).
    END.*/

    DO iX3 = 1 TO iAntDar:
       ASSIGN iAarMndDag = iAarMnd * 100 + iX3.
       FIND SBudMalDag EXCLUSIVE-LOCK WHERE SBudMalDag.MalId = piMalId
                                        AND SBudMalDag.AarMnd = iAarMnd 
                                        AND SBudMalDag.AarMndDag = iAarMndDag NO-ERROR.
       IF NOT AVAILABLE SBudMalDag THEN
       DO:
           CREATE SBudMalDag.
           ASSIGN SBudMalDag.MalId = piMalId
                  SBudMalDag.AarMnd = iAarMnd
                  SBudMalDag.AarMndDag = iAarMndDag
                  SBudMalDag.Prosent = dProcDag[iX3]
                  SBudMalDag.DbProsent = dDbProcDag[iX3].
       END.
    END.
    ASSIGN dTestProc = 0.
    FOR EACH SBudMalDag WHERE SBudMalDag.MalId = piMalId
                          AND SBudMalDag.AarMnd = iAarMnd NO-LOCK:
        ASSIGN dTestProc = dTestProc + SBudMalDag.Prosent.
    END.

    IF dTestProc <> 100 THEN
      DO:
        ASSIGN iAarMndDag = iAarMnd * 100 + iAntDar.
        FIND SBudMalDag EXCLUSIVE-LOCK WHERE SBudMalDag.MalId = piMalId
                                         AND SBudMalDag.AarMnd = iAarMnd 
                                         AND SBudMalDag.AarMndDag = iAarMndDag NO-ERROR.
        IF AVAILABLE SBudMalDag THEN
          DO:
            ASSIGN SBudMalDag.Prosent = SBudMalDag.Prosent + (100 - dTestProc).
            RELEASE SBudMalDag.
          END.
      END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetTotMan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetTotMan Procedure 
PROCEDURE GetTotMan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO iX = 1 TO 12:
    FOR EACH StLinje WHERE 
        StLinje.Butik       = piStatButikkNr AND  
        StLinje.StTypeId    = "BUTSTAT" AND 
        StLinje.PerId       = "MANED" AND 
        StLinje.AarPerLinNr = INTEGER(STRING(piHAar,"9999") + STRING(iX,"999"))
        NO-LOCK:
        
        ASSIGN dTotMan[iX]  = StLinje.VerdiSolgt
               dManKost[iX] = StLinje.VVarekost.
    END.
END.

ASSIGN dTestProc = 0.
DO iX = 1 TO 12:
    dProc[iX] = (dTotMan[iX] * 100) / dTotAr.
    IF dProc[iX] = ? THEN dProc[iX] = 0.
    ASSIGN dTestProc = dTestProc + dProc[iX].
    dDbProc[iX] = ((dTotMan[iX] - dManKost[iX]) * 100) / dTotMan[iX].
    IF dDbProc[iX] = ? THEN dDbProc[iX] = 0.
END.

IF dTestProc = 0 THEN
  DO iX = 1 TO 12:
    ASSIGN dProc[iX] = 100 / 12.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteMalDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteMalDag Procedure 
PROCEDURE WriteMalDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteMalMan) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteMalMan Procedure 
PROCEDURE WriteMalMan :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO ix = 1 TO 12:
  ASSIGN iAarMnd = piInAar * 100 + iX.
  FIND SBudMalManed EXCLUSIVE-LOCK WHERE SBudMalManed.MalId = piMalId
                                     AND SBudMalManed.AarMnd = iAarMnd NO-ERROR.
  IF NOT AVAILABLE SBudMalManed THEN
  DO:
      CREATE SBudMalManed.
      ASSIGN SBudMalManed.MalId = piMalId
             SBudMalManed.AarMnd = iAarMnd
             SBudMalManed.Prosent = dProc[iX]
             SBudMalManed.DbProsent = dDbProc[iX].
  END.
END.

ASSIGN dTestProc = 0.
FOR EACH SBudMalManed WHERE SBudMalManed.MalId = piMalId NO-LOCK:
    ASSIGN dTestProc = dTestProc + SBudMalManed.Prosent.
END.

IF dTestProc <> 100 THEN
DO:
  ASSIGN iAarMnd = piInAar * 100 + 12.
  FIND SBudMalManed EXCLUSIVE-LOCK WHERE SBudMalManed.MalId = piMalId
                                     AND SBudMalManed.AarMnd = iAarMnd NO-ERROR.
  IF AVAILABLE SBudMalManed THEN
    DO:
      ASSIGN SBudMalManed.Prosent = SBudMalManed.Prosent + (100 - dTestProc).
      RELEASE SBudMalManed.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

