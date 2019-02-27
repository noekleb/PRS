&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
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

DEFINE OUTPUT PARAMETER cFilnamn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSenast AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dDatum  AS DATE        NO-UNDO.
DEFINE VARIABLE iAarPerlinNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE dDateLoop AS DATE        NO-UNDO.

DEFINE BUFFER bufsyspara FOR syspara.

DEFINE TEMP-TABLE tt_dag NO-UNDO
    FIELD butik AS INTE
    FIELD datum AS DATE
    FIELD oms AS DECI
    INDEX bd IS PRIMARY UNIQUE butik datum.

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


FOR EACH syspara WHERE SysPara.SysHId = 210 AND
                       SysPara.SysGr  = 276   NO-LOCK.
    iButikkNr = SysPara.ParaNr.
    cSenast   = TRIM(SysPara.Parameter1). /* "20170203". */
    /* Vi kör nästa dag - det sparade dqatumet = senast körda */
    dDatum = DATE(INT(SUBSTR(cSenast,5,2)),INT(SUBSTR(cSenast,7,2)),INT(SUBSTR(cSenast,1,4))) + 1 NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.
    DO dDateLoop = dDatum TO TODAY - 1:
        FIND FIRST BokforingsBilag WHERE BokforingsBilag.OmsetningsDato = dDatum AND Bokforingsbilag.ButikkNr = SysPara.ParaNr NO-LOCK NO-ERROR.
        IF NOT AVAIL BokforingsBilag OR BokforingsBilag.EODMottatt = FALSE THEN
            NEXT.
        iAarPerLinNr = YEAR(dDateLoop) * 1000.
        iAarPerlinNr = iAarPerLinNr + (dDateLoop - DATE(12,31,YEAR(dDateLoop) - 1)).
        RUN getData.
        FIND bufSysPara WHERE ROWID(bufsyspara) = ROWID(syspara).
        syspara.parameter1 = STRING(YEAR(dDateLoop),"9999") + STRING(MONTH(dDateLoop),"99") + STRING(DAY(dDateLoop),"99").
        FIND CURRENT bufsyspara NO-LOCK.
        RELEASE bufsyspara.
    END.
        
END.
cFilnamn = SESSION:TEMP-DIR + "JF_Timeplan_" + STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99") + ".csv".
OUTPUT TO VALUE(cFilnamn).
FOR EACH TT_DAG:
    PUT UNFORMATTED tt_dag.butik ";" 
        STRING(YEAR(tt_dag.datum),"9999") "-" STRING(MONTH(tt_dag.datum),"99") "-" STRING(DAY(tt_dag.datum),"99") ";"
        ROUND(tt_dag.oms,0) SKIP.
END.
OUTPUT CLOSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-getData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getData Procedure 
PROCEDURE getData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE tt_dag.
    ASSIGN tt_dag.butik = iButikkNr
           tt_dag.datum = dDateLoop.
    /* strängt taget så är det väl bara 1 post */
    FOR EACH stlinje WHERE stlinje.butik = iButikkNr AND stlinje.sttypeid = "BUTSTAT" AND stlinje.perid = "DAG" AND stlinje.aarperlinnr = iAarperlinnr NO-LOCK.
        tt_dag.oms = tt_dag.oms + StLinje.VerdiSolgt.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

