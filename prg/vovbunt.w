&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"sdo/dovbunt.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEF VAR lAdding AS LOG NO-UNDO.
DEFINE VARIABLE hDataSource AS HANDLE     NO-UNDO.
DEFINE VARIABLE hNavigation  AS HANDLE     NO-UNDO.
DEFINE VARIABLE cPassordkrav   AS CHAR NO-UNDO.
DEFINE VARIABLE hEtikettVindu AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dovbunt.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Merknad 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-56 
&Scoped-Define DISPLAYED-FIELDS RowObject.BuntNr RowObject.Merknad ~
RowObject.DatoOppdatert RowObject.fuKlOppdatert RowObject.OppdatertAv 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FI-Tekst 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Etikett 
     LABEL "Etiketter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Oppdater 
     LABEL "&Oppdater..." 
     SIZE 20 BY 1.14.

DEFINE VARIABLE FI-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Overføringsbunt" 
      VIEW-AS TEXT 
     SIZE 20 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 4.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B-Etikett AT ROW 1.71 COL 45 NO-TAB-STOP 
     B-Oppdater AT ROW 1.71 COL 64 NO-TAB-STOP 
     RowObject.BuntNr AT ROW 1.95 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RowObject.Merknad AT ROW 2.95 COL 23 COLON-ALIGNED FORMAT "X(250)"
          VIEW-AS FILL-IN 
          SIZE 59 BY 1
     RowObject.DatoOppdatert AT ROW 3.95 COL 23 COLON-ALIGNED
          LABEL "Oppdatert dato/kl"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.fuKlOppdatert AT ROW 3.95 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.OppdatertAv AT ROW 3.95 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     FI-Tekst AT ROW 1 COL 2 COLON-ALIGNED NO-LABEL
     RECT-56 AT ROW 1.48 COL 3
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dovbunt.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {sdo/dovbunt.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 4.67
         WIDTH              = 85.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-Etikett IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Oppdater IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.BuntNr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.DatoOppdatert IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FI-Tekst IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.fuKlOppdatert IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuKlOppdatert:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Merknad IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.OppdatertAv IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Etikett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Etikett vTableWin
ON CHOOSE OF B-Etikett IN FRAME F-Main /* Etiketter */
DO:
    DEFINE VARIABLE cStrekKodeListe AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cAntallListe    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCOunt          AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cButFraListe    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cButTilListe    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iButFra         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iButTil         AS INTEGER    NO-UNDO.
    RUN getButikker IN hDataSource (OUTPUT cButFraListe,OUTPUT cButTilListe).
    IF NUM-ENTRIES(cButFraListe) = 1 OR NUM-ENTRIES(cButTilListe) = 1 THEN
        RETURN.
    IF NUM-ENTRIES(cButFraListe) = 2 AND NUM-ENTRIES(cButTilListe) = 2 THEN
        ASSIGN iButFra = INT(ENTRY(2,cButFraListe))
               iButTil = INT(ENTRY(2,cButTilListe)).
    ELSE DO:
        RUN d-velgovetibut.w (cButFraListe,cButTilListe,OUTPUT iButFra,OUTPUT iButTil).
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN.
    END.
    RUN getEtiketter IN hDataSource (iButFra,iButTil,OUTPUT cStrekKodeListe,OUTPUT cAntallListe).
    IF cStrekKodeListe = "" THEN
        RETURN NO-APPLY.
    IF NOT VALID-HANDLE(hEtikettVindu) THEN
        RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (CURRENT-WINDOW).
    DO iCount = 1 TO NUM-ENTRIES(cStrekKodeListe):
        IF VALID-HANDLE(hEtikettVindu) THEN
            RUN NyEtikett IN hEtikettVindu (ENTRY(iCount,cStrekKodeListe),
                                            INT(ENTRY(iCount,cAntallListe)),0).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Oppdater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Oppdater vTableWin
ON CHOOSE OF B-Oppdater IN FRAME F-Main /* Oppdater... */
DO:
  ASSIGN
      lAdding = TRUE
      .
  RUN Oppdater.

  RUN refreshRow IN hDataSource.
/*   RUN dataAvailable IN hDataSource   */
/*     ( INPUT "SAME" /* CHARACTER */). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.Merknad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Merknad vTableWin
ON TAB OF RowObject.Merknad IN FRAME F-Main /* Merknad */
DO:
  IF RowObject.BuntNr:SCREEN-VALUE = "0" THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN EnableDisable (2).
  RUN dataAvailable IN hDataSource
    ( INPUT "SAME" /* CHARACTER */).
  APPLY "ENTRY" TO RowObject.Merknad IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE collectChanges vTableWin 
PROCEDURE collectChanges :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER pcChanges AS CHARACTER NO-UNDO.
  DEFINE INPUT-OUTPUT PARAMETER pcInfo    AS CHARACTER NO-UNDO.

  DEF VAR piAntFelt AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT-OUTPUT pcChanges, INPUT-OUTPUT pcInfo).

  /* Code placed here will execute AFTER standard behavior.    */

  IF lAdding THEN
  DO:
      ASSIGN
          pcChanges = pcChanges +
                      (IF pcChanges <> ""
                         THEN CHR(1)
                         ELSE "") +
                      "DatoOppdatert" + CHR(1) + STRING(TODAY)    + CHR(1) +
                      "TidOppdatert"  + CHR(1) + STRING(TIME)     + CHR(1) +
                      "OppdatertAv"   + CHR(1) + USERID("SkoTex") + CHR(1) +
                      "BatchNr"       + CHR(1) + "0"
          piAntFelt = INT(ENTRY(1,pcInfo)) + 4
          lAdding   = FALSE
          .
      ENTRY ( 1 , pcInfo ) = string(piAntFelt).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dataAvailable vTableWin 
PROCEDURE dataAvailable :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcRelative  AS CHARACTER NO-UNDO.
  DEFINE       VARIABLE  cColValues AS CHARACTER  NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcRelative).
   
  /* Code placed here will execute AFTER standard behavior.    */
  RUN EnableDisable (1).
  IF VALID-HANDLE(hDatasource) THEN DO:
      ASSIGN cColValues = DYNAMIC-FUNCTION('colValues':U IN hDatasource,
                           INPUT "DatoOppdatert" /* CHARACTER */).
      IF cColValues = "?" OR ENTRY(2,cColValues,CHR(1)) <> "?" THEN DO:
          RUN disableObject.
      END.
  END.
  ELSE DO:
      RUN enableObject.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deleteRecord vTableWin 
PROCEDURE deleteRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plOk AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      plOk = FALSE
      .
  MESSAGE "Vil du ta bort bunten?" SKIP
          "Alle tilhørende transaksjoner blir også tatt bort."
      VIEW-AS ALERT-BOX question BUTTONS YES-NO TITLE "Bekreft"
      UPDATE plOk.
  IF plOk = FALSE THEN
      RETURN "AVBRYT".

  RUN SUPER.
  
  /* Code placed here will execute AFTER standard behavior.    */
  IF DYNAMIC-FUNCTION('getRecordState':U) = "NoRecordAvailable" THEN DO:
      DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
      RUN disableObject.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE destroyObject vTableWin 
PROCEDURE destroyObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(hEtikettVindu) THEN
      APPLY "CLOSE" TO hEtikettVindu.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableObject vTableWin 
PROCEDURE disableObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN B-Oppdater:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableDisable vTableWin 
PROCEDURE EnableDisable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piMode AS INT NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    CASE piMode:
      WHEN 1 THEN
        /* Datasettet avgjør */
        DO:
          ASSIGN B-Oppdater:SENSITIVE = RowObject.DatoOppdatert:SCREEN-VALUE = "" AND
                                      RowObject.BuntNr:SCREEN-VALUE <> "0"
                 B-Etikett:SENSITIVE  = RowObject.BuntNr:SCREEN-VALUE <> "0".
        END.
      WHEN 2 THEN
          ASSIGN
            B-Oppdater:SENSITIVE = FALSE
            .
      WHEN 3 THEN
          ASSIGN
            B-Oppdater:SENSITIVE = TRUE
            .
       
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBuntNr vTableWin 
PROCEDURE getBuntNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  DEF OUTPUT PARAMETER iBuntNr AS INT FORMAT "->>>>>>9" NO-UNDO.

  iBuntNr = INT(RowObject.BuntNr:SCREEN-VALUE IN FRAME F-Main).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDatoOppdatert vTableWin 
PROCEDURE getDatoOppdatert :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER dDatoOppdatert AS DATE FORMAT "99/99/99" NO-UNDO.

  dDatoOppdatert = DATE(RowObject.DatoOppdatert:SCREEN-VALUE IN FRAME F-Main).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  
 ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U)
        hNavigation = DYNAMIC-FUNCTION('getTableIOSource':U).

  RUN SUPER.
  
  /* Code placed here will execute AFTER standard behavior.    */

  SUBSCRIBE TO "getBuntNr" ANYWHERE.
  SUBSCRIBE TO "getDatoOppdatert" ANYWHERE.

  {syspara.i 5 4 8 cPassordkrav}
  IF RowObject.DatoOppdatert:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" THEN
      RUN disableObject.
   ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U)
          hNavigation = DYNAMIC-FUNCTION('getTableIOSource':U).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Oppdater vTableWin 
PROCEDURE Oppdater :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plOk             AS LOG    NO-UNDO.

  MESSAGE "Skal overføringstransaksjonene klargjøres for oppdatering?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bekreft"
      UPDATE plOk.
  IF plOk = FALSE THEN
      RETURN NO-APPLY.
  IF cPassordkrav = "1" THEN DO:
      RUN d-bekreftbruker.w ("Bekreft brukerid").
      IF RETURN-VALUE = "AVBRYT" THEN DO:
          MESSAGE "Lagring avbrutt"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE
          RUN AssignOppdatertAv IN hDataSource (RETURN-VALUE).
  END.

  ASSIGN
      B-Oppdater:SENSITIVE IN FRAME F-Main = FALSE
      .
  RUN updateRecord.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE printObject vTableWin 
PROCEDURE printObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cObject AS CHARACTER  NO-UNDO.
DEFINE VARIABLE         iType   AS INTEGER INIT ?   NO-UNDO.
  IF cObject = "ovBunt" THEN DO:
      RUN PrintOverfor IN hDataSource
           ( INPUT INT(RowObject.BuntNr:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFokus vTableWin 
PROCEDURE SetFokus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "ENTRY":U TO RowObject.Merknad IN FRAME F-Main.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateRecord vTableWin 
PROCEDURE updateRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lNy    AS LOGICAL    NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF TRIM(RowObject.Merknad:SCREEN-VALUE IN FRAME {&FRAME-NAME}) = "" THEN DO:
      MESSAGE "Registrer merknad"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO RowObject.Merknad.
      RETURN.
  END.
  lNy = RowObject.BuntNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "0".
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  IF lNy THEN DO:
      DYNAMIC-FUNCTION('openQuery':U IN hDataSource).
      APPLY "TAB" TO RowObject.Merknad.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

