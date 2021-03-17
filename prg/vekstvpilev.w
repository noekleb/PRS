&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dekstvpilev.i"}.



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

DEF VAR cKundeValues AS CHAR NO-UNDO.
DEF VAR cLookupValue AS CHAR NO-UNDO.
DEF VAR cMapping     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dekstvpilev.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.KortNavn RowObject.Navn ~
RowObject.AktivLev RowObject.PrioNr RowObject.LevNr RowObject.EDB-System 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS B-SokLev RECT-55 
&Scoped-Define DISPLAYED-FIELDS RowObject.EkstVPILevNr RowObject.KortNavn ~
RowObject.Navn RowObject.AktivLev RowObject.PrioNr RowObject.LevNr ~
RowObject.EDB-System 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS T-Modell FI-LevNamn 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokLev  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1 TOOLTIP "Starter Alt-S søkefunksjonen".

DEFINE VARIABLE FI-LevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 73 BY 8.57.

DEFINE VARIABLE T-Modell AS LOGICAL INITIAL no 
     LABEL "Modell/Artikkelhåndtering" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     B-SokLev AT ROW 6.71 COL 28.6 NO-TAB-STOP 
     RowObject.EkstVPILevNr AT ROW 1.24 COL 16 COLON-ALIGNED
          LABEL "Ekst VPI Levnr."
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.KortNavn AT ROW 2.19 COL 16 COLON-ALIGNED FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     RowObject.Navn AT ROW 3.19 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
     RowObject.AktivLev AT ROW 4.52 COL 18
          LABEL "Aktiv"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.PrioNr AT ROW 5.48 COL 16 COLON-ALIGNED
          LABEL "Prio"
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     T-Modell AT ROW 5.76 COL 41
     RowObject.LevNr AT ROW 6.71 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     FI-LevNamn AT ROW 6.71 COL 31.4 COLON-ALIGNED NO-LABEL
     RowObject.EDB-System AT ROW 7.91 COL 16 COLON-ALIGNED
          LABEL "Mappingtabell"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEMS "Item 1" 
          DROP-DOWN-LIST
          SIZE 53 BY 1
     RECT-55 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dekstvpilev.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dekstvpilev.i}
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
         HEIGHT             = 8.62
         WIDTH              = 73.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}
{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.AktivLev IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX RowObject.EDB-System IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.EkstVPILevNr IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FI-LevNamn IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.KortNavn IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.PrioNr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX T-Modell IN FRAME F-Main
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

&Scoped-define SELF-NAME B-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLev vTableWin
ON CHOOSE OF B-SokLev IN FRAME F-Main /* ... */
OR F10 OF RowObject.LevNr
DO:
  cLookupValue = "LevNr".
  RUN JBoxDLookup.w ("LevBas;LevNr;LevNamn","where true", INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
    RowObject.LevNr:SCREEN-VALUE = cLookupValue.
  ELSE
    RowObject.LevNr:SCREEN-VALUE = "".
  RowObject.LevNr:MODIFIED = TRUE.

  cLookupValue = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + RowObject.LevNr:SCREEN-VALUE ).
  IF cLookupValue NE "" THEN 
    FI-LevNamn:SCREEN-VALUE = cLookupValue.
  ELSE
      FI-LevNamn:SCREEN-VALUE = "".
  DYNAMIC-FUNCTION('setDataModified':U,
     INPUT true /* LOGICAL */).
  RETURN NO-APPLY.
END.

/*   cLookupValue = "Vg".                                                                                            */
/*   RUN JBoxDLookup.w ("VarGr;Vg;VgBeskr,Individ;","where true,FIRST Individ OF VarGr", INPUT-OUTPUT cLookupValue). */
/*                                                                                                                   */
/*   IF cLookupValue NE "" THEN DO:                                                                                  */
/*     fi-iVg:SCREEN-VALUE = cLookupValue.                                                                           */
/*     /*DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").*/                                                    */
/*     RUN OpenQuery.                                                                                                */
/*   END.                                                                                                            */
/*   RETURN NO-APPLY.                                                                                                */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.LevNr vTableWin
ON TAB OF RowObject.LevNr IN FRAME F-Main /* LevNr */
OR "RETURN" OF RowObject.LevNr 
DO:
    cKundeValues = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + RowObject.LevNr:SCREEN-VALUE ).
    IF cKundeValues NE "" THEN
      FI-LevNamn:SCREEN-VALUE = cKundeValues.

  
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
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      RowObject.EkstVPILevNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      RowObject.EkstVPILevNr:SENSITIVE = FALSE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE confirmExit vTableWin 
PROCEDURE confirmExit :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT-OUTPUT PARAMETER plCancel AS LOGICAL NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  DEF VAR pbDataModified AS LOG NO-UNDO.
  ASSIGN
    pbDataModified = DYNAMIC-FUNCTION('getDataModified':U)
      .
  DO WITH FRAME {&FRAME-NAME}:
    IF pbDataModified THEN
    DO:
        MESSAGE "Det er gjort endringer på posten." SKIP
                "Disse må lagres eller kanseleres før programmet kan avsluttes."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN
            plCancel = TRUE /* Flagger at avsluttning skal avbrytes */
            .
        RETURN NO-APPLY.
    END.
  END.

  RUN SUPER( INPUT-OUTPUT plCancel).

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN 
      RowObject.EkstVPILevNr:SENSITIVE = TRUE
      .
  END.
  RUN SetFokus. /* Setter fokus i ønsket felt. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */
  DEF VAR pbT-Modell AS LOG NO-UNDO.

  RUN GetHarModell IN DYNAMIC-FUNCTION('getDataSource':U) (OUTPUT pbT-Modell).

  DO WITH FRAME {&FRAME-NAME}:
      IF pbT-Modell THEN
          T-Modell:SCREEN-VALUE = "yes".
      ELSE
          T-Modell:SCREEN-VALUE = "no".

      cLookupValue  = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + RowObject.LevNr:SCREEN-VALUE).
      IF cLookupValue NE "" THEN 
        FI-LevNamn:SCREEN-VALUE = cLookupValue.
      ELSE
        FI-LevNamn:SCREEN-VALUE = "".
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH ImpHode NO-LOCK:
    cMapping = cMapping + ',' +
               ImpHode.EDB-System.                     
  END.
  ASSIGN 
      /*RowObject.EDB-System:SCREEN-VALUE IN FRAME F-Main = ENTRY(1,cMapping)*/
      RowObject.EDB-System:LIST-ITEMS  IN FRAME F-Main  = cMapping.
  
  RUN SUPER.

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
DO WITH FRAME {&FRAME-NAME}:
  IF RowObject.EkstVPILevNr:SENSITIVE = TRUE THEN
    RUN ApplyEntry ("EkstVPILevNr").
  ELSE
    RUN ApplyEntry ("KortNavn").
END.

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

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  IF RETURN-VALUE <> "ADM-ERROR" THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      RowObject.EkstVPILevNr:SENSITIVE = FALSE
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

