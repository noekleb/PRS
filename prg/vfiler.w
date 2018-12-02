&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dfiler.i"}.


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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dfiler.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.FilNavn RowObject.Innlest ~
RowObject.Dato RowObject.Kl RowObject.Dobbel RowObject.Oppdatert ~
RowObject.Katalog RowObject.Overfort RowObject.Backup RowObject.Slettet ~
RowObject.Feil 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-7 
&Scoped-Define DISPLAYED-FIELDS RowObject.FilNavn RowObject.fuInnlestInfo ~
RowObject.Innlest RowObject.Dato RowObject.Kl RowObject.fuOppdatertInfo ~
RowObject.Dobbel RowObject.Oppdatert RowObject.Katalog ~
RowObject.fuOverfortInfo RowObject.Overfort RowObject.Backup ~
RowObject.AntLinjer RowObject.fuSlettetInfo RowObject.Slettet ~
RowObject.Feil 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 159 BY 4.76.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.FilNavn AT ROW 1.48 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.fuInnlestInfo AT ROW 1.48 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.Innlest AT ROW 1.52 COL 145
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.Dato AT ROW 2.43 COL 12 COLON-ALIGNED
          LABEL "Dato/kl"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.Kl AT ROW 2.43 COL 25.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     RowObject.fuOppdatertInfo AT ROW 2.43 COL 109 COLON-ALIGNED
          LABEL "Behandlet"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.Dobbel AT ROW 2.48 COL 48
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.Oppdatert AT ROW 2.48 COL 145
          LABEL "Behandlet"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.Katalog AT ROW 3.38 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.fuOverfortInfo AT ROW 3.38 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.Overfort AT ROW 3.43 COL 145
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.Backup AT ROW 3.48 COL 48
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.AntLinjer AT ROW 4.33 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     RowObject.fuSlettetInfo AT ROW 4.33 COL 109 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     RowObject.Slettet AT ROW 4.38 COL 145
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     RowObject.Feil AT ROW 4.57 COL 48
          LABEL "Har inneholdt eller inneholder transaksjoner med feil"
          VIEW-AS TOGGLE-BOX
          SIZE 53 BY .81
     RECT-7 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dfiler.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dfiler.i}
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
         HEIGHT             = 4.86
         WIDTH              = 159.6.
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

/* SETTINGS FOR FILL-IN RowObject.AntLinjer IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Dato IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Feil IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.fuInnlestInfo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuInnlestInfo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuOppdatertInfo IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       RowObject.fuOppdatertInfo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuOverfortInfo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuOverfortInfo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN RowObject.fuSlettetInfo IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       RowObject.fuSlettetInfo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR TOGGLE-BOX RowObject.Oppdatert IN FRAME F-Main
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetAntLinjer vTableWin 
PROCEDURE SetAntLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piAntLinjer AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN 
      RowObject.AntLinjer:SCREEN-VALUE = string(piAntLinjer)
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFeilFunnet vTableWin 
PROCEDURE SetFeilFunnet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN 
      RowObject.Feil:SCREEN-VALUE = "*"
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetInnlest vTableWin 
PROCEDURE SetInnlest :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN 
      RowObject.Innlest:SCREEN-VALUE = "*"
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetSlettet vTableWin 
PROCEDURE SetSlettet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN 
      RowObject.Slettet:SCREEN-VALUE = "*"
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

