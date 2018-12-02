&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          vpi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject
       {"dvpiartbas.i"}.


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
&Scoped-define DATA-FIELD-DEFS "dvpiartbas.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Vg RowObject.ProvKod ~
RowObject.VgKat RowObject.RabKod RowObject.BildNr RowObject.ValKod ~
RowObject.LevNr RowObject.VMId RowObject.StrTypeID RowObject.ProdNr ~
RowObject.SaSong RowObject.BehKode RowObject.Farg RowObject.MatKod ~
RowObject.Klack RowObject.Inner-Id RowObject.Ov-Id RowObject.Slit-Id ~
RowObject.Last-Id RowObject.Anv-Id 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-58 
&Scoped-Define DISPLAYED-FIELDS RowObject.Vg RowObject.ProvKod ~
RowObject.VgKat RowObject.RabKod RowObject.BildNr RowObject.ValKod ~
RowObject.LevNr RowObject.VMId RowObject.StrTypeID RowObject.ProdNr ~
RowObject.SaSong RowObject.BehKode RowObject.Farg RowObject.MatKod ~
RowObject.Klack RowObject.Inner-Id RowObject.Ov-Id RowObject.Slit-Id ~
RowObject.Last-Id RowObject.Anv-Id 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS T-1 T-15 T-2 T-16 T-3 T-17 T-4 T-18 T-5 ~
T-19 T-6 T-20 T-7 T-8 T-9 T-10 T-11 T-12 T-13 T-14 FI-Label-2 FI-Label-3 ~
FI-Label-4 FI-Label-5 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Label-2 AS CHARACTER FORMAT "X(256)":U INITIAL "VPI info" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Label-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Lokal info" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Label-4 AS CHARACTER FORMAT "X(256)":U INITIAL "VPI info" 
      VIEW-AS TEXT 
     SIZE 11 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Label-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Lokal info" 
      VIEW-AS TEXT 
     SIZE 27 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 146 BY 14.76.

DEFINE VARIABLE T-1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-10 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-11 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-12 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-13 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-14 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-15 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-16 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-17 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-18 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-19 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-20 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-3 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-4 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-5 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-6 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-7 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-8 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE T-9 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Vg AT ROW 1.95 COL 15 COLON-ALIGNED
          LABEL "Varegruppe" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.ProvKod AT ROW 1.95 COL 84 COLON-ALIGNED
          LABEL "Prov.kode" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-1 AT ROW 2.19 COL 39
     T-15 AT ROW 2.19 COL 108
     RowObject.VgKat AT ROW 2.91 COL 15 COLON-ALIGNED
          LABEL "Kategori"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.RabKod AT ROW 2.91 COL 84 COLON-ALIGNED
          LABEL "Maks rabatt" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-2 AT ROW 3.14 COL 39
     T-16 AT ROW 3.14 COL 108
     RowObject.BildNr AT ROW 3.86 COL 15 COLON-ALIGNED FORMAT ">>>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     RowObject.ValKod AT ROW 3.86 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-3 AT ROW 4.1 COL 39
     T-17 AT ROW 4.1 COL 108
     RowObject.LevNr AT ROW 4.81 COL 15 COLON-ALIGNED
          LABEL "Leverandør" FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.VMId AT ROW 4.81 COL 84 COLON-ALIGNED FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-4 AT ROW 5.05 COL 39
     T-18 AT ROW 5.05 COL 108
     RowObject.StrTypeID AT ROW 5.76 COL 15 COLON-ALIGNED FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.ProdNr AT ROW 5.76 COL 84 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-5 AT ROW 6 COL 39
     T-19 AT ROW 6 COL 108
     RowObject.SaSong AT ROW 6.71 COL 15 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     RowObject.BehKode AT ROW 6.71 COL 84 COLON-ALIGNED
          LABEL "Beh.kode" FORMAT ">9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-6 AT ROW 6.95 COL 39
     T-20 AT ROW 6.95 COL 108
     RowObject.Farg AT ROW 7.67 COL 15 COLON-ALIGNED
          LABEL "Farge" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-7 AT ROW 7.91 COL 39
     RowObject.MatKod AT ROW 8.62 COL 15 COLON-ALIGNED
          LABEL "Material" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-8 AT ROW 8.86 COL 39
     RowObject.Klack AT ROW 9.57 COL 15 COLON-ALIGNED FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-9 AT ROW 9.81 COL 39
     RowObject.Inner-Id AT ROW 10.52 COL 15 COLON-ALIGNED
          LABEL "Innersåle" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-10 AT ROW 10.76 COL 39
     RowObject.Ov-Id AT ROW 11.48 COL 15 COLON-ALIGNED
          LABEL "Innerfor" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-11 AT ROW 11.71 COL 39
     RowObject.Slit-Id AT ROW 12.43 COL 15 COLON-ALIGNED
          LABEL "Slitesåle" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-12 AT ROW 12.67 COL 39
     RowObject.Last-Id AT ROW 13.38 COL 15 COLON-ALIGNED
          LABEL "Læst" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     T-13 AT ROW 13.62 COL 39
     RowObject.Anv-Id AT ROW 14.33 COL 15 COLON-ALIGNED
          LABEL "Bruk" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     T-14 AT ROW 14.57 COL 39
     FI-Label-2 AT ROW 1.24 COL 17 NO-LABEL
     FI-Label-3 AT ROW 1.24 COL 28.6 NO-LABEL
     FI-Label-4 AT ROW 1.24 COL 86 NO-LABEL
     FI-Label-5 AT ROW 1.24 COL 97 NO-LABEL
     RECT-58 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dvpiartbas.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" ?  
      ADDITIONAL-FIELDS:
          {dvpiartbas.i}
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
         HEIGHT             = 14.76
         WIDTH              = 148.6.
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

/* SETTINGS FOR FILL-IN RowObject.Anv-Id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.BehKode IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.BildNr IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Farg IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN FI-Label-2 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Label-3 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Label-4 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Label-5 IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN RowObject.Inner-Id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.Klack IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Last-Id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.LevNr IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.MatKod IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.Ov-Id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.ProvKod IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.RabKod IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.SaSong IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN RowObject.Slit-Id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.StrTypeID IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR TOGGLE-BOX T-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-10 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-11 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-12 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-13 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-14 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-15 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-16 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-17 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-18 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-19 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-20 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-3 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-4 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-7 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-9 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Vg IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN RowObject.VgKat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.VMId IN FRAME F-Main
   EXP-FORMAT                                                           */
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

