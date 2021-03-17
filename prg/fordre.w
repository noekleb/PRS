&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS fFrameWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrfrm.w - ADM2 SmartFrame Template

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

DEFINE VARIABLE cAlle AS CHARACTER  NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-67 FI-OrdreNrFra FI-OrdreNrTil ~
FI-SendtFra FI-SendtTil CB-HKOrdre FI-EkstIdFra FI-EkstIdTil ~
btnSokVarebokNr FI-BekreftetFra FI-BekreftetTil CB-fraERP FI-LevNrFra ~
FI-LevNrTil FI-LevertFra FI-LevertTil CB-Bekreftet FI-VarebokNr CB-Status ~
B-Nullstill b-Kalk-2 b-Kalk-3 b-Kalk-4 b-Kalk-5 b-Kalk-6 b-Kalk1 
&Scoped-Define DISPLAYED-OBJECTS FI-OrdreNrFra FI-OrdreNrTil FI-SendtFra ~
FI-SendtTil CB-HKOrdre FI-EkstIdFra FI-EkstIdTil FI-BekreftetFra ~
FI-BekreftetTil CB-fraERP FI-LevNrFra FI-LevNrTil FI-LevertFra FI-LevertTil ~
CB-Bekreftet FI-VarebokNr CB-Status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON b-Kalk-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk-3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk-4 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk-5 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk-6 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON b-Kalk1 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS
     LABEL "b kalk 4" 
     SIZE 5 BY 1.

DEFINE BUTTON B-Nullstill 
     LABEL "Nullstill" 
     SIZE 16 BY 1.14.

DEFINE BUTTON btnSokVarebokNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE VARIABLE CB-Bekreftet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bekreftet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-fraERP AS CHARACTER FORMAT "X(256)":U 
     LABEL "fraERP" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-HKOrdre AS CHARACTER FORMAT "X(256)":U 
     LABEL "HK ordre" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Status AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BekreftetFra AS DATE FORMAT "99/99/99":U 
     LABEL "Bekreftet" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BekreftetTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EkstIdFra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekstern ref" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-EkstIdTil AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevertFra AS DATE FORMAT "99/99/99":U 
     LABEL "Levert" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevertTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNrFra AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Leverandør" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LevNrTil AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OrdreNrFra AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-OrdreNrTil AS INTEGER FORMAT ">>>>>>>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SendtFra AS DATE FORMAT "99/99/99":U 
     LABEL "Sendt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SendtTil AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-VarebokNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Vareh.bok" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-67
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 129 BY 5.24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FI-OrdreNrFra AT ROW 1.24 COL 16 COLON-ALIGNED
     FI-OrdreNrTil AT ROW 1.24 COL 31 COLON-ALIGNED NO-LABEL
     FI-SendtFra AT ROW 1.24 COL 62 COLON-ALIGNED
     FI-SendtTil AT ROW 1.24 COL 81.2 COLON-ALIGNED NO-LABEL
     CB-HKOrdre AT ROW 1.24 COL 111 COLON-ALIGNED
     FI-EkstIdFra AT ROW 2.43 COL 16 COLON-ALIGNED
     FI-EkstIdTil AT ROW 2.43 COL 31 COLON-ALIGNED NO-LABEL
     btnSokVarebokNr AT ROW 4.81 COL 42.6 NO-TAB-STOP 
     FI-BekreftetFra AT ROW 2.43 COL 62 COLON-ALIGNED
     FI-BekreftetTil AT ROW 2.43 COL 81 COLON-ALIGNED NO-LABEL
     CB-fraERP AT ROW 2.43 COL 111 COLON-ALIGNED
     FI-LevNrFra AT ROW 3.62 COL 16 COLON-ALIGNED
     FI-LevNrTil AT ROW 3.62 COL 31 COLON-ALIGNED NO-LABEL
     FI-LevertFra AT ROW 3.62 COL 62 COLON-ALIGNED
     FI-LevertTil AT ROW 3.62 COL 81 COLON-ALIGNED NO-LABEL
     CB-Bekreftet AT ROW 3.62 COL 111 COLON-ALIGNED
     FI-VarebokNr AT ROW 4.81 COL 16 COLON-ALIGNED
     CB-Status AT ROW 4.81 COL 62 COLON-ALIGNED
     B-Nullstill AT ROW 4.81 COL 113
     b-Kalk-2 AT ROW 2.43 COL 78 NO-TAB-STOP 
     b-Kalk-3 AT ROW 3.62 COL 78 NO-TAB-STOP 
     b-Kalk-4 AT ROW 1.24 COL 97.2 NO-TAB-STOP 
     b-Kalk-5 AT ROW 2.38 COL 97 NO-TAB-STOP 
     b-Kalk-6 AT ROW 3.62 COL 97 NO-TAB-STOP 
     b-Kalk1 AT ROW 1.24 COL 78.2 NO-TAB-STOP 
     RECT-67 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.2 BY 5.48.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartFrame
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target
   Other Settings: PERSISTENT-ONLY COMPILE
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
  CREATE WINDOW fFrameWin ASSIGN
         HEIGHT             = 5.48
         WIDTH              = 129.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}
{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Options          = ""
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME b-Kalk-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-2 fFrameWin
ON CHOOSE OF b-Kalk-2 IN FRAME fMain /* b kalk 4 */
DO:
  RUN Cal.w (FI-SendtTil:HANDLE).
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-3 fFrameWin
ON CHOOSE OF b-Kalk-3 IN FRAME fMain /* b kalk 4 */
DO:
  RUN Cal.w (FI-LevertFra:HANDLE).
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-4 fFrameWin
ON CHOOSE OF b-Kalk-4 IN FRAME fMain /* b kalk 4 */
DO:
  RUN Cal.w (FI-SendtTil:HANDLE).
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-5 fFrameWin
ON CHOOSE OF b-Kalk-5 IN FRAME fMain /* b kalk 4 */
DO:
  RUN Cal.w (FI-BekreftetTil:HANDLE).
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk-6 fFrameWin
ON CHOOSE OF b-Kalk-6 IN FRAME fMain /* b kalk 4 */
DO:
  RUN Cal.w (FI-BekreftetTil:HANDLE).
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME b-Kalk1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL b-Kalk1 fFrameWin
ON CHOOSE OF b-Kalk1 IN FRAME fMain /* b kalk 4 */
DO:
  RUN Cal.w (FI-SendtFra:HANDLE).
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Nullstill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Nullstill fFrameWin
ON CHOOSE OF B-Nullstill IN FRAME fMain /* Nullstill */
DO:
  RUN NullstillKrit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokVarebokNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokVarebokNr fFrameWin
ON CHOOSE OF btnSokVarebokNr IN FRAME fMain /* ... */
DO:
  DEF VAR cLookupValue  AS CHAR NO-UNDO.

  cLookupValue = "VareBehNr".
  piLoop = 1.
  RUN JBoxDLookup.w ("VareBehHode;VareBehNr;VareBehBeskrivelse;Oppdatert|Oppdatert;!VareBehType","where VareBehHode.VareBehType = " + string(piLoop), INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    FI-VarebokNr:SCREEN-VALUE = cLookupValue.
    /*DYNAMIC-FUNCTION("setToolbar",hUpdToolBar,"modified").*/
    RUN StartSok.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Bekreftet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Bekreftet fFrameWin
ON VALUE-CHANGED OF CB-Bekreftet IN FRAME fMain /* Bekreftet */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-fraERP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-fraERP fFrameWin
ON VALUE-CHANGED OF CB-fraERP IN FRAME fMain /* fraERP */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-HKOrdre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HKOrdre fFrameWin
ON VALUE-CHANGED OF CB-HKOrdre IN FRAME fMain /* HK ordre */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Status
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Status fFrameWin
ON VALUE-CHANGED OF CB-Status IN FRAME fMain /* Status */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BekreftetFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BekreftetFra fFrameWin
ON LEAVE OF FI-BekreftetFra IN FRAME fMain /* Bekreftet */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BekreftetTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BekreftetTil fFrameWin
ON LEAVE OF FI-BekreftetTil IN FRAME fMain
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-EkstIdFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EkstIdFra fFrameWin
ON LEAVE OF FI-EkstIdFra IN FRAME fMain /* Ekstern ref */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-EkstIdTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EkstIdTil fFrameWin
ON LEAVE OF FI-EkstIdTil IN FRAME fMain
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevertFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevertFra fFrameWin
ON LEAVE OF FI-LevertFra IN FRAME fMain /* Levert */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevertTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevertTil fFrameWin
ON LEAVE OF FI-LevertTil IN FRAME fMain
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNrFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNrFra fFrameWin
ON LEAVE OF FI-LevNrFra IN FRAME fMain /* Leverandør */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LevNrTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LevNrTil fFrameWin
ON LEAVE OF FI-LevNrTil IN FRAME fMain
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-OrdreNrFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-OrdreNrFra fFrameWin
ON LEAVE OF FI-OrdreNrFra IN FRAME fMain /* Ordrenr */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-OrdreNrTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-OrdreNrTil fFrameWin
ON LEAVE OF FI-OrdreNrTil IN FRAME fMain
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-SendtFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SendtFra fFrameWin
ON LEAVE OF FI-SendtFra IN FRAME fMain /* Sendt */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-SendtTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-SendtTil fFrameWin
ON LEAVE OF FI-SendtTil IN FRAME fMain
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-VarebokNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-VarebokNr fFrameWin
ON LEAVE OF FI-VarebokNr IN FRAME fMain /* Vareh.bok */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK fFrameWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN
   /* Now enable the interface  if in test mode - otherwise this happens when
      the object is explicitly initialized from its container. */
   RUN initializeObject.
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects fFrameWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI fFrameWin  _DEFAULT-DISABLE
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
  HIDE FRAME fMain.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI fFrameWin  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FI-OrdreNrFra FI-OrdreNrTil FI-SendtFra FI-SendtTil CB-HKOrdre 
          FI-EkstIdFra FI-EkstIdTil FI-BekreftetFra FI-BekreftetTil CB-fraERP 
          FI-LevNrFra FI-LevNrTil FI-LevertFra FI-LevertTil CB-Bekreftet 
          FI-VarebokNr CB-Status 
      WITH FRAME fMain.
  ENABLE RECT-67 FI-OrdreNrFra FI-OrdreNrTil FI-SendtFra FI-SendtTil CB-HKOrdre 
         FI-EkstIdFra FI-EkstIdTil btnSokVarebokNr FI-BekreftetFra 
         FI-BekreftetTil CB-fraERP FI-LevNrFra FI-LevNrTil FI-LevertFra 
         FI-LevertTil CB-Bekreftet FI-VarebokNr CB-Status B-Nullstill b-Kalk-2 
         b-Kalk-3 b-Kalk-4 b-Kalk-5 b-Kalk-6 b-Kalk1 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCombos fFrameWin 
PROCEDURE initCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN CB-Status:LIST-ITEM-PAIRS = cAlle + ", "
             CB-Status:screen-value = " ".
      for each SysPara NO-LOCK WHERE
          SysPara.SysHId  = 5 AND
          SysPara.SysGr   = 3 AND
          SysPara.Beskrivelse <> "":
          CB-Status:add-last(string(SysPara.ParaNr,"99") + "   " + 
                             REPLACE(SysPara.Beskrivelse,","," "),STRING(SysPara.ParaNr)).
      end.

      ASSIGN CB-HKOrdre:LIST-ITEMS = cAlle + ",,Ja,YES,Nei,no"
             CB-HKOrdre:screen-value = "NO".

      ASSIGN CB-fraERP:LIST-ITEMS = cAlle + ",,Ja,YES,Nei,no"
             CB-fraERP:screen-value = "NO".

      ASSIGN CB-Bekreftet:LIST-ITEMS = cAlle + ",,Ja,YES,Nei,no"
             CB-Bekreftet:screen-value = "NO".

    END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject fFrameWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.
  RUN NullstillKrit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillKrit fFrameWin 
PROCEDURE NullstillKrit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN 
      CB-fraERP:screen-value = " " 
      CB-Bekreftet:screen-value = " "
      CB-HkOrdre:screen-value = " "
      CB-Status:SCREEN-VALUE = ""
      FI-OrdreNrFra:SCREEN-VALUE = ""
      FI-EkstIdFra:SCREEN-VALUE = ""
      FI-LevNrTil:SCREEN-VALUE = ""
      FI-OrdreNrTil:SCREEN-VALUE = ""
      FI-EkstIdTil:SCREEN-VALUE = ""
      FI-LevNrTil:SCREEN-VALUE = ""
      FI-VareBokNr:SCREEN-VALUE = ""
      FI-SendtFra:SCREEN-VALUE = ?
      FI-SendtTil:SCREEN-VALUE = ?
      FI-BekreftetFra:SCREEN-VALUE = ?
      FI-BekreftetTil:SCREEN-VALUE = ?
      FI-LevertFra:SCREEN-VALUE = ?
      FI-LevertTil:SCREEN-VALUE = ?
      .
  RUN StartSok.
  APPLY "entry" TO FI-OrdreNrFra.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok fFrameWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFields   AS CHAR NO-UNDO.
  DEF VAR pcValues   AS CHAR NO-UNDO.
  DEF VAR pcSort     AS CHAR NO-UNDO.
  DEF VAR pcOperator AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.
  DEF VAR iCount      AS INTE NO-UNDO.
  DEF VAR iTst        AS INTE NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN pcFeltListe = "OrdreNr,EkstId,LevNr,VareBehNr,SendtDato,BekreftetDato,LeveringsDato,OrdreStatus,HKOrdre,fraERP,BekreftetOrdre".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):

        CASE ENTRY(iCount,pcFeltliste):
            WHEN "OrdreNr" THEN DO:
                IF int(FI-OrdreNrFra:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ArtikkelNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-OrdreNrFra:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF int(FI-OrdreNrTil:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ArtikkelNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-OrdreNrTil:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "EkstId" THEN DO:
                IF FI-EkstIdFra:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EkstId"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-EkstIdFra:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-EkstIdTil:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EkstId"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-EkstIdTil:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "LevNr" THEN DO:
                IF int(FI-LevNrFra:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LevNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-LevNrFra:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF int(FI-LevNrTil:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LevNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-LevNrTil:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "VareBehNr" THEN DO:
                IF int(FI-VarebokNr:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "VareBehNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-VarebokNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "SendtDato" THEN DO:
                IF FI-SendtFra:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "SendtDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-SendtFra:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-SendtTil:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "SendtDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-SendtTil:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "BekreftetDato" THEN DO:
                IF FI-BekreftetFra:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BekreftetDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-BekreftetFra:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-BekreftetTil:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BekreftetDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-BekreftetTil:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "LeveringsDato" THEN DO:
                IF FI-LevertFra:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LeveringsDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-LevertFra:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-LevertTil:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LeveringsDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           FI-LevertTil:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "OrdreStatus" THEN DO:
                IF CB-Status:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "OrdreStatus"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           CB-Status:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "HKOrdre" THEN DO:
                IF CB-HKOrdre:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "HKOrdre"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           CB-HKOrdre:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "fraERP" THEN DO:
                IF CB-fraERP:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "fraERP"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           CB-fraERP:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "BekreftetOrdre" THEN DO:
                IF CB-Bekreftet:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BekreftetOrdre"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) +
                           CB-Bekreftet:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
        END CASE.
    END.
  END.

/*   MESSAGE "pcFields:" pcFields SKIP       */
/*           "pcValues:" pcValues SKIP       */
/*           "pcSort:" pcSort SKIP           */
/*           "pcOperator:" pcOperator SKIP   */
/*           "pcFeltListe:" pcFeltListe SKIP */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.  */

  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

