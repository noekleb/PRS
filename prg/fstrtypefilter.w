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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartFrame
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS maxStrTypeId BUTTON-2 FI-Str CB-Avdeling ~
CB-HuvGr RECT-1 
&Scoped-Define DISPLAYED-OBJECTS maxStrTypeId FI-Str CB-Avdeling CB-HuvGr ~
FI-Fltertekst 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "Blank" 
     SIZE 11.4 BY 1.14.

DEFINE VARIABLE CB-Avdeling AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Avdeling" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE CB-HuvGr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Hovedgr" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 43 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Fltertekst AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 9 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Str AS CHARACTER FORMAT "X(256)":U 
     LABEL "Alfafordeling" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE maxStrTypeId AS INTEGER FORMAT ">>>>>9":U INITIAL 999 
     LABEL "Maks. strtype" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 97 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     maxStrTypeId AT ROW 1.71 COL 67.2 COLON-ALIGNED WIDGET-ID 2
     BUTTON-2 AT ROW 2.91 COL 85.4
     FI-Str AT ROW 2.91 COL 67.2 COLON-ALIGNED
     CB-Avdeling AT ROW 1.81 COL 9.6 COLON-ALIGNED
     CB-HuvGr AT ROW 2.81 COL 9.6 COLON-ALIGNED
     FI-Fltertekst AT ROW 1.24 COL 1 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.57 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.2 BY 3.24.


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
         HEIGHT             = 3.24
         WIDTH              = 98.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB fFrameWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW fFrameWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   NOT-VISIBLE FRAME-NAME Custom                                        */
ASSIGN 
       FRAME fMain:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Fltertekst IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       FI-Fltertekst:READ-ONLY IN FRAME fMain        = TRUE.

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

&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 fFrameWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Blank */
DO:
  ASSIGN
      CB-Avdeling:SCREEN-VALUE = "0"
      CB-HuvGr:SCREEN-VALUE    = "0"
      FI-Str:SCREEN-VALUE      = ""
      .
  RUN startsok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Avdeling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Avdeling fFrameWin
ON VALUE-CHANGED OF CB-Avdeling IN FRAME fMain /* Avdeling */
DO:
    RUN initCbHuvg.
    RUN StartSok.
    PUBLISH "setEntry".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-HuvGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-HuvGr fFrameWin
ON VALUE-CHANGED OF CB-HuvGr IN FRAME fMain /* Hovedgr */
DO:
    RUN StartSok.
    PUBLISH "setEntry".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Str
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Str fFrameWin
ON TAB OF FI-Str IN FRAME fMain /* Alfafordeling */
OR "RETURN" OF FI-Str
DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME maxStrTypeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL maxStrTypeId fFrameWin
ON TAB OF maxStrTypeId IN FRAME fMain /* Maks. strtype */
OR "RETURN" OF maxStrTypeId
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
  DISPLAY maxStrTypeId FI-Str CB-Avdeling CB-HuvGr FI-Fltertekst 
      WITH FRAME fMain.
  ENABLE maxStrTypeId BUTTON-2 FI-Str CB-Avdeling CB-HuvGr RECT-1 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCbAvdelingNr fFrameWin 
PROCEDURE initCbAvdelingNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN CB-Avdeling:LIST-ITEM-PAIRS = cAlle + ",0"
             CB-Avdeling:screen-value = "0".
      for each Avdeling no-lock:
        CB-Avdeling:add-last(string(Avdeling.AvdelingNr,"zz9") + "/" + 
                         REPLACE(Avdeling.AvdelingNavn,","," "),(Avdeling.AvdelingNr)).
      end.
      ASSIGN 
          CB-HuvGr:LIST-ITEM-PAIRS = cAlle + ",0"
          CB-HuvGr:screen-value = "0"
          CB-HuvGr:SENSITIVE = FALSE
          .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCbHuvg fFrameWin 
PROCEDURE initCbHuvg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
          CB-HuvGr:LIST-ITEM-PAIRS = cAlle + ",0"
          CB-HuvGr:screen-value = "0"
          .

      for each HuvGr NO-LOCK WHERE
          HuvGr.AvdelingNr = int(CB-Avdeling:SCREEN-VALUE):
        CB-HuvGr:add-last(string(HuvGr.Hg,"zzz9") + "   " + 
                         REPLACE(HuvGr.HgBeskr,","," "),(HuvGr.Hg)).
      end.

      IF CB-Avdeling:screen-value = "0" THEN
          CB-HuvGr:SENSITIVE = FALSE.
      ELSE
          CB-HuvGr:SENSITIVE = TRUE.
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
  RUN initCbAvdelingNr.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilter fFrameWin 
PROCEDURE setFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piAvdelingNr AS INT NO-UNDO.
  DEF INPUT PARAMETER piHg         AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          CB-Avdeling:SCREEN-VALUE = string(piAvdelingNr)
          .
      RUN initCbHuvg.
      ASSIGN
          CB-HuvGr:SCREEN-VALUE = STRING(piHg)
          .
  END.
  RUN StartSok.

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
    ASSIGN pcFeltListe = "AvdelingNr,Hg,AlfaFordeling,StrTypeId".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "AvdelingNr" THEN DO:
                IF CB-Avdeling:SCREEN-VALUE <> "0" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AvdelingNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-Avdeling:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Hg" THEN DO:
                IF CB-HuvGr:SCREEN-VALUE <> "0" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Hg"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           CB-HuvGr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "AlfaFordeling" THEN DO:
                /*IF trim(FI-STr:SCREEN-VALUE) <> "" THEN */
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AlfaFordeling"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           ("*" + FI-Str:SCREEN-VALUE + "*")
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "matches".
            END.
            WHEN "StrTypeId" THEN DO:
                IF maxStrTypeId:SCREEN-VALUE <> "0" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "StrTypeId"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           maxStrTypeId:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
        END CASE.
    END.
  END.
/* 

      Lager,Storrelser,Aktiv,Annonse

 
      ASSIGN CB-Bildeikasse:LIST-ITEM-PAIRS = "[Alle],"
      ASSIGN CB-HKstyrt:LIST-ITEM-PAIRS = "[Alle],"
      ASSIGN CB-LokalPris:LIST-ITEM-PAIRS = "[Alle],"
      ASSIGN CB-Vareikasse:LIST-ITEM-PAIRS = "[Alle],"

 
 */


/* Tar bort siste entry i listen som er dummy */
ENTRY ( NUM-ENTRIES(pcFeltListe) , pcFeltListe ) = "Hg".

/*   MESSAGE pcFields SKIP                      */
/*           pcValues SKIP                      */
/*           pcSort SKIP                        */
/*           pcOperator SKIP                    */
/*           pcFeltListe SKIP                   */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */

  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).
  PUBLISH "setEntry".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

