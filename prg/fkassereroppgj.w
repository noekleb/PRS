&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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
DEF VAR cTekst AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS B-ButikkNr FI-ButikkNr FI-PoseNr BUTTON-1 ~
FI-Dato FI-KassererNr B-Dato B-KassererNr RECT-58 
&Scoped-Define DISPLAYED-OBJECTS FI-ButikkNr FI-PoseNr FI-Dato ~
FI-KassererNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-ButikkNr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-Dato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-KassererNr 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-1 
     LABEL "Blank filter" 
     SIZE 15 BY 1.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KassererNr AS INTEGER FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kasserernr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-PoseNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Pose" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 100 BY 4.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B-ButikkNr AT ROW 1.48 COL 33
     FI-ButikkNr AT ROW 1.48 COL 13 COLON-ALIGNED
     FI-PoseNr AT ROW 1.48 COL 49.8 COLON-ALIGNED
     BUTTON-1 AT ROW 1.48 COL 84
     FI-Dato AT ROW 2.48 COL 13 COLON-ALIGNED
     FI-KassererNr AT ROW 3.48 COL 13 COLON-ALIGNED
     B-Dato AT ROW 2.48 COL 33
     B-KassererNr AT ROW 3.48 COL 33
     RECT-58 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.2 BY 4.14.


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
         HEIGHT             = 4.14
         WIDTH              = 100.2.
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

&Scoped-define SELF-NAME B-ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ButikkNr fFrameWin
ON CHOOSE OF B-ButikkNr IN FRAME fMain /* ... */
OR F10 OF FI-ButikkNr
DO:
  ASSIGN
      cTekst = FI-ButikkNr:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gbutiker.w (
    INPUT-OUTPUT cTekst,      /* Feltliste   Komma separert */
    "",                       /* Feltverdier (chr(1) sep)   */ 
    "",                       /* Feltoperatorer komma separert */
    FI-ButikkNr:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  cTekst = RETURN-VALUE.
MESSAGE cTekst
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-ButikkNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Dato fFrameWin
ON CHOOSE OF B-Dato IN FRAME fMain /* ... */
or F10 of FI-Dato
DO:

  do with frame {&FRAME-NAME}:  

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "'Datosøk'"
  }   
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KassererNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KassererNr fFrameWin
ON CHOOSE OF B-KassererNr IN FRAME fMain /* ... */
OR F10 OF FI-KassererNr
DO:
  ASSIGN
      cTekst = FI-KassererNr:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gforsalj.w (
    INPUT-OUTPUT cTekst,      /* Returstreng - chr(1) separert */
    "",                       /* Feltliste   Komma separert */
    "",                       /* Feltverdier (chr(1) sep)   */ 
    "",                       /* Feltoperatorer komma separert */
    FI-KassererNr:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-KassererNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 fFrameWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Blank filter */
DO:
  RUN BlankFilter.
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ButikkNr fFrameWin
ON LEAVE OF FI-ButikkNr IN FRAME fMain /* Butikk */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato fFrameWin
ON DELETE-CHARACTER OF FI-Dato IN FRAME fMain /* Dato */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato fFrameWin
ON LEAVE OF FI-Dato IN FRAME fMain /* Dato */
DO:
  RUN StartSok.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-KassererNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-KassererNr fFrameWin
ON LEAVE OF FI-KassererNr IN FRAME fMain /* Kasserernr */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-PoseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-PoseNr fFrameWin
ON LEAVE OF FI-PoseNr IN FRAME fMain /* Pose */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankFilter fFrameWin 
PROCEDURE BlankFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME fMain:
      ASSIGN
          FI-ButikkNr:SCREEN-VALUE   = ""
          FI-KassererNr:SCREEN-VALUE = ""
          FI-Dato:SCREEN-VALUE       = ""
          FI-PoseNr:SCREEN-VALUE     = ""
          .  
  END.

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
  DISPLAY FI-ButikkNr FI-PoseNr FI-Dato FI-KassererNr 
      WITH FRAME fMain.
  ENABLE B-ButikkNr FI-ButikkNr FI-PoseNr BUTTON-1 FI-Dato FI-KassererNr B-Dato 
         B-KassererNr RECT-58 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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
  DEF VAR pcFields    AS CHAR NO-UNDO.
  DEF VAR pcValues    AS CHAR NO-UNDO.
  DEF VAR pcSort      AS CHAR NO-UNDO.
  DEF VAR pcOperator  AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.

  ASSIGN FRAME {&FRAME-NAME}
      FI-ButikkNr
      FI-KassererNr
      FI-Dato
      pcFeltListe ="ButikkNr,Dato,KassererNr,PoseNr"
      FI-PoseNr
      .

  IF FI-PoseNr <> "" THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "PoseNr"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   FI-PoseNr + 
                   (IF SUBSTRING(FI-PoseNr,1,1) = "*"
                      THEN "*"
                      ELSE "")
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     (IF SUBstring(FI-PoseNr,1,1) = "*"
                        THEN "MATCHES"
                        ELSE "BEGINS")
       .
  IF FI-ButikkNr <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "ButikkNr"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(FI-ButikkNr)
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     "="
       .

  IF FI-Dato <> ? THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Dato"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(FI-Dato) 
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     "="
       .
  
  IF FI-KassererNr <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "KassererNr"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(FI-KassererNr) 
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     "="
       .
  ASSIGN
      pcSort     = ""
      .

  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).

/* MESSAGE                                */
/*     "SokSdo" pcFields SKIP             */
/*              pcValues SKIP             */
/*              pcSort SKIP               */
/*              pcOperator SKIP           */
/*              pcFeltListe               */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

