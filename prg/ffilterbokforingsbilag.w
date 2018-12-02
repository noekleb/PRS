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
&Scoped-Define ENABLED-OBJECTS FILL-IN-4 RECT-3 B-LevNrBlank-2 FI-ButikkNr ~
CB-Godkjent B-LevNrBlank-3 FI-Dato FI-AAr CB-Sendt BUTTON-SokDato-2 ~
B-LevNrBlank B-SokLevNr 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-4 FI-ButikkNr CB-Godkjent FI-Dato ~
FI-AAr CB-Sendt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-LevNrBlank  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-LevNrBlank-2  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-LevNrBlank-3  NO-FOCUS
     LABEL "Blank" 
     SIZE 7 BY 1.

DEFINE BUTTON B-SokLevNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato-2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Godkjent AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Godkjent" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]","1",
                     "Godkjent"," 2",
                     "Ikke godkjent"," 3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sendt AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "Sendt" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]","1",
                     "Sendt"," 2",
                     "Ikke sendt"," 3"
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AAr AS INTEGER FORMAT "zzzz":U INITIAL 0 
     LABEL "År" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT "zzzzzz":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 149 BY 2.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     FILL-IN-4 AT ROW 1 COL 1 NO-LABEL
     B-LevNrBlank-2 AT ROW 2.91 COL 29.6 NO-TAB-STOP 
     FI-ButikkNr AT ROW 1.95 COL 9 COLON-ALIGNED
     CB-Godkjent AT ROW 1.95 COL 46.6 COLON-ALIGNED
     B-LevNrBlank-3 AT ROW 1.95 COL 92.4 NO-TAB-STOP 
     FI-Dato AT ROW 1.95 COL 72 COLON-ALIGNED
     FI-AAr AT ROW 2.91 COL 9 COLON-ALIGNED
     CB-Sendt AT ROW 2.91 COL 46.6 COLON-ALIGNED
     BUTTON-SokDato-2 AT ROW 1.95 COL 88
     B-LevNrBlank AT ROW 1.95 COL 29.6 NO-TAB-STOP 
     B-SokLevNr AT ROW 1.95 COL 25 NO-TAB-STOP 
     RECT-3 AT ROW 1.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149.8 BY 3.48.


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
         HEIGHT             = 3.76
         WIDTH              = 149.8.
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

/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME fMain
   ALIGN-L                                                              */
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

&Scoped-define SELF-NAME B-LevNrBlank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank fFrameWin
ON CHOOSE OF B-LevNrBlank IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-ButikkNr:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-butikkNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank-2 fFrameWin
ON CHOOSE OF B-LevNrBlank-2 IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-Aar:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Aar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LevNrBlank-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LevNrBlank-3 fFrameWin
ON CHOOSE OF B-LevNrBlank-3 IN FRAME fMain /* Blank */
DO:
  ASSIGN FI-Dato:SCREEN-VALUE = "".
  APPLY "TAB" TO FI-Dato.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLevNr fFrameWin
ON CHOOSE OF B-SokLevNr IN FRAME fMain /* ... */
OR F10 OF FI-ButikkNr
DO:
  DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  ASSIGN
      cTekst = "".

  /* Kaller søkerutine */
  RUN gbutiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    FI-ButikkNr:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) = 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        FI-ButikkNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
        .

        /* Flagger at det er gjort endringer på recorden og trigger toolbar. */
        APPLY "TAB":U TO FI-ButikkNr.
        RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 fFrameWin
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME fMain /* ... */
or F10 of FI-Dato
DO:

  def var wTittel as char no-undo.
  assign FI-Dato = date(FI-Dato:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  APPLY "TAB" TO FI-Dato.
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Godkjent
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Godkjent fFrameWin
ON VALUE-CHANGED OF CB-Godkjent IN FRAME fMain /* Godkjent */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Sendt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Sendt fFrameWin
ON VALUE-CHANGED OF CB-Sendt IN FRAME fMain /* Sendt */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-AAr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-AAr fFrameWin
ON RETURN OF FI-AAr IN FRAME fMain /* År */
OR "TAB":U OF FI-AAr DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ButikkNr fFrameWin
ON RETURN OF FI-ButikkNr IN FRAME fMain /* Butikk */
OR "TAB":U OF FI-ButikkNr DO:
    RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato fFrameWin
ON TAB OF FI-Dato IN FRAME fMain /* Dato */
OR "TAB":U OF FI-Dato DO:
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
  DISPLAY FILL-IN-4 FI-ButikkNr CB-Godkjent FI-Dato FI-AAr CB-Sendt 
      WITH FRAME fMain.
  ENABLE FILL-IN-4 RECT-3 B-LevNrBlank-2 FI-ButikkNr CB-Godkjent B-LevNrBlank-3 
         FI-Dato FI-AAr CB-Sendt BUTTON-SokDato-2 B-LevNrBlank B-SokLevNr 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          FI-ButikkNr:SCREEN-VALUE = ""
          FI-Aar:SCREEN-VALUE = ""
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Startsok fFrameWin 
PROCEDURE Startsok :
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
    ASSIGN pcFeltListe = "ButikkNr,Aar,GodkjentFlagg,SendtRegnskap,OmsetningsDato".

    DO iCount = 1 TO NUM-ENTRIES(pcFeltListe):
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "ButikkNr" THEN DO:
                IF int(FI-ButikkNr:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ButikkNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-butikkNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Aar" THEN DO:
                IF int(FI-Aar:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Aar"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-Aar:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "GodkjentFlagg" THEN DO:
                IF CB-Godkjent:SCREEN-VALUE <> "1" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "GodkjentFlagg"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           (IF int(CB-Godkjent:SCREEN-VALUE) = 2 THEN "yes" ELSE "no")
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "SendtRegnskap" THEN DO:
                IF CB-Sendt:SCREEN-VALUE <> "1" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "SendtRegnskap"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                    (IF int(CB-Sendt:SCREEN-VALUE) = 2 THEN "yes" ELSE "no")
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Omsetningsdato" THEN DO:
                IF INPUT FI-Dato <> ? THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Omsetningsdato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE chr(1)) + 
                           FI-Dato:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
        END CASE.
    END.
  END.

/*   MESSAGE                                */
/*           pcFields SKIP                  */
/*           pcValues SKIP                  */
/*           pcSort SKIP                    */
/*           pcOperator SKIP                */
/*           pcFeltListe                    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

