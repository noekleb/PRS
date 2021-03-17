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
DEF VAR cAlle  AS CHAR NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-NextBut CB-Typ FI-ButikkNr ~
BUTTON-NextDt FI-Dato BUTTON-PrevBut BUTTON-PrevDt B-ButikkNr B-Dato ~
RECT-58 
&Scoped-Define DISPLAYED-OBJECTS CB-Typ FI-ButikkNr FI-Dato 

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

DEFINE BUTTON BUTTON-NextBut 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.4 BY 1 TOOLTIP "Neste post".

DEFINE BUTTON BUTTON-NextDt 
     IMAGE-UP FILE "icon/next.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.4 BY 1 TOOLTIP "Neste post".

DEFINE BUTTON BUTTON-PrevBut 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.4 BY 1 TOOLTIP "Forrige post".

DEFINE BUTTON BUTTON-PrevDt 
     IMAGE-UP FILE "icon/prev.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.4 BY 1 TOOLTIP "Forrige post".

DEFINE VARIABLE CB-Typ AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Typ" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEM-PAIRS "Item1",0
     DROP-DOWN-LIST
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butik" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 18 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 51 BY 4.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-NextBut AT ROW 2.71 COL 42.4 NO-TAB-STOP 
     CB-Typ AT ROW 1.57 COL 13.2 COLON-ALIGNED
     FI-ButikkNr AT ROW 2.62 COL 13 COLON-ALIGNED
     BUTTON-NextDt AT ROW 3.67 COL 42.4 NO-TAB-STOP 
     FI-Dato AT ROW 3.67 COL 13 COLON-ALIGNED
     BUTTON-PrevBut AT ROW 2.71 COL 37.8 NO-TAB-STOP 
     BUTTON-PrevDt AT ROW 3.67 COL 37.8 NO-TAB-STOP 
     B-ButikkNr AT ROW 2.67 COL 33
     B-Dato AT ROW 3.67 COL 33
     RECT-58 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 51 BY 4.14.


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
         WIDTH              = 51.
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
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-ButikkNr:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
  APPLY "RETURN" TO FI-ButikkNr.
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


&Scoped-define SELF-NAME BUTTON-NextBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NextBut fFrameWin
ON CHOOSE OF BUTTON-NextBut IN FRAME fMain /* Neste */
DO:
    FIND Butiker WHERE Butiker.Butik = FI-ButikkNr NO-LOCK NO-ERROR.
    IF AVAIL Butiker THEN
        FIND NEXT Butiker NO-LOCK NO-ERROR.
    IF AVAIL Butiker THEN DO:
        ASSIGN FI-ButikkNr = Butiker.Butik
               FI-ButikkNr:SCREEN-VALUE = STRING(FI-ButikkNr).
        APPLY "RETURN" TO FI-ButikkNr.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NextDt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NextDt fFrameWin
ON CHOOSE OF BUTTON-NextDt IN FRAME fMain /* Neste */
DO:
    IF FI-Dato < TODAY THEN DO:
        ASSIGN FI-Dato = FI-Dato + 1
               FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
        APPLY "RETURN" TO FI-Dato.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-PrevBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-PrevBut fFrameWin
ON CHOOSE OF BUTTON-PrevBut IN FRAME fMain /* Forrige */
DO:
    FIND Butiker WHERE Butiker.Butik = FI-ButikkNr NO-LOCK NO-ERROR.
    IF AVAIL Butiker THEN
        FIND PREV Butiker NO-LOCK NO-ERROR.
    IF AVAIL Butiker THEN DO:
        ASSIGN FI-ButikkNr = Butiker.Butik
               FI-ButikkNr:SCREEN-VALUE = STRING(FI-ButikkNr).
        APPLY "RETURN" TO FI-ButikkNr.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-PrevDt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-PrevDt fFrameWin
ON CHOOSE OF BUTTON-PrevDt IN FRAME fMain /* Forrige */
DO:
    ASSIGN FI-Dato = FI-Dato - 1
           FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
    APPLY "RETURN" TO FI-Dato.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Typ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Typ fFrameWin
ON VALUE-CHANGED OF CB-Typ IN FRAME fMain /* Typ */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ButikkNr fFrameWin
ON LEAVE OF FI-ButikkNr IN FRAME fMain /* Butik */
DO:
    IF SELF:MODIFIED THEN DO:
        SELF:MODIFIED = FALSE.
        RUN StartSok.  
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-ButikkNr fFrameWin
ON RETURN OF FI-ButikkNr IN FRAME fMain /* Butik */
OR "TAB" OF FI-ButikkNr DO:
    SELF:MODIFIED = FALSE.
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato fFrameWin
ON DELETE-CHARACTER OF FI-Dato IN FRAME fMain /* Datum */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato fFrameWin
ON LEAVE OF FI-Dato IN FRAME fMain /* Datum */
DO:
    IF SELF:MODIFIED THEN DO:
        SELF:MODIFIED = FALSE.
        RUN StartSok.  
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato fFrameWin
ON RETURN OF FI-Dato IN FRAME fMain /* Datum */
OR "TAB" OF FI-Dato DO:
    SELF:MODIFIED = FALSE.
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
          CB-Typ:SCREEN-VALUE        = "0"
          FI-ButikkNr:SCREEN-VALUE   = ""
          FI-Dato:SCREEN-VALUE       = ""
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
  DISPLAY CB-Typ FI-ButikkNr FI-Dato 
      WITH FRAME fMain.
  ENABLE BUTTON-NextBut CB-Typ FI-ButikkNr BUTTON-NextDt FI-Dato BUTTON-PrevBut 
         BUTTON-PrevDt B-ButikkNr B-Dato RECT-58 
      WITH FRAME fMain.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFileterverdier fFrameWin 
PROCEDURE getFileterverdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER cButik AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER dDato  AS DATE       NO-UNDO.
   DO WITH FRAME {&FRAME-NAME}:
       ASSIGN cButik = FI-ButikkNr:SCREEN-VALUE
              dDato  = INPUT FI-Dato.
   END.
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
      ASSIGN CB-Typ:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cAlle + "," + "0," +
             "1 Endast varukost,1,2 Pris/varukost,2," +
             "3 Endast pris,3,1+2 Varukost,4,2+3 Pris,5".

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
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCombos.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-Dato = TODAY
             FI-Dato:SCREEN-VALUE = STRING(FI-Dato).
      FIND FIRST Butiker NO-LOCK NO-ERROR.
      IF AVAIL Butiker THEN
          ASSIGN FI-ButikkNr = Butiker.Butik
                 FI-ButikkNr:SCREEN-VALUE = STRING(FI-ButikkNr).
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
  DEF VAR pcFields    AS CHAR NO-UNDO.
  DEF VAR pcValues    AS CHAR NO-UNDO.
  DEF VAR pcSort      AS CHAR NO-UNDO.
  DEF VAR pcOperator  AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.

  ASSIGN FRAME {&FRAME-NAME}
      CB-Typ
      FI-ButikkNr
      FI-Dato
      pcFeltListe ="Feilkode,ButikkNr,Dato"
      .

  IF CB-Typ <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Feilkode"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   IF CB-Typ < 4 THEN STRING(CB-Typ) ELSE IF CB-Typ = 4 THEN "3"
                       ELSE "1"
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     IF CB-Typ < 4 THEN "=" ELSE IF CB-Typ = 4 THEN "<" ELSE ">"
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
  
  ASSIGN
      pcSort     = ""
      .

  PUBLISH "SokSdo" (pcFields,
                    pcValues,
                    pcSort,
                    pcOperator,
                    pcFeltListe
                   ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

