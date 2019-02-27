&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  Forfatter:   TN 4/8-98
  Beskrivelse: Kompilering av et program
  Parametere:  
  Endringer:
------------------------------------------------------------------------*/

CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */

/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
&ELSE
&ENDIF

/* Preprossessor direktiver ---                                         */

/* Buffer og Temp-Table Definisjoner ---                                */

/* Lokale variabler ---                                                 */
def var retur-verdi as char initial "<avbryt>" no-undo.
def var wCompNavn   as char                    no-undo.
DEF VAR chrBuffer   AS CHAR   NO-UNDO FORMAT 'X(128)'.
DEF VAR chrTempFile AS CHAR   NO-UNDO FORMAT 'X(128)'.
DEF VAR intRC       AS INTE   NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Sok RECT-2 RECT-3 FILL-IN-Info1 ~
FILL-IN-Info3 FILL-IN-CompNavn TOGGLE-Listing TOGGLE-Xref FILL-IN-Info2 ~
FILL-IN-Saveinto Btn_OK Btn_Cancel BUTTON-Propath 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Info1 FILL-IN-Info3 ~
FILL-IN-CompNavn TOGGLE-Listing TOGGLE-Xref FILL-IN-Info2 FILL-IN-Saveinto 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 14 BY 1.1
     BGCOLOR 8 .

DEFINE BUTTON Btn_Sok 
     IMAGE-UP FILE "icon\e-sokpr":U
     LABEL "Button 1" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON BUTTON-Propath 
     LABEL "Pr&opath..." 
     SIZE 14 BY 1.1.

DEFINE VARIABLE FILL-IN-CompNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Program" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Info1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 65 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Info2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 65 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Info3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 65 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-Saveinto AS CHARACTER FORMAT "X(256)":U 
     LABEL "&Save into" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 5.24.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 2.62.

DEFINE VARIABLE TOGGLE-Listing AS LOGICAL INITIAL no 
     LABEL "&Listing (til C:~\LISTING.TMP)" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE TOGGLE-Xref AS LOGICAL INITIAL no 
     LABEL "&XREF  (til C:~\XREF.TMP)" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_Sok AT ROW 1.95 COL 64.8
     FILL-IN-Info1 AT ROW 6.95 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-Info3 AT ROW 8.38 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-CompNavn AT ROW 1.95 COL 12 COLON-ALIGNED HELP
          "Tast inn porgramnavn"
     TOGGLE-Listing AT ROW 3.14 COL 14
     TOGGLE-Xref AT ROW 4.1 COL 14
     FILL-IN-Info2 AT ROW 7.67 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-Saveinto AT ROW 5.05 COL 12 COLON-ALIGNED HELP
          "Eventull save into"
     Btn_OK AT ROW 9.57 COL 40
     Btn_Cancel AT ROW 9.57 COL 56
     BUTTON-Propath AT ROW 9.52 COL 3
     RECT-2 AT ROW 1.24 COL 2
     RECT-3 AT ROW 6.71 COL 2
     SPACE(0.00) SKIP(1.34)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kompiler et program"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Kompiler et program */
DO:
  &SCOP Cop COMPILE VALUE(FILL-IN-Compnavn:SCREEN-VALUE) V6FRAME USE-REVVIDEO SAVE
  &SCOP Cow COMPILE VALUE(FILL-IN-Compnavn:SCREEN-VALUE) SAVE
  &SCOP Si INTO VALUE
  &SCOP Li LISTING C:\Listing.tmp
  &SCOP Xr XREF C:\Xref.tmp

  DEF VAR wComp   AS CHAR NO-UNDO.
  DEF VAR wMain   AS CHAR NO-UNDO.
  DEF VAR wExt    AS CHAR NO-UNDO.
  DEF VAR wTid    AS INTE NO-UNDO.
  DEF VAR wStrTid AS CHAR NO-UNDO.  
  DEF VAR wSvar   AS LOGI NO-UNDO INIT YES.
  
  ASSIGN TOGGLE-Listing
         TOGGLE-Xref.
  
  IF FILL-IN-CompNavn:SCREEN-VALUE = "" THEN DO:
     MESSAGE "Programnavn må oppgis."
         VIEW-AS ALERT-BOX ERROR TITLE "Feil".
     APPLY "ENTRY" TO FILL-IN-CompNavn.
     RETURN NO-APPLY.   
  END.
     
  IF R-INDEX(FILL-IN-CompNavn:SCREEN-VALUE,".") > 0 THEN 
       ASSIGN wExt  = SUBSTR(FILL-IN-CompNavn:SCREEN-VALUE,R-INDEX(FILL-IN-CompNavn:SCREEN-VALUE,".") + 1)
              wMain = SUBSTR(FILL-IN-CompNavn:SCREEN-VALUE,1,LENGTH(FILL-IN-CompNavn:SCREEN-VALUE) - (LENGTH(wExt) + 1)).
  ELSE ASSIGN wMain = FILL-IN-CompNavn:SCREEN-VALUE.       
     
  IF wExt = "" THEN DO:
     IF SEARCH(wMain + ".p") <> ? AND
        SEARCH(wMain + ".w") <> ?
     THEN DO:
        MESSAGE "Finner både"        SKIP
                wMain + ".p" + " og" SKIP
                wMain + ".w."        SKIP(1)
                "Du må angi hvilket av dem du vil kompilere." 
           VIEW-AS ALERT-BOX ERROR TITLE "Fant to filer".               
        APPLY "ENTRY" TO FILL-IN-CompNavn.
        RETURN NO-APPLY.  
     END.      
     IF SEARCH(wMain + ".p") <> ? THEN 
          ASSIGN wExt = "p".
     ELSE ASSIGN wExt = "w".     
  END. 
 
  IF SEARCH(wMain + "." + wExt) = ? THEN DO:
     MESSAGE "Finner ikke programmet." 
        VIEW-AS ALERT-BOX ERROR TITLE "Feil".
     APPLY "ENTRY" TO FILL-IN-CompNavn.    
     RETURN NO-APPLY.   
  END.
   
  ASSIGN FILL-IN-CompNavn:SCREEN-VALUE = wMain + "." + wExt
         FILL-IN-Saveinto = FILL-IN-Saveinto:SCREEN-VALUE
         wComp = (IF TOGGLE-Listing THEN "L" ELSE " ") + (IF TOGGLE-XRef THEN "X" ELSE " ") + (IF FILL-IN-Saveinto <> "" THEN "S" ELSE " ")
         chrBuffer   = FILL(" ",128)
         chrTempFile = FILL(" ",128)
         FILL-IN-Info1:SCREEN-VALUE = "Kompilerer programmet."
         FILL-IN-Info2:SCREEN-VALUE = ""
         FILL-IN-Info3:SCREEN-VALUE = "Vennligst vent.".
  
  DISABLE ALL WITH FRAME {&FRAME-NAME}.
  
  RUN GetTempPathA(128, OUTPUT chrBuffer, OUTPUT intRC).
  RUN GetTempFileNameA(chrBuffer, 'CMP', 0, OUTPUT chrTempFile, OUTPUT intRC).

  OUTPUT TO value(chrTempFile).
  
  PUT UNFORMATTED "Kompilering av " FILL-IN-Compnavn:SCREEN-VALUE SKIP(1).
   
  ASSIGN wTid = ETIME(TRUE)
         wTid = 0. 
   
  IF wExt = "p" THEN 
  CASE wComp:
     WHEN "LXS" THEN {&Cop} {&Si}(FILL-IN-Saveinto) {&Li} {&Xr}.   
     WHEN "LX " THEN {&Cop}                         {&Li} {&Xr}.
     WHEN "L S" THEN {&Cop} {&Si}(FILL-IN-Saveinto) {&Li}      .
     WHEN "L  " THEN {&Cop}                         {&Li}      .
     WHEN " XS" THEN {&Cop} {&Si}(FILL-IN-Saveinto)       {&Xr}.
     WHEN " X " THEN {&Cop}                               {&Xr}.
     WHEN "  S" THEN {&Cop} {&Si}(FILL-IN-Saveinto)            .
     OTHERWISE       {&Cop}                                    .
  END CASE.   
  ELSE
  CASE wComp:
     WHEN "LXS" THEN {&Cow} {&Si}(FILL-IN-Saveinto) {&Li} {&Xr}.   
     WHEN "LX " THEN {&Cow}                         {&Li} {&Xr}.
     WHEN "L S" THEN {&Cow} {&Si}(FILL-IN-Saveinto) {&Li}      .
     WHEN "L  " THEN {&Cow}                         {&Li}      .
     WHEN " XS" THEN {&Cow} {&Si}(FILL-IN-Saveinto)       {&Xr}.
     WHEN " X " THEN {&Cow}                               {&Xr}.
     WHEN "  S" THEN {&Cow} {&Si}(FILL-IN-Saveinto)            .
     OTHERWISE       {&Cow}                                    .
  END CASE.
  PUT UNFORMATTED SKIP(1) "--Slutt--".
  OUTPUT CLOSE.
  
  ASSIGN wTid    = ETIME - wTid
         wStrTid = "Kompileringen tok " + (IF wTid < 1000 THEN "under ett sekund." ELSE (TRIM(STRING(wTid / 1000,">>>>>>>.<<<<<<<")) + " sekunder."))
         FILL-IN-Info1:SCREEN-VALUE = ""
         FILL-IN-Info2:SCREEN-VALUE = "Kompilering ferdig."
         FILL-IN-Info3:SCREEN-VALUE = "".
 
  IF NOT COMPILER:ERROR AND NOT COMPILER:WARNING THEN 
     MESSAGE wStrTid SKIP(1)
             "Programmet kompilerte feilfritt!"
        VIEW-AS ALERT-BOX INFORMATION TITLE "Kompilering ferdig".
  ELSE DO:
     HIDE MESSAGE NO-PAUSE.
     IF COMPILER:ERROR THEN 
        MESSAGE wStrTid SKIP
                "Det er imidlertid en feil på linje " + STRING(COMPILER:ERROR-ROW) + "." SKIP(1)
                "Vil du se feilmeldingen(e)?"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO TITLE "Kompilering ferdig"
               UPDATE wSvar.
     ELSE   
        MESSAGE wStrTid SKIP
                "En advarsel ble imidlertid gitt." SKIP(1)
                "Vil du se advarselen?"
           VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO TITLE "Kompilering ferdig"
               UPDATE wSvar.
               
     IF wSvar = YES THEN
          RUN d-visfil.w (0,"Kompileringsresultat",chrTempFile,"F",0,"").
  END.   
  
  ASSIGN wCompNavn = FILL-IN-CompNavn:SCREEN-VALUE.
  
  OS-DELETE VALUE(chrTempFile).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kompiler et program */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sok Dialog-Frame
ON CHOOSE OF Btn_Sok IN FRAME Dialog-Frame /* Button 1 */
DO:
  DEF VAR wSvar AS LOGI NO-UNDO.
  ASSIGN FILL-IN-CompNavn.
  SYSTEM-DIALOG GET-FILE FILL-IN-CompNavn
        TITLE      "Velg program ..."
        FILTERS    "Progress UIB-filer (*.w)"               "*.w",
                   "Progress kildekode (*.p)"               "*.p"
        MUST-EXIST
        USE-FILENAME
        UPDATE wSvar.
  IF wSvar = YES THEN DO WITH FRAME {&FRAME-NAME}:
     DISPLAY FILL-IN-CompNavn.
     APPLY "ENTRY" TO Btn_Ok.
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Propath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Propath Dialog-Frame
ON CHOOSE OF BUTTON-Propath IN FRAME Dialog-Frame /* Propath... */
DO:
  RUN d-propath.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-CompNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CompNavn Dialog-Frame
ON F2 OF FILL-IN-CompNavn IN FRAME Dialog-Frame /* Program */
DO:
  APPLY "CHOOSE" TO Btn_Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CompNavn Dialog-Frame
ON LEAVE OF FILL-IN-CompNavn IN FRAME Dialog-Frame /* Program */
DO:
  DEF VAR wMain AS CHAR NO-UNDO.
  DEF VAR wExt  AS CHAR NO-UNDO.
  IF NOT SELF:SCREEN-VALUE MATCHES("*.exe*") THEN DO:
     IF R-INDEX(FILL-IN-CompNavn:SCREEN-VALUE,".") > 0 THEN 
           ASSIGN wExt  = SUBSTR(FILL-IN-CompNavn:SCREEN-VALUE,R-INDEX(FILL-IN-CompNavn:SCREEN-VALUE,".") + 1)
                  wMain = SUBSTR(FILL-IN-CompNavn:SCREEN-VALUE,1,LENGTH(FILL-IN-CompNavn:SCREEN-VALUE) - (LENGTH(wExt) + 1)).
     ELSE ASSIGN wMain = FILL-IN-CompNavn:SCREEN-VALUE.
     IF SEARCH(wMain + ".p") <> ? AND
        SEARCH(wMain + ".w") <> ? THEN.
     ELSE DO:
        IF SEARCH(wMain + ".p") <> ? THEN 
             ASSIGN wExt = "p".
        ELSE ASSIGN wExt = "w".
     END.
     
     IF SEARCH(wMain + "." + wExt) <> ? THEN ASSIGN SELF:SCREEN-VALUE = SEARCH(wMain + "." + wExt).
  END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-CompNavn Dialog-Frame
ON RETURN OF FILL-IN-CompNavn IN FRAME Dialog-Frame /* Program */
DO:
  RUN ReturnTast.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-Saveinto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Saveinto Dialog-Frame
ON F2 OF FILL-IN-Saveinto IN FRAME Dialog-Frame /* Save into */
DO:
  APPLY "CHOOSE" TO Btn_Sok.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-Saveinto Dialog-Frame
ON RETURN OF FILL-IN-Saveinto IN FRAME Dialog-Frame /* Save into */
DO:
  RUN ReturnTast.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Listing
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Listing Dialog-Frame
ON RETURN OF TOGGLE-Listing IN FRAME Dialog-Frame /* Listing (til C:\LISTING.TMP) */
DO:
  RUN ReturnTast.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOGGLE-Xref
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOGGLE-Xref Dialog-Frame
ON RETURN OF TOGGLE-Xref IN FRAME Dialog-Frame /* XREF  (til C:\XREF.TMP) */
DO:
  RUN ReturnTast.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */
{defaultbtn.i Btn_Ok} /* Setter evt. default-knapp */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


assign wCompNavn = "".
if VALID-HANDLE(wLibHandle) then
  RUN SistKompilert IN wLibHandle (input-output wCompNavn). 

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY  UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  DO WITH FRAME {&FRAME-NAME}: 
     ASSIGN FILL-IN-CompNavn = wCompNavn 
            FILL-IN-Info1    = "P-programmer kompileres med V6FRAME og USE-REVVIDEO."
            FILL-IN-Info2    = "W-programmer compileres uten."
            FILL-IN-Info3    = "Det er ikke nødvendig å angi extention eller save into."
            FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + "  [SkoTex]".
  
  END.
  
  {lng.i} RUN enable_UI.

  apply "ENTRY":U to FILL-IN-CompNavn in frame {&FRAME-NAME}.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}. 
  
END.
if VALID-HANDLE(wLibHandle) then
  RUN SistKompilert IN wLibHandle (input-output wCompNavn). 
  
RUN disable_UI.
RETURN "".

PROCEDURE GetTempPathA EXTERNAL 'KERNEL32.DLL':
   DEFINE INPUT PARAMETER intSize AS LONG.
   DEFINE OUTPUT PARAMETER chrBuffer AS CHARACTER.
   DEFINE RETURN PARAMETER intRC AS LONG.
END PROCEDURE. 

PROCEDURE GetTempFileNameA EXTERNAL 'KERNEL32.DLL':
   DEFINE INPUT PARAMETER chrpath AS CHARACTER.
   DEFINE INPUT PARAMETER chrPrefix AS CHARACTER.
   DEFINE INPUT PARAMETER intUnique AS LONG.
   DEFINE OUTPUT PARAMETER chrTempFile AS CHARACTER.
   DEFINE RETURN PARAMETER intRC AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Info1 FILL-IN-Info3 FILL-IN-CompNavn TOGGLE-Listing 
          TOGGLE-Xref FILL-IN-Info2 FILL-IN-Saveinto 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_Sok RECT-2 RECT-3 FILL-IN-Info1 FILL-IN-Info3 FILL-IN-CompNavn 
         TOGGLE-Listing TOGGLE-Xref FILL-IN-Info2 FILL-IN-Saveinto Btn_OK 
         Btn_Cancel BUTTON-Propath 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnTast Dialog-Frame 
PROCEDURE ReturnTast :
/*------------------------------------------------------------------------------
  Purpose:     Håndterer return-tast på objekter som ikke tas hånd om av
               session:data-entry-return: COMBO-BOX, RADIO-SET og TOGGLE-BOX.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF SESSION:DATA-ENTRY-RETURN THEN
        APPLY "TAB"    TO SELF.
   ELSE APPLY "CHOOSE" TO Btn_Ok IN FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


