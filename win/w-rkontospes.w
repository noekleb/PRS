&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Kassarapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Kassarapport 
/*------------------------------------------------------------------------

  File: b-biled.w

  Description: Bestillingsprogram for bestilling av kassarapport

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Tom Nøkleby, Polygon Software AS

  Created: 12/2-99 Oslo

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
def var wOk        as log  format "Ja/Nei" no-undo.
def var wKriterier as char no-undo. /* Pakket liste med kriteriene           */
def var wJobbNr    as int  no-undo. /* Jobbnummer som utskriften er tildelt. */
def var wDivData   as char no-undo. /* For transport av tilleggsinfo.        */
def var wStatus    as char no-undo. /* Tilbakemelding fra andre programmer   */

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-SokButik1 BUTTON-SokButik2 FI-Butik1 ~
FI-Butik2 FI-Dato1 BUTTON-SokDato Btn_OK Btn_Cancel Btn_Help RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik1 FI-Butik2 FI-Dato1 FILL-IN-3 ~
FILL-IN-4 FILL-IN-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Kassarapport AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Start utskrift" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokButik1  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokButik2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butik1 AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butik2 AS INTEGER FORMAT ">>>>>9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Kriterier" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Fra:" 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Til:" 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.2 BY 5.91.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 5.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokButik1 AT ROW 2.67 COL 34
     BUTTON-SokButik2 AT ROW 2.67 COL 54.6
     FI-Butik1 AT ROW 2.62 COL 17 COLON-ALIGNED
     FI-Butik2 AT ROW 2.62 COL 38 COLON-ALIGNED NO-LABEL
     FI-Dato1 AT ROW 3.86 COL 17 COLON-ALIGNED
     BUTTON-SokDato AT ROW 3.86 COL 34
     Btn_OK AT ROW 1.67 COL 62.4
     Btn_Cancel AT ROW 2.91 COL 62.4
     Btn_Help AT ROW 5.86 COL 62.6
     FILL-IN-3 AT ROW 1.24 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 1.91 COL 17.4 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 1.95 COL 38 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 1.48 COL 61.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 6.81.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Kassarapport ASSIGN
         HIDDEN             = YES
         TITLE              = "Bestilling av Kontospesifikasjon"
         HEIGHT             = 6.81
         WIDTH              = 80
         MAX-HEIGHT         = 27.67
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 27.67
         VIRTUAL-WIDTH      = 160
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Kassarapport
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Kassarapport)
THEN C-Kassarapport:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Kassarapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Kassarapport C-Kassarapport
ON END-ERROR OF C-Kassarapport /* Bestilling av Kontospesifikasjon */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Kassarapport C-Kassarapport
ON WINDOW-CLOSE OF C-Kassarapport /* Bestilling av Kontospesifikasjon */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Kassarapport
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  apply "CLOSE":U to THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Kassarapport
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Kassarapport
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Start utskrift */
DO:
  run Utskrift.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokButik1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik1 C-Kassarapport
ON CHOOSE OF BUTTON-SokButik1 IN FRAME DEFAULT-FRAME /* ... */
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Butik1
    &Program     = d-bbutiker.w
    &Frame       = {&FRAME-NAME}
  }   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokButik2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik2 C-Kassarapport
ON CHOOSE OF BUTTON-SokButik2 IN FRAME DEFAULT-FRAME /* ... */
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Butik2
    &Program     = d-bbutiker.w
    &Frame       = {&FRAME-NAME}
  }   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Kassarapport
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato1
DO:

  def var wTittel as char no-undo.
  assign FI-Dato1 = date(FI-Dato1:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato1
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butik1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butik1 C-Kassarapport
ON TAB OF FI-Butik1 IN FRAME DEFAULT-FRAME /* Butikk */
or "RETURN":U of FI-Butik1
DO:
  if int(FI-Butik1:screen-value) > 0 then
    display FI-Butik1:screen-value @ FI-butik2 with frame {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Kassarapport 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Statusmelding */
status input "Angi kriterier for utskrift av Kontospesifikasjon".

assign
  FI-Dato1 = today.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i} 

  apply "ENTRY":U to FI-Butik1.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Kassarapport  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Kassarapport)
  THEN DELETE WIDGET C-Kassarapport.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Kassarapport  _DEFAULT-ENABLE
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
  DISPLAY FI-Butik1 FI-Butik2 FI-Dato1 FILL-IN-3 FILL-IN-4 FILL-IN-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Kassarapport.
  ENABLE BUTTON-SokButik1 BUTTON-SokButik2 FI-Butik1 FI-Butik2 FI-Dato1 
         BUTTON-SokDato Btn_OK Btn_Cancel Btn_Help RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Kassarapport.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Kassarapport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Kassarapport 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
def var wTekst       as char no-undo.
def var wFilter      as char no-undo.
def var wDato1       as char no-undo.
def var wRapTittel   as char no-undo.
def var wBrukerId    as char no-undo.
def var wTittel      as char no-undo.
def var wKunde       as char no-undo.
def var wKriterier   as char no-undo.
def var wButikkLbl   as char no-undo.
def var wSkrevetLbl  as char no-undo.
def var wKollonneLbl as char no-undo.
def var wSideLbl     as char no-undo.
def var wVi          as char no-undo.
def var wKontoSpes   as char no-undo.

def var wOTHER-PARAMETERS  as char no-undo.
def var wRB-REPORT-LIBRARY as char no-undo.
def var wRB-REPORT-NAME    as char no-undo.
def var wRB-DB-CONNECTION  as char no-undo.

assign frame {&FRAME-NAME}
  FI-Butik1
  FI-Butik2
  FI-Dato1
  wDato1 = string(month(FI-Dato1),"99") + "/" +
           string(day(FI-Dato1),"99") + "/" + 
           string(year(FI-Dato1),"9999").

/* Navn på firma */                      
{syspara.i 1 1 100 wKunde}           
wKunde  = "KUNDE = " + wKunde.

/* Oppkobling av Skotex database */                      
if valid-handle(wLibHandle) then
  run RpbSkoDB in wLibHandle (output wRB-DB-CONNECTION).
if wRB-DB-CONNECTION = "" then
 {syspara.i 1 1 10 wRB-DB-CONNECTION}

/* Rapportens tittel */
{syspara.i 6 3 1 wTittel}           
wTittel = "TITTEL = " + wTittel.

/* Ledetekst kriterier tittel */
{syspara.i 6 3 2 wKriterier}           
wKriterier = "KRITERIER = " + 
             wKriterier + " " + 
             string(FI-Butik1) + "-" + string(FI-Butik2).
/* Ledetekst dato. */
{syspara.i 6 3 3 wTekst}           
wKriterier = wKriterier + " " + wTekst + " " +
             string(FI-Dato1).
             
/* Vårt firmanavn */
{syspara.i 1 1 101 wTekst}           
wVi = "VI = " + wTekst.

/* Label forran butikknavn og nummer. */
{syspara.i 6 3 4 wButikkLbl}           
wButikkLbl = "BUTIKK = " + wButikkLbl.

/* Label forran skrevet dato/tid. */
{syspara.i 6 3 5 wSkrevetLbl}           
wSkrevetLbl = "SKREVET = " + wSkrevetLbl.

/* Kollonnelabler. */
{syspara.i 6 3 6 wKollonneLbl}           
wKollonneLbl = "KOLLONNER = " + wKollonneLbl.

/* Sidenummerlabl. */
{syspara.i 6 3 7 wSideLbl}           
wSideLbl = "SIDE = " + wSideLbl.

/* Kollonnelabler. */
{syspara.i 6 3 8 wKontoSpes}           
wKontoSpes = "KONTOSPES = " + wKontoSpes.

assign
  wBrukerId  = "BRUKER = " + userid("dictdb")
  wRB-REPORT-LIBRARY   = "rpb\dagsoppgjor.prl"           
  wRB-REPORT-NAME      = "Kontospesifikasjon"               
  wOTHER-PARAMETERS    = wBrukerId    + "~n" + 
                         wKunde       + "~n" +
                         wTittel      + "~n" + 
                         wKriterier   + "~n" + 
                         wButikkLbl   + "~n" + 
                         wSkrevetLbl  + "~n" + 
                         wKollonneLbl + "~n" + 
                         wSideLbl     + "~n" +
                         wVi          + "~n" +                      
                         wKontoSpes
  .
                      
if FI-Butik2 < FI-Butik1 then
  do:
    message "Til butikken er mindre enn fra butikken!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.

assign
  FI-Butik1 = int(FI-Butik1:screen-value in frame DEFAULT-FRAME)
  FI-Butik2 = int(FI-Butik2:screen-value in frame DEFAULT-FRAME)
  wFilter   = "Konto.Dato    = " + string(wDato1)    + " and " +
              "Konto.Butikk >= " + string(FI-Butik1) + " and " + 
              "Konto.Butikk <= " + string(FI-Butik2).

{sww.i}
RUN  aderb\_prntrb2(
       wRB-REPORT-LIBRARY,              /* RB-REPORT-LIBRARY */     
       wRB-REPORT-NAME,                 /* RB-REPORT-NAME */
       wRB-DB-CONNECTION,               /* RB-DB-CONNECTION */
       "O",                             /* RB-INCLUDE-RECORDS */
       wFilter,                         /* RB-FILTER */
       "",                              /* RB-MEMO-FILE */
       "?",                             /* RB-PRINT-DESTINATION */
       "?",                             /* RB-PRINTER-NAME */
       "",                              /* RB-PRINTER-PORT */
       "",                              /* RB-OUTPUT-FILE */
        0,                              /* RB-NUMBER-COPIES  - zero */
        0,                              /* RB-BEGIN-PAGE - zero */
        0,                              /* RB-END-PAGE - zero */
       no,                              /* RB-TEST-PATTERN */
       "",                              /* RB-WINDOW-TITLE */
      yes,                              /* RB-DISPLAY-ERRORS */
      yes,                              /* RB-DISPLAY-STATUS */
       no,                              /* RB-NO-WAIT */
      wOTHER-PARAMETERS,                /* RB-OTHER-PARAMETERS */
       "").                   /* RB-STATUS-FILE */
{swn.i}
apply "ENTRY":U to FI-Butik1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

