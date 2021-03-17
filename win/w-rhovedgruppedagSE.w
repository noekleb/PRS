&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Bilderapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Bilderapport 
/*------------------------------------------------------------------------

  File: b-biled.w

  Description: Bestillingsprogram for bestilling av billedrapport.
               Programmet henter kriterier fra bruker, sender disse
               videre til rutine som oppretter datasettet.
               Når datasettet er opprettet, startes rutine som 
               legger data ut til fil og Excel aktiveres.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Tom Nøkleby, T.N.Consult

  Created: 24/5-98 - Stokholm.

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
DEF VAR cKollonneLblDef AS CHARACTER 
       INIT "Dato,Avd1,Avd2,Avd3,Avd4,Avd5,Avd6,Avd7,Avd8,Øvrigt,Total" NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS BUTTON-SokButik1 BUTTON-SokButik2 ~
BUTTON-SokDato-2 FI-Butik1 FI-Butik2 FI-Dato1 FI-Dato2 BUTTON-SokDato ~
RS-Akkumulering Btn_OK Btn_Cancel Btn_Help Btn_OK-2 RECT-1 RECT-2 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik1 FI-Butik2 FI-Dato1 FI-Dato2 ~
RS-Akkumulering FILL-IN-3 FILL-IN-4 FILL-IN-5 FILL-IN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Bilderapport AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON Btn_OK-2 AUTO-GO 
     LABEL "Start &excel" 
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

DEFINE BUTTON BUTTON-SokDato-2 
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

DEFINE VARIABLE FI-Dato2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Akkumulering:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(256)":U INITIAL "Kriterier" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Fra:" 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "Til:" 
      VIEW-AS TEXT 
     SIZE 13.6 BY .62 NO-UNDO.

DEFINE VARIABLE RS-Akkumulering AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Dag", 1,
"Måned", 2
     SIZE 23 BY 1.1 NO-UNDO.

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
     BUTTON-SokDato-2 AT ROW 3.86 COL 54.6
     FI-Butik1 AT ROW 2.62 COL 17 COLON-ALIGNED
     FI-Butik2 AT ROW 2.62 COL 38 COLON-ALIGNED NO-LABEL
     FI-Dato1 AT ROW 3.86 COL 17 COLON-ALIGNED
     FI-Dato2 AT ROW 3.86 COL 38 COLON-ALIGNED NO-LABEL
     BUTTON-SokDato AT ROW 3.86 COL 34
     RS-Akkumulering AT ROW 5.05 COL 19.2 NO-LABEL
     Btn_OK AT ROW 1.67 COL 62.4
     Btn_Cancel AT ROW 4.48 COL 62.4
     Btn_Help AT ROW 5.86 COL 62.6
     FILL-IN-3 AT ROW 1.24 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 1.91 COL 17.4 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 1.95 COL 38 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 5.19 COL 3 COLON-ALIGNED NO-LABEL
     Btn_OK-2 AT ROW 2.91 COL 62
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
  CREATE WINDOW C-Bilderapport ASSIGN
         HIDDEN             = YES
         TITLE              = "Bestilling av Hovedgrupperapport"
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
/* SETTINGS FOR WINDOW C-Bilderapport
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
/* SETTINGS FOR FILL-IN FILL-IN-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Bilderapport)
THEN C-Bilderapport:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Bilderapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Bilderapport C-Bilderapport
ON END-ERROR OF C-Bilderapport /* Bestilling av Hovedgrupperapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Bilderapport C-Bilderapport
ON WINDOW-CLOSE OF C-Bilderapport /* Bestilling av Hovedgrupperapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Bilderapport
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  apply "CLOSE":U to THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Bilderapport
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Bilderapport
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Start utskrift */
DO:
  run Utskrift ("rpb").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK-2 C-Bilderapport
ON CHOOSE OF Btn_OK-2 IN FRAME DEFAULT-FRAME /* Start excel */
DO:
  run Utskrift ("excel").    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokButik1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik1 C-Bilderapport
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik2 C-Bilderapport
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Bilderapport
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


&Scoped-define SELF-NAME BUTTON-SokDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Bilderapport
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato2
DO:

  def var wTittel as char no-undo.
  assign FI-Dato2 = date(FI-Dato1:screen-value in frame {&FRAME-NAME}).

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato2
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butik1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butik1 C-Bilderapport
ON TAB OF FI-Butik1 IN FRAME DEFAULT-FRAME /* Butikk */
or "RETURN":U of FI-Butik1
DO:
  if int(FI-Butik1:screen-value) > 0 then
    display FI-Butik1:screen-value @ FI-butik2 with frame {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato1 C-Bilderapport
ON TAB OF FI-Dato1 IN FRAME DEFAULT-FRAME /* Dato */
or return of FI-Dato1
DO:
  display FI-Dato1:screen-value @ FI-Dato2 with frame {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Bilderapport 


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
status input "Angi kriterier for utskrift av Hovedgruppe/Dag rapport".
FIND FIRST Butiker NO-LOCK.
    ASSIGN FI-Butik1 = Butiker.Butik.
FIND LAST Butiker NO-LOCK.
    ASSIGN FI-Butik2 = Butiker.Butik.
assign
  FI-Dato1 = today
  FI-Dato2 = today.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Bilderapport  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Bilderapport)
  THEN DELETE WIDGET C-Bilderapport.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Bilderapport  _DEFAULT-ENABLE
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
  DISPLAY FI-Butik1 FI-Butik2 FI-Dato1 FI-Dato2 RS-Akkumulering FILL-IN-3 
          FILL-IN-4 FILL-IN-5 FILL-IN-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Bilderapport.
  ENABLE BUTTON-SokButik1 BUTTON-SokButik2 BUTTON-SokDato-2 FI-Butik1 FI-Butik2 
         FI-Dato1 FI-Dato2 BUTTON-SokDato RS-Akkumulering Btn_OK Btn_Cancel 
         Btn_Help Btn_OK-2 RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Bilderapport.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Bilderapport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Bilderapport 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
DEFINE INPUT  PARAMETER cTyp AS CHARACTER  NO-UNDO.
def var wTekst       as char no-undo.
def var wFilter      as char no-undo.
def var wDato1       as char no-undo.
def var wDato2       as char no-undo.
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

assign frame {&FRAME-NAME}
  FI-Butik1
  FI-Butik2
  FI-Dato1
  FI-Dato2
  RS-Akkumulering
  wDato1 = string(month(FI-Dato1),"99") + "/" +
           string(day(FI-Dato1),"99") + "/" + 
           string(year(FI-Dato1),"9999")
  wDato2 = string(month(FI-Dato2),"99") + "/" +
           string(day(FI-Dato2),"99") + "/" + 
           string(year(FI-Dato2),"9999").

/* Navn på firma */                      
{syspara.i 1 1 100 wKunde}           
wKunde  = "KUNDE = " + wKunde.

/* Rapportens tittel */
if RS-Akkumulering = 1 then
  {syspara.i 6 1 1 wTittel}           
else  
  {syspar2.i 6 1 1 wTittel}           
wTittel = "TITTEL = " + wTittel.

/* Ledetekst kriterier tittel */
{syspara.i 6 1 2 wKriterier}           
wKriterier = "KRITERIER = " + 
             wKriterier + " " + 
             string(FI-Butik1) + "-" + string(FI-Butik2).
/* Ledetekst dato. */
{syspara.i 6 1 3 wTekst}           
wKriterier = wKriterier + " " + wTekst + " " +
             wDato1 + "-" + wDato2.
             
/* Vårt firmanavn */
{syspara.i 1 1 101 wTekst}           
wVi = "VI = " + wTekst.

/* Label forran butikknavn og nummer. */
{syspara.i 6 1 4 wButikkLbl}           
wButikkLbl = "BUTIKK = " + wButikkLbl.

/* Label forran skrevet dato/tid. */
{syspara.i 6 1 5 wSkrevetLbl}           
wSkrevetLbl = "SKREVET = " + wSkrevetLbl.

/* Kollonnelabler. */
{syspara.i 6 1 8 wKollonneLbl}           
IF NUM-ENTRIES(wKollonneLbl) = 11 THEN
    ASSIGN wKollonneLbl = "KOLLONNER = " + wKollonneLbl.
ELSE
    ASSIGN wKollonneLbl = "KOLLONNER = " + cKollonneLblDef.
/* Sidenummerlabl. */
{syspara.i 6 1 7 wSideLbl}
wSideLbl = "SIDE = " + wSideLbl.

if FI-Butik2 < FI-Butik1 then
  do:
    message "Til butikken er mindre enn fra butikken!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.

if FI-Dato2 < FI-Dato1 then
  do:
    message "Til dato er mindre enn fra dato!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.
           
if RS-Akkumulering = 1 then
  assign
    FI-Butik1 = int(FI-Butik1:screen-value in frame DEFAULT-FRAME)
    FI-Butik2 = int(FI-Butik2:screen-value in frame DEFAULT-FRAME)
    wFilter   = "Butiker.Butik >= " + string(FI-Butik1) + " and " + 
                "Butiker.Butik <= " + string(FI-Butik2) + " and " +
                "Dags_Rap.Dato >= " + string(wDato1)  + " and " + 
                "Dags_Rap.dato <= " + string(wDato2).
else
  assign
    FI-Butik1 = int(FI-Butik1:screen-value in frame DEFAULT-FRAME)
    FI-Butik2 = int(FI-Butik2:screen-value in frame DEFAULT-FRAME)
    wFilter   = "Butiker.Butik >= " + string(FI-Butik1) + " and " + 
                "Butiker.Butik <= " + string(FI-Butik2) + " and " +
                "Dags_Rap.Dato >= " + string(wDato1)  + " and " + 
                "Dags_Rap.dato <= " + string(wDato2).
             
{sww.i}
IF cTyp = "excel" THEN
  RUN skrivhgrapexcelSE.p (TRIM(ENTRY(2,wKunde,"=")),
                         TRIM(ENTRY(2,wTittel,"=")),
                         TRIM(ENTRY(2,wKriterier,"=")),
                         TRIM(ENTRY(2,wButikkLbl,"=")),
                         wKollonneLbl,
                         wSideLbl,
                         FI-Butik1,      
                         FI-Butik2,      
                         FI-Dato1,       
                         FI-Dato2,       
                         RS-Akkumulering).
ELSE
    RUN skrivhgrapX.p (TRIM(ENTRY(2,wKunde,"=")),
                           TRIM(ENTRY(2,wTittel,"=")),
                           TRIM(ENTRY(2,wKriterier,"=")),
                           TRIM(ENTRY(2,wButikkLbl,"=")),
                           wKollonneLbl,
                           wSideLbl,
                           FI-Butik1,      
                           FI-Butik2,      
                           FI-Dato1,       
                           FI-Dato2,       
                           RS-Akkumulering,
                           wVi).
{swn.i}
apply "ENTRY":U to FI-Butik1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

