&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Plukkliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Plukkliste 
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

DEF VAR wButListe as CHAR NO-UNDO.
DEF VAR wDato1    as DATE NO-UNDO.
DEF VAR wDato2    as DATE NO-UNDO.
DEF VAR wVg1      as INT  NO-UNDO.
DEF VAR wVg2      as INT  NO-UNDO.
DEF VAR wLopNr1   as INT  NO-UNDO.
DEF VAR wLopNr2   as INT  NO-UNDO.
DEF VAR wButik1   as INT  NO-UNDO.
DEF VAR wButik2   as INT  NO-UNDO.
DEF VAR wT-PrDag  as LOG  NO-UNDO.
DEF VAR wT-Lager  AS LOG  NO-UNDO.
DEF VAR iAntButikk AS INT  NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEFINE VARIABLE lHoppaOver AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cHoppaOver AS CHARACTER   NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS T-Lager T-Alle T-PrDag FI-Butik1 FI-Butik2 ~
FI-Dato1 BUTTON-SokButik1 FI-Dato2 FI-Vg1 FI-Vg2 FI-LopNr1 BUTTON-SokVg2 ~
FI-LopNr2 FI-Plukkbutikk Btn_OK BUTTON-SokDato-2 Btn_Cancel Btn_Help ~
BUTTON-SokVg1 BUTTON-SokButik2 BUTTON-SokDato BUTTON-SokButik-2 RECT-1 ~
RECT-2 
&Scoped-Define DISPLAYED-OBJECTS T-Lager T-Alle FI-Info T-PrDag FI-Butik1 ~
FI-Butik2 FI-Dato1 FI-Dato2 FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 ~
FI-Plukkbutikk FILL-IN-3 FILL-IN-4 FILL-IN-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Plukkliste AS WIDGET-HANDLE NO-UNDO.

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

DEFINE BUTTON BUTTON-SokButik-2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

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

DEFINE BUTTON BUTTON-SokVg1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokVg2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butik1 AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butik2 AS INTEGER FORMAT "zzzzz9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato2 AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 77 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr1 AS INTEGER FORMAT "zzz9":U INITIAL 1 
     LABEL "Løpenummer" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr2 AS INTEGER FORMAT "zzzzz9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Plukkbutikk AS INTEGER FORMAT "zzzzz9":U INITIAL 0 
     LABEL "Plukkbutikk" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg1 AS INTEGER FORMAT "zzz9":U INITIAL 1 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg2 AS INTEGER FORMAT "zzzzz9":U INITIAL 999999 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

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
     SIZE 59.2 BY 9.81.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 17.6 BY 9.81.

DEFINE VARIABLE T-Alle AS LOGICAL INITIAL no 
     LABEL "Alle poster" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.6 BY .81 NO-UNDO.

DEFINE VARIABLE T-Lager AS LOGICAL INITIAL yes 
     LABEL "Vis lager pr butikk/str." 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81 NO-UNDO.

DEFINE VARIABLE T-PrDag AS LOGICAL INITIAL no 
     LABEL "Sidebryt pr. dag" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     T-Lager AT ROW 9.86 COL 17
     T-Alle AT ROW 8.67 COL 38.4
     FI-Info AT ROW 11.57 COL 2 NO-LABEL
     T-PrDag AT ROW 8.67 COL 17
     FI-Butik1 AT ROW 2.62 COL 15 COLON-ALIGNED
     FI-Butik2 AT ROW 2.62 COL 36 COLON-ALIGNED NO-LABEL
     FI-Dato1 AT ROW 3.86 COL 15 COLON-ALIGNED
     BUTTON-SokButik1 AT ROW 2.67 COL 33.2
     FI-Dato2 AT ROW 3.86 COL 36 COLON-ALIGNED NO-LABEL
     FI-Vg1 AT ROW 5.05 COL 15 COLON-ALIGNED
     FI-Vg2 AT ROW 5.05 COL 36 COLON-ALIGNED NO-LABEL
     FI-LopNr1 AT ROW 6.24 COL 15 COLON-ALIGNED
     BUTTON-SokVg2 AT ROW 5.05 COL 54.6
     FI-LopNr2 AT ROW 6.24 COL 36 COLON-ALIGNED NO-LABEL
     FI-Plukkbutikk AT ROW 7.48 COL 15 COLON-ALIGNED
     Btn_OK AT ROW 1.67 COL 62.4
     BUTTON-SokDato-2 AT ROW 3.86 COL 54.6
     Btn_Cancel AT ROW 2.91 COL 62.4
     Btn_Help AT ROW 9.86 COL 63
     BUTTON-SokVg1 AT ROW 5.05 COL 33.2
     BUTTON-SokButik2 AT ROW 2.67 COL 54.6
     BUTTON-SokDato AT ROW 3.86 COL 33.2
     FILL-IN-3 AT ROW 1.24 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN-4 AT ROW 1.95 COL 15 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 1.95 COL 36 COLON-ALIGNED NO-LABEL
     BUTTON-SokButik-2 AT ROW 7.48 COL 33.2
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 1.48 COL 61.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.4 BY 12.


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
  CREATE WINDOW C-Plukkliste ASSIGN
         HIDDEN             = YES
         TITLE              = "Bestilling av plukkliste"
         HEIGHT             = 12
         WIDTH              = 78.4
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
/* SETTINGS FOR WINDOW C-Plukkliste
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-4 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Plukkliste)
THEN C-Plukkliste:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Plukkliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Plukkliste C-Plukkliste
ON END-ERROR OF C-Plukkliste /* Bestilling av plukkliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Plukkliste C-Plukkliste
ON WINDOW-CLOSE OF C-Plukkliste /* Bestilling av plukkliste */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Plukkliste
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  apply "CLOSE":U to THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Plukkliste
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Plukkliste
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* Start utskrift */
DO:




  assign frame {&FRAME-NAME}
    FI-Butik1
    FI-Butik2
    FI-Plukkbutikk
    FI-Vg1
    FI-Vg2
    FI-LopNr1
    FI-LopNr2
    FI-Dato1
    FI-Dato2
    T-PrDag
    T-Alle
    T-Lager
    .
  iAntButikk = 0.
  IF FI-Plukkbutikk > 0 AND NOT CAN-FIND(butiker WHERE butiker.butik = FI-Plukkbutikk) THEN DO:
      MESSAGE "Feil Plukkbutikk" SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO FI-Plukkbutikk.
      RETURN NO-APPLY.
  END.
  FOR EACH Butiker NO-LOCK WHERE
      Butiker.Butik >= FI-Butik1 AND
      Butiker.Butik <= FI-Butik2:

      iAntButikk = iAntButikk + 1.
  END.
  IF iAntButikk > 3 THEN 
  DO:
      MESSAGE "Maks 3 butikker kan tas med i utvalget." SKIP
          iantbutikk fi-butik1 fi-butik2
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF INPUT FI-Plukkbutikk <> 0 AND iAntButikk > 1 THEN DO:
      MESSAGE "Maks 1 butikk kan tas med i utvalget" SKIP
          "ved angitt plukkbutikk"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  FI-Info = "Bygger datasett.....".
  display FI-Info with frame DEFAULT-FRAME.

  {sww.i}

  
  run ByggJobbLinje.
  if return-value = "AVBRYT" then
    do:
      {swn.i}
      return no-apply.
    end.
    
  run Utskrift.      
  {swn.i}

  FI-Info = " ".
  display FI-Info with frame DEFAULT-FRAME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokButik-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik-2 C-Plukkliste
ON CHOOSE OF BUTTON-SokButik-2 IN FRAME DEFAULT-FRAME /* ... */
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Plukkbutikk
    &Program     = d-bbutiker.w
    &Frame       = {&FRAME-NAME}
  }   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokButik1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik1 C-Plukkliste
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokButik2 C-Plukkliste
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Plukkliste
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato-2 C-Plukkliste
ON CHOOSE OF BUTTON-SokDato-2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato2
DO:

  def var wTittel as char no-undo.
  assign FI-Dato2 = date(FI-Dato2:screen-value in frame {&FRAME-NAME}).

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


&Scoped-define SELF-NAME BUTTON-SokVg1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg1 C-Plukkliste
ON CHOOSE OF BUTTON-SokVg1 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Vg1
DO:

  do with frame {&FRAME-NAME}:  
  cTekst = "Vg".
  RUN JBoxDLookup.w ("VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                     "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK",
                     INPUT-OUTPUT cTekst).


  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  ELSE FI-Vg1:SCREEN-VALUE = cTekst.
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokVg2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokVg2 C-Plukkliste
ON CHOOSE OF BUTTON-SokVg2 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Vg2
DO:

  do with frame {&FRAME-NAME}:  

      cTekst = "Vg".
      RUN JBoxDLookup.w ("VarGr;vg;vgBeskr,HuvGr;Hg;HgBeskr,Moms;MomsProc@3,Avdeling;AvdelingNr;AvdelingNavn",
                         "WHERE true,FIRST HuvGr OF VarGr NO-LOCK, FIRST Moms of VarGr, FIRST Avdeling OF HuvGr NO-LOCK",
                         INPUT-OUTPUT cTekst).


      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN NO-APPLY.
      ELSE FI-Vg2:SCREEN-VALUE = cTekst.
  
  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butik1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butik1 C-Plukkliste
ON TAB OF FI-Butik1 IN FRAME DEFAULT-FRAME /* Butikk */
or "RETURN":U of FI-Butik1
DO:
  if int(FI-Butik1:screen-value) > 0 then
    display FI-Butik1:screen-value @ FI-butik2 with frame {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Alle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Alle C-Plukkliste
ON VALUE-CHANGED OF T-Alle IN FRAME DEFAULT-FRAME /* Alle poster */
DO:
  assign
    T-Alle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Plukkliste 


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
status input "Angi kriterier for utskrift av plukkliste".

{syspara.i 1 20 1 cHoppaOver}

lHoppaOver = cHoppaOver = "1".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggJobbLinje C-Plukkliste 
PROCEDURE ByggJobbLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wKriterier as char no-undo.
  def var wStatus    as char no-undo.
  
  do with frame DEFAULT-FRAME:
    /* Pakker Recid til Lister */
    assign
      wKriterier = string(input FI-Butik1) + "," +
                   string(input FI-Butik2) + "|" + string(input FI-Plukkbutikk) + "," + 
                   string(month(input FI-Dato1)) + "|" +
                   string(day(input FI-Dato1)) + "|" +
                   string(year(input FI-Dato1),"9999") + "," +
                   string(month(input FI-Dato2)) + "|" +
                   string(day(input FI-Dato2)) + "|" +
                   string(year(input FI-Dato2),"9999") + "," +
                   string(input FI-Vg1) + "," + 
                   string(input FI-Vg2) + "," + 
                   string(input FI-LopNr1) + "," + 
                   string(input FI-LopNr2) + "," + 
                   (if input T-PrDag 
                     then "TRUE"
                     else "FALSE")  + "," + 
                   (if input T-Alle 
                     then "TRUE"
                     else "FALSE")  + "," +
                   (if input T-Lager 
                     then "TRUE"
                     else "FALSE")
                   .
  end.
                 
  
  /* Finner første ledige jobbnummer. */
  run finnjobb.p (input-output wJobbNr, input-output wDivData, output wStatus).
  if wStatus <> "OK" then
    return "AVBRYT".
  
  /* Sender med JobbNr og programnavn */
  assign
    wDivData = 
      string(wJobbNr)  + ";" + /* Jobbnummer */
      program-name(1)  + ";" + /* Bestillende program */
      "x-plukkliste"   + ";" + /* Program som skal utføres */
      ""               + ";" + /* Start dato */
      ""               + ";" + /* Start tid */
      userid("dictdb") + ";" + /* Brukerid på den som bestiller jobben. */
      "F"              + ";" + /* Startes av - Forgrunn */
      "Plukkliste "    + ";" + /* Merknad */
      wKriterier.              /* Kriterier for utskriftsjobb. */
  
  /* Starter program som oppretter jobbrecord. */
  run opprjobb.p (input-output wDivData, output wStatus).
  if wStatus <> "OK" then
    do:
      run DeBugJobb ("Etter Opprett jobb: " + wStatus).
      return "AVBRYT".
    end.
    
  /* Starter oppdatering av datasett i forgrunnen */
  run runjobb.p (input wJobbNr, input-output wDivData, output wStatus,this-procedure).
  if wStatus <> "OK" then
    do:
      run DeBugJobb ("Etter oppdatering av datasett: " + wStatus).
      return no-apply "AVBRUTT".
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeBugJobb C-Plukkliste 
PROCEDURE DeBugJobb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter wStatus as char no-undo.

/* DEBUG */
find Jobb no-lock where Jobb.JobbNr = wJobbNr.
message 
    "Status:" wStatus skip(1)
    "Jobbrecord:" Jobb.JobbNr skip
    "Bestilt av" Jobb.BestiltAv    skip
    "Bestillingsdato:" Jobb.BestillingsDato skip
    "Bestillingstid:" STRING(Jobb.BestillingsTid,"HH:MM:SS") skip
    "Startprogram:" Jobb.StartProgram skip
    "Esekprogram:" Jobb.EksekProgram skip
    "Startes av:" Jobb.StartesAv    skip
    "Merknad:" Jobb.Merknad      skip
    "Kriterier:" Jobb.Kriterier    skip
    "StartDato:" Jobb.StartDato    skip
    "StartTid:" STRING(Jobb.StartTid,"HH:MM:SS")
    "FerdigDato:" Jobb.FerdigDato skip
    "FerdigTid:" string(Jobb.FerdigTid,"HH:MM:SS") skip
    "Tidsbruk:" string(Jobb.FerdigTid - Jobb.StartTid,"HH:MM:SS") skip
    "JobbStatus:" jobb.JobbStatus.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Plukkliste  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Plukkliste)
  THEN DELETE WIDGET C-Plukkliste.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Plukkliste  _DEFAULT-ENABLE
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
  DISPLAY T-Lager T-Alle FI-Info T-PrDag FI-Butik1 FI-Butik2 FI-Dato1 FI-Dato2 
          FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 FI-Plukkbutikk FILL-IN-3 FILL-IN-4 
          FILL-IN-5 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Plukkliste.
  ENABLE T-Lager T-Alle T-PrDag FI-Butik1 FI-Butik2 FI-Dato1 BUTTON-SokButik1 
         FI-Dato2 FI-Vg1 FI-Vg2 FI-LopNr1 BUTTON-SokVg2 FI-LopNr2 
         FI-Plukkbutikk Btn_OK BUTTON-SokDato-2 Btn_Cancel Btn_Help 
         BUTTON-SokVg1 BUTTON-SokButik2 BUTTON-SokDato BUTTON-SokButik-2 RECT-1 
         RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Plukkliste.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Plukkliste.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPlukkbutikk C-Plukkliste 
PROCEDURE getPlukkbutikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER iPlukkbutikk AS INTEGER     NO-UNDO.
    iPlukkbutikk = FI-Plukkbutikk.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nullstill C-Plukkliste 
PROCEDURE Nullstill :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var wAntall as int no-undo.

BUTIKK:
FOR EACH Butiker NO-LOCK where
  Butiker.Butik >= FI-Butik1 and
  Butiker.Butik <= FI-Butik2:

  TRANSLOGG:
  FOR EACH TransLogg WHERE
    TransLogg.Plukket = false and
    TransLogg.Butik   = Butiker.Butik and
    TransLogg.Vg     >= FI-Vg1 and
    TransLogg.Vg     <= FI-Vg2 and
    TransLogg.LopNr  >= FI-LopNr1 and
    TransLogg.LopNr  <= FI-LopNr2 and
    TransLogg.Dato   >= FI-Dato1 and
    TransLogg.Dato   <= FI-Dato2
    TRANSACTION:
    
    wAntall = wAntall + 1.
    FI-Info = "Nullstiller translogg " + string(wAntall).
    if wAntall modulo 25 = 0 then
      display FI-Info with frame DEFAULT-FRAME.    
      
    assign
      TransLogg.Plukket = true.

  END. /* TRANSLOGG */
END. /* BUTIKK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Plukkliste 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
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
def var wVgLbl       as char no-undo.
def var wLopNrLbl    as char no-undo.
def var wVi          as char no-undo.
def var wKollonner   as char no-undo.
def var wAntall      as int  no-undo.

def var wOTHER-PARAMETERS  as char no-undo.
def var wRB-REPORT-LIBRARY as char no-undo.
def var wRB-REPORT-NAME    as char no-undo.
def var wRB-DB-CONNECTION  as char no-undo.

assign
  wDato1 = string(FI-Dato1)
  wDato2 = string(FI-Dato2).
/*
  wDato1 = string(month(FI-Dato1),"99") + "/" +
           string(day(FI-Dato1),"99") + "/" + 
           string(year(FI-Dato1),"9999")
  wDato2 = string(month(FI-Dato2),"99") + "/" +
           string(day(FI-Dato2),"99") + "/" + 
           string(year(FI-Dato2),"9999").
*/           

if FI-Butik2 < FI-Butik1 then
  do:
    message "Til butikken er mindre enn fra butikken!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.
if FI-Vg2 < FI-Vg1 then
  do:
    message "Til varegruppen er mindre enn fra varegruppen!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.
if FI-LopNr2 < FI-LopNr1 then
  do:
    message "Til løpenummer er mindre enn fra løpenummer!"
      view-as alert-box message title "Melding".
    return no-apply.
  end.

/* Bygger liste over butikker. */
assign
  wButListe = ""
  wAntall   = 0.
BUTIKKLISTE:
FOR EACH Butiker NO-LOCK where
  Butiker.Butik >= FI-Butik1 and
  Butiker.Butik <= FI-Butik2:

  /* Maks antall butikker som kan spesifiseres. */
  wAntall = wAntall + 1.
  if wAntall > 10 then
    LEAVE BUTIKKLISTE.

  wButListe = wButListe +
              (if wButListe = ""
                 THEN ""
                 ELSE ",") +
              STRING(Butiker.Butik).
END. /* BUTIKKLISTE */

/* Navn på firma */                      
{syspara.i 1 1 100 wKunde}           
wKunde  = "KUNDE = " + wKunde.

/* Oppkobling av Work database */                      
if valid-handle(wLibHandle) then
  run RpbWrDB in wLibHandle (output wRB-DB-CONNECTION).
if wRB-DB-CONNECTION = "" then
 {syspara.i 1 1 11 wRB-DB-CONNECTION}

/* Rapportens tittel */
if T-PrDag then
  {syspara.i 6 130 1 wTittel}           
else
  {syspar2.i 6 130 1 wTittel}           
wTittel = "TITTEL = " + wTittel.

/* Ledetekst varegruppe */
{syspara.i 6 130 8 wVgLbl}           
wVgLbl = "VAREGR = " + wVgLbl.

/* Ledetekst løpenummer */
{syspara.i 6 130 9 wLopNrLbl}           
wLopNrLbl = "LOPNR = " + wLopNrLbl.

/* Ledetekst kriterier tittel */
do:
  {syspara.i 6 130 2 wKriterier}           
  wKriterier = "KRITERIER = " + 
               wKriterier + " " + 
               string(FI-Butik1) + "-" + string(FI-Butik2).
  {syspara.i 6 130 3 wTekst}           
  wKriterier = wKriterier + " " + wTekst + " " +
               wDato1 + "-" + wDato2.
  {syspara.i 6 130 8 wTekst}           
  wKriterier = wKriterier + " " + wTekst + " " +
               string(FI-Vg1) + "-" + string(FI-Vg2).
  {syspara.i 6 130 9 wTekst}           
  wKriterier = wKriterier + " " + wTekst + " " +
               string(FI-LopNr1) + "-" + string(FI-LopNr2).
end. 
             
/* Vårt firmanavn */
{syspara.i 1 1 101 wTekst}           
wVi = "VI = " + wTekst.

/* Label forran butikknavn og nummer. */
{syspara.i 6 130 4 wButikkLbl}           
wButikkLbl = "BUTIKK = " + wButikkLbl.

/* Label forran skrevet dato/tid. */
{syspara.i 6 130 5 wSkrevetLbl}           
wSkrevetLbl = "SKREVET = " + wSkrevetLbl.

/* Kollonnelabler. */
{syspara.i 6 130 6 wKollonneLbl}           
wKollonneLbl = "KOLLONNER = " + wKollonneLbl.

/* Sidenummerlabl. */
{syspara.i 6 130 7 wSideLbl}           
wSideLbl = "SIDE = " + wSideLbl.

assign
  wButListe  = "BUTIKKLISTE = " + wButListe + " , , , , , , , , , , , , , , , , , , , , , , , , , "
  wFilter    = "JobbLinje.JobbNr = " + string(wJobbNr)
  /*
  wFilter    = "JobbLinje.JobbNr = " + string(wJobbNr) +
               (if T-Alle 
                  then ""
                  else " and JobbLinje.DecX[49] > 0 ")
  */
  wBrukerId  = "BRUKER = " + userid("dictdb")
  wRB-REPORT-LIBRARY   = "rpb\plukkliste.prl"           
  wRB-REPORT-NAME      = (IF T-Lager
                            THEN "Plukkliste2"
                            ELSE "Plukkliste")               
  wOTHER-PARAMETERS    = wBrukerId    + "~n" + 
                         wKunde       + "~n" +
                         wTittel      + "~n" + 
                         wKriterier   + "~n" + 
                         wButikkLbl   + "~n" + 
                         wSkrevetLbl  + "~n" + 
                         wKollonneLbl + "~n" + 
                         wSideLbl     + "~n" +
                         wVi          + "~n" +                      
                         wVgLbl       + "~n" +                      
                         wButListe    + "~n" +
                         wLopNrLbl.

FI-Info = "Starter utskrift.... ".
display FI-Info with frame DEFAULT-FRAME.    

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
apply "ENTRY":U to FI-Butik1.

IF NOT lHoppaOver THEN DO:
    message "Skal transaksjonene merkes som plukket?"
            view-as alert-box question button yes-no title "Bekreftelse"
            update wSvar as log.
    if wSvar then
      run Nullstill.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisInfo C-Plukkliste 
PROCEDURE VisInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wFI-Info as char no-undo.

  assign
    FI-Info = wFI-Info.
  display FI-Info with frame DEFAULT-FRAME.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

