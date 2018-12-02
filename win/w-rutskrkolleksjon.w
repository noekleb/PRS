&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
def input parameter wListeType as char no-undo.

/* Local Variable Definitions ---                                       */
def var wListerRecid          as recid no-undo.
def var wLayout               as int   no-undo.
def var wLSort                as int   no-undo.
def var wIkkePrButikk         as char  no-undo.
def var wTyper                as char  no-undo.
def var wProAntall            as int   no-undo.
DEF VAR wStatistikkutskrifter AS CHAR  NO-UNDO.
/* For styring av jobbkø */
DEF var wJobbNr    as INT  NO-UNDO.
DEF var wDivData   as CHAR NO-UNDO.
DEF var wStatus    as CHAR NO-UNDO.
def var wKriterier as char no-undo.
def var wTittel    as char no-undo.
def var wCl        as int  no-undo.
def var wStTypeId  as char no-undo.

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
&Scoped-Define ENABLED-OBJECTS B-SokListe RECT-53 RECT-9 FI-ListeNr ~
CB-Layout CB-Sortering FI-Kopier Btn_Done B-Utskrift Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-ListeNr FI-Navn CB-Layout CB-Sortering ~
FI-Kopier T-Sidebryt FI-Status 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokListe 
     IMAGE-UP FILE "icon/e-sokpr":U NO-FOCUS
     LABEL "" 
     SIZE 4.6 BY 1.1
     FONT 24.

DEFINE BUTTON B-Utskrift 
     LABEL "&Start utskrift..." 
     SIZE 37 BY 1.14.

DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help DEFAULT 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Layout AS CHARACTER FORMAT "X(256)":U 
     LABEL "Layout" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 73 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sortering AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sortering" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 73 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kopier AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 1 
     LABEL "Ant. kopier" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ListeNr AS INTEGER FORMAT "-zzzzzzzz9":U INITIAL 0 
     LABEL "Liste nr/navn" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Status AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 6.91.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 93 BY 6.19.

DEFINE VARIABLE T-Sidebryt AS LOGICAL INITIAL no 
     LABEL "Sidebryt pr. vareguppe og leverandør" 
     VIEW-AS TOGGLE-BOX
     SIZE 53 BY .81 NO-UNDO.

DEFINE VARIABLE CB-Periode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Periodetype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE T-VisPerLin AS LOGICAL INITIAL no 
     LABEL "Vis periodelinjer" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE BUTTON BUTTON-SokDato1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Fra dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato2 AS DATE FORMAT "99/99/99":U 
     LABEL "Til dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Fra år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinjeNr1 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LinjeNr2 AS INTEGER FORMAT "zz9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilAar AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Til år/Uke" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-SokListe AT ROW 1.95 COL 36.2
     FI-ListeNr AT ROW 2 COL 18 COLON-ALIGNED
     FI-Navn AT ROW 2 COL 39 COLON-ALIGNED NO-LABEL
     CB-Layout AT ROW 3.38 COL 18 COLON-ALIGNED
     CB-Sortering AT ROW 4.81 COL 18 COLON-ALIGNED
     FI-Kopier AT ROW 6.24 COL 18 COLON-ALIGNED
     T-Sidebryt AT ROW 6.33 COL 39
     FI-Status AT ROW 14.81 COL 2 NO-LABEL
     Btn_Done AT ROW 16 COL 2
     B-Utskrift AT ROW 16 COL 31
     Btn_Help AT ROW 16 COL 80
     RECT-53 AT ROW 7.67 COL 2
     RECT-9 AT ROW 1.48 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94.6 BY 16.38.

DEFINE FRAME FRAME-Blank
     CB-Periode AT ROW 1.14 COL 16 COLON-ALIGNED
     T-VisPerLin AT ROW 1.24 COL 41
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.2 ROW 8
         SIZE 90.6 BY 6.38.

DEFINE FRAME FRAME-Progress
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.6 ROW 5.33
         SIZE 89.6 BY 2.

DEFINE FRAME FRAME-StatDato
     BUTTON-SokDato1 AT ROW 1.29 COL 30 NO-TAB-STOP 
     FI-Dato1 AT ROW 1.24 COL 14 COLON-ALIGNED
     FI-Dato2 AT ROW 2.43 COL 14 COLON-ALIGNED
     BUTTON-SokDato2 AT ROW 2.43 COL 30 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.43
         SIZE 43.8 BY 2.86.

DEFINE FRAME FRAME-StatLinje
     FI-FraAar AT ROW 1.24 COL 13.8 COLON-ALIGNED
     FI-LinjeNr1 AT ROW 1.24 COL 22.8 COLON-ALIGNED NO-LABEL
     FI-TilAar AT ROW 2.43 COL 13.8 COLON-ALIGNED
     FI-LinjeNr2 AT ROW 2.43 COL 22.8 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.43
         SIZE 43.8 BY 2.86.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Utskrift av kolleksjon"
         HEIGHT             = 16.38
         WIDTH              = 94.6
         MAX-HEIGHT         = 22.24
         MAX-WIDTH          = 138.2
         VIRTUAL-HEIGHT     = 22.24
         VIRTUAL-WIDTH      = 138.2
         TOP-ONLY           = yes
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
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Blank:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-Progress:FRAME = FRAME FRAME-Blank:HANDLE
       FRAME FRAME-StatDato:FRAME = FRAME FRAME-Blank:HANDLE
       FRAME FRAME-StatLinje:FRAME = FRAME FRAME-Blank:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Navn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Status IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR TOGGLE-BOX T-Sidebryt IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       T-Sidebryt:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FRAME FRAME-Blank
   NOT-VISIBLE                                                          */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-StatLinje:MOVE-AFTER-TAB-ITEM (T-VisPerLin:HANDLE IN FRAME FRAME-Blank)
       XXTABVALXX = FRAME FRAME-StatDato:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-Progress:HANDLE)
       XXTABVALXX = FRAME FRAME-StatLinje:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-StatDato:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME FRAME-Blank:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-Periode IN FRAME FRAME-Blank
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CB-Periode:HIDDEN IN FRAME FRAME-Blank           = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-VisPerLin IN FRAME FRAME-Blank
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       T-VisPerLin:HIDDEN IN FRAME FRAME-Blank           = TRUE.

/* SETTINGS FOR FRAME FRAME-Progress
                                                                        */
/* SETTINGS FOR FRAME FRAME-StatDato
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-StatDato:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-StatLinje
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-StatLinje:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Blank
/* Query rebuild information for FRAME FRAME-Blank
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Blank */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-StatDato
/* Query rebuild information for FRAME FRAME-StatDato
     _Query            is NOT OPENED
*/  /* FRAME FRAME-StatDato */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-StatLinje
/* Query rebuild information for FRAME FRAME-StatLinje
     _Query            is NOT OPENED
*/  /* FRAME FRAME-StatLinje */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME FRAME-Progress:HANDLE
       ROW             = 1.71
       COLUMN          = 2
       HEIGHT          = .71
       WIDTH           = 88
       HIDDEN          = no
       SENSITIVE       = yes.
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {35053A22-8589-11D1-B16A-00C0F0283628} type: ProgressBar */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Utskrift av kolleksjon */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Utskrift av kolleksjon */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokListe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokListe C-Win
ON CHOOSE OF B-SokListe IN FRAME DEFAULT-FRAME
or F10 of B-SokListe
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-ListeNr
    &Program     = d-bLister.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Lister no-lock where
                    recid(Lister) = int(return-value) no-error."
    &ExtraParam  = "wListeType"
  }   
  if available Lister then
    do:
      assign
        wListerRecid = recid(Lister).
      RUN VisListe.
      RUN SideBryt.
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Utskrift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Utskrift C-Win
ON CHOOSE OF B-Utskrift IN FRAME DEFAULT-FRAME /* Start utskrift... */
DO:
  IF NOT AVAILABLE Lister THEN
      FIND Lister NO-LOCK WHERE
      recid(Lister) = wListerRecid NO-ERROR.
  if not available Lister then
  DO:
      MESSAGE "Listepost ikke tilgjengelig."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      return.
  END.
  
  run Utskrift.
  if return-value = "AVBRYT" then
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-StatDato
&Scoped-define SELF-NAME BUTTON-SokDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato1 C-Win
ON CHOOSE OF BUTTON-SokDato1 IN FRAME FRAME-StatDato /* ... */
or F10 of FI-Dato1
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato1 = date(FI-Dato1:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato1
      &Program     = kalender.w
      &Frame       = FRAME-StatDato
      &ExtraParam  = "input wTittel"
    }   
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato2 C-Win
ON CHOOSE OF BUTTON-SokDato2 IN FRAME FRAME-StatDato /* ... */
or F10 of FI-Dato2
DO:
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato2 = date(FI-Dato2:screen-value).

    wTittel = "Statistikkdato".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato2
      &Program     = kalender.w
      &Frame       = FRAME-StatDato
      &ExtraParam  = "input wTittel"
    } 
    return no-apply.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME CB-Layout
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Layout C-Win
ON VALUE-CHANGED OF CB-Layout IN FRAME DEFAULT-FRAME /* Layout */
DO:
  RUN InitSortering.
  assign
    wLayout = int(entry(1,CB-Layout:screen-value,":")).
  display
    CB-Sortering
  with frame {&FRAME-NAME}.
  if can-do(wStatistikkutskrifter,string(wLayout)) then
    run PeriodeFrame (1).
  else
    run PeriodeFrame (0).
  RUN Sidebryt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-Blank
&Scoped-define SELF-NAME CB-Periode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Periode C-Win
ON VALUE-CHANGED OF CB-Periode IN FRAME FRAME-Blank /* Periodetype */
DO:
  assign frame FRAME-Blank
    CB-Periode.
    
  /* "DAG,UKE,MANED,AAR" */
  case lookup(CB-Periode,CB-Periode:List-Items):
    when 1 then /* DAG */
      do:
        assign
          FI-Dato1  = date(1,1,year(today))
          FI-Dato2  = today.          
        hide frame FRAME-StatLinje no-pause.
        display
          FI-Dato1
          FI-Dato2
        with frame FRAME-StatDato.
        view frame FRAME-StatDato.
        apply "entry":U to FI-Dato1 in frame FRAME-StatDato.
      end.
    when 2 then /* UKE */
      do:
        assign
          FI-FraAar:label = "Fra år/uke"
          FI-TilAar:label = "Til år/uke"
          FI-FraAar   = year(today)
          FI-TilAar   = year(today)
          FI-LinjeNr1 = 1
          FI-LinjeNr2 = 53.          
        hide frame FRAME-StatDato no-pause.
        display
          FI-FraAar
          FI-TilAar
          FI-LinjeNr1
          FI-LinjeNr2
        with frame FRAME-StatLinje.
        view frame FRAME-StatLinje.
        apply "entry":U to FI-FraAar in frame FRAME-StatLinje.
      end.
    when 3 then /* MANED */
      do:
        assign
          FI-FraAar:label = "Fra år/mnd"
          FI-TilAar:label = "Til år/mnd"
          FI-FraAar   = year(today)
          FI-TilAar   = year(today)
          FI-LinjeNr1 = 1
          FI-LinjeNr2 = 12.          
        hide frame FRAME-StatDato no-pause.
        display
          FI-FraAar
          FI-TilAar
          FI-LinjeNr1
          FI-LinjeNr2
        with frame FRAME-StatLinje.
        view frame FRAME-StatLinje.
        apply "entry":U to FI-FraAar in frame FRAME-StatLinje.
      end.
    when 4 then /* AAR */
      do:
        assign
          FI-FraAar:label = "Fra år"
          FI-TilAar:label = "Til år"
          FI-FraAar   = year(today)
          FI-TilAar   = year(today)
          FI-LinjeNr1:visible = false
          FI-LinjeNr2:visible = false.          
        hide frame FRAME-StatDato no-pause.
        display
          FI-FraAar
          FI-TilAar
        with frame FRAME-StatLinje.
        view frame FRAME-StatLinje.
        apply "entry":U to FI-FraAar in frame FRAME-StatLinje.
      end.
  end case. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-StatDato
&Scoped-define SELF-NAME FI-Dato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato2 C-Win
ON LEAVE OF FI-Dato2 IN FRAME FRAME-StatDato /* Til dato */
or TAB of FI-Dato2
or RETURN of FI-Dato2
DO:
  apply "entry":U to B-Utskrift in frame DEFAULT-FRAME.
/*   return no-apply. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-StatLinje
&Scoped-define SELF-NAME FI-LinjeNr2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LinjeNr2 C-Win
ON LEAVE OF FI-LinjeNr2 IN FRAME FRAME-StatLinje
or TAB of FI-LinjeNr2
or RETURN of FI-LinjeNr2
DO:
  apply "entry":U to B-Utskrift in frame DEFAULT-FRAME.
/*   return no-apply. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilAar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilAar C-Win
ON LEAVE OF FI-TilAar IN FRAME FRAME-StatLinje /* Til år/Uke */
or TAB of FI-Dato2
or RETURN of FI-Dato2
DO:
  assign frame FRAME-Blank
    CB-Periode.
    
  if CB-Periode = "AAR" then
    do:
      apply "entry":U to B-Utskrift in frame DEFAULT-FRAME.
/*       return no-apply. */
    end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   IF VALID-HANDLE(chCtrlFrame) THEN
       RELEASE OBJECT chCtrlFrame NO-ERROR.
   IF VALID-HANDLE(CtrlFrame) THEN
       DELETE OBJECT CtrlFrame NO-ERROR.
   ASSIGN CtrlFrame   = ?
          chCtrlFrame = ?.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/*{syspara.i 5 100 1 wIkkePrButikk}*/
ASSIGN 
  wIkkePrButikk         = "105,106,620"
  wStatistikkutskrifter = "250,251,252,253,301,401,501,601,701,801,901,1001"
  .
/*
FOR EACH SysPara NO-LOCK WHERE
    SysPara.SysHId = 5 AND
    SysPara.SysGr  = 100:
  ASSIGN
    wIkkePrButikk = wIkkePrButikk + 
                    (IF wIkkePrButikk = ""
                       THEN ""
                       ELSE ",") + 
                    SysPara.Parameter1.
END.
*/

/* Setter sentrallager butikk */
{syspara.i 5 1 1 wCl INT}

/* Bygger en liste over listetyper. */  
wTyper = "".  
for each SysPara no-lock where
  SysPara.SysHId = 7 and
  SysPara.SysGr  = 1:
  
  wTyper = wTyper + 
           (if wTyper = ""
              then ""
              else ",") + 
           SysPara.Parameter1.   
end.    

/* Initierer combo-box for valg av statistikkperiode */
assign
  wStTypeId             = wListeType
  CB-Periode            = "MANED"
  CB-Periode:list-items = "DAG,UKE,MANED,AAR".  

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  ASSIGN
      T-VisPerLin = FALSE
      .
  RUN enable_UI.
  {lng.i} 
  ASSIGN
      T-Sidebryt:HIDDEN = TRUE
      .

  case lookup(wListeType,wTyper):
    when 1  then CURRENT-WINDOW:title = "Utskrift kolleksjonslister".
    when 2  then CURRENT-WINDOW:title = "Utskrift artikkellister".
    when 3  then CURRENT-WINDOW:title = "Utskrift kundelister".
    when 4  then CURRENT-WINDOW:title = "Utskrift varegruppelister".
    when 5  then CURRENT-WINDOW:title = "Utskrift leverandørlister".
    when 6  then CURRENT-WINDOW:title = "Utskrift hovedgruppelister".
    when 7  then CURRENT-WINDOW:title = "Utskrift kassererlister".
    when 8  then CURRENT-WINDOW:title = "Utskrift medlemslister".
    when 9  then CURRENT-WINDOW:title = "Utskrift avdelingsslister".
    when 10 then CURRENT-WINDOW:title = "Utskrift selgerlister".
    otherwise   CURRENT-WINDOW:title = "Ukjent listetype".
  end.
  
  /* NB: Husk at InitListe blir kjørt fra kallende program */
  /*     Frame initiering blir gjordt der.                 */
  
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-rutskrkolleksjon.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    CtrlFrame:NAME = "CtrlFrame":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-rutskrkolleksjon.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DeBugJobb C-Win 
PROCEDURE DeBugJobb :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter wStatus as char no-undo.

/* DEBUG */
find Jobb no-lock where Jobb.JobbNr = wJobbNr.
message "Statu: " wStatus skip(1)
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY FI-ListeNr FI-Navn CB-Layout CB-Sortering FI-Kopier T-Sidebryt 
          FI-Status 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-SokListe RECT-53 RECT-9 FI-ListeNr CB-Layout CB-Sortering FI-Kopier 
         Btn_Done B-Utskrift Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Dato1 FI-Dato2 
      WITH FRAME FRAME-StatDato IN WINDOW C-Win.
  ENABLE BUTTON-SokDato1 FI-Dato1 FI-Dato2 BUTTON-SokDato2 
      WITH FRAME FRAME-StatDato IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-StatDato}
  DISPLAY FI-FraAar FI-LinjeNr1 FI-TilAar FI-LinjeNr2 
      WITH FRAME FRAME-StatLinje IN WINDOW C-Win.
  ENABLE FI-FraAar FI-LinjeNr1 FI-TilAar FI-LinjeNr2 
      WITH FRAME FRAME-StatLinje IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-StatLinje}
  VIEW FRAME FRAME-Progress IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Progress}
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Blank}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FremdriftProgressBar C-Win 
PROCEDURE FremdriftProgressBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wLokAntall as int no-undo.

  IF wLOKAntall > chCtrlFrame:ProgressBar:max THEN
      RETURN.

  if wLOKAntall >= 5 then
    chCtrlFrame:ProgressBar:Value = wLOKAntall.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSideBryt C-Win 
PROCEDURE GetSideBryt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER lSideBryt AS LOG INITIAL FALSE NO-UNDO.

  ASSIGN FRAME Default-Frame
      T-SideBryt
      lSideBryt = T-SideBryt
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideProgressBar C-Win 
PROCEDURE HideProgressBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  assign
    chCtrlFrame:Visible = false.
  hide frame FRAME-Progress no-pause.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLayout C-Win 
PROCEDURE InitLayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".
    
  do with frame DEFAULT-FRAME:  
  case lookup(wListeType,wTyper):
    when 1 then /* Kolleksjonsrapporter */
      {rutskrkolleksjon.i &SysHId=6 &Fra=100 &Til=129 &Ekstra=" "}
    when 2 then /* Artikkelrapporter */
      {rutskrkolleksjon.i &SysHId=6 &Fra=150 &Til=299 &Ekstra=" "}
    when 3 then /* Kundelister - pt 300 */
      {rutskrkolleksjon.i &SysHId=6 &Fra=601 &Til=699 &Ekstra=" "}
    when 4 then /* Varegruppelister */
      {rutskrkolleksjon.i &SysHId=6 &Fra=301 &Til=399 &Ekstra=" "}
    when 5 then /* Varegruppelister */
      {rutskrkolleksjon.i &SysHId=6 &Fra=400 &Til=499 &Ekstra=" "}
    when 6 then /* Hovedgruppelister */
      {rutskrkolleksjon.i &SysHId=6 &Fra=500 &Til=599 &Ekstra=" "}
    when 7 then /* Selgerlister */
      {rutskrkolleksjon.i &SysHId=6 &Fra=700 &Til=799 &Ekstra=" "}
    when 8 then /* Medlemslister */
      {rutskrkolleksjon.i &SysHId=6 &Fra=800 &Til=899 &Ekstra=" "}
    when 9 then /* Avdelingslister */
      {rutskrkolleksjon.i &SysHId=6 &Fra=900 &Til=999 &Ekstra=" "}
    when 10 then /* Selger */
      {rutskrkolleksjon.i &SysHId=6 &Fra=1000 &Til=1999 &Ekstra=" "}
    otherwise
      do:
        message "Fra w-rutskriftkoleksjon:initlayout: lookup(wListeType,wTyper):" lookup(wListeType,wTyper) skip
                "wListeType:" wListeType skip
                "wTyper:" wTyper view-as alert-box.
      end.
  end case.
  assign  
    wLayout = int(entry(1,entry(1,CB-Layout:list-items),":")).
  end. /* FRAMESCOOP */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitListe C-Win 
PROCEDURE InitListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def input parameter wRecid as int no-undo.
  
  assign
    wListerRecid = wRecid.
  
  run VisListe.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitSortering C-Win 
PROCEDURE InitSortering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".
    
  /* Setter opp frame-scoop. */
  do with frame DEFAULT-FRAME:  
  assign
    CB-Sortering = " "
    CB-Sortering:List-Items = " ".

  for each SysPara no-lock where
    SysPara.SysHId = 9 and
    SysPara.SysGr  = int(entry(1,CB-Layout:screen-value in frame DEFAULT-FRAME,":")):
          
    CB-Sortering:List-items = CB-Sortering:list-items + 
                              (if CB-Sortering:list-items = ""
                                 then ""
                                 else ",") + 
                               string(SysPara.ParaNr,"zzz9") + ": " + 
                               SysPara.Parameter1.
  end.      
  assign
    CB-Sortering = entry(1,CB-Sortering:list-items).
  end. /* FRAMESCOOP */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PeriodeFrame C-Win 
PROCEDURE PeriodeFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def input parameter ipLayout as int no-undo.

do with frame DEFAULT-FRAME:  

  if ipLayout = 1 then
   do:
     view frame FRAME-StatDato. 
     view frame FRAME-StatLinje. 
     assign
       CB-Periode:visible in frame FRAME-Blank = true
       CB-Periode:sensitive in frame FRAME-Blank = true
       T-VisPerLin:visible in frame FRAME-Blank = true
       T-VisPerLin:sensitive in frame FRAME-Blank = true
       .
     display CB-Periode with frame FRAME-BLANK.
     apply "VALUE-CHANGED":U to CB-Periode in frame FRAME-Blank.
   end.
 else do:
     assign
       CB-Periode:visible in frame FRAME-Blank = false
       T-VisPerLin:visible in frame FRAME-Blank = false
       .
     hide frame FRAME-StatDato no-pause. 
     hide frame FRAME-StatLinje no-pause. 
 end.
end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SideBryt C-Win 
PROCEDURE SideBryt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF can-do("200,201,202",trim(entry(1,CB-Layout:SCREEN-VALUE  IN FRAME default-frame,":"))) THEN
    ASSIGN
        T-Sidebryt:HIDDEN    = FALSE
        T-Sidebryt:sensitive = TRUE
          .
  ELSE 
    ASSIGN
      T-Sidebryt:HIDDEN    = true
      T-Sidebryt:sensitive = FALSE
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Utskrift C-Win 
PROCEDURE Utskrift :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wInnData           as char no-undo.
  def var wPeriTekst         as char no-undo.
  def var wLagerListeInnhold as int  no-undo.
  DEF VAR pdFraDato          AS DATE NO-UNDO.
  DEF VAR pdTilDato          AS DATE NO-UNDO.
  
  /* Hvis listeunderlaget er oppdatert pr. butikk sjekkes det om layoutet tillater dette. */
  if can-do(wIkkePrButikk,trim(entry(1,CB-Layout:screen-value in frame DEFAULT-FRAME,":"))) then
    do:  
      if Lister.Kriterier[ 8] = "TRUE" then
        do:
          message "Rapport " + CB-Layout:screen-value in frame DEFAULT-FRAME + "." skip(1)
                  "Kan ikke benyttes på listeunderlag som er oppdatert pr. butikk!"
                  view-as alert-box message title "Melding".
          return no-apply.
        end.
    end. /* Sjekk pr. butikk */

  {sww.i}

  assign frame FRAME-Blank
    CB-Periode
    T-VisPerLin.
  ASSIGN
      T-SideBryt.

  /* "DAG,UKE,MANED,AAR" */
  case lookup(CB-Periode,CB-Periode:List-Items):
    when 1 then /* DAG */
      do with frame FRAME-StatDato:
        assign 
          wInnData = string(year(input FI-Dato1)) + "," + 
                     string(year(input FI-Dato2)) + "," +  
                     string(input FI-Dato1 - date(1,1,year(input FI-Dato1)) + 1) + "," +  
                     string(input FI-Dato2 - date(1,1,year(input FI-Dato2)) + 1)
          wPeriTekst = "      " + string(input FI-Dato1) + " - " + string(input FI-Dato2).                       
      end.
    when 4 then /* AAR */
      do with frame FRAME-StatLinje:
        assign 
          wInnData = string(input FI-FraAar) + "," + 
                     string(input FI-TilAar) + ",1,1"
          wPeriTekst = "      " + string(input FI-FraAar) + " - " + string(input FI-TilAar).  
      end.    
    otherwise /* Alle andre periodevalg */
      do with frame FRAME-StatLinje:
        assign 
          wInnData = string(input FI-FraAar) + "," + 
                     string(input FI-TilAar) + "," +  
                     string(input FI-LinjeNr1) + "," +  
                     string(input FI-LinjeNr2)
          wPeriTekst = "      " + string(input FI-FraAar) + "/" + string(input FI-LinjeNr1)
                                + " - " +
                                string(input FI-TilAar) + "/" + string(input FI-LinjeNr2) + 
                                "  (" + CB-Periode + ")".                       
      end.    
  end case.

  if wInnData = ? then 
    wInnData = "".
  else 
    wInnData = wInnData + 
               ",<Ledig>" + "," + 
               CB-Periode + "," + 
               (if T-VisPerLin then "yes" else "false") + "," +
               wPeriTekst.
  assign
    FI-Status = "".
  display FI-Status with frame DEFAULT-FRAME.

  /* Kobling mot nytt utskriftsprogram */
  assign
    wLayout = int(entry(1,CB-Layout:screen-value in frame DEFAULT-FRAME,":"))
    wLSort  = int(entry(1,CB-Sortering:screen-value in frame DEFAULT-FRAME,":")).
  
  /* For lagerlisten skal det spørres om hvilken innhold lagerlisten skal ha. */
  if can-do("150,200,201,202,212,213",string(wLayout)) then
    do:
      run d-sjekklager.w (output wLagerListeInnhold).
      if return-value = "AVBRYT" then
        return "AVBRYT".
      /* I wInndata legges parameter i entry 5. */
      /* wInnData er blank her for liste 150.   */
      ENTRY(1, wInnData) = string(wLagerListeInnhold).
    end.
  /* Kontoutskriften skal spørre etter datoavgrensing */
  IF CAN-DO("620",STRING(wLayout)) THEN
  DO:
      RUN d-kontoperiode.w (OUTPUT pdFraDato, OUTPUT pdTilDato).
      IF RETURN-VALUE = "AVBRYT" THEN
          RETURN "AVBRYT".
      /* I wInndata legges parameter i entry 5. */
      ENTRY(5, wInnData) = string(pdFraDato) + "|" + string(pdTilDato).
  END.
  /* Teller opp antall linjer som skal behandles. */
  wPROAntall = 0.
  for each ListeLinje of Lister no-lock:
    wPROAntall = wPROAntall + 1.
  end.
  /* Init av progressbar */ 
  RUN VisProgressBar.
  run skrivkolleksjon.p (wListerRecid, wLayout, wLSort, string(input FI-Kopier), wInnData, this-procedure).
  if return-value = "AVBRYT" then
    do:
      message "Feil ved utskrift av datasett!"
        view-as alert-box error title "Utskriftsfeil".
      RUN HideProgressBar.
      return no-apply.
    end.
  RUN HideProgressBar.
  
  assign
    FI-Status = "".
  display FI-Status with frame DEFAULT-FRAME.
    
  {swn.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisListe C-Win 
PROCEDURE VisListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  find Lister no-lock where
    recid(Lister) = wListerRecid no-error.
  if not available Lister then
    return "AVBRYT".
    
  RUN InitLayout.
  
  /* Default er allt valgt. */
  assign
    FI-ListeNr = Lister.ListeNr
    FI-Navn    = Lister.Beskrivelse.
    
  display
    CB-Layout
    FI-ListeNr
    FI-Navn
  with frame {&FRAME-NAME}.
  
  run InitSortering.
  display
    CB-Sortering
  with frame {&FRAME-NAME}.

  if can-do(wStatistikkutskrifter,string(wLayout)) then
    RUN PeriodeFrame (1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisProgressBar C-Win 
PROCEDURE VisProgressBar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  if wPROAntall < 5 then 
    return. /* For få poster */
    
  view frame FRAME-Progress.
  assign
    chCtrlFrame:Visible = true
    chCtrlFrame:ProgressBar:Min     = 1
    chCtrlFrame:ProgressBar:Max     = wPROAntall
    chCtrlFrame:ProgressBar:Value   = 1.
    
  wPROAntall = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

