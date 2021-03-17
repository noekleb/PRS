&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE cFilNavn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCols          AS INTEGER EXTENT 15  NO-UNDO.
DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iToRight       AS INTEGER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageHeight    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFirmanavn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAdress        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPostadress AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cButiksnr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cButiksnamn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAlle AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBildkatalog AS CHARACTER   NO-UNDO.
{ pdf_inc.i "THIS-PROCEDURE"}


DEFINE TEMP-TABLE ttVgHdr NO-UNDO
    FIELD vg    AS INTE
    FIELD soldAar1Lbl AS CHAR
    FIELD soldAar2Lbl AS CHAR
    FIELD lager    AS INTE
    FIELD budget   AS INTE
    FIELD lev      AS INTE
    INDEX vg IS PRIMARY UNIQUE vg.

DEFINE TEMP-TABLE ttVgRow NO-UNDO
    FIELD vg    AS INTE
    FIELD butik AS INTE
    FIELD soldAar1 AS INTE
    FIELD soldAar2 AS INTE
    FIELD sumfsg1 AS DECI
    FIELD sumfsg2 AS DECI
    FIELD sumvarekost1 AS DECI
    FIELD sumvarekost2 AS DECI
    FIELD tb%1     AS DECI
    FIELD tb%2     AS DECI
    INDEX vgb IS PRIMARY UNIQUE vg butik.

DEFINE TEMP-TABLE ttRapHdr NO-UNDO
    FIELD levnr AS INTE
    FIELD vg AS INTE
    FIELD artikkelnr AS DECI
    FIELD inpris AS DECI
    FIELD utpris AS DECI
    FIELD levartnr AS CHAR
    FIELD farg AS CHAR
    FIELD bild AS CHAR
    INDEX vgla IS PRIMARY UNIQUE vg levnr artikkelnr.

DEFINE TEMP-TABLE ttRapRow NO-UNDO
    FIELD artikkelnr AS DECI
    FIELD butik AS INTE
    FIELD antal AS INTE
    INDEX artbut IS PRIMARY UNIQUE artikkelnr butik.

DEFINE VARIABLE iHrdRow AS INTEGER EXTENT 6    NO-UNDO.
DEFINE VARIABLE iHrdCol AS INTEGER EXTENT 5    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Dato1 FI-fsg1from BUTTON-SokDato1 ~
FI-fsg1to FI-fsg2from FI-fsg2to B-Rapport B-SesongBest 
&Scoped-Define DISPLAYED-OBJECTS FI-SesongBest FI-Dato1 FI-fsg1from ~
FI-fsg1to FI-fsg2from FI-fsg2to 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd C-Win 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-SesongBest  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SesongFsg1  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SesongFsg2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Dato1 AS DATE FORMAT "99/99/99":U 
     LABEL "Från datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-fsg1from AS DATE FORMAT "99/99/99":U 
     LABEL "Period 1 sålt från/till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-fsg1to AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-fsg2from AS DATE FORMAT "99/99/99":U 
     LABEL "Period 2 sålt från/till" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-fsg2to AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SesongBest AS CHARACTER FORMAT "X(10)":U 
     LABEL "Säsong beställningar" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SesongFsg1 AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sålt period 1" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-SesongFsg2 AS CHARACTER FORMAT "X(10)":U 
     LABEL "Sålt period 2" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-SesongBest AT ROW 3.86 COL 29 COLON-ALIGNED
     B-SesongFsg1 AT ROW 11 COL 45 NO-TAB-STOP 
     B-SesongFsg2 AT ROW 11 COL 84 NO-TAB-STOP 
     FI-Dato1 AT ROW 3.86 COL 61.6 COLON-ALIGNED
     FI-fsg1from AT ROW 5.29 COL 29 COLON-ALIGNED
     BUTTON-SokDato1 AT ROW 3.86 COL 78.6 NO-TAB-STOP 
     FI-fsg1to AT ROW 5.29 COL 45 COLON-ALIGNED NO-LABEL
     FI-fsg2from AT ROW 6.52 COL 29 COLON-ALIGNED
     FI-fsg2to AT ROW 6.52 COL 45 COLON-ALIGNED NO-LABEL
     B-Rapport AT ROW 8.38 COL 31
     FI-SesongFsg1 AT ROW 11 COL 28 COLON-ALIGNED
     FI-SesongFsg2 AT ROW 11 COL 67 COLON-ALIGNED
     B-SesongBest AT ROW 3.86 COL 46 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120 BY 12.1.


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
         TITLE              = "Beställningsanalys"
         HEIGHT             = 12.1
         WIDTH              = 120
         MAX-HEIGHT         = 25.81
         MAX-WIDTH          = 134.4
         VIRTUAL-HEIGHT     = 25.81
         VIRTUAL-WIDTH      = 134.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/DevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON B-SesongFsg1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-SesongFsg1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON B-SesongFsg2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-SesongFsg2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-SesongBest IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-SesongFsg1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-SesongFsg1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-SesongFsg2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FI-SesongFsg2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Beställningsanalys */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Beställningsanalys */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport C-Win
ON CHOOSE OF B-Rapport IN FRAME DEFAULT-FRAME /* Rapport */
DO:
  DEFINE VARIABLE dtfr1 AS DATE        NO-UNDO.
  DEFINE VARIABLE dtto1 AS DATE        NO-UNDO.
  DEFINE VARIABLE dtfr2 AS DATE        NO-UNDO.
  DEFINE VARIABLE dtto2 AS DATE        NO-UNDO.
  DEFINE VARIABLE iAarPerLinNr AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAarPerLinNrSlut AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAarPerLinNrTmp AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAarPerLinNrSlutTmp AS INTEGER     NO-UNDO.

/*
DEFINE TEMP-TABLE ttVgHdr NO-UNDO
    FIELD vg    AS INTE
    FIELD soldAar1Lbl AS CHAR
    FIELD soldAar2Lbl AS CHAR
    FIELD lager    AS INTE
    FIELD budget   AS INTE
    FIELD lev      AS INTE
    INDEX vg IS PRIMARY UNIQUE vg.

DEFINE TEMP-TABLE ttVgRow NO-UNDO
    FIELD vg    AS INTE
    FIELD butik AS INTE
    FIELD soldAar1 AS INTE
    FIELD soldAar2 AS INTE
    FIELD sumfsg1 AS DECI
    FIELD sumfsg2 AS DECI
    FIELD sumvarekost1 AS DECI
    FIELD sumvarekost2 AS DECI
    FIELD tb%1     AS DECI
    FIELD tb%2     AS DECI
    INDEX vgb IS PRIMARY UNIQUE vg butik.

DEFINE TEMP-TABLE ttRapHdr NO-UNDO
    FIELD levnr AS INTE
    FIELD vg AS INTE
    FIELD artikkelnr AS DECI
    FIELD inpris AS INTE
    FIELD utpris AS DECI
    FIELD levartnr AS CHAR
    FIELD farg AS CHAR
    INDEX vgla IS PRIMARY UNIQUE vg levnr artikkelnr.

DEFINE TEMP-TABLE ttRapRow NO-UNDO
    FIELD artikkelnr AS DECI
    FIELD butik AS INTE
    FIELD antal AS INTE
    INDEX artbut IS PRIMARY UNIQUE artikkelnr butik.

*/

run RapportPDF.
/*                                                                                                            */
/*     FOR EACH artbas NO-LOCK WHERE artbas.sasong = INT(FI-SesongBest:SCREEN-VALUE):                         */
/*         IF CAN-FIND(FIRST besthode OF artbas) THEN DO:                                                     */
/*             FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.                                                 */
/*             IF NOT AVAIL artpris THEN                                                                      */
/*                 NEXT.                                                                                      */
/*             FIND farg OF artbas NO-LOCK NO-ERROR.                                                          */
/*             CREATE ttRapHdr.                                                                               */
/*             ASSIGN ttRapHdr.levnr      = artbas.levnr                                                      */
/*                    ttRapHdr.vg         = artbas.vg                                                         */
/*                    ttRapHdr.artikkelnr = artbas.artikkelnr                                                 */
/*                    ttRapHdr.inpris     = artpris.varekost[1]                                               */
/*                    ttRapHdr.utpris     = artpris.pris[1]                                                   */
/*                    ttRapHdr.levartnr   = artbas.levkod                                                     */
/*                    ttRapHdr.farg       = IF AVAIL farg THEN farg.farbeskr ELSE "".                         */
/*         END.                                                                                               */
/*         FOR EACH besthode OF artbas NO-LOCK:                                                               */
/*             FOR EACH beststr OF besthode WHERE beststr.beststat = besthode.beststat NO-LOCK.               */
/*                 FIND ttRapRow WHERE ttRapRow.artikkelnr = artbas.artikkelnr AND                            */
/*                                     ttRapRow.butik      = beststr.butik NO-ERROR.                          */
/*                 IF NOT AVAIL ttRapRow THEN DO:                                                             */
/*                     CREATE ttRapRow.                                                                       */
/*                     ASSIGN ttRapRow.artikkelnr = artbas.artikkelnr                                         */
/*                            ttRapRow.butik      = beststr.butik.                                            */
/*                 END.                                                                                       */
/*                 ttRapRow.antal = ttRapRow.antal + BestStr.Bestilt.                                         */
/*             END.                                                                                           */
/*         END.                                                                                               */
/*         FIND ttVgHdr WHERE ttVgHdr.vg = artbas.vg NO-ERROR.                                                */
/*         IF NOT AVAIL ttVgHdr THEN DO:                                                                      */
/*             CREATE ttVgHdr.                                                                                */
/*             ASSIGN ttVgHdr.vg = artbas.vg.                                                                 */
/*         END.                                                                                               */
/*     END.                                                                                                   */
/*     FIND sasong WHERE sasong.sasong = INT(FI-SesongFsg1:SCREEN-VALUE) NO-LOCK.                             */
/*     ASSIGN dtfr1 = SaSong.StartDato                                                                        */
/*            dtto1 = SaSong.SluttDato.                                                                       */
/*     FIND sasong WHERE sasong.sasong = INT(FI-SesongFsg2:SCREEN-VALUE) NO-LOCK.                             */
/*     ASSIGN dtfr2 = SaSong.StartDato                                                                        */
/*            dtto2 = SaSong.SluttDato.                                                                       */
/*                                                                                                            */
/*     ASSIGN iAarPerLinNr = YEAR(dtfr1) * 1000 + dtfr1 - DATE(12,31,YEAR(dtfr1) - 1).                        */
/*     IF YEAR(dtfr1) <> YEAR(dtto1) THEN DO:                                                                 */
/*         iAarPerLinNrSlut = YEAR(dtfr1) * 1000 + DATE(12,31,YEAR(dtfr1)) - DATE(12,31,YEAR(dtfr1) - 1).     */
/*         iAarPerLinNrTmp  = YEAR(dtto1) * 1000 + 1.                                                         */
/*         iAarPerLinNrSlutTmp = YEAR(dtto1) * 1000 + dtto1 - DATE(12,31,YEAR(dtto1) - 1).                    */
/*     END.                                                                                                   */
/*     ELSE DO:                                                                                               */
/*         iAarPerLinNrSlut = YEAR(dtto1) * 1000 + DATE(12,31,YEAR(dtto1)) - DATE(12,31,YEAR(dtto1) - 1).     */
/*         iAarPerLinNrTmp     = ?.                                                                           */
/*         iAarPerLinNrSlutTmp = ?.                                                                           */
/*     END.                                                                                                   */
/*     RUN getStlinjeData (iAarPerLinNr,iAarPerLinNrSlut,1).                                                  */
/*     IF iAarPerLinNrTmp <> ? THEN                                                                           */
/*         RUN getStlinjeData (iAarPerLinNrTmp,iAarPerLinNrSlutTmp,1).                                        */
/*     IF FI-SesongFsg2:SCREEN-VALUE <> "" THEN DO:                                                           */
/*         ASSIGN iAarPerLinNr = YEAR(dtfr2) * 1000 + dtfr2 - DATE(12,31,YEAR(dtfr2) - 1).                    */
/*         IF YEAR(dtfr2) <> YEAR(dtto2) THEN DO:                                                             */
/*             iAarPerLinNrSlut = YEAR(dtfr2) * 1000 + DATE(12,31,YEAR(dtfr2)) - DATE(12,31,YEAR(dtfr2) - 1). */
/*             iAarPerLinNrTmp  = YEAR(dtto2) * 1000 + 1.                                                     */
/*             iAarPerLinNrSlutTmp = YEAR(dtto2) * 1000 + dtto2 - DATE(12,31,YEAR(dtto2) - 1).                */
/*         END.                                                                                               */
/*         ELSE DO:                                                                                           */
/*             iAarPerLinNrSlut = YEAR(dtto2) * 1000 + DATE(12,31,YEAR(dtto2)) - DATE(12,31,YEAR(dtto2) - 1). */
/*             iAarPerLinNrTmp     = ?.                                                                       */
/*             iAarPerLinNrSlutTmp = ?.                                                                       */
/*         END.                                                                                               */
/*         RUN getStlinjeData (iAarPerLinNr,iAarPerLinNrSlut,2).                                              */
/*         IF iAarPerLinNrTmp <> ? THEN                                                                       */
/*             RUN getStlinjeData (iAarPerLinNrTmp,iAarPerLinNrSlutTmp,2).                                    */
/*     END.                                                                                                   */
/*                                                                                                            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongBest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongBest C-Win
ON CHOOSE OF B-SesongBest IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTekst    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Sasong;SasBeskr;Sasong;StartDato"
                     ,"WHERE Sasong.StartDato <> ? AND Sasong.SluttDato <> ?"
                      ,""                                                  
                      ,"Sasong,StartDato",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).
        IF bOK THEN DO:
            ASSIGN FI-SesongBest:SCREEN-VALUE = entry(1,cTekst,"|")
                   FI-SesongBest     = entry(1,cTekst,"|")
                   FI-SesongBest:TOOLTIP = entry(1,cTekst,"|").
            FI-Dato1:SCREEN-VALUE = entry(2,cTekst,"|").
            FIND sasong WHERE sasong.sasong = INT(entry(1,cTekst,"|")) NO-LOCK.
            ASSIGN FI-fsg1from  = DATE(MONTH(SaSong.StartDato),DAY(SaSong.StartDato),YEAR(SaSong.StartDato) - 2)
                   FI-fsg1to    = DATE(MONTH(SaSong.SluttDato),DAY(SaSong.SluttDato),YEAR(SaSong.SluttDato) - 2)
                   FI-fsg2from  = DATE(MONTH(SaSong.StartDato),DAY(SaSong.StartDato),YEAR(SaSong.StartDato) - 1)
                   FI-fsg2to    = DATE(MONTH(SaSong.SluttDato),DAY(SaSong.SluttDato),YEAR(SaSong.SluttDato) - 1).

            DISPLAY FI-fsg1from FI-fsg1to FI-fsg2from FI-fsg2to WITH FRAME {&FRAME-NAME}.

     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongFsg1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongFsg1 C-Win
ON CHOOSE OF B-SesongFsg1 IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTekst    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Sasong;SasBeskr;Sasong;StartDato"
                     ,"WHERE Sasong.StartDato <> ? AND Sasong.SluttDato <> ?"
                      ,""                                                  
                      ,"Sasong",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).
        IF bOK THEN DO:
            ASSIGN FI-SesongFsg1:SCREEN-VALUE = cTekst
                   FI-SesongFsg1     = cTekst
                   FI-SesongFsg1:TOOLTIP = cTekst.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SesongFsg2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SesongFsg2 C-Win
ON CHOOSE OF B-SesongFsg2 IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cRowIdList AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cTekst    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.

    RUN JBoxLookup.w (THIS-PROCEDURE,50,
                      "Sasong;SasBeskr;Sasong;StartDato"
                     ,"WHERE Sasong.StartDato <> ? AND Sasong.SluttDato <> ?"
                      ,""                                                  
                      ,"Sasong",   /* <- return values for these fields */
                        OUTPUT cTekst,
                        OUTPUT bOK).
        IF bOK THEN DO:
            ASSIGN FI-SesongFsg2:SCREEN-VALUE = cTekst
                   FI-SesongFsg2     = cTekst
                   FI-SesongFsg2:TOOLTIP = cTekst.
     END.
     /* incl/CustDevMode.i */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato1 C-Win
ON CHOOSE OF BUTTON-SokDato1 IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Dato1
DO:
    DEFINE VARIABLE wTittel AS CHARACTER   NO-UNDO.
  do with frame DEFAULT-FRAME:  
    assign 
      FI-Dato1 = date(FI-Dato1:screen-value).

    wTittel = "Beställningsdatum".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-Dato1
      &Program     = kalender.w
      &Frame       = DEFAULT-FRAME
      &ExtraParam  = "input wTittel"
    }   
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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
{syspara.i 10 1 2 cBildkatalog}
cBildkatalog = RIGHT-TRIM(cBildkatalog,"\") + "\".
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN Butikslista.
  RUN enable_UI.
  
/*   RUN RapportPDF. */


  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Butikslista C-Win 
PROCEDURE Butikslista :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iWebshop AS INTEGER     NO-UNDO.
    
    {syspara.i 150 1 1 cTmp} /* har vi webshop */
    
    IF cTmp = "1" THEN DO:
        {syspara.i 150 1 2 iWebshop INT}
    END.
    
    FOR EACH Butiker NO-LOCK:
        IF butiker.butik = iWebshop THEN
            NEXT.
        IF NOT CAN-FIND(FIRST kasse WHERE kasse.butik = butiker.butik) THEN
            NEXT.
        cButiksNr = cButiksNr + (IF cButiksNr <> "" THEN "," ELSE "") + STRING(butiker.butik).
        cButiksnamn = cButiksnamn + (IF cButiksnamn <> "" THEN "," ELSE "") + STRING(butiker.kortnavn).
    END.


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
  DISPLAY FI-SesongBest FI-Dato1 FI-fsg1from FI-fsg1to FI-fsg2from FI-fsg2to 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Dato1 FI-fsg1from BUTTON-SokDato1 FI-fsg1to FI-fsg2from FI-fsg2to 
         B-Rapport B-SesongBest 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData C-Win 
PROCEDURE GetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE dtfr1 AS DATE        NO-UNDO.
  DEFINE VARIABLE dtto1 AS DATE        NO-UNDO.
  DEFINE VARIABLE dtfr2 AS DATE        NO-UNDO.
  DEFINE VARIABLE dtto2 AS DATE        NO-UNDO.
  DEFINE VARIABLE iAarPerLinNr AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAarPerLinNrSlut AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAarPerLinNrTmp AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iAarPerLinNrSlutTmp AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cBild AS CHARACTER   NO-UNDO.
  EMPTY TEMP-TABLE ttVgHdr.
  EMPTY TEMP-TABLE ttVgRow.
  EMPTY TEMP-TABLE ttRapHdr.
  EMPTY TEMP-TABLE ttRapRow.

/*
DEFINE TEMP-TABLE ttVgHdr NO-UNDO
    FIELD vg    AS INTE
    FIELD soldAar1Lbl AS CHAR
    FIELD soldAar2Lbl AS CHAR
    FIELD lager    AS INTE
    FIELD budget   AS INTE
    FIELD lev      AS INTE
    INDEX vg IS PRIMARY UNIQUE vg.

DEFINE TEMP-TABLE ttVgRow NO-UNDO
    FIELD vg    AS INTE
    FIELD butik AS INTE
    FIELD soldAar1 AS INTE
    FIELD soldAar2 AS INTE
    FIELD sumfsg1 AS DECI
    FIELD sumfsg2 AS DECI
    FIELD sumvarekost1 AS DECI
    FIELD sumvarekost2 AS DECI
    FIELD tb%1     AS DECI
    FIELD tb%2     AS DECI
    INDEX vgb IS PRIMARY UNIQUE vg butik.

DEFINE TEMP-TABLE ttRapHdr NO-UNDO
    FIELD levnr AS INTE
    FIELD vg AS INTE
    FIELD artikkelnr AS DECI
    FIELD inpris AS INTE
    FIELD utpris AS DECI
    FIELD levartnr AS CHAR
    FIELD farg AS CHAR
    INDEX vgla IS PRIMARY UNIQUE vg levnr artikkelnr.

DEFINE TEMP-TABLE ttRapRow NO-UNDO
    FIELD artikkelnr AS DECI
    FIELD butik AS INTE
    FIELD antal AS INTE
    INDEX artbut IS PRIMARY UNIQUE artikkelnr butik.

                        */
  DO WITH FRAME {&FRAME-NAME}:
      FOR EACH artbas NO-LOCK WHERE artbas.sasong = INT(FI-SesongBest:SCREEN-VALUE):
          IF CAN-FIND(FIRST besthode OF artbas WHERE BestHode.BestillingsDato >= INPUT FI-Dato1) THEN DO:
              FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
              IF NOT AVAIL artpris THEN
                  NEXT.
              FIND farg OF artbas NO-LOCK NO-ERROR.
              cBild = "".
              FIND Bilderegister WHERE Bilderegister.BildNr = INTEGER(ArtBas.Artikkelnr) NO-LOCK NO-ERROR.
              IF AVAIL Bilderegister THEN DO:
                  /* Slett Bilderegister + filer på disk */
                  IF SEARCH(cBildKatalog + "mini" + Bilderegister.FilNavn) <> ? THEN
                       cBild = cBildKatalog + "mini" + Bilderegister.FilNavn.
              END.
              CREATE ttRapHdr.
              ASSIGN ttRapHdr.levnr      = artbas.levnr
                     ttRapHdr.vg         = artbas.vg
                     ttRapHdr.artikkelnr = artbas.artikkelnr
                     ttRapHdr.inpris     = artpris.varekost[1]
                     ttRapHdr.utpris     = artpris.pris[1]
                     ttRapHdr.levartnr   = artbas.levkod
                     ttRapHdr.farg       = IF AVAIL farg THEN farg.farbeskr ELSE ""
                     ttRapHdr.bild       = cBild.
          END.
          FOR EACH besthode OF artbas NO-LOCK:
              FOR EACH beststr OF besthode WHERE beststr.beststat = besthode.beststat NO-LOCK.
                  FIND ttRapRow WHERE ttRapRow.artikkelnr = artbas.artikkelnr AND
                                      ttRapRow.butik      = beststr.butik NO-ERROR.
                  IF NOT AVAIL ttRapRow THEN DO:
                      CREATE ttRapRow.
                      ASSIGN ttRapRow.artikkelnr = artbas.artikkelnr
                             ttRapRow.butik      = beststr.butik.
                  END.
                  ttRapRow.antal = ttRapRow.antal + BestStr.Bestilt.
              END.
          END.
          FIND ttVgHdr WHERE ttVgHdr.vg = artbas.vg NO-ERROR.
          IF NOT AVAIL ttVgHdr THEN DO:
              CREATE ttVgHdr.
              ASSIGN ttVgHdr.vg = artbas.vg.
          END.
      END.
      ASSIGN INPUT FI-fsg1from 
             INPUT FI-fsg1to 
             INPUT FI-fsg2from 
             INPUT FI-fsg2to.
/*       FIND sasong WHERE sasong.sasong = INT(FI-SesongFsg1:SCREEN-VALUE) NO-LOCK. */
      ASSIGN dtfr1 = FI-fsg1from  /* SaSong.StartDato  */
             dtto1 = FI-fsg1to.   /* SaSong.SluttDato. */
/*       FIND sasong WHERE sasong.sasong = INT(FI-SesongFsg2:SCREEN-VALUE) NO-LOCK. */
      ASSIGN dtfr2 = FI-fsg2from /* SaSong.StartDato */
             dtto2 = FI-fsg2to.  /* SaSong.SluttDato.  */

      ASSIGN iAarPerLinNr = YEAR(dtfr1) * 1000 + dtfr1 - DATE(12,31,YEAR(dtfr1) - 1).  
      IF YEAR(dtfr1) <> YEAR(dtto1) THEN DO:
          iAarPerLinNrSlut = YEAR(dtfr1) * 1000 + DATE(12,31,YEAR(dtfr1)) - DATE(12,31,YEAR(dtfr1) - 1).
          iAarPerLinNrTmp  = YEAR(dtto1) * 1000 + 1.   
          iAarPerLinNrSlutTmp = YEAR(dtto1) * 1000 + dtto1 - DATE(12,31,YEAR(dtto1) - 1).  
      END.
      ELSE DO:
          iAarPerLinNrSlut = YEAR(dtto1) * 1000 + DATE(12,31,YEAR(dtto1)) - DATE(12,31,YEAR(dtto1) - 1).
          iAarPerLinNrTmp     = ?.
          iAarPerLinNrSlutTmp = ?.
      END.
      RUN getStlinjeData (iAarPerLinNr,iAarPerLinNrSlut,1).
      IF iAarPerLinNrTmp <> ? THEN
          RUN getStlinjeData (iAarPerLinNrTmp,iAarPerLinNrSlutTmp,1).

      ASSIGN iAarPerLinNr = YEAR(dtfr2) * 1000 + dtfr2 - DATE(12,31,YEAR(dtfr2) - 1).  
      IF YEAR(dtfr2) <> YEAR(dtto2) THEN DO:
          iAarPerLinNrSlut = YEAR(dtfr2) * 1000 + DATE(12,31,YEAR(dtfr2)) - DATE(12,31,YEAR(dtfr2) - 1).
          iAarPerLinNrTmp  = YEAR(dtto2) * 1000 + 1.   
          iAarPerLinNrSlutTmp = YEAR(dtto2) * 1000 + dtto2 - DATE(12,31,YEAR(dtto2) - 1).  
      END.
      ELSE DO:
          iAarPerLinNrSlut = YEAR(dtto2) * 1000 + DATE(12,31,YEAR(dtto2)) - DATE(12,31,YEAR(dtto2) - 1).
          iAarPerLinNrTmp     = ?.
          iAarPerLinNrSlutTmp = ?.
      END.
      RUN getStlinjeData (iAarPerLinNr,iAarPerLinNrSlut,2).
      IF iAarPerLinNrTmp <> ? THEN
          RUN getStlinjeData (iAarPerLinNrTmp,iAarPerLinNrSlutTmp,2).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getStlinjeData C-Win 
PROCEDURE getStlinjeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iStartDag AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iSlutDag  AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iTyp      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAarPerLinNr      AS INTEGER     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
FOR EACH ttVgHdr:
    DO ii = 1 TO NUM-ENTRIES(cButiksnr):
        DO iAarPerLinNr = iStartDag TO iSlutDag:
            FOR EACH StLinje WHERE StLinje.Butik = INT(ENTRY(ii,cButiksnr)) AND StTypeId = 'VAREGR' AND
                PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND StLinje.DataObjekt = STRING(ttVgHdr.vg,"999999") USE-INDEX AarPerLinNr NO-LOCK:
                FIND ttVgRow WHERE ttVgRow.vg    = ttVgHdr.vg AND
                                   ttVgRow.butik = stlinje.butik NO-ERROR.
                IF NOT AVAIL ttVgRow THEN DO:
                    CREATE ttVgRow.
                    ASSIGN ttVgRow.vg    = ttVgHdr.vg
                           ttVgRow.butik = stlinje.butik.
                END.
                IF iTyp = 1 THEN
                    ASSIGN ttVgRow.soldAar1     = ttVgRow.soldAar1     + StLinje.AntSolgt
                           ttVgRow.sumfsg1      = ttVgRow.sumfsg1      + StLinje.VerdiSolg
                           ttVgRow.sumvarekost1 = ttVgRow.sumvarekost1 + StLinje.VVarekost.
                ELSE
                    ASSIGN ttVgRow.soldAar2     = ttVgRow.soldAar2     + StLinje.AntSolgt 
                           ttVgRow.sumfsg2      = ttVgRow.sumfsg2      + StLinje.VerdiSolg
                           ttVgRow.sumvarekost2 = ttVgRow.sumvarekost2 + StLinje.VVarekost.
            END.
        END.
    END.
END.
/*
DEFINE TEMP-TABLE ttVgHdr NO-UNDO
    FIELD vg    AS INTE
    FIELD soldAar1Lbl AS CHAR
    FIELD soldAar2Lbl AS CHAR
    FIELD lager    AS INTE
    FIELD budget   AS INTE
    FIELD lev      AS INTE
    INDEX vg IS PRIMARY UNIQUE vg.

DEFINE TEMP-TABLE ttVgRow NO-UNDO
    FIELD vg           AS INTE
    FIELD butik        AS INTE
    FIELD soldAar1     AS INTE
    FIELD soldAar2     AS INTE
    FIELD sumfsg1      AS DECI
    FIELD sumfsg2      AS DECI
    FIELD sumvarekost1 AS DECI
    FIELD sumvarekost2 AS DECI
    FIELD tb%1         AS DECI
    FIELD tb%2         AS DECI
    INDEX vgb IS PRIMARY UNIQUE vg butik.

DEFINE TEMP-TABLE ttRapHdr NO-UNDO
    FIELD levnr AS INTE
    FIELD vg AS INTE
    FIELD artikkelnr AS DECI
    FIELD inpris AS INTE
    FIELD utpris AS DECI
    FIELD levartnr AS CHAR
    FIELD farg AS CHAR
    INDEX vgla IS PRIMARY UNIQUE vg levnr artikkelnr.

DEFINE TEMP-TABLE ttRapRow NO-UNDO
    FIELD artikkelnr AS DECI
    FIELD butik AS INTE
    FIELD antal AS INTE
    INDEX artbut IS PRIMARY UNIQUE artikkelnr butik.

                        */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HeaderData C-Win 
PROCEDURE HeaderData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dRepDY AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cRightCol AS CHAR     NO-UNDO.
DEFINE VARIABLE iMinusCol AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTmp AS INTEGER     NO-UNDO.
DEFINE VARIABLE II AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNextRow AS INTEGER     NO-UNDO.
cRightCol = "230,340,450,560".
iMinusCol = 50.

dRepDY = 803.

RUN pdf_set_font ("Spdf", "Helvetica-Bold",14).
iTmp = 120.
RUN pdf_text_xy_dec ("Spdf",STRING(ttVgHdr.vg),iTmp - bredd(STRING(ttVgHdr.vg)),dRepDY).

dRepDY = 780.
iNextRow = 23.


RUN pdf_set_font ("Spdf", "Helvetica",12).

DO ii = 1 TO NUM-ENTRIES(cButiksnr):
    IF ii = 5 THEN
        LEAVE.
    IF ENTRY(ii,cButiksNr) = "" THEN
        LEAVE.
    FIND ttVgRow WHERE ttVgRow.vg = ttVgHdr.vg AND ttVgRow.butik = INT(ENTRY(ii,cButiksNr)) NO-LOCK NO-ERROR.
    IF NOT AVAIL ttVgRow THEN
        NEXT.
    iTmp = INT(ENTRY(ii,cRightCol)).

    RUN pdf_text_xy_dec ("Spdf",STRING(ttVgRow.soldAar1),iTmp - iMinusCol - bredd(STRING(ttVgRow.soldAar1)),dRepDY).
    RUN pdf_text_xy_dec ("Spdf",STRING(ttVgRow.tb%1) + "%",iTmp - bredd(STRING(ttVgRow.tb%1) + "%"),dRepDY).
    
/*     RUN pdf_text_xy_dec ("Spdf",STRING(ttVgRow.sumfsg1),iTmp - iMinusCol - bredd(STRING(ttVgRow.sumfsg1)),dRepDY - 46).           */
/*     RUN pdf_text_xy_dec ("Spdf",STRING(ttVgRow.sumvarekost1),iTmp - iMinusCol - bredd(STRING(ttVgRow.sumvarekost1)),dRepDY - 69). */
    
    RUN pdf_text_xy_dec ("Spdf",STRING(ttVgRow.soldAar2),iTmp - iMinusCol - bredd(STRING(ttVgRow.soldAar2)),dRepDY - iNextRow).
    RUN pdf_text_xy_dec ("Spdf",STRING(ttVgRow.tb%2) + "%",iTmp - bredd(STRING(ttVgRow.tb%2) + "%"),dRepDY - iNextRow).


END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LinjeData C-Win 
PROCEDURE LinjeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE iToggle AS INTEGER     NO-UNDO.    /* hanterar vänster - höger */
DEFINE VARIABLE iBoxRow AS INTEGER     NO-UNDO.    /* Håller reda på vilken vglevartboxrad vi är i */
DEFINE VARIABLE dY AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iX AS INTEGER     NO-UNDO.
DEFINE VARIABLE dRowDiff AS DECIMAL     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE cAntPos AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cImageY AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dDim1 AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dDim2 AS DECIMAL     NO-UNDO.
/* DEFINE TEMP-TABLE ttRapHdr NO-UNDO                    */
/*     FIELD levnr AS INTE                               */
/*     FIELD vg AS INTE                                  */
/*     FIELD artikkelnr AS DECI                          */
/*     FIELD inpris AS INTE                              */
/*     FIELD utpris AS DECI                              */
/*     FIELD levartnr AS CHAR                            */
/*     FIELD farg AS CHAR                                */
/*     INDEX vgla IS PRIMARY UNIQUE vg levnr artikkelnr. */
/* 
    FIELD artikkelnr AS DECI
    FIELD butik AS INTE
    FIELD antal AS INTE
    INDEX artbut IS PRIMARY UNIQUE artikkelnr butik.
*/
/* 
RUN PageFrames(2).
 */

    dY = 667.
    iX = 122.
    iBoxRow = 3.
    dRowDiff = 14.8.
    cAntPos = "150,170,190,210".
        cImageY = "87,161,233,309,383,457,531,605,679,753,827".
    iToggle = 1.  /* när vi skrivit Row 12 och toggle är = 2 och vi änte är i sista break så bygger vi en ny sida PageFrames(2). */
    FOR EACH ttRapHdr WHERE ttRapHdr.vg = ttVgHdr.vg BREAK BY ttRapHdr.vg BY levnr BY levartnr:
        RUN pdf_text_xy_dec ("Spdf","Lev:",15 + ((iToggle - 1) * 285),dY).
        RUN pdf_text_xy_dec ("Spdf",STRING(ttRapHdr.levnr),120 + ((iToggle - 1) * 285) - bredd(STRING(ttRapHdr.levnr)),dY).

        RUN pdf_text_xy_dec ("Spdf","Ink:",15 + ((iToggle - 1) * 285),dY - dRowDiff).
        RUN pdf_text_xy_dec ("Spdf",STRING(ROUND(ttRapHdr.inpris,0)),120 + ((iToggle - 1) * 285) - bredd(STRING(ROUND(ttRapHdr.inpris,0))),dY - dRowDiff).

        RUN pdf_text_xy_dec ("Spdf","Ut:",15 + ((iToggle - 1) * 285),dY - 2 * dRowDiff).
        RUN pdf_text_xy_dec ("Spdf",STRING(ROUND(ttRapHdr.utpris,0)),120 + ((iToggle - 1) * 285) - bredd(STRING(ROUND(ttRapHdr.utpris,0))),dY - 2 * dRowDiff).
            
/*         RUN pdf_text_xy_dec ("Spdf","ART:",15 + ((iToggle - 1) * 285),dY - 3 * dRowDiff). */
/*         RUN pdf_text_xy_dec ("Spdf",ttRapHdr.levartnr,120 + ((iToggle - 1) * 285) - bredd(ttRapHdr.levartnr),dY - 3 * dRowDiff). */
        RUN pdf_text_xy_dec ("Spdf",ttRapHdr.levartnr,15 + ((iToggle - 1) * 285),dY - 3 * dRowDiff).
        
        RUN pdf_text_xy_dec ("Spdf","Färg:",15 + ((iToggle - 1) * 285),dY - 4 * dRowDiff).
        RUN pdf_text_xy_dec ("Spdf",ttRapHdr.farg,50 + ((iToggle - 1) * 285),dY - 4 * dRowDiff).
/*         RUN pdf_text_xy_dec ("Spdf",ttRapHdr.farg,120 + ((iToggle - 1) * 285) - bredd(ttRapHdr.farg),dY - 4 * dRowDiff). */
        IF ttRapHdr.bild <> "" THEN DO:
            RUN pdf_load_image IN h_PDFinc ("Spdf",STRING(ttRapHdr.artikkelnr),ttRapHdr.bild).
            dDim1 = 78 / pdf_ImageDim ("Spdf",STRING(ttRapHdr.artikkelnr),"WIDTH").
            dDim2 = 71 / pdf_ImageDim ("Spdf",STRING(ttRapHdr.artikkelnr),"HEIGHT").
            RUN pdf_place_image2 IN h_PDFinc ("Spdf",
                                           STRING(ttRapHdr.artikkelnr),
                                           215 + ((iToggle - 1) * 285),
                                           INT(ENTRY(iBoxRow,cImageY)),
                                           pdf_ImageDim ("Spdf",STRING(ttRapHdr.artikkelnr),"WIDTH") * MIN(dDim1,dDim2),
                                           pdf_ImageDim ("Spdf",STRING(ttRapHdr.artikkelnr),"HEIGHT") * MIN(dDim1,dDim2)).
/*             RUN pdf_text_xy_dec ("Spdf",STRING(pdf_ImageDim ("Spdf",STRING(ttRapHdr.artikkelnr),"WIDTH")),150 + ((iToggle - 1) * 285),dY - 4 * dRowDiff).  */
/*             RUN pdf_text_xy_dec ("Spdf",STRING(pdf_ImageDim ("Spdf",STRING(ttRapHdr.artikkelnr),"HEIGHT")),190 + ((iToggle - 1) * 285),dY - 4 * dRowDiff). */
        END.

        DO ii = 1 TO NUM-ENTRIES(cButiksnr):
            IF ii > 4 OR INT(ENTRY(ii,cButiksnr)) = 0 THEN
                LEAVE.
            FIND ttRapRow WHERE ttRapRow.artikkelnr = ttRapHdr.artikkelnr AND
                                ttRapRow.butik      = INT(ENTRY(ii,cButiksnr)) NO-ERROR.
            IF AVAIL ttRapRow THEN
                RUN pdf_text_xy_dec ("Spdf",STRING(ttRapRow.antal),INT(ENTRY(ii,cAntPos)) + ((iToggle - 1) * 285) - bredd(STRING(ttRapRow.antal)),dY - 2 * dRowDiff).

        END.
        IF iToggle = 2 THEN
            iBoxRow = iBoxRow + 1.
        iToggle = IF iToggle = 2 THEN 1 ELSE 2.
        IF iToggle = 1 THEN
            dY = dY - 5 * dRowDiff.
        IF iBoxRow = 12 AND NOT LAST-OF(ttRapHdr.vg) THEN DO:
            RUN pdf_new_page ("Spdf").
            RUN PageFrames(2).
            iBoxRow = 1.
            dY = 815.
        END.

    END.
/*   RUN pdf_load_image IN h_PDFinc ("Spdf","GANTLOGO",cImageFile).                         */
/*       RUN pdf_place_image2 IN h_PDFinc ("Spdf",                                          */
/*                                        "GANTLOGO",                                       */
/*                                        iLeftMargin,                                      */
/*                                        30,                                               */
/*                                        pdf_ImageDim ("Spdf","GANTLOGO","WIDTH") * .25,   */
/*                                        pdf_ImageDim ("Spdf","GANTLOGO","HEIGHT") * .25). */
/*                                                                                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter C-Win 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFrames C-Win 
PROCEDURE PageFrames :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iTyp AS INTEGER     NO-UNDO.
DEFINE VARIABLE iX AS INTEGER     NO-UNDO.
DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
DEFINE VARIABLE dY AS DECIMAL     NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
IF iTyp = 1 THEN DO:
    RUN pdf_rect2 ("Spdf", 10,14,570,666,0.5).
    iX = 130.
    iY = 680.
    RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 666, 0.5).
    
    iX = 151.
    DO ii = 1 TO 4:
        RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 666, 0.5).
        iX = iX + 21.
    END.
    iX = 295.
    RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 666, 0.5).
    
    iX = 415.
    DO ii = 1 TO 5:
        RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 666, 0.5).
        iX = iX + 21.
    END.
    DO ii = 1 TO 8:
        iY = iY - 74.
        RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).
    END.

    dY = 680.
    iX = 10.
    DO i2 = 1 TO 9:
        IF i2 > 1 THEN DO:
            dY = 680.
            dY = dY - ((i2 - 1) * 74).
        END.
        DO ii = 1 TO 4:
            dY = dY - 14.8.
    /*         RUN pdf_line IN h_PDFinc  ("Spdf", iX, dY, 130, iY, 0.5). */
            RUN pdf_line_dec IN h_PDFinc ("Spdf",DEC(iX),dY,DEC(130),dY,0.5).
            RUN pdf_line_dec IN h_PDFinc ("Spdf",DEC(iX + 285),dY,DEC(130 + 285),dY,0.5).
    /*         RUN pdf_line IN h_PDFinc  ("Spdf", iX + 165, iY, 120, iY, 0.5). */
        END.
    END.

END.
ELSE DO:
    RUN pdf_rect2 ("Spdf", 10,14,570,814,0.5).
    iX = 130.
    iY = 828.
    DO ii = 1 TO 5:
        RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 814, 0.5).
        iX = iX + 21.
    END.
    iX = 295.
    RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 814, 0.5).
    iX = 415.
    DO ii = 1 TO 5:
        RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 814, 0.5).
        iX = iX + 21.
    END.

    DO ii = 1 TO 10:
        iY = iY - 74.
        RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).
    END.
    dY = 828.
    iX = 10.
    DO i2 = 1 TO 11:
        IF i2 > 1 THEN DO:
            dY = 828.
            dY = dY - ((i2 - 1) * 74).
        END.
        DO ii = 1 TO 4:
            dY = dY - 14.8.
    /*         RUN pdf_line IN h_PDFinc  ("Spdf", iX, dY, 130, iY, 0.5). */
            RUN pdf_line_dec IN h_PDFinc ("Spdf",DEC(iX),dY,DEC(130),dY,0.5).
            RUN pdf_line_dec IN h_PDFinc ("Spdf",DEC(iX + 285),dY,DEC(130 + 285),dY,0.5).
    /*         RUN pdf_line IN h_PDFinc  ("Spdf", iX + 165, iY, 120, iY, 0.5). */
        END.
    END.
END.

/* 
 


RUN pdf_rect2 ("Spdf", 10,680,570,150,0.5).
iY = 800.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).
iY = iY - 30.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).
iY = iY - 30.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).
iY = iY - 30.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).
iY = iY - 30.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).

RUN pdf_line IN h_PDFinc  ("Spdf", 10, iY, 580, iY, 0.5).

iX = 130.
iY = 830.
RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 150, 0.5).
iX = iX + 110.
RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 150, 0.5).
iX = iX + 110.
RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 150, 0.5).
iX = iX + 110.
RUN pdf_line IN h_PDFinc  ("Spdf", iX, iY, iX, iY - 150, 0.5). 
 
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeaderFrame C-Win 
PROCEDURE PageHeaderFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dX AS INTEGER     NO-UNDO.
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.
DEFINE VARIABLE cLabels AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  ASSIGN iHrdRow[1] = 800
         iHrdRow[2] = 777
         iHrdRow[3] = 754
         iHrdRow[4] = 730
         iHrdRow[5] = 706
         iHrdRow[6] = 682.
  ASSIGN iHrdCol[1] = 16
         iHrdCol[2] = 136
         iHrdCol[3] = 246
         iHrdCol[4] = 356
         iHrdCol[5] = 466.


RUN pdf_rect2 ("Spdf", 10,680,570,148,0.5).
dY = 798.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, dY, 580, dY, 0.5).
dY = dY - 23.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, dY, 580, dY, 0.5).
dY = dY - 23.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, dY, 580, dY, 0.5).
dY = dY - 24.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, dY, 580, dY, 0.5).
dY = dY - 24.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, dY, 580, dY, 0.5).
dY = dY - 24.
RUN pdf_line IN h_PDFinc  ("Spdf", 10, dY, 580, dY, 0.5).

dX = 130.
dY = 828.
RUN pdf_line IN h_PDFinc  ("Spdf", dX, dY, dX, dY - 148, 0.5).
dX = dX + 110.
RUN pdf_line IN h_PDFinc  ("Spdf", dX, dY, dX, dY - 148, 0.5).
dX = dX + 110.
RUN pdf_line IN h_PDFinc  ("Spdf", dX, dY, dX, dY - 148, 0.5).
dX = dX + 110.
RUN pdf_line IN h_PDFinc  ("Spdf", dX, dY, dX, dY - 148, 0.5).

RUN pdf_set_font ("Spdf", "Helvetica-Bold",14).

cLabels = "GRUPP:,SÅLT:,SÅLT:,LAGER:,BUDGET/ANM:,LEV:".

DO ii = 1 TO 6:
    RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,cLabels),iHrdCol[1],iHrdRow[ii]).
END.
DO ii = 1 TO MIN(NUM-ENTRIES(cButiksnamn),4):
    IF ENTRY(ii,cButiksnamn) = "" THEN
        NEXT.
    RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,cButiksnamn),iHrdCol[ii + 1],iHrdRow[1]).

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF C-Win 
PROCEDURE RapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iColLabelPage AS INTEGER     NO-UNDO.

/*   ASSIGN iHrdRow[1] = 775  */
/*          iHrdRow[2] = 752  */
/*          iHrdRow[3] = 729  */
/*          iHrdRow[4] = 706  */
/*          iHrdRow[5] = 683  */
/*          iHrdRow[6] = 670. */
/*   ASSIGN iHrdCol[1] = 14   */
/*          iHrdCol[2] = 124  */
/*          iHrdCol[3] = 234  */
/*          iHrdCol[4] = 344  */
/*          iHrdCol[5] = 454. */

  RUN GetData.
  FOR EACH ttVgRow:
    tb%1 = ROUND(((sumfsg1 - sumvarekost1) / sumfsg1) * 100,1) NO-ERROR.
    IF tb%1 = ? THEN tb%1 = 0.
    tb%2 = ROUND(((sumfsg2 - sumvarekost2) / sumfsg2) * 100,1) NO-ERROR.
    IF tb%2 = ? THEN tb%2 = 0.
END.

  IF NOT CAN-FIND(FIRST ttVgHdr) THEN DO:
      MESSAGE "Rapport tom"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  cFilNavn = SESSION:TEMP-DIR + "Skohornet" + "_" + STRING(TIME) + ".pdf".
  RUN pdf_new ("Spdf",cFilNavn).
  RUN pdf_set_BottomMargin ("Spdf", 20).
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation ("Spdf","portrait").

  iPageHeight = pdf_PageHeight ("Spdf").
  iPageWidth  = pdf_PageWidth ("Spdf").
  
  RUN pdf_set_VerticalSpace ("Spdf",13).

  FOR EACH ttVgHdr BREAK BY ttVgHdr.vg:
      RUN pdf_new_page ("Spdf").
      RUN PageHeaderFrame.
      RUN PageFrames(1). /* När vi har skrivit ut heade kan resten inte vara så stort */
      RUN HeaderData.
      RUN LinjeData.
  END.


  iColLabelPage = 1.
  RUN pdf_new_page ("Spdf").
  RUN PageFrames(2). /* När vi har skrivit ut heade kan resten inte vara så stort */
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
/*   RUN RasterTst(1). */
/*   RUN RasterTst(2). */
  RUN pdf_close ("Spdf").
 /*  RUN SendEmail IN THIS-PROCEDURE. */
  RUN browse2pdf\viewxmldialog.w (cFilNavn,"PDF Template").



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RasterTst C-Win 
PROCEDURE RasterTst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iMode AS INTEGER     NO-UNDO.
DEFINE VARIABLE dY AS INTEGER     NO-UNDO.
DEFINE VARIABLE dx AS INTEGER     NO-UNDO.

IF iMode = 1 THEN DO:
            RUN pdf_text_xy_dec ("Spdf","830",200,830).
            RUN pdf_text_xy_dec ("Spdf","820",200,820).
            RUN pdf_text_xy_dec ("Spdf","810",200,810).
            RUN pdf_text_xy_dec ("Spdf","800",200,800).
            RUN pdf_text_xy_dec ("Spdf","750",200,750).
/*             RUN pdf_text_xy_dec ("Spdf","700",200,700). */
            RUN pdf_text_xy_dec ("Spdf","650",200,650).
            RUN pdf_text_xy_dec ("Spdf","600",200,600).
            RUN pdf_text_xy_dec ("Spdf","550",200,550).
            RUN pdf_text_xy_dec ("Spdf","500",200,500).
            RUN pdf_text_xy_dec ("Spdf","450",200,450).
            RUN pdf_text_xy_dec ("Spdf","400",200,400).
            RUN pdf_text_xy_dec ("Spdf","350",200,350).
            RUN pdf_text_xy_dec ("Spdf","300",200,300).
            RUN pdf_text_xy_dec ("Spdf","250",200,250).
            RUN pdf_text_xy_dec ("Spdf","200",200,200).
            RUN pdf_text_xy_dec ("Spdf","150",200,150).
            RUN pdf_text_xy_dec ("Spdf","100",200,100).
            RUN pdf_text_xy_dec ("Spdf","50",200,50).
            RUN pdf_text_xy_dec ("Spdf","40",200,40).
            RUN pdf_text_xy_dec ("Spdf","30",200,30).
            RUN pdf_text_xy_dec ("Spdf","20",200,20).
            RUN pdf_text_xy_dec ("Spdf","10",200,10).
END.
ELSE DO:
    RUN pdf_text_xy_dec ("Spdf",".",10,700).
    RUN pdf_text_xy_dec ("Spdf",".",20,700).
    RUN pdf_text_xy_dec ("Spdf",".",30,700).
    RUN pdf_text_xy_dec ("Spdf",".",40,700).
    RUN pdf_text_xy_dec ("Spdf",".",50,700).
    RUN pdf_text_xy_dec ("Spdf",".",60,700).
    RUN pdf_text_xy_dec ("Spdf",".",70,700).
    RUN pdf_text_xy_dec ("Spdf",".",80,700).
    RUN pdf_text_xy_dec ("Spdf",".",90,700).
    RUN pdf_text_xy_dec ("Spdf","x",100,700).
    RUN pdf_text_xy_dec ("Spdf",".",110,700).
    RUN pdf_text_xy_dec ("Spdf",".",120,700).
    RUN pdf_text_xy_dec ("Spdf",".",130,700).
    RUN pdf_text_xy_dec ("Spdf",".",140,700).
    RUN pdf_text_xy_dec ("Spdf",".",150,700).
    RUN pdf_text_xy_dec ("Spdf",".",160,700).
    RUN pdf_text_xy_dec ("Spdf",".",170,700).
    RUN pdf_text_xy_dec ("Spdf",".",180,700).
    RUN pdf_text_xy_dec ("Spdf",".",190,700).
    RUN pdf_text_xy_dec ("Spdf","x",200,700).
    RUN pdf_text_xy_dec ("Spdf",".",210,700).
    RUN pdf_text_xy_dec ("Spdf",".",220,700).
    RUN pdf_text_xy_dec ("Spdf",".",230,700).
    RUN pdf_text_xy_dec ("Spdf",".",240,700).
    RUN pdf_text_xy_dec ("Spdf",".",250,700).
    RUN pdf_text_xy_dec ("Spdf",".",260,700).
    RUN pdf_text_xy_dec ("Spdf",".",270,700).
    RUN pdf_text_xy_dec ("Spdf",".",280,700).
    RUN pdf_text_xy_dec ("Spdf",".",290,700).
    RUN pdf_text_xy_dec ("Spdf","x",300,700).
    RUN pdf_text_xy_dec ("Spdf",".",310,700).
    RUN pdf_text_xy_dec ("Spdf",".",320,700).
    RUN pdf_text_xy_dec ("Spdf",".",330,700).
    RUN pdf_text_xy_dec ("Spdf",".",340,700).
    RUN pdf_text_xy_dec ("Spdf",".",350,700).
    RUN pdf_text_xy_dec ("Spdf",".",360,700).
    RUN pdf_text_xy_dec ("Spdf",".",370,700).
    RUN pdf_text_xy_dec ("Spdf",".",380,700).
    RUN pdf_text_xy_dec ("Spdf",".",390,700).
    RUN pdf_text_xy_dec ("Spdf","x",400,700).
    RUN pdf_text_xy_dec ("Spdf",".",410,700).
    RUN pdf_text_xy_dec ("Spdf",".",420,700).
    RUN pdf_text_xy_dec ("Spdf",".",430,700).
    RUN pdf_text_xy_dec ("Spdf",".",440,700).
    RUN pdf_text_xy_dec ("Spdf",".",450,700).
    RUN pdf_text_xy_dec ("Spdf",".",460,700).
    RUN pdf_text_xy_dec ("Spdf",".",470,700).
    RUN pdf_text_xy_dec ("Spdf",".",480,700).
    RUN pdf_text_xy_dec ("Spdf",".",490,700).
    RUN pdf_text_xy_dec ("Spdf","x",500,700).
    RUN pdf_text_xy_dec ("Spdf",".",510,700).
    RUN pdf_text_xy_dec ("Spdf",".",520,700).
    RUN pdf_text_xy_dec ("Spdf",".",530,700).
    RUN pdf_text_xy_dec ("Spdf",".",540,700).
    RUN pdf_text_xy_dec ("Spdf",".",550,700).
    RUN pdf_text_xy_dec ("Spdf",".",560,700).
    RUN pdf_text_xy_dec ("Spdf",".",570,700).
    RUN pdf_text_xy_dec ("Spdf",".",580,700).
    RUN pdf_text_xy_dec ("Spdf",".",590,700).
    RUN pdf_text_xy_dec ("Spdf","x",600,700).
END.






END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd C-Win 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

