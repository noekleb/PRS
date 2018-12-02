&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR lNullager   AS LOGICAL    NO-UNDO.
  DEFINE VAR iSortering  AS INTEGER    NO-UNDO.
  DEFINE VAR cButikker   AS CHARACTER  NO-UNDO.
  DEFINE VAR cAvgrensButikker   AS CHARACTER  NO-UNDO.
  DEFINE VAR dFraDato AS DATE NO-UNDO.
  DEFINE VAR dTilDato AS DATE NO-UNDO.
  DEFINE VAR lPeriode AS LOG  NO-UNDO.
  DEFINE VAR lVisStr  AS LOGICAL NO-UNDO.
  DEFINE VAR lNullposter AS LOGICAL    NO-UNDO.
  DEFINE VAR lArtAnalys AS LOGICAL NO-UNDO.
&ELSE
  DEFINE OUTPUT PARAMETER lNullposter      AS LOGICAL    NO-UNDO.
  DEFINE OUTPUT PARAMETER iSortering       AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER cButikker        AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cAvgrensButikker AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER dFraDato         AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER dTilDato         AS DATE NO-UNDO.
  DEFINE OUTPUT PARAMETER lPeriode         AS LOGICAL  NO-UNDO.
  DEFINE OUTPUT PARAMETER lVisStr          AS LOGICAL  NO-UNDO.
  DEFINE OUTPUT PARAMETER lNullager        AS LOGICAL  NO-UNDO.
  DEFINE OUTPUT PARAMETER lArtAnalys       AS LOGICAL  NO-UNDO.
&ENDIF


/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cReturVerdi       AS CHARACTER INIT "AVBRYT" NO-UNDO.
DEFINE VARIABLE IO-Liste          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButikerRowIdList AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wTittel           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSprak            AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTitel            AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE TT_TillgButikker
    FIELD butik LIKE butiker.butik.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Butikker RECT-64 RECT-65 RECT-66 TG-VisStr ~
TG-0Lager TG-Analys TOGGLE-4 RS-Sortering T-Periode FI-Butikker Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS TG-VisStr TG-0Lager TG-Analys TOGGLE-4 ~
RS-Sortering T-Periode FI-FraDato FI-TilDato FI-Butikker 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Butikker 
     IMAGE-UP FILE "icon/e-sokpr.ico":U NO-FOCUS
     LABEL "&Merk..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato1  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokDato2  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butikker AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikker" 
     VIEW-AS FILL-IN 
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraDato AS DATE FORMAT "99/99/99":U 
     LABEL "Periode" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilDato AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Sortering AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Varegruppesortering", 1,
"Leverandørsortering", 2
     SIZE 37 BY 1.67 NO-UNDO.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 4.52.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 2.62.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 50 BY 1.67.

DEFINE VARIABLE T-Periode AS LOGICAL INITIAL no 
     LABEL "Periodisert" 
     VIEW-AS TOGGLE-BOX
     SIZE 37 BY .81 NO-UNDO.

DEFINE VARIABLE TG-0Lager AS LOGICAL INITIAL yes 
     LABEL "0 Lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .76 NO-UNDO.

DEFINE VARIABLE TG-Analys AS LOGICAL INITIAL no 
     LABEL "ArtikelAnalys" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .76 NO-UNDO.

DEFINE VARIABLE TG-VisStr AS LOGICAL INITIAL yes 
     LABEL "Vis størrelser" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .76 NO-UNDO.

DEFINE VARIABLE TOGGLE-4 AS LOGICAL INITIAL yes 
     LABEL "Inkluder: 0 Solgt, 0 Kjøpt" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Butikker AT ROW 8.62 COL 46.6 NO-TAB-STOP 
     TG-VisStr AT ROW 1.29 COL 4
     TG-0Lager AT ROW 2.14 COL 4
     TG-Analys AT ROW 2.14 COL 20
     BUTTON-SokDato1 AT ROW 7.14 COL 27.4 NO-TAB-STOP 
     TOGGLE-4 AT ROW 3 COL 4
     RS-Sortering AT ROW 3.91 COL 4 NO-LABEL
     T-Periode AT ROW 6.14 COL 4
     FI-FraDato AT ROW 7.14 COL 11 COLON-ALIGNED
     FI-TilDato AT ROW 7.14 COL 30.4 COLON-ALIGNED NO-LABEL
     FI-Butikker AT ROW 8.62 COL 11 COLON-ALIGNED
     Btn_OK AT ROW 10.14 COL 2
     Btn_Cancel AT ROW 10.14 COL 36.8
     BUTTON-SokDato2 AT ROW 7.14 COL 46.6 NO-TAB-STOP 
     RECT-64 AT ROW 1.14 COL 2
     RECT-65 AT ROW 5.81 COL 2
     RECT-66 AT ROW 8.43 COL 2
     SPACE(6.00) SKIP(1.22)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Avgrensing lagerliste" 
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-SokDato1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-SokDato2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-Butikker:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-FraDato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TilDato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Avgrensing lagerliste */
DO:
    ASSIGN lNullager   = INPUT TG-0lager
           lArtAnalys  = INPUT TG-Analys
           lNullposter = INPUT TOGGLE-4
           iSortering  = INPUT RS-Sortering
           cButikker   = FI-Butikker:SCREEN-VALUE
           lVisStr     = INPUT TG-VisStr
           cReturVerdi = "OK".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Avgrensing lagerliste */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Butikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Butikker Dialog-Frame
ON CHOOSE OF B-Butikker IN FRAME Dialog-Frame /* Merk... */
DO:
    DEFINE VARIABLE bOK               AS LOGICAL    NO-UNDO.
    DEFINE VARIABLE cButikerIdList    AS CHARACTER  NO-UNDO.

    ASSIGN cButikerIdList = REPLACE(FI-Butikker,",","|").
/*                                                      */
/*   assign                                             */
/*     IO-Liste = FI-Butikker.                          */
/*   run d-tagbutikerBgrp.w (input-output IO-Liste).    */
/*   IF RETURN-VALUE = "Avbryt" THEN                    */
/*         RETURN NO-APPLY.                             */
/*   IF IO-Liste <> "" THEN                             */
/*       ASSIGN FI-Butikker = IO-Liste                  */
/*              FI-Butikker:SCREEN-VALUE = FI-Butikker. */
     RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                         "Butiker;Butik;ButNamn",
                         "where CAN-DO('" + FI-Butikker + "',STRING(Butiker.Butik))",
                         INPUT-OUTPUT cButikerRowIdList,
                         "Butik",
                         INPUT-OUTPUT cButikerIdList,
                         "","",
                         OUTPUT bOK).
     IF bOK THEN DO:
         ASSIGN FI-Butikker = REPLACE(cButikerIdList,"|",",")
                FI-Butikker:SCREEN-VALUE = FI-Butikker.
     END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF INPUT T-Periode = TRUE THEN
  DO:
      IF INPUT FI-FraDato = ? OR INPUT FI-TilDato = ? THEN
      DO:
          MESSAGE "Fra og til dato må angis."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      IF (INPUT FI-FraDato > INPUT FI-TilDato) THEN
      DO:
          MESSAGE "Feil datoangivelse."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato1 Dialog-Frame
ON CHOOSE OF BUTTON-SokDato1 IN FRAME Dialog-Frame /* ... */
or F10 of FI-FraDato
DO:
  do with frame Dialog-Frame:  
    assign 
      FI-FraDato = date(FI-FraDato:screen-value).

    wTittel = "Datosøk".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-FraDato
      &Program     = kalender.w
      &Frame       = Dialog-Frame
      &ExtraParam  = "input wTittel"
    } 
    return no-apply.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato2 Dialog-Frame
ON CHOOSE OF BUTTON-SokDato2 IN FRAME Dialog-Frame /* ... */
or F10 of FI-TilDato
DO:
  do with frame Dialog-Frame:  
    assign 
      FI-TilDato = date(FI-TilDato:screen-value).

    wTittel = "Datosøk".
  
    /* Start søkeprogram */
    {soek.i
      &Felt        = FI-TilDato
      &Program     = kalender.w
      &Frame       = Dialog-Frame
      &ExtraParam  = "input wTittel"
    } 
    return no-apply.  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Periode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Periode Dialog-Frame
ON VALUE-CHANGED OF T-Periode IN FRAME Dialog-Frame /* Periodisert */
DO:
  
  IF INPUT T-Periode = TRUE THEN
      ASSIGN
      FI-FraDato:SENSITIVE      = TRUE
      FI-TilDato:SENSITIVE      = TRUE
      BUTTON-SokDato1:SENSITIVE = TRUE 
      BUTTON-SokDato2:SENSITIVE = TRUE
      .
  ELSE
      ASSIGN
      FI-FraDato:SENSITIVE      = FALSE 
      FI-TilDato:SENSITIVE      = FALSE
      BUTTON-SokDato1:SENSITIVE = FALSE
      BUTTON-SokDato2:SENSITIVE = FALSE
      .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
   cSprak = TRIM(Bruker.Lng).

IF CAN-DO("SVE,SE",TRIM(cSprak)) THEN
  ASSIGN cTitel = "Avgrensning lagerlista".
ELSE
  ASSIGN cTitel = "Avgrensing lagerliste".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitBrukerButikker.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.


DO WITH FRAME Default-Frame:
  IF INPUT T-Periode THEN
    ASSIGN
        dFraDato = INPUT FI-FraDato
        dTilDato = INPUT FI-TilDato
        lPeriode = INPUT T-Periode
        .
  ELSE DO:
      ASSIGN
          dFraDato = ?
          dTilDato = ?
          lPeriode = FALSE
          .
  END.
END.

RUN disable_UI.
RETURN cReturVerdi.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY TG-VisStr TG-0Lager TG-Analys TOGGLE-4 RS-Sortering T-Periode 
          FI-FraDato FI-TilDato FI-Butikker 
      WITH FRAME Dialog-Frame.
  ENABLE B-Butikker RECT-64 RECT-65 RECT-66 TG-VisStr TG-0Lager TG-Analys 
         TOGGLE-4 RS-Sortering T-Periode FI-Butikker Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitBrukerButikker Dialog-Frame 
PROCEDURE InitBrukerButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND Bruker WHERE Bruker.BrukerID = USERID("skotex") NO-LOCK.
/*     FIND BrukerGrp WHERE BrukerGrp.BrGrpNr = Bruker.BrGrpNr NO-LOCK.                                        */
/*     FOR EACH butikktilgang OF BrukerGrp WHERE CAN-DO(cAvgrensButikker,STRING(butikktilgang.butik)) NO-LOCK. */
/*         ASSIGN FI-Butikker = FI-Butikker + (IF FI-Butikker <> "" THEN "," ELSE "") +                        */
/*                               STRING(butikktilgang.butik).                                                  */
/*     END.                                                                                                    */
    FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                      ButikkTeam.TeamTypeId = 2.
        FOR EACH ButikkKobling OF ButikkTeam.
            IF NOT CAN-DO(cAvgrensButikker,STRING(ButikkKobling.butik)) THEN
                NEXT.
            FIND Butiker WHERE Butiker.Butik = ButikkKobling.butik NO-LOCK NO-ERROR.
            IF NOT AVAIL Butiker THEN
                NEXT.
            ASSIGN cButikerRowIdList = cButikerRowIdList + (IF cButikerRowIdList <> "" THEN "," ELSE "") + STRING(ROWID(Butiker)).
            FIND TT_TillgButikker WHERE TT_TillgButikker.Butik = ButikkKobling.butik NO-ERROR.
            IF NOT AVAIL TT_TillgButikker THEN DO:
                    CREATE TT_TillgButikker.
                    ASSIGN TT_TillgButikker.Butik = ButikkKobling.butik.
            END.
        END.
    END.
    FOR EACH TT_TillgButikker BY TT_TillgButikker.butik:
        ASSIGN FI-Butikker = FI-Butikker + (IF FI-Butikker <> "" THEN "," ELSE "") + STRING(TT_TillgButikker.Butik).
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

