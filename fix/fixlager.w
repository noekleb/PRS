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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
def var wStop as log no-undo.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-44 RECT-45 FI-Vg1 FI-Vg2 FI-LopNr1 ~
FI-LopNr2 FI-FraButik FI-TilButik Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 ~
FI-FraButik FI-TilButik FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Start" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-FraButik AS INTEGER FORMAT "z9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr1 AS INTEGER FORMAT "zzz9":U INITIAL 0 
     LABEL "Løpenummer" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LopNr2 AS INTEGER FORMAT "zzz9":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TilButik AS INTEGER FORMAT "z9":U INITIAL 99 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg1 AS INTEGER FORMAT "zz9":U INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vg2 AS INTEGER FORMAT "zz9":U INITIAL 999 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 5.48.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 60 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Vg1 AT ROW 2.91 COL 17 COLON-ALIGNED
     FI-Vg2 AT ROW 2.91 COL 32 COLON-ALIGNED NO-LABEL
     FI-LopNr1 AT ROW 4.1 COL 17 COLON-ALIGNED
     FI-LopNr2 AT ROW 4.1 COL 32 COLON-ALIGNED NO-LABEL
     FI-FraButik AT ROW 5.29 COL 17 COLON-ALIGNED
     FI-TilButik AT ROW 5.29 COL 32 COLON-ALIGNED NO-LABEL
     FI-Info AT ROW 7.67 COL 2 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 1.52 COL 49
     Btn_Cancel AT ROW 2.76 COL 49
     Btn_Help AT ROW 6.19 COL 49
     RECT-44 AT ROW 1.95 COL 4
     RECT-45 AT ROW 8.86 COL 4
     "Lagerfil opprettes med utgangspunkt i ArtLAg" VIEW-AS TEXT
          SIZE 44 BY .62 AT ROW 9.57 COL 11
     "Fra" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 19
     "Til" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 34
     "Kriterier" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 5
          FONT 6
     SPACE(51.39) SKIP(8.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Korreksjon/opprettelse av Lager"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Info IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Korreksjon/opprettelse av Lager */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel Dialog-Frame
ON CHOOSE OF Btn_Cancel IN FRAME Dialog-Frame /* Avbryt */
DO:
  assign
    wStop = true.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Start */
DO:
  run OpprettLager.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i &WindowName = "Fix av Lager"}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN enable_UI.
  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

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
  DISPLAY FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 FI-FraButik FI-TilButik FI-Info 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-44 RECT-45 FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 FI-FraButik 
         FI-TilButik Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettLager Dialog-Frame 
PROCEDURE OpprettLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk          as log format "Ja/Nei" no-undo.
  def var wLoop        as int                 no-undo.
  def var wSkjerm      as char                no-undo.
  def var wTekst       as char                no-undo.
  def var wTilbud      as log                 no-undo.
  
  assign frame Dialog-Frame
    FI-Vg1
    FI-Vg2
    FI-LopNr1
    FI-LopNr2
    FI-FraButik
    FI-TilButik
    wOk = false
    FI-Info:screen-value in frame Dialog-Frame = "Sett inn avgrensning.".

  message "Skal oppdatering/opprettelse av Lager startes?"
         view-as alert-box BUTTONS YES-NO title "Start av jobb"
         update wOk.
  if wOk <> true then
    return no-apply.
  
  /* Leser aktuelle artikkler. */
  ARTBAS:
  for each ArtBas no-lock where
    ArtBas.Vg    >= FI-Vg1    and
    ArtBas.Vg    <= FI-Vg2    and
    ArtBas.LopNr >= FI-LopNr1 and
    ArtBas.LopNr <= FI-LopNr2:

    /* Bruker avbryter. */
    PROCESS EVENTS.
    if wStop then
      do:
        message "Skal korreksjonsrutinen stoppes? " view-as alert-box buttons YES-NO 
                 set wStop.
        if wStop = true then
          return no-apply.
      end.

    /* Info til bruker. */
    assign
      FI-Info:screen-value in frame Dialog-Frame =
        "Artikkel: " + string(ArtBas.ArtikkelNr) + " - " +
        string(ArtBas.Vg) + "/" + string(ArtBas.LopNr) + " " +
        ArtBas.Beskr.
    
    /* Lager korrigeres/opprettes pr. butikk. */
    BUTIKKLOOP:
    for each Butiker no-lock where
      Butiker.Butik >= FI-FraButik and
      Butiker.Butik <= FI-TilButik TRANSACTION:         

      /* Butikk 0 driter vi i. */
      if Butiker.Butik = 0 then
        next BUTIKKLOOP.

      /* Sletter eksisterende lagerposter for åunngå dobbeloppdatering. */
      find Lager exclusive-lock where
        Lager.ArtikkelNr = ArtBas.ArtikkelNr and
        Lager.Butik      = Butiker.Butik no-error.
      if available Lager then
        delete Lager.
      
      /* Opprettet ny post. */
      do:
        create Lager.
        assign 
          Lager.ArtikkelNr = ArtBas.ArtikkelNr
          Lager.Butik      = Butiker.Butik.
      end.

      /* Leser artlag for butikken og artikkel */
      ARTLAGLOOP:
      for each ArtLag exclusive-lock where
        ArtLag.Vg    = ArtBas.Vg    and
        ArtLag.LopNr = ArtBas.LopNr and
        ArtLag.Butik = Butiker.Butik:
        
        /* Oppdaterer lagerpost med gammel morro. */
        assign
          Lager.JustAnt      = Lager.JustAnt      + ArtLag.Primo
          Lager.JustVerdi    = Lager.JustVerdi    + (ArtLag.Primo * ArtLag.NetPris)
          Lager.LagAnt       = Lager.LagAnt       + artLag.LagAnt
          Lager.VVareKost    = (if Lager.VVareKost = 0
                                 then ArtLag.NetPris          
                                 else Lager.VVareKost)
          Lager.VerdiSolgt   = Lager.VerdiSolgt   + ArtLag.SalSum
          Lager.AntSolgt     = Lager.AntSolgt     + ArtLag.SalAnt
          Lager.KjopAnt      = Lager.KjopAnt      + ArtLag.Kopant
          Lager.KjopVerdi    = Lager.KjopVerdi    + ArtLag.Kopsum
          Lager.OvAnt        = Lager.OvAnt        + ArtLAg.Overfort
          Lager.GjenkjopAnt  = Lager.GjenkjopAnt  + ArtLag.RetAnt          
          Lager.ReklAnt      = Lager.ReklAnt      + ArtLag.RekAnt
          Lager.SvinnAnt     = Lager.SvinnAnt     + ArtLag.primo.
          
          /* Oppdaterer nye felt i ArtLag. */
          assign
            ArtLag.ArtikkelNr  = ArtBas.ArtikkelNr
            artlag.antsolgt    = artlag.salant
            artlag.kjopant     = artlag.kopant
            artlag.ovant       = artlag.overfort
            artlag.reklant     = artlag.rekant
            artlag.gjenkjopant = artlag.retant
            artlag.SvinnAnt    = artlag.primo.
      end. /* ARTLAGLOOP */
      
      /* Flagger at oppdatering er OK */
      assign
        wOk = true.
    end. /* BUTIKKLOOP TRANSACTION*/
  end. /* ARTBAS */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

