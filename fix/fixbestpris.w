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
&Scoped-Define ENABLED-OBJECTS FILL-IN-16 RECT-44 RECT-45 FI-BestNr1 ~
FI-BestNr2 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-16 FI-Info FI-BestNr1 FI-BestNr2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ByttElement Dialog-Frame 
FUNCTION ByttElement RETURNS CHARACTER
  ( input ipSkjerm as char,
    input ipElement as int,
    input ipNyttElement as char,
    input ipDelimiter as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE VARIABLE FI-BestNr1 AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Bestillingsnummer" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BestNr2 AS INTEGER FORMAT ">>>>>>9":U INITIAL 9999999 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 83 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-16 AS CHARACTER FORMAT "X(256)":U INITIAL "Korreksjon av kalkyle på bestillinger med status 4 etter import fra FlexiCon" 
      VIEW-AS TEXT 
     SIZE 71 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 5.48.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-16 AT ROW 11.48 COL 8 COLON-ALIGNED NO-LABEL
     FI-Info AT ROW 7.67 COL 2 COLON-ALIGNED NO-LABEL
     FI-BestNr1 AT ROW 3.38 COL 33 COLON-ALIGNED
     FI-BestNr2 AT ROW 3.38 COL 48 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 1.71 COL 72
     Btn_Cancel AT ROW 2.95 COL 72
     Btn_Help AT ROW 6.38 COL 72
     RECT-44 AT ROW 1.95 COL 4
     RECT-45 AT ROW 8.86 COL 4
     "Flytter Rab1-Kr til Rab1-% feltet" VIEW-AS TEXT
          SIZE 44 BY .95 AT ROW 9.81 COL 24
          FGCOLOR 12 FONT 6
     "Fra" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 35
     "Til" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 50
     "Kriterier" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 5
          FONT 6
     SPACE(73.59) SKIP(12.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Korreksjon/opprettelse av ArtPris"
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Korreksjon/opprettelse av ArtPris */
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
  run OpprettPris.
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
{genlib.i &WindowName = "Fix av ArtPris"}

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
  DISPLAY FILL-IN-16 FI-Info FI-BestNr1 FI-BestNr2 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-16 RECT-44 RECT-45 FI-BestNr1 FI-BestNr2 Btn_OK Btn_Cancel 
         Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettPris Dialog-Frame 
PROCEDURE OpprettPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk          as log format "Ja/Nei" no-undo.
  def var wLoop        as int                 no-undo.
  def var wProfilNr    as int                 no-undo.
  def var wSkjerm      as char                no-undo.
  def var wTekst       as char                no-undo.
  def var wDatoF       as date                no-undo.
  def var wDatoT       as date                no-undo.
  def var wTilbud      as log                 no-undo.
  
  assign frame Dialog-Frame
    FI-BestNr1
    FI-BestNr2
    wOk = false
    FI-Info:screen-value in frame Dialog-Frame = "Sett inn avgrensning.".

  message "Skal kalkylekorreksjonen startes?"
         view-as alert-box BUTTONS YES-NO title "Start av jobb"
         update wOk.
  if wOk <> true then
    return no-apply.
  
  /* Leser bestillingene */
  BESTHODE:
  for each BestHode no-lock where
    BestHode.BestNr >= FI-BestNr1 and
    BestHode.BestNr <= FI-BestNr2 and
    BestHode.BestStat = 4:

    /* Info til bruker. */
    assign
      FI-Info:screen-value in frame Dialog-Frame =
        "Bestilling: " + string(BestHode.BestNr).
        
    /* Henter artikkelen */
    find ArtBas no-lock where
      ArtBas.ArtikkelNr = BestHode.ArtikkelNr.
    find LevBas no-lock of ArtBas.
    find Valuta of LevBas no-lock.      

    /* Leser prisene */
    LOOPEN:
    for each BestPris of BestHode exclusive-lock where
      BestPris.BestStat = 4:
     
      /* Bruker avbryter. */
      PROCESS EVENTS.
      if wStop then
        do:
          message "Skal korreksjonsrutinen stoppes? " view-as alert-box buttons YES-NO 
                   set wStop.
          if wStop = true then
            do:
              wStop = false.
              return no-apply "AVBRYT".
            end.
        end.

      /* Flytter Rab1Kr til Rab1% feltet. */
      assign
        BestPris.Rab1% = BestPris.Rab1Kr.

      /* Bygger KalkyleStrengen */
      wSkjerm = string(BestPris.ValPris) + ";" +
                string(BestPris.InnkjopsPris) + ";" +
                string(BestPris.Rab1Kr) + ";" +
                string(BestPris.Rab1%) + ";" +
                string(BestPris.Rab2Kr) + ";" +
                string(BestPris.Rab2%) + ";" +
                string(BestPris.Frakt) + ";" +
                string(BestPris.Frakt%) + ";" +
                string(BestPris.DivKostKr) + ";" +
                string(BestPris.DivKost%) + ";" +
                string(BestPris.Rab3Kr) + ";" +
                string(BestPris.Rab3%) + ";" +
                string(BestPris.VareKost) + ";" +
                string(BestPris.MvaKr) + ";" +
                string(BestPris.Mva%) + ";" +
                string(BestPris.DBKr) + ";" +
                string(BestPris.DB%) + ";" +
                string(BestPris.Pris) + ";" +
                string(BestPris.EuroPris) + ";" +
                (if BestPris.EuroManuel = true
                   then "True"
                   else "False") + ";" +
                string(today) + ";" +
                string(today) + ";" +
                "0"  + ";" +
                "0"  + ";" +
                "false".
      assign
        wTilbud = false.

      /* Starter omkalkulering.                         */
      /* NB: Kalkulasjonen skjer i prosedyrebilboteket. */
      if valid-handle(wLibHandle) then
        run Omregning in wLibHandle
             (input recid(ArtBas), 
              input BestPris.ProfilNr,
              input-output wSkjerm,
              input BestPris.Mva%,
              input Valuta.ValKurs, 
              input 4,
              input false).
             
      /* Lagrer den nye kalkylen */        
      assign
        BestPris.ValPris      = dec(entry(1,wSkjerm,";"))
        BestPris.InnkjopsPris = dec(entry(2,wSkjerm,";"))
        BestPris.Rab1Kr       = dec(entry(3,wSkjerm,";"))
        BestPris.Rab1%        = dec(entry(4,wSkjerm,";"))
        BestPris.Rab2Kr       = dec(entry(5,wSkjerm,";"))
        BestPris.Rab2%        = dec(entry(6,wSkjerm,";"))
        BestPris.Frakt        = dec(entry(7,wSkjerm,";"))
        BestPris.Frakt%       = dec(entry(8,wSkjerm,";"))
        BestPris.DivKostKr    = dec(entry(9,wSkjerm,";"))
        BestPris.DivKost%     = dec(entry(10,wSkjerm,";"))
        BestPris.Rab3Kr       = dec(entry(11,wSkjerm,";"))
        BestPris.Rab3%        = dec(entry(12,wSkjerm,";"))
        BestPris.VareKost     = dec(entry(13,wSkjerm,";"))
        BestPris.MvaKr        = dec(entry(14,wSkjerm,";"))
        BestPris.Mva%         = dec(entry(15,wSkjerm,";"))
        BestPris.DbKr         = dec(entry(16,wSkjerm,";"))
        BestPris.DB%          = dec(entry(17,wSkjerm,";"))
        BestPris.Pris         = dec(entry(18,wSkjerm,";"))
        BestPris.EuroPris     = dec(entry(19,wSkjerm,";"))
        BestPris.EuroManuel   = false.  

    END. /* LOOPEN */

  end. /* BESTHODE  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ByttElement Dialog-Frame 
FUNCTION ByttElement RETURNS CHARACTER
  ( input ipSkjerm as char,
    input ipElement as int,
    input ipNyttElement as char,
    input ipDelimiter as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var ipLoop  as int no-undo.
  def var ipTekst as char no-undo.
  
  ipTekst = "".
  do ipLoop = 1 to num-entries(ipSkjerm,ipDelimiter):
    assign ipTekst = ipTekst + 
           (if ipTekst = ""
              then ""
              else ipDelimiter) +
           (if ipLoop = ipElement 
              then ipNyttElement
              else entry(ipLoop,ipSkjerm,ipDelimiter)). 
  end.

  RETURN ipTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

