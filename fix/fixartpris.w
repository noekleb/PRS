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
FI-LopNr2 FI-Profiler Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-Info FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 ~
FI-Profiler 

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

DEFINE VARIABLE FI-Profiler AS CHARACTER FORMAT "X(256)":U INITIAL "1" 
     LABEL "ProfilNrListe" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

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
     SIZE 60 BY 5.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-Info AT ROW 7.67 COL 2 COLON-ALIGNED NO-LABEL
     FI-Vg1 AT ROW 2.91 COL 17 COLON-ALIGNED
     FI-Vg2 AT ROW 2.91 COL 32 COLON-ALIGNED NO-LABEL
     FI-LopNr1 AT ROW 4.1 COL 17 COLON-ALIGNED
     FI-LopNr2 AT ROW 4.1 COL 32 COLON-ALIGNED NO-LABEL
     FI-Profiler AT ROW 5.29 COL 17 COLON-ALIGNED HELP
          "Kommaseparert liste med profilnummer"
     Btn_OK AT ROW 1.52 COL 49
     Btn_Cancel AT ROW 2.76 COL 49
     Btn_Help AT ROW 6.19 COL 49
     RECT-44 AT ROW 1.95 COL 4
     RECT-45 AT ROW 8.86 COL 4
     "Priser hentes fra prisfelt i gamle SkoTex og oppdateres" VIEW-AS TEXT
          SIZE 54 BY .62 AT ROW 10.76 COL 8
     "i de nye prisfeltene." VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 11.48 COL 23
     "Mangler ArtPris, opprettes denne. Finnes ArtPris, skrives" VIEW-AS TEXT
          SIZE 55 BY .62 AT ROW 12.67 COL 7
     "prisene over med informasjon fra de gamle prisfeltene." VIEW-AS TEXT
          SIZE 52 BY .62 AT ROW 13.38 COL 9
     "PrisProfil må være satt på butikkene" VIEW-AS TEXT
          SIZE 44 BY .95 AT ROW 9.33 COL 12
          FGCOLOR 12 FONT 6
     "Fra" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 19
     "Til" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 34
     "Kriterier" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 5
          FONT 6
     SPACE(51.39) SKIP(12.37)
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


&Scoped-define SELF-NAME FI-Profiler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Profiler Dialog-Frame
ON LEAVE OF FI-Profiler IN FRAME Dialog-Frame /* ProfilNrListe */
DO:
  if input FI-Profiler = "" then
    do:
      message "Profilnummer må angis!"
              view-as alert-box title "Melding".
      return no-apply.
    end.
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
  DISPLAY FI-Info FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 FI-Profiler 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-44 RECT-45 FI-Vg1 FI-Vg2 FI-LopNr1 FI-LopNr2 FI-Profiler Btn_OK 
         Btn_Cancel Btn_Help 
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
    FI-Vg1
    FI-Vg2
    FI-LopNr1
    FI-LopNr2
    wOk = false
    FI-Info:screen-value in frame Dialog-Frame = "Sett inn avgrensning.".

  message "Skal oppdatering/opprettelse av ArtPris startes?"
         view-as alert-box BUTTONS YES-NO title "Start av jobb"
         update wOk.
  if wOk <> true then
    return no-apply.
  
  /* Leser varegruppene */
  VAREGR:
  for each VarGr no-lock where
    VarGr.Vg >= FI-Vg1 and 
    VarGr.Vg <= FI-Vg2:
  
  /* Momsen. */
  find Moms of VarGr no-lock no-error.
  if not available Moms then
    next VAREGR.
  
  /* Leser aktuelle artikkler. */
  ARTBAS:
  for each ArtBas no-lock where
    ArtBas.Vg     = VarGr.Vg and
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
    
    /* Leverandør */
    find LevBas of ArtBas no-lock no-error.
    if not available LevBas then
      next ARTBAS.

    find Valuta of ArtBas no-lock no-error.
    if not available Valuta then
      next ARTBAS.
            
    LOOPEN:
    do wLoop = 1 to num-entries(FI-Profiler):

      assign
        wProfilNr = int(entry(wLoop,FI-Profiler)).
      
      find PrisProfil no-lock where
        PrisProfil.ProfilNr = wProfilNr no-error.
      if not available PrisProfil then
        next LOOPEN.

      /* Henter strengen (wSkjerm) fra kalkylemodulen. */  
      if valid-handle(wLibHandle) then
        run InitKalkyle in wLibHandle
          (input recid(ArtBas), 
           input PrisProfil.ProfilNr,
           input-output wSkjerm,
           input Moms.MomsProc,
           input Valuta.ValKurs, 
           input 1,
           input false).      
           
      FIND FIRST Butiker NO-LOCK where
        Butiker.Butik > 0 and
        butiker.ProfilNr = wProfilNr and
        can-find(first ArtLag where 
                       ArtLag.Butik = Butiker.Butik and
                       ArtLag.Vg    = ArtBas.Vg     and
                       ArtLag.LopNr = ArtBas.LopNr) NO-ERROR.

      assign
        wTilbud = false.

      find first ArtLag no-lock where
        ArtLag.Butik = Butiker.Butik and
        ArtLag.Vg    = ArtBas.Vg     and
        ArtLag.LopNr = ArtBas.LopNr no-error.
      if available ArtLag then
        do:
          /* Setter valutapris fra ArtBas. */
          wSkjerm = ByttElement(input wSkjerm,
                          input 1,
                          input string(ArtBas.ValPris),
                          input ";").
          /* Innkjøpspris */
          wSkjerm = ByttElement(input wSkjerm,
                          input 2,
                          input string(ArtLag.Normpris),
                          input ";").
          /* Varekost (Innkjøpspris korrigert for rabatter) */
          wSkjerm = ByttElement(input wSkjerm,
                          input 13,
                          input string(ArtLag.Normpris),
                          input ";").
          /* Pris */
          wSkjerm = ByttElement(input wSkjerm,
                          input 18,
                          input string(ArtLag.Pris),
                          input ";").

          /* Sjekker om tilbud er aktivt. Hvis Ja, settes tilbudspris o.l. */
          if ArtLag.ReaFom > 800000 then
            TILBUDSJEKK:
            do:
              /* Setter fra/til dato. */
              assign
                wDatoF  = date(
                               int(substring(string(ArtLag.ReaFom,"999999"),3,2)),
                               int(substring(string(ArtLag.ReaFom,"999999"),5,2)),
                               1900 + int(substring(string(ArtLag.ReaFom,"999999"),1,2))
                              )
                wDatoT  = date(
                               int(substring(string(ArtLag.ReaTom,"999999"),3,2)),
                               int(substring(string(ArtLag.ReaTom,"999999"),5,2)),
                               1900 + int(substring(string(ArtLag.ReaTom,"999999"),1,2))
                              ).
              /* TilbudsPris */
              if wDatoF <= today and
                 wDatoT >= today then
                AKTIVT_TILBUD:
                do:
                  assign
                    wTilbud = true.

                  /* Tilbud fra */
                  wSkjerm = ByttElement(input wSkjerm,
                                        input 21,
                                        input string(wDatoF),
                                        input ";").
                  /* Tilbud til */
                  wSkjerm = ByttElement(input wSkjerm,
                                        input 22,
                                        input string(wDatoT),
                                        input ";").
                  /* Tilbudspris */
                  wSkjerm = ByttElement(input wSkjerm,
                                        input 18,
                                        input string(ArtLag.ReaPris),
                                        input ";").
                  /* Tilbud fra tid */
                  wSkjerm = ByttElement(input wSkjerm,
                                        input 23,
                                        input "0",
                                        input ";").
                  /* Tilbud til tid */
                  wSkjerm = ByttElement(input wSkjerm,
                                        input 24,
                                        input string((24 * 60 * 60) - 60),
                                        input ";").
                  /* Tilbud timestyrt */
                  wSkjerm = ByttElement(input wSkjerm,
                                        input 25,
                                        input "false",
                                        input ";").
                end. /* AKTIVT_TILBUD */
            end. /* TILBUDSJEKK */
        end. /* Tilgjengelig ArtLAg */

        /* Starter omkalkulering.                              */
        /* Simulerer at cursor forlater prisfeltet i kalkylen. */
        /* NB: Kalkulasjonen skjer i prosedyrebilboteket.      */
        if valid-handle(wLibHandle) then
          run Omregning in wLibHandle
            (input recid(ArtBas),
             input PrisProfil.ProfilNr,
             input-output wSkjerm,
             input Moms.MomsProc,
             input Valuta.ValKurs,
             input 18,
             input wTilbud).

        /* Fiffer opp strengen med felt som mangler.*/
        if wTilbud then
          wSkjerm = wSkjerm                          + ";" +  /* Felt 1 --> 19      */
                    "False"                          + ";" +  /* 20 EuroManuel      */
                    string(wDatoF)                   + ";" +  /* 21 TilbudFraDato   */
                    string(wDatoT)                   + ";" +  /* 22 TilbudTilDato   */
                    string(0)                        + ";" +  /* 23 TilbudFraTid    */
                    string((23 * 60 * 60) + 59 * 60) + ";" +  /* 24 TilbudTilTid    */
                    "false".                                  /* 25 TilbudTimestyrt */
        else
          wSkjerm = wSkjerm         + ";" +  /* Felt 1 --> 19      */
                    "False"         + ";" +  /* 20 EuroManuel      */
                    string(today)   + ";" +  /* 21 AktivFraDato    */
                    ""              + ";" +  /* 22 TilbudFraDato   */
                    string(0)       + ";" +  /* 25 AktivFraTid     */
                    string(0)       + ";" +  /* 26 TilbudTilTid    */
                    "false".                 /* 27 TilbudTimestyrt */

        /* Oppdaterer ordinærkalkyle */
        if valid-handle(wLibHandle) then
          run LagreArtPris in wLibHandle
              (input recid(ArtBas),
               input PrisProfil.ProfilNr,
               input-output wSkjerm,
               input false,  /* wTilbud = false - Dvs ordinær kaflkyle.         */
               input true,   /* Direkte oppdatering av prisene som er kalkulert */
               input false). /* Ingen oppdatering av prisko.    
                               */
        /* er tilbud aktiv, skal tilbudspris også oppdateres. */
        if wTilbud then
          if valid-handle(wLibHandle) then
            run LagreArtPris in wLibHandle
                (input recid(ArtBas),
                 input PrisProfil.ProfilNr,
                 input-output wSkjerm,
                 input wTilbud,
                 input true,   /* Direkte oppdatering av prisene som er kalkulert */
                 input false). /* Ingen oppdatering av prisko.                    */
        assign
          wOk = true.
    END. /* LOOPEN */

  end. /* ARTBAS */
  end. /* VAREGR  */

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

