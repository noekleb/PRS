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

  def temp-table tmpBilder
    field BildePeker as char.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-44 RECT-45 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS FI-BildNr1 FI-BildNr2 FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "&Start" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-BildNr1 AS INTEGER FORMAT "zzz9":U INITIAL 1 
     LABEL "Bildenummer" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BildNr2 AS INTEGER FORMAT "zzz9":U INITIAL 9999 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 61 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 43 BY 2.38.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 1.91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-BildNr1 AT ROW 2.91 COL 16 COLON-ALIGNED
     FI-BildNr2 AT ROW 2.91 COL 29 COLON-ALIGNED NO-LABEL
     FI-Info AT ROW 12.48 COL 1 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 1.95 COL 49
     Btn_Cancel AT ROW 3.38 COL 49
     Btn_Help AT ROW 11 COL 49
     RECT-44 AT ROW 1.95 COL 3
     RECT-45 AT ROW 13.67 COL 3
     "Kriterier" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 1.48 COL 4
          FONT 6
     "Til" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 31
     "Fra" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.19 COL 18
     "Korrigerer bilder for 19/9-99." VIEW-AS TEXT
          SIZE 58 BY .62 AT ROW 14.38 COL 5
     SPACE(1.79) SKIP(0.75)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Innlesning av bilder i databasen"
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

/* SETTINGS FOR FILL-IN FI-BildNr1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-BildNr2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 4.57
       COLUMN       = 3
       HEIGHT       = 7.52
       WIDTH        = 42.2
       HIDDEN       = no
       SENSITIVE    = yes.
      Image-Sko:NAME = "Image-Sko":U .
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      Image-Sko:MOVE-AFTER(FI-BildNr2:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Innlesning av bilder i databasen */
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
  RUN LesInnBilde.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Image-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Image-Sko Dialog-Frame OCX.DblClick
PROCEDURE Image-Sko.Picbuf.DblClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

  if available BildeRegister then
    run d-visbil.w (input recid(BildeRegister)).
  return no-apply.    


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i &WindowName = "Fix av artlag.storl"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame  _CONTROL-LOAD
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

OCXFile = SEARCH( "fixlaptopbilder.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "fixlaptopbilder.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  RUN control_load.
  DISPLAY FI-BildNr1 FI-BildNr2 FI-Info 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-44 RECT-45 Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnBilde Dialog-Frame 
PROCEDURE LesInnBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wOk          as log format "Ja/Nei" no-undo.
  def var wReturnValue as char                no-undo.
  def var wLoop        as int                 no-undo.
  def var wSkjerm      as char                no-undo.
  def var wTilbud      as log                 no-undo.
  def var wUnique      as char                no-undo.
  def var wBildePeker  as char                no-undo.
  def var wNyttFilNavn as char                no-undo.
  def var wFilNavn     as char                no-undo. 
  def var wBildNr      as int                 no-undo.
  def var wKatalog     as char                no-undo.
  def var wSlettBilde  as char                no-undo.
  
  assign frame Dialog-Frame
    FI-BildNr1
    FI-BildNr2
    wOk = false
    FI-Info:screen-value in frame Dialog-Frame = "Sett inn avgrensning.".

  message "Skal innlesning av bilder starte?"
         view-as alert-box BUTTONS YES-NO title "Start av innlesning"
         update wOk.
  if wOk <> true then
    return no-apply.
    
    
  output to value("LapTopfix.Log") append.
  export string(today) string(time).
  output close.
  
  BILDEREGISTER:
  /* Leser aktuelle leverandører. */
  for each artbas exclusive-lock where
    artbas.Edato = 09/19/1999:

    output to value("LapTopfix.Log") append.
    export ArtBas.ArtikkelNr 
           ArtBas.BildNr
           ArtBas.Vg
           ArtBas.LopNr
           .
    output close.
    
    find bilderegister of ArtBas no-lock no-error.

    /* Bruker avbryter. */
    PROCESS EVENTS.
    if wStop then
      do:
        message "Skal innlesning stoppes? " view-as alert-box buttons YES-NO 
                 set wStop.
        if wStop = true then
          leave BILDEREGISTER.
      end.

    /* Info til bruker. */
    assign
      FI-Info:screen-value in frame Dialog-Frame =
        "Bilde: " + string(Bilderegister.BildNr) + " " + BildeRegister.Merknad.       

    assign
      wBildePeker = if available BildeRegister 
                      then BildeRegister.FilNavn
                      else "".
    if valid-handle(wLibHandle) then
     run HentBildePeker in wLibHandle (input BildeRegister.BildNr, input 1, wBildePeker, output wBildePeker).
    
    assign
      chIMAGE-Sko:Picbuf:filename  = search(wBildePeker)
      chIMAGE-Sko:Picbuf:AutoScale = True.        
    wOk = chIMAGE-Sko:Picbuf:load.    

    /* Finner første ledige bildenummer */
    if valid-handle(wLibHandle) then
    do:
      run BildeNummer in wLibHandle (input "B", output wBildNr, output wFilNavn, output wKatalog).
  
      if return-value = "AVBRYT" then
        do:
          message "Klarte ikke å skape et filnavn!" view-as alert-box 
            title "Melding".
          return no-apply.
        end.
      find BildeRegister no-lock where
         BildeRegister.BildNr = wBildNr no-error.

      assign ArtBas.BildNr = wBildNr.

    end.
    else do:
      message "Prosedyrebiblioteket er ikke startet!" view-as alert-box Title "Melding".
      return no-apply.
    end.
    
    /* Tildeler filnavn */
    chIMAGE-Sko:Picbuf:FileName = wKatalog + "\" + wFilNavn.
  
    /* Lagrer bilde på hd. ------------------------------------------------ */
    If chIMAGE-Sko:Picbuf:WriteCompression = 0 Then  /* Filen skal komprimeres.    */
      chIMAGE-Sko:Picbuf:WriteCompression = 65.
    chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
    If chIMAGE-Sko:Picbuf:WriteCompression <> 0 Then /* Filen skal komprimeres.    */
      chIMAGE-Sko:Picbuf:WriteCompression = 0.
    
    /* Leser inn filen. */
    assign wReturnValue = "AVBRYT".
    if search(wKatalog + "\" + wFilNavn) <> ? then
      do:
        if valid-handle(wLibHandle) then
          run LesInnBilde in wLibHandle (BildeRegister.BildNr, wKatalog + "\" + wFilNavn, output wReturnValue).
      end.
    if wReturnValue = "AVBRYT" then
      do:
        message "Feil ved lasting av bilde " BildeRegister.BildNr
          view-as alert-box error title "Feil".
      end.

    find first tmpBilder where
      tmpBilder.BildePeker = wBildePeker no-error.
    if not available tmpBilder then
      do:
        create tmpBilder.
        tmpBilder.BildePeker = wBildePeker.
      end.
    
    /* Flagger at oppdatering er OK */
    assign
      wOk = true.      
      
  end. /* BILDEREGISTER */
  
  if available ArtBas  then release ArtBas.
  if available ArtLag  then release ArtLag.
  if available Lager   then release Lager.
  
  for each tmpBilder:
    os-delete value(tmpBilder.BildePeker).
    delete tmpBilder.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

