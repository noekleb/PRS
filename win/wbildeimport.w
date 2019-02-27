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
DEF INPUT PARAMETER lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cBildeKatalog AS CHAR NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE Image-Sko AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chImage-Sko AS COMPONENT-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 44 BY 7.81.


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
         TITLE              = "Bildeimport"
         HEIGHT             = 7.81
         WIDTH              = 44
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME Image-Sko ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1.24
       COLUMN          = 2
       HEIGHT          = 7.52
       WIDTH           = 42.2
       HIDDEN          = no
       SENSITIVE       = yes.
/* Image-Sko OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Bildeimport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Bildeimport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Image-Sko
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Image-Sko C-Win OCX.DblClick
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

/* Bildekatalog */ 
{syspara.i 10 1 2 cBildeKatalog}
ASSIGN cBildeKatalog = TRIM(cBildeKatalog,"\") + "\".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluttProcedure C-Win 
PROCEDURE avsluttProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

OCXFile = SEARCH( "wbildeimport.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chImage-Sko = Image-Sko:COM-HANDLE
    UIB_S = chImage-Sko:LoadControls( OCXFile, "Image-Sko":U)
    Image-Sko:NAME = "Image-Sko":U
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "wbildeimport.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnBilde C-Win 
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
  
/*   assign frame Dialog-Frame                                                                                   */
/*     FI-BildNr1                                                                                                */
/*     FI-BildNr2                                                                                                */
/*     wOk = false                                                                                               */
/*     FI-Info:screen-value in frame Dialog-Frame = "Sett inn avgrensning.".                                     */
/*                                                                                                               */
/*   message "Skal innlesning av bilder starte?"                                                                 */
/*          view-as alert-box BUTTONS YES-NO title "Start av innlesning"                                         */
/*          update wOk.                                                                                          */
/*   if wOk <> true then                                                                                         */
/*     return no-apply.                                                                                          */
/*                                                                                                               */
/*   /* Leser aktuelle leverandører. */                                                                          */
/*   BILDEREGISTER:                                                                                              */
/*   for each BildeRegister exclusive-lock where                                                                 */
/*     BildeRegister.BildNr >= FI-BildNr1 and                                                                    */
/*     bildeRegister.BildNr <= FI-BildNr2:                                                                       */
/*                                                                                                               */
/*     /* Bruker avbryter. */                                                                                    */
/*     PROCESS EVENTS.                                                                                           */
/*     if wStop then                                                                                             */
/*       do:                                                                                                     */
/*         message "Skal innlesning stoppes? " view-as alert-box buttons YES-NO                                  */
/*                  set wStop.                                                                                   */
/*         if wStop = true then                                                                                  */
/*           leave BILDEREGISTER.                                                                                */
/*       end.                                                                                                    */
/*                                                                                                               */
/*     /* Info til bruker. */                                                                                    */
/*     assign                                                                                                    */
/*       FI-Info:screen-value in frame Dialog-Frame =                                                            */
/*         "Bilde: " + string(Bilderegister.BildNr) + " " + BildeRegister.Merknad.                               */
/*                                                                                                               */
/*     assign                                                                                                    */
/*       wBildePeker = if available BildeRegister                                                                */
/*                       then BildeRegister.FilNavn                                                              */
/*                       else "".                                                                                */
/*     if valid-handle(wLibHandle) then                                                                          */
/*      run HentBildePeker in wLibHandle (input BildeRegister.BildNr, input 1, wBildePeker, output wBildePeker). */
/*                                                                                                               */
/*     assign                                                                                                    */
/*       chIMAGE-Sko:Picbuf:filename  = search(wBildePeker)                                                      */
/*       chIMAGE-Sko:Picbuf:AutoScale = True.                                                                    */
/*     wOk = chIMAGE-Sko:Picbuf:load.                                                                            */
/*                                                                                                               */
/*     /* Leser inn filen. */                                                                                    */
/*     assign wReturnValue = "AVBRYT".                                                                           */
/*     if search(wBildePeker) <> ? then                                                                          */
/*       do:                                                                                                     */
/*         if valid-handle(wLibHandle) then                                                                      */
/*           run LesInnBilde in wLibHandle (BildeRegister.BildNr, wBildePeker, output wReturnValue).             */
/*       end.                                                                                                    */
/*     if wReturnValue = "AVBRYT" then                                                                           */
/*       do:                                                                                                     */
/*         message "Feil ved lasting av bilde " BildeRegister.BildNr                                             */
/*           view-as alert-box error title "Feil".                                                               */
/*       end.                                                                                                    */
/*                                                                                                               */
/*     /* Flagger at oppdatering er OK */                                                                        */
/*     assign                                                                                                    */
/*       wOk = true.                                                                                             */
/*                                                                                                               */
/*   end. /* BILDEREGISTER */                                                                                    */
/*                                                                                                               */
/*   if available ArtBas  then release ArtBas.                                                                   */
/*   if available ArtLag  then release ArtLag.                                                                   */
/*   if available Lager   then release Lager.                                                                    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyttBilde C-Win 
PROCEDURE NyttBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER  lArtikkelNr   AS DEC  NO-UNDO.
  DEFINE INPUT PARAMETER  cFilkatalog   AS CHAR NO-UNDO.
  DEFINE INPUT PARAMETER  cVPIBildeKode AS CHAR NO-UNDO.

  DEF VAR cClipTekst     as CHAR NO-UNDO.
  def var wReturnValue   as char no-undo.
  DEF VAR wBildNr        AS DEC  NO-UNDO.
  DEF VAR cFilNavn       AS CHAR NO-UNDO.

  def buffer bufArtBas for ArtBas.

  /* Setter fullstendig filnavn */
  IF SEARCH(cFilkatalog + "\" + cVPIBildeKode) <> ? THEN
      cFilNavn = SEARCH(cFilkatalog + "\" + cVPIBildeKode).
  ELSE IF SEARCH(cFilkatalog + cVPIBildeKode) <> ? THEN
      cFilNavn = SEARCH(cFilkatalog + cVPIBildeKode).

  /* Dobbelsjekk */
  IF SEARCH(cFilNavn) = ? THEN
      RETURN.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
      RETURN.

  IF NOT VALID-HANDLE(wLibHandle) THEN DO:
      MESSAGE "Prosedyrebiblioteket er ikke startet!" VIEW-AS ALERT-BOX TITLE "Melding".
      RETURN NO-APPLY.
  end.

  /* Nullstiller error flagg for ocx. */
  chIMAGE-Sko:Picbuf:errornumber = 0.

  /* Legger bilde inn i buffer fra ClipBoard. */
  chIMAGE-Sko:PicBuf:CLEAR(2).
  ASSIGN chIMAGE-Sko:PicBuf:FILENAME = cFilNavn.
  chIMAGE-Sko:PicBuf:LOAD() NO-ERROR.
  IF chIMAGE-Sko:Picbuf:errornumber <> 0 THEN DO:
      /*
      MESSAGE "Feil på bildefil (2)."
          SKIP chIMAGE-Sko:Picbuf:errornumber
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
      */
      RETURN "AVBRYT".
  END.

  FIND Bilderegister WHERE Bilderegister.BildNr = INTEGER(ArtBas.Artikkelnr) NO-ERROR.
  IF AVAIL Bilderegister THEN DO:
      /* Slett Bilderegister + filer på disk */
      IF SEARCH(cBildeKatalog + Bilderegister.FilNavn) <> ? THEN
          OS-DELETE VALUE(cBildeKatalog + Bilderegister.FilNavn).
      IF SEARCH(cBildeKatalog + "mini" + Bilderegister.FilNavn) <> ? THEN
          OS-DELETE VALUE(cBildeKatalog + "mini" + Bilderegister.FilNavn).
      FOR EACH BildeData OF BildeRegister:
          DELETE BildeData.
      END.
      DELETE BildeRegister.
  END.
  BILDENUMMER:
  DO:
      ASSIGN wBildnr = INTEGER(ArtBas.ArtikkelNr).
      CREATE BildeRegister.
      ASSIGN BildeRegister.BildNr = wBildNr
             BildeRegister.FilNavn = STRING(ArtBas.ArtikkelNr) + ".jpg".
      FIND CURRENT BildeRegister NO-LOCK NO-ERROR.
  
  END. /* BILDENUMMER */
 
  /* Tildeler filnavn */
  chIMAGE-Sko:Picbuf:FileName = cBildeKatalog + BildeRegister.FilNavn.
  
  /* Lagrer bilde på hd. ------------------------------------------------ */
  IF chIMAGE-Sko:Picbuf:WriteCompression = 0 THEN  /* Filen skal komprimeres.    */ 
     chIMAGE-Sko:Picbuf:WriteCompression = 65.                                      
  chIMAGE-Sko:Picbuf:Store.                 /* Lagre filen til HD.        */
  IF chIMAGE-Sko:Picbuf:WriteCompression <> 0 THEN /* Filen skal komprimeres.    */ 
     chIMAGE-Sko:Picbuf:WriteCompression = 0.                                       
  /*
  RUN w-forminskStor.p (Bilderegister.Bildnr).   
  chIMAGE-Sko:PicBuf:CLEAR(2).
  ASSIGN chIMAGE-Sko:PicBuf:FILENAME = cBildeKatalog + BildeRegister.FilNavn.
  chIMAGE-Sko:PicBuf:LOAD() NO-ERROR.
  */  
  /* Leser inn filen. */
  ASSIGN wReturnValue = "AVBRYT".
  IF SEARCH(chIMAGE-Sko:Picbuf:FileName) <> ? THEN DO:
      IF VALID-HANDLE(wLibHandle) then
        RUN LesInnBilde in wLibHandle (BildeRegister.BildNr, chIMAGE-Sko:Picbuf:FileName, output wReturnValue).
  END.
  IF wReturnValue = "AVBRYT" THEN DO:
      MESSAGE "Feil ved lasting av bilde " BildeRegister.BildNr
        VIEW-AS ALERT-BOX ERROR TITLE "Feil".
  END.
  ELSE DO:
      FIND bufArtBas EXCLUSIVE-LOCK WHERE
        recid(bufArtBAs) = recid(ArtBas) NO-ERROR.
      IF AVAILABLE bufArtBas THEN DO:
          ASSIGN bufArtBas.BildNr = wBildNr.
          RELEASE bufArtBas.
      END.
      RUN w-Forminsk.w (Bilderegister.Bildnr).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Win 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR wOk AS LOG NO-UNDO.

if not available ArtBas then
    return.
do with frame DEFAULT-FRAME:  
  {visbilde.i
    &BldOcx = "chIMAGE-Sko"
    &BildNr = "ArtBas.BildNr"
/*     &BildNr = "input ArtBas.BildNr" */
  }
end.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

