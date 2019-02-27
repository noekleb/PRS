&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wRecid as recid NO-UNDO.
  define var wModus as char  no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid as recid NO-UNDO.
  define input        parameter wModus as char  no-undo.
&ENDIF

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES VarGr HuvGr moms

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame VarGr.Vg VarGr.VgBeskr VarGr.Hg ~
HuvGr.HgBeskr VarGr.MomsKod Moms.MomsProc VarGr.Kost_Proc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame VarGr.VgBeskr VarGr.Hg ~
VarGr.MomsKod VarGr.Kost_Proc 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame VarGr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame VarGr
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH VarGr SHARE-LOCK, ~
      EACH HuvGr OF VarGr SHARE-LOCK, ~
      EACH moms OF VarGr SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame VarGr HuvGr moms
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame VarGr
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame HuvGr
&Scoped-define THIRD-TABLE-IN-QUERY-Dialog-Frame moms


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS VarGr.VgBeskr VarGr.Hg VarGr.MomsKod ~
VarGr.Kost_Proc 
&Scoped-define ENABLED-TABLES VarGr
&Scoped-define FIRST-ENABLED-TABLE VarGr
&Scoped-define DISPLAYED-TABLES VarGr HuvGr Moms
&Scoped-define FIRST-DISPLAYED-TABLE VarGr
&Scoped-define SECOND-DISPLAYED-TABLE HuvGr
&Scoped-define THIRD-DISPLAYED-TABLE Moms
&Scoped-Define ENABLED-OBJECTS BUTTON-SokHg EDITOR-VgKat BUTTON-MerkKat ~
Btn_OK Btn_Cancel Btn_Help BUTTON-SokMoms 
&Scoped-Define DISPLAYED-FIELDS VarGr.Vg VarGr.VgBeskr VarGr.Hg ~
HuvGr.HgBeskr VarGr.MomsKod Moms.MomsProc VarGr.Kost_Proc 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-VgKat FI-Info 

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
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-MerkKat 
     LABEL "Kategorier..." 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SokHg 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-SokMoms 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE EDITOR-VgKat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 31.2 BY 4 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      VarGr, 
      HuvGr, 
      moms SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     VarGr.Vg AT ROW 1.19 COL 18.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     VarGr.VgBeskr AT ROW 2.19 COL 18.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32.6 BY 1
     BUTTON-SokHg AT ROW 3.19 COL 27
     VarGr.Hg AT ROW 3.19 COL 18.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     HuvGr.HgBeskr AT ROW 3.19 COL 29.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     VarGr.MomsKod AT ROW 4.19 COL 18.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.8 BY 1
     Moms.MomsProc AT ROW 4.19 COL 28 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     VarGr.Kost_Proc AT ROW 5.19 COL 18.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     EDITOR-VgKat AT ROW 6.29 COL 20.8 NO-LABEL
     BUTTON-MerkKat AT ROW 6.33 COL 52.8
     FI-Info AT ROW 10.76 COL 1 NO-LABEL
     Btn_OK AT ROW 11.95 COL 1
     Btn_Cancel AT ROW 11.95 COL 16.8
     Btn_Help AT ROW 11.95 COL 52.8
     BUTTON-SokMoms AT ROW 4.19 COL 25.6
     "Kategorier:" VIEW-AS TEXT
          SIZE 10.4 BY .62 AT ROW 6.43 COL 10.2
     SPACE(47.20) SKIP(6.13)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold varegrupperegister"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Info IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN HuvGr.HgBeskr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Moms.MomsProc IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VarGr.Vg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.VarGr,SkoTex.HuvGr OF SkoTex.VarGr,SkoTex.moms OF SkoTex.VarGr"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Vedlikehold varegrupperegister */
DO:
  RUN LagreVg.
  if return-value = "AVBRYT" then
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold varegrupperegister */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-MerkKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-MerkKat Dialog-Frame
ON CHOOSE OF BUTTON-MerkKat IN FRAME Dialog-Frame /* Kategorier... */
DO:
  def var wTagListe as char no-undo.
  def var wLoop     as int  no-undo.
  
  def buffer bufVgKat for VgKat.

  /* Hent eksisterende tagging og bygg opp wTagListe. */
  do for bufVgKat:
    for each bufVgKat no-lock where
      bufVgKat.Vg = VarGr.Vg:
      assign
        wTagListe = wTagListe +
                    (if wTagListe <> ""
                       then ","
                       else "") +
                    string(bufVgKat.VgKat) + ";" + string(bufVgKat.KatNr).
    end.
  end.

  /* Starter tagRutine. */
  run d-tagvgkat.w (input-output wTagListe, VarGr.Vg).

  /* Er Taglisten <> blank, skal den lagres. */
  if wTagListe <> "" then
    LAGRE-TAG:
    do for bufVgKat TRANSACTION:
      do wLoop = 1 to num-entries(wTagListe):
        find bufVgKat exclusive-lock where
          bufVgKat.Vg = VarGr.Vg and
          bufVgKat.VgKat = int(entry(1,entry(wLoop,wTagListe),';')) no-error.
        if not available bufVgKat then
          do:
            find Kategori no-lock where
              Kategori.KatNr = int(entry(2,entry(wLoop,wTagListe),';')) no-error.
            create bufVgKat.
            assign
              bufVgKat.Vg    = VarGr.Vg
              bufVgKat.VgKat = int(entry(1,entry(wLoop,wTagListe),';'))
              bufVgKat.KatNr = if available Kategori
                              then Kategori.KatNr
                              else ?.
            release bufVgKat.
          end.
      end.
    end. /* LAGRE-TAG */
  
  run VisVg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokHg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokHg Dialog-Frame
ON CHOOSE OF BUTTON-SokHg IN FRAME Dialog-Frame /* ... */
or F10 of VarGr.Hg
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = VarGr.Hg
    &Program     = d-bhuvgr.w
    &Frame       = Dialog-Frame
    &PostRun     = "find HuvGr no-lock where
                    recid(HuvGr) = int(return-value) no-error."
    &OptDisp     = "HuvGr.HgBeskr when available HuvGr"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMoms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMoms Dialog-Frame
ON CHOOSE OF BUTTON-SokMoms IN FRAME Dialog-Frame /* ... */
or F10 of VarGr.MomsKod
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = VarGr.MomsKod
    &Program     = d-bmoms.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Moms no-lock where
                    recid(Moms) = int(return-value) no-error."
    &OptDisp     = "Moms.MomsProc    when available Moms"
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME EDITOR-VgKat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDITOR-VgKat Dialog-Frame
ON ANY-PRINTABLE OF EDITOR-VgKat IN FRAME Dialog-Frame
{noedit.i}
DO:
  return no-apply.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VarGr.Hg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VarGr.Hg Dialog-Frame
ON TAB OF VarGr.Hg IN FRAME Dialog-Frame /* Hovedgruppe */
or "return":U of VarGr.Hg
DO:
  find HuvGr no-lock where
    HuvGr.Hg = input VarGr.Hg no-error.
  if available HuvGr then
    display HuvGr.HgBeskr with frame Dialog-Frame.
  else
    display " " @ HuvGr.HgBeskr with frame Dialog-Frame.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VarGr.MomsKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VarGr.MomsKod Dialog-Frame
ON TAB OF VarGr.MomsKod IN FRAME Dialog-Frame /* Momskode */
or "RETURN":U of VarGr.MomsKod
DO:
  find Moms no-lock where
    Moms.MomsKod = input VarGr.MomsKod no-error.
  if available Moms then
    display Moms.MomsProc with frame Dialog-Frame.
  else
    display " " @ Moms.MomsProc with frame Dialog-Frame.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

find VarGr no-lock where
  recid(VarGr) = wRecid no-error.
if available VarGr then
  do:
    find HuvGr of VarGr no-lock no-error.
    find Moms  of VarGr no-lock no-error.
  end.
    
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i} RUN enable_UI.

  run VisVg.
  
  if wModus = "Ny" then
    assign
      VarGr.Vg:sensitive = true.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
return wRetur-Verdi.

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
  DISPLAY EDITOR-VgKat FI-Info 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE HuvGr THEN 
    DISPLAY HuvGr.HgBeskr 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Moms THEN 
    DISPLAY Moms.MomsProc 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE VarGr THEN 
    DISPLAY VarGr.Vg VarGr.VgBeskr VarGr.Hg VarGr.MomsKod VarGr.Kost_Proc 
      WITH FRAME Dialog-Frame.
  ENABLE VarGr.VgBeskr BUTTON-SokHg VarGr.Hg VarGr.MomsKod VarGr.Kost_Proc 
         EDITOR-VgKat BUTTON-MerkKat Btn_OK Btn_Cancel Btn_Help BUTTON-SokMoms 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreVg Dialog-Frame 
PROCEDURE LagreVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wHgEndret as log no-undo.

do with frame Dialog-Frame transaction:
  find HuvGr no-lock where
    HuvGr.Hg = input VarGr.Hg no-error.
  if not available HuvGr then
    do:
      message "Ukjent hovedgruppe:" input VarGr.Hg
      view-as alert-box title "Lagringsfeil".        
      return no-apply "AVBRYT".
    end.

  find Moms no-lock where
    Moms.MomsKod = input VarGr.MomsKod no-error.
  if not available Moms then
    do:
      message "Ukjent momskode:" input VarGr.MomsKod
      view-as alert-box title "Lagringsfeil".        
      return no-apply "AVBRYT".
    end.

  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      if can-find(VarGr where
                  VarGr.Vg = int(VarGr.Vg:screen-value)) then
        do:
          message "Varegruppe finnes allerede med gruppenr:" VarGr.Vg:screen-value
          view-as alert-box title "Lagringsfeil".        
          return no-apply "AVBRYT".
        end.
                  
      if int(VarGr.Vg:screen-value) = 0 then
        do:
          message "VarGrNr må være større enn 0"
          view-as alert-box title "Lagringsfeil".
          return no-apply "AVBRYT".
        end.
        
      create VarGr.
      assign 
        VarGr.Vg = int(VarGr.Vg:screen-value)
        wRecid   = recid(VarGr).
    end.
  else 
    find VarGr Exclusive-lock where
      recid(VarGr) = wRecid no-error.
      
  /* Sjekker om HG er endret på varegruppen */
  if input VarGr.Hg <> VarGr.Hg then
    wHgEndret = true.
  else
    wHgEndret = false.
      
  assign
    VarGr.VgBeskr
    VarGr.MomsKod
    VarGr.Hg
    VarGr.Kost_Proc
    wRetur-Verdi = "OK".
    
  /* Oppdaterer alle artikkler på varegruppen hvs HG er endret. */
  if wHgEndret then
    do:
      FI-Info = "".
      for each ArtBas exclusive-lock where
        ArtBas.Vg = VarGr.Vg:
    
        assign
          ArtBas.Hg = VarGr.Hg.
    
        /* Melding til bruker */
        FI-Info = "Setter ny hovedgruppe på artikkel " + string(ArtBas.Vg) + "/" + 
                  (if ArtBas.LopNr = ?
                     then "?"
                     else string(ArtBas.LopNr)).
        display FI-Info with frame Dialog-Frame.
    
      end.
    end.
    
end. /* TRANSACTION */    

FI-Info = "".
display FI-Info with frame Dialog-Frame.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisVg Dialog-Frame 
PROCEDURE VisVg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  assign 
    EDITOR-VgKat = "".
  for each VgKat of VarGr no-lock,
      each Kategori of VgKat no-lock:
    assign 
      EDITOR-VgKat =  EDITOR-VgKat + string(VgKat.VgKat,"z9") + ": " + 
                      Kategori.Beskrivelse + chr(13).
  end.
  display 
    EDITOR-VgKat
  with frame Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

