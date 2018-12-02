&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR iStrTypeId  AS INT NO-UNDO.
  DEF VAR iAvdelingNr AS INT NO-UNDO.
  DEF VAR iHg         AS INT NO-UNDO.

  ASSIGN
      iStrTypeId   = ?
      iAvdelingNr  = 2
      iHg          = 0
      .
&ELSE
  DEF INPUT-OUTPUT PARAMETER iStrTypeId  AS INT NO-UNDO.
  DEF INPUT        PARAMETER iAvdelingNR AS INT NO-UNDO.
  DEF INPUT        PARAMETER iHg         AS INT NO-UNDO.
&ENDIF

DEFINE VARIABLE cAlle   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOk     AS CHAR       INITIAL "AVBRYT" NO-UNDO.
DEFINE VARIABLE lNy     AS LOG        INITIAL FALSE NO-UNDO.
DEFINE VARIABLE cStorl  AS CHAR       NO-UNDO.
DEFINE VARIABLE cHalvNr AS CHAR       NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Storl

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StrTStr StrType

/* Definitions for BROWSE BROWSE-Storl                                  */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Storl StrTStr.SeqNr StrTStr.SoStorl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Storl 
&Scoped-define QUERY-STRING-BROWSE-Storl FOR EACH StrTStr OF StrType NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Storl OPEN QUERY BROWSE-Storl FOR EACH StrTStr OF StrType NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Storl StrTStr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Storl StrTStr


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame StrType.AvdelingNr StrType.Hg ~
StrType.StrTypeID StrType.KortNavn StrType.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame StrType.KortNavn ~
StrType.Beskrivelse 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame StrType
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame StrType
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Storl}
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH StrType SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH StrType SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame StrType
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame StrType


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS StrType.KortNavn StrType.Beskrivelse 
&Scoped-define ENABLED-TABLES StrType
&Scoped-define FIRST-ENABLED-TABLE StrType
&Scoped-Define ENABLED-OBJECTS RECT-57 RECT-58 FI-Storl B-Insert ~
BROWSE-Storl B-Tag Btn_OK Btn_Cancel Btn_Help B-Ny B-Slett 
&Scoped-Define DISPLAYED-FIELDS StrType.AvdelingNr StrType.Hg ~
StrType.StrTypeID StrType.KortNavn StrType.Beskrivelse 
&Scoped-define DISPLAYED-TABLES StrType
&Scoped-define FIRST-DISPLAYED-TABLE StrType
&Scoped-Define DISPLAYED-OBJECTS FI-Storl 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Insert  NO-FOCUS
     LABEL "Sett inn over" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-Ny  NO-FOCUS
     LABEL "Ny" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-Slett  NO-FOCUS
     LABEL "Slett" 
     SIZE 18 BY 1.14.

DEFINE BUTTON B-Tag 
     LABEL "Velg størrelser" 
     SIZE 18 BY 1.14.

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

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(256)":U 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65.8 BY 5.95.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65.8 BY 12.86.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Storl FOR 
      StrTStr SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      StrType SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Storl Dialog-Frame _STRUCTURED
  QUERY BROWSE-Storl NO-LOCK DISPLAY
      StrTStr.SeqNr FORMAT ">9":U
      StrTStr.SoStorl FORMAT "x(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 20 BY 11.43 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     StrType.AvdelingNr AT ROW 1.52 COL 16 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 45 BY 1
     StrType.Hg AT ROW 2.52 COL 16 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 45 BY 1
     StrType.StrTypeID AT ROW 3.52 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     StrType.KortNavn AT ROW 4.52 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     StrType.Beskrivelse AT ROW 5.52 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     FI-Storl AT ROW 7.1 COL 16 COLON-ALIGNED
     B-Insert AT ROW 9.33 COL 39
     BROWSE-Storl AT ROW 8.19 COL 18
     B-Tag AT ROW 10.52 COL 39
     Btn_OK AT ROW 20.05 COL 1
     Btn_Cancel AT ROW 20.05 COL 17
     Btn_Help AT ROW 20.05 COL 51.6
     B-Ny AT ROW 8.14 COL 39
     B-Slett AT ROW 18.38 COL 39
     RECT-57 AT ROW 1 COL 1
     RECT-58 AT ROW 6.95 COL 1
     SPACE(0.00) SKIP(1.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrering av størrelsestype"
         CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
/* BROWSE-TAB BROWSE-Storl B-Insert Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX StrType.AvdelingNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX StrType.Hg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StrType.StrTypeID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Storl
/* Query rebuild information for BROWSE BROWSE-Storl
     _TblList          = "skotex.StrTStr OF skotex.StrType"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = skotex.StrTStr.SeqNr
     _FldNameList[2]   = skotex.StrTStr.SoStorl
     _Query            is OPENED
*/  /* BROWSE BROWSE-Storl */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.StrType"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Registrering av størrelsestype */
DO:
    ASSIGN
        cOk = "OK"
        .
    RUN lagrePost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrering av størrelsestype */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Insert
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Insert Dialog-Frame
ON CHOOSE OF B-Insert IN FRAME Dialog-Frame /* Sett inn over */
DO:
    RUN nyStorrelse (1).
    APPLY "ENTRY" TO FI-Storl.  
    RETURN NO-APPLY.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Ny Dialog-Frame
ON CHOOSE OF B-Ny IN FRAME Dialog-Frame /* Ny */
DO:

  RUN nyStorrelse (0).
  APPLY "ENTRY" TO FI-Storl.  
  RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett Dialog-Frame
ON CHOOSE OF B-Slett IN FRAME Dialog-Frame /* Slett */
DO:
  IF AVAILABLE STrTStr THEN
  DO TRANSACTION:
      FIND CURRENT StrTStr EXCLUSIVE-LOCK.
      DELETE StrTstr.
      BROWSE-Storl:DELETE-CURRENT-ROW( ).
      APPLY "ENTRY" TO FI-Storl.  
      RETURN NO-APPLY.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tag Dialog-Frame
ON CHOOSE OF B-Tag IN FRAME Dialog-Frame /* Velg størrelser */
DO:
    DEFINE VARIABLE cStrListe AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount    AS INTEGER    NO-UNDO.
    DEFINE VARIABLE prRowId   AS ROWID      NO-UNDO.

    FOR EACH StrTStr NO-LOCK WHERE
        StrTStr.STrTypeId = iStrTypeId
        BY StrTStr.StrTypeId
        BY StrTStr.SeqNr:

        ASSIGN
            cStrListe = cStrListe + 
                        (IF cStrListe = ""
                           THEN ""
                           ELSE ",") + 
                        StrTStr.SoStorl
            .
    END.

    RUN d-tagstrkonv.w (INPUT iStrTypeID,INPUT StrType.Beskrivelse,OUTPUT cStrListe).
    IF cStrListe <> "" THEN 
    DO:
      run LagreStorrelser (cStrListe).
      FIND FIRST StrTStr NO-LOCK WHERE
          StrTStr.StrTypeId = iStrTypeId NO-ERROR.
      prRowId = ROWID(StrTStr).
      {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}  
      BROWSE-Storl:set-repositioned-row(BROWSE-Storl:NUM-ITERATIONS,'conditional').
      BROWSE-Storl:QUERY:REPOSITION-TO-ROWID(prRowId).
    END.
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
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN
      cOk = "OK"
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl Dialog-Frame
ON RETURN OF FI-Storl IN FRAME Dialog-Frame /* Størrelse */
OR "TAB" OF FI-Storl
DO:
  IF KEYFUNCTION(LASTKEY) = " " THEN
      RETURN NO-APPLY.

  RUN nyStorrelse(0).
  APPLY "ENTRY" TO FI-Storl.  
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Storl
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{dproclibstart.i}

ASSIGN
    lNy = iStrTypeId = ?
    .

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  {syspara.i 1 1 16 cHalvNr}
  {syspara.i 1 100 1 cAlle}
  IF cAlle = "" THEN
      ASSIGN cAlle = "[Alle]".
  RUN initCbAvdeling.
 
  IF iStrTypeId = ? THEN
      RUN opprettStrTypeId.
  FIND StrType NO-LOCK WHERE
            StrType.StrTypeId = iStrTypeId NO-ERROR.
  IF NOT AVAILABLE StrType THEN
  DO:
      MESSAGE "Ukjent eller ikke angitt størrelsestype" iStrTypeId ". Eller ingen flere ledig i intervallet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  FIND StrType NO-LOCK WHERE
      StrType.StrTypeId = iStrTypeId NO-ERROR.
  IF AVAILABLE StrType THEN 
  DO:
      DISPLAY StrType.AvdelingNr 
            StrType.Beskrivelse 
        WITH FRAME Dialog-Frame.
      RUN initCBHovedGr.
      DISPLAY
          StrType.Hg StrType.StrTypeID StrType.KortNavn 
          WITH FRAME Dialog-Frame.
      ASSIGN 
      StrType.Hg:SCREEN-VALUE = string(StrType.Hg)
      .
  END.
  {&OPEN-QUERY-Dialog-Frame}
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* Rydder bort ved avbryt av nyregistrering */
IF cOk = "AVBRYT" AND lNy THEN
    RUN slettStrType.
RETURN cOk.

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
  DISPLAY FI-Storl 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE StrType THEN 
    DISPLAY StrType.AvdelingNr StrType.Hg StrType.StrTypeID StrType.KortNavn 
          StrType.Beskrivelse 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-57 RECT-58 StrType.KortNavn StrType.Beskrivelse FI-Storl B-Insert 
         BROWSE-Storl B-Tag Btn_OK Btn_Cancel Btn_Help B-Ny B-Slett 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Dialog-Frame 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  assign
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 or
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  /* Sjekker om det er benyttet gyldige tegn i halvnummer. */
  /* Er det ikke det, tas halvnummeret bort.               */
  /*
  if NUM-ENTRIES(wStorl,".") = 2 then
    DO:
      if NOT CAN-DO(wDecimaler,ENTRY(2,wStorl,".")) then
        wStorl = ENTRY(1,wStorl,".").
    END.
  */
  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCbAvdeling Dialog-Frame 
PROCEDURE initCbAvdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN StrType.AvdelingNr:LIST-ITEM-PAIRS = cAlle + ",0"
             StrType.AvdelingNr:screen-value = "0".
      for each Avdeling no-lock:
        StrType.AvdelingNr:add-last(string(Avdeling.AvdelingNr,"zz9") + "/" + 
                           REPLACE(Avdeling.AvdelingNavn,","," "),(Avdeling.AvdelingNr)).
      end.
      ASSIGN 
          StrType.Hg:LIST-ITEM-PAIRS = cAlle + ",0"
          StrType.Hg:screen-value = "0"
          StrType.Hg:SENSITIVE = FALSE
          .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initCBHovedGr Dialog-Frame 
PROCEDURE initCBHovedGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          pcTekst = cAlle + ",0"
          .
      IF StrType.AvdelingNr:SCREEN-VALUE = '0' THEN
      DO:
          ASSIGN
              StrType.Hg:LIST-ITEM-PAIRS = pcTekst
              StrType.Hg:SCREEN-VALUE    = entry(2,pcTekst)
              .
      END.
      ELSE DO:
          FOR EACH HuvGr NO-LOCK WHERE
              HuvGr.AvdelingNr = int(StrType.AvdelingNr:SCREEN-VALUE):

              ASSIGN
                  pcTekst = pcTekst + 
                            (IF pcTekst = "" 
                                THEN ""
                                ELSE ",") + 
                              STRING(HuvGr.Hg) + "/" + HuvGr.HgBeskr + "," +
                              STRING(HuvGr.Hg)
                  .
          END.
          ASSIGN
              StrType.Hg:LIST-ITEM-PAIRS = pcTekst
              StrType.Hg:SCREEN-VALUE    = entry(2,pcTekst)
              .
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagrePost Dialog-Frame 
PROCEDURE lagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME} TRANSACTION:
      FIND StrType EXCLUSIVE-LOCK WHERE
          StrType.StrTypeId = iStrTypeId.
      ASSIGN
          StrType.KortNavn
          StrType.Beskrivelse
          StrType.AvdelingNr
          StrType.Hg
          .
      RUN settStrTypeFelt.p (StrType.StrTypeID).
  END.
  FIND CURRENT StrType NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lagreStorrelser Dialog-Frame 
PROCEDURE lagreStorrelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pcListe AS CHAR NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  do:
    /* Først kaster vi alle de gamle størrelsene */
    for each StrTStr WHERE StrTStr.StrTypeId = iStrTypeId exclusive-lock:
      delete StrTStr.
    end.

    /* Så oppretter vi nye poster for alle de nye */
    do piLoop = 1 to num-entries(pcListe,CHR(1)) with frame Dialog-Frame:
    
      if entry(piLoop,pcListe,CHR(1)) <> "" then
        do:
          create StrTStr.
          assign
              StrTStr.StrTypeId = iStrTypeID
              StrTSTr.SeqNr     = piLoop
              StrTStr.SoStorl   = entry(piLoop,pcListe,CHR(1))
              .
        end.
    end.
    
    RUN settStrTypeFelt.p (iStrTypeID).
  end.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyStorrelse Dialog-Frame 
PROCEDURE nyStorrelse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       piInsert = 0 - Ny størrelse på sløutten av browser.
                        = 1 - Legges inn over valgt størrelse.
  
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piInsert AS INT NO-UNDO.

  DEF BUFFER bStrTStr FOR StrTStr.

  DEF VAR piSeqNr   AS INT   NO-UNDO.
  DEF VAR prRowId   AS ROWID NO-UNDO.
  DEF VAR plSvar    AS LOG   NO-UNDO.
  DEF VAR piStrKode AS INT   NO-UNDO.

  DO WITH FRAME {&FRAME-NAME} TRANSACTION:
      
      ASSIGN
          FI-Storl
          piSeqNr  = IF AVAILABLE STrTstr
                       THEN StrTStr.SEqNr
                       ELSE ?
          .
      IF FI-Storl = "" THEN
      DO:
          MESSAGE "Størrelse må angis"
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      RUN FixStorl(INPUT-OUTPUT FI-Storl).

      /* Kontroll av halvnummer */
      IF NUM-ENTRIES(FI-Storl,".") > 1 THEN
      DO:
          IF NOT CAN-DO(cHalvNr,ENTRY(2,FI-STorl,".")) THEN
          DO:
              MESSAGE "Ugyldig angivelse av halvnummer." SKIP
                      "Kun " + cHalvNr + " kan benyttes."
                  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
              RETURN NO-APPLY.
          END.
      END.
      /* Kontroll mot StrKonv. */
      IF NOT CAN-FIND(StrKonv WHERE
                      StrKonv.Storl = FI-Storl) THEN
      DO:
          plSvar = FALSE.
          MESSAGE "Størrelsen (" + FI-Storl + ") er ikke angitt i størrelsesregisteret." SKIP
                  "Skal ny størrelse legges opp?"
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Ny størrelse"
              UPDATE plSvar.
          IF plSvar <> TRUE THEN
              RETURN NO-APPLY.
          ELSE DO:
              FIND LAST StrKonv NO-LOCK USE-INDEX StrKode.
              IF AVAILABLE StrKonv THEN
                  piStrKode = StrKonv.StrKode + 1.
              ELSE
                  piStrKode = 1.
              CREATE StrKonv.
              ASSIGN
                  StrKonv.StrKode = piStrKode
                  StrKonv.Storl   = FI-Storl
                  StrKonv.Merknad = ""
                  .
              FIND CURRENT StrKonv NO-LOCK.
          END.

      END.

      fi-Storl:SCREEN-VALUE = FI-Storl.

      IF CAN-FIND(FIRST StrTStr WHERE 
                  StrTStr.StrTypeId = iStrTypeId AND
                  StrTStr.SoStorl   = FI-Storl USE-INDEX Storl) THEN
      DO:
          MESSAGE "Størrelsen er allerede registrert for størrelsestypen."
              VIEW-AS ALERT-BOX WARNING BUTTONS OK.
          RETURN NO-APPLY.
      END.
      ELSE DO:
          /* Oppretter ny størrelse på slutten av browser. */
          IF piInsert = 0 THEN
          DO:
              FIND LAST bStrTStr NO-LOCK WHERE
                  bStrTSTr.StrTypeId = iStrTypeId USE-INDEX StrTStr NO-ERROR.
              IF AVAILABLE bStrTStr THEN 
                  piSeqNr = bStrTSTr.SeqNr + 1.
              ELSE 
                  piSeqNr = 1.
          END.
          /* Setter inn ny størrelse over valgt størrelse. */
          ELSE DO:
              IF piSeqNr = ? THEN
              DO:
                  FIND LAST bStrTStr NO-LOCK WHERE
                      bStrTSTr.StrTypeId = iStrTypeId USE-INDEX StrTStr NO-ERROR.
                  IF AVAILABLE bStrTStr THEN 
                      piSeqNr = bStrTSTr.SeqNr + 1.
                  ELSE 
                      piSeqNr = 1.
              END.
              ELSE DO:
                  INSERT-RAD:
                  FOR EACH bStrTStr EXCLUSIVE-LOCK WHERE
                      bStrTStr.STrTypeId = iStrTypeId
                      BREAK BY bStrTStr.StrTypeId 
                            BY bStrTStr.SeqNr DESCENDING:

                      IF bStrTSTr.SeqNr < piSeqNr THEN
                          LEAVE INSERT-RAD.

                      ASSIGN
                          bStrTSTr.SeqNr = bSTrTStr.SeqNr + 1
                          .
                  END. /* INSERT-RAD */
              END.
          END.

          CREATE StrTStr.
          ASSIGN
              StrTSTr.StrTypeId = iStrTypeId
              StrTStr.SeqNr     = piSeqNr
              StrTSTr.SoStorl   = FI-Storl
              prRowId           = ROWID(StrTStr)
              .
          FIND CURRENT StrTStr NO-LOCK.
          {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}  
          BROWSE-Storl:set-repositioned-row(BROWSE-Storl:NUM-ITERATIONS,'conditional').
          BROWSE-Storl:QUERY:REPOSITION-TO-ROWID(prRowId).

      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettStrTypeId Dialog-Frame 
PROCEDURE opprettStrTypeId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO TRANSACTION:
      FIND LAST StrType NO-LOCK WHERE
          StrTypeId >= int(STRING(iAvdelingNr) + "00") AND
          StrTypeId <= int(STRING(iAvdelingNr) + "99") NO-ERROR.
      IF NOT AVAILABLE StrType THEN
          iStrTypeId = int(STRING(iAvdelingNr) + "00") + 1.
      ELSE
          iStrTypeId = StrType.StrTypeId + 1.

      IF iStrTypeId > int(STRING(iAvdelingNr) + "99") THEN
      DO:
          iStrTypeId = ?.
          RETURN.
      END.

      CREATE StrType.
      ASSIGN
          StrType.StrTypeId  = iStrTypeId
          StrType.AvdelingNr = iAvdelingNr
          StrType.Hg         = iHg
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slettStrType Dialog-Frame 
PROCEDURE slettStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

SLETT:    
DO TRANSACTION:
    FIND StrType WHERE
        StrType.StrTypeId = iStrTypeId NO-ERROR.
    IF NOT AVAILABLE StrType THEN
        LEAVE SLETT.

    FOR EACH StrTStr OF StrType:
        DELETE StrTStr.
    END.
    DELETE StrType.
END. /* SLETT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

