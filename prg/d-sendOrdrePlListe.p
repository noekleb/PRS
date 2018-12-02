&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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
DEFINE INPUT  PARAMETER cOrdrefil  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iButikkNr  AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER dPlListeId AS DECIMAL   NO-UNDO.

DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailCC  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEmailELoggserver AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMailFrom AS CHAR        NO-UNDO.
DEFINE VARIABLE iLevNr  AS INTEGER NO-UNDO.


/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel Btn_Help ~
E-Meddelande 
&Scoped-Define DISPLAYED-OBJECTS FI-MailTo FI-MailFra FI-Emne FI-Vedlegg ~
E-Meddelande FI-Meddelande 

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

DEFINE VARIABLE E-Meddelande AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 76 BY 6.67 NO-UNDO.

DEFINE VARIABLE FI-Emne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MailFra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail fra" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MailTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail til" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Meddelande AS CHARACTER FORMAT "X(256)":U INITIAL "Meddelande:" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Vedlegg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vedlegg" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.33 COL 97.8
     FI-MailTo AT ROW 1.95 COL 15.6 COLON-ALIGNED
     Btn_Cancel AT ROW 2.57 COL 97.8
     FI-MailFra AT ROW 3.14 COL 15.6 COLON-ALIGNED
     Btn_Help AT ROW 3.86 COL 97.8
     FI-Emne AT ROW 4.33 COL 15.6 COLON-ALIGNED
     FI-Vedlegg AT ROW 5.52 COL 15.6 COLON-ALIGNED
     E-Meddelande AT ROW 6.81 COL 17.6 NO-LABEL
     FI-Meddelande AT ROW 6.71 COL 4.4 NO-LABEL
     RECT-1 AT ROW 1.24 COL 2
     SPACE(17.59) SKIP(0.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Send ordre"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FI-Emne IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MailFra IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MailTo IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Meddelande IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Vedlegg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Send ordre */
DO:
  DEF VAR lMailOK  AS logi      NO-UNDO.
  DEF VAR cMessage AS CHAR      NO-UNDO.   
  DEF VAR crlf     AS CHARACTER NO-UNDO.
    
  crlf = CHR(13) + CHR(10).
  ASSIGN E-Meddelande.
    
  IF CAN-DO('1,J,Y,True,Yes',cEmailELoggserver) THEN 
  DO TRANSACTION:
    FIND CURRENT PlListeHode EXCLUSIVE-LOCK.
    ASSIGN
      PlListeHode.PlMerknad = E-Meddelande.
    FIND CURRENT PlListeHode NO-LOCK.
    
    /* Logger ordren for sending via ELogg server. */
    FIND ELogg WHERE 
         ELogg.TabellNavn     = "plListeHode" AND
         ELogg.EksterntSystem = "MAILSUPORD"    AND
         ELogg.Verdier        = STRING(plListeHode.plListeId) NO-ERROR.
    IF NOT AVAIL Elogg THEN DO:
        CREATE Elogg.
        ASSIGN ELogg.TabellNavn     = "plListeHode"
               ELogg.EksterntSystem = "MAILSUPORD"   
               ELogg.Verdier        = STRING(plListeHode.plListeId).
    END.
    ASSIGN ELogg.EndringsType = 1
           ELogg.Behandlet    = FALSE.
    RELEASE ELogg.
    
    MESSAGE 'Suppleringsordren er lagt klar for sending og vil bli sendt i løpet av kort tid.'
    VIEW-AS ALERT-BOX TITLE 'Ordre klargjort for sending'.
    
  END.
  ELSE DO:

    E-Meddelande = REPLACE(E-Meddelande,CHR(10),crlf).

    FILE-INFO:FILE-NAME = FI-Vedlegg.

    RUN sendmail_tsl.p ("SUPPLORDRE",
                        FI-Emne,
                        FILE-INFO:FULL-PATHNAME,
                        E-Meddelande,                        
                        "",
                        "") NO-ERROR.
    /*
    RUN prssmtpmailv5_7a.p (
        /*mailhub    */   cMailhub,
        /*EmailTo    */   FI-MailTo,
        /*EmailFrom  */   FI-MailFra,
        /*EmailCC    */   cEmailCC,
        /*Attachments*/   ENTRY(NUM-ENTRIES(FI-Vedlegg,"\"),FI-Vedlegg,"\"),
        /*LocalFiles */   FI-Vedlegg,
        /*Subject    */   FI-Emne,
        /*Body       */   E-Meddelande,
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   cDoAUTH,
        /*C_AuthType */   cAuthType,
        /*C_User     */   cUser,
        /*C_Password */   cPassword,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
        IF lMailOk = FALSE THEN DO:
            MESSAGE "Sending avbrutt med melding:" SKIP
                    cMessage 
                    VIEW-AS ALERT-BOX.
        END.
    */
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Send ordre */
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
/*   MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FI-Vedlegg = cOrdrefil.
    {syspara.i 50 50 1 cMailhub }
    {syspara.i 50 50 2 cDoAUTH  }
    {syspara.i 50 50 3 cAuthType}
    {syspara.i 50 50 4 cUser    }
    {syspara.i 50 50 5 cPassword}
    {syspara.i 50 50 20 FI-MailTo}
    {syspar2.i 50 50 20 cEmailCC}
    {syspara.i 50 50 32 cEmailELoggserver}
    {syspara.i 50 50 29 cMailFrom}
    {syspara.i 5 24 3 iLevNr INT}

    cMailFrom = TRIM(cMailFrom).

  FIND PlListeHode NO-LOCK WHERE
    PlListeHode.PlListeId = dPlListeId NO-ERROR.
         
  FIND Butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
  FI-Emne = "Suppl.ordre fra " + STRING(Butiker.Butik) + ' ' + Butiker.butnamn.
  
  FI-MailFra = (IF cMailFrom = '' THEN Butiker.ePostAdresse ELSE cMailFrom).
  IF PlListeHode.LevNr <> iLevNr THEN
  DO:
      FIND LevBas NO-LOCK WHERE
          LevBas.LevNr = PlListeHode.LevNr NO-ERROR.
      IF AVAILABLE LevBas THEN 
          FI-MailTo = (IF TRIM(LevBas.E_MailLev) <> '' THEN TRIM(LevBas.E_MailLev) ELSE FI-MailTo). 
  END.

  IF cDoAUTH = "0" THEN
      ASSIGN cDoAUTH   = "FALSE"
             cAuthType = ""
             cUser     = ""
             cPassword = "".
  ELSE
      cDoAUTH = "TRUE".
  RUN enable_UI.
  APPLY "ENTRY" TO E-Meddelande.
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
  DISPLAY FI-MailTo FI-MailFra FI-Emne FI-Vedlegg E-Meddelande FI-Meddelande 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 Btn_OK Btn_Cancel Btn_Help E-Meddelande 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

