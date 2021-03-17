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
DEFINE VARIABLE cOrdrefil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK FI-MailTo Btn_Cancel Btn_Help ~
E-Meddelande 
&Scoped-Define DISPLAYED-OBJECTS FI-MailTo FI-MailTO20 FI-MailFra ~
FI-MailCC-20 FI-Emne FI-Mailhub FI-Vedlegg FI-DoAUTH E-Meddelande ~
FI-AuthType FI-User FI-Password FI-Meddelande 

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
     SIZE 44.4 BY 6.67 NO-UNDO.

DEFINE VARIABLE FI-AuthType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Authtype" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-DoAUTH AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auth" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Emne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Emne" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MailCC-20 AS CHARACTER FORMAT "X(256)":U 
     LABEL "MailCC_para_2_20" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MailFra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail fra" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Mailhub AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mailhub" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MailTo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mail til" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MailTO20 AS CHARACTER FORMAT "X(256)":U 
     LABEL "MailTo_para20" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Meddelande AS CHARACTER FORMAT "X(256)":U INITIAL "Meddelande:" 
      VIEW-AS TEXT 
     SIZE 13 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Password AS CHARACTER FORMAT "X(256)":U 
     LABEL "Password" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-User AS CHARACTER FORMAT "X(256)":U 
     LABEL "User" 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vedlegg AS CHARACTER FORMAT "X(256)":U 
     LABEL "Vedlegg" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 111 BY 12.86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.33 COL 115.2
     FI-MailTo AT ROW 1.95 COL 15.6 COLON-ALIGNED
     FI-MailTO20 AT ROW 1.95 COL 76.4 COLON-ALIGNED
     Btn_Cancel AT ROW 2.57 COL 115.2
     FI-MailFra AT ROW 3.14 COL 15.6 COLON-ALIGNED
     FI-MailCC-20 AT ROW 3.14 COL 76.4 COLON-ALIGNED
     Btn_Help AT ROW 3.86 COL 115.2
     FI-Emne AT ROW 4.33 COL 15.6 COLON-ALIGNED
     FI-Mailhub AT ROW 4.43 COL 76.4 COLON-ALIGNED
     FI-Vedlegg AT ROW 5.52 COL 15.6 COLON-ALIGNED
     FI-DoAUTH AT ROW 5.62 COL 76.4 COLON-ALIGNED
     E-Meddelande AT ROW 6.81 COL 17.6 NO-LABEL
     FI-AuthType AT ROW 6.81 COL 76.4 COLON-ALIGNED
     FI-User AT ROW 8 COL 76.4 COLON-ALIGNED
     FI-Password AT ROW 9.29 COL 76.4 COLON-ALIGNED
     FI-Meddelande AT ROW 6.86 COL 4.4 NO-LABEL
     RECT-1 AT ROW 1.24 COL 2
     SPACE(18.19) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Test send ordre"
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

/* SETTINGS FOR FILL-IN FI-AuthType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-DoAUTH IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Emne IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MailCC-20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MailFra IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Mailhub IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-MailTO20 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Meddelande IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Password IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-User IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Vedlegg IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Test send ordre */
DO:
    DEF VAR lMailOK AS logi NO-UNDO.
    DEF VAR cMessage AS CHAR NO-UNDO.
    DEFINE VARIABLE crlf AS CHARACTER   NO-UNDO.

    crlf = CHR(13) + CHR(10).
    ASSIGN E-Meddelande.
    E-Meddelande = REPLACE(E-Meddelande,CHR(10),crlf).

  RUN prssmtpmailv5_7a.p (
        /*mailhub    */   FI-Mailhub,
        /*EmailTo    */   FI-MailTo,
        /*EmailFrom  */   FI-MailFra,
        /*EmailCC    */   "",
        /*Attachments*/   "", /* ENTRY(NUM-ENTRIES(FI-Vedlegg,"\"),FI-Vedlegg,"\"), */
        /*LocalFiles */   "", /* FI-Vedlegg, */
        /*Subject    */   FI-Emne,
        /*Body       */   E-Meddelande,
        /*MIMEHeader */   "CharSet=iso8859-1",
        /*BodyType   */   "",
        /*Importance */   0,
        /*L_DoAUTH   */   FI-DoAUTH,
        /*C_AuthType */   FI-AuthType,
        /*C_User     */   FI-User,
        /*C_Password */   FI-Password,
        /*oSuccessful*/  OUTPUT lMailOK,
        /*vMessage   */  OUTPUT cMessage) NO-ERROR.
        IF lMailOk = FALSE THEN DO:
            MESSAGE "Sending avbrutt med melding:" SKIP
                    cMessage 
                    VIEW-AS ALERT-BOX.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Test send ordre */
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN FI-MailTO.
  IF FI-MailTO = "" THEN DO:
      MESSAGE "Lägg in 'Mail til'"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
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
    
    IF SEARCH('tnc.txt') <> ? THEN 
    DO:  
      /*
          c_p1_from      = 'info@polygon.se'                 
          c_p3_hub       = 'smtp.office365.com:587'
          c_p4_usr       = 'info@polygon.se'
          c_p5_pwd       = 'Uddeva11a'  
      */
        {syspara.i 50 50 1 FI-Mailhub }
        {syspara.i 50 50 2 FI-DoAUTH  }
        {syspara.i 50 50 3 FI-AuthType}
        {syspara.i 50 50 4 FI-User    }
        {syspara.i 50 50 5 FI-Password}
        {syspara.i 50 50 20 FI-MailTO20}
        {syspar2.i 50 50 20 FI-MailCC-20}
        {syspara.i 5 1 1 iButikkNr INT}
        
        ASSIGN 
            FI-User     = 'info@polygon.se'
            FI-MailTO20 = 'tomn@nsoft.no'
            FI-Mailhub  = 'smtp.office365.com:587'
            FI-Password = 'Uddeva11a'
            .
    END.
    ELSE DO:
        {syspara.i 50 50 1 FI-Mailhub }
        {syspara.i 50 50 2 FI-DoAUTH  }
        {syspara.i 50 50 3 FI-AuthType}
        {syspara.i 50 50 4 FI-User    }
        {syspara.i 50 50 5 FI-Password}
        {syspara.i 50 50 20 FI-MailTO20}
        {syspar2.i 50 50 20 FI-MailCC-20}
        {syspara.i 5 1 1 iButikkNr INT}
    END.

  FIND Butiker WHERE butiker.butik = iButikkNr NO-LOCK NO-ERROR.
  FI-Emne = "Testmail fra " + Butiker.butnamn.
  /*FI-MailFra = Butiker.ePostAdresse.*/
  FI-MailFra = FI-User.
  
  IF FI-DoAUTH = "0" THEN
      ASSIGN FI-DoAUTH   = "FALSE"
             FI-AuthType = ""
             FI-User     = ""
             FI-Password = "".
  ELSE
      FI-DoAUTH = "TRUE".


  RUN enable_UI.
  APPLY "ENTRY" TO FI-MailTO.
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
  DISPLAY FI-MailTo FI-MailTO20 FI-MailFra FI-MailCC-20 FI-Emne FI-Mailhub 
          FI-Vedlegg FI-DoAUTH E-Meddelande FI-AuthType FI-User FI-Password 
          FI-Meddelande 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 Btn_OK FI-MailTo Btn_Cancel Btn_Help E-Meddelande 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

