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
/* Parameters Definitions ---                                           */
  DEFINE VARIABLE cSubject      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTo           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cVedleggListe AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lVisaDialog   AS LOGICAL   NO-UNDO.
/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Send CB-TO FI-Amne E-message SEL-Vedlegg ~
Btn_OK Btn_Cancel Btn_Help B-Vedlegg B-Slett FILL-IN-1 
&Scoped-Define DISPLAYED-OBJECTS CB-TO FI-Amne E-message SEL-Vedlegg ~
FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Send 
     LABEL "Sänd" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Slett 
     IMAGE-UP FILE "icon/e-del.bmp":U
     LABEL "Slett" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON B-Vedlegg 
     IMAGE-UP FILE "icon\e-vedlegg.bmp":U
     LABEL "Vedlegg" 
     SIZE 4.6 BY 1.05.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-TO AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Tom","tomn@polygonsoftware.no",
                     "Ken1","ken1@polygonsoftware.no",
                     "kjellkenneth","kjellkenneth@hotmail.com",
                     "Felaktig","felaktig"
     DROP-DOWN-LIST
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE E-message AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 87 BY 10.57 NO-UNDO.

DEFINE VARIABLE FI-Amne AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ämne" 
     VIEW-AS FILL-IN 
     SIZE 53 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "Skriv ett medelande" 
      VIEW-AS TEXT 
     SIZE 40 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE SEL-Vedlegg AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 58 BY 8.57 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-Send AT ROW 6.48 COL 106
     CB-TO AT ROW 3.86 COL 10 COLON-ALIGNED NO-LABEL
     FI-Amne AT ROW 2.67 COL 10 COLON-ALIGNED
     E-message AT ROW 6.38 COL 11 NO-LABEL
     SEL-Vedlegg AT ROW 17.19 COL 11.2 NO-LABEL
     Btn_OK AT ROW 1.24 COL 107
     Btn_Cancel AT ROW 2.48 COL 107
     Btn_Help AT ROW 4.48 COL 107
     B-Vedlegg AT ROW 17.19 COL 71 NO-TAB-STOP 
     B-Slett AT ROW 18.38 COL 71 NO-TAB-STOP 
     FILL-IN-1 AT ROW 5.52 COL 9.4 COLON-ALIGNED NO-LABEL
     SPACE(72.19) SKIP(20.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Mail"
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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Mail */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Send
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Send Dialog-Frame
ON CHOOSE OF B-Send IN FRAME Dialog-Frame /* Sänd */
DO:
    ASSIGN cSubject      = FI-Amne:SCREEN-VALUE
           cTo           = CB-TO:SCREEN-VALUE
           cMessage      = E-Message:SCREEN-VALUE
           cVedleggListe = SEL-Vedlegg:LIST-ITEMS.
    RUN sendmail.p(cSubject,cTo,cMessage,cVedleggListe).
    IF RETURN-VALUE <> "" THEN DO:
        MESSAGE RETURN-VALUE
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett Dialog-Frame
ON CHOOSE OF B-Slett IN FRAME Dialog-Frame /* Slett */
DO:
    DEFINE VARIABLE iIdx AS INTEGER    NO-UNDO.
    IF SEL-Vedlegg:LIST-ITEMS = "" THEN
        RETURN.
    iIdx = LOOKUP(SEL-Vedlegg:SCREEN-VALUE,SEL-Vedlegg:LIST-ITEMS).
    SEL-Vedlegg:DELETE(SEL-Vedlegg:SCREEN-VALUE).
    IF SEL-Vedlegg:LIST-ITEMS = "" THEN
        RETURN.
    ASSIGN SEL-Vedlegg:SCREEN-VALUE = 
        ENTRY(IF NUM-ENTRIES(SEL-Vedlegg:LIST-ITEMS) >= iIdx THEN iIdx ELSE iIdx - 1,SEL-Vedlegg:LIST-ITEMS).
    APPLY "ENTRY" TO SEL-Vedlegg.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Vedlegg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Vedlegg Dialog-Frame
ON CHOOSE OF B-Vedlegg IN FRAME Dialog-Frame /* Vedlegg */
DO:
    DEFINE VARIABLE cVedleggFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE OKpressed   AS LOGICAL    NO-UNDO.
    SYSTEM-DIALOG GET-FILE cVedleggFil
        TITLE      "Välj fil att bifoga"
        MUST-EXIST
        UPDATE OKpressed.
    IF OKpressed = TRUE THEN DO:
        IF CAN-DO(SEL-Vedlegg:LIST-ITEMS,cVedleggFil) THEN
            MESSAGE "Redan vald !"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            SEL-Vedlegg:ADD-LAST(cVedleggFil).
            APPLY "ENTRY" TO SEL-Vedlegg.
            SEL-Vedlegg:SCREEN-VALUE = cVedleggFil.
        END.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   CB-TO:SCREEN-VALUE     = ENTRY(2,CB-TO:LIST-ITEM-PAIRS).
  
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
  DISPLAY CB-TO FI-Amne E-message SEL-Vedlegg FILL-IN-1 
      WITH FRAME Dialog-Frame.
  ENABLE B-Send CB-TO FI-Amne E-message SEL-Vedlegg Btn_OK Btn_Cancel Btn_Help 
         B-Vedlegg B-Slett FILL-IN-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Skicka Dialog-Frame 
PROCEDURE Skicka :
/* /*------------------------------------------------------------------------------                                    */
/*   Purpose:                                                                                                          */
/*   Parameters:  <none>                                                                                               */
/*   Notes:                                                                                                            */
/* ------------------------------------------------------------------------------*/                                    */
/* def var chSession   as com-handle.                                                                                  */
/* def var chMessage   as com-handle.                                                                                  */
/* def var chRecipient as com-handle.                                                                                  */
/* def var chAttachment as com-handle.                                                                                 */
/* DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.                                                                       */
/* DO WITH FRAME {&FRAME-NAME}:                                                                                        */
/*   IF NUM-ENTRIES(cTo,"@") <> 2 THEN DO:                                                                             */
/*       MESSAGE "Felaktig mailadress"                                                                                 */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                        */
/*       RETURN "avbryt".                                                                                              */
/*   END.                                                                                                              */
/*                                                                                                                     */
/*   create "MAPI.session" chSession.                                                                                  */
/* /*   chSession:logon("Microsoft Outlook"). */                                                                       */
/* /*   chSession:logon("Default Exchange Profile", NO, Yes, 0). */                                                    */
/*   chSession:logon.                                                                                                  */
/*   chMessage = chSession:outbox:messages:add.                                                                        */
/*   chMessage:Subject = cSubject.                                                                                     */
/*   chMessage:Type    = "IPM.Note".                                                                                   */
/*   chMessage:TEXT    = cMessage.                                                                                     */
/*                                                                                                                     */
/* /* Create one Recipient */                                                                                          */
/*   chRecipient = chMessage:Recipients:Add.                                                                           */
/*   chRecipient:name = cTo.                                                                                           */
/*   chRecipient:Type = 1.                                                                                             */
/*   chRecipient:resolve.                                                                                              */
/* /*                                                                                                               */ */
/* /*                                                                                                               */ */
/* /* Skapa vedlegg */                                                                                                 */
/*   IF cVedleggListe <> "" THEN DO iCount = 1 TO NUM-ENTRIES(cVedleggListe):                                          */
/*      chAttachment = chMessage:Attachments:Add.                                                                      */
/*      chAttachment:name = ENTRY(NUM-ENTRIES(ENTRY(iCount,cVedleggListe),"\"),ENTRY(iCount,cVedleggListe),"\").       */
/*      chAttachment:Type = 1.                                                                                         */
/*      chAttachment:ReadFromFile(ENTRY(iCount,cVedleggListe)).                                                        */
/*   END.                                                                                                              */
/* END.                                                                                                                */
/* /*                                                                                                               */ */
/* /* /* Spara och sänd */                                                                                          */ */
/* chMessage:Update.                                                                                                   */
/* chMessage:send(Yes, YES, 0).                                                                                        */
/*                                                                                                                     */
/* release object chAttachment NO-ERROR.                                                                               */
/* release object chRecipient NO-ERROR.                                                                                */
/* release object chMessage NO-ERROR.                                                                                  */
/* release object chSession NO-ERROR.                                                                                  */
/*                                                                                                                     */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

