&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

/* Local Variable Definitions ---                                       */

DEF VAR obOk       AS LOG  NO-UNDO.
DEF VAR ocError    AS CHAR NO-UNDO.
DEF VAR ocRetParam AS CHAR NO-UNDO.
DEF VAR cTekst     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-5 RECT-6 B-getCheck ~
fi-MemberId fi-Bar_Code FI-ButikkNr btnButikk B-insertTransaction ~
fi-MemberId-2 fi-MEMBER_SEARCH_TYPE fi-B_Id btnBong B-isMember-2 ~
B-getAvailableOffers B-isMember btnMember B-insertMember B-updateMember ~
B-getMember fiMobilNr fiForNavn fiEtternavn fieMail RS-Kjonn fi-MedlemsNr ~
fi-EksterntId fiAdresse fiPostNr fiPostSted fiDatoFodt fiButikkNr BtnDone ~
FILL-IN-4 FILL-IN-5 FILL-IN-13 FILL-IN-7 
&Scoped-Define DISPLAYED-OBJECTS fi-MemberId fi-Bar_Code FI-ButikkNr ~
fi-MemberId-2 fi-MEMBER_SEARCH_TYPE fi-B_Id fi-BongNR fi-Belop fiPersonNr ~
fiMobilNr fiForNavn fiEtternavn fieMail RS-Kjonn fi-MedlemsNr fi-EksterntId ~
fiAdresse fiPostNr fiPostSted fiDatoFodt fiButikkNr FILL-IN-4 FILL-IN-5 ~
FILL-IN-13 FILL-IN-7 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-getAvailableOffers 
     LABEL "getAvailableOffers..." 
     SIZE 31 BY 1.14.

DEFINE BUTTON B-getCheck 
     LABEL "getCheck..." 
     SIZE 31 BY 1.14.

DEFINE BUTTON B-getMember 
     LABEL "getMember ..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-insertMember 
     LABEL "insertMember ..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-insertTransaction 
     LABEL "insertTransaction..." 
     SIZE 31 BY 1.14.

DEFINE BUTTON B-isMember 
     LABEL "isMemberTest..." 
     SIZE 31 BY 1.14.

DEFINE BUTTON B-isMember-2 
     LABEL "isMemberTelefonNr..." 
     SIZE 31 BY 1.14.

DEFINE BUTTON B-updateMember 
     LABEL "updateMember ..." 
     SIZE 24 BY 1.14.

DEFINE BUTTON btnBong 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnButikk 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnMember 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE fi-Bar_Code AS CHARACTER FORMAT "X(256)":U INITIAL "200001021190" 
     LABEL "Bar_Code" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi-Belop AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beløp" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi-BongNR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kvitto Nr" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE fi-B_Id AS CHARACTER FORMAT "X(256)":U 
     LABEL "B_Id" 
     VIEW-AS FILL-IN 
     SIZE 30.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-EksterntId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Eksternt Id" 
     VIEW-AS FILL-IN 
     SIZE 30.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-MedlemsNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Medlemsnr" 
     VIEW-AS FILL-IN 
     SIZE 30.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-MemberId AS CHARACTER FORMAT "X(256)":U INITIAL "M-00019664" 
     LABEL "MemberId" 
     VIEW-AS FILL-IN 
     SIZE 30.2 BY 1 NO-UNDO.

DEFINE VARIABLE fi-MemberId-2 AS CHARACTER FORMAT "X(256)":U INITIAL "M-00019665" 
     LABEL "MemberId" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fi-MEMBER_SEARCH_TYPE AS CHARACTER FORMAT "X(256)":U INITIAL "accountNumber" 
     LABEL "cMEMBER_SEARCH_TYPE" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE fiAdresse AS CHARACTER FORMAT "X(256)":U 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1 NO-UNDO.

DEFINE VARIABLE fiButikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 176 
     LABEL "Butikknr" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fiDatoFodt AS DATE FORMAT "99.99.9999":U 
     LABEL "Fødselsdato" 
     VIEW-AS FILL-IN 
     SIZE 20.8 BY 1 NO-UNDO.

DEFINE VARIABLE fieMail AS CHARACTER FORMAT "X(256)":U 
     LABEL "eMail" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE fiEtternavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE fiForNavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "ForNavn" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE FILL-IN-13 AS CHARACTER FORMAT "X(256)":U INITIAL "offerService" 
      VIEW-AS TEXT 
     SIZE 30.4 BY .62
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "CheckService" 
      VIEW-AS TEXT 
     SIZE 30.4 BY .62
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-5 AS CHARACTER FORMAT "X(256)":U INITIAL "TransactionService" 
      VIEW-AS TEXT 
     SIZE 30.4 BY .62
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(256)":U INITIAL "MemberService" 
      VIEW-AS TEXT 
     SIZE 30.4 BY .62
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fiMobilNr AS CHARACTER FORMAT "X(256)":U INITIAL "41365436" 
     LABEL "MobilNr" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1
     BGCOLOR 16  NO-UNDO.

DEFINE VARIABLE fiPersonNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Personnr" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1 NO-UNDO.

DEFINE VARIABLE fiPostNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE fiPostSted AS CHARACTER FORMAT "X(256)":U 
     LABEL "Poststed" 
     VIEW-AS FILL-IN 
     SIZE 93 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Kjonn AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ukjent", 0,
"Mann", 1,
"Kvinne", 2
     SIZE 34 BY 1.1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 204.2 BY 5.24.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 204.2 BY 5.24.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 89.2 BY 8.38.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 17.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-getCheck AT ROW 1.95 COL 3 WIDGET-ID 4
     fi-MemberId AT ROW 1.95 COL 48.8 COLON-ALIGNED
     fi-Bar_Code AT ROW 1.95 COL 92.4 COLON-ALIGNED
     FI-ButikkNr AT ROW 7.76 COL 49 COLON-ALIGNED
     btnButikk AT ROW 7.76 COL 81.4 WIDGET-ID 72 NO-TAB-STOP 
     B-insertTransaction AT ROW 7.91 COL 3 WIDGET-ID 20
     fi-MemberId-2 AT ROW 7.91 COL 109.6 COLON-ALIGNED
     fi-MEMBER_SEARCH_TYPE AT ROW 7.91 COL 167.2 COLON-ALIGNED
     fi-B_Id AT ROW 8.86 COL 48.8 COLON-ALIGNED
     btnBong AT ROW 8.86 COL 81.4 WIDGET-ID 68 NO-TAB-STOP 
     fi-BongNR AT ROW 10.05 COL 49 COLON-ALIGNED
     fi-Belop AT ROW 11.24 COL 49 COLON-ALIGNED
     B-isMember-2 AT ROW 13.71 COL 59.4 WIDGET-ID 76
     B-getAvailableOffers AT ROW 14.1 COL 3 WIDGET-ID 60
     B-isMember AT ROW 14.33 COL 94 WIDGET-ID 32
     btnMember AT ROW 14.33 COL 125 WIDGET-ID 74 NO-TAB-STOP 
     B-insertMember AT ROW 14.33 COL 130 WIDGET-ID 40
     B-updateMember AT ROW 14.33 COL 154 WIDGET-ID 54
     B-getMember AT ROW 14.33 COL 178 WIDGET-ID 38
     fiPersonNr AT ROW 16.24 COL 109 COLON-ALIGNED
     fiMobilNr AT ROW 17.29 COL 109 COLON-ALIGNED
     fiForNavn AT ROW 18.33 COL 109 COLON-ALIGNED
     fiEtternavn AT ROW 19.38 COL 109 COLON-ALIGNED
     fieMail AT ROW 20.43 COL 109 COLON-ALIGNED
     RS-Kjonn AT ROW 21.48 COL 111 NO-LABEL WIDGET-ID 62
     fi-MedlemsNr AT ROW 22.67 COL 109 COLON-ALIGNED
     fi-EksterntId AT ROW 23.71 COL 109 COLON-ALIGNED
     fiAdresse AT ROW 24.81 COL 109 COLON-ALIGNED
     fiPostNr AT ROW 25.91 COL 109 COLON-ALIGNED
     fiPostSted AT ROW 27 COL 109 COLON-ALIGNED
     fiDatoFodt AT ROW 28.1 COL 109.2 COLON-ALIGNED
     fiButikkNr AT ROW 29.19 COL 109 COLON-ALIGNED
     BtnDone AT ROW 31.24 COL 192 WIDGET-ID 6
     FILL-IN-4 AT ROW 1.29 COL 1.6 COLON-ALIGNED NO-LABEL
     FILL-IN-5 AT ROW 7.1 COL 1.6 COLON-ALIGNED NO-LABEL
     FILL-IN-13 AT ROW 13.14 COL 1.8 COLON-ALIGNED NO-LABEL
     FILL-IN-7 AT ROW 13.29 COL 92 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.48 COL 3 WIDGET-ID 12
     RECT-2 AT ROW 7.57 COL 1.8 WIDGET-ID 16
     RECT-5 AT ROW 13.52 COL 2 WIDGET-ID 56
     RECT-6 AT ROW 13.62 COL 92.2 WIDGET-ID 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 206.4 BY 31.52
         DEFAULT-BUTTON BtnDone WIDGET-ID 100.


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
         TITLE              = "Gant Exclusive - Mayflower"
         HEIGHT             = 31.52
         WIDTH              = 206.4
         MAX-HEIGHT         = 31.52
         MAX-WIDTH          = 206.4
         VIRTUAL-HEIGHT     = 31.52
         VIRTUAL-WIDTH      = 206.4
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
/* SETTINGS FOR FILL-IN fi-Belop IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-BongNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPersonNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiPersonNr:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Gant Exclusive - Mayflower */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Gant Exclusive - Mayflower */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-getAvailableOffers
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-getAvailableOffers C-Win
ON CHOOSE OF B-getAvailableOffers IN FRAME DEFAULT-FRAME /* getAvailableOffers... */
DO:
  RUN asMayflowerTest ('OfferService','getAvailableOffers',
                   TRIM(fi-EksterntId:SCREEN-VALUE)
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'getAvailableOffers' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-getCheck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-getCheck C-Win
ON CHOOSE OF B-getCheck IN FRAME DEFAULT-FRAME /* getCheck... */
DO:
  RUN asMayflowerTest ('CheckService','getCheck',
                   fi-MemberId:SCREEN-VALUE + '|' + fi-Bar_Code:SCREEN-VALUE
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'getCheck' SKIP
      'obOk:' obOk SKIP(1)
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-getMember
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-getMember C-Win
ON CHOOSE OF B-getMember IN FRAME DEFAULT-FRAME /* getMember ... */
DO:
  RUN asMayflowerTest ('MemberService','getMember',
                   TRIM(fi-EksterntId:SCREEN-VALUE) + '|' + 
                   '176'
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'getMember' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-insertMember
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-insertMember C-Win
ON CHOOSE OF B-insertMember IN FRAME DEFAULT-FRAME /* insertMember ... */
DO:
  RUN asMayflowerTest ('MemberService','insertMember',
                   fiPersonNr:SCREEN-VALUE + '|' + 
                   fiMobilNr:SCREEN-VALUE + '|' + 
                   fiFornavn:SCREEN-VALUE + '|' + 
                   fiEtterNavn:SCREEN-VALUE + '|' + 
                   fieMail:SCREEN-VALUE + '|' +
                   RS-Kjonn:SCREEN-VALUE + '|' +
                   fiAdresse:SCREEN-VALUE + '|' +
                   fiPostNr:SCREEN-VALUE + '|' +
                   fiPostSted:SCREEN-VALUE + '|' +
                   fiDatoFodt:SCREEN-VALUE + '|' +
                   fiButikkNr:SCREEN-VALUE + '|' 
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).

  IF TRIM(ENTRY(1,ocRetParam,"|")) <> '' THEN
  DO TRANSACTION:
    FIND Medlem EXCLUSIVE-LOCK WHERE
        Medlem.MedlemsNr = DEC(fi-MedlemsNr:SCREEN-VALUE) NO-ERROR.
    IF AVAILABLE Medlem THEN
    DO:
        Medlem.EksterntMedlemsNr = TRIM(ENTRY(1,ocRetParam,"|")).
        RELEASE Medlem.
    END.
  END.

  MESSAGE 
      'insertMember' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-insertTransaction
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-insertTransaction C-Win
ON CHOOSE OF B-insertTransaction IN FRAME DEFAULT-FRAME /* insertTransaction... */
DO:
  RUN asMayflowerTest ('transactionService','insertTransaction',
                   fi-B_Id:SCREEN-VALUE + '|' +
                   fi-MemberId-2:SCREEN-VALUE + '|' + fi-MEMBER_SEARCH_TYPE:SCREEN-VALUE
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'insertTransaction' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-isMember
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-isMember C-Win
ON CHOOSE OF B-isMember IN FRAME DEFAULT-FRAME /* isMemberTest... */
DO:
  RUN asMayflower ('MemberService','isMember','MemberId',
                   TRIM(fi-MemberId:SCREEN-VALUE)
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'isMember' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-isMember-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-isMember-2 C-Win
ON CHOOSE OF B-isMember-2 IN FRAME DEFAULT-FRAME /* isMemberTelefonNr... */
DO:
  RUN asMayflower ('MemberService','isMember','mobile',
                   TRIM(fiMobilNr:SCREEN-VALUE)
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'isMember' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-updateMember
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-updateMember C-Win
ON CHOOSE OF B-updateMember IN FRAME DEFAULT-FRAME /* updateMember ... */
DO:
  RUN asMayflowerTest ('MemberService','updateMember',
                   fi-EksterntId:SCREEN-VALUE + '|' +
                   fiPersonNr:SCREEN-VALUE + '|' + 
                   fiMobilNr:SCREEN-VALUE + '|' + 
                   fiFornavn:SCREEN-VALUE + '|' + 
                   fiEtterNavn:SCREEN-VALUE + '|' + 
                   fieMail:SCREEN-VALUE + '|' +
                   '176'
                   , OUTPUT obOk, OUTPUT ocError, OUTPUT ocRetParam).
  MESSAGE 
      'updateMember' SKIP
      'obOk:' obOk SKIP
      'ocReturn:' ocError SKIP(1)
      'ocRetParam:' ocRetParam 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBong C-Win
ON CHOOSE OF btnBong IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF fi-B_Id DO:

  obOk = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "BongHode"
                     + ";ButikkNr"
                     + ";Dato"
                     + ";KasseNr"
                     + ";BongNr"
                     + ";Belop"
                     + ";!B_Id|>>>>>>>>>>>>>>>>>>>>>>>9"
                     ,
                   "WHERE ButikkNr = '" +  FI-ButikkNr:SCREEN-VALUE + "'"
                    ,""
                    ,"B_Id,ButikkNr,KasseNr,Dato,Belop,BongNr",
                    OUTPUT cTekst,
                    OUTPUT obOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF obOk AND cTekst NE "" THEN DO:
    ASSIGN 
        fi-B_Id:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
        fi-Belop:SCREEN-VALUE     = ENTRY(5,cTekst,"|")
        fi-BongNr:SCREEN-VALUE    = ENTRY(6,cTekst,"|")
       .
    APPLY "any-printable" TO fi-B_Id.
  END.
  APPLY "ENTRY" TO fi-B_Id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikk C-Win
ON CHOOSE OF btnButikk IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF fi-ButikkNr DO:

  obOk = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Butiker"
                     + ";Butik"
                     + ";ButNamn"
                     + ";KortNavn"
                     ,
                   "WHERE true"
                    ,""
                    ,"Butik,ButNamn",
                    OUTPUT cTekst,
                    OUTPUT obOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF obOk AND cTekst NE "" THEN DO:
    ASSIGN 
       fi-ButikkNr:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
       .
    APPLY "any-printable" TO fi-ButikkNr.
  END.
  APPLY "ENTRY" TO fi-ButikkNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME DEFAULT-FRAME /* Avslutt */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMember
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMember C-Win
ON CHOOSE OF btnMember IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF fi-EksterntId DO:

  obOk = FALSE.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "Medlem"
                     + ";MedlemsNr"
                     + ";EksterntMedlemsNr"
                     + ";Fornavn"
                     + ";Etternavn"
                     + ";PersonNr"
                     + ";MobilTlf"
                     + ";ePostAdresse"
                     + ";Adresse1"
                     + ";PostNr"
                     ,
                   "WHERE true"
                    ,""
                    ,"EksterntMedlemsNr,MedlemsNr,Fornavn,Etternavn,PersonNr,MobilTlf,ePostAdresse,Adresse1,PostNr",
                    OUTPUT cTekst,
                    OUTPUT obOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.
  IF obOk AND cTekst NE "" THEN DO:
    ASSIGN 
       fi-EksterntId:SCREEN-VALUE = ENTRY(1,cTekst,"|")
        fi-MedlemsNr:SCREEN-VALUE = ENTRY(2,cTekst,"|")

        fiPersonNr:SCREEN-VALUE   = ENTRY(5,cTekst,"|")
        fiMobilNr:SCREEN-VALUE    = ENTRY(6,cTekst,"|")
        fiForNavn:SCREEN-VALUE    = ENTRY(3,cTekst,"|")
        fiEtternavn:SCREEN-VALUE  = ENTRY(4,cTekst,"|")
        fieMail:SCREEN-VALUE      = ENTRY(7,cTekst,"|")
        fiAdresse:SCREEN-VALUE    = ENTRY(8,cTekst,"|")
        fiPostNr:SCREEN-VALUE     = ENTRY(9,cTekst,"|")
/*         fiDatoFodt:SCREEN-VALUE   = ENTRY(4,cTekst,"|") */
       .
    APPLY "any-printable" TO fi-EksterntId.
  END.
  APPLY "ENTRY" TO fi-EksterntId.
END.

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
  DISPLAY fi-MemberId fi-Bar_Code FI-ButikkNr fi-MemberId-2 
          fi-MEMBER_SEARCH_TYPE fi-B_Id fi-BongNR fi-Belop fiPersonNr fiMobilNr 
          fiForNavn fiEtternavn fieMail RS-Kjonn fi-MedlemsNr fi-EksterntId 
          fiAdresse fiPostNr fiPostSted fiDatoFodt fiButikkNr FILL-IN-4 
          FILL-IN-5 FILL-IN-13 FILL-IN-7 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-5 RECT-6 B-getCheck fi-MemberId fi-Bar_Code 
         FI-ButikkNr btnButikk B-insertTransaction fi-MemberId-2 
         fi-MEMBER_SEARCH_TYPE fi-B_Id btnBong B-isMember-2 
         B-getAvailableOffers B-isMember btnMember B-insertMember 
         B-updateMember B-getMember fiMobilNr fiForNavn fiEtternavn fieMail 
         RS-Kjonn fi-MedlemsNr fi-EksterntId fiAdresse fiPostNr fiPostSted 
         fiDatoFodt fiButikkNr BtnDone FILL-IN-4 FILL-IN-5 FILL-IN-13 FILL-IN-7 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

