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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE hServer AS HANDLE     NO-UNDO.
/* DEF VAR pv_hClientSocket AS HANDLE NO-UNDO. */

DEFINE VARIABLE cConnect AS CHARACTER INIT "-H socketserver -S posquest" NO-UNDO.
DEFINE VARIABLE cButs    AS CHARACTER                    NO-UNDO.
DEFINE VARIABLE dDatum   AS DATE                         NO-UNDO.
DEFINE VARIABLE cCaller AS CHARACTER   NO-UNDO.
DEFINE TEMP-TABLE tt_lager NO-UNDO
    FIELD butik AS INTEGER
    FIELD cBLager AS CHAR
    FIELD cBLagersend AS CHAR
    FIELD AntLager AS DECI
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 
&Scoped-Define DISPLAYED-OBJECTS FI-Antal 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StartUp C-Win 
FUNCTION StartUp RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD teststartupp C-Win 
FUNCTION teststartupp RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Antal AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Antal skannade" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1.67
     FONT 8 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 3.33.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Antal AT ROW 2.19 COL 22.2 COLON-ALIGNED
     RECT-4 AT ROW 1.33 COL 1.4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 52 BY 4.05.


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
         TITLE              = "PRS Socketserver"
         HEIGHT             = 4.05
         WIDTH              = 52
         MAX-HEIGHT         = 29.67
         MAX-WIDTH          = 134.4
         VIRTUAL-HEIGHT     = 29.67
         VIRTUAL-WIDTH      = 134.4
         MAX-BUTTON         = no
         RESIZE             = no
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
/* SETTINGS FOR FILL-IN FI-Antal IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PRS Socketserver */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PRS Socketserver */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
ON CLOSE OF THIS-PROCEDURE DO:
    hServer:DISABLE-CONNECTIONS().
    DELETE OBJECT hServer.
    RUN disable_UI.

/*     RUN Close_socket. */
/*     APPLY "CLOSE" TO hServer. */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* RUN "POSMessageClient.p" PERSISTENT SET hServer. */
/* RUN ConBatchServer PERSISTENT ("-S 31234"). */

SUBSCRIBE TO "Item":U ANYWHERE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* Eftersom detta är ett serverprogram, så måste vi testa om det är startat sedan tidigare    */
    /* Därför gör vi ett testconnect som klient. Om vi får OK så måste vi disconnecta och avsluta */
    IF teststartupp() = TRUE THEN DO:
/*         MESSAGE "VI KAN INTE STARTA"           */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
        RETURN.
    END.
    RUN InitButiker.
    dDatum = TODAY.
    startup().
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Close_socket C-Win 
PROCEDURE Close_socket :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hServer:DISABLE-CONNECTIONS().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConnectHandler C-Win 
PROCEDURE ConnectHandler :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip_hSocket AS HANDLE NO-UNDO. /* handle to the client socket that just connected */
 /* When this Client Socket writes a message to the server socket, the procedure "ReadHandler" is run in THIS-PROCEDURE*/
     ip_hSocket:SET-READ-RESPONSE-PROCEDURE("ReadMessage":U,THIS-PROCEDURE).
 RETURN. /* all done */

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
  DISPLAY FI-Antal 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-4 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButiker C-Win 
PROCEDURE InitButiker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH butiker NO-LOCK:
        cButs = cButs + (IF cbuts <> "" THEN "," ELSE "") + STRING(butiker.butik).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Item C-Win 
PROCEDURE Item :
/* /*------------------------------------------------------------------------------ */
/*   Purpose: */
/*   Parameters:  <none> */
/*   Notes: */
/* ------------------------------------------------------------------------------*/ */
/* DEF INPUT PARAMETER ip_cMessage AS CHAR NO-UNDO. */
/* DEFINE VARIABLE cEntry1 AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cEntry2 AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cId AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cPris AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE cSokEAN AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE dVikt AS DECIMAL    NO-UNDO. */
/* DEFINE VARIABLE cLinkinfo AS CHARACTER  NO-UNDO. */
/* DEFINE VARIABLE ilinklengd AS INTEGER    NO-UNDO. */
/*    */
/* DEFINE VARIABLE cType AS CHARACTER   NO-UNDO. */
/*    */
/*    */
/* /*  cRet = chr(27) + "B0" + chr(27) + "$" + CHR(13) +                  */ */
/* /*         chr(27) + ".0&1" + CHR(3) + CHR(13)                         */ */
/* /*         + "&2" + CHR(13) +                                          */ */
/* /*         CHR(27) + "B1" + CHR(27) + " &3" + CHR(3) + CHR(27) + "B0". */ */
/*  cRet = chr(27) + "B0" + chr(27) + "$" + CHR(13) + */
/*         chr(27) + ".0&1" + CHR(3) + CHR(13) */
/* /*         + "&2" */ + CHR(13) + */
/*         CHR(27) + "B1" + CHR(27) + " &2" +  CHR(27) + "B0" + chr(27) + CHR(13) + chr(13)  + chr(13) + "&3" + CHR(3) + CHR(27) + "B0". */
/*    */
/* /*  ASSIGN FI-Padr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip_cMessage. */ */
/* /*  ASSIGN FI-Padr:SCREEN-VALUE = ip_cMessage.                        */ */
/*  cEntry1 = ENTRY(1,ip_cMessage,">") + ">". */
/*  cEntry2 = ENTRY(1,ENTRY(2,ip_cMessage,">"),"<"). */
/*    */
/*  /* trimma bort chr(13) */ */
/*  cEntry2 = TRIM(cEntry2,CHR(13)). */
/*    */
/*  cType = ENTRY(1,ip_cMessage). */
/*  MESSAGE NUM-ENTRIES(ip_cMessage) */
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*  IF cType = "E" THEN */
/*      FI-EAN = ENTRY(2,ip_cMessage). */
/*  ELSE IF cType = "A" THEN */
/*      FI-Artikkel = ENTRY(2,ip_cMessage). */
/*    */
/*  IF cType = "E" THEN */
/*      FIND strekkode WHERE strekkode.kode = FI-EAN NO-LOCK NO-ERROR. */
/*  IF AVAIL strekkode THEN */
/*      FIND artbas OF strekkode NO-LOCK NO-ERROR. */
/*  IF NOT AVAIL artbas AND cType = "A" THEN */
/*      FIND artbas WHERE artbas.artikkelnr = DECI(FI-Artikkel) NO-LOCK NO-ERROR. */
/*  FI-Type = TRIM(ENTRY(3,cEntry1,"="),">"). */
/*  IF AVAIL artbas THEN DO: */
/*      FI-EAN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-EAN. */
/*      FI-Artikkel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-Artikkel. */
/*      FI-Type:SCREEN-VALUE = FI-Type. */
/*    */
/*      fi-antal = fi-antal + 1. */
/*    */
/*      FIND FIRST ArtPris WHERE Artpris.artikkelnr = DECI(FI-Artikkel) NO-LOCK NO-ERROR. */
/*    */
/*      FI-Namn:SCREEN-VALUE = "". */
/*      FI-Pris:SCREEN-VALUE = "0". */
/*      FI-Extrapris:SCREEN-VALUE = "0". */
/*    */
/*      IF AVAIL ArtPris THEN DO: */
/*          fi-namn = SUBSTR(artbas.bongtekst,1,20). */
/*          fi-namn:SCREEN-VALUE = fi-namn. */
/*          dVikt = 1. */
/*          IF artpris.tilbud = TRUE THEN */
/*              fi-extrapris = artpris.pris[2]. */
/*          ELSE */
/*              fi-pris = artpris.pris[1]. */
/*          cRet = SUBSTITUTE(cRet,"","      NO INFO",""). */
/*          cRet = "<data " + cId + ">" + cRet + "</data>". */
/*      END. */
/*  END. */
/*  ELSE DO: */
/*      cRet = "FEL, okänd artikkel". */
/*  END. */
/*  RUN PostMessage ("Item","new",cRet). */
/*  fi-antal:SCREEN-VALUE = STRING(fi-antal). */
/*  fi-lyckade:SCREEN-VALUE = STRING(fi-lyckade). */
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ItemPRS C-Win 
PROCEDURE ItemPRS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*  DEF INPUT PARAMETER ip_cMessage AS CHAR NO-UNDO.                                                     */
/*  DEFINE VARIABLE cEntry1 AS CHARACTER  NO-UNDO.                                                       */
/*  DEFINE VARIABLE cEntry2 AS CHARACTER  NO-UNDO.                                                       */
/* DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.                                                           */
/* DEFINE VARIABLE cId AS CHARACTER  NO-UNDO.                                                            */
/* DEFINE VARIABLE cPris AS CHARACTER  NO-UNDO.                                                          */
/* DEFINE VARIABLE cSokEAN AS CHARACTER  NO-UNDO.                                                        */
/*  cRet = chr(27) + "B0" + chr(27) + "$" + CHR(13) +                                                    */
/*         chr(27) + ".0&1" + CHR(3) + CHR(13)                                                           */
/*         + "&2" + CHR(13) +                                                                            */
/*         CHR(27) + "B1" + CHR(27) + " &3" + CHR(3) + CHR(27) + "B0".                                   */
/*                                                                                                       */
/* /*  ASSIGN FI-Padr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip_cMessage. */                              */
/* /*  ASSIGN FI-Padr:SCREEN-VALUE = ip_cMessage.                        */                              */
/*  cEntry1 = ENTRY(1,ip_cMessage,">") + ">".                                                            */
/*  cEntry2 = ENTRY(1,ENTRY(2,ip_cMessage,">"),"<").                                                     */
/*                                                                                                       */
/*  /* trimma bort chr(13) */                                                                            */
/*  cEntry2 = TRIM(cEntry2,CHR(13)).                                                                     */
/*  IF cEntry2 BEGINS "FF" THEN                                                                          */
/*      FI-EAN = SUBSTR(cEntry2,2).                                                                      */
/*  ELSE IF cEntry2 BEGINS "F" OR cEntry2 BEGINS "A" THEN                                                */
/*      FI-EAN = SUBSTR(cEntry2,2).                                                                      */
/*  cId = ENTRY(2,ip_cMessage," ").                                                                      */
/*  FI-Type = TRIM(ENTRY(3,cEntry1,"="),">").                                                            */
/*                                                                                                       */
/*  FI-EAN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-EAN.                                                 */
/*  FI-Type:SCREEN-VALUE = FI-Type.                                                                      */
/*                                                                                                       */
/*  fi-antal = fi-antal + 1.                                                                             */
/*                                                                                                       */
/*  cSokEAN = FI-EAN.                                                                                    */
/*                                                                                                       */
/*  FIND strekkode WHERE strekkode.kode = cSokEAN NO-LOCK NO-ERROR.                                      */
/*  IF NOT AVAIL strekkode THEN                                                                          */
/*      FIND strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cSokEAN)) + cSokEAN NO-LOCK NO-ERROR. */
/*  IF AVAIL Strekkode THEN DO:                                                                          */
/*      FIND Artbas OF strekkode NO-LOCK NO-ERROR.                                                       */
/*      IF AVAIL ArtBas THEN DO:                                                                         */
/*          fi-namn = SUBSTR(artbas.beskr,1,20).                                                         */
/*          fi-namn:SCREEN-VALUE = fi-namn.                                                              */
/*          fi-pris = 0.                                                                                 */
/*          IF LENGTH(cSokEAN) = 13 AND CAN-DO("20,21,22",SUBSTR(cSokEAN,1,2)) THEN DO:                  */
/*              CASE SUBSTR(cSokEAN,1,2):                                                                */
/*                  WHEN "20" THEN                                                                       */
/*                      ASSIGN fi-pris = DECI(SUBSTR(cSokEAN,9,2)) + (DECI(SUBSTR(cSokEAN,11,2)) * .01). */
/*                  WHEN "21" THEN                                                                       */
/*                      ASSIGN fi-pris = DECI(SUBSTR(cSokEAN,9,3)) + (DECI(SUBSTR(cSokEAN,12,1)) * .1).  */
/*                  WHEN "22" THEN                                                                       */
/*                      ASSIGN fi-pris = DECI(SUBSTR(cSokEAN,9,4)).                                      */
/*              END CASE.                                                                                */
/*          END.                                                                                         */
/*          ELSE DO:                                                                                     */
/*              FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.                                           */
/*              IF AVAIL artpris THEN                                                                    */
/*                  fi-pris = artpris.pris[1].                                                           */
/*          END.                                                                                         */
/*          IF fi-pris > 0 THEN DO:                                                                      */
/*              fi-lyckade = fi-lyckade + 1.                                                             */
/*              fi-pris:SCREEN-VALUE = STRING(fi-pris).                                                  */
/*              cPris = STRING(fi-pris,">>>>9.99").                                                      */
/*              cRet = SUBSTITUTE(cRet,FI-namn," ",cPris).                                               */
/*              cRet = "<data " + cId + ">" + cRet + "</data>".                                          */
/*          END.                                                                                         */
/*          ELSE DO:                                                                                     */
/*              cRet = SUBSTITUTE(cRet,"","      NO INFO","").                                           */
/*              cRet = "<data " + cId + ">" + cRet + "</data>".                                          */
/*          END.                                                                                         */
/*      END.                                                                                             */
/*      ELSE DO:                                                                                         */
/*          cRet = SUBSTITUTE(cRet,"","      NO INFO","").                                               */
/*          cRet = "<data " + cId + ">" + cRet + "</data>".                                              */
/*      END.                                                                                             */
/*  END.                                                                                                 */
/*  ELSE DO:                                                                                             */
/*      cRet = SUBSTITUTE(cRet,"","      NO INFO","").                                                   */
/*      cRet = "<data " + cId + ">" + cRet + "</data>".                                                  */
/*  END.                                                                                                 */
/*  RUN PostMessage ("Item","new",cRet).                                                                 */
/* /*     <ESC>B0<ESC>$                            */                                                    */
/* /*    <ESC>.0{name}<ETX>                        */                                                    */
/* /*    {info}                                    */                                                    */
/* /*    <ESC>B1<ESC>.8<#0x80> {price}<ETX><ESC>B0 */                                                    */
/*  fi-antal:SCREEN-VALUE = STRING(fi-antal).                                                            */
/*  fi-lyckade:SCREEN-VALUE = STRING(fi-lyckade).                                                        */
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Lager C-Win 
PROCEDURE Lager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cParameter AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER cReturn    AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
   DEFINE VARIABLE cType       AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE dArtikkelnr AS DECIMAL     NO-UNDO.
   DEFINE VARIABLE cStrekkode  AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cLager AS CHARACTER   NO-UNDO.
   cReturn = "FEL,Inget lager".
   IF dDatum < TODAY THEN
        ASSIGN FI-Antal = 0
               dDatum   = TODAY.
   FI-Antal = FI-Antal + 1.
   FI-Antal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(FI-Antal).
   RELEASE Artbas.
   IF NUM-ENTRIES(cParameter,CHR(2)) = 3 THEN DO:
       cType       = ENTRY(1,cParameter,CHR(2)).
       IF cType = "E" THEN
           cStrekkode = ENTRY(2,cParameter,CHR(2)).
       ELSE
           dArtikkelnr = DECI(ENTRY(2,cParameter,CHR(2))) NO-ERROR.
       iButikkNr = INT(ENTRY(3,cParameter,CHR(2))).
       IF NOT ERROR-STATUS:ERROR THEN DO:
           IF cType = "E" THEN DO:
               FIND strekkode WHERE strekkode.kode = cStrekkode NO-LOCK NO-ERROR.
               IF AVAIL Strekkode THEN
                   FIND Artbas OF strekkode NO-LOCK NO-ERROR.
           END.
           IF NOT AVAIL ArtBas AND cType = "A" THEN
               FIND Artbas WHERE artbas.artikkelnr = dArtikkelnr NO-LOCK NO-ERROR.
       END.
       IF AVAIL ArtBas THEN DO:
           RUN SocketLager (INPUT iButikkNr, OUTPUT cReturn).
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PostMessage C-Win 
PROCEDURE PostMessage :
DEF INPUT PARAMETER ip_cMessage AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ip_cChannel AS CHAR NO-UNDO.
 DEF INPUT PARAMETER ip_cData    AS CHAR NO-UNDO.
 DEF VAR lv_memData    AS MEMPTR NO-UNDO. /* storage area for the message */
DEF VAR lv_memWeb AS MEMPTR NO-UNDO. /* storage area for the message */
DEFINE VARIABLE lv_cMessage AS CHARACTER  NO-UNDO.
 DEF VAR lv_hMessageBuffer AS MEMPTR NO-UNDO.
 
 DEF VAR lv_cWeb AS CHAR NO-UNDO.

 IF NOT hServer:CONNECTED() THEN RETURN.

 ASSIGN lv_cWeb = ip_cData.
 
/*  SET-SIZE(lv_hMessageBuffer) = LENGTH(lv_cMessage). */

 IF LENGTH(lv_cWeb) GT 0 THEN
 DO:
  SET-SIZE(lv_memWeb) = LENGTH(lv_cWeb).
  PUT-STRING(lv_memWeb,1,LENGTH(lv_cWeb)) = lv_cWeb.
  
  IF hServer:CONNECTED()
     THEN hServer:WRITE(lv_memWeb,1,GET-SIZE(lv_memWeb)). /* write the received message to the client */
  
/*   MESSAGE hServer:CONNECTED()   */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  SET-SIZE(lv_memWeb) = 0.
 END.

 READKEY PAUSE 0.
 
/*  hServer:DISCONNECT(). */


/*  PUT-LONG(lv_hMessageBuffer,1) = LENGTH(lv_cMessage).                     */
/*  PUT-STRING(lv_hMessageBuffer,5,LENGTH(lv_cMessage)) = lv_cMessage.       */
/*                                                                           */
/*  hServer:WRITE(lv_hMessageBuffer,1,GET-SIZE(lv_hMessageBuffer)). */
/*                                                                           */
/*  SET-SIZE(lv_hMessageBuffer) = 0.                                         */
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadMessage C-Win 
PROCEDURE ReadMessage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF VAR lv_memData       AS MEMPTR NO-UNDO. /* storage area for the message */
 DEF VAR lv_memMessage AS MEMPTR NO-UNDO. /* storage area for the message */
 DEFINE VARIABLE cPostnr AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE lv_cData AS CHAR   NO-UNDO. /* the actual message */
 DEFINE VARIABLE lv_hSocket AS HANDLE NO-UNDO. /* a handle to client sockets */
 DEFINE VARIABLE iSize AS INTEGER    NO-UNDO.
 DEF VAR lv_iMessageSize AS INT NO-UNDO.
 DEFINE VARIABLE cSubRutin AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cParameter AS CHARACTER   NO-UNDO.
 DEFINE VARIABLE cReturnValue AS CHARACTER   NO-UNDO.
iSize = 4.

 IF NOT SELF:CONNECTED() THEN RETURN.
 
 SET-SIZE(lv_memData) = iSize. /* all the messages in this demo are 4 bytes long. You have to know the message size in advance */

 SELF:READ(lv_memData,1,iSize,READ-AVAILABLE) NO-ERROR. /* read the message from the client and put it into memory */

/*  SET-SIZE(lv_memData) = 4. /* all the messages in this demo are 4 bytes long. You have to know the message size in advance */ */
/*                                                                                                                               */
/*  SELF:READ(lv_memData,1,4,READ-AVAILABLE) NO-ERROR. /* read the message from the client and put it into memory */             */

 IF ERROR-STATUS:ERROR THEN RETURN. /* ooops */

 ASSIGN lv_iMessageSize = GET-LONG(lv_memData,1).

 SET-SIZE(lv_memData) = 0.
 SET-SIZE(lv_memData) = lv_iMessageSize.
 
 SELF:READ(lv_memData,1,lv_iMessageSize,READ-AVAILABLE) NO-ERROR. /* read the message from the client and put it into memory */

 IF ERROR-STATUS:ERROR THEN RETURN. /* ooops */
 
 ASSIGN lv_cData = GET-STRING(lv_memData,1,lv_iMessageSize). /* convert the memory address into the message string */
 ASSIGN cSubRutin  = ENTRY(1,lv_cData,CHR(1))
        cCaller    = ENTRY(2,lv_cData,CHR(1))
        cParameter = ENTRY(3,lv_cData,CHR(1)).
 RUN VALUE(cSubRutin) (cParameter, OUTPUT cReturnValue) NO-ERROR.

/*  IF ENTRY(1,cReturnValue) = "FEL" THEN                        */
/*      ASSIGN ENTRY(2,lv_cData,CHR(1)) = cCaller + ",FEL"       */
/*             ENTRY(3,lv_cData,CHR(1)) = ENTRY(2,cReturnValue). */
/*  ELSE                                                         */
     ASSIGN ENTRY(3,lv_cData,CHR(1)) = cReturnvalue.
 SET-SIZE(lv_memMessage) = 4 + LENGTH(lv_cData).
 PUT-LONG(lv_memMessage,1) = LENGTH(lv_cData).
 PUT-STRING(lv_memMessage,5,LENGTH(lv_cData)) = lv_cData.
 ASSIGN lv_hSocket = SESSION:FIRST-SOCKET. /* get the first connected client socket. */
 DO WHILE VALID-HANDLE(lv_hSocket): /* loop through all connected client sockets */
  IF lv_hSocket:CONNECTED()
     THEN lv_hSocket:WRITE(lv_memMessage,1,GET-SIZE(lv_memMessage)). /* write the received message to the client */
  ASSIGN lv_hSocket = lv_hSocket:NEXT-SIBLING. /* move on to the next connected client socket */
 END. /* do while */
 SET-SIZE(lv_memData) = 0. /* always do this - set memptr size to zero, otherwise you start eating memory ... */
 SET-SIZE(lv_memMessage) = 0.

 RETURN. /* all's done */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SocketLager C-Win 
PROCEDURE SocketLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER iButikkNr AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cLager AS CHARACTER   NO-UNDO.
/* DEFINE INPUT  PARAMETER cData AS CHARACTER   NO-UNDO. */
DEFINE VARIABLE cStrekkode    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dArtikkelnr   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cStorlekar AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReturStr  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cHarLager  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEgenFsg   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLU        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iButMax    AS INTEGER     NO-UNDO.
DEFINE VARIABLE lLagerFinns AS LOGICAL     NO-UNDO.
EMPTY TEMP-TABLE TT_lager.
IF NOT AVAIL artbas THEN
    RETURN.
FOR EACH strtstr WHERE strtstr.strtypeid = artbas.strtypeid NO-LOCK:
    cStorlekar = cstorlekar + (IF cStorlekar <> "" THEN "," ELSE "") + TRIM(strtstr.sostorl).
END.
cStorlekar = "Totfsg," + cStorlekar.
cHarLager = FILL(",",NUM-ENTRIES(cStorlekar) - 1).
cEgenFsg  = cHarLager. /* spec av anropande butiks fsg / str */
ENTRY(1,cHarLager) = "x".
DO  ii = 1 TO NUM-ENTRIES(cButs):
    iButMax = MAXIMUM(iButMax,LENGTH(ENTRY(ii,cButs))).
/*     CREATE tt_lager.                              */
/*     ASSIGN tt_lager.butik  = INT(ENTRY(ii,cButs)) */
/*            tt_lager.cLager = cHarLager.           */
END.

DO ii = 1 TO NUM-ENTRIES(cButs):
    FOR EACH artlag WHERE artlag.artikkelnr = artbas.artikkelnr and
                          artlag.butik = INT(ENTRY(ii,cButs)) AND 
/*                           artlag.lagant > 0 AND */
                          CAN-DO(cStorlekar,TRIM(artlag.storl)) NO-LOCK:
        FIND tt_lager WHERE tt_lager.butik = INT(ENTRY(ii,cButs)) NO-ERROR.
        IF NOT AVAIL tt_lager THEN DO:
            CREATE tt_lager.
            ASSIGN tt_lager.butik  = artlag.butik
                   tt_lager.cBLager = FILL(",",NUM-ENTRIES(cStorlekar) - 1).
        END.
        IF artlag.lagant > 0 THEN
            tt_lager.antlager = tt_lager.antlager + artlag.lagant.
        iLU = LOOKUP(TRIM(artlag.storl),cStorlekar).
        IF artlag.lagant > 0 THEN DO:
            ENTRY(iLU,tt_lager.cBLager) = STRING(artlag.lagant).
            ENTRY(iLU,cHarLager) = "x".
        END.
        /* entry 1 håller på antsolgt */
        ENTRY(1,cBLager) = STRING(INT(ENTRY(1,cBLager)) + ArtLag.AntSolgt).
/*         IF artlag.butik = iButikknr THEN                   */
/*             ENTRY(iLU,cEgenFsg) = STRING(artlag.antsolgt). */
    END.
END.
DO ii = 1 TO NUM-ENTRIES(cHarLager):
    IF ENTRY(ii,cHarLager) <> "" THEN DO:
        cReturStr = cReturStr + (IF cReturStr <> "" THEN "," ELSE "") + ENTRY(ii,cStorlekar).
/*             FILL(" ",iMax - LENGTH(ENTRY(ii,cStorlekar))) + ENTRY(ii,cStorlekar). */
    END.
END.
/*  */
FOR EACH tt_lager WHERE tt_lager.butik <> iButikkNr AND tt_lager.antlager = 0:
    DELETE tt_lager.
END.
IF NOT CAN-FIND(FIRST tt_lager WHERE tt_lager.butik <> iButikkNr) THEN DO:
    /* egna lagret endast kvar, on man inte har lag så skall den tas bort */
    FIND FIRST tt_lager NO-ERROR.
    IF AVAIL tt_lager AND tt_lager.antlager = 0 THEN
        DELETE tt_lager.

END.
/* nu har vi minst 1 butik med lager, */

FOR EACH tt_lager:
    DO ii = 1 TO NUM-ENTRIES(cHarLager):
        IF ENTRY(ii,cHarLager) <> "" THEN DO:
            tt_lager.cBLagersend = tt_lager.cBLagersend + (IF tt_lager.cBLagersend <> "" THEN "," ELSE "") + 
                         (IF ENTRY(ii,tt_lager.cBLager) <> "" THEN ENTRY(ii,tt_lager.cBLager) ELSE "").
/*             IF tt_lager.butik = iButikkNr AND ii > 1 THEN                                                                                     */
/*                 ENTRY(NUM-ENTRIES(tt_lager.cBLagersend),tt_lager.cBLagersend) = ENTRY(NUM-ENTRIES(tt_lager.cBLagersend),tt_lager.cBLagersend) */
/*                                                                                 + " / " + STRING(INT(ENTRY(ii,cEgenFsg))).                    */
/*                 FILL(" ",iMax - LENGTH(ENTRY(ii,tt_lager.cLager))) + ENTRY(ii,tt_lager.cLager). */
        END.
    END.
    tt_lager.cBLagersend = REPLACE(tt_lager.cBLagersend," ","").
END.
IF NOT CAN-FIND(FIRST tt_lager WHERE ) THEN
    cLager = "FEL,Inget lager".
ELSE DO:
        cLager = cReturStr.
/*     cLager = FILL(" ", ibutmax + 1) + cReturStr + CHR(13). */
    FOR EACH tt_lager:
/*         cLager = cLager + FILL(" ",iButMax - LENGTH(STRING(tt_lager.butik))) + string(tt_lager.butik) + " " + */
        cLager = cLager + ";" + string(tt_lager.butik) + "," + 
                         tt_lager.cBlagersend.

    END.
END.
/* OUTPUT TO "clipboard".                                                                                              */
/* PUT UNFORMATTED FILL(" ", ibutmax + 1) cReturStr SKIP.                                                              */
/* FOR EACH tt_lager:                                                                                                  */
/*     PUT UNFORMATTED FILL(" ",iButMax - LENGTH(STRING(tt_lager.butik))) tt_lager.butik " " tt_lager.clagersend SKIP. */
/* END.                                                                                                                */
/* OUTPUT CLOSE.                                                                                                       */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SocketLagerOrg C-Win 
PROCEDURE SocketLagerOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* DEFINE OUTPUT PARAMETER cLager AS CHARACTER   NO-UNDO.                                                                    */
/* /* DEFINE INPUT  PARAMETER cData AS CHARACTER   NO-UNDO. */                                                               */
/* DEFINE VARIABLE cStrekkode    AS CHARACTER   NO-UNDO.                                                                     */
/* DEFINE VARIABLE dArtikkelnr   AS DECIMAL     NO-UNDO.                                                                     */
/* DEFINE VARIABLE cButs         AS CHARACTER INIT "1,323"  NO-UNDO.                                                         */
/* DEFINE VARIABLE cStorlekar AS CHARACTER   NO-UNDO.                                                                        */
/* DEFINE VARIABLE cReturStr AS CHARACTER   NO-UNDO.                                                                         */
/* DEFINE VARIABLE cHarLager AS CHARACTER   NO-UNDO.                                                                         */
/* DEFINE VARIABLE ii      AS INTEGER     NO-UNDO.                                                                           */
/* DEFINE VARIABLE iLU     AS INTEGER     NO-UNDO.                                                                           */
/* DEFINE VARIABLE iMax    AS INTEGER     NO-UNDO.                                                                           */
/* DEFINE VARIABLE iButMax AS INTEGER     NO-UNDO.                                                                           */
/* DEFINE VARIABLE lLagerFinns AS LOGICAL     NO-UNDO.                                                                       */
/* EMPTY TEMP-TABLE TT_lager.                                                                                                */
/*                                                                                                                           */
/* IF NOT AVAIL artbas THEN                                                                                                  */
/*     RETURN.                                                                                                               */
/* FOR EACH strtstr WHERE strtstr.strtypeid = artbas.strtypeid:                                                              */
/*     cStorlekar = cstorlekar + (IF cStorlekar <> "" THEN "," ELSE "") + TRIM(strtstr.sostorl).                             */
/* END.                                                                                                                      */
/* cHarLager = FILL(",",NUM-ENTRIES(cStorlekar) - 1).                                                                        */
/* DO  ii = 1 TO NUM-ENTRIES(cButs):                                                                                         */
/*     iButMax = MAXIMUM(iButMax,LENGTH(ENTRY(ii,cButs))).                                                                   */
/*     CREATE tt_lager.                                                                                                      */
/*     ASSIGN tt_lager.butik  = INT(ENTRY(ii,cButs))                                                                         */
/*            tt_lager.cLager = cHarLager.                                                                                   */
/* END.                                                                                                                      */
/*                                                                                                                           */
/* FOR EACH tt_lager:                                                                                                        */
/*     FOR EACH artlag WHERE artlag.artikkelnr = artbas.artikkelnr and                                                       */
/*                           artlag.butik = tt_lager.butik AND                                                               */
/*                           artlag.lagant > 0 AND                                                                           */
/*                           CAN-DO(cStorlekar,TRIM(artlag.storl)):                                                          */
/*         iLU = LOOKUP(TRIM(artlag.storl),cStorlekar).                                                                      */
/*         ENTRY(iLU,tt_lager.cLager) = STRING(artlag.lagant).                                                               */
/*         ENTRY(iLU,cHarLager) = "x".                                                                                       */
/*         iMax = MAXIMUM(iMax,LENGTH(ENTRY(iLU,cStorlekar)),LENGTH(ENTRY(iLU,tt_lager.clager))).                            */
/*         lLagerFinns = TRUE.                                                                                               */
/*     END.                                                                                                                  */
/* END.                                                                                                                      */
/* iMax = iMax + 1. /* maxlängd + 1 */                                                                                       */
/* DO ii = 1 TO NUM-ENTRIES(cHarLager):                                                                                      */
/*     IF ENTRY(ii,cHarLager) <> "" THEN                                                                                     */
/*         cReturStr = cReturStr + (IF cReturStr <> "" THEN "," ELSE "") +                                                   */
/*             FILL(" ",iMax - LENGTH(ENTRY(ii,cStorlekar))) + ENTRY(ii,cStorlekar).                                         */
/* END.                                                                                                                      */
/* FOR EACH tt_lager:                                                                                                        */
/*     DO ii = 1 TO NUM-ENTRIES(cHarLager):                                                                                  */
/*         IF ENTRY(ii,cHarLager) <> "" THEN                                                                                 */
/*             tt_lager.cLagersend = tt_lager.cLagersend + (IF tt_lager.cLagersend <> "" THEN "," ELSE "") +                 */
/*                 FILL(" ",iMax - LENGTH(ENTRY(ii,tt_lager.cLager))) + ENTRY(ii,tt_lager.cLager).                           */
/*     END.                                                                                                                  */
/* END.                                                                                                                      */
/* IF lLagerFinns = FALSE THEN                                                                                               */
/*     cLager = "Inget lager".                                                                                               */
/* ELSE DO:                                                                                                                  */
/*     cLager = FILL(" ", ibutmax + 1) + cReturStr + CHR(13).                                                                */
/*     FOR EACH tt_lager:                                                                                                    */
/*         cLager = cLager + FILL(" ",iButMax - LENGTH(STRING(tt_lager.butik))) + string(tt_lager.butik) + " " +             */
/*                          tt_lager.clagersend + CHR(13).                                                                   */
/*     END.                                                                                                                  */
/* END.                                                                                                                      */
/* /* OUTPUT TO "clipboard".                                                                                              */ */
/* /* PUT UNFORMATTED FILL(" ", ibutmax + 1) cReturStr SKIP.                                                              */ */
/* /* FOR EACH tt_lager:                                                                                                  */ */
/* /*     PUT UNFORMATTED FILL(" ",iButMax - LENGTH(STRING(tt_lager.butik))) tt_lager.butik " " tt_lager.clagersend SKIP. */ */
/* /* END.                                                                                                                */ */
/* /* OUTPUT CLOSE.                                                                                                       */ */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StartUp C-Win 
FUNCTION StartUp RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 DEFINE VARIABLE lOkEnable AS LOGICAL    NO-UNDO.
 CREATE SERVER-SOCKET hServer. /* create the server socket and store the handle in hServer */

 /* ensure that the server can received connection requests. In this example, the server
    is using the localmachine and a service of 1234. Obviously the host and service
    can be replaced with any machine or service that you need */
 ASSIGN lOkEnable = hServer:ENABLE-CONNECTIONS(cConnect) NO-ERROR.

 IF lOkEnable THEN DO:
     /* when a client connects to this server, run the procedure "ConnectHandler" in THIS-PROCEDURE */
     hServer:SET-CONNECT-PROCEDURE("ConnectHandler":U,THIS-PROCEDURE).
 END.
 ELSE RETURN FALSE.
 RETURN TRUE. /* all's ok !*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION teststartupp C-Win 
FUNCTION teststartupp RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 CREATE SOCKET hServer NO-ERROR.
/*  hServer:CONNECT("-H ken1lap3 -S 9101":U). */
 hServer:CONNECT(cConnect) NO-ERROR.
 IF hServer:CONNECTED() THEN DO:
     MESSAGE "LKJ"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     /* Detta är ett serverprogram */
     /* Om vi får kontakt så måste vi avsluta. Vi kan inte starta 2 instanser */
     hServer:DISCONNECT().
     RETURN TRUE.
 END.
 ELSE
     RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

