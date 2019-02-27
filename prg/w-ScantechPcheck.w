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

DEFINE VARIABLE hClient AS HANDLE     NO-UNDO.

DEFINE VARIABLE iProfilnr AS INTEGER  INIT 1   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-4 FI-TYpe FI-EAN FI-Antal ~
FI-Namn FI-Pris FI-Lyckade FI-Link FI-Linkpris 
&Scoped-Define DISPLAYED-OBJECTS FI-TYpe FI-EAN FI-Antal FI-Namn FI-Pris ~
FI-Lyckade FI-Link FI-Linkpris 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Antal AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antal skannade" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.67
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-EAN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sista skannade" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Link AS CHARACTER FORMAT "X(256)":U 
     LABEL "Linknamn" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Linkpris AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Linkpris" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Lyckade AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antal lyckade" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.67
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Namn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Pris AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-TYpe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Scannertyp" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74 BY 10.48.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 41 BY 10.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-TYpe AT ROW 3.14 COL 17.8 COLON-ALIGNED
     FI-EAN AT ROW 4.38 COL 17.8 COLON-ALIGNED
     FI-Antal AT ROW 5.48 COL 99 COLON-ALIGNED
     FI-Namn AT ROW 5.62 COL 17.8 COLON-ALIGNED
     FI-Pris AT ROW 6.91 COL 17.8 COLON-ALIGNED
     FI-Lyckade AT ROW 7.86 COL 99 COLON-ALIGNED
     FI-Link AT ROW 8.14 COL 18 COLON-ALIGNED
     FI-Linkpris AT ROW 9.71 COL 17.8 COLON-ALIGNED
     RECT-1 AT ROW 2.43 COL 2
     RECT-4 AT ROW 2.43 COL 80
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122 BY 13.14.


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
         TITLE              = "PRS priskontroll"
         HEIGHT             = 13.14
         WIDTH              = 122
         MAX-HEIGHT         = 24.48
         MAX-WIDTH          = 134.4
         VIRTUAL-HEIGHT     = 24.48
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
ASSIGN 
       FI-Antal:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-EAN:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Link:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Linkpris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Lyckade:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Namn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Pris:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-TYpe:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* PRS priskontroll */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* PRS priskontroll */
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
    RUN disable_UI.
    APPLY "CLOSE" TO hClient.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

RUN "SG15messageclient.p" PERSISTENT SET hClient.
/* RUN ConBatchServer PERSISTENT ("-S 31234"). */

SUBSCRIBE TO "Item":U ANYWHERE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*     FIND FIRST butikk NO-LOCK.                                            */
/*     CURRENT-WINDOW:TITLE = CURRENT-WINDOW:TITLE + "   -  " + butikk.navn. */
    CURRENT-WINDOW:TITLE = CURRENT-WINDOW:TITLE + "   -  DEMO".
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
  DISPLAY FI-TYpe FI-EAN FI-Antal FI-Namn FI-Pris FI-Lyckade FI-Link FI-Linkpris 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-4 FI-TYpe FI-EAN FI-Antal FI-Namn FI-Pris FI-Lyckade 
         FI-Link FI-Linkpris 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Item C-Win 
PROCEDURE Item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip_cMessage AS CHAR NO-UNDO.
 DEFINE VARIABLE cEntry1 AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cEntry2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cId AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPris AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSokEAN AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dVikt AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cLinkinfo AS CHARACTER  NO-UNDO.
DEFINE VARIABLE ilinklengd AS INTEGER    NO-UNDO.
DEFINE BUFFER bufArtbas FOR artbas.
 cRet = chr(27) + "B0" + chr(27) + "$" + CHR(13) +
        chr(27) + ".0&1" + CHR(3) + CHR(13) + CHR(13) +
        CHR(27) + "B1" + CHR(27) + " &2" +  CHR(27) + "B0" + chr(27) + CHR(13) + chr(13)  + chr(13) + "&3" + CHR(3) + CHR(27) + "B0".

 cEntry1 = ENTRY(1,ip_cMessage,">") + ">".
 cEntry2 = ENTRY(1,ENTRY(2,ip_cMessage,">"),"<").

 /* trimma bort chr(13) */
 cEntry2 = TRIM(cEntry2,CHR(13)).
 IF cEntry2 BEGINS "FF" THEN
     FI-EAN = SUBSTR(cEntry2,3).
 ELSE IF cEntry2 BEGINS "F" OR cEntry2 BEGINS "A" THEN
     FI-EAN = SUBSTR(cEntry2,2).
 cId = ENTRY(2,ip_cMessage," ").
 FI-Type = TRIM(ENTRY(3,cEntry1,"="),">").

 FI-EAN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-EAN.
 FI-Type:SCREEN-VALUE = FI-Type.

 fi-antal = fi-antal + 1.


 IF LENGTH(FI-EAN) = 13 AND CAN-DO("20,21,22,23,24,25",SUBSTR(FI-EAN,1,2)) THEN DO:
     cSokEAN = "20" + SUBSTR(FI-EAN,3,6) + "00000".
 END.
 ELSE
     cSokEAN = FI-EAN.
/*  FIND FIRST Pris WHERE pris.ean = DECI(cSokEAN) NO-LOCK NO-ERROR. */
 FIND strekkode WHERE strekkode.kode = cSokEAN NO-LOCK NO-ERROR.
 IF NOT AVAIL strekkode THEN
     FIND strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cSokEAN)) + cSokEAN NO-LOCK NO-ERROR.

 FI-Namn:SCREEN-VALUE = "".
 FI-Pris:SCREEN-VALUE = "0".
 FI-Link:SCREEN-VALUE = "".
 FI-Linkpris:SCREEN-VALUE = "0".

 IF AVAIL Strekkode THEN DO:
     FIND FIRST artbas OF strekkode NO-LOCK NO-ERROR.
     IF AVAIL artbas THEN DO:
         fi-namn = SUBSTR(artbas.bongtekst,1,20).
         fi-namn:SCREEN-VALUE = fi-namn.
         dVikt = 1.
         FIND artpris WHERE artpris.artikkelnr = artbas.artikkelnr AND artpris.profilnr = iProfilnr NO-LOCK NO-ERROR.
         IF NOT AVAIL artpris THEN
             FIND FIRST artpris WHERE artpris.artikkelnr = artbas.artikkelnr NO-LOCK NO-ERROR.
         IF AVAIL artpris THEN DO:
             IF artpris.tilbud = TRUE THEN
                 fi-pris = artpris.pris[2].
             ELSE
                 fi-pris = artpris.pris[1].
         END.
         IF LENGTH(FI-EAN) = 13 AND CAN-DO("20,21,22",SUBSTR(FI-EAN,1,2)) THEN DO:
             CASE SUBSTR(FI-EAN,1,2):
                 WHEN "20" THEN
                     ASSIGN fi-pris = DECI(SUBSTR(FI-EAN,9,2)) + (DECI(SUBSTR(FI-EAN,11,2)) * .01).
                 WHEN "21" THEN
                     ASSIGN fi-pris = DECI(SUBSTR(FI-EAN,9,3)) + (DECI(SUBSTR(FI-EAN,12,1)) * .1).
                 WHEN "22" THEN
                     ASSIGN fi-pris = DECI(SUBSTR(FI-EAN,9,4)).
             END CASE.
         END.
         ELSE IF LENGTH(FI-EAN) = 13 AND CAN-DO("23,24,25",SUBSTR(FI-EAN,1,2)) THEN DO:
             CASE SUBSTR(FI-EAN,1,2):
                 WHEN "23" THEN
                     ASSIGN dVikt = DECI(SUBSTR(FI-EAN,9,1)) + (DECI(SUBSTR(FI-EAN,10,3)) * .001).
                 WHEN "24" THEN
                     ASSIGN dVikt = DECI(SUBSTR(FI-EAN,9,2)) + (DECI(SUBSTR(FI-EAN,11,2)) * .01).
                 WHEN "25" THEN
                     ASSIGN dVikt = DECI(SUBSTR(FI-EAN,9,3)) + (DECI(SUBSTR(FI-EAN,12,1)) * .1).
             END CASE.
         END.
         fi-link = "".
         FI-linkpris = 0.
         IF artbas.linkvarenr > 0 THEN DO:
              FIND bufartbas WHERE bufartbas.artikkelnr = artbas.linkvarenr NO-LOCK NO-ERROR.
              IF AVAIL bufartbas THEN DO:
                  FIND artpris WHERE artpris.artikkelnr = bufartbas.artikkelnr AND artpris.profilnr = iProfilnr NO-LOCK NO-ERROR.
                  IF NOT AVAIL artpris THEN
                      FIND FIRST artpris WHERE artpris.artikkelnr = bufartbas.artikkelnr NO-LOCK NO-ERROR.
                  IF AVAIL artpris THEN DO:
                      cLinkInfo = SUBSTR(bufartbas.bongtekst,1,14).
                      fi-link = bufartbas.bongtekst.
                  END.
              END.
              cLinkinfo = FILL(" ",20).
              fi-linkpris = IF artpris.tilbud = TRUE THEN artpris.pris[2] ELSE artpris.pris[1].
              cLinkInfo = "+ " + SUBSTR(bufartbas.bongtekst,1,14) + "  " + TRIM(STRING(fi-linkpris,">>>9.99")).
         END.
         fi-link:SCREEN-VALUE = fi-link.
         fi-linkpris:SCREEN-VALUE = STRING(fi-linkpris).
         fi-pris = ROUND(dVikt * fi-pris,2).
         IF fi-pris > 0 THEN DO:
             fi-lyckade = fi-lyckade + 1.
             fi-pris:SCREEN-VALUE = STRING(fi-pris).
             cPris = STRING(fi-pris,">>>>9.99").
             cRet = SUBSTITUTE(cRet,FI-namn,"Pris: " + cPris,cLinkinfo).
             cRet = "<data " + cId + ">" + cRet + "</data>".
         END.
         ELSE DO:
             cRet = SUBSTITUTE(cRet,"","      NO INFO","").
             cRet = "<data " + cId + ">" + cRet + "</data>".
         END.
     END.
     ELSE DO:
         cRet = SUBSTITUTE(cRet,"","      NO INFO","").
         cRet = "<data " + cId + ">" + cRet + "</data>".
     END.
 END.
 ELSE DO:
     cRet = SUBSTITUTE(cRet,"","      NO INFO","").
     cRet = "<data " + cId + ">" + cRet + "</data>".
 END.
 RUN PostMessage ("Item","new",cRet).
/*     <ESC>B0<ESC>$                            */
/*    <ESC>.0{name}<ETX>                        */
/*    {info}                                    */
/*    <ESC>B1<ESC>.8<#0x80> {price}<ETX><ESC>B0 */
 fi-antal:SCREEN-VALUE = STRING(fi-antal).
 fi-lyckade:SCREEN-VALUE = STRING(fi-lyckade).
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ItemORG C-Win 
PROCEDURE ItemORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip_cMessage AS CHAR NO-UNDO.
 DEFINE VARIABLE cEntry1 AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE cEntry2 AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRet AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cId AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPris AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSokEAN AS CHARACTER  NO-UNDO.
 cRet = chr(27) + "B0" + chr(27) + "$" + CHR(13) +
        chr(27) + ".0&1" + CHR(3) + CHR(13) + "&2" + CHR(13) +
        CHR(27) + "B1" + CHR(27) + " &3" + CHR(3) + CHR(27) + "B0".

/*  ASSIGN FI-Padr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ip_cMessage. */
/*  ASSIGN FI-Padr:SCREEN-VALUE = ip_cMessage.                        */
 cEntry1 = ENTRY(1,ip_cMessage,">") + ">".
 cEntry2 = ENTRY(1,ENTRY(2,ip_cMessage,">"),"<").

 /* trimma bort chr(13) */
 cEntry2 = TRIM(cEntry2,CHR(13)).
 IF cEntry2 BEGINS "FF" THEN
     FI-EAN = SUBSTR(cEntry2,2).
 ELSE IF cEntry2 BEGINS "F" OR cEntry2 BEGINS "A" THEN
     FI-EAN = SUBSTR(cEntry2,2).
 cId = ENTRY(2,ip_cMessage," ").
 FI-Type = TRIM(ENTRY(3,cEntry1,"="),">").

 FI-EAN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-EAN.
 FI-Type:SCREEN-VALUE = FI-Type.

 fi-antal = fi-antal + 1.

 cSokEAN = FI-EAN.

 FIND strekkode WHERE strekkode.kode = cSokEAN NO-LOCK NO-ERROR.
 IF NOT AVAIL strekkode THEN
     FIND strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cSokEAN)) + cSokEAN NO-LOCK NO-ERROR.
 IF AVAIL Strekkode THEN DO:
     FIND Artbas OF strekkode NO-LOCK NO-ERROR.
     IF AVAIL ArtBas THEN DO:
         fi-namn = SUBSTR(artbas.beskr,1,20).
         fi-namn:SCREEN-VALUE = fi-namn.
         fi-pris = 0.
         IF LENGTH(cSokEAN) = 13 AND CAN-DO("20,21,22",SUBSTR(cSokEAN,1,2)) THEN DO:
             CASE SUBSTR(cSokEAN,1,2):
                 WHEN "20" THEN
                     ASSIGN fi-pris = DECI(SUBSTR(cSokEAN,9,2)) + (DECI(SUBSTR(cSokEAN,11,2)) * .01).
                 WHEN "21" THEN
                     ASSIGN fi-pris = DECI(SUBSTR(cSokEAN,9,3)) + (DECI(SUBSTR(cSokEAN,12,1)) * .1).
                 WHEN "22" THEN
                     ASSIGN fi-pris = DECI(SUBSTR(cSokEAN,9,4)).
             END CASE.
         END.
         ELSE DO:
             FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
             IF AVAIL artpris THEN
                 fi-pris = artpris.pris[1].
         END.
         IF fi-pris > 0 THEN DO:
             fi-lyckade = fi-lyckade + 1.
             fi-pris:SCREEN-VALUE = STRING(fi-pris).
             cPris = STRING(fi-pris,">>>>9.99").
             cRet = SUBSTITUTE(cRet,FI-namn," ",cPris).
             cRet = "<data " + cId + ">" + cRet + "</data>".
         END.
         ELSE DO:
             cRet = SUBSTITUTE(cRet,"","      NO INFO","").
             cRet = "<data " + cId + ">" + cRet + "</data>".
         END.
     END.
     ELSE DO:
         cRet = SUBSTITUTE(cRet,"","      NO INFO","").
         cRet = "<data " + cId + ">" + cRet + "</data>".
     END.
 END.
 ELSE DO:
     cRet = SUBSTITUTE(cRet,"","      NO INFO","").
     cRet = "<data " + cId + ">" + cRet + "</data>".
 END.
 RUN PostMessage ("Item","new",cRet).
/*     <ESC>B0<ESC>$                            */
/*    <ESC>.0{name}<ETX>                        */
/*    {info}                                    */
/*    <ESC>B1<ESC>.8<#0x80> {price}<ETX><ESC>B0 */
 fi-antal:SCREEN-VALUE = STRING(fi-antal).
 fi-lyckade:SCREEN-VALUE = STRING(fi-lyckade).
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

