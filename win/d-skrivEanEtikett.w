&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_etikett NO-UNDO LIKE etikett
       FIELD individnr LIKE Individ.IndividNr
       .



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
DEFINE INPUT  PARAMETER cStrekkode AS CHARACTER  NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE wCL              AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLAYOUT  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSKRIVER AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iKampanjeId     LIKE Kampanjehode.KampanjeId  NO-UNDO.
DEFINE VARIABLE iBrukerButikk AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPkSdlbutNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cReturnValues AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

DEFINE TEMP-TABLE TT_StrekKoder
    FIELD iLopnr AS INTE
    FIELD cKode  AS CHAR
    FIELD iAntall AS INTE
    FIELD Vg        LIKE ArtBas.Vg
    FIELD LopNr     LIKE ArtBas.Lopnr
    FIELD Bongtekst LIKE ArtBas.Bongtekst
    FIELD Pris      AS DECIMAL
    FIELD Pris2     AS DECIMAL
    INDEX Lopnr iLopnr.

{etikettlogg.i &NEW=NEW}

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 Btn_OK CB-Printer Btn_Cancel ~
FI-Strekkode Btn_Help TG-Lager FI-AntEti FI-StartEti RS-Pristype 
&Scoped-Define DISPLAYED-OBJECTS CB-Printer FI-Strekkode TG-Alle TG-Lager ~
FI-AntEti FI-StartEti RS-Pristype CB-Butikker 

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

DEFINE VARIABLE CB-Butikker AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Printer AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntEti AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Antall etiketter" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StartEti AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Start etikett (1-24)" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Pristype AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Normalpris", 1,
"Aktuell pris", 2
     SIZE 23 BY 1.91 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35 BY 10.91.

DEFINE VARIABLE TG-Alle AS LOGICAL INITIAL NO 
     LABEL "Alle EAN/UPC" 
     VIEW-AS TOGGLE-BOX
     SIZE 18.8 BY .81 NO-UNDO.

DEFINE VARIABLE TG-Lager AS LOGICAL INITIAL NO 
     LABEL "Lager" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.33 COL 37
     CB-Printer AT ROW 1.62 COL 1.4 COLON-ALIGNED NO-LABEL
     Btn_Cancel AT ROW 2.52 COL 37
     FI-Strekkode AT ROW 2.81 COL 13.2 COLON-ALIGNED
     TG-Alle AT ROW 3.86 COL 15.2
     Btn_Help AT ROW 3.86 COL 37
     TG-Lager AT ROW 4.62 COL 15.2
     FI-AntEti AT ROW 5.62 COL 26.4 COLON-ALIGNED
     FI-StartEti AT ROW 6.67 COL 26.4 COLON-ALIGNED
     RS-Pristype AT ROW 8.14 COL 11 NO-LABEL
     CB-Butikker AT ROW 10.48 COL 9 COLON-ALIGNED
     RECT-51 AT ROW 1.29 COL 1.6
     SPACE(15.59) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Etikettparametre"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_etikett T "NEW SHARED" NO-UNDO SkoTex etikett
      ADDITIONAL-FIELDS:
          FIELD individnr LIKE Individ.IndividNr
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-Butikker IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-Strekkode:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR TOGGLE-BOX TG-Alle IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Etikettparametre */
DO:
  RUN SaveSettings.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Etikettparametre */
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
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF FI-AntEti:SENSITIVE THEN DO:
    IF NOT INT(FI-AntEti:SCREEN-VALUE) > 0 THEN DO:
        MESSAGE "Registrer antall"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-AntEti.
        RETURN NO-APPLY.
    END.
    IF INT(FI-StartEti:SCREEN-VALUE) < 1 OR INT(FI-StartEti:SCREEN-VALUE) > 24 THEN DO:
        MESSAGE "Gyldig verdi 1-24"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-StartEti.
        RETURN NO-APPLY.
    END.
  END.
  
  /* Henter butikk for RFID skrivere. */
  FIND SysPara WHERE 
    SysPara.SysHId = 5 AND 
    SysPara.SysGr = 21 AND 
    SysPara.ParaNr =  INT(CB-Printer:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE SysPara AND SysPara.Beskrivelse MATCHES '*RFID*' THEN
  HENT_BUTIKKNR: 
  DO:
      FIND Bruker NO-LOCK WHERE 
        Bruker.BrukerID = USERID('Skotex') NO-ERROR.
      IF AVAILABLE Bruker AND Bruker.ButikkNr <> 0 THEN 
      DO:
         ASSIGN 
            iPkSdlbutNr = Bruker.ButikkNr
            .
         LEAVE HENT_BUTIKKNR.
      END.
      ELSE DO:
          cReturnValues = ''.
          RUN JBoxDLookup.w ("Butiker;butik;ButNamn", 
                             "WHERE true",
                             INPUT-OUTPUT cReturnValues).
          IF cReturnValues NE "" THEN DO:
            ASSIGN iPkSdlbutNr = INT(ENTRY(1,cReturnValues,"|"))
                   .        
          END.
      END.
  END. /* HENT_BUTIKKNR - RFID skriver håndtering slutt. */ 
  
  RUN SkapaEtikettLogg.
  IF RETURN-VALUE <> "AVBRYT" THEN
      RUN x-etikettsend.w (INT(CB-Printer:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Printer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Printer Dialog-Frame
ON VALUE-CHANGED OF CB-Printer IN FRAME Dialog-Frame
DO:
  ASSIGN FI-StartEti:SENSITIVE = CAN-DO(cStartEtiPrinter,SELF:SCREEN-VALUE).
  IF FI-StartEti:SENSITIVE THEN
      APPLY "ENTRY" TO FI-StartEti.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Lager
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Lager Dialog-Frame
ON VALUE-CHANGED OF TG-Lager IN FRAME Dialog-Frame /* Lager */
DO:
      ASSIGN FI-AntEti:SENSITIVE = NOT SELF:CHECKED
             FI-AntEti:SCREEN-VALUE = "1"
             CB-Butikker:SCREEN-VALUE = ENTRY(2,CB-Butikker:LIST-ITEM-PAIRS)
             CB-Butikker:SENSITIVE = SELF:CHECKED.
      IF iBrukerButikk <> 0 AND CAN-DO(CB-Butikker:LIST-ITEM-PAIRS,STRING(iBrukerButikk)) THEN
          CB-Butikker:SCREEN-VALUE = STRING(iBrukerButikk) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 5 1 1 wCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = wCl NO-ERROR.
IF NOT AVAILABLE clButiker THEN
  DO:
    MESSAGE "Sentrallager er ikke lagt opp!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
    RETURN NO-APPLY.
  END.
FIND Butiker NO-LOCK WHERE
  Butiker.Butik = wCl NO-ERROR.
  
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF cStrekKode BEGINS "Kampanje" THEN DO:
      ASSIGN iKampanjeId = INT(ENTRY(2,cStrekKode,"=")).
  END.
  ELSE DO:
      FIND Strekkode WHERE StrekKode.Kode = cStrekkode NO-LOCK NO-ERROR.
      IF NOT AVAIL StrekKode THEN DO:
          MESSAGE "Finner ikke strekkode."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN.
      END.
      ELSE DO:
          ASSIGN FI-Strekkode = cStrekkode.
          FIND ArtBas WHERE ArtBas.Artikkelnr = strekkode.Artikkelnr NO-LOCK.
          FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
          
          FIND ArtPris NO-LOCK WHERE
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
              ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN
            FIND ArtPris NO-LOCK WHERE
              ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
              ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
      END.
  END.
  RUN GetLastPrinter.
  RUN InitCombo.
  ASSIGN CB-Butikker = ENTRY(2,CB-Butikker:LIST-ITEM-PAIRS).
  {lng.i}
  RUN enable_UI.
  IF iKampanjeId > 0 THEN DO:
      APPLY "VALUE-CHANGED" TO CB-Printer.
      ASSIGN TG-Lager:CHECKED = TRUE
             TG-Lager:SENSITIVE = FALSE.
      APPLY "VALUE-CHANGED" TO TG-Lager.
  END.
  ELSE DO:
      IF ENTRY(2,CB-Butikker:LIST-ITEM-PAIRS) = "" OR ArtBas.Lager = FALSE THEN
          ASSIGN TG-Lager:SENSITIVE = FALSE.
      IF CAN-FIND(FIRST Strekkode OF ArtBas WHERE StrekKode.KodeType = 1 AND
                        Strekkode.Kode <> cStrekkode) THEN
          ASSIGN TG-Alle:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
      APPLY "VALUE-CHANGED" TO CB-Printer.
      APPLY "ENTRY" TO FI-AntEti.
  END.
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
  DISPLAY CB-Printer FI-Strekkode TG-Alle TG-Lager FI-AntEti FI-StartEti 
          RS-Pristype CB-Butikker 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-51 Btn_OK CB-Printer Btn_Cancel FI-Strekkode Btn_Help TG-Lager 
         FI-AntEti FI-StartEti RS-Pristype 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLastPrinter Dialog-Frame 
PROCEDURE GetLastPrinter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME default-frame:
    RUN HentParametre IN wLibHandle ("ETIKETTER", "LAYOUT",  OUTPUT cLAYOUT).
    RUN HentParametre IN wLibHandle ("ETIKETTER", "SKRIVER", OUTPUT cSKRIVER).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lBrukerButOK AS LOGICAL     NO-UNDO.
    DEFINE BUFFER bSysPara FOR SysPara.
    FIND Bruker WHERE Bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
    IF AVAIL Bruker THEN
        ASSIGN iBrukerButikk = Bruker.ButikkNr.
    FOR EACH Butiker WHERE CAN-FIND(FIRST ArtLag OF Butiker): 
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                                                 Butiker.ButNamn + "," + STRING(Butiker.Butik).
        IF Butiker.butik = iBrukerButikk THEN
            lBrukerButOK = TRUE.
    END.
    IF lBrukerButOK = FALSE THEN
        ASSIGN iBrukerButikk = 0.
    ASSIGN CB-Butikker:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        IF cListItemPairs = "" THEN "," ELSE cListItemPairs.
    
    ASSIGN cListItemPairs = "".
    FOR EACH SysPara WHERE SysPara.SysHId = 5 AND 
                       SysPara.SysGr = 21 NO-LOCK:
        IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"AVAIL") THEN DO:
            FIND bSysPara WHERE bSysPara.SysHId = 5 AND 
                                bSysPara.SysGr  = 20 AND
                                bSysPara.ParaNr = SysPara.ParaNr NO-LOCK NO-ERROR.
            IF AVAIL bSysPara THEN DO:
                ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                       bSysPara.Parameter2 + "," + TRIM(STRING(bSysPara.ParaNr)).
                IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"START") THEN
                    ASSIGN cStartEtiPrinter = cStartEtiPrinter + (IF cStartEtiPrinter = "" THEN "" ELSE ",") + 
                       TRIM(STRING(bSysPara.ParaNr)).
            END.
        END.
    END.
    ASSIGN CB-Printer:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}= cListItemPairs.
    IF cSKRIVER <> ? AND CAN-DO(cListItemPairs,cSKRIVER) THEN
        CB-Printer = INT(cSKRIVER).
    ELSE
        CB-Printer = INT(ENTRY(2,cListItemPairs)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveSettings Dialog-Frame 
PROCEDURE SaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "LAYOUT", ENTRY(LOOKUP(CB-Printer:SCREEN-VALUE,cListItemPairs) - 1,CB-Printer:LIST-ITEM-PAIRS)).
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "SKRIVER", CB-Printer:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEtikettLogg Dialog-Frame 
PROCEDURE SkapaEtikettLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStrekkoderTilUtskrift AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAntallPerStrekKode    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount                 AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iTmpAnt                AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cInfoRad1              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad2              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad3              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad4              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLagerEan              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cLagerStr              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCOunt2                AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lTilbud                AS LOGICAL    NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF NOT iKampanjeId > 0 THEN
        lTilbud = IF RS-Pristype:SCREEN-VALUE = "1" THEN FALSE ELSE ArtPris.Tilbud.
    IF iKampanjeId > 0 THEN DO:
        ASSIGN iCount = 1. /* för att få in en extra etikket innan */
        FIND KampanjeHode WHERE KampanjeHode.KampanjeId = iKampanjeId NO-LOCK NO-ERROR.
        IF AVAIL KampanjeHode THEN DO:
            FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK:
                FIND ArtBas WHERE ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-LOCK NO-ERROR.
                IF AVAIL ArtBas THEN DO:
                    FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                                       ArtPris.ProfilNr   = KampanjeLinje.ProfilNr NO-LOCK NO-ERROR.
                    IF AVAIL Artpris THEN FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0:
                        FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
                        IF AVAIL StrKonv THEN DO:
                            ASSIGN iTmpAnt = 0.
                            FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                          ArtLag.Storl      = StrKonv.Storl AND
                                                          ArtLag.Butik      = INT(CB-Butikker:SCREEN-VALUE) AND
                                                          ArtLag.lagant     > 0:
                                ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
                            END.
                            IF iTmpAnt > 0 THEN DO:
                                CREATE TT_StrekKoder.
                                ASSIGN iCount                = iCount + 1 
                                       TT_StrekKoder.iLopnr  = iCount
                                       TT_StrekKoder.cKode   = StrekKode.Kode
                                       TT_StrekKoder.iAntall = iTmpAnt
                                       TT_StrekKoder.Vg      = ArtBas.Vg   
                                       TT_StrekKoder.LopNr   = ArtBas.Lopnr
                                       TT_StrekKoder.Bongtekst = ArtBas.Bongtekst                             
                                       TT_StrekKoder.Pris      = KampanjeLinje.Pris[2]
                                       TT_StrekKoder.Pris2     = IF KampanjeHode.NormalPris THEN 0 ELSE ArtPris.Pris[1].
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
    ELSE IF NOT TG-Alle:CHECKED THEN DO:
        IF TG-Lager:CHECKED THEN DO:
            IF StrekKode.Strkode = 0 THEN DO:
                MESSAGE "Strekkode ikke knyttet til størrelse"
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN "AVBRYT".
            END.
            FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
            IF NOT AVAIL StrKonv THEN
                LEAVE.
            FIND butiker WHERE butiker.butik = INT(CB-Butikker:SCREEN-VALUE) NO-LOCK NO-ERROR.
            FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
            IF NOT AVAILABLE ArtPris THEN
              FIND ArtPris NO-LOCK WHERE
                ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
            lTilbud = IF RS-Pristype:SCREEN-VALUE = "1" THEN FALSE ELSE ArtPris.Tilbud.
     
            FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                          ArtLag.Storl      = StrKonv.Storl AND
                                          ArtLag.Butik      = INT(CB-Butikker:SCREEN-VALUE) AND
                                          ArtLag.lagant     > 0:
                ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
            END.
            IF iTmpAnt > 0 THEN DO:
                CREATE TT_StrekKoder.
                ASSIGN TT_StrekKoder.iLopnr  = 1
                       TT_StrekKoder.cKode   = cStrekkode
                       TT_StrekKoder.iAntall = iTmpAnt
                       TT_StrekKoder.Vg      = ArtBas.Vg   
                       TT_StrekKoder.LopNr   = ArtBas.Lopnr
                       TT_StrekKoder.Bongtekst = ArtBas.Bongtekst
                       TT_StrekKoder.Pris      = ArtPris.Pris[IF lTilbud THEN 2 ELSE 1]
                       TT_StrekKoder.Pris2     = IF lTilbud THEN ArtPris.Pris[1] ELSE 0.
            END.
        END.
        ELSE DO:
            CREATE TT_StrekKoder.
            ASSIGN TT_StrekKoder.iLopnr  = 1
                   TT_StrekKoder.cKode   = cStrekkode
                   TT_StrekKoder.iAntall = INPUT FI-AntEti
                   TT_StrekKoder.Vg      = ArtBas.Vg   
                   TT_StrekKoder.LopNr   = ArtBas.Lopnr
                   TT_StrekKoder.Bongtekst = ArtBas.Bongtekst                             
                   TT_StrekKoder.Pris      = ArtPris.Pris[IF lTilbud THEN 2 ELSE 1]
                   TT_StrekKoder.Pris2     = IF lTilbud THEN ArtPris.Pris[1] ELSE 0.
        END.
    END.
    ELSE DO:
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0 AND 
                                               NOT StrekKode.Kode BEGINS "02":
            IF NOT CAN-DO(cLagerStr,STRING(StrekKode.StrKode)) THEN DO:
                ASSIGN cLagerEan = cLagerEan + (IF cLagerEan <> "" THEN "," ELSE "") + StrekKode.Kode
                       cLagerStr = cLagerStr + (IF cLagerStr <> "" THEN "," ELSE "") + STRING(StrekKode.StrKode).
            END.
        END.
        FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0 AND 
                                               StrekKode.Kode BEGINS "02":
            IF NOT CAN-DO(cLagerStr,STRING(StrekKode.StrKode)) THEN DO:
                ASSIGN cLagerEan = cLagerEan + (IF cLagerEan <> "" THEN "," ELSE "") + StrekKode.Kode
                       cLagerStr = cLagerStr + (IF cLagerStr <> "" THEN "," ELSE "") + STRING(StrekKode.StrKode).
            END.
        END.
        DO iCount2 = 1 TO NUM-ENTRIES(cLagerEan):
            FIND StrekKode WHERE StrekKode.Kode = ENTRY(iCount2,cLagerEan) NO-LOCK.
/*         FOR EACH StrekKode OF ArtBas NO-LOCK WHERE StrekKode.KodeType > 0: */
/*             FOR EACH StrekKode OF ArtBas WHERE StrekKode.KodeType = 1: */
            ASSIGN iTmpAnt = 0.
            IF TG-Lager:CHECKED THEN DO:
                FIND butiker WHERE butiker.butik = INT(CB-Butikker:SCREEN-VALUE) NO-LOCK NO-ERROR.
                FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                IF NOT AVAILABLE ArtPris THEN
                  FIND ArtPris NO-LOCK WHERE
                    ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                    ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                lTilbud = IF RS-Pristype:SCREEN-VALUE = "1" THEN FALSE ELSE ArtPris.Tilbud.
                FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN DO:
                    FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                  ArtLag.Storl      = StrKonv.Storl AND
                                                  ArtLag.Butik      = INT(CB-Butikker:SCREEN-VALUE) AND
                                                  ArtLag.lagant     > 0:
                        ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
                    END.
                END.
            END.
            ELSE 
                ASSIGN iTmpAnt = INT(FI-AntEti:SCREEN-VALUE).
            IF iTmpAnt > 0 THEN DO:
                CREATE TT_StrekKoder.
                ASSIGN iCount                = iCount + 1 
                       TT_StrekKoder.iLopnr  = iCount
                       TT_StrekKoder.cKode   = StrekKode.Kode
                       TT_StrekKoder.iAntall = iTmpAnt
                       TT_StrekKoder.Vg      = ArtBas.Vg   
                       TT_StrekKoder.LopNr   = ArtBas.Lopnr
                       TT_StrekKoder.Bongtekst = ArtBas.Bongtekst                             
                       TT_StrekKoder.Pris      = ArtPris.Pris[IF lTilbud THEN 2 ELSE 1]
                       TT_StrekKoder.Pris2     = IF lTilbud THEN ArtPris.Pris[1] ELSE 0.
            END.
        END.
    END.

    IF NOT CAN-FIND(FIRST TT_StrekKoder)THEN DO:
        MESSAGE "Inget etiketter til utskrift"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "AVBRYT".
    END.
    IF INT(FI-StartEti:SCREEN-VALUE) > 1 THEN DO:
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik = 0
               EtikettLogg.SeqNr = 0
               EtikettLogg.Storl  = "STARTETIKETT"
               EtikettLogg.Ant = INT(FI-StartEti:SCREEN-VALUE).
        /* Ant avänds för att ange startetikett */
    END.
    IF iKampanjeId > 0 AND AVAIL Kampanjehode THEN DO:
        FIND butiker WHERE butiker.butik = INT(CB-Butikker:SCREEN-VALUE) NO-LOCK NO-ERROR.
        ASSIGN cInfoRad1 = "KAMPANJE " + STRING(iKampanjeId)
               cInfoRad2 = IF AVAIL kampanjehode THEN STRING(KampanjeHode.StartDato) + " - " +
                             STRING(KampanjeHode.SluttDato) ELSE ""
               cInfoRad3 = IF AVAIL Butiker THEN Butiker.butnamn ELSE ""
               cInfoRad4 = "SLUTT".
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = iPkSdlbutNr
          EtikettLogg.Vg        = 0   
          EtikettLogg.LopNr     = 0
          EtikettLogg.Ant       = 0
          EtikettLogg.Storl     = "INFO"
          EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
          EtikettLogg.Pris      = 0
          EtikettLogg.Pris2     = 0
          EtikettLogg.SeqNr     = 1.
    END.
    FOR EACH TT_StrekKoder:
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = iPkSdlbutNr
          EtikettLogg.Vg        = TT_StrekKoder.Vg   
          EtikettLogg.LopNr     = TT_StrekKoder.LopNr
          EtikettLogg.Ant       = TT_StrekKoder.iAntall
          EtikettLogg.Storl     = TT_StrekKoder.cKode
          EtikettLogg.Bongtekst = TT_StrekKoder.Bongtekst
          EtikettLogg.Pris      = TT_StrekKoder.Pris
          EtikettLogg.Pris2     = TT_StrekKoder.Pris2
          EtikettLogg.SeqNr     = TT_StrekKoder.iLopnr.
    END.
    IF iKampanjeId > 0 AND AVAIL Kampanjehode THEN DO:
        ASSIGN cInfoRad4 = "START".
        CREATE EtikettLogg.
        ASSIGN
          EtikettLogg.Butik     = iPkSdlbutNr
          EtikettLogg.Vg        = 0   
          EtikettLogg.LopNr     = 0
          EtikettLogg.Ant       = 0
          EtikettLogg.Storl     = "INFO"
          EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
          EtikettLogg.Pris      = 0
          EtikettLogg.Pris2     = 0
          EtikettLogg.SeqNr     = 99000.
    END.
/*     DO iCount = 1 TO NUM-ENTRIES(cStrekkoderTilUtskrift):                                                                                */
/*     create EtikettLogg.                                                                                                                  */
/*     assign                                                                                                                               */
/*       EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */                                                          */
/*       EtikettLogg.Vg        = ArtBas.Vg                                                                                                  */
/*       EtikettLogg.LopNr     = ArtBas.Lopnr                                                                                               */
/*       EtikettLogg.Ant       = INT(FI-AntEti:SCREEN-VALUE)                                                                                */
/*       EtikettLogg.Storl     = (IF iCount = 1 THEN FI-StartEti:SCREEN-VALUE + "," ELSE "") + STRING(ENTRY(iCount,cStrekkoderTilUtskrift)) */
/*       EtikettLogg.Bongtekst = ArtBas.Bongtekst                                                                                           */
/*       EtikettLogg.Pris      = ArtPris.Pris[if ArtPris.Tilbud                                                                             */
/*                            then 2                                                                                                        */
/*                            else 1]                                                                                                       */
/*       EtikettLogg.SeqNr     = iCount.                                                                                                    */
/*     END.                                                                                                                                 */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEtikettLoggOrg Dialog-Frame 
PROCEDURE SkapaEtikettLoggOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cStrekkoderTilUtskrift AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAntallPerStrekKode    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount                 AS INTEGER   NO-UNDO.
  DEFINE VARIABLE iTmpAnt                AS INTEGER   NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
        
    IF NOT TG-Alle:CHECKED THEN DO:
        IF TG-Lager:CHECKED THEN DO:
            FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
            IF NOT AVAIL StrKonv THEN
                LEAVE.
            FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                          ArtLag.Storl      = StrKonv.Storl AND
                                          ArtLag.Butik      = INT(CB-Butikker:SCREEN-VALUE) AND
                                          ArtLag.lagant     > 0:
                ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
            END.
            IF iTmpAnt > 0 THEN
                ASSIGN cStrekkoderTilUtskrift = cStrekkode
                       cAntallPerStrekKode    = STRING(iTmpAnt).
        END.
        ELSE
            ASSIGN cStrekkoderTilUtskrift = cStrekkode.
    END.
    ELSE DO:
        FOR EACH StrekKode OF ArtBas WHERE StrekKode.KodeType > 0:
/*             FOR EACH StrekKode OF ArtBas WHERE StrekKode.KodeType = 1: */
            IF TG-Lager:CHECKED THEN DO:
                FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
                IF AVAIL StrKonv THEN DO:
                    ASSIGN iTmpAnt = 0.
                    FOR EACH ArtLag NO-LOCK WHERE ArtLag.ArtikkelNr = ArtBas.ArtikkelNr AND
                                                  ArtLag.Storl      = StrKonv.Storl AND
                                                  ArtLag.Butik      = INT(CB-Butikker:SCREEN-VALUE) AND
                                                  ArtLag.lagant     > 0:
                        ASSIGN iTmpAnt = iTmpAnt + ArtLag.LagAnt.
                    END.
                    IF iTmpAnt > 0 THEN
                        ASSIGN cStrekkoderTilUtskrift = cStrekkoderTilUtskrift +
                            (IF cStrekkoderTilUtskrift = "" THEN "" ELSE ",")  + StrekKode.Kode
                               cAntallPerStrekKode    = cAntallPerStrekKode + 
                                   (IF cAntallPerStrekKode = "" THEN "" ELSE ",") + STRING(iTmpAnt).
                END.
            END.
            ELSE 
            ASSIGN cStrekkoderTilUtskrift = cStrekkoderTilUtskrift +
                (IF cStrekkoderTilUtskrift = "" THEN "" ELSE ",") +
                StrekKode.Kode.
        END.
    END.
    IF cStrekkoderTilUtskrift = "" THEN DO:
        MESSAGE "Inget etiketter til utskrift"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "AVBRYT".
    END.
    IF INT(FI-StartEti:SCREEN-VALUE) > 1 THEN DO:
        CREATE EtikettLogg.
        ASSIGN EtikettLogg.Butik = 0
               EtikettLogg.SeqNr = 0
               EtikettLogg.Storl  = "STARTETIKETT"
               EtikettLogg.Ant = INT(FI-StartEti:SCREEN-VALUE).
        /* Ant avänds för att ange startetikett */
    END.
    DO iCount = 1 TO NUM-ENTRIES(cStrekkoderTilUtskrift):
    CREATE EtikettLogg.
    ASSIGN
      EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */
      EtikettLogg.Vg        = ArtBas.Vg
      EtikettLogg.LopNr     = ArtBas.Lopnr
      EtikettLogg.Ant       = IF TG-Lager:CHECKED THEN INT(ENTRY(iCount,cAntallPerStrekKode)) ELSE INT(FI-AntEti:SCREEN-VALUE)
      EtikettLogg.Storl     = STRING(ENTRY(iCount,cStrekkoderTilUtskrift))
      EtikettLogg.Bongtekst = ArtBas.Bongtekst
      EtikettLogg.Pris      = ArtPris.Pris[IF ArtPris.Tilbud
                           THEN 2
                           ELSE 1]
      EtikettLogg.SeqNr     = iCount.
    END.
/*     DO iCount = 1 TO NUM-ENTRIES(cStrekkoderTilUtskrift):                                                                                */
/*     create EtikettLogg.                                                                                                                  */
/*     assign                                                                                                                               */
/*       EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */                                                          */
/*       EtikettLogg.Vg        = ArtBas.Vg                                                                                                  */
/*       EtikettLogg.LopNr     = ArtBas.Lopnr                                                                                               */
/*       EtikettLogg.Ant       = INT(FI-AntEti:SCREEN-VALUE)                                                                                */
/*       EtikettLogg.Storl     = (IF iCount = 1 THEN FI-StartEti:SCREEN-VALUE + "," ELSE "") + STRING(ENTRY(iCount,cStrekkoderTilUtskrift)) */
/*       EtikettLogg.Bongtekst = ArtBas.Bongtekst                                                                                           */
/*       EtikettLogg.Pris      = ArtPris.Pris[if ArtPris.Tilbud                                                                             */
/*                            then 2                                                                                                        */
/*                            else 1]                                                                                                       */
/*       EtikettLogg.SeqNr     = iCount.                                                                                                    */
/*     END.                                                                                                                                 */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

