&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEF INPUT PARAMETER wCurrent-Window    AS HANDLE NO-UNDO.
DEF INPUT PARAMETER wParentHandle      AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cEmptyField AS CHARACTER LABEL "" NO-UNDO.
DEFINE VARIABLE cStorl      LIKE StrKonv.Storl    NO-UNDO.
DEFINE VARIABLE hEtikettVindu  AS HANDLE     NO-UNDO.
DEFINE VARIABLE hFlyttEanVindu AS HANDLE     NO-UNDO.
DEFINE VARIABLE cHKinst        AS CHARACTER  NO-UNDO.
DEFINE BUFFER bStrekKode FOR StrekKode.

DEFINE VARIABLE hBestnr AS HANDLE     NO-UNDO.
DEF VAR bOk AS LOG NO-UNDO.
DEFINE VARIABLE cTidskrift AS CHARACTER NO-UNDO.
DEF BUFFER bufArtBas FOR ArtBas.

DEF BUFFER b2Strekkode FOR Strekkode.
DEF BUFFER bArtBas FOR ArtBAs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-Strekkode
&Scoped-define BROWSE-NAME BROWSE-Strekkode

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES StrekKode

/* Definitions for BROWSE BROWSE-Strekkode                              */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Strekkode StrekKode.Kode ~
StrekKode.KodeType StrekKode.HovedNr StrekKode.StrKode getStorl() @ cStorl ~
StrekKode.IKasse StrekKode.Bestillingsnummer StrekKode.ERPNr ~
StrekKode.RegistrertDato StrekKode.EDato StrekKode.BrukerID ~
"" @ cEmptyField 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Strekkode ~
StrekKode.Bestillingsnummer StrekKode.ERPNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Strekkode StrekKode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Strekkode StrekKode
&Scoped-define QUERY-STRING-BROWSE-Strekkode FOR EACH StrekKode ~
      WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-Strekkode OPEN QUERY BROWSE-Strekkode FOR EACH StrekKode ~
      WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Strekkode StrekKode
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Strekkode StrekKode


/* Definitions for FRAME FRAME-Strekkode                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Strekkode ~
    ~{&OPEN-QUERY-BROWSE-Strekkode}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-NyEan BROWSE-Strekkode BUTTON-Endre ~
BUTTON-Slett BUTTON-HovedNr BUTTON-IKasse B-SlettUbrukte B-GenererEAN ~
BUTTON-EtiBatch BUTTON-EanEti B-Plakat B-VpiEAN B-Slett02Dubletter ~
B-FlyttEan 
&Scoped-Define DISPLAYED-OBJECTS FI-ArtIkasseTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-FlyttEan 
     LABEL "Flytt Ean" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-GenererEAN 
     LABEL "&Generer EAN" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Plakat 
     LABEL "Plakatutsk&rift" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-Slett02Dubletter 
     LABEL "Slett 02-dubletter" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-SlettUbrukte  NO-FOCUS
     LABEL "Slett ubrukte 02" 
     SIZE 22 BY 1.14.

DEFINE BUTTON B-VpiEAN 
     LABEL "Hent EAN &fra VPI" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-EanEti 
     LABEL "Etikett EAN/&UPC" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Endre 
     LABEL "&Endre..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-EtiBatch 
     LABEL "Sa&mleetiketter" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-HovedNr 
     LABEL "&Hovednr" 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-IKasse 
     LABEL "&I kasse..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-NyEan 
     LABEL "N&y kode..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON BUTTON-Slett 
     LABEL "Sle&tt" 
     SIZE 22 BY 1.14.

DEFINE VARIABLE FI-ArtIkasseTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Artikkel ikke i kasse" 
      VIEW-AS TEXT 
     SIZE 49 BY .91
     FGCOLOR 12 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Strekkode FOR 
      StrekKode SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Strekkode C-Win _STRUCTURED
  QUERY BROWSE-Strekkode NO-LOCK DISPLAY
      StrekKode.Kode FORMAT "X(20)":U
      StrekKode.KodeType FORMAT ">9":U
      StrekKode.HovedNr FORMAT "*/":U COLUMN-FONT 8
      StrekKode.StrKode FORMAT ">>>9":U
      getStorl() @ cStorl
      StrekKode.IKasse FORMAT "J/N":U
      StrekKode.Bestillingsnummer COLUMN-LABEL "Bestillingsnr" FORMAT "X(25)":U
      StrekKode.ERPNr FORMAT "X(20)":U
      StrekKode.RegistrertDato COLUMN-LABEL "Opprettet" FORMAT "99/99/9999":U
      StrekKode.EDato FORMAT "99/99/9999":U
      StrekKode.BrukerID FORMAT "X(10)":U
      "" @ cEmptyField
  ENABLE
      StrekKode.Bestillingsnummer HELP "Merknad"
      StrekKode.ERPNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 156.2 BY 19.62 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Strekkode
     BUTTON-NyEan AT ROW 3.38 COL 168
     BROWSE-Strekkode AT ROW 3.48 COL 1.8
     BUTTON-Endre AT ROW 4.62 COL 168
     BUTTON-Slett AT ROW 5.86 COL 168
     BUTTON-HovedNr AT ROW 7.1 COL 168
     BUTTON-IKasse AT ROW 8.57 COL 168
     B-SlettUbrukte AT ROW 18.38 COL 168 NO-TAB-STOP 
     B-GenererEAN AT ROW 9.81 COL 168
     BUTTON-EtiBatch AT ROW 11.29 COL 168
     BUTTON-EanEti AT ROW 12.52 COL 168
     B-Plakat AT ROW 13.91 COL 168
     B-VpiEAN AT ROW 15.62 COL 168
     B-Slett02Dubletter AT ROW 17.14 COL 168
     B-FlyttEan AT ROW 19.71 COL 168
     FI-ArtIkasseTxt AT ROW 2.1 COL 2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 9.86
         SIZE 207.8 BY 23.1.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 32.29
         WIDTH              = 208
         MAX-HEIGHT         = 32.29
         MAX-WIDTH          = 208
         VIRTUAL-HEIGHT     = 32.29
         VIRTUAL-WIDTH      = 208
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
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-Strekkode
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BROWSE-Strekkode BUTTON-NyEan FRAME-Strekkode */
/* SETTINGS FOR FILL-IN FI-ArtIkasseTxt IN FRAME FRAME-Strekkode
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       FI-ArtIkasseTxt:HIDDEN IN FRAME FRAME-Strekkode           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Strekkode
/* Query rebuild information for BROWSE BROWSE-Strekkode
     _TblList          = "SkoTex.StrekKode"
     _Options          = "NO-LOCK"
     _Where[1]         = "SkoTex.StrekKode.ArtikkelNr = ArtBas.ArtikkelNr"
     _FldNameList[1]   = SkoTex.StrekKode.Kode
     _FldNameList[2]   = SkoTex.StrekKode.KodeType
     _FldNameList[3]   > SkoTex.StrekKode.HovedNr
"StrekKode.HovedNr" ? "*~~/" "logical" ? ? 8 ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = SkoTex.StrekKode.StrKode
     _FldNameList[5]   > "_<CALC>"
"getStorl() @ cStorl" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > SkoTex.StrekKode.IKasse
"StrekKode.IKasse" ? "J/N" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > SkoTex.StrekKode.Bestillingsnummer
"StrekKode.Bestillingsnummer" "Bestillingsnr" ? "character" ? ? ? ? ? ? yes "Merknad" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > SkoTex.StrekKode.ERPNr
"StrekKode.ERPNr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > SkoTex.StrekKode.RegistrertDato
"StrekKode.RegistrertDato" "Opprettet" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = SkoTex.StrekKode.EDato
     _FldNameList[11]   = SkoTex.StrekKode.BrukerID
     _FldNameList[12]   > "_<CALC>"
""""" @ cEmptyField" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Strekkode */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Strekkode
/* Query rebuild information for FRAME FRAME-Strekkode
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Strekkode */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FlyttEan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FlyttEan C-Win
ON CHOOSE OF B-FlyttEan IN FRAME FRAME-Strekkode /* Flytt Ean */
DO:
  /*
  IF StrekKode.Kode BEGINS "02" OR LENGTH(StrekKode.kode) <> 13 THEN
      RETURN NO-APPLY.
  */
  IF NOT VALID-HANDLE(hFlyttEanVindu) THEN
      RUN w-Tmpflyttean.w PERSISTENT SET hFlyttEanVindu (C-Win,wParentHandle).
  IF VALID-HANDLE(hFlyttEanVindu) THEN
      RUN NyEan IN hFlyttEanVindu (StrekKode.artikkelnr,StrekKode.Kode,Strekkode.StrKode,getStorl()).
  /*   RUN ButtonEnaDis. */
  ASSIGN CURRENT-WINDOW                = wCurrent-Window
         THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.
  APPLY "ENTRY" TO {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-GenererEAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-GenererEAN C-Win
ON CHOOSE OF B-GenererEAN IN FRAME FRAME-Strekkode /* Generer EAN */
DO:
    DEF VAR cStorrelser AS CHARACTER  NO-UNDO.
    DEF VAR iEANNrType  AS INT        NO-UNDO.
    DEF VAR cKjedeavtale AS CHAR NO-UNDO.

    FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE LevBas AND LevBas.Kjedeavtale THEN
    DO:
      {syspara.i 2 4 26 cKjedeavtale}
       IF CAN-DO('1,j,y,ja,yes,true',cKjedeavtale) THEN
       DO:
           MESSAGE 
             "Ikke mulig å generere EAN. Originale EANkoder må registreres."
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN NO-APPLY.
       END.
    END.

    iEANNrType = 1.
    IF NOT CAN-FIND(StrType OF ArtBas) THEN DO:
        MESSAGE "Ugyldig størrelsetype"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE IF ArtBas.StrTypeId > 2 THEN DO:
        RUN d-strTilEan.w (ArtBas.StrTypeId,OUTPUT cStorrelser, OUTPUT iEANNrType).
        IF cStorrelser = "" THEN
            RETURN NO-APPLY.
            
    END.
    /*
    ELSE IF ArtBas.Artikkelnr = dec(ArtBas.Vg) THEN DO:
        MESSAGE 
          "EAN-koder genereres ikke for PLU-artikkler."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    */
    RUN genStrekKode.p (ArtBas.ArtikkelNr,iEANNrType,cStorrelser).

    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    RUN ButtonEnaDis.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Plakat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Plakat C-Win
ON CHOOSE OF B-Plakat IN FRAME FRAME-Strekkode /* Plakatutskrift */
DO:
    RUN wSkrivUtPlakat.w (StrekKode.Artikkelnr).
    /*   RUN ButtonEnaDis. */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Slett02Dubletter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Slett02Dubletter C-Win
ON CHOOSE OF B-Slett02Dubletter IN FRAME FRAME-Strekkode /* Slett 02-dubletter */
DO:
    DEFINE VARIABLE cStrekkoder AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE BUFFER b1Strekkode FOR Strekkode.
    DEFINE BUFFER b2Strekkode FOR Strekkode.

    FOR EACH b1Strekkode OF ArtBas WHERE NOT b1Strekkode.kode BEGINS "02" NO-LOCK:
        FIND b2Strekkode OF ArtBas WHERE b2Strekkode.kode BEGINS "02" AND 
                                        b2Strekkode.strkode = b1Strekkode.strkode NO-LOCK NO-ERROR.
        IF AVAIL b2Strekkode AND NOT CAN-DO(cStrekkoder,b2Strekkode.kode) THEN
            ASSIGN cStrekkoder = cStrekkoder + (IF cStrekkoder <> "" THEN "," ELSE "") + b2Strekkode.kode.
    END.
    IF cStrekkoder <> "" THEN DO:
        MESSAGE "Ønsker du å slette disse EAN-kodene?" SKIP
            REPLACE(cStrekkoder,",",CHR(10))
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lOK AS LOGICAL.
        IF lOK THEN DO iCount = 1 TO NUM-ENTRIES(cStrekkoder):
            FIND b1Strekkode OF artbas WHERE b1Strekkode.kode = ENTRY(iCount,cStrekkoder) NO-ERROR.
            IF AVAIL b1Strekkode THEN
                DELETE b1Strekkode.
        END.
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    RUN ButtonEnaDis.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SlettUbrukte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SlettUbrukte C-Win
ON CHOOSE OF B-SlettUbrukte IN FRAME FRAME-Strekkode /* Slett ubrukte 02 */
OR F10 OF B-Slettubrukte
DO:
   DEFINE VARIABLE cStrekkodeRowIdList AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cStrekkodeIdList    AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE bOK                 AS LOGICAL    NO-UNDO.
   DEFINE VARIABLE cCanDoList          AS CHARACTER   NO-UNDO.

   FOR EACH strekkode OF artbas NO-LOCK WHERE
       NOT CAN-FIND(FIRST artlag WHERE artlag.artikkelnr = strekkode.artikkelnr AND
                    artlag.strkode = strekkode.strkode):
       IF LENGTH(StrekKode.Kode) = 13 AND SUBSTRING(StrekKode.Kode,1,2) = '02' THEN 
         cCanDoList = cCanDoList + (IF cCanDoList <> "" THEN "," ELSE "") + strekkode.kode.
   END.
    RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                        "Strekkode;Kode;!Artikkelnr",
                        "where strekkode.artikkelnr = '" + string(artbas.artikkelnr) + "'" 
                         + " AND CAN-DO('" + cCanDoList + "',kode)" ,
                        INPUT-OUTPUT cStrekkodeRowIdList,
                        "Kode",
                        INPUT-OUTPUT cStrekkodeIdList,
                        "","",
                        OUTPUT bOK).
    IF bOK THEN DO:
        RUN SlettUbrukte (cStrekkodeIdList).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VpiEAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VpiEAN C-Win
ON CHOOSE OF B-VpiEAN IN FRAME FRAME-Strekkode /* Hent EAN fra VPI */
DO:
    DEFINE VARIABLE cStorrelser AS CHARACTER  NO-UNDO.
    IF NOT CAN-FIND(StrType OF ArtBas) THEN DO:
        MESSAGE "Ugyldig størrelsetype"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
/*     ELSE IF INT(ArtBas.Artikkelnr) = ArtBas.Vg THEN DO: */
/*         MESSAGE                                         */
/*         "EAN-koder genereres ikke for PLU-artikkler."   */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.          */
/*         RETURN NO-APPLY.                                */
/*     END.                                                */
    DO:
        IF cHKinst = "no" THEN
            RUN genStrekKode.p (ArtBas.ArtikkelNr,1,"HKVPI").
        ELSE
            RUN genStrekKode.p (ArtBas.ArtikkelNr,1,"HKVPI_HKINST").
/*         IF RETURN-VALUE = "FEILSTRTYPE" THEN                                       */
/*             MESSAGE "VPI-registeret har forskjellig størrelsestype." SKIP          */
/*                     "Ønsker du å hente"                                            */
/*                 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lHent AS LOGICAL. */
/*         IF lHent THEN                                                              */
/*             RUN genStrekKode.p (ArtBas.ArtikkelNr,1,"HKVPI" + CHR(1) + "TRUE").    */
    END.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
       APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
    RUN ButtonEnaDis.
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Strekkode
&Scoped-define SELF-NAME BROWSE-Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Strekkode C-Win
ON DEFAULT-ACTION OF BROWSE-Strekkode IN FRAME FRAME-Strekkode
DO:
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
      BROWSE {&BROWSE-NAME}:REFRESH().

  IF BUTTON-Endre:SENSITIVE THEN
      APPLY "CHOOSE" TO BUTTON-Endre.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Strekkode C-Win
ON ROW-DISPLAY OF BROWSE-Strekkode IN FRAME FRAME-Strekkode
DO:
    DEF BUFFER bufStrKonv FOR StrKonv.

/*  4/1-07 ken1   IF AVAIL StrType AND StrekKode.KodeType = 1 AND NOT StrekKode.Kode BEGINS "02" THEN */
    IF AVAIL StrType AND StrekKode.KodeType = 1 AND StrekKode.StrKode  > 0 THEN
    DO:
        FIND FIRST bufStrKonv NO-LOCK WHERE
            bufStrKonv.StrKode = Strekkode.StrKode NO-ERROR.
        IF AVAILABLE bufStrKonv THEN
        ASSIGN StrekKode.StrKode:BGCOLOR IN BROWSE {&BROWSE-NAME} = 
                 IF NOT CAN-FIND(FIRST StrTStr WHERE StrTStr.StrTypeId = StrType.StrTypeId AND 
                                                     StrTStr.SoStorl   = bufStrKonv.Storl) 
                   THEN 12 
                   ELSE ?.
        ELSE ASSIGN StrekKode.StrKode:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12.
    END.
    ELSE ASSIGN StrekKode.StrKode:BGCOLOR IN BROWSE {&BROWSE-NAME} = 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Strekkode C-Win
ON VALUE-CHANGED OF BROWSE-Strekkode IN FRAME FRAME-Strekkode
DO:
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
      BROWSE {&BROWSE-NAME}:REFRESH().
  RUN ButtonEnaDis.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME StrekKode.Bestillingsnummer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StrekKode.Bestillingsnummer BROWSE-Strekkode _BROWSE-COLUMN C-Win
ON LEAVE OF StrekKode.Bestillingsnummer IN BROWSE BROWSE-Strekkode /* Bestillingsnr */
DO:
  FIND bStrekkode NO-LOCK WHERE
    bStrekkode.Kode = Strekkode.Kode:screen-value IN BROWSE BROWSE-STrekkode NO-ERROR.
  IF AVAILABLE bStrekkode AND Strekkode.ERPNr:screen-value IN BROWSE BROWSE-STrekkode = '' THEN 
    DISPLAY bStrekkode.ERPNr @ Strekkode.ERPNr WITH BROWSE BROWSE-STrekkode.

  IF AVAILABLE bStrekkode AND 
      Strekkode.Bestillingsnummer:screen-value IN BROWSE BROWSE-STrekkode <> ''  AND
      bStrekkode.EDato = TODAY AND 
      (TIME - bSTrekkode.ETid) <= 3
      THEN
  BEST_NR_SJEKK:
  DO:
      FOR EACH b2Strekkode NO-LOCK WHERE
        b2Strekkode.Bestillingsnummer = Strekkode.Bestillingsnummer:screen-value IN BROWSE BROWSE-STrekkode AND
        RECID(b2Strekkode) <> RECID(bStrekkode) AND 
        b2Strekkode.ArtikkelNr <> bStrekkode.ArtikkelNr:
        FIND bArtBas OF b2Strekkode NO-LOCK NO-ERROR.

        IF AVAILABLE bArtBas THEN
        DO:
          IF bArtBas.LevNr = ArtBas.LevNr AND bArtBas.ArtikkelNr <> bStrekkode.ArtikkelNr THEN
          DO:
              MESSAGE 'Det finnes allerede en artikkel/strekkode med dette bestillingsnummeret på denne leverandør/produsent.' skip
                      'Artikkel: ' bArtBas.ArtikkelNr bArtBas.LevKod bArtBAs.Beskr bArtBas.LevFargKod  
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              /*Strekkode.Bestillingsnummer:screen-value IN BROWSE BROWSE-STrekkode = ''.*/
              LEAVE BEST_NR_SJEKK.
          END.
        END.
      END.
  END. /* BEST_NR_SJEKK */



END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EanEti
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EanEti C-Win
ON CHOOSE OF BUTTON-EanEti IN FRAME FRAME-Strekkode /* Etikett EAN/UPC */
DO:
    IF AVAIL ArtBas AND ArtBas.IndividType <> 0 THEN DO:
        MESSAGE "Artikkelen er registrert som INDIVID." SKIP
                "Gå til arkfane 'Individ' for utskrift av strekkode."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    RUN d-skrivEanEtikett.w (StrekKode.Kode).
    /*   RUN ButtonEnaDis. */
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Endre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Endre C-Win
ON CHOOSE OF BUTTON-Endre IN FRAME FRAME-Strekkode /* Endre... */
DO:
  RUN d-strekkode.w (?,ArtBas.ArtikkelNr,ArtBas.StrTypeId,StrekKode.Kode,StrekKode.KodeType).
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
      BROWSE {&BROWSE-NAME}:REFRESH().
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EtiBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EtiBatch C-Win
ON CHOOSE OF BUTTON-EtiBatch IN FRAME FRAME-Strekkode /* Samleetiketter */
DO:
  DEFINE VARIABLE iAnt AS INTEGER    NO-UNDO.
  IF AVAIL ArtBas AND ArtBas.IndividType <> 0 THEN DO:
      MESSAGE "Artikkelen er registrert som INDIVID." SKIP
              "Gå til arkfane 'Individ' for utskrift av strekkode."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  RUN d-AntalEti.w (OUTPUT iAnt).
  IF iAnt = 0 THEN
      RETURN NO-APPLY.
  IF NOT VALID-HANDLE(hEtikettVindu) THEN
      RUN w-TmpEtikett.w PERSISTENT SET hEtikettVindu (C-Win).
  IF VALID-HANDLE(hEtikettVindu) THEN
      RUN NyEtikett IN hEtikettVindu (StrekKode.Kode,iAnt,0).
  /*   RUN ButtonEnaDis. */
  ASSIGN CURRENT-WINDOW                = wCurrent-Window
         THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.
  APPLY "ENTRY" TO {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-HovedNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-HovedNr C-Win
ON CHOOSE OF BUTTON-HovedNr IN FRAME FRAME-Strekkode /* Hovednr */
DO:
  DEFINE BUFFER bStrekKode FOR StrekKode.

  MESSAGE "Bytte av hovednr for størrelse?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
  IF lChoice THEN 
  DO TRANSACTION:
      BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
      FIND CURRENT StrekKode EXCLUSIVE-LOCK.

      /* Nullstiller hovednummer for alle strekkoder på størrelsen */
      FOR EACH bStrekKode OF ArtBas EXCLUSIVE-LOCK:
          /* Går fortere pga. indeks. */
          IF bStrekkode.StrKode = Strekkode.StrKode THEN
          DO:
              ASSIGN bStrekKode.HovedNr = FALSE.
          END.
      END.

      ASSIGN StrekKode.HovedNr = TRUE.
      FIND CURRENT StrekKode NO-LOCK.
  END. /* TRANSACTION */
  BROWSE {&BROWSE-NAME}:REFRESH().
  APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-IKasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-IKasse C-Win
ON CHOOSE OF BUTTON-IKasse IN FRAME FRAME-Strekkode /* I kasse... */
DO:
  DEFINE VARIABLE lIkasse AS LOGICAL    NO-UNDO.
  IF StrekKode.Kode BEGINS "02" AND StrekKode.StrKode = 0 THEN DO:
      MESSAGE "Strekkode kann ikke endres. Må finnes for kassen."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE IF StrekKode.Kode = STRING(ArtBas.ArtikkelNr) AND 
     StrekKode.Kode = STRING(ArtBas.Vg)THEN DO:
      MESSAGE "PLU-strekkode kann ikke endres."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE IF ArtBas.Pakke AND {&BROWSE-NAME}:MAX-DATA-GUESS = 1 AND 
           CAN-FIND(FIRST PakkeLinje OF ArtBas) THEN DO:
      MESSAGE "Strekkode kann ikke endre, det finnes pakkelinjer koblet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
/*   ELSE IF CAN-FIND(FIRST Pakkelinje WHERE PakkeLinje.PkArtikkelNr = ArtBas.ArtikkelNr */
/*                                 AND PakkeLinje.StrKode = StrekKode.StrKode) THEN DO:  */
/*       MESSAGE "Strekkoden er brukt i en pakke."                                       */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                          */
/*       RETURN NO-APPLY.                                                                */
/*   END.                                                                                */
/*   ASSIGN lIKasse = StrekKode.Ikasse. */
/*   RUN d-strekikasse.w (INPUT-OUTPUT lIKasse). */
/*   IF lIKasse = StrekKode THEN                 */
/*       RETURN NO-APPLY.                        */
  MESSAGE "Strekkoden har " + (IF Strekkode.IKasse = FALSE THEN "ikke " ELSE "") + "i kasse satt," SKIP
          "ønsker du å bytte?"
      VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE choice AS LOGICAL.
  IF NOT choice THEN RETURN.
  DO TRANSACTION:
      FIND CURRENT StrekKode EXCLUSIVE NO-ERROR.
      IF NOT LOCKED StrekKode THEN
          ASSIGN StrekKode.IKasse = NOT Strekkode.IKasse.
      FIND CURRENT StrekKode NO-LOCK.
  END. /* TRANSACTION */
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
      BROWSE {&BROWSE-NAME}:REFRESH().
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NyEan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NyEan C-Win
ON CHOOSE OF BUTTON-NyEan IN FRAME FRAME-Strekkode /* Ny kode... */
DO:
/*   IF DECI(ArtBas.Artikkelnr) = ArtBas.Vg THEN DO: */
/*        MESSAGE                                    */
/*        "Registreres ikke for PLU-artikkler."      */
/*            VIEW-AS ALERT-BOX INFO BUTTONS OK.     */
/*        RETURN NO-APPLY.                           */
/*   END.                                            */
  RUN d-strekkode.w (THIS-PROCEDURE,ArtBas.ArtikkelNr,ArtBas.StrTypeId,"",1).

  IF  cTidskrift <> '' AND 
      CAN-FIND(FIRST Strekkode WHERE
               Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
               Strekkode.Kode BEGINS cTidskrift AND
              LENGTH(Strekkode.Kode) = 13) THEN 
  DO:
      MESSAGE "Artikler som legges opp med, eller har EAN strekkoder som begynner med " + cTidskrift + ", legges normalt opp som kr. 100 + Mva i utpris." SKIP
              "Varekosten vil bli beregnes basert på den DB% som er lagt inn i klakylen." SKIP
              "Er varekosten i kalkylen satt til 0 kr. Vil varekost bli beregnet basert på kostnadsprosent fra varegruppen."
          VIEW-AS ALERT-BOX INFORMATION BUTTON OK TITLE "".
  END.

  IF CAN-FIND(FIRST Strekkode WHERE
              Strekkode.ArtikkelNr = Artbas.ArtikkelNr) THEN
    BROWSE {&BROWSE-NAME}:REFRESH ( ).
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
/*   RUN ButtonEnaDis. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Win
ON CHOOSE OF BUTTON-Slett IN FRAME FRAME-Strekkode /* Slett */
DO:
  DEFINE VARIABLE lHovedNr AS LOGICAL    NO-UNDO.
  /* Test om det bara finns en streckkod på varan */
/*   FIND bStrekkode WHERE bStrekkode.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR. */
/*   IF AVAIL bStrekKode THEN DO:                                                      */
/*       MESSAGE "Strekkode kann ikke slettes. Må finnes for kassen."                  */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                        */
/*       RETURN NO-APPLY.                                                              */
/*   END.                                                                              */
  IF ArtBas.IndividType > 0 AND 
         CAN-FIND(FIRST Individ WHERE Individ.ArtikkelNr = ArtBas.ArtikkelNr AND 
                                      Individ.StrKode = StrekKode.StrKode) THEN DO:
      MESSAGE "Individ er koblet til denne strekkoden, kann ikke slettes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  IF StrekKode.Kode = STRING(ArtBas.ArtikkelNr) AND 
     StrekKode.Kode = STRING(ArtBas.Vg)THEN DO:
      MESSAGE "PLU-strekkode kann ikke slettes."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  ELSE IF ArtBas.Pakke AND {&BROWSE-NAME}:MAX-DATA-GUESS = 1 AND 
           CAN-FIND(FIRST PakkeLinje OF ArtBas) THEN DO:
      MESSAGE "Strekkode kann ikke slettes, det finnes pakkelinjer koblet."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
/*   ELSE IF CAN-FIND(FIRST Pakkelinje WHERE PakkeLinje.PkArtikkelNr = ArtBas.ArtikkelNr */
/*                                 AND PakkeLinje.Kode = StrekKode.Kode) THEN DO:        */
/*       MESSAGE "Strekkoden er brukt i en pakke."                                       */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                          */
/*       RETURN NO-APPLY.                                                                */
/*   END.                                                                                */
/*   ELSE IF Artbas.StrTypeId <> 2 AND StrekKode.Kode BEGINS "02" AND StrekKode.StrKode = 0 THEN DO: */
/*       MESSAGE "Strekkode kann ikke slettes. Må finnes for kassen."                                */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                      */
/*       RETURN NO-APPLY.                                                                            */
/*   END.                                                                                            */
  MESSAGE "Vill du slette posten? " VIEW-AS ALERT-BOX QUESTION
                BUTTONS YES-NO UPDATE wSvar AS LOGI.
  IF wSvar THEN 
  DO TRANSACTION:
      FIND CURRENT StrekKode EXCLUSIVE NO-WAIT NO-ERROR.
      IF AVAIL StrekKode THEN DO:
        FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE
            VPIArtBas.VareNr = Strekkode.Kode:
            ASSIGN
                VPIArtBas.ArtikkelNr = 0
                .
        END.
        ASSIGN lHovedNr = StrekKode.HovedNr.
        DELETE StrekKode.
        BROWSE {&BROWSE-NAME}:DELETE-CURRENT-ROW().
        IF lHovedNr THEN DO:
            FIND FIRST StrekKode OF ArtBas NO-ERROR.
            IF AVAIL StrekKode THEN DO:
                ASSIGN StrekKode.HovedNr = TRUE.
                BROWSE {&BROWSE-NAME}:REFRESH().
            END.
        END.
      END.
      ELSE
          MESSAGE "Posten er under oppdatering av annen bruker" SKIP
                  "og slettes ikke."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END. /* TRANSACTION */
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
/*   RUN ButtonEnaDis. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.             

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  DO:
/*    RUN SaveBrowseSettings. */
    IF VALID-HANDLE(wParentHandle) THEN
      RUN SlettProg IN wParentHandle.
    IF VALID-HANDLE(hEtikettVindu) THEN
        APPLY "CLOSE" TO hEtikettVindu.
    RUN disable_UI.
  END.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
{syspara.i 1 1 18 cHKinst}

/* Sjekker om tidskrift koder skal konverteres. */
{syspara.i 2 4 23 cTidskrift}
IF cTidskrift = "0" THEN
    cTidskrift = "".

ON ALT-Y OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-NyEan IN FRAME FRAME-Strekkode.
  END.
ON ALT-E OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Endre IN FRAME FRAME-Strekkode.
  END.
ON ALT-T OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME FRAME-Strekkode.
  END.
ON ALT-H OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-HovedNr IN FRAME FRAME-Strekkode.
  END.
ON ALT-I OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-IKasse IN FRAME FRAME-Strekkode.
  END.
ON ALT-G OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-GenererEan IN FRAME FRAME-Strekkode.
  END.
ON ALT-M OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-EtiBatch IN FRAME FRAME-Strekkode.
  END.
ON ALT-U OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-EanEti IN FRAME FRAME-Strekkode.
  END.
ON ALT-R OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-Plakat IN FRAME FRAME-Strekkode.
  END.
ON ALT-F OF C-Win ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-VPIEan IN FRAME FRAME-Strekkode.
  END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  FIND StrType OF ArtBas NO-LOCK NO-ERROR.
  RUN getBestNrHandle.
  RUN enable_UI.
  RUN buttonenadis.
  .
  {lng.i}
  ASSIGN FI-ArtIkasseTxt:HIDDEN = ArtBas.IKasse.
  SUBSCRIBE TO "ByttObjekt" IN wParentHandle NO-ERROR.
  BROWSE {&BROWSE-NAME}:SET-REPOSITIONED-ROW(BROWSE {&BROWSE-NAME}:DOWN,"CONDITIONAL").
  APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  ON ALT-P OF FRAME {&FRAME-NAME} ANYWHERE
  DO:
      IF VALID-HANDLE(hEtikettVindu) THEN
          RUN SkrivUt IN hEtikettVindu NO-ERROR.
  END.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-Win 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FRAME {&FRAME-NAME}:SENSITIVE = artbas.sanertdato = ?.
    IF FRAME {&FRAME-NAME}:SENSITIVE THEN DO:
        ASSIGN /* BUTTON-Ny:SENSITIVE       = ArtBas.LopNr <> ? AND ArtBas.LopNr <> 0 */
               BUTTON-Endre:SENSITIVE    = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? AND 
                                         /* NOT ArtBas.Pakke AND */
                                         (/*NOT StrekKode.Kode BEGINS "02" AND*/ (StrekKode.Kodetype = 0 OR StrekKode.Kodetype = 1))
               button-HovedNr:SENSITIVE  = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? AND
                                          StrekKode.HovedNr = FALSE /* AND ArtBas.HkStyrt = FALSE */
               B-GenererEAN:SENSITIVE    = BUTTON-NyEAN:SENSITIVE
               BUTTON-Slett:SENSITIVE    = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
               BUTTON-IKasse:SENSITIVE   = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?
               BUTTON-EanEti:SENSITIVE   = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? AND 
                              (StrekKode.KodeType = 0 OR StrekKode.KodeType = 1 OR StrekKode.KodeType = 2)
               BUTTON-EtiBatch:SENSITIVE = BUTTON-EanEti:SENSITIVE.
               IF cHKinst = "no" THEN 
                   B-VpiEAN:SENSITIVE = CAN-FIND(VpiArtBas WHERE VPIArtBas.EkstVPILevNr = 1 AND VpiArtBas.ArtikkelNr = ArtBas.ArtikkelNr).
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER dArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  FIND ArtBas WHERE ArtBas.ArtikkelNr = dArtikkelNr NO-LOCK.
  FIND StrType OF ArtBas NO-LOCK NO-ERROR.
  IF VALID-HANDLE(hFlyttEanVindu) THEN
      APPLY "CLOSE" TO hFlyttEanVindu.
  PROCESS EVENTS.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-ArtIkasseTxt:HIDDEN = ArtBas.IKasse.
      {sww.i}  
      {&OPEN-QUERY-{&BROWSE-NAME}}
      APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
      RUN ButtonEnaDis.
      {swn.i}
  END.
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
  /* Hide all frames. */
  HIDE FRAME FRAME-Strekkode.
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
  DISPLAY FI-ArtIkasseTxt 
      WITH FRAME FRAME-Strekkode.
  ENABLE BUTTON-NyEan BROWSE-Strekkode BUTTON-Endre BUTTON-Slett BUTTON-HovedNr 
         BUTTON-IKasse B-SlettUbrukte B-GenererEAN BUTTON-EtiBatch 
         BUTTON-EanEti B-Plakat B-VpiEAN B-Slett02Dubletter B-FlyttEan 
      WITH FRAME FRAME-Strekkode.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Strekkode}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genVareId C-Win 
PROCEDURE genVareId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEFINE VARIABLE iVareId LIKE ArtBas.VareId NO-UNDO.                         */
/*   DEFINE BUFFER bArtBas FOR ArtBas.                                           */
/*   FIND LAST bArtBas USE-INDEX VareId NO-LOCK NO-ERROR.                        */
/*   ASSIGN iVareId = IF bArtBas.VareId = 0 THEN 250001 ELSE bArtBas.VareId + 1. */
/*   FIND CURRENT ArtBas EXCLUSIVE.                                              */
/*   ASSIGN ArtBas.VareId = iVareId.                                             */
/*   FIND CURRENT ArtBas NO-LOCK NO-ERROR.                                       */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getBestNrHandle C-Win 
PROCEDURE getBestNrHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    hBestnr = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(hBestnr):
        IF hBestnr:NAME = "Bestillingsnummer" THEN
            LEAVE.
        hBestnr = hBestnr:NEXT-COLUMN.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Win 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   IF FRAME FRAME-Strekkode:MOVE-TO-TOP() THEN.
   APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
/*    APPLY "ENTRY" TO BUTTON-Ny. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQueryRep C-Win 
PROCEDURE OpenQueryRep :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER rStrekKode AS ROWID      NO-UNDO.
    {&OPEN-QUERY-{&BROWSE-NAME}}
    REPOSITION {&BROWSE-NAME} TO ROWID rStrekKode.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntry C-Win 
PROCEDURE SetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN */
        APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettUbrukte C-Win 
PROCEDURE SlettUbrukte :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cKoder AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
   DO ii = 1 TO NUM-ENTRIES(cKoder,"|"):
       FIND Strekkode WHERE strekkode.kode = ENTRY(ii,cKoder,"|") NO-ERROR.
       IF AVAIL strekkode THEN
           DELETE strekkode.
   END.
   {&OPEN-QUERY-{&BROWSE-NAME}}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND StrKonv OF StrekKode NO-LOCK NO-ERROR.
  RETURN IF AVAIL StrKonv THEN StrKonv.Storl ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

