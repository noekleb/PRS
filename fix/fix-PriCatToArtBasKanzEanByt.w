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

DEFINE TEMP-TABLE TT_Pricat
/* 1  */  FIELD modell     AS DECIMAL
/* 2  */  FIELD levnr      AS INTEGER
/* 3  */  FIELD levkod     AS CHARACTER
/* 4  */  FIELD ean        AS CHARACTER
/* 5  */  FIELD varetekst  AS CHARACTER
/* 6  */  FIELD fargekode  AS INTEGER 
/* 7  */  FIELD levfarkode AS CHARACTER 
/* 8  */  FIELD storl      AS CHARACTER 
/* 9  */  FIELD strtype    AS INTEGER 
/* 10 */  FIELD varemerke  AS CHARACTER 
/* 11 */  FIELD enh        AS CHARACTER
/* 12 */  FIELD ant        AS INTEGER   
/* 13 */  FIELD levpris    AS DECIMAL  
/* 14 */  FIELD vg         AS INTEGER  
/* 15 */  FIELD ntoforh    AS DECIMAL  
/* 16 */  FIELD utpris     AS DECIMAL
/* 17 */  FIELD artikkelnr AS DECIMAL
/* 18 */  FIELD mva        AS DECIMAL.

DEFINE BUFFER bArtbas FOR Artbas.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-62 FI-Filnavn B-Lesinn B-Skapa 
&Scoped-Define DISPLAYED-OBJECTS FI-Filnavn FI-AntallA FI-AntallE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Lesinn 
     LABEL "Les inn" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Skapa 
     LABEL "Testa om finn" 
     SIZE 17 BY 1.14.

DEFINE VARIABLE FI-AntallA AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall Artikkler" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntallE AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall Ean" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filnavn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Drag-and-drop-Fil" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 DROP-TARGET NO-UNDO.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 75 BY 3.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Filnavn AT ROW 5.29 COL 21 COLON-ALIGNED
     B-Lesinn AT ROW 6.71 COL 24
     FI-AntallA AT ROW 6.71 COL 58 COLON-ALIGNED
     FI-AntallE AT ROW 7.91 COL 58 COLON-ALIGNED
     B-Skapa AT ROW 9.33 COL 24
     "" VIEW-AS TEXT
          SIZE 73 BY 1.67 AT ROW 3.1 COL 5
          BGCOLOR 10 FONT 6
     " Lev VPI" VIEW-AS TEXT
          SIZE 73 BY 1.67 AT ROW 1.43 COL 5
          BGCOLOR 10 FONT 6
     RECT-62 AT ROW 1.24 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 80
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
                                                                        */
/* SETTINGS FOR FILL-IN FI-AntallA IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-AntallE IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME B-Lesinn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Lesinn C-Win
ON CHOOSE OF B-Lesinn IN FRAME DEFAULT-FRAME /* Les inn */
DO:
    DEFINE VARIABLE cString AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iAnt  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iAntE AS INTEGER    NO-UNDO.
    INPUT FROM VALUE(FI-Filnavn:SCREEN-VALUE).
    REPEAT:
        IMPORT UNFORMATTED cString.
        IF cString = "" THEN
            NEXT.
        iAnt = iAnt + 1.
        CREATE TT_Pricat.
        ASSIGN modell     = deci(ENTRY(1,cString,";"))
               levnr      = int(ENTRY(2,cString,";"))
               levkod     = ENTRY(3,cString,";")
               ean        = ENTRY(4,cString,";")
               varetekst  = ENTRY(5,cString,";")
               fargekode  = int(ENTRY(6,cString,";"))
               levfarkode = ENTRY(7,cString,";")
               storl      = ENTRY(8,cString,";")
               strtype    = int(ENTRY(9,cString,";"))
               varemerke  = ENTRY(10,cString,";")
/*                enh        = ENTRY(11,cString,";")      */
/*                ant        = int(ENTRY(12,cString,";")) */
               levpris    = deci(ENTRY(13,cString,";"))
               vg         = int(ENTRY(23,cString,";"))
               ntoforh    = deci(ENTRY(25,cString,";"))
               utpris     = deci(REPLACE(ENTRY(31,cString,";")," ",""))
               mva        = 24.
    END.
    INPUT CLOSE.
/*     OUTPUT TO "CLIPBOARD".             */
/*                                        */
/*     FOR EACH TT_Pricat:                */
/*         DISP TT_pricat WITH WIDTH 300. */
/*     END.                               */
/*     OUTPUT CLOSE.                      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Skapa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Skapa C-Win
ON CHOOSE OF B-Skapa IN FRAME DEFAULT-FRAME /* Testa om finn */
DO:
    DEFINE BUFFER bStrekkode FOR strekkode.
    FOR EACH TT_Pricat.
        FIND Artbas WHERE artbas.levkod = TT_Pricat.levkod AND
                          artbas.farg = TT_Pricat.fargekode AND
                          artbas.levnr = tt_pricat.levnr AND
                          artbas.beskr = varetekst NO-ERROR.
        IF AVAIL artbas THEN DO:
            FIND strkonv WHERE TRIM(strkonv.storl) = tt_pricat.storl.
            FIND strekkode OF artbas WHERE strekkode.strkode = strkonv.strkode NO-ERROR.
            IF AVAIL strekkode THEN DO:
                CREATE bstrekkode.
                BUFFER-COPY strekkode EXCEPT kode TO bstrekkode.
                bstrekkode.kode = TRIM(ean).
                RELEASE bstrekkode.
            END.

        END.
/*         ASSIGN modell     = deci(ENTRY(1,cString,";"))                  */
/*                levnr      = int(ENTRY(2,cString,";"))                   */
/*                levkod     = ENTRY(3,cString,";")                        */
/*                ean        = ENTRY(4,cString,";")                        */
/*                varetekst  = ENTRY(5,cString,";")                        */
/*                fargekode  = int(ENTRY(6,cString,";"))                   */
/*                levfarkode = ENTRY(7,cString,";")                        */
/*                storl      = ENTRY(8,cString,";")                        */
/*                strtype    = int(ENTRY(9,cString,";"))                   */
/*                varemerke  = ENTRY(10,cString,";")                       */
/*                levpris    = deci(ENTRY(13,cString,";"))                 */
/*                vg         = int(ENTRY(23,cString,";"))                  */
/*                ntoforh    = deci(ENTRY(25,cString,";"))                 */
/*                utpris     = deci(REPLACE(ENTRY(31,cString,";")," ","")) */
/*                mva        = 24.                                         */
/*                                                                         */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Filnavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Filnavn C-Win
ON DROP-FILE-NOTIFY OF FI-Filnavn IN FRAME DEFAULT-FRAME /* Drag-and-drop-Fil */
DO:
    SELF:SCREEN-VALUE = SELF:GET-DROPPED-FILE(1).
  SELF:END-FILE-DROP().
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
/* ON CREATE OF ArtBas OVERRIDE DO: END.    */
/* ON WRITE  OF ArtBas OVERRIDE DO: END.    */
/* ON CREATE OF ArtPris OVERRIDE DO: END.   */
/* ON WRITE  OF ArtPris OVERRIDE DO: END.   */
/* ON CREATE OF StrekKode OVERRIDE DO: END. */
/* ON WRITE  OF StrekKode OVERRIDE DO: END. */
/* ON CREATE OF Lager OVERRIDE DO: END.     */
/* ON WRITE  OF Lager OVERRIDE DO: END.     */

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
  DISPLAY FI-Filnavn FI-AntallA FI-AntallE 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-62 FI-Filnavn B-Lesinn B-Skapa 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaArtBas C-Win 
PROCEDURE SkapaArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     DEFINE BUFFER bArtBas FOR ArtBas.
     FIND VarGr WHERE VarGr.Vg = TT_Pricat.vg NO-LOCK.
/*      IF CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = TT_Pricat.Modell) THEN */
/*          RETURN.                                                         */
     FIND LAST bArtbas WHERE bArtbas.Vg = TT_Pricat.vg USE-INDEX vglopnr NO-LOCK NO-ERROR.
     CREATE Artbas.
     ASSIGN
            ArtBas.AktivDato      = TODAY
            ArtBas.Aktivert       = TRUE
/*             ArtBas.ArtikkelNr     = TT_Pricat.modell */
            ArtBas.Beskr          = TT_Pricat.varetekst
            ArtBas.BongTekst      = TRIM(SUBSTR(TT_Pricat.varetekst,1,20))
            ArtBas.Farg           = TT_Pricat.Fargekode
            ArtBas.Hg             = Vargr.hg
            ArtBas.IKasse         = TRUE
            ArtBas.KjentPaHK      = TRUE
            ArtBas.KundeRabatt    = TRUE
            ArtBas.LevKod         = TT_Pricat.levkod
            ArtBas.LevFargKod     = TT_Pricat.levfarkod
            ArtBas.LevNr          = TT_Pricat.Levnr
            ArtBas.LopNr          = IF AVAIL bArtBas THEN bArtbas.lopnr + 1 ELSE 1
            ArtBas.OLLager        = TRUE
            ArtBas.OPris          = FALSE
            ArtBas.SalgsEnhet     = "Par"
            ArtBas.Storrelser     = TRUE
            ArtBas.StrTypeID      = TT_Pricat.strtype
            ArtBas.Vg             = TT_Pricat.vg
            ArtBas.VgKat          = 1
            Artbas.vmid           = INT(TT_Pricat.varemerke).
     TT_Pricat.ArtikkelNr = ArtBas.Artikkelnr.
      RELEASE ArtBas.
      RUN genStrekKode.p (TT_Pricat.ArtikkelNr,1,"").
/* /* 10 */  FIELD varemerke  AS CHARACTER */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaArtPris C-Win 
PROCEDURE SkapaArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE dDB%     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dDBKr    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMva%    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMvaKr   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPris    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dValpris AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dInpris  AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dVarekost AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPrisUMoms AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMomsMarg AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dMomsen AS DECIMAL    NO-UNDO.
    ASSIGN dMvaKr     = ROUND((TT_Pricat.Mva / (100 + TT_Pricat.Mva)) * TT_Pricat.utpris,2)
           dDBKr      = TT_Pricat.utpris - dMvaKr - TT_Pricat.ntoforh
           dDB%       = round(100 * dDBKr / (TT_Pricat.utpris - dMvaKr),2)
           dMva%      = TT_Pricat.Mva
           dPris      = TT_Pricat.utpris
           dPrisUMoms = TT_Pricat.utpris - dMvaKr.
        .
    CREATE ArtPris.
    ASSIGN  /* nyckelfält */
        ArtPris.ArtikkelNr = TT_Pricat.artikkelnr
        ArtPris.ProfilNr   = 1.
    ASSIGN
        ArtPris.AktivFraDato    = TODAY
        ArtPris.DB%[1]          = dDB%
        ArtPris.DBKr[1]         = dDBKr
        ArtPris.EuroManuel      = TRUE
        ArtPris.LevNr           = TT_Pricat.Levnr
        ArtPris.Mva%[1]         = TT_Pricat.Mva
        ArtPris.MvaKr[1]        = dMvaKr
        ArtPris.Pris[1]         = TT_Pricat.utpris
        ArtPris.ValPris[1]      = TT_Pricat.levpris
        ArtPris.InnkjopsPris[1] = TT_Pricat.levpris
        Artpris.Rab1Kr[1]       = TT_Pricat.levpris - TT_Pricat.ntoforh
        Artpris.Rab1%[1]        = IF Artpris.Rab1Kr[1] = 0 THEN 0 ELSE ROUND(Artpris.Rab1Kr[1] / TT_Pricat.levpris,2)
        ArtPris.VareKost[1]     = TT_Pricat.ntoforh.
    RELEASE ArtPris.

END PROCEDURE.

/* /* 13 */  FIELD levpris    AS DECIMAL  */
/* /* 15 */  FIELD ntoforh    AS DECIMAL  */
/* /* 16 */  FIELD utpris     AS DECIMAL. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaStrekKode C-Win 
PROCEDURE SkapaStrekKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     CREATE Strekkode.                                                 */
/*     ASSIGN StrekKode.Kode       = STRING(TT_Vare.Ean,"9999999999999") */
/*            StrekKode.ArtikkelNr = TT_Vare.Modell                      */
/*            StrekKode.StrKode    = TT_Vare.Storrelsesnr                */
/*            StrekKode.KodeType   = 1                                   */
/*            StrekKode.VareId     = TT_Vare.Modell                      */
/*            StrekKode.HovedNr    = TT_Vare.Storrelsesnr = 0.           */
/*            StrekKode.IKasse     = TRUE.                               */
/*     RELEASE strekkode.                                                */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

