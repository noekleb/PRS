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
DEF INPUT PARAMETER iBatchNr AS INT  NO-UNDO.
DEF INPUT PARAMETER cTabell  AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
def var cEDB-System      as char  no-undo.
def var cEDB-Tabell      as char  no-undo.
def var c2EDB-Tabell     as char  no-undo.
DEF VAR iCl              AS INT   NO-UNDO.
DEF VAR iProfilNr        AS INT   NO-UNDO.
DEF VAR iBrGrNr          AS INT   NO-UNDO.
DEF VAR cButikkTeam      AS CHAR  NO-UNDO.
DEF VAR rBestHodeRecid   AS RECID NO-UNDO.
DEF VAR cFileName        AS CHAR  NO-UNDO. 
DEF VAR cExcEkstent      AS CHAR  NO-UNDO.
DEF VAR cKunde           AS CHAR  NO-UNDO.
DEF VAR cSkoTex          AS CHAR  NO-UNDO.
DEF VAR lLapTop          AS LOG   NO-UNDO.

DEF VAR lBestNye         AS LOG NO-UNDO.
DEF VAR lBestOverskriv   AS LOG NO-UNDO.
DEF VAR cImpTabell       LIKE ImpKonv.Tabell     NO-UNDO.
DEF VAR cImpHg           LIKE ImpKonv.Tabell     NO-UNDO.
DEF VAR cImpFarg         LIKE ImpKonv.Tabell     NO-UNDO.
DEF VAR cImpSort         LIKE ImpKonv.Tabell     NO-UNDO.
DEF VAR cImpMaterial     LIKE ImpKonv.Tabell     NO-UNDO.

DEF VAR pcSkjerm   AS CHAR NO-UNDO.
DEF VAR pdcValPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR pdcInnPris AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEF VAR pdcRab1%   AS DEC FORMAT "->>>,>>>,>>9.99" NO-UNDO.

DEF TEMP-TABLE tSort
  FIELD SortId   AS CHAR
  FIELD ButikkNr AS INT
  FIELD AntSort  AS INT
  FIELD AntPar   AS INT
  FIELD Typ      AS CHAR
  FIELD SeqNr    AS INT
  .

DEF TEMP-TABLE tmpRapp
  FIELD Felt1  AS CHAR
  FIELD Felt2  AS CHAR
  FIELD Felt3  AS CHAR
  FIELD Felt4  AS CHAR
  FIELD Felt5  AS CHAR
  FIELD Felt6  AS CHAR
  FIELD Felt7  AS CHAR
  FIELD Felt8  AS CHAR
  FIELD Felt9  AS CHAR
  FIELD Felt10 AS CHAR
  .

DEF STREAM sExportFile.

{runlib.i}
{methodexcel.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define DISPLAYED-OBJECTS FI-Info 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ByttElement C-Win 
FUNCTION ByttElement RETURNS CHARACTER
  ( input ipSkjerm as char,
    input ipElement as int,
    input ipNyttElement as char,
    input ipDelimiter as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LevDato C-Win 
FUNCTION LevDato RETURNS DATE
  ( pdBDato AS date,
    pcDato AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Info AT ROW 1.52 COL 2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 74.4 BY 2.14.


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
         TITLE              = "Import av bestillinger"
         HEIGHT             = 2.14
         WIDTH              = 74.4
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
/* SETTINGS FOR FILL-IN FI-Info IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Import av bestillinger */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Import av bestillinger */
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Kode for konvertering av artikkelnummer ved import. */
{syspara.i 1 2 7 cEDB-System}
if cEDB-System = "" then
  cEDB-System = "FLEXICON".
{syspar2.i 1 2 7 cEDB-Tabell}
if cEDB-Tabell = "" then
  cEDB-Tabell = "BestHode".

{syspar2.i 1 2 6 c2EDB-Tabell}
if c2EDB-Tabell = "" then
  c2EDB-Tabell = "ArtBas".

/* Centrallager */
{syspara.i 5 1 1 iCl INT}

{syspara.i 1 4   1 cExcEkstent}
cExcEkstent = if cExcEkstent = "" then "sdv" else cExcEkstent.   
{syspara.i 1 1 100 cKunde}
{syspara.i 1 1 101 cSkoTex}

/* Henter parametre for konvertering. */
{syspar2.i 1 2    7 cImpTabell}
{syspar2.i 1 2 1002 cImpHg}
{syspar2.i 1 2 1004 cImpFarg}
{syspar2.i 1 2 1009 cImpSort}
{syspar2.i 1 2 1005 cImpMaterial}

/* Sjekker om det kjøres på en LapTop */
if valid-handle(wLibHAndle) then
  run SjekkLapTop in wLibHandle (output lLapTop).
ELSE 
  lLapTop = FALSE.

/* Profilnr for sentrallager. */
FIND Butiker NO-LOCK WHERE
    Butiker.butik = iCl NO-ERROR.
ASSIGN
    iProfilNr = Butiker.ProfilNr
    .

FIND ImportHode NO-LOCK WHERE
    ImportHode.BatchNr = iBatchNr.

FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK.
IF AVAIL Bruker THEN DO:
    FIND BrukerGrp WHERE BrukerGrp.BrGrpNr = Bruker.BrGrpNr NO-LOCK NO-ERROR.
    IF NOT AVAIL BrukerGrp THEN DO:
        MESSAGE "Feil brukergruppe." VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY "AVBRYT".
    END.
    ASSIGN iBrGrNr = BrukerGrp.BrGrpNr.
END.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  {lng.i}
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
  DISPLAY FI-Info 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Feilliste C-Win 
PROCEDURE Feilliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  DEF VAR piAntPoster AS INT NO-UNDO.

  {sww.i}
  
  /* Finner temporært filnavn. */
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle ("ImpBest", cExcEkstent, output cFileName). 
  
  /* Åpner stream */
  OUTPUT STREAM sExportFile TO VALUE(cFileName) NO-ECHO.
  
  /* Legger ut overskrifter. */
  STATUS DEFAULT "Eksporterer data...".
  EXPORT STREAM sExportFile DELIMITER ";"
    "FEILLISTE Bestillingsimport"
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    ""
    SKIP.                                 
  EXPORT STREAM sExportFile DELIMITER ";"
    "Sesong"
    "Leverandør"
    "Lev.artikkelNr"
    "Type"
    "BestillingsNr"
    "Material"
    "Farge"
    "Leveringstid"
    "Lev.inndeling"
    "Feilmedling"
    SKIP.                                 
                                  
  /* Eksporterer data */
  EKSPORT:
  FOR EACH tmpRapp no-lock:
                    
    EXPORT STREAM sExportFile DELIMITER ";"
      /* A */ tmpRapp.Felt1     
      /* B */ tmpRapp.Felt2        
      /* C */ tmpRapp.Felt3        
      
      /* D */ tmpRapp.Felt4          
      
      /* E */ tmpRapp.Felt5     
      /* F */ tmpRapp.Felt6    
      /* G */ tmpRapp.Felt7  
      /* H */ tmpRapp.Felt8  
      /* I */ tmpRapp.Felt9    
      /* J */ tmpRapp.felt10
      .
                                
  END. /* EKSPORT */
                                  
  /* Lukker stream */
  OUTPUT STREAM sExportFile CLOSE.
  
  STATUS DEFAULT "Importerer data i Excel...".
  CREATE "Excel.Application" chExcelApplication.  
  chExcelApplication:Visible = FALSE.                                     
  chWorkbooks = chExcelApplication:Workbooks:OpenText(cFileName,2,1,1,1,1,FALSE,TRUE,FALSE,FALSE,FALSE).
 
  STATUS DEFAULT "Setter aktivt ark...".
  chWorkSheets = chExcelApplication:Sheets:Item(1).
 
  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:J2"):Font:Bold = TRUE.
  chWorkSheets:Range("A1:J2"):Font:Italic = TRUE.

  STATUS DEFAULT "Blokkinndeling...".
  chWorkSheets:Range("C:C"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("D:D"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("J:J"):borders(10):LineStyle     = 9. /*** Dobbelt linje ****/
  chWorkSheets:Range("A2:J2"):borders(10):LineStyle   = 9. /*** Dobbelt linje ****/
  
  STATUS DEFAULT "Setter nummerformat...".
  /*
  chWorkSheets:Range("E:E"):NumberFormat = "# ##0".
  chWorkSheets:Range("F:F"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("G:G"):NumberFormat = "# ##0,00".
  chWorkSheets:Range("H:H"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("I:I"):NumberFormat = "# ##0".
  chWorkSheets:Range("J:J"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("K:K"):NumberFormat = "# ##0,0".
  chWorkSheets:Range("L:L"):NumberFormat = "# ##0,0".
  */

  STATUS DEFAULT "Setter overskrift...".
  chWorkSheets:Range("A1:C1"):Merge().
  chWorkSheets:Range("A1:C1"):HorizontalAlignment = 3.
        
  chWorkSheets:Range("C2:C2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.
  chWorkSheets:Range("E2:E2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("F2:F2"):HorizontalAlignment = 4.   
  
  chWorkSheets:Range("G2:G2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("H2:H2"):HorizontalAlignment = 4.   

  chWorkSheets:Range("I2:I2"):HorizontalAlignment = 4.
  
  chWorkSheets:Range("J2:J2"):HorizontalAlignment = 4.   
  
  STATUS DEFAULT "Setter AutoFit...".
  chWorkSheets:Columns("A:J"):AutoFit().

  STATUS DEFAULT "Setter Kriterier...".
  chWorkSheets:PageSetup:PrintTitleRows = "A1:X2".  
  chWorkSheets:PageSetup:LeftHeader     = "Kriterier - <Blank>".
  chWorkSheets:PageSetup:RightHeader    = "Antall poster: " + String(piAntPoster).
  chWorkSheets:PageSetup:LeftFooter     = cKunde.
  chWorkSheets:PageSetup:RightFooter    = cSkoTex.
  chWorksheets:PageSetup:PrintArea      = "A:j".
  chWorkSheets:PageSetup:Orientation    = 2.
  chWorkSheets:PageSetup:FitToPagesWide = 1.
  
  STATUS DEFAULT "Setter FreezePanes...".
  chWorkSheets:Range("A3"):Select().
  chExcelApplication:ActiveWindow:FreezePanes = True.
  
  /* Legger inn sumlinjer. */                                        
  /* Excel macro som gjør jobben.
  Range("A4").Select
    Selection.Subtotal GroupBy:=1, Function:=xlSum, TotalList:=Array(5, 7, 9), _
        Replace:=True, PageBreaks:=True, SummaryBelowData:=True  
  */
  STATUS DEFAULT "Setter summeringer...".
  /*chWorkSheets:Range("E4:M50"):Subtotal(1 ,1 ,"5 , 7, 9" ,TRUE ,TRUE ,TRUE ).*/   
  
  chExcelApplication:Visible = TRUE.
  
  RELEASE OBJECT chWorksheets NO-ERROR.            /* release com-handles */
  RELEASE OBJECT chWorkbooks NO-ERROR.             /* release com-handles */
  RELEASE OBJECT chExcelApplication NO-ERROR.      /* release com-handles */

  STATUS DEFAULT "".

  {swn.i}


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importer C-Win 
PROCEDURE Importer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop       AS INT  NO-UNDO.
  DEF VAR pi2Loop      AS INT  NO-UNDO.
  DEF VAR piAntall     AS INT  EXTENT 50 NO-UNDO.
  DEF VAR pcEkstId     AS CHAR NO-UNDO.
  DEF VAR piBestNr     AS INT  NO-UNDO.
  DEF VAR pdLevDato    AS DATE NO-UNDO.
  DEF VAR piSum        AS INT  NO-UNDO.
  DEF VAR piSeqNr      AS INT  NO-UNDO.
  DEF VAR pcInndeling  AS CHAR NO-UNDO.
  DEF VAR pcStorl      AS CHAR NO-UNDO.
  DEF VAR pcStorrelser AS CHAR NO-UNDO.
  DEF VAR pcMaterial   AS CHAR NO-UNDO.
  DEF VAR pcFarge      AS CHAR NO-UNDO.
  DEF VAR pcSortId     AS CHAR NO-UNDO.
  DEF VAR pdBestDato   AS DATE NO-UNDO.
  
  DEF VAR h_PrisKo     AS HANDLE NO-UNDO.
  
  /* Renser feilloggen */
  FOR EACH tmpRapp:
      DELETE tmpRapp.
  END.

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_Prisko.

  /* For å få med all informasjon som kan samles på en bestilling i */
  /* FlexiCON, må dette brytes opp i flere bestillinger i SkoTex.   */
  /* Denne oppløsningen håndteres i Break By satsen.                */
  MAINLOOP:
  FOR EACH ImportLinje EXCLUSIVE-LOCK WHERE
    ImportLinje.BatchNr = iBatchNr AND
    ImportLinje.Tabell  = cTabell 
    BREAK 
    BY ImportLinje.BatchNr
    BY ImportLinje.Tabell
    BY ImportLinje.Felt[ 1] /* Sesong   */
    BY ImportLinje.Felt[ 2] /* LevNr    */
    BY ImportLinje.Felt[ 3] /* LevArtNr */
    BY ImportLinje.Felt[12] /* BestNr   */
    BY ImportLinje.Felt[ 5] /* Material */
    BY ImportLinje.Felt[ 6] /* Farge    */
    BY ImportLinje.Felt[ 4] /* LevTid   */
    :
    
    /* Tømmer temp-file */
    IF FIRST-OF(ImportLinje.Felt[ 6]) THEN
    DO:
      ASSIGN
          piSeqNr     = 0
          pcInndeling = ""
          piAntall    = 0
          .
      FOR EACH tSort:
        DELETE tSort.
      END.
    END.

    /* Konverterer material */
    ASSIGN
        pcMaterial = ImportLinje.Felt[ 5]
        .
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpMaterial AND 
        ImpKonv.EksterntId = pcMaterial NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        pcMaterial = ImpKonv.InterntId
        .

    /* Konverterer farge */
    ASSIGN
        pcFarge = ImportLinje.Felt[ 6]
        .
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpFarg AND 
        ImpKonv.EksterntId = pcFarge NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        pcFarge = ImpKonv.InterntId
        .

    /* -- Konverterer SortId for leverandørsortmentet her. -- */
    ASSIGN
        pcSortId = ImportLinje.Felt[ 7]
        .
    FIND FIRST ImpKonv NO-LOCK WHERE 
        ImpKonv.EDB-System = cEDB-System AND 
        ImpKonv.Tabell     = cImpSort AND 
        ImpKonv.EksterntId = ImportLinje.Felt[ 2] + ";" + pcSortId NO-ERROR.
    IF AVAILABLE ImpKonv THEN
      ASSIGN
        pcSortId = entry(2,ImpKonv.InterntId,";")
        .

    /* Logger sortimentene */
    FIND tSort WHERE
      tSort.SortId   = pcSortId AND
      tSort.ButikkNr = int(ImportLinje.Felt[ 7]) NO-ERROR.
    IF NOT AVAILABLE tSort THEN
    DO:
        CREATE tSort.
        ASSIGN
            piSeqNr      = piSeqNr + 1
            tSort.SortId = pcSortId
            tSort.Butikk = INT(ImportLinje.Felt[ 8])
            tSort.Typ    = ImportLinje.Felt[13]
            tSort.SeqNr  = piSeqNr
            .
    END.
    ASSIGN
      tSort.AntSort = INT(ImportLinje.Felt[ 9])
      tSort.AntPar  = INT(ImportLinje.Felt[10])
      .
    
    /* Henter priser fra bestillingen */
    ASSIGN
      pdcValPris = DEC(ImportLinje.Felt[15])
      pdcInnPris = DEC(ImportLinje.Felt[11])
      pdcRab1%   = DEC(ImportLinje.Felt[17])
      .

    /* Nå legger vi opp bestillingen */
    IF LAST-OF(ImportLinje.Felt[ 6]) THEN
    BESTILLING:
    DO:
      /* Kontrollerer om leverandørsinndelingen finnes. */
      IF ImportLinje.Felt[13] = "A" THEN
      DO:
        IF NOT CAN-FIND(LevSort WHERE
                        LevSort.LevNr  = int(ImportLinje.Felt[2]) AND
                        LevSort.SortId = pcSortId) THEN
        DO:
          CREATE tmpRapp.
          ASSIGN
              tmpRapp.Felt1  = ImportLinje.Felt[ 1] 
              tmpRapp.Felt2  = ImportLinje.Felt[ 2] 
              tmpRapp.Felt3  = ImportLinje.Felt[ 3] 
              tmpRapp.Felt4  = ImportLinje.Felt[13] 
              tmpRapp.Felt5  = ImportLinje.Felt[12] 
              tmpRapp.Felt6  = pcMaterial 
              tmpRapp.Felt7  = pcFarge 
              tmpRapp.Felt8  = ImportLinje.Felt[ 4] 
              tmpRapp.Felt9  = pcSortId 
              tmpRapp.Felt10 = "* Ukjent LEVERANDØRSINNDELING."
              .
          NEXT MAINLOOP.
        END.
      END.

      ASSIGN
          piBestNr = 0
          piLoop   = piLoop + 1
          FI-Info  = "Behandler post " + STRING(piLoop) + "."
          .
      DISPLAY
          FI-Info
      WITH FRAME Default-Frame.

      /* Bygger eksternt ID på artikkelen. */
      ASSIGN
        pcEkstId   = ImportLinje.Felt[2]                    + "|" + /* LevNr    */
                     ImportLinje.Felt[3]                    + "|" + /* LevArtNr */
                     string(int(ImportLinje.Felt[6]),"999") + "|" + /* Farge    */
                     string(int(ImportLinje.Felt[5]),"999")         /* Material */
      .

      /* Henter konvreg for artikkelen. */
      find first KonvReg no-lock where
        KonvReg.EDB-System = cEDB-System and
        KonvReg.Tabell     = c2EDB-Tabell AND
        KonvReg.EkstId     = pcEkstId no-error.
      IF AVAILABLE KonvReg THEN
        FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = dec(KonvReg.InterntId) NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
      DO:
          CREATE tmpRapp.
          ASSIGN
              tmpRapp.Felt1  = ImportLinje.Felt[ 1] 
              tmpRapp.Felt2  = ImportLinje.Felt[ 2] 
              tmpRapp.Felt3  = ImportLinje.Felt[ 3] 
              tmpRapp.Felt4  = ImportLinje.Felt[13] 
              tmpRapp.Felt5  = ImportLinje.Felt[12] 
              tmpRapp.Felt6  = pcMaterial 
              tmpRapp.Felt7  = pcFarge 
              tmpRapp.Felt8  = ImportLinje.Felt[ 4] 
              tmpRapp.Felt9  = pcSortId 
              tmpRapp.Felt10 = "* Ukjent ARTIKKEL på bestillingen."
              .
          NEXT MAINLOOP.
      END.

      /* Henter relaterte poster. */
      FIND Valuta OF ArtBas NO-LOCK NO-ERROR.

      /* Bygger eksternt ID på bestillingen. */
      ASSIGN
        pcEkstId   = ImportLinje.Felt[ 1] + "|" +  
                     ImportLinje.Felt[ 2] + "|" +
                     ImportLinje.Felt[ 3] + "|" +
                     ImportLinje.Felt[12] + "|" +  
                     ImportLinje.Felt[ 5] + "|" +
                     ImportLinje.Felt[ 6] + "|" +
                     ImportLinje.Felt[ 4]
        .
      
      /* Sjekker om bestillingen er importert fra før. */
      find first KonvReg EXCLUSIVE-LOCK where
        KonvReg.EDB-System = cEDB-System and
        KonvReg.Tabell     = cEDB-Tabell AND
        KonvReg.EkstId     = pcEkstId no-error.
      /* Finnes bestillingen fra før, slettes den første importerte versjonen. */

      IF AVAILABLE KonvReg THEN
      DO:
        FIND BestHode EXCLUSIVE-LOCK WHERE
            BestHode.BestNr = int(KonvReg.InterntID) NO-ERROR.
        DELETE KonvReg.
        IF AVAILABLE BestHode THEN
        DO:
          FOR EACH BestSort OF BestHode EXCLUSIVE-LOCK:
              DELETE BestSort.
          END.
          FOR EACH BestPris OF BestHode EXCLUSIVE-LOCK:
              DELETE BestPris.
          END.
          FOR EACH BestLinje OF BestHode EXCLUSIVE-LOCK:
              DELETE BestLinje.
          END.
          FOR EACH BestHLev OF BestHode EXCLUSIVE-LOCK:
              DELETE BestHLev.
          END.
          FOR EACH BestKasse OF BestHode EXCLUSIVE-LOCK:
              DELETE BestKasse.
          END.
          FOR EACH BestLevert OF BestHode EXCLUSIVE-LOCK:
              DELETE BestLevert.
          END.
          FOR EACH BestStr OF BestHode EXCLUSIVE-LOCK:
              DELETE BestStr.
          END.
          FOR EACH FriButik OF BestHode EXCLUSIVE-LOCK:
              DELETE FriButik.
          END.
          DELETE BestHode.
        END.
      END.
      /* Setter leveringsdato */
      ASSIGN
          pdLevDato = LevDato(pdBestDato,
                          ImportLinje.Felt[4])
          NO-ERROR.

      /* Oppretter bestillingen. */
      RUN opprettbestilling.p (RECID(ArtBas),
                               TODAY,
                               pdLevDato,
                               FALSE,
                               iCl,
                               "",
                               iBrGrNr,
                               cButikkTeam,
                               OUTPUT rBestHodeRecid
                              ).

      /* Henter den opprettede bestillingen. */
      FIND BestHode EXCLUSIVE-LOCK WHERE
          RECID(BestHode) = rBestHodeRecid NO-ERROR.
      IMPORTLOGG:
      DO:
          CREATE tmpRapp.
          ASSIGN
              tmpRapp.Felt1  = ImportLinje.Felt[ 1] 
              tmpRapp.Felt2  = ImportLinje.Felt[ 2] 
              tmpRapp.Felt3  = ImportLinje.Felt[ 3] 
              tmpRapp.Felt4  = ImportLinje.Felt[13] 
              tmpRapp.Felt5  = ImportLinje.Felt[12] 
              tmpRapp.Felt6  = pcMaterial 
              tmpRapp.Felt7  = pcFarge 
              tmpRapp.Felt8  = ImportLinje.Felt[ 4] 
              tmpRapp.Felt9  = pcSortId 
              tmpRapp.Felt10 = "OK - Importert (" + STRING(BestHode.BestNr) + ")." 
              .
      END. /* IMPORTLOGG */

      /* Oppdaterer bestillingen med ekstra informasjon. */
      ASSIGN
          BestHode.EkstId      = ImportLinje.Felt[ 1] + " " +
                                 ImportLinje.Felt[ 3] + " " +
                                 ImportLinje.Felt[12] + " " +
                                 ImportLinje.Felt[ 5] + " " +
                                 ImportLinje.Felt[ 6]
          BestHode.Beskrivelse = "Imp FlexiCON "      + " " +    
                                 ImportLinje.Felt[ 1] + " " +
                                 ImportLinje.Felt[ 2] + " " +
                                 ImportLinje.Felt[ 3] + " " +
                                 ImportLinje.Felt[12] + " " +
                                 ImportLinje.Felt[ 5] + " " +
                                 pcFarge
          BestHode.LapTop      = lLapTop
          BestHode.BestillingsDato
                               = DATE(int(ENTRY(2,ImportLinje.Felt[22],"-")),
                                      int(ENTRY(3,ImportLinje.Felt[22],"-")),
                                      int(ENTRY(1,ImportLinje.Felt[22],"-"))
                                     )
          pdBestDato           = BestHode.BestillingsDato
          .

      /* Henter kalkylse som er lik Artikkels kalkyle */
      FIND FIRST BestPris OF BestHode NO-ERROR.

      /* Korrigerer kalkylen med bestillings kalkyle. */
      IF AVAILABLE BestPris THEN
        RUN Kalkyle (h_PrisKo).

      /* Oppretter koblingsposten. */
      IF NOT AVAILABLE KonvReg THEN 
      DO:
        /* Flagger at artikkelen er importert */  
        create KonvReg.
        assign
          KonvReg.EDB-System = cEDB-System 
          KonvReg.Tabell     = cEDB-Tabell   
          KonvReg.EkstId     = pcEkstId
          KonvReg.InterntID  = string(BestHode.BestNr).
      END.
      
      /* Legger opp de faste bestillingene. */
      FOR EACH tSort 
        BREAK BY tSort.SeqNr:

        IF tSort.Typ = "A" THEN
        FASTE-INNDELINGER:
        DO:
          FIND BestSort EXCLUSIVE-LOCK WHERE
              BestSort.BestNr = BestHode.BestNr AND
              BestSort.SortId = tSort.SortId NO-ERROR.
          /* Oppretter BestSort hvis den ikke finnes. */
          IF NOT AVAILABLE BestSort THEN
          OPPRETTBESTSORT:
          DO:
            ASSIGN
              pcStorrelser = ""
              .
            FOR EACH LevSAnt NO-LOCK WHERE
                LevSAnt.LevNr  = BestHode.LevNr AND
                LevSAnt.SortId = tSort.SortId:
                ASSIGN
                    pcStorrelser = pcStorrelser + 
                                   (IF pcStorrelser = ""
                                      THEN ""
                                      ELSE " ") +
                                   TRIM(LevSAnt.SoStorl).

            END.
            CREATE BestSort.
            ASSIGN
              piSum           = 0
              BestSort.BestNr = BestHode.BestNr
              BestSort.SortId = tSort.SortId
              BestSort.Storrelser = pcStorrelser
              .
            FOR EACH LevSAnt NO-LOCK WHERE
                LevSAnt.LevNr  = ArtBas.LevNr AND
                LevSAnt.SortId = tSort.SortId 
                BREAK BY LevSAnt.SeqNr:
  
              IF FIRST(LevSAnt.SeqNr) THEN
                  ASSIGN
                    BestSort.Fordeling   = ""
                    BestSort.StrInterval = TRIM(LevSAnt.SoStorl) + " -"
                    .
              IF LAST(LevSAnt.SeqNr) THEN
                  ASSIGN
                    BestSort.StrInterval = BestSort.StrInterval + " " + TRIM(LevSAnt.SoStorl)
                    .
              ASSIGN
                  piSum              = piSum + LevSAnt.SoAnt
                  BestSort.Fordeling = BestSort.Fordeling + 
                                       (IF BestSort.Fordeling <> ""
                                          THEN " " 
                                          ELSE "") + 
                                       STRING(LevSAnt.SoAnt).
            END.
            ASSIGN
              BestSort.Antall  = piSum
              .
          END. /* OPPRETTBESTSORT */
          ASSIGN
            BestSort.AntSort = BestSort.AntSort + tSort.AntSort
            .
          FOR EACH LevSAnt NO-LOCK WHERE
              LevSAnt.LevNr  = ArtBas.LevNr AND
              LevSAnt.SortId = tSort.SortId 
              BREAK BY LevSAnt.SeqNr:
            FIND BestStr EXCLUSIVE-LOCK WHERE
              BestStr.BestNr   = BestHode.BestNr AND
              BestStr.BestStat = BestHode.BestStat AND
              BestStr.Butik    = tSort.ButikkNr AND
              BestStr.Storl    = LevSAnt.SoStorl NO-ERROR.
            IF NOT AVAILABLE BestStr THEN
            DO:
              /* Oppretter BestStr */
              CREATE BestStr.
              ASSIGN
                BestStr.BestNr   = BestHode.BestNr
                BestStr.BestStat = BestHode.BestStat
                BestStr.Butik    = tSort.ButikkNr
                BestStr.Storl    = LevSAnt.SoStorl
                .
            END.
            ASSIGN    
              BestStr.Bestilt  = BestStr.Bestilt + (LevSAnt.SoAnt * tSort.AntSort)
              .
          END. 
        END. /* FASTE-INNDELINGER */
        ELSE
        FRIE-INNDELINGER:
        DO:
          FIND FIRST BestSort EXCLUSIVE-LOCK WHERE
              BestSort.BestNr = BestHode.BestNr AND
              BestSort.Fri    = TRUE NO-ERROR.
          
          /* Fikser størrelsen. */
          ASSIGN pcStorl = tSort.sortId.
          RUN FiksStorl IN wLibHandle (INPUT-OUTPUT pcStorl).

          /* Summerer opp i matrisen */
          ASSIGN
            pi2Loop = LOOKUP(TRIM(pcStorl),BestSort.Storrelser," ")
            piAntall[pi2Loop] = piAntall[pi2Loop] + tSort.AntSort
            .

          FIND FriButik EXCLUSIVE-LOCK WHERE
              FriButik.BestNr   = BestHode.BestNr AND
              Fributik.Butik    = tSort.ButikkNr AND 
              FriButik.BestStat = BestHode.BestStat NO-ERROR.
          IF NOT AVAILABLE FriButik THEN
          DO:
              CREATE FriButik.
              ASSIGN
                FriButik.BestNr   = BestHode.BestNr 
                Fributik.Butik    = tSort.ButikkNr  
                FriButik.BestStat = BestHode.BestStat 
                .
          END.
          FIND BestStr EXCLUSIVE-LOCK WHERE
            BestStr.BestNr   = BestHode.BestNr AND
            BestStr.BestStat = BestHode.BestStat AND
            BestStr.Butik    = tSort.ButikkNr AND
            BestStr.Storl    = tSort.SortId NO-ERROR.
          IF NOT AVAILABLE BestStr THEN
          DO:
            /* Oppretter BestStr */
            CREATE BestStr.
            ASSIGN
              BestStr.BestNr   = BestHode.BestNr
              BestStr.BestStat = BestHode.BestStat
              BestStr.Butik    = tSort.ButikkNr
              BestStr.Storl    = tSort.SortId
              .
          END.
          ASSIGN    
            BestStr.Bestilt  = BestStr.Bestilt + tSort.AntSort
            .

          /* Legger på plass inndelingen */
          IF LAST(tSort.SeqNr) THEN
          DO:
            DO pi2Loop = 1 TO NUM-ENTRIES(BestSort.Storrelser," "):
              ASSIGN
                FriButik.TotAntal          = FriButik.TotAntal + piAntall[pi2Loop]
                FriButik.FriAntal[pi2Loop] = piAntall[pi2Loop]
                .
            END.
          END.
        END. /* FRIE-INNDELINGER */
      END. /* tSort */

      {sww.i}
      RUN bytbeststatus.p (rBestHodeRecid, "+",0).
      {swn.i}                           
    END. /* BESTILLING */
  END. /* MAINLOOP */

  IF VALID-HANDLE(h_PrisKo) THEN
      DELETE PROCEDURE h_PrisKo.

  /* Utskrift av feilrapport. */
  IF CAN-FIND(FIRST tmpRapp) THEN
      RUN Feilliste.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkyle C-Win 
PROCEDURE Kalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ph_PrisKo AS HANDLE NO-UNDO.

  ASSIGN
    pcSkjerm = string(BestPris.ValPris) + ";" +
               string(BestPris.InnKjopsPris) + ";" +
               string(BestPris.Rab1Kr) + ";" +
               string(BestPris.Rab1%) + ";" +
               string(BestPris.Rab2Kr) + ";" +
               string(BestPris.Rab2%) + ";" +
               string(BestPris.Frakt) + ";" +
               string(BestPris.Frakt%) + ";" +
               string(BestPris.DivKostKr) + ";" +
               string(BestPris.DivKost%) + ";" +
               string(BestPris.Rab3Kr) + ";" +
               string(BestPris.Rab3%) + ";" +
               string(BestPris.VareKost) + ";" +
               string(BestPris.MvaKr) + ";" +
               string(BestPris.Mva%) + ";" +
               string(BestPris.DBKr) + ";" +
               string(BestPris.DB%) + ";" +
               string(BestPris.Pris) + ";" +
               string(BestPris.EuroPris) + ";" +
               STRING(BestPris.EuroManuel) + ";" + 
               ";0;;;0;0;yes"
               .

  /* ValutaPris */
  pcSkjerm = ByttElement(input pcSkjerm,
                  input 1,
                  input string(pdcValPris),
                  input ";").                          

  /* Starter omkalkulering.                              */
  run Omregning IN ph_PrisKo
      (input recid(ArtBas),
       input BestPris.ProfilNr,
       input-output pcSkjerm,
       input BestPris.Mva%,
       input Valuta.ValKurs,
       input 1,
       input false).
  ASSIGN
      pcSkjerm = pcSkjerm + ";yes;0;;;0;0;yes".

  /*
  /* Innpris */
  pcSkjerm = ByttElement(input pcSkjerm,
                  input 2,
                  input string(pdcInnPris),
                  input ";").                          

  /* Starter omkalkulering.                              */
  run Omregning IN ph_PrisKo
      (input recid(ArtBas),
       input BestPris.ProfilNr,
       input-output pcSkjerm,
       input BestPris.Mva%,
       input Valuta.ValKurs,
       input 2,
       input false).
  ASSIGN
      pcSkjerm = pcSkjerm + ";yes;0;;;0;0;yes".
  */

  /* Rabatt% 1 */
  pcSkjerm = ByttElement(input pcSkjerm,
                  input 4,
                  input string(pdcRab1%),
                  input ";").                          

  /* Starter omkalkulering.                              */
  run Omregning IN ph_PrisKo
      (input recid(ArtBas),
       input BestPris.ProfilNr,
       input-output pcSkjerm,
       input BestPris.Mva%,
       input Valuta.ValKurs,
       input 4,
       input false).
  ASSIGN
      pcSkjerm = pcSkjerm + ";yes;0;;;0;0;yes".

  ASSIGN
    BestPris.ValPris      = dec(ENTRY(1,pcSkjerm,";"))
    BestPris.InnKjopsPris = dec(ENTRY(2,pcSkjerm,";"))
    BestPris.Rab1Kr       = dec(ENTRY(3,pcSkjerm,";"))
    BestPris.Rab1%        = dec(ENTRY(4,pcSkjerm,";"))
    BestPris.Rab2Kr       = dec(ENTRY(5,pcSkjerm,";"))
    BestPris.Rab2%        = dec(ENTRY(6,pcSkjerm,";"))
    BestPris.Frakt        = dec(ENTRY(7,pcSkjerm,";"))
    BestPris.Frakt%       = dec(ENTRY(8,pcSkjerm,";"))
    BestPris.DivKostKr    = dec(ENTRY(9,pcSkjerm,";"))
    BestPris.DivKost%     = dec(ENTRY(10,pcSkjerm,";"))
    BestPris.Rab3Kr       = dec(ENTRY(11,pcSkjerm,";"))
    BestPris.Rab3%        = dec(ENTRY(12,pcSkjerm,";"))
    BestPris.VareKost     = dec(ENTRY(13,pcSkjerm,";"))
    BestPris.MvaKr        = dec(ENTRY(14,pcSkjerm,";"))
    BestPris.Mva%         = dec(ENTRY(15,pcSkjerm,";"))
    BestPris.DBKr         = dec(ENTRY(16,pcSkjerm,";"))
    BestPris.DB%          = dec(ENTRY(17,pcSkjerm,";"))
    BestPris.Pris         = dec(ENTRY(18,pcSkjerm,";"))
    BestPris.EuroPris     = dec(ENTRY(19,pcSkjerm,";"))
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettPara C-Win 
PROCEDURE SettPara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plBestNye       AS LOG NO-UNDO.
  DEF INPUT PARAMETER plBestOverskriv AS LOG NO-UNDO.
  
  ASSIGN
    lBestNye       = plBestNye      
    lBestOverskriv = plBestOverskriv
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartImport C-Win 
PROCEDURE StartImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN Importer.
  RUN StoppImport.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StoppImport C-Win 
PROCEDURE StoppImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "close" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ByttElement C-Win 
FUNCTION ByttElement RETURNS CHARACTER
  ( input ipSkjerm as char,
    input ipElement as int,
    input ipNyttElement as char,
    input ipDelimiter as char) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var ipLoop  as int no-undo.
  def var ipTekst as char no-undo.
  
  ipTekst = "".
  do ipLoop = 1 to num-entries(ipSkjerm,ipDelimiter):
    assign ipTekst = ipTekst + 
           (if ipTekst = ""
              then ""
              else ipDelimiter) +
           (if ipLoop = ipElement 
              then ipNyttElement
              else entry(ipLoop,ipSkjerm,ipDelimiter)). 
  end.

  RETURN ipTekst.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LevDato C-Win 
FUNCTION LevDato RETURNS DATE
  ( pdBDato AS date,
    pcDato AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR pdDato AS DATE NO-UNDO.
  DEF VAR piLoop AS INT  NO-UNDO.
  
  /* Ugyldig år/uke angivelse */
  IF LENGTH(pcDato) <> 3 THEN
    pdDato = ?.
  ELSE 
  LOOPEN:
  DO:
    /* Sjekker for ugyldige tegn */
    DO piLoop = 1 TO 3:
      IF NOT CAN-DO("0,1,2,3,4,5,6,7,8,9",SUBSTRING(pcDato,piLoop,1)) THEN
      DO:
          pdDato = ?.
          LEAVE LOOPEN.
      END.
    END.
    /* Konverterer dato - Den kommer på formen MMÅ - Måned og år.*/
    ASSIGN
      pdDato = date(
                    INT(SUBSTRING(pcDato,1,2)),           /* Måned */
                    (INT(SUBSTRING(pcDato,3,1)) * 7) - 3, /* Antall uker * dager i en uke - 3 dager) */
                    year(pdBDato)                         /* År fra bestillingsdato. */
                   )
                   .  
  END. /* LOOPEN */

  /* Sjekker at leveringsdato ikke blir mindre enn bestillingsdato. */
  DO WHILE pdDato < pdBDato:
      ASSIGN
          pdDato = pdDato + 1
          .
  END.

  RETURN pdDato.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

