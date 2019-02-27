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
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cTypeListe  AS CHAR INIT "" NO-UNDO. /* A = aggregerad */
    DEFINE VAR cOrdreListe AS CHAR INIT "3452,3452" NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cTypeListe  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER cOrdreListe AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cPdfFil AS CHAR NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
    DEFINE VARIABLE iCentralLager LIKE Butiker.Butik    NO-UNDO.
    DEFINE VARIABLE iCLProfilNr   LIKE Butiker.ProfilNr NO-UNDO.
    DEFINE VARIABLE cFirmaNavn    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrgLabels    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iPageSize     AS INTEGER INIT 64   NO-UNDO.
    DEFINE VARIABLE cVisSjerm     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cLogo         AS CHAR       NO-UNDO.
/* col variabler */
    DEFINE VARIABLE iColSortId      AS INTEGER INIT  6 NO-UNDO.
    DEFINE VARIABLE iColAntSortFRI  AS INTEGER INIT  6 NO-UNDO.
    DEFINE VARIABLE iColAntSort     AS INTEGER INIT 18 NO-UNDO.
    DEFINE VARIABLE iColGanger      AS INTEGER INIT 21 NO-UNDO.
    DEFINE VARIABLE iColAntall      AS INTEGER INIT 23 NO-UNDO.
    DEFINE VARIABLE iColStrInterval AS INTEGER INIT 27 NO-UNDO.
    DEFINE VARIABLE iColFordeling   AS INTEGER INIT 36 NO-UNDO.
    DEFINE VARIABLE iColTotAntal    AS INTEGER INIT 57 NO-UNDO.
    DEFINE VARIABLE iColRab1%       AS INTEGER INIT 61 NO-UNDO.
    DEFINE VARIABLE iColValPris     AS INTEGER INIT 66 NO-UNDO.
/* end col variabler */
    DEFINE TEMP-TABLE TT_OrdreHode
        FIELD OrdreNr       LIKE Ordre.Ordrenr
        FIELD SendtDato     LIKE Ordre.SendtDato
        FIELD LevNamn       LIKE LevBas.levnamn
        FIELD levadr        LIKE LevBas.levadr 
        FIELD levponr       LIKE LevBas.levponr
        FIELD levpadr       LIKE LevBas.levpadr
        FIELD levland       LIKE LevBas.levland
        FIELD levlabels     AS   CHARACTER
        FIELD CLButNamn     LIKE Butiker.ButNamn
        FIELD CLLevAdresse1 LIKE Butiker.LevAdresse1 
        FIELD CLLevAdresse2 LIKE Butiker.LevAdresse2 
        FIELD CLLevPostBoks LIKE Butiker.LevPostBoks 
        FIELD CLLevPostNr   LIKE Butiker.LevPostNr
        FIELD CLLevPostAdr  AS CHAR FORMAT "X(20)"
        FIELD CLLevKontakt  LIKE Butiker.LevKontakt
        FIELD CLLevMerknad  LIKE Butiker.LevMerknad
        FIELD CLLevTelefon  LIKE Butiker.LevTelefon
        FIELD ValutaKode    AS CHARACTER
        INDEX OrdreNr OrdreNr.
/*         FIELD BestHodeListe AS CHAR     */
/*         FIELD AvvikStatus   AS LOGICAL. */
    
    DEFINE TEMP-TABLE TT_OrdreArtBas
        FIELD OrdreNr     LIKE Ordre.Ordrenr
        FIELD ArtikkelNr  LIKE ArtBas.ArtikkelNr
        FIELD LevKod      LIKE ArtBas.LevKod
        FIELD ArtInfo     AS CHARACTER
        FIELD BildeFil    AS CHARACTER
        FIELD ValutaKode  AS CHARACTER
        INDEX OrdreNr OrdreNr ArtikkelNr.

    DEFINE TEMP-TABLE TT_PrintOrdre
       FIELD OrdreNr     LIKE Ordre.Ordrenr
       FIELD BestNr      LIKE BestHode.BestNr
       FIELD AntBestNrGrp AS INTE
       FIELD AntBestNrAv  AS INTE
       FIELD ArtikkelSlut AS LOGI
       FIELD ArtikkelNr  LIKE ArtBas.ArtikkelNr
       FIELD Direktelev  LIKE BestHode.DirekteLev
       FIELD Butik       LIKE Butiker.Butik
       FIELD SortButik   LIKE Butiker.Butik
       FIELD SortID      LIKE BestSort.SortID
       FIELD StrInterval LIKE BestSort.StrInterval
       FIELD Storrelser  LIKE BestSort.Storrelser
       FIELD Fordeling   LIKE BestSort.Fordeling
       FIELD FriFordeling LIKE BestSort.Fordeling
       FIELD AntSort     LIKE BestSort.AntSort
       FIELD Rab1%       LIKE BestPris.Rab1%
       FIELD Antall      LIKE BestSort.Antall
       FIELD TotAntall   LIKE BestSort.Antall
       FIELD Fri         AS LOGICAL
       FIELD ValPris     LIKE BestPris.ValPris
       FIELD LevDato     LIKE BestHode.LevDato
       FIELD ValutaKode  AS CHARACTER
       .
    DEFINE VARIABLE cLevNamn     LIKE levbas.levnamn  NO-UNDO.
    DEFINE VARIABLE cOrdreLabel AS CHARACTER FORMAT "X(20)" NO-UNDO.
    DEFINE VARIABLE cOrdreNr    AS CHARACTER    NO-UNDO.
    DEFINE BUFFER CLButiker FOR Butiker.
    DEFINE FRAME PageHeader
       HEADER
          "<ALIGN=BASE><FArial><R4><P12><B><C6>" cLevNamn "<C35><P24>" TRIM(cOrdreLabel)
          "<P12>" cOrdreNr "</B><C75><P10>" PAGE-NUMBER FORMAT ">>" SKIP
          "<R5><C6><FROM><R5><C78><LINE>" SKIP
          WITH PAGE-TOP STREAM-IO WIDTH 255.


    {xPrint.i}
    {runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK BUTTON-Rapport Btn_Cancel Btn_Help 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddFriAntal Dialog-Frame 
FUNCTION AddFriAntal RETURNS CHARACTER
  ( INPUT cSumFriantal AS CHARACTER,INPUT cAddFriantal AS CHARACTER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixStorrelser Dialog-Frame 
FUNCTION FixStorrelser RETURNS CHARACTER
  ( INPUT cStorrelser AS CHARACTER, INPUT cFriAntall AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtInfo Dialog-Frame 
FUNCTION getArtInfo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBildeFil Dialog-Frame 
FUNCTION getBildeFil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLabels Dialog-Frame 
FUNCTION getLabels RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPostAdr Dialog-Frame 
FUNCTION getPostAdr RETURNS CHARACTER
  ( INPUT ipPostNr AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD StripNotat Dialog-Frame 
FUNCTION StripNotat RETURNS CHARACTER
  ( INPUT cNotat AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
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

DEFINE BUTTON BUTTON-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.95 COL 54
     BUTTON-Rapport AT ROW 3.14 COL 28
     Btn_Cancel AT ROW 3.19 COL 54
     Btn_Help AT ROW 5.19 COL 54
     SPACE(12.59) SKIP(14.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME BUTTON-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Rapport Dialog-Frame
ON CHOOSE OF BUTTON-Rapport IN FRAME Dialog-Frame /* Rapport */
DO:
    RUN SkrivRapport.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Logo for ordreutskrift. */
{syspara.i 5 4 30 cLogo}
IF cLogo = "" THEN
    cLogo = "icon\orderlogo.bmp".

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {sww.i}
  {syspara.i 5 1 1 iCentralLager INT}
  FIND CLButiker WHERE CLButiker.Butik = iCentralLager NO-LOCK NO-ERROR.
  IF NOT AVAIL CLButiker THEN DO:
      MESSAGE "Centrallager savnes"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  ASSIGN iCLProfilNr = CLButiker.ProfilNr.
  {syspara.i 1 1 100 cFirmaNavn}
  RUN InitLabels.
  RUN SkapaTT_PrintOrdre.
  RUN SkrivRapport.
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
  ENABLE Btn_OK BUTTON-Rapport Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitLabels Dialog-Frame 
PROCEDURE InitLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       1 Rapportnavn           Ordre   
               2 Leveringsadresse      Leveringsadresse
               3 Tlf/Kontakt:          Tlf/Kontakt:
               4 Leverandørs adresse   Leverandør
               5 Standardinndeling     Std
               6 Antall i inndeling    Ant
               7 Antall inndelinger    Kar
               8 Leveringstid          Leveringstidspunkt
               9 Størrelsesgruppe      StrGrp
              10 Fordeling antall par  Fordeling
              11 Valutapris            V-Pris
              12 Rabatt%               Rab%
              13 Totalt antall par     AntPar
              14 Sum                   Sum
              15 Skrevet Id/Dato/Tid   Skrevet:
              16 Side                  Side:
              17 Totalsum              Totalt
              18 Leveringssted         Leveringssted:
              19 Varegruppe            Vg:
              20 Sesong                Sesong:
              21 Fargekode             Farge:
              22 Material              Material:
              23 Hæl                   Hæl:
              24 For                   For:
              25 Overdel               Overdel:
              26 Slitesåle             Slitesåle:
              27 Læst                  Læst:
              28 Bruksområde           Bruksområde:
              29 Egen referanse        Ref.nr:
              30 Fakturaadresse        Fakt.adresse
------------------------------------------------------------------------------*/
    ASSIGN cOrgLabels = FILL(CHR(1),29). /* 30 entries */ 
    FOR EACH SysPara WHERE SysPara.SysHId = 6 AND SysPara.SysGr = 105 AND SysPara.ParaNr <= 30:
        ASSIGN ENTRY(SysPara.ParaNr,cOrgLabels,CHR(1)) = TRIM(SysPara.Parameter1).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KalkylerGrupper Dialog-Frame 
PROCEDURE KalkylerGrupper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntBestNr      AS INTEGER    NO-UNDO.
  DEFINE VARIABLE rPO             AS ROWID      NO-UNDO.
  DEFINE VARIABLE iFgAntBestNrGrp AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntBestNrAv    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lArtikkelSlut   AS LOGICAL    NO-UNDO.
  DEFINE BUFFER   bTT_PrintOrdre FOR TT_PrintOrdre.

  FOR EACH TT_OrdreHode BREAK BY TT_OrdreHode.OrdreNr:
    FOR EACH TT_OrdreArtBas WHERE TT_OrdreArtBas.OrdreNr = TT_OrdreHode.OrdreNr BY 
                                      TT_OrdreArtBas.ArtikkelNr:
      FOR EACH TT_PrintOrdre WHERE TT_PrintOrdre.Ordrenr = TT_OrdreHode.Ordrenr AND 
                                   TT_PrintOrdre.ArtikkelNr = TT_OrdreArtBas.ArtikkelNr
                        BREAK  BY TT_PrintOrdre.Ordrenr 
                               BY TT_PrintOrdre.SortButik
                               BY TT_PrintOrdre.ArtikkelNr
                               BY TT_PrintOrdre.BestNr
                               BY TT_PrintOrdre.Fri
                               BY TT_PrintOrdre.SortID.
          IF FIRST-OF(TT_PrintOrdre.BestNr) THEN DO:
              ASSIGN rPO        = ROWID(TT_PrintOrdre)
                     iAntBestNr = 0.
          END.
          ASSIGN iAntBestNr = iAntBestNr + 1.
          IF LAST-OF(TT_PrintOrdre.BestNr) THEN DO:
              FIND bTT_PrintOrdre WHERE ROWID(bTT_PrintOrdre) = rPO.
              ASSIGN bTT_PrintOrdre.AntBestNrGrp = iAntBestNr + 1 +
                       (IF LAST-OF(TT_PrintOrdre.ArtikkelNr) THEN 2 ELSE 0).
          END.
      END.
      FIND bTT_PrintOrdre WHERE ROWID(bTT_PrintOrdre) = rPO.
      ASSIGN bTT_PrintOrdre.AntBestNrGrp = bTT_PrintOrdre.AntBestNrGrp + 2
             bTT_PrintOrdre.ArtikkelSlut = TRUE.
    END.
      FOR EACH TT_PrintOrdre WHERE TT_PrintOrdre.Ordrenr = TT_OrdreHode.Ordrenr 
                               BY TT_PrintOrdre.Ordrenr 
                               BY TT_PrintOrdre.SortButik
                               BY TT_PrintOrdre.ArtikkelNr
                               BY TT_PrintOrdre.BestNr
                               BY TT_PrintOrdre.Fri
                               BY TT_PrintOrdre.SortID.
          IF TT_PrintOrdre.AntBestNrGrp = 0 THEN
              ASSIGN TT_PrintOrdre.AntBestNrGrp = iFgAntBestNrGrp - 1
                     TT_PrintOrdre.AntBestNrAv  = iAntBestNrAv
                     iFgAntBestNrGrp            = iFgAntBestNrGrp - 1
                     TT_PrintOrdre.ArtikkelSlut = lArtikkelSlut.
          ELSE
              ASSIGN TT_PrintOrdre.AntBestNrAv  = TT_PrintOrdre.AntBestNrGrp
                     iFgAntBestNrGrp            = TT_PrintOrdre.AntBestNrGrp
                     iAntBestNrAv               = TT_PrintOrdre.AntBestNrGrp
                     lArtikkelSlut              = TT_PrintOrdre.ArtikkelSlut.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NySide Dialog-Frame 
PROCEDURE NySide :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
  PAGE.
  VIEW FRAME PageHeader.
  ASSIGN ioRad = 5.
  RUN SkrivDato(INPUT-OUTPUT ioRad).
  ASSIGN ioRad = ioRad + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_PrintOrdre Dialog-Frame 
PROCEDURE SkapaTT_PrintOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntOrdre    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFriAntal    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iTotFriAntal AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cReturString AS CHARACTER  NO-UNDO.
  DEF BUFFER bTT_PrintOrdre FOR TT_PrintOrdre.

  DO iAntOrdre = 1 TO NUM-ENTRIES(cOrdreListe):
    FIND Ordre WHERE Ordre.OrdreNr = INT(ENTRY(iAntOrdre,cOrdreListe)) NO-LOCK NO-ERROR.
    IF NOT AVAIL Ordre THEN
        NEXT.
    FIND LevBas OF Ordre NO-LOCK NO-ERROR.
    RELEASE TT_OrdreHode.
    CREATE TT_OrdreHode.
    ASSIGN TT_OrdreHode.OrdreNr       = Ordre.OrdreNr
           TT_OrdreHode.SendtDato     = Ordre.SendtDato
           TT_OrdreHode.LevNamn       = LevBas.LevNamn
           TT_OrdreHode.levadr        = LevBas.levadr 
           TT_OrdreHode.levponr       = LevBas.levponr 
           TT_OrdreHode.levpadr       = LevBas.levpadr 
           TT_OrdreHode.levland       = LevBas.levland
           .
    IF Ordre.LevAdresse1 <> "" THEN
        ASSIGN TT_OrdreHode.CLButNamn     = CLButiker.ButNamn
               TT_OrdreHode.CLLevAdresse1 = Ordre.LevAdresse1
               TT_OrdreHode.CLLevAdresse2 = Ordre.LevAdresse2
               TT_OrdreHode.CLLevPostBoks = Ordre.LevPostBoks
               TT_OrdreHode.CLLevPostNr   = Ordre.LevPostNr
               TT_OrdreHode.CLLevKontakt  = Ordre.LevKontakt
               TT_OrdreHode.CLLevMerknad  = Ordre.LevMerknad
               TT_OrdreHode.CLLevTelefon  = Ordre.LevTelefon
               .
    ELSE IF CLButiker.Levadresse1 <> "" THEN
        ASSIGN TT_OrdreHode.CLButNamn     = CLButiker.ButNamn
               TT_OrdreHode.CLLevAdresse1 = CLButiker.LevAdresse1
               TT_OrdreHode.CLLevAdresse2 = CLButiker.LevAdresse2
               TT_OrdreHode.CLLevPostBoks = CLButiker.LevPostBoks
               TT_OrdreHode.CLLevPostNr   = CLButiker.LevPostNr
               TT_OrdreHode.CLLevKontakt  = CLButiker.LevKontakt
               TT_OrdreHode.CLLevMerknad  = CLButiker.LevMerknad
               TT_OrdreHode.CLLevTelefon  = IF CLButiker.LevTelefon <> "" THEN CLButiker.LevTelefon
                                              ELSE CLButiker.BuTel
               .
    ELSE 
        ASSIGN TT_OrdreHode.CLButNamn     = CLButiker.ButNamn
               TT_OrdreHode.CLLevAdresse1 = CLButiker.BuAdr
               TT_OrdreHode.CLLevPostNr   = CLButiker.BuPonr
               TT_OrdreHode.CLLevKontakt  = IF CLButiker.LevKontakt <> "" THEN CLButiker.LevKontakt
                                             ELSE CLButiker.BuKon
               TT_OrdreHode.CLLevMerknad  = CLButiker.LevMerknad
               TT_OrdreHode.CLLevTelefon  = IF CLButiker.LevTelefon <> "" THEN CLButiker.LevTelefon
                                              ELSE CLButiker.BuTel
               .
    ASSIGN TT_OrdreHode.CLLevPostAdr  = getPostAdr(TT_OrdreHode.CLLevPostAdr)
           TT_OrdreHode.levlabels     = getLabels().
/*            BestHodeListe */
/*            AvvikStatus   */
    FOR EACH BestHode WHERE BestHode.OrdreNr = TT_OrdreHode.OrdreNr NO-LOCK.
/*       ASSIGN TT_OrdreHode.BestHodeListe     = TT_OrdreHode.BestHodeListe + */
/*              (IF TT_OrdreHode.BestHodeListe = "" THEN "" ELSE ",") +       */
/*              STRING(BestHode.BestNr).                                      */
/*       IF TT_OrdreHode.AvvikStatus = FALSE AND BestHode.BestStat > 4 THEN */
/*           ASSIGN TT_OrdreHode.AvvikStatus = TRUE.                        */
      FIND ArtBas OF BestHode NO-LOCK NO-ERROR.
      IF AVAIL ArtBas THEN DO:
        FIND TT_OrdreArtBas WHERE TT_OrdreArtBas.OrdreNr    = BestHode.OrdreNr AND
                                  TT_OrdreArtBas.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
        IF NOT AVAIL TT_OrdreArtBas THEN DO:
            RELEASE TT_OrdreArtBas.
            CREATE TT_OrdreArtBas.
            ASSIGN TT_OrdreArtBas.OrdreNr    = BestHode.OrdreNr
                   TT_OrdreArtBas.ArtikkelNr = BestHode.ArtikkelNr
                   TT_OrdreArtBas.LevKod     = ArtBas.LevKod
                .
            ASSIGN TT_OrdreArtBas.ArtInfo    = getArtInfo()
                   TT_OrdreArtBas.BildeFil   = getBildeFil(ArtBas.Bildnr)
                   TT_OrdreArtBas.ValutaKod  = TT_OrdreArtBas.ValutaKod +
                         IF LOOKUP(ArtBas.valkod,TT_OrdreArtBas.ValutaKod,CHR(1)) <> 0 THEN "" 
                              ELSE (IF TT_OrdreArtBas.ValutaKod = "" THEN "" ELSE CHR(1)) + CAPS(ArtBas.valkod).
                   TT_OrdreHode.ValutaKod  = TT_OrdreHode.ValutaKod +
                         IF LOOKUP(ArtBas.valkod,TT_OrdreHode.ValutaKod,CHR(1)) <> 0 THEN "" 
                              ELSE (IF TT_OrdreHode.ValutaKod = "" THEN "" ELSE CHR(1)) + CAPS(ArtBas.valkod).
        END.
        IF BestHode.DirekteLev = FALSE THEN DO:
            FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr AND
                                BestPris.BestStat = BestHode.BestStat AND
                                BestPris.ProfilNr = iCLProfilNr NO-LOCK NO-ERROR.
            IF NOT AVAIL BestPris THEN
                NEXT.
            FOR EACH BestSort OF BestHode WHERE BestSort.Fri = FALSE NO-LOCK:
/*               FIND bTT_PrintOrdre WHERE                                                      */
/*                      bTT_PrintOrdre.OrdreNr      = BestHode.OrdreNr  AND                     */
/*                      bTT_PrintOrdre.ArtikkelNr   = ArtBas.ArtikkelNr AND                     */
/*                      bTT_PrintOrdre.SortID       = BestSort.SortId   AND                     */
/*                      bTT_PrintOrdre.StrInterval  = TRIM(BestSort.StrInterval) AND            */
/*                      bTT_PrintOrdre.Storrelser   = TRIM(BestSort.Storrelser)  AND            */
/*                      bTT_PrintOrdre.Fordeling    = TRIM(BestSort.Fordeling)   AND            */
/*                      bTT_PrintOrdre.Butik        = iCentralLager AND                         */
/*                      bTT_PrintOrdre.Rab1%        = BestPris.Rab1% AND                        */
/*                      bTT_PrintOrdre.ValPris      = BestPris.ValPris NO-ERROR.                */
/*               IF AVAIL bTT_PrintOrdre THEN                                                   */
/*                   ASSIGN bTT_PrintOrdre.AntSort = bTT_PrintOrdre.AntSort + BestSort.AntSort. */
/*               ELSE                                                                           */
              DO:
                CREATE TT_PrintOrdre.
                ASSIGN TT_PrintOrdre.OrdreNr      = BestHode.OrdreNr
                       TT_PrintOrdre.BestNr       = BestHode.BestNr
                       TT_PrintOrdre.ArtikkelNr   = ArtBas.ArtikkelNr
                       TT_PrintOrdre.Direktelev   = BestHode.DirekteLev
                       TT_PrintOrdre.SortID       = BestSort.SortId
                       TT_PrintOrdre.StrInterval  = REPLACE(TRIM(BestSort.StrInterval)," ", "")
                       TT_PrintOrdre.Storrelser   = TRIM(BestSort.Storrelser)
                       TT_PrintOrdre.Fordeling    = TRIM(BestSort.Fordeling)
                       TT_PrintOrdre.AntSort      = BestSort.AntSort
                       TT_PrintOrdre.Antall       = BestSort.Antall
                       TT_PrintOrdre.TotAntall    = BestSort.Antall * BestSort.AntSort
                       TT_PrintOrdre.Butik        = iCentralLager
                       TT_PrintOrdre.SortButik    = 0
                       TT_PrintOrdre.Rab1%        = BestPris.Rab1%
                       TT_PrintOrdre.ValPris      = BestPris.ValPris
                       TT_PrintOrdre.LevDato      = BestHode.LevDato
                       TT_PrintOrdre.ValutaKod    = ArtBas.valkod.
                RELEASE TT_PrintOrdre.
              END.
            END.
        END.
        ELSE IF BestHode.DirekteLev = TRUE THEN DO:
          FOR EACH BestSort OF BestHode WHERE BestSort.Fri = FALSE NO-LOCK:
            FOR EACH BestKasse OF BestSort NO-LOCK:
                FIND Butiker WHERE Butiker.Butik = BestKasse.Butik NO-LOCK NO-ERROR.
                IF NOT AVAIL Butiker THEN
                    NEXT.
                FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr AND
                                    BestPris.BestStat = BestHode.BestStat AND
                                    BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
                IF NOT AVAIL BestPris THEN
                    FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr AND
                                        BestPris.BestStat = BestHode.BestStat AND
                                        BestPris.ProfilNr = iCLProfilNr NO-LOCK NO-ERROR.
                IF NOT AVAIL BestPris THEN
                    NEXT.
                RELEASE bTT_PrintOrdre.
                IF cTypeListe = "A" THEN 
                  FIND FIRST bTT_PrintOrdre WHERE
                       bTT_PrintOrdre.OrdreNr      = BestHode.OrdreNr    AND
                       bTT_PrintOrdre.BestNr       = BestHode.BestNr     AND
                       bTT_PrintOrdre.ArtikkelNr   = ArtBas.ArtikkelNr   AND
                       bTT_PrintOrdre.SortButik    = 0                   AND
                       bTT_PrintOrdre.SortID       = BestSort.SortId     AND 
                       bTT_PrintOrdre.StrInterval  = REPLACE(TRIM(BestSort.StrInterval)," ", "") AND
                       bTT_PrintOrdre.Storrelser   = TRIM(BestSort.Storrelser) AND
                       bTT_PrintOrdre.Fordeling    = TRIM(BestSort.Fordeling)  AND
                       bTT_PrintOrdre.Antall       = BestSort.Antall           AND
                       bTT_PrintOrdre.Rab1%        = BestPris.Rab1%            AND
                       bTT_PrintOrdre.ValPris      = BestPris.ValPris          AND
                       bTT_PrintOrdre.ValutaKod    = ArtBas.valkod NO-ERROR.
                IF AVAIL bTT_PrintOrdre THEN
                    ASSIGN bTT_PrintOrdre.AntSort = bTT_PrintOrdre.AntSort + BestKasse.Antal
                           bTT_PrintOrdre.TotAntall = bTT_PrintOrdre.TotAntall +
                              BestKasse.Antal * BestSort.Antall.
                ELSE DO:
                  CREATE TT_PrintOrdre.
                  ASSIGN TT_PrintOrdre.OrdreNr      = BestHode.OrdreNr
                         TT_PrintOrdre.BestNr       = BestHode.BestNr
                         TT_PrintOrdre.ArtikkelNr   = ArtBas.ArtikkelNr
                         TT_PrintOrdre.Direktelev   = BestHode.DirekteLev
                         TT_PrintOrdre.SortID       = BestSort.SortId
                         TT_PrintOrdre.StrInterval  = REPLACE(TRIM(BestSort.StrInterval)," ", "")
                         TT_PrintOrdre.Storrelser   = TRIM(BestSort.Storrelser)
                         TT_PrintOrdre.Fordeling    = TRIM(BestSort.Fordeling)
                         TT_PrintOrdre.AntSort      = BestKasse.Antal
                         TT_PrintOrdre.Antall       = BestSort.Antall
                         TT_PrintOrdre.TotAntall    = BestSort.Antall * BestKasse.Antal
                         TT_PrintOrdre.Butik        = IF cTypeListe = "A" THEN iCentralLager
                                                        ELSE BestKasse.Butik
                         TT_PrintOrdre.SortButik    = IF BestKasse.Butik = iCentralLager 
                                                      OR cTypeListe = "A" THEN 0
                                                           ELSE BestKasse.Butik
                         TT_PrintOrdre.Rab1%        = BestPris.Rab1%
                         TT_PrintOrdre.ValPris      = BestPris.ValPris
                         TT_PrintOrdre.LevDato      = BestHode.LevDato
                         TT_PrintOrdre.ValutaKod    = ArtBas.valkod.
                  RELEASE TT_PrintOrdre.
              END.
            END.
          END.
        END.
        IF CAN-FIND(FIRST FriButik OF BestHode) THEN DO:
            FIND BestSort OF BestHode WHERE BestSort.Fri = TRUE NO-LOCK NO-ERROR.
            IF AVAIL BestSort THEN DO:
                FOR EACH Fributik OF BestHode NO-LOCK.
                    ASSIGN cFriAntal = "0" + 
                                FILL(" 0",NUM-ENTRIES(BestSort.Storrelser," ") - 1)
                           iTotFriAntal = 0.
                    DO iCount = 1 TO NUM-ENTRIES(BestSort.Storrelser," "):
                        ASSIGN ENTRY(iCount,cFriAntal," ") = 
                            STRING(INT(ENTRY(iCount,cFriAntal," ")) + FriButik.FriAntal[iCount])
                               iTotFriAntal = iTotFriAntal + FriButik.FriAntal[iCount].
                    END.

                    FIND Butiker WHERE Butiker.Butik = FriButik.Butik NO-LOCK NO-ERROR.
                    IF NOT AVAIL Butiker THEN
                        NEXT.
                    FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr AND
                                        BestPris.BestStat = BestHode.BestStat AND
                                        BestPris.ProfilNr = Butiker.ProfilNr NO-LOCK NO-ERROR.
                    IF NOT AVAIL BestPris THEN
                        FIND BestPris WHERE BestPris.BestNr   = BestHode.BestNr AND
                                            BestPris.BestStat = BestHode.BestStat AND
                                            BestPris.ProfilNr = iCLProfilNr NO-LOCK NO-ERROR.
                    IF NOT AVAIL BestPris THEN
                        NEXT.
                    RELEASE bTT_PrintOrdre.
                    IF cTypeListe = "A" THEN 
                      FIND FIRST bTT_PrintOrdre WHERE
                           bTT_PrintOrdre.OrdreNr      = BestHode.OrdreNr    AND
                           bTT_PrintOrdre.BestNr       = BestHode.BestNr     AND
                           bTT_PrintOrdre.ArtikkelNr   = ArtBas.ArtikkelNr   AND
                           bTT_PrintOrdre.Fri          = TRUE AND
                           bTT_PrintOrdre.SortButik    = 0                   AND
                           bTT_PrintOrdre.Rab1%        = BestPris.Rab1%            AND
                           bTT_PrintOrdre.ValPris      = BestPris.ValPris          AND
                           bTT_PrintOrdre.ValutaKod    = ArtBas.valkod NO-ERROR.
                    
                    IF AVAIL bTT_PrintOrdre THEN
                        ASSIGN bTT_PrintOrdre.Antall = bTT_PrintOrdre.Antall + iTotFriAntal
                               bTT_PrintOrdre.TotAntall = bTT_PrintOrdre.TotAntall + iTotFriAntal
                               bTT_PrintOrdre.FriFordeling = AddFriAntal(bTT_PrintOrdre.FriFordeling,cFriAntal).
                    ELSE DO:
                      CREATE TT_PrintOrdre.
                      ASSIGN TT_PrintOrdre.OrdreNr      = BestHode.OrdreNr
                             TT_PrintOrdre.BestNr       = BestHode.BestNr
                             TT_PrintOrdre.ArtikkelNr   = ArtBas.ArtikkelNr
                             TT_PrintOrdre.Direktelev   = TRUE
                             TT_PrintOrdre.SortID       = "FRI"
                             TT_PrintOrdre.StrInterval  = ENTRY(1,BestSort.Storrelser," ") + "-" + 
                                                          ENTRY(NUM-ENTRIES(BestSort.Storrelser," "),BestSort.Storrelser," ")
                             TT_PrintOrdre.Storrelser   = BestSort.Storrelser
                             TT_PrintOrdre.FriFordeling = cFriAntal
                             TT_PrintOrdre.AntSort      = 1
                             TT_PrintOrdre.Antall       = iTotFriAntal
                             TT_PrintOrdre.TotAntall    = iTotFriAntal
                             TT_PrintOrdre.Butik        = IF cTypeListe = "A" THEN iCentralLager
                                                            ELSE FriButik.Butik
                             TT_PrintOrdre.SortButik    = IF FriButik.Butik = iCentralLager 
                                                          OR cTypeListe = "A" THEN 0
                                                               ELSE FriButik.Butik
                             TT_PrintOrdre.Rab1%        = BestPris.Rab1%
                             TT_PrintOrdre.Fri          = TRUE
                             TT_PrintOrdre.ValPris      = BestPris.ValPris
                             TT_PrintOrdre.LevDato      = BestHode.LevDato
                             TT_PrintOrdre.ValutaKod    = ArtBas.valkod.
                    END.
                END.
            END.
        END.
      END.
    END.
  END.
  FOR EACH TT_PrintOrdre WHERE TT_PrintOrdre.Fri = TRUE:
      ASSIGN cReturString = FixStorrelser(TT_PrintOrdre.Storrelser,TT_PrintOrdre.FriFordeling)
             TT_PrintOrdre.Storrelser   = ENTRY(1,cReturString,CHR(1))
             TT_PrintOrdre.FriFordeling = ENTRY(2,cReturString,CHR(1)).
  END.
  RUN KalkylerGrupper.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivAdresseInfo Dialog-Frame 
PROCEDURE SkrivAdresseInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
  PUT UNFORMATTED
      "<FArial><R" STRING(ioRad + 1) "><C6><B><U>" ENTRY(4,TT_OrdreHode.levlabels,CHR(1)) "<C40>" ENTRY(30,TT_OrdreHode.levlabels,CHR(1)) "</B></U>" SKIP
      "<R" STRING(ioRad + 2) "><C6><B>" TT_OrdreHode.LevNamn "<C40>" cFirmaNavn "</B>" SKIP
      "<R" STRING(ioRad + 3) "><C6>" TT_OrdreHode.Levadr "<C40>" CLButiker.BuAdr SKIP
      "<R" STRING(ioRad + 4) "><C6>" TT_OrdreHode.Levponr " " TT_OrdreHode.Levpadr 
                             "<C40>" IF getPostAdr(CLButiker.BuPonr) = "" THEN 
                             CLButiker.BuPadr ELSE CLButiker.BuPonr " " getPostAdr(CLButiker.BuPonr) SKIP
      "<R" STRING(ioRad + 5) "><C6>" TT_OrdreHode.Levland "<C40>" CLButiker.BuKon "/" CLButiker.BuTel SKIP.
  ASSIGN ioRad = ioRad + 6.
/*   RUN SkrivUnderStrek(INPUT-OUTPUT ioRad,6,78). */

/*   LevNamn */    
/*   levadr  */
/*   levponr */
/*   levpadr */
/*   levland */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivArtikkelInfo Dialog-Frame 
PROCEDURE SkrivArtikkelInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT  PARAMETER ioRad     AS INTEGER    NO-UNDO.
   DEFINE VARIABLE                iCount1   AS INTEGER    NO-UNDO.
   DEFINE VARIABLE                iPlusInfo AS INTEGER    NO-UNDO.
   PUT UNFORMATTED "<FArial><R" STRING(ioRad + 1) "><C6><B>" ENTRY(1,ENTRY(1,TT_OrdreArtBas.ArtInfo,CHR(1)),CHR(2)) "</B>" SKIP.
   IF NUM-ENTRIES(ENTRY(1,TT_OrdreArtBas.ArtInfo,CHR(1)),CHR(2)) = 2 THEN
       PUT UNFORMATTED "<R" STRING(ioRad + 2) "><C35><P8>" ENTRY(2,ENTRY(1,TT_OrdreArtBas.ArtInfo,CHR(1)),CHR(2)) "<P10>" SKIP.
   DO iCount1 = 2 TO NUM-ENTRIES(TT_OrdreArtBas.ArtInfo,CHR(1)):
       PUT UNFORMATTED "<R" STRING(ioRad + iCount1) "><C6>" ENTRY(1,ENTRY(iCount1,TT_OrdreArtBas.ArtInfo,CHR(1)),CHR(2)) SKIP.
       IF NUM-ENTRIES(ENTRY(iCount1,TT_OrdreArtBas.ArtInfo,CHR(1)),CHR(2)) = 2 THEN
           PUT UNFORMATTED "<R" STRING(ioRad + iCount1 + 1) "><C35><P8>" ENTRY(2,ENTRY(iCount1,TT_OrdreArtBas.ArtInfo,CHR(1)),CHR(2)) "<P10>" SKIP.

   END.
   IF TT_OrdreArtBas.BildeFil <> "" THEN 
       PUT UNFORMATTED
          "<TRANSPARENT=false><R" STRING(ioRad + 5)  ",5><C78><#3><R" STRING(ioRad + 1) ",25><C70><IMAGE#3=" 
          TT_OrdreArtBas.BildeFil  ">".

   ASSIGN ioRad = ioRad + IF TT_OrdreArtBas.BildeFil = "" THEN iCount1 ELSE 5.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivDato Dialog-Frame 
PROCEDURE SkrivDato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
    PUT UNFORMATTED "<FArial><C6><R" STRING(ioRad) ">" STRING(TODAY) + IF TT_OrdreHode.SendtDato <> ? THEN 
                                 " (Order sendt: " + STRING(TT_OrdreHode.SendtDato) + ")" ELSE "" SKIP.
    IF SEARCH(cLogo) <> ? THEN DO:
        ASSIGN FILE-INFO:File-NAME = cLogo.
        PUT UNFORMATTED
/*             "<TRANSPARENT=false><R" STRING(ioRad + 6) ",25><C74><#3><R" STRING(ioRad - 3) ",25><C55,2><IMAGE#3=" */
            "<TRANSPARENT=false><R2,5><C55><#3><R4,9><C75><IMAGE#3="
            FILE-INFO:FULL-PATHNAME + ">".
    END.

    ASSIGN ioRad = ioRad + 1.
/* 
   FILE-INFO:File-NAME = ".\cliparts\nrk.bmp".
  Put STREAM Skriv1 CONTROL
    "<TRANSPARENT=false><R12,5><C13><#3><R5,25><C5,2><IMAGE#3=" +
    FILE-INFO:FULL-PATHNAME + ">".

 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivLablar Dialog-Frame 
PROCEDURE SkrivLablar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
/*   5 7 6 9 10 13 11 12 17 */
      
      PUT UNFORMATTED "<FCourier NEW><B><R" STRING(ioRad)
           "><C" STRING(iColSortId) ">" ENTRY(5,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColAntSort) ">" ENTRY(7,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColAntall) ">" ENTRY(6,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColStrInterval) ">" ENTRY(9,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColFordeling) ">" ENTRY(10,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColTotAntal -
                        IF LENGTH(ENTRY(13,TT_OrdreHode.levlabels,CHR(1))) > 4 THEN
                          LENGTH(ENTRY(13,TT_OrdreHode.levlabels,CHR(1))) - 4 ELSE 0) ">" ENTRY(13,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColRab1%) ">" ENTRY(12,TT_OrdreHode.levlabels,CHR(1))
            "<C" STRING(iColValPris + 13 - LENGTH(ENTRY(11,TT_OrdreHode.levlabels,CHR(1)))) ">" ENTRY(11,TT_OrdreHode.levlabels,CHR(1)) "</B>" SKIP.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivLeveransAdresse Dialog-Frame 
PROCEDURE SkrivLeveransAdresse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad  AS INTEGER          NO-UNDO.
  DEFINE INPUT        PARAMETER ipButik LIKE Butiker.Butik NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cRad       AS CHARACTER EXTENT 8 NO-UNDO.
  ASSIGN cRad[1] = ENTRY(2,TT_OrdreHode.levlabels,CHR(1)).
  IF ipButik <> iCentralLager THEN DO:
      FIND Butiker WHERE Butiker.Butik = ipButik NO-LOCK.
      ASSIGN cRad[2] = Butiker.ButNamn
             cRad[3] = IF Butiker.LevAdresse1 <> "" THEN Butiker.LevAdresse1 ELSE Butiker.BuAdr
             cRad[4] = IF Butiker.LevAdresse1 <> "" THEN Butiker.LevAdresse2 ELSE ""
             cRad[5] = IF Butiker.LevAdresse1 <> "" THEN Butiker.LevPostBoks ELSE ""
             cRad[6] = IF Butiker.LevAdresse1 <> "" THEN 
                 (IF getPostAdr(Butiker.LevPostNr) <> "" THEN Butiker.LevPostNr + " " + getPostAdr(Butiker.LevPostNr)
                  ELSE "") ELSE
                      IF getPostAdr(Butiker.BuPoNr) <> "" THEN Butiker.BuPoNr + " " + getPostAdr(Butiker.BuPoNr) ELSE
                      Butiker.BuPadr
             cRad[7] = (IF Butiker.LevKontakt <> "" THEN Butiker.LevKontakt + "\"
                                ELSE "") + Butiker.LevTelefon
             cRad[8] = Butiker.LevMerknad.

  END.
  ELSE DO:
      IF TT_OrdreHode.CLLevAdresse1 <> CLButiker.BuAdr AND 
         TT_OrdreHode.CLLevPostNr   <> CLButiker.BuPonr THEN
          ASSIGN cRad[2] = TT_OrdreHode.CLButNamn
                 cRad[3] = TT_OrdreHode.CLLevAdresse1
                 cRad[4] = TT_OrdreHode.CLLevAdresse2
                 cRad[5] = TT_OrdreHode.CLLevPostBoks
                 cRad[6] = (IF TT_OrdreHode.CLLevPostNr <> "" THEN TT_OrdreHode.CLLevPostNr + " "
                                ELSE "") + TT_OrdreHode.CLLevPostAdr.
      ASSIGN cRad[7] = (IF TT_OrdreHode.CLLevKontakt <> "" THEN TT_OrdreHode.CLLevKontakt + "\"
                                ELSE "") + TT_OrdreHode.CLLevTelefon
             cRad[8] = TT_OrdreHode.CLLevMerknad.
             
  END.
  IF ipButik = iCentrallager THEN DO:
      ASSIGN ioRad = ioRad + 1.
      PUT UNFORMATTED "<FArial><R" STRING(ioRad) "><C60><B><U>" cRad[1] "</B></U>" SKIP.
      DO iCount = 2 TO 8:
          IF cRad[iCount] <> "" THEN DO:
              ASSIGN ioRad = ioRad + 1.
              PUT UNFORMATTED "<R" STRING(ioRad) "><C60>" cRad[iCount] SKIP.
          END.
      END.
      IF ioRad < 10 THEN
          ASSIGN ioRad = 10.
  END.
  ELSE DO:
/*       RUN SkrivUnderStrek(INPUT-OUTPUT ioRad,6,78). */
      ASSIGN ioRad = ioRad + 1.
      PUT UNFORMATTED "<FArial><R" STRING(ioRad) "><C6><B><U>" cRad[1] "</B></U>" SKIP.
      DO iCount = 2 TO 8:
          IF cRad[iCount] <> "" THEN DO:
              ASSIGN ioRad = ioRad + 1.
              PUT UNFORMATTED "<R" STRING(ioRad) "><C6>" cRad[iCount] SKIP.
          END.
      END.
      IF ioRad < 10 THEN
          ASSIGN ioRad = 10.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRapport Dialog-Frame 
PROCEDURE SkrivRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil      AS CHAR NO-UNDO.
  DEF VAR iRad           AS INTE NO-UNDO.
  DEF VAR cSendtDato     AS CHAR NO-UNDO.
  DEF VAR fArtTotalTmp   AS DECI DECIMALS 2 NO-UNDO.
  DEF VAR fArtTotal      AS DECI DECIMALS 2 NO-UNDO.
  DEF VAR cOrdreAntal    AS CHAR NO-UNDO. /* for å samle ordreAntal,ulike valutaer ett entry för varje valuta */
  DEF VAR iOrdreAntal    AS INTE NO-UNDO. /* for å summera entryna i ordreAntal ved ulike valutaer */
  DEF VAR cOrdreTotal    AS CHAR NO-UNDO. /* for å samle ordretotal ved ulike valutaer */
  DEF VAR cArtikkelTotal AS CHAR NO-UNDO. /* for å samle artikkeltotal ved ulike valutaer */
  DEF VAR iValutaIdx     AS INTE NO-UNDO.
  DEF VAR iCount         AS INTE NO-UNDO.
  IF VALID-HANDLE(wLibHandle) THEN
     RUN GetTempFileName IN wLibHandle ("Ordre", "xpr", OUTPUT pcRappFil). 
  ASSIGN cPdfFil = pcRappFil. /* REPLACE(pcRappFil,"xpr","pdf"). */
  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(80).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
/*   PUT CONTROL '<PREVIEW=ZoomToWidth>'. */
/*   PUT CONTROL '<PREVIEW=PDF>'. */
  PUT CONTROL '<SILENT=TRUE>'.
  PUT CONTROL '<PRINT=NO>'.
  /*put control "<PrinterSetup>". */
  FOR EACH TT_OrdreHode BREAK BY TT_OrdreHode.OrdreNr:
    ASSIGN cLevNamn    = TT_OrdreHode.Levnamn
           cOrdreLabel = ENTRY(1,TT_OrdreHode.levlabels,CHR(1))
           cOrdreNr    = TRIM(STRING(TT_OrdreHode.OrdreNr)).
    IF NOT FIRST(TT_OrdreHode.OrdreNr) THEN DO:
        PAGE.
        OUTPUT CLOSE.
        OUTPUT TO VALUE(pcRappFil) APPEND PAGED PAGE-SIZE VALUE(80). /* iPageSize */
        VIEW FRAME PageHeader.

    END.
    ELSE
     VIEW FRAME PageHeader.
    ASSIGN iRad = 5
           cOrdreTotal = FILL(CHR(1),NUM-ENTRIES(TT_OrdreHode.ValutaKod,CHR(1)) - 1)
           cOrdreAntal = cOrdreTotal.

    RUN SkrivDato(INPUT-OUTPUT iRad).
        
    IF cTypeListe = "" THEN DO:
        RUN SkrivAdresseInfo(INPUT-OUTPUT iRad). /* iRad = 5 */
    END.
    FOR EACH TT_PrintOrdre WHERE TT_PrintOrdre.Ordrenr = TT_OrdreHode.Ordrenr 
                        BREAK /* BY TT_PrintOrdre.Ordrenr */
                               BY TT_PrintOrdre.SortButik
                               BY TT_PrintOrdre.ArtikkelNr
                               BY TT_PrintOrdre.BestNr
                               BY TT_PrintOrdre.Fri
                               BY TT_PrintOrdre.SortID.
        ACCUMULATE Totantal (TOTAL BY BestNr BY ArtikkelNr).
        IF FIRST-OF(SortButik) THEN DO:
            IF cTypeListe = "" THEN DO:
                IF TT_PrintOrdre.Butik = iCentrallager THEN
                    ASSIGN iRad = 6.
                RUN SkrivLeveransAdresse(INPUT-OUTPUT iRad,TT_PrintOrdre.Butik).
            END.
            iRad = iRad + 1.
            RUN SkrivSpecHeader(INPUT-OUTPUT iRad).
        END.
        IF FIRST-OF(TT_PrintOrdre.ArtikkelNr) THEN DO:
            IF iRad > 56 THEN DO:
                RUN NySide(INPUT-OUTPUT iRad).
            END.
            FIND TT_OrdreArtBas WHERE TT_OrdreArtBas.OrdreNr = TT_OrdreHode.OrdreNr AND
                 TT_OrdreArtBas.ArtikkelNr = TT_PrintOrdre.ArtikkelNr.
            RUN SkrivArtikkelInfo(INPUT-OUTPUT iRad).
            ASSIGN cArtikkelTotal = FILL(CHR(1),NUM-ENTRIES(TT_OrdreArtBas.ValutaKod,CHR(1)) - 1).
        END.
        IF FIRST-OF(TT_PrintOrdre.BestNr) THEN DO:
            IF NOT FIRST(TT_PrintOrdre.BestNr) AND iRad > 58 AND TT_PrintOrdre.AntBestNrGrp > 4 THEN DO:
                RUN NySide(INPUT-OUTPUT iRad).
                RUN SkrivSpecHeader(INPUT-OUTPUT iRad).
            END.
            RUN SkrivRefnr(INPUT-OUTPUT iRad,"").
        END.
/*         IF iPageSize - iRad < TT_PrintOrdre.AntBestNrAv - TT_PrintOrdre.AntBestNrGrp THEN DO: */
        IF iRad > 59 AND TT_PrintOrdre.AntBestNrGrp > 3 THEN DO:
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) "><C60>= = = =>" /* string(iRad) */ SKIP.
            RUN NySide(INPUT-OUTPUT iRad).
            RUN SkrivSpecHeader(INPUT-OUTPUT iRad).
            RUN SkrivRefnr(INPUT-OUTPUT iRad,"()").
        END.
        IF TT_PrintOrdre.Fri = FALSE THEN DO:
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
                          "><C" STRING(iColSortId)      ">" SortId 
                           "<C" STRING(iColAntSort)     ">" STRING(AntSort,"zz9") 
                           "<C" STRING(iColGanger)      ">" "*" 
                           "<C" STRING(iColAntall)      ">" STRING(Antall,"zz9")
                           "<C" STRING(iColStrInterval) ">" StrInterval 
                           "<C" STRING(iColFordeling)   ">" Fordeling
                           "<C" STRING(iColTotAntal)    ">" STRING(TotAntal,"zzz9")
                           "<C" STRING(iColValPris)     ">" STRING(ValPris,">>>,>>>,>>9.99") "</B>" SKIP.
            ASSIGN iRad = iRad + 1.
        END.
        ELSE DO: /* TT_PrintOrdre.Fri = TRUE */ 
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) 
/*                 "><C" STRING(iColSortId)      ">" SortId */
                 "><C" STRING(iColAntSortFRI)  "><P9>" Storrelser "<P10>"
                 "<C" STRING(iColTotAntal)    ">" STRING(TotAntal,"zzz9") 
                 "<C" STRING(iColValPris)     ">" STRING(ValPris,">>>,>>>,>>9.99") SKIP
                 "<R" STRING(iRad + 1) "><C" STRING(iColAntSortFRI) "><P9>" FriFordeling "<P10></B>" SKIP.
            ASSIGN iRad = iRad + IF NOT LAST-OF(TT_PrintOrdre.BestNr) THEN 2 ELSE 1.
        END.
        IF LAST-OF(TT_PrintOrdre.BestNr) THEN DO:
            ASSIGN iRad = iRad - 1
                   fArtTotalTmp = (ACCUM TOTAL BY BestNr TotAntal) * ValPris * (1 - (Rab1% / 100))
                   fArtTotal    = fArtTotal + fArtTotalTmp
                   iValutaIdx   = LOOKUP(TT_PrintOrdre.ValutaKod,TT_OrdreArtBas.ValutaKod,CHR(1))
                   ENTRY(iValutaIdx,cArtikkelTotal,CHR(1)) = 
                                 STRING(DECI(ENTRY(iValutaIdx,cArtikkelTotal,CHR(1))) + fArtTotalTmp)
                   iValutaIdx   = LOOKUP(TT_PrintOrdre.ValutaKod,TT_OrdreHode.ValutaKod,CHR(1))
                   ENTRY(iValutaIdx,cOrdreAntal,CHR(1)) = 
                              STRING(INT(ENTRY(iValutaIdx,cOrdreAntal,CHR(1))) + ACCUM TOTAL BY BestNr TotAntal).
                   ENTRY(iValutaIdx,cOrdreTotal,CHR(1)) = 
                                 STRING(DECI(ENTRY(iValutaIdx,cOrdreTotal,CHR(1))) + fArtTotalTmp).
            RUN SkrivUnderStrek(INPUT-OUTPUT iRad,iColTotAntal - 1,60).
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) "><C" STRING(iColTotAntal - 1) ">" STRING(ACCUM TOTAL BY BestNr TotAntal,">,>>9")
                "<C" STRING(iColRab1%)  ">" STRING(TT_PrintOrdre.Rab1%,">>9.9")
                "<C" STRING(iColValPris + 12 - LENGTH(TRIM(STRING(fArtTotalTmp,">>>,>>>,>>9.99")))) ">(" 
                    TRIM(STRING(fArtTotalTmp,">>>,>>>,>>9.99")) ")</B>" SKIP.
            ASSIGN iRad = iRad + 1 /* IF TT_PrintOrdre.Fri THEN 1 ELSE 0 */.
        END.
        IF LAST-OF(TT_PrintOrdre.ArtikkelNr) THEN DO:
            RUN SkrivUnderStrek(INPUT-OUTPUT iRad,iColTotAntal - 1,iColValPris + 12).
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) "><C" STRING(iColTotAntal - 2) ">" STRING(ACCUM TOTAL BY TT_PrintOrdre.ArtikkelNr TotAntal,">>,>>9")
                 "<C" STRING(iColRab1%)  ">" /* ENTRY(1,TT_OrdreArtBas.ValutaKod,CHR(1)) */
                 "<C" STRING(iColValPris) ">" STRING(DECI(ENTRY(1,cArtikkelTotal,CHR(1))),">>>,>>>,>>9.99")
                                                     ENTRY(1,TT_OrdreArtBas.ValutaKod,CHR(1)) "</B>" SKIP.
            ASSIGN iRad      = iRad + 1
                   fArtTotal = 0.
            DO iCount = 2 TO NUM-ENTRIES(cArtikkelTotal,CHR(1)):
                PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) "><C" STRING(iColRab1%) ">" ENTRY(iCount,TT_OrdreArtBas.ValutaKod,CHR(1))
                    "<C" STRING(iColValPris) ">" STRING(DECI(ENTRY(iCount,cArtikkelTotal,CHR(1))),">>>,>>>,>>9.99") "</B>" 
                                               SKIP.
                ASSIGN iRad = iRad + 1.
            END.

        END.
        IF LAST(TT_PrintOrdre.SortButik) THEN DO: /* skriv ut ordertotal */
            IF iRad + 2 + NUM-ENTRIES(cOrdreTotal,CHR(1)) > iPageSize THEN DO:
                RUN NySide(INPUT-OUTPUT iRad).
            END.
            ASSIGN iRad = iRad + 2.
            PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) "><C" STRING(iColValPris) "><P14>"
                    ENTRY(1,TT_OrdreHode.levlabels,CHR(1)) "<P10></B>" SKIP.
            RUN SkrivUnderStrek(INPUT-OUTPUT iRad,iColTotAntal - 2,iColValPris + 12).
            ASSIGN iOrdreAntal = 0.
            DO iCount = 1 TO NUM-ENTRIES(cOrdreTotal,CHR(1)):
/*                 PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) "><C" STRING(iColRab1%) ">" ENTRY(iCount,TT_OrdreHode.ValutaKod,CHR(1)) */
/*                     "<C" STRING(iColValPris) ">" STRING(DECI(ENTRY(iCount,cOrdreTotal,CHR(1))),">>>,>>>,>>9.99")                           */
/*                                       "</B>" SKIP.                                                                                         */
                PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) ">"
                    "<C" STRING(iColTotAntal - 2)    ">" STRING(INT(ENTRY(iCount,cOrdreAntal,CHR(1))),">>,>>9")
                    "<C" STRING(iColValPris) ">" STRING(DECI(ENTRY(iCount,cOrdreTotal,CHR(1))),">>>,>>>,>>9.99")
                                                 ENTRY(iCount,TT_OrdreHode.ValutaKod,CHR(1))
                                      "</B>" SKIP.
                ASSIGN iRad = iRad + 1
                       iOrdreAntal = iOrdreAntal + INT(ENTRY(iCount,cOrdreAntal,CHR(1))).
            END.
            IF NUM-ENTRIES(cOrdreTotal,CHR(1)) > 1 THEN DO:
                ASSIGN iRad = iRad - 1.
                RUN SkrivUnderStrek(INPUT-OUTPUT iRad,iColTotAntal - 2,iColValPris + 12).
                PUT UNFORMATTED "<FCourier NEW><B><R" STRING(iRad) ">"
                    "<C" STRING(iColTotAntal - 2)    ">" STRING(iOrdreAntal,">>,>>9").
            END.
        END.
        IF LAST-OF(TT_PrintOrdre.SortButik) THEN DO:
            PAGE.
            ASSIGN iRad = 3.
            IF NOT LAST(TT_PrintOrdre.SortButik) THEN DO:
                ASSIGN iRad = 5.
                RUN SkrivDato(INPUT-OUTPUT iRad).
            END.
        END.
    END.
  END.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivRefnr Dialog-Frame 
PROCEDURE SkrivRefnr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
    DEFINE INPUT        PARAMETER cOpt AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE c1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c2 AS CHARACTER NO-UNDO.
    IF cOpt = "()" THEN
        ASSIGN c1 = "(.."
               c2 = "..)".
    PUT UNFORMATTED "<FArial><R" STRING(ioRad + 1) "><C6>" c1 "RefNr " TT_PrintOrdre.BestNr 
        (IF TT_PrintOrdre.LevDato <> ? THEN " " + ENTRY(8,TT_OrdreHode.levlabels,CHR(1)) + 
                                            " " + STRING(TT_PrintOrdre.LevDato) ELSE "") c2 
        "<C" STRING(iColValPris + 7) "><B><U>" CAPS(TT_PrintOrdre.ValutaKod) "</B></U>" SKIP.
    ASSIGN ioRad = ioRad + 2.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivSpecHeader Dialog-Frame 
PROCEDURE SkrivSpecHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad  AS INTEGER          NO-UNDO.
  RUN SkrivUnderStrek(INPUT-OUTPUT ioRad,6,78).
  RUN SkrivLablar(INPUT ioRad).
  RUN SkrivUnderStrek(INPUT-OUTPUT ioRad,6,78).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivUnderstrek Dialog-Frame 
PROCEDURE SkrivUnderstrek :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER ioRad AS INTEGER    NO-UNDO.
  DEFINE INPUT        PARAMETER iFraCol AS INTEGER    NO-UNDO.
  DEFINE INPUT        PARAMETER iTilCol AS INTEGER    NO-UNDO.
  
  PUT UNFORMATTED
        SUBSTITUTE("<R&1><C&2><FROM><R&1><C&3><LINE>",STRING(ioRad + 1),STRING(iFraCol),STRING(iTilCol)) SKIP.
  ASSIGN ioRad = ioRad + 1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE XPrintRapport Dialog-Frame 
PROCEDURE XPrintRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEF VAR pcBildeFil   AS CHAR   NO-UNDO.                                                */
/*   DEF VAR piCopies     AS INT    NO-UNDO.                                                */
/*   DEF VAR pcRappFil    AS CHAR   NO-UNDO.                                                */
/*   DEF VAR pcRowIdent   AS CHAR   NO-UNDO.                                                */
/*   DEF VAR pcRowValues  AS CHAR   NO-UNDO.                                                */
/*   DEF VAR pcColValues  AS CHAR   NO-UNDO.                                                */
/*   DEF VAR pcSkadeListe AS CHAR   NO-UNDO.                                                */
/*   DEF VAR ph_Dummy     AS HANDLE NO-UNDO.                                                */
/*   DEF VAR pcRegNr      AS CHAR   NO-UNDO.                                                */
/*   DEF VAR piLoop       AS INT    NO-UNDO.                                                */
/*   DEF VAR pcTekst      AS CHAR   NO-UNDO.                                                */
/*   DEF VAR piRad        AS dec    NO-UNDO.                                                */
/*                                                                                          */
/*   DEF VAR iRad         AS INTE INIT 5 NO-UNDO.                                           */
/*   DEF VAR iButik       AS INTE INIT ? NO-UNDO.                                           */
/*   DEF VAR iKasse       AS INTE INIT ? NO-UNDO.                                           */
/*   DEF VAR cRader       AS CHAR        NO-UNDO.                                           */
/*   DEF VAR cCols        AS CHAR        NO-UNDO.                                           */
/*                                                                                          */
/*   DEF VAR iSolgtAnt    AS INTE    NO-UNDO.                                               */
/*   DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.                                               */
/*   DEF VAR iAntKunder   AS INTE    NO-UNDO.                                               */
/*                                                                                          */
/*  DO WITH FRAME {&FRAME-NAME}:                                                            */
/*   ASSIGN                                                                                 */
/*     piCopies = 1                                                                         */
/*     .                                                                                    */
/*  STATUS DEFAULT "Skriver ut, venligst vent...".                                          */
/*                                                                                          */
/*   /* Henter tempfilnavn */                                                               */
/* /*  RUN GetTmpFileName IN hLibHandle ("utk","xpr", OUTPUT pcRappFil). */                 */
/*   if valid-handle(wLibHandle) then                                                       */
/*     run GetTempFileName in wLibHandle ("AktRap", "xpr", output pcRappFil).               */
/*                                                                                          */
/*    /* Åpner stream til skriverfil. */                                                    */
/*   output TO value(pcRappFil) PAGED page-size 60.                                         */
/*   put control '<PREVIEW=ZoomToWidth>'.                                                   */
/* /*   If nCopies > 1 then                                                              */ */
/* /*     put control substitute("<x&1>", string(nCopies) ).  /* For the fun ! */        */ */
/* /*                                                                                    */ */
/* /*   If tDialog then                                                                  */ */
/* /*     put control "<Printer?>".     /* xPrint will display the Printer Dialog Box */ */ */
/* /*   If tSetup then                                                                   */ */
/*     put control "<PrinterSetup>". /* xPrint will display the Printer Setup Box */        */
/* /*          _____________________________________________________________________        */
/* */                                                                                       */
/*                                                                                          */
/*                                                                                          */
/* /*  if xLanguage <> '' then                */                                            */
/* /*     put CONTROL '<LANG=' xLanguage '>'. */                                            */
/* /*   Put control                                        */                               */
/* /*             "<UNITS=MM><LINECOLOR=BLUE></PROGRESS>". */                               */
/*   Define Frame PageBottom                                                                */
/*          header                                                                          */
/*             "<C75><P10><FArial></B>" Page-Number format ">>"  "/ <#Pages>"               */
/*             skip(6)                                                                      */
/*             with page-Top stream-io width 255.                                           */
/*   view frame PageBottom.                                                                 */
/*                                                                                          */
/*   ASSIGN cCols = "3,8.5,15.5,20.5,32.5,37.5,44.5,50,55.5,64.5,74".                                                                 */
/*                                                                                                                                    */
/*   FOR EACH tmpAktRapp                                                                                                              */
/*      BY tmpAktRapp.Butik                                                                                                           */
/*      BY tmpAktRapp.Kasse                                                                                                           */
/*      BY tmpAktRapp.Tid:                                                                                                            */
/*      IF tmpAktRapp.Butik <> iButik OR tmpAktRapp.Kasse <> iKasse THEN DO:                                                          */
/*          IF iButik <> ? THEN DO:                                                                                                   */
/*              DEF VAR iRad         AS INTE INIT 5 NO-UNDO.                                                                          */
/*              PUT UNFORMATTED "<FCourier NEW><P10><B><R" + STRING(iRad) + ">" +                                                     */
/*              "<C6>" +  "TOT" +                                                                                                     */
/*              "<C10>" + STRING(iSolgtAnt,">>,>>9")    +                                                                             */
/* /*              "<C17>" + STRING(tmpAktRapp.Solgt%Ant,">9.9")   + */                                                               */
/*              "<C22>" + STRING(dSolgtVerdi,">,>>>,>>9.99")   +                                                                      */
/* /*              "<C34>" + STRING(tmpAktRapp.Solgt%Verdi,">9.9")  + */                                                              */
/*              "<C39>" + STRING(iAntKunder,">>,>>9")    +                                                                            */
/* /*              "<C46>" + STRING(tmpAktRapp.Ant%Kunder,">9.9")   + */                                                              */
/*              "<C52>" + STRING(iSolgtAnt / iAntKunder,">9.9")   +                                                                   */
/*              "<C57>" + STRING(dSolgtVerdi / iAntKunder,">>,>>9.99") +                                                              */
/*              "<C66>" + STRING(dSolgtVerdi / iSolgtAnt,">>,>>9.99") + "</B>".                                                       */
/*               RUN Ramar(cRader,cCols).                                                                                             */
/*              PAGE.                                                                                                                 */
/*          END.                                                                                                                      */
/*          ASSIGN iRad        = 5                                                                                                    */
/*                 iSolgtAnt   = 0                                                                                                    */
/*                 dSolgtVerdi = 0                                                                                                    */
/*                 iAntKunder  = 0.                                                                                                   */
/*          PUT UNFORMATTED "<FArial><P20><R" + STRING(iRad) + "><B><C25>Aktivitetsrapport "  +                                       */
/*              ENTRY(RS-VisPr * 2 - 1,RS-VisPr:RADIO-BUTTONS) + "</B>".                                                              */
/*          PUT UNFORMATTED "<FArial><P14><R" + STRING(iRad + 2) + "><B>" +                                                           */
/*              (IF FI-TilDato = FI-FraDato THEN "<C35>" ELSE "<C31>") + FI-FraDato:SCREEN-VALUE  + (IF FI-TilDato <> FI-FraDato THEN */
/*                                                       " - " + FI-TilDato:SCREEN-VALUE ELSE "") + "</B>".                                 */
/*          ASSIGN iRad = iRad + 5.                                                                                                         */
/*          IF tmpAktRapp.Butik <> 0 THEN                                                                                                   */
/*              PUT UNFORMATTED "<P10><R" + STRING(iRad) + "><B><C10>" + ENTRY(2 * 2 - 1,RS-VisPr:RADIO-BUTTONS) +                          */
/*                  ": " + STRING(tmpAktRapp.Butik) + " " + tmpAktRapp.Navn + "</B>".                                                       */
/*          ASSIGN iRad = iRad + 1.                                                                                                         */
/*          IF tmpAktRapp.Kasse <> 0 THEN                                                                                                   */
/*              PUT UNFORMATTED "<P10><R" + STRING(iRad) + "><B><C10>" + ENTRY(1,RS-VisPr:RADIO-BUTTONS) +                                  */
/*                  ": " + STRING(tmpAktRapp.Kasse) + "</B>".                                                                               */
/*                                                                                                                                          */
/*          ASSIGN iRad   = iRad + 2                                                                                                        */
/*                 cRader = STRING(iRad).                                                                                                   */
/*          PUT UNFORMATTED "<FCourier New><P10><B><R" + STRING(iRad) + "><C10>Antall<C25>Verdi<C39>Antall<C52>Par/<C59>Verdi/<C67>Verdi/". */
/*          PUT UNFORMATTED "<P10><R" + STRING(iRad + 1) + ">" +                                                                            */
/*                      "<C6>Tid<C11>Solgt<C18>%<C25>Solgt<C35>%<C39>Kunder<C47>%<C51>Kunde<C60>Kunde<C69>Par</B>".                         */
/*          ASSIGN iRad   = iRad + 2                                                                                                        */
/*                 cRader = cRader + "," + STRING(iRad)                                                                                     */
/*                 iButik = tmpAktRapp.Butik                                                                                                */
/*                 iKasse = tmpAktRapp.Kasse.                                                                                               */
/*      END.                                                                                                                                */
/*      ASSIGN cRader = cRader + "," + STRING(iRad + 1).                                                                                    */
/* /*                   */                                                                                                                  */
/* /*      iBlankPixels */                                                                                                                  */
/* /*      iTknPixels). */                                                                                                                  */
/*      PUT UNFORMATTED "<FCourier NEW><P10><R" + STRING(iRad) + ">" +                                                                      */
/*      "<C6>" +  STRING(tmpAktRapp.Tid) +                                                                                                  */
/*      "<C10>" + STRING(tmpAktRapp.SolgtAnt,">>,>>9")    +                                                                                 */
/*      "<C17>" + STRING(tmpAktRapp.Solgt%Ant,">9.9")   +                                                                                   */
/*      "<C22>" + STRING(tmpAktRapp.SolgtVerdi,">,>>>,>>9.99")   +                                                                          */
/*      "<C34>" + STRING(tmpAktRapp.Solgt%Verdi,">9.9")  +                                                                                  */
/*      "<C39>" + STRING(tmpAktRapp.AntKunder,">>,>>9")    +                                                                                */
/*      "<C46>" + STRING(tmpAktRapp.Ant%Kunder,">9.9")   +                                                                                  */
/*      "<C52>" + STRING(tmpAktRapp.ParPrKunde,">9.9")   +                                                                                  */
/*      "<C57>" + STRING(tmpAktRapp.VerdiPrKunde,">>,>>9.99") +                                                                             */
/*      "<C66>" + STRING(tmpAktRapp.VerdiPrPar,">>,>>9.99").                                                                                */
/*      ASSIGN iRad = iRad + 1                                                                                                              */
/*             iSolgtAnt   = iSolgtAnt   + tmpAktRapp.SolgtAnt                                                                              */
/*             dSolgtVerdi = dSolgtVerdi + tmpAktRapp.SolgtVerdi                                                                            */
/*             iAntKunder  = iAntKunder  + tmpAktRapp.AntKunder                                                                             */
/*             cRader = cRader + "," + STRING(iRad + 1).                                                                                    */
/*   END.                                                                                                                                   */
/*   PUT UNFORMATTED "<FCourier NEW><P10><B><R" + STRING(iRad) + ">" +                                                                      */
/*   "<C6>" +  "TOT" +                                                                                                                      */
/*   "<C10>" + STRING(iSolgtAnt,">>,>>9")    +                                                                                              */
/* /*              "<C17>" + STRING(tmpAktRapp.Solgt%Ant,">9.9")   + */                                                                     */
/*   "<C22>" + STRING(dSolgtVerdi,">,>>>,>>9.99")   +                                                                                       */
/* /*              "<C34>" + STRING(tmpAktRapp.Solgt%Verdi,">9.9")  + */                                                                    */
/*   "<C39>" + STRING(iAntKunder,">>,>>9")    +                          */
/* /*              "<C46>" + STRING(tmpAktRapp.Ant%Kunder,">9.9")   + */ */
/*   "<C52>" + STRING(iSolgtAnt / iAntKunder,">9.9")   +                 */
/*   "<C57>" + STRING(dSolgtVerdi / iAntKunder,">>,>>9.99") +            */
/*   "<C66>" + STRING(dSolgtVerdi / iSolgtAnt,">>,>>9.99") + "</B>".     */
/*   RUN Ramar(cRader,cCols).                                            */
/*   /* Legger ut bilinformasjonen */                                    */
/*                                                                       */
/*   /* Lukker stream */                                                 */
/*   OUTPUT TO TERMINAL.                                                 */
/*                                                                       */
/*   /* Klargjør rapportfilnavnet */                                     */
/*   ASSIGN                                                              */
/*     FILE-INFO:File-NAME = pcRappFil.                                  */
/*                                                                       */
/*   /* Sender filen til visning og utskrift. */                         */
/*  Run printFile( FILE-INFO:FULL-PATHNAME ).                            */
/*                                                                       */
/*   STATUS DEFAULT " ".                                                 */
/* END. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddFriAntal Dialog-Frame 
FUNCTION AddFriAntal RETURNS CHARACTER
  ( INPUT cSumFriantal AS CHARACTER,INPUT cAddFriantal AS CHARACTER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.

  DO iCount = 1 TO NUM-ENTRIES(cSumFriantal," "):
      ASSIGN ENTRY(iCount,cSumFriantal," ") = STRING(INT(ENTRY(iCount,cSumFriantal," ")) +
                                              INT(ENTRY(iCount,cAddFriantal," "))).
  END.
  RETURN cSumFriantal.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixStorrelser Dialog-Frame 
FUNCTION FixStorrelser RETURNS CHARACTER
  ( INPUT cStorrelser AS CHARACTER, INPUT cFriAntall AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cStrl  AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cAnt   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iMaxLen AS INTEGER    NO-UNDO.
  /* Ta bort storlekar där det är 0(noll,null) i antal */
  DO iCount = 1 TO NUM-ENTRIES(cFriAntall," "):
      IF ENTRY(iCount,cFriantall," ") <> "0" THEN DO:
          ASSIGN cStrl = cStrl + (IF cStrl = "" THEN "" ELSE CHR(1)) + ENTRY(iCount,cStorrelser," ")
                 cAnt  = cAnt  + (IF cAnt  = "" THEN "" ELSE CHR(1)) + ENTRY(iCount,cFriantall," ").
      END.
  END.
  DO iCount = 1 TO NUM-ENTRIES(cStrl,CHR(1)):
      ASSIGN iMaxLen = MAXIMUM(LENGTH(ENTRY(iCount,cStrl,CHR(1))),LENGTH(ENTRY(iCount,cAnt,CHR(1))))
             ENTRY(iCount,cStrl,CHR(1)) = FILL(" ",iMaxLen - LENGTH(ENTRY(iCount,cStrl,CHR(1)))) + ENTRY(iCount,cStrl,CHR(1))
             ENTRY(iCount,cAnt,CHR(1)) = FILL(" ",iMaxLen - LENGTH(ENTRY(iCount,cAnt,CHR(1)))) + ENTRY(iCount,cAnt,CHR(1)).
  END.
  RETURN REPLACE(cStrl,CHR(1)," ") + CHR(1) + REPLACE(cAnt,CHR(1)," ").   /* Function return value. */
/*   DO iCount = 1 TO NUM-ENTRIES(cFriAntall," "):                                               */
/*       ASSIGN iFirst = IF iFirst = 0 AND ENTRY(iCount,cFriantall," ") <> "0" THEN              */
/*                      iCount ELSE iFirst                                                       */
/*              iLast  = IF ENTRY(iCount,cFriantall," ") <> "0" THEN                             */
/*                      iCount ELSE iLast.                                                       */
/*   END.                                                                                        */
/*   DO iCount = iFirst TO iLast:                                                                */
/*       ASSIGN cStrl = cStrl + (IF cStrl = "" THEN "" ELSE " ") + ENTRY(iCount,cStorrelser," ") */
/*              cAnt = cAnt + (IF cAnt = "" THEN "" ELSE " ") + ENTRY(iCount,cFriAntall," ").    */
/*   END.                                                                                        */
/*   RETURN cStrl + CHR(1) + cAnt.   /* Function return value. */                                */
/*   DEFINE VARIABLE iFirst  AS INTEGER    NO-UNDO.                                                                              */
/*   DEFINE VARIABLE iLast   AS INTEGER    NO-UNDO.                                                                              */
/*   DEFINE VARIABLE iCount  AS INTEGER    NO-UNDO.                                                                              */
/*   DEFINE VARIABLE cStrl   AS CHARACTER  NO-UNDO.                                                                              */
/*   DEFINE VARIABLE cAnt    AS CHARACTER  NO-UNDO.                                                                              */
/*   DEFINE VARIABLE iMaxLen AS INTEGER    NO-UNDO.                                                                              */
/*   /* strippa bortinledande ach avslutande 0-or */                                                                             */
/*   DO iCount = 1 TO NUM-ENTRIES(cFriAntall," "):                                                                               */
/*       ASSIGN iFirst = IF iFirst = 0 AND ENTRY(iCount,cFriantall," ") <> "0" THEN                                              */
/*                      iCount ELSE iFirst                                                                                       */
/*              iLast  = IF ENTRY(iCount,cFriantall," ") <> "0" THEN                                                             */
/*                      iCount ELSE iLast.                                                                                       */
/*   END.                                                                                                                        */
/*   DO iCount = iFirst TO iLast:                                                                                                */
/*       ASSIGN cStrl = cStrl + (IF cStrl = "" THEN "" ELSE " ") + ENTRY(iCount,cStorrelser," ")                                 */
/*              cAnt = cAnt + (IF cAnt = "" THEN "" ELSE " ") + ENTRY(iCount,cFriAntall," ").                                    */
/*   END.                                                                                                                        */
/*   /* SLUT strippa bortinledande ach avslutande 0-or */                                                                        */
/*   /* Ta bort storlekar med 0 i antal i sträng */                                                                              */
/*   ASSIGN cStrl = REPLACE(cStrl," ",CHR(1))                                                                                    */
/*          cAnt  = REPLACE(cAnt," ",CHR(1)).                                                                                    */
/*                                                                                                                               */
/*   DO iCount = 1 TO NUM-ENTRIES(cAnt,CHR(1)):                                                                                  */
/*       IF ENTRY(iCount,cAnt,CHR(1)) = "0" THEN                                                                                 */
/*           ASSIGN ENTRY(iCount,cStrl,CHR(1)) =                                                                                 */
/*   END.                                                                                                                        */
/*   DO iCount = 1 TO NUM-ENTRIES(cStrl," "):                                                                                    */
/*       IF LENGTH(ENTRY(iCount,cStrl," ")) > iMaxLen THEN                                                                       */
/*           ASSIGN iMaxLen = LENGTH(ENTRY(iCount,cStrl," ")).                                                                   */
/*   END.                                                                                                                        */
/*   DO iCount = 1 TO NUM-ENTRIES(cAnt," "):                                                                                     */
/*       IF LENGTH(ENTRY(iCount,cAnt," ")) > iMaxLen THEN                                                                        */
/*           ASSIGN iMaxLen = LENGTH(ENTRY(iCount,cAnt," ")).                                                                    */
/*   END.                                                                                                                        */
/*   DO iCount = 1 TO NUM-ENTRIES(cStrl,CHR(1)):                                                                                 */
/*       ASSIGN ENTRY(iCount,cStrl,CHR(1)) = FILL(" ",iMaxLen - LENGTH(ENTRY(iCount,cStrl,CHR(1)))) + ENTRY(iCount,cStrl,CHR(1)) */
/*              ENTRY(iCount,cAnt,CHR(1)) = FILL(" ",iMaxLen - LENGTH(ENTRY(iCount,cAnt,CHR(1)))) + ENTRY(iCount,cAnt,CHR(1)).   */
/*   END.                                                                                                                        */
/*   RETURN REPLACE(cStrl,CHR(1)," ") + CHR(1) + REPLACE(cAnt,CHR(1)," ").   /* Function return value. */                        */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtInfo Dialog-Frame 
FUNCTION getArtInfo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iCount1      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iIdx         AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSasong      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSasongTxt   AS CHARACTER  INIT "Sesong:" NO-UNDO.
DEFINE VARIABLE cMaterial    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cMaterialTxt AS CHARACTER  INIT "Material:" NO-UNDO.
DEFINE VARIABLE cKlack       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKlackTxt    AS CHARACTER  INIT "Hæl:" NO-UNDO.
DEFINE VARIABLE cInnerSula   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cInnerSulaTxt AS CHARACTER INIT "Innersåle:" NO-UNDO.
DEFINE VARIABLE cOvandel     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOvandelTxt  AS CHARACTER  INIT "Foder:" NO-UNDO.
DEFINE VARIABLE cSlitSula    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSlitSulaTxt AS CHARACTER  INIT "Slitesåle:" NO-UNDO.
DEFINE VARIABLE cLastSko     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLastSkoTxt  AS CHARACTER  INIT "Læst:" NO-UNDO.
DEFINE VARIABLE cAnvKod      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cAnvKodTxt   AS CHARACTER  INIT "Bruk:" NO-UNDO.
DEFINE VARIABLE cNotat       AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE cNotatTmp    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLevFargKode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVgTxt       AS CHARACTER  INIT "Vg:"NO-UNDO.
DEFINE VARIABLE cTempInfo    AS CHARACTER EXTENT 9 NO-UNDO.
DEFINE VARIABLE cRad         AS CHARACTER EXTENT 4 NO-UNDO.

ASSIGN cVgTxt       = ENTRY(19,TT_OrdreHode.levlabels,CHR(1))
       cSasongTxt   = ENTRY(20,TT_OrdreHode.levlabels,CHR(1))
       cMaterialTxt = ENTRY(22,TT_OrdreHode.levlabels,CHR(1))
       cKlackTxt    = ENTRY(23,TT_OrdreHode.levlabels,CHR(1))
       cInnerSulaTxt = ENTRY(24,TT_OrdreHode.levlabels,CHR(1))
       cOvandelTxt  = ENTRY(25,TT_OrdreHode.levlabels,CHR(1))
       cSlitSulaTxt = ENTRY(26,TT_OrdreHode.levlabels,CHR(1))
       cLastSkoTxt  = ENTRY(27,TT_OrdreHode.levlabels,CHR(1))
       cAnvKodTxt   = ENTRY(28,TT_OrdreHode.levlabels,CHR(1)).

IF ArtBas.VisDivInfo[1] = TRUE THEN DO:
    ASSIGN cSasong = ArtBas.DivInfo[1].
    IF cSasong = "" THEN DO:
        FIND Sasong WHERE Sasong.Sasong = ArtBas.Sasong NO-LOCK NO-ERROR.
        IF AVAIL Sasong THEN
            ASSIGN cSasong = Sasong.SasBeskr.
    END.
    IF cSasong <> "" THEN
        ASSIGN cTempInfo[2] = cSasongTxt + cSasong.
END.
IF ArtBas.VisDivInfo[2] = TRUE THEN DO:
    ASSIGN cMaterial = ArtBas.DivInfo[2].
    IF cMaterial = "" THEN DO:
        FIND Material WHERE Material.MatKod = ArtBas.MatKod NO-LOCK NO-ERROR.
        IF AVAIL Material THEN
            ASSIGN cMaterial = Material.MatBeskr.
    END.
    IF cMaterial <> "" THEN
        ASSIGN cTempInfo[3] = cMaterialTxt + cMaterial.
END.
IF ArtBas.VisDivInfo[3] = TRUE THEN DO:
    ASSIGN cKlack = ArtBas.DivInfo[3].
    IF cKlack = "" THEN DO:
        ASSIGN cKlack = STRING(ArtBas.Klack).
    END.
    IF cKlack <> "" THEN
        ASSIGN cTempInfo[4] = cKlackTxt + cKlack.
END.
IF ArtBas.VisDivInfo[4] = TRUE THEN DO:
    ASSIGN cInnersula = ArtBas.DivInfo[4].
    IF cInnersula = "" THEN DO:
        FIND InnerSula WHERE InnerSula.inner-id = ArtBas.inner-id NO-LOCK NO-ERROR.
        IF AVAIL InnerSula THEN
            ASSIGN cInnerSula = InnerSula.InnerBeskr.
    END.
    IF cInnersula <> "" THEN
        ASSIGN cTempInfo[5] = cInnerSulaTxt + cInnerSula.
END.
IF ArtBas.VisDivInfo[5] = TRUE THEN DO:
    ASSIGN cOvandel = ArtBas.DivInfo[5].
    IF cOvandel = "" THEN DO:
        FIND Ovandel WHERE Ovandel.ov-id = ArtBas.ov-id NO-LOCK NO-ERROR.
        IF AVAIL Ovandel THEN
            ASSIGN cOvandel = Ovandel.OvBeskr.
    END.
    IF cOvandel <> "" THEN
        ASSIGN cTempInfo[6] = cOvandelTxt + cOvandel.
END.
IF ArtBas.VisDivInfo[6] = TRUE THEN DO:
    ASSIGN cSlitSula = ArtBas.DivInfo[6].
    IF cSlitSula = "" THEN DO:
        FIND SlitSula WHERE SlitSula.slit-id = ArtBas.slit-id NO-LOCK NO-ERROR.
        IF AVAIL SlitSula THEN
            ASSIGN cSlitSula = SlitSula.SlitBeskr.
    END.
    IF cSlitSula <> "" THEN
        ASSIGN cTempInfo[7] = cSlitSulaTxt + cSlitSula.
END.
IF ArtBas.VisDivInfo[7] = TRUE THEN DO:
    ASSIGN cLastSko = ArtBas.DivInfo[7].
    IF cLastSko = "" THEN DO:
        FIND Last-Sko WHERE Last-Sko.last-id = ArtBas.last-id NO-LOCK NO-ERROR.
        IF AVAIL Last-Sko THEN
            ASSIGN cLastSko = Last-Sko.LastBeskr.
    END.
    IF cLastSko <> "" THEN
        ASSIGN cTempInfo[8] = cLastSkoTxt + cLastSko.
END.
IF ArtBas.VisDivInfo[8] = TRUE THEN DO:
    ASSIGN cAnvKod = ArtBas.DivInfo[8].
    IF cAnvKod = "" THEN DO:
        FIND Anv-Kod WHERE Anv-Kod.anv-id = ArtBas.anv-id NO-LOCK NO-ERROR.
        IF AVAIL Anv-Kod THEN
            ASSIGN cAnvKod = Anv-Kod.AnvBeskr.
    END.
    IF cAnvKod <> "" THEN
        ASSIGN cTempInfo[9] = cAnvKodTxt + cAnvKod.
END.
ASSIGN iIdx = 1.                       
IF ArtBas.VisDivInfo[9] = TRUE AND TRIM(ArtBas.Notat) <> "" THEN DO:
    ASSIGN cNotatTmp = TRIM(ArtBas.Notat).
    /* om något entry i strängen är längre än 45 */
    IF NUM-ENTRIES(cNotatTmp,CHR(10)) > 1 THEN DO:
        DO iCount1 = 1 TO NUM-ENTRIES(cNotatTmp,CHR(10)):
            IF LENGTH(ENTRY(iCount1,cNotatTmp,CHR(10))) > 45 THEN DO:
                ASSIGN cNotatTmp = REPLACE(cNotatTmp,CHR(10)," ").
                LEAVE.
            END.
        END.
    END.
    /* leta upp blanka så vi kan dela upp strängen i bitar */ 
    IF NUM-ENTRIES(cNotatTmp,CHR(10)) = 1 AND LENGTH(cNotatTmp) > 45 THEN DO:
        ASSIGN cNotatTmp = StripNotat(cNotatTmp).
    END.
    DO iCount1 = 1 TO NUM-ENTRIES(cNotatTmp,CHR(10)):
        IF ENTRY(iCount1,cNotatTmp,CHR(10)) <> "" THEN
            ASSIGN cNotat[iIdx] = ENTRY(iCount1,cNotatTmp,CHR(10))
                   iIdx = iIdx + 1.
        IF iIdx > 4 THEN
            LEAVE.
    END.
END.
IF ArtBas.VisDivInfo[10] = TRUE THEN DO:
   ASSIGN cLevFargKode = ArtBas.LevFargKod.
   IF cLevFargKode = "" THEN DO:
       FIND Farg WHERE Farg.Farg = ArtBas.Farg NO-LOCK NO-ERROR.
       IF AVAIL Farg THEN
           ASSIGN cLevFargKode = Farg.FarBeskr.
   END.
END.
IF ArtBas.VisDivInfo[11] = TRUE THEN DO:
   ASSIGN cTempInfo[1] = cVgTxt + STRING(ArtBas.Vg).
END.
ASSIGN cRad[1] = ArtBas.LevKod + IF cLevFargKode <> "" THEN " " + cLevFargKode ELSE ""
       iIdx    = 2.

DO iCount1 = 1 TO 9:
    IF cTempInfo[iCount1] <> "" THEN DO:
        IF LENGTH(cRad[iIdx]) + LENGTH(cTempInfo[iCount1]) > 35 THEN DO:
            IF iIdx = 4 THEN
                LEAVE.
            ELSE
                ASSIGN iIdx = iIdx + 1.
        END.
        ASSIGN cRad[iIdx] = cRad[iIdx] + (IF cRad[iIdx] = "" THEN "" ELSE ",") +
            cTempInfo[iCount1].
    END.
END.
ASSIGN iIdx = 1.
DO iCount1 = 1 TO 4:
    IF cNotat[iCount1] <> "" THEN
        ASSIGN cRad[iIdx] = cRad[iIdx] + CHR(2) + cNotat[iCount1]
               iIdx = iIdx + 1.
END.
RETURN cRad[1] + (IF cRad[2] <> "" THEN CHR(1) + cRad[2] ELSE "") +
                 (IF cRad[3] <> "" THEN CHR(1) + cRad[3] ELSE "") + 
                 (IF cRad[4] <> "" THEN CHR(1) + cRad[4] ELSE "").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBildeFil Dialog-Frame 
FUNCTION getBildeFil RETURNS CHARACTER
  ( INPUT ipBildNr AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBildeFil AS CHARACTER  NO-UNDO.
  FIND BildeRegister NO-LOCK WHERE
    BildeRegister.BildNr = ipBildNr NO-ERROR.
  IF AVAIL BildeRegister AND TRIM(BildeRegister.FilNavn) <> "" THEN DO:
    IF VALID-HANDLE(wLibHandle) THEN
      RUN HentBildePeker IN wLibHandle (INPUT ipBildNr, 1, BildeRegister.FilNavn, OUTPUT cBildeFil).
  END.
  /* cBlanktBilde */
  RETURN cBildeFil.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLabels Dialog-Frame 
FUNCTION getLabels RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cReturLabels AS CHARACTER  NO-UNDO.
  ASSIGN cReturLabels = cOrgLabels.
  IF LevBas.Lng <> "DES" AND LevBas.Lng <> "" THEN DO:
      FIND FIRST SysGruppe NO-LOCK where
        SysGruppe.SysHId = 5 and
        SysGruppe.SysGr  >= 201 and
        SysGruppe.SysGr  <= 249 and
        SysGruppe.Beskrivelse = LevBas.Lng NO-ERROR.
      IF AVAIL SysGruppe THEN 
      FOR EACH SysPara OF SysGruppe WHERE SysPara.ParaNr <= 29:
          IF SysPara.Parameter1 <> "" THEN
          ASSIGN ENTRY(SysPara.ParaNr,cReturLabels,CHR(1)) = SysPara.Parameter1.
      END.
  END.
  RETURN cReturLabels.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPostAdr Dialog-Frame 
FUNCTION getPostAdr RETURNS CHARACTER
  ( INPUT ipPostNr AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND Post WHERE Post.PostNr = ipPostNr NO-LOCK NO-ERROR.
  RETURN IF AVAIL Post THEN Post.Beskrivelse ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION StripNotat Dialog-Frame 
FUNCTION StripNotat RETURNS CHARACTER
  ( INPUT cNotat AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLastBlank AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cString    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lLastBlank AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE c10        AS CHARACTER  NO-UNDO.
  /* strip av multipla blanka */
  DO iCount = 1 TO LENGTH(cNotat):
      IF NOT (SUBSTR(cNotat,iCount,1) = " " AND lLastBlank = TRUE) THEN
          ASSIGN cString = cString + SUBSTR(cNotat,iCount,1)
                 lLastBlank = SUBSTR(cNotat,iCount,1) = " ".
  END.
  /* Lägg in CHR(10) så att varje del blir <= 45 */
  DO iCount = 1 TO LENGTH(cString):
      ASSIGN iCount2 = iCount2 + 1.
      IF SUBSTR(cString,iCount,1) = " " THEN DO:
          IF iCount2 > 45 THEN
              ASSIGN c10     = c10 + (IF c10 = "" THEN "" ELSE ",") + STRING(iLastBlank)
                     iCount2 = 0.
          ELSE
              ASSIGN iLastBlank = iCount. 
      END.
  END.
  /* Byt ut blank mot CHR(10) */
  DO iCount = 1 TO NUM-ENTRIES(c10):
      OVERLAY(cString, INT(ENTRY(iCount,c10))) = CHR(10).
  END.


  RETURN cString.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

