&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* DEFINE VAR ipBuntNr LIKE ovBunt.BuntNr  NO-UNDO. */
DEFINE INPUT  PARAMETER rTelleHode AS ROWID      NO-UNDO.
DEFINE INPUT  PARAMETER cQry AS CHARACTER  NO-UNDO.

DEFINE VARIABLE cTitle          AS CHARACTER INIT "Telleliste" NO-UNDO.
DEFINE VARIABLE iRad            AS INTEGER         NO-UNDO.
DEFINE VARIABLE cColLabelString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintString    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iAntDiff        AS INTEGER    NO-UNDO.
DEFINE VARIABLE dVerdiDiff      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iAntallPar      AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntallTalt     AS INTEGER    NO-UNDO.
DEFINE VARIABLE dNedskrevet     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dSalgsVerdi     AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dOpprVerdi      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dOpptVerdi      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cButNamn LIKE Butiker.ButNamn NO-UNDO.
DEFINE VARIABLE iCL             AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCLProfilnr     AS INTEGER    NO-UNDO.
DEFINE VARIABLE cRub   AS CHARACTER EXTENT 14 INITIAL
    ["Artikkelnr","Beskrivelse","VG/Lopnr","Strl","Levkod","Ant par","Ant talt","Not","Salgsverdi","Varekost","Diff",
    "Verdi diff","Oppr verdi","Talt verdi"] NO-UNDO.
DEFINE VARIABLE iCols  AS INTEGER  EXTENT 14 INITIAL
/*     [6,10,16,33,37,44,54,59,64,73,82,87,96,105] NO-UNDO. */
    [6,12,29,32,38,54,60,65,67,76,82,87,96,104] NO-UNDO.
DEFINE VARIABLE iRight AS INTEGER EXTENT 14 INITIAL
    [1,0,0,1,0,1,1,0,1,1,1,1,1,1] NO-UNDO.
/*     [0,1,0,0,1,0,1,1,1,1,1,1,1,1] NO-UNDO. */

DEFINE VARIABLE iRow AS INTEGER     NO-UNDO.

DEF STREAM Eksport.
DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><P12><B><C6>" STRING(TODAY)
      "<P12></B><C110><P9>" PAGE-NUMBER FORMAT ">>>>" SKIP
      "<R4><C6><B>Butikk:" cButNamn "</B>" SKIP
      "<R5><C6><FROM><R5><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.

/* {xPrint.i} */
{runlib.i}
{ pdf_inc.i "NOT SUPER"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getDataLinje Procedure 
FUNCTION getDataLinje RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SumRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SumRad Procedure 
FUNCTION SumRad RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{sww.i}
/* RUN InitLabels för språkhantering */ 
    FIND TelleHode WHERE ROWID(TelleHode) = rTelleHode NO-LOCK NO-ERROR.
    IF TelleHode.TTId = 8 THEN
        ASSIGN cRub[9] = "Nedskr".
    IF AVAIL TelleHode THEN DO:
        {syspara.i 1 1 100 cKundenavn}
        {syspara.i 1 1 101 cPolygon}
        {syspara.i 5 1 1 iCl INT}
        FIND TransType WHERE TransType.TTId = TelleHode.TTId NO-LOCK NO-ERROR.
        IF AVAIL TransType THEN
            ASSIGN cTitle = cTitle + "-" + TransType.Beskrivelse.
        FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
        ASSIGN iCLProfilNr = Butiker.ProfilNr.
        FIND Butiker WHERE Butiker.Butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR.
        ASSIGN cButNamn = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".
        RUN TellelistePDF.
    END.
    ELSE DO:
        MESSAGE "Finner ikke TelleHode."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
{swn.i}
RETURN RETURN-VALUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-InitPrintString) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitPrintString Procedure 
PROCEDURE InitPrintString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cRight AS CHARACTER  NO-UNDO.
    DO iCount = 1 TO EXTENT(iCols):
        ASSIGN cRight = IF iRight[iCount] = 0 THEN "" ELSE 
                 "<RIGHT=C+" + (IF iCount = EXTENT(iCols) THEN STRING(113 - iCols[iCount] - 1) 
                                ELSE STRING(iCols[iCount + 1] - iCols[iCount] - 1)) + ">".
        ASSIGN cPrintString = cPrintString + "<C" + STRING(iCols[iCount]) + ">" + cRight + "&" + STRING(iCount,"99").
    END.
    ASSIGN cColLabelString = "<U>" + cPrintString + "</U>".
    DO iCount = 1 TO EXTENT(iCols):
        ASSIGN cColLabelString = REPLACE(cColLabelString,"&" + STRING(iCount,"99"),cRub[iCount]).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LoadFonts) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LoadFonts Procedure 
PROCEDURE LoadFonts :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN pdf_load_font IN h_PDFinc ("Spdf","Arial","c:\windows\fonts\arial.ttf","pdfinclude\arial.afm","").
RUN pdf_load_font IN h_PDFinc ("Spdf","Arialbd","c:\windows\fonts\arialbd.ttf","pdfinclude\arial.afm","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-new_page) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new_page Procedure 
PROCEDURE new_page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_new_page IN h_PDFinc ("Spdf").
/*   irow = pdf_PageHeight("Spdf") - pdf_TopMargin("Spdf") - 110. */
/*   irow = pdf_TopMargin("Spdf") + 5. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageHeader Procedure 
PROCEDURE PageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",16).
  RUN pdf_text_at IN h_PDFinc ("Spdf",STRING(TODAY),5).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cTitle,60).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",cButnamn,5).
  RUN pdf_line IN h_PDFinc ("Spdf",14,530,800,530,0.5).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",9).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_skip IN h_PDFinc ("Spdf").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PFooter Procedure 
PROCEDURE PFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
              PUT UNFORMATTED "<R45><C6><FROM><R45><C113><LINE>" SKIP
                              "<C6>" cKundenavn "<C1><CENTER=C113>" cPolygon SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellelistePDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellelistePDF Procedure 
PROCEDURE TellelistePDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hQuery       AS HANDLE NO-UNDO.
  DEF VAR hBuffer      AS HANDLE NO-UNDO.
  DEF VAR pcRappFil    AS CHAR    NO-UNDO.
  DEF VAR iRapportValg AS INTEGER NO-UNDO.
  DEF VAR lOK          AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE iOldPage        AS INTEGER     NO-UNDO.
  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  
  RUN gvelgutskrtell.w (OUTPUT iRapportValg).
  IF iRapportValg = 0 THEN
    RETURN NO-APPLY.
  
  RUN InitPrintString.
  IF cQry = "" THEN DO:
      ASSIGN cQry = "FOR EACH TelleLinje WHERE Tellelinje.Tellenr = " + STRING(TelleHode.Tellenr) + " NO-LOCK BY TelleLinje.ArtikkelNr".
  END.
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(BUFFER Tellelinje:HANDLE).
  hQuery:QUERY-PREPARE(cQry).
  hQuery:QUERY-OPEN().

  IF VALID-HANDLE(wLibHandle) THEN
     RUN GetTempFileName IN wLibHandle ("STelleListe", "pdf", OUTPUT pcRappFil). 
  RUN pdf_new IN h_PDFinc ("Spdf",pcRappFil).
/*   RUN LoadFonts. */
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
/*   MESSAGE pdf_PageHeight ("Spdf") SKIP   */
/*           pdf_PageWidth ("Spdf")         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  RUN new_page.

/*   RUN PageHeader. */
/*   PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P8><R+2>".                                 */
/*   PUT UNFORMATTED cColLabelString SKIP.                                                                    */
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      iRow = irow + 10.
      IF pdf_Page ("Spdf") > iOldPage THEN DO:
    /*       iColLabelPage = iColLabelPage + 1. */
          RUN PageHeader.
/*           RUN ColLabels. */
          iOldPage = pdf_Page ("Spdf").
      END.
      RUN pdf_text IN h_PDFinc ("Spdf","TELLING " + STRING(tellelinje.tellenr + irow)).
      RUN pdf_skip IN h_PDFinc ("Spdf").
/*       RUN pdf_text IN h_PDFinc ("Spdf", "TELLING " + STRING(tellelinje.tellenr)). */
/*      ASSIGN lOK = TRUE.                                                                                    */
/*       /* iRapportValg: 10=alla,11=inga 0-0,20=bara negativ diff ,30=bara positiv diff */                   */
/*       IF iRapportValg = 11 AND TelleLinje.AntallPar = 0 AND TelleLinje.AntallTalt = 0 THEN                 */
/*           lOK = FALSE.                                                                                     */
/*       ELSE IF iRapportValg = 20 AND TelleLinje.AntallDiff >= 0 THEN                                        */
/*           lOK = FALSE.                                                                                     */
/*       ELSE IF iRapportValg = 30 AND TelleLinje.AntallDiff <= 0 THEN                                        */
/*           lOK = FALSE.                                                                                     */
/*       ELSE IF iRapportValg = 40 AND TelleLinje.AntallPar = TelleLinje.AntallTalt THEN                      */
/*           lOK = FALSE.                                                                                     */
/* /*       IF lSvar = TRUE AND TelleLinje.AntallPar = 0 AND TelleLinje.AntallTalt = 0 THEN */                */
/* /*           NEXT.                                                                       */                */
/*       IF lOK = TRUE THEN DO:                                                                               */
/*           ASSIGN iAntDiff    = iAntDiff    + TelleLinje.AntallDiff                                         */
/*                  dVerdiDiff  = dVerdiDiff  + TelleLinje.VerdiDiff                                          */
/*                  iAntallPar  = iAntallPar  + TelleLinje.AntallPar                                          */
/*                  iAntallTalt = iAntallTalt + TelleLinje.AntallTalt                                         */
/*                  dNedskrevet = dNedskrevet + TelleLinje.Nedskrevet                                         */
/*                  dOpprVerdi  = dOpprVerdi  + TelleLinje.OpprVerdi                                          */
/*                  dOpptVerdi  = dOpptVerdi  + TelleLinje.OpptVerdi.                                         */
/*           PUT UNFORMATTED getDataLinje() SKIP.                                                             */
/* /*           IF LINE-COUNTER = 40 /* AND NOT LAST(TelleLinje.ArtikkelNr) */ THEN DO:       */              */
/* /*               RUN PFooter.                                                              */              */
/* /*               PAGE.                                                                     */              */
/* /*               VIEW FRAME PageHeader.                                                    */              */
/* /*               PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>". */              */
/* /*               PUT UNFORMATTED cColLabelString SKIP.                                     */              */
/* /*           END.                                                                          */              */
/*       END. */
      hQuery:GET-NEXT().
/*       IF NOT hQuery:QUERY-OFF-END AND LINE-COUNTER = 40 /* AND NOT LAST(TelleLinje.ArtikkelNr) */ THEN DO: */
/*               RUN PFooter.                                                                                 */
/*               PAGE.                                                                                        */
/*               VIEW FRAME PageHeader.                                                                       */
/*               PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P8><R+2>".                     */
/*               PUT UNFORMATTED cColLabelString SKIP.                                                        */
/*           END.                                                                                             */
  END.
/*   PUT UNFORMATTED SumRad() SKIP.                                                                           */
/*   RUN PFooter.                                                                                             */
/*   OUTPUT CLOSE.                                                                                            */
/*   OUTPUT TO TERMINAL.                                                                                      */
/*                                                                                                            */
/*   /* Klargjør rapportfilnavnet */                                                                          */
/*   ASSIGN FILE-INFO:File-NAME = pcRappFil.                                                                  */
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  hQuery = ?.
  
  RUN pdf_close IN h_PDFinc ("Spdf").
 RUN browse2pdf\viewxmldialog.w (pcRappFil,"Telleliste").
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getDataLinje Procedure 
FUNCTION getDataLinje RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTmpData AS CHARACTER EXTENT 14 NO-UNDO.
    DEFINE VARIABLE cColLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
      FIND ArtBas WHERE ArtBas.ArtikkelNr = TelleLinje.ArtikkelNr NO-LOCK NO-ERROR.
      IF TelleHode.TTId <> 8 THEN DO:
          FIND Artpris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
                             ArtPris.ProfilNr   = Butiker.ProfilNr NO-LOCK NO-ERROR.
          IF NOT AVAIL ArtPris THEN
              FIND Artpris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
                                 ArtPris.ProfilNr   = iCLProfilNr NO-LOCK NO-ERROR.
          IF NOT AVAIL ArtPris THEN
              FIND FIRST Artpris WHERE Artpris.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK NO-ERROR.
      END.
      ASSIGN cTmpData[1] = (IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE "0")
             cTmpData[2] = (IF AVAIL ArtBas THEN SUBSTR(ArtBas.Bongtekst,1,17) ELSE "Ukjent")
             cTmpData[3] = TelleLinje.VgLopNr
             cTmpData[4] = IF TelleLinje.Storl = "" THEN " " ELSE TelleLinje.Storl
             cTmpData[5] = TelleLinje.LevKod
             cTmpData[6] = STRING(TelleLinje.AntallPar)
             cTmpData[7] = STRING(TelleLinje.AntallTalt)
             cTmpData[8] = "___"
             cTmpData[9] = IF TelleHode.TTId = 8 THEN STRING(TelleLinje.Nedskrevet,"->>,>>,>>9.99")
                 ELSE STRING(TelleLinje.AntallTalt * Artpris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1],"->>,>>>,>>9.99")
             dSalgsVerdi = dSalgsVerdi + DECI(cTmpData[9])
             cTmpData[10] = STRING(TelleLinje.VVareKost,"->>,>>>,>>9.99")
             cTmpData[11] = STRING(TelleLinje.AntallDiff)
             cTmpData[12] = STRING(TelleLinje.VerdiDiff,"->>,>>>,>>9.99")
             cTmpData[13] = STRING(TelleLinje.OpprVerdi,"->>,>>>,>>9.99")
             cTmpData[14] = STRING(TelleLinje.OpptVerdi,"->>,>>>,>>9.99").
/*       ASSIGN cTmpData[1] = STRING(TelleLinje.Butik)                                           */
/*              cTmpData[2] = (IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE "0")          */
/*              cTmpData[3] = (IF AVAIL ArtBas THEN SUBSTR(ArtBas.Bongtekst,1,17) ELSE "Ukjent") */
/*              cTmpData[4] = TelleLinje.VgLopNr                                                 */
/*              cTmpData[5] = IF TelleLinje.Storl = "" THEN " " ELSE TelleLinje.Storl            */
/*              cTmpData[6] = TelleLinje.LevKod                                                  */
/*              cTmpData[7] = STRING(TelleLinje.AntallPar)                                       */
/*              cTmpData[8] = STRING(TelleLinje.AntallTalt)                                      */
/*              cTmpData[9] = STRING(TelleLinje.Nedskrevet,"->>,>>,>>9.99")                      */
/*              cTmpData[10] = STRING(TelleLinje.VVareKost,"->>,>>>,>>9.99")                     */
/*              cTmpData[11] = STRING(TelleLinje.AntallDiff)                                     */
/*              cTmpData[12] = STRING(TelleLinje.VerdiDiff,"->>,>>>,>>9.99")                     */
/*              cTmpData[13] = STRING(TelleLinje.OpprVerdi,"->>,>>>,>>9.99")                     */
/*              cTmpData[14] = STRING(TelleLinje.OpptVerdi,"->>,>>>,>>9.99").                    */
    ASSIGN cColLabels = cPrintString.
    DO iCount = 1 TO EXTENT(cTmpData):
        ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),cTmpData[iCount]).
    END.
  RETURN cColLabels.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SumRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SumRad Procedure 
FUNCTION SumRad RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cColLabels AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    ASSIGN cColLabels = cPrintString.
    DO iCount = 1 TO 14:
        CASE iCount:
            WHEN 6 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(iAntallPar)).
            WHEN 7 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(iAntallTalt)).
            WHEN 9 THEN DO:
                IF Tellehode.TTId <> 8 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(dSalgsVerdi,"->>,>>>,>>9.99")).
/*                 ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(dNedskrevet,"->>,>>>,>>9.99")). */
            END.
            WHEN 11 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(iAntDiff)).
            WHEN 12 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(dVerdiDiff,"->>,>>>,>>9.99")).
            WHEN 13 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(dOpprVerdi,"->>,>>>,>>9.99")).
            WHEN 14 THEN
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(dOpptVerdi,"->>,>>>,>>9.99")).
            OTHERWISE
                ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99")," ").

        END CASE.
        IF iCount <> 11 AND iCount <> 12 THEN
            ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99")," ").
        ELSE IF iCount = 11 THEN
            ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(iAntDiff)).
        ELSE
            ASSIGN cColLabels = REPLACE(cColLabels,"&" + STRING(iCount,"99"),STRING(dVerdiDiff,"->>,>>>,>>9.99")).
    END.
  RETURN "<B>" + cColLabels + "</B>".   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

