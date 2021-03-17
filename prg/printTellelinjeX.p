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
DEFINE INPUT  PARAMETER cParam AS CHAR     NO-UNDO.
DEFINE INPUT  PARAMETER cQry AS CHARACTER  NO-UNDO.

DEF VAR rTelleHode AS ROWID NO-UNDO.
DEF VAR rTellelinje AS ROWID NO-UNDO.

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

DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 14  NO-UNDO.
DEFINE VARIABLE iToRight       AS INTEGER EXTENT 14  NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.


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
         HEIGHT             = 14.95
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
{sww.i}
/* RUN InitLabels för språkhantering */ 
/*     MESSAGE cQry                           */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    rTellehode = TO-ROWID(ENTRY(2,cParam,'|')).
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
/*         RUN TellelisteXPrint. */
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

&IF DEFINED(EXCLUDE-ColLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColLabels Procedure 
PROCEDURE ColLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
/*   DEFINE VARIABLE iX AS INTEGER     NO-UNDO.        */
/*   MESSAGE pdf_PointSize ("Spdf")                    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","",iCols[1]). */
/*   iX = pdf_TextX("Spdf").                           */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",138),iCols[1]). */
/*   RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf") + 36, pdf_TextY("Spdf") + 1, pdf_PageWidth("Spdf") - 30 , */
/*                  pdf_TextY("Spdf") + 1, 1).                                                                          */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf"). */
  DO ii = 1 TO EXTENT(cRub):
      IF iCols[ii] <> 0 THEN
          RUN pdf_text_at IN h_PDFinc ("Spdf",cRub[ii],iCols[ii]).
      ELSE
          RUN pdf_text_to IN h_PDFinc ("Spdf",cRub[ii],iToRight[ii]).
  END.
  RUN pdf_skip    IN h_PDFinc ("Spdf").
/*   RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf") + 36, pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 30 , */
/*                  pdf_TextY("Spdf") + 5, 1).                                                                          */
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf") + 6, pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 17 , pdf_TextY("Spdf") + 5, 1).
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getDataLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getDataLinje Procedure 
PROCEDURE getDataLinje :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cTmpData AS CHARACTER NO-UNDO.
    cTmpData = FILL(CHR(1),13).
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
      ASSIGN ENTRY(1,cTmpData,CHR(1)) = (IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE "0")
             ENTRY(2,cTmpData,CHR(1)) = (IF AVAIL ArtBas THEN SUBSTR(ArtBas.Bongtekst,1,17) ELSE "Ukjent")
             ENTRY(3,cTmpData,CHR(1)) = TelleLinje.VgLopNr
             ENTRY(4,cTmpData,CHR(1)) = IF TelleLinje.Storl = "" THEN " " ELSE TelleLinje.Storl
             ENTRY(5,cTmpData,CHR(1)) = TelleLinje.LevKod
             ENTRY(6,cTmpData,CHR(1)) = STRING(TelleLinje.AntallPar)
             ENTRY(7,cTmpData,CHR(1)) = STRING(TelleLinje.AntallTalt)
             ENTRY(8,cTmpData,CHR(1)) = "___"
             ENTRY(9,cTmpData,CHR(1)) = IF TelleHode.TTId = 8 THEN STRING(TelleLinje.Nedskrevet,"->>,>>,>>9.99")
                 ELSE STRING(TelleLinje.AntallTalt * Artpris.Pris[IF ArtPris.Tilbud THEN 2 ELSE 1],"->>,>>>,>>9.99")
             dSalgsVerdi = dSalgsVerdi + DECI(ENTRY(9,cTmpData,CHR(1)))
             ENTRY(10,cTmpData,CHR(1)) = STRING(TelleLinje.VVareKost,"->>,>>>,>>9.99")
             ENTRY(11,cTmpData,CHR(1)) = STRING(TelleLinje.AntallDiff)
             ENTRY(12,cTmpData,CHR(1)) = STRING(TelleLinje.VerdiDiff,"->>,>>>,>>9.99")
             ENTRY(13,cTmpData,CHR(1)) = STRING(TelleLinje.OpprVerdi,"->>,>>>,>>9.99")
             ENTRY(14,cTmpData,CHR(1)) = STRING(TelleLinje.OpptVerdi,"->>,>>>,>>9.99").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PageFooter Procedure 
PROCEDURE PageFooter :
/*------------------------------------------------------------------------------
  Purpose:  Procedure to Print Page Footer -- on all pages.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Display a Sample Watermark on every page */
/*   RUN pdf_watermark IN h_PDFinc ("Spdf","Reservasjon Nettbutikk","Courier-Bold",34,.87,.87,.87,80,500). */

  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).
  RUN pdf_line IN h_PDFinc  ("Spdf", 20, pdf_BottomMargin ("Spdf") - 4, pdf_PageWidth("Spdf") - 20 , pdf_BottomMargin ("Spdf") - 4, 1).
/*   RUN pdf_line IN h_PDFinc  ("Spdf", 0, pdf_TextY("Spdf") - 5, pdf_PageWidth("Spdf") - 20 , pdf_TextY("Spdf") - 5, 1). */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_to IN h_PDFinc  ("Spdf",  "Side: "
                           + STRING(pdf_page("Spdf"))
                           + " (" + pdf_TotalPages("Spdf") + ")", 138).

/*   vlines = 1. /* Restart our count for the linking */ */

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
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Telledato: " + STRING(TelleHode.startdato),6).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cTitle,40).
/*   RUN pdf_text_align IN h_PDFinc ("Spdf",cTitle,"CENTER", iLeftMargin, iRMarginPos). */
/*   RUN pdf_text_align IN h_PDFinc ("Spdf",cTitle,"CENTER", 10, 100). */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf") + 6, pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 17 , pdf_TextY("Spdf") + 5, 1).
/*   RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , */
/*                  pdf_TextY("Spdf") + 5, 1).                                                                     */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",138),iCols[1]). */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN COlLabels.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SumRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumRad Procedure 
PROCEDURE SumRad PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBelopp AS CHAR EXTENT 14    NO-UNDO.
  DEFINE VARIABLE ii      AS INTEGER     NO-UNDO.
    ASSIGN cBelopp[6] = STRING(iAntallPar)
           cBelopp[7] = STRING(iAntallTalt)
           cBelopp[9] = STRING(dSalgsVerdi,"->>,>>>,>>9.99")       
           cBelopp[11] = string(iAntDiff)
           cBelopp[12] = STRING(dVerdiDiff,"->>,>>>,>>9.99")
           cBelopp[13] = STRING(dOpprVerdi,"->>,>>>,>>9.99")
           cBelopp[14] = STRING(dOpptVerdi,"->>,>>>,>>9.99").

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    DO ii = 6 TO 14:
        IF CAN-DO("6,7,9,11,12,13,14",STRING(11)) THEN DO:
            IF iCols[ii] <> 0 THEN
                RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(cBelopp[ii]),iCols[ii]).
            ELSE
                RUN pdf_text_to IN h_PDFinc ("Spdf",TRIM(cBelopp[ii]),iToRight[ii]).
        END.
    END.
    RUN pdf_skip    IN h_PDFinc ("Spdf").

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
  DEF VAR cRowidList   AS CHAR       NO-UNDO.
  DEFINE VARIABLE cTmpData AS CHARACTER NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  
/*   RUN gvelgutskrtell.w (OUTPUT iRapportValg). */
/*   if iRapportValg = 0 then                    */
/*     return no-apply.                          */
  ASSIGN iCols[1] = 0   
         iCols[2] = 21  
         iCols[3] = 68  
         iCols[4] = 0   
         iCols[5] = 95  
         iCols[6] = 0   
         iCols[7] = 0   
         iCols[8] = 168 
         iCols[9] = 0   
         iCols[10] = 0
         iCols[11] = 0
         iCols[12] = 0
         iCols[13] = 0
         iCols[14] = 0.

ASSIGN 
    iToRight[1] = 9
    iToRight[2] = 0
    iToRight[3] = 0
    iToRight[4] = 40
    iToRight[5] = 0
    iToRight[6] = 64
    iToRight[7] = 70
    iToRight[8] = 0
    iToRight[9] = 83
    iToRight[10] = 90
    iToRight[11] = 94
    iToRight[12] = 104
    iToRight[13] = 114
    iToRight[14] = 123.

  IF cQry = "" THEN 
  DO:
    IF ENTRY(4,cParam,'|') = '*' THEN
      ASSIGN cQry = "FOR EACH TelleLinje WHERE Tellelinje.Tellenr = " + STRING(TelleHode.Tellenr) + " NO-LOCK BY TelleLinje.ArtikkelNr".
    ELSE
      ASSIGN 
        cRowidList = ENTRY(4,cParam,'|')
        cQry = "FOR EACH TelleLinje WHERE can-do(" + QUOTER(cRowidList) + ",string(rowid(tellelinje))) NO-LOCK BY TelleLinje.ArtikkelNr".

  END.

  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(BUFFER Tellelinje:HANDLE).
  hQuery:QUERY-PREPARE(cQry).
  hQuery:QUERY-OPEN().


  ASSIGN pcRappFil = SESSION:TEMP-DIR + "Tellelinje" + "_" + STRING(Tellehode.tellenr) + ".pdf".
  
  RUN pdf_new IN h_PDFinc ("Spdf",pcRappFil).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
/*   RUN new_page. */
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
  RUN pdf_new_page IN h_PDFinc ("Spdf").
/*   iColLabelPage = 1. */
  
/*   PUT UNFORMATTED cColLabelString SKIP. */
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
     ASSIGN lOK = TRUE.
      /* iRapportValg: 10=alla,11=inga 0-0,20=bara negativ diff ,30=bara positiv diff */
      IF iRapportValg = 11 AND TelleLinje.AntallPar = 0 AND TelleLinje.AntallTalt = 0 THEN
          lOK = FALSE.
      ELSE IF iRapportValg = 20 AND TelleLinje.AntallDiff >= 0 THEN
          lOK = FALSE.
      ELSE IF iRapportValg = 30 AND TelleLinje.AntallDiff <= 0 THEN
          lOK = FALSE.
      ELSE IF iRapportValg = 40 AND TelleLinje.AntallPar = TelleLinje.AntallTalt THEN
          lOK = FALSE.
/*       IF lSvar = TRUE AND TelleLinje.AntallPar = 0 AND TelleLinje.AntallTalt = 0 THEN */
/*           NEXT.                                                                       */
      IF lOK = TRUE THEN DO:
          ASSIGN iAntDiff    = iAntDiff    + TelleLinje.AntallDiff
                 dVerdiDiff  = dVerdiDiff  + TelleLinje.VerdiDiff
                 iAntallPar  = iAntallPar  + TelleLinje.AntallPar 
                 iAntallTalt = iAntallTalt + TelleLinje.AntallTalt
                 dNedskrevet = dNedskrevet + TelleLinje.Nedskrevet
                 dOpprVerdi  = dOpprVerdi  + TelleLinje.OpprVerdi 
                 dOpptVerdi  = dOpptVerdi  + TelleLinje.OpptVerdi.
/*           PUT UNFORMATTED getDataLinje() SKIP. */
          RUN getDataLinje(OUTPUT cTmpData).
          DO ii = 1 TO NUM-ENTRIES(cTmpData,CHR(1)):
              IF iCols[ii] <> 0 THEN
                  RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(ENTRY(ii,cTmpData,CHR(1))),iCols[ii]).
              ELSE
                  RUN pdf_text_to IN h_PDFinc ("Spdf",TRIM(ENTRY(ii,cTmpData,CHR(1))),iToRight[ii]).
          END.
          RUN pdf_skip    IN h_PDFinc ("Spdf").
      END.
      hQuery:GET-NEXT().
  END.
  RUN SumRad.
  RUN pdf_close IN h_PDFinc ("Spdf").
  hQuery:QUERY-CLOSE().
  DELETE OBJECT hQuery.
  hQuery = ?.
 RUN browse2pdf\viewxmldialog.w (pcRappFil,"TELLING").
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

