&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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

DEFINE INPUT  PARAMETER hTempTable AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER cType      AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cOrdreUtFil AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer     AS HANDLE      NO-UNDO.
DEF VAR rTelleHode AS ROWID NO-UNDO.
DEF VAR rTellelinje AS ROWID NO-UNDO.

DEFINE VARIABLE cTitle          AS CHARACTER INIT "Ordreforslag" NO-UNDO.
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
DEFINE VARIABLE iAntfelt AS INTEGER INIT 11    NO-UNDO.
DEFINE VARIABLE cRub   AS CHARACTER EXTENT 13 INITIAL
    ["Artikkelnr","Lev.art","Artnavn","Farge","Str","Enhet","Utpris","Enh.pris","Lager","Bestilt.","Bestilt verdi"] NO-UNDO.
DEFINE VARIABLE cLevRub AS CHARACTER EXTENT 4 INITIAL
    ["Levnr","Navn","Antall","Sum"]  NO-UNDO.
DEFINE VARIABLE iCols  AS INTEGER  EXTENT 13 NO-UNDO.
DEFINE VARIABLE iLevCols  AS INTEGER  EXTENT 4 NO-UNDO.
DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 13  NO-UNDO.
DEFINE VARIABLE iToRight       AS INTEGER EXTENT 13  NO-UNDO.
DEFINE VARIABLE iLevToRight       AS INTEGER EXTENT 4  NO-UNDO.
DEFINE VARIABLE cTmpData AS CHARACTER EXTENT 13 NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
DEFINE VARIABLE cButiker       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLevNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLevSumantal AS INTEGER     NO-UNDO.
DEFINE VARIABLE dLevSum      AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lSumPage     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSumPageInserted     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iAntPages     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPGnumLev     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntPagesLev       AS INTEGER     NO-UNDO.
DEFINE TEMP-TABLE TT_LevSum NO-UNDO
    FIELD levnr   AS INTE
    FIELD levnavn AS CHAR
    FIELD levant  AS INTE
    FIELD lsum    AS DECI
    INDEX levnr IS PRIMARY UNIQUE levnr.

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
         HEIGHT             = 18.43
         WIDTH              = 62.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* hBrwLinje */



/*                                                                                         */
/*     rTellehode = TO-ROWID(ENTRY(2,cParam,'|')).                                         */
/*     FIND TelleHode WHERE ROWID(TelleHode) = rTelleHode NO-LOCK NO-ERROR.                */
/*                                                                                         */
/*     IF TelleHode.TTId = 8 THEN                                                          */
/*         ASSIGN cRub[9] = "Nedskr".                                                      */
/*     IF AVAIL TelleHode THEN DO:                                                         */
/*         {syspara.i 1 1 100 cKundenavn} */
/*         {syspara.i 1 1 101 cPolygon}   */
        {syspara.i 5 1 1 iCl INT}
/*         FIND TransType WHERE TransType.TTId = TelleHode.TTId NO-LOCK NO-ERROR.          */
/*         IF AVAIL TransType THEN                                                         */
/*             ASSIGN cTitle = cTitle + "-" + TransType.Beskrivelse.                       */
        FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
        ASSIGN iCLProfilNr = Butiker.ProfilNr.
/*         MESSAGE SOURCE-PROCEDURE:INTERNAL-ENTRIES SKIP */
/*             SOURCE-PROCEDURE:NAME                      */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.         */
/*         FIND Butiker WHERE Butiker.Butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR. */
/*         ASSIGN cButNamn = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".          */
/* /*         RUN TellelisteXPrint. */                                                     */
        IF NOT CAN-DO("F,O",cType) THEN
            RETURN.
        IF cType = "F" THEN
            ASSIGN cRub[10]  = "Forslag"
                   cRub[11] = "Verdi".
        RUN SkrivPDF.
/*     END.                                                                                */
/*     ELSE DO:                                                                            */
/*         MESSAGE "Finner ikke TelleHode."                                                */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                          */
/*     END.                                                                                */

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
  IF lSumPage = FALSE THEN DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
      DO ii = 1 TO iAntfelt:
          IF iCols[ii] <> 0 THEN
              RUN pdf_text_at IN h_PDFinc ("Spdf",cRub[ii],iCols[ii]).
          ELSE
              RUN pdf_text_to IN h_PDFinc ("Spdf",cRub[ii],iToRight[ii]).
      END.
      RUN pdf_skip    IN h_PDFinc ("Spdf").
    /*   RUN pdf_skip   IN h_PDFinc   ("Spdf").               */
    /*   RUN pdf_text_refline  IN h_PDFinc    ("Spdf","AT").  */
    /*   RUN pdf_skip   IN h_PDFinc   ("Spdf").               */
    /*   RUN pdf_text_refline   IN h_PDFinc   ("Spdf","TO").  */
    /*   RUN pdf_skip    IN h_PDFinc  ("Spdf").               */
      RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf") + 6, pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 17 , pdf_TextY("Spdf") + 5, 1).
      RUN pdf_skip    IN h_PDFinc ("Spdf").
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
  END.
  ELSE DO:

      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
      DO ii = 1 TO 4:
          IF iLevCols[ii] <> 0 THEN
              RUN pdf_text_at IN h_PDFinc ("Spdf",cLevRub[ii],iLevCols[ii]).
          ELSE
              RUN pdf_text_to IN h_PDFinc ("Spdf",cLevRub[ii],iLevToRight[ii]).
      END.
      RUN pdf_skip    IN h_PDFinc ("Spdf").
    /*   RUN pdf_skip   IN h_PDFinc   ("Spdf").               */
    /*   RUN pdf_text_refline  IN h_PDFinc    ("Spdf","AT").  */
    /*   RUN pdf_skip   IN h_PDFinc   ("Spdf").               */
    /*   RUN pdf_text_refline   IN h_PDFinc   ("Spdf","TO").  */
    /*   RUN pdf_skip    IN h_PDFinc  ("Spdf").               */
      RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf") + 6, pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 17 , pdf_TextY("Spdf") + 5, 1).
      RUN pdf_skip    IN h_PDFinc ("Spdf").
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
  END.

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
    DEFINE VARIABLE dKost AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iLagant AS INTEGER     NO-UNDO.
    IF cType = "O" THEN
        ASSIGN dKost   = hBuffer:BUFFER-FIELD("KostBekreftet"):BUFFER-VALUE()
               iAntall = hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE().
    ELSE IF cType = "F" THEN
        ASSIGN dKost   = hBuffer:BUFFER-FIELD("KostForslag"):BUFFER-VALUE()
               iAntall = hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE().
    /* "tst" */
/*     ASSIGN cTmpData[1] = STRING(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) */
/*              cTmpData[2] = TRIM("KAPPEL.SAT 150*200*50")                           */
/*              cTmpData[3] = TRIM("AAAAAAAAA0AAAAAAAAA0AAAAAAAAA0")                  */
/*              cTmpData[4] = TRIM(hBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE()) */
/*              cTmpData[5] = TRIM("XXXXXXXXXL")                                      */
/*              cTmpData[6] = TRIM("AAAAA")                                           */
/*              cTmpData[7] = STRING(33333,">>>,>>9.99")                              */
/*              cTmpData[8] = STRING(22222,">>>,>>9.99")                              */
/*              cTmpData[9] = STRING(999)                                             */
/*              cTmpData[10] = STRING(999)                                            */
/*              cTmpData[11] = STRING(88888,">>>,>>9.99")                             */
    
/*  !!! ORIG                                                                                                         */
    ASSIGN cTmpData[1] = STRING(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE())
             cTmpData[2] = TRIM(ArtBas.Levkod)
             cTmpData[3] = TRIM(ArtBas.Beskr)
             cTmpData[4] = TRIM(hBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE())
             cTmpData[5] = TRIM(hBuffer:BUFFER-FIELD("Alfastr"):BUFFER-VALUE())
             cTmpData[6] = TRIM(ArtBas.SalgsEnhet)
             cTmpData[7] = IF AVAIL artpris THEN STRING(Artpris.pris[1],">>>,>>9.99") ELSE STRING(0,">>>,>>9.99")
             cTmpData[8] = STRING(ROUND(dKost / iAntall,2),">>>,>>9.99")
             cTmpData[10] = STRING(iAntall)
             cTmpData[11] = STRING(dKost,">>>,>>9.99")

                 
                 
/*              cTmpData[6] = TRIM(hBuffer:BUFFER-FIELD("LevFargKod"):BUFFER-VALUE() + "Gråvitsvartgul") */
/*              cTmpData[9] = STRING(hBuffer:BUFFER-FIELD("KostForslag"):BUFFER-VALUE(),">>,>>9.99")     */
/*             cTmpData[10] = STRING(hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE())               */
/*             cTmpData[11] = STRING(hBuffer:BUFFER-FIELD("KostBekreftet"):BUFFER-VALUE(),">>,>>9.99")   */
/*             cTmpData[12] = STRING(hBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE())                       */
/*             cTmpData[13] = TRIM(hBuffer:BUFFER-FIELD("Levnamn"):BUFFER-VALUE()).                      */
      .
      /* Finn lager */
      FIND artlag WHERE artlag.artikkelnr = DECI(cTmpData[1]) AND 
                        ArtLag.butik      = butiker.butik AND 
                        artlag.strkode    = int(hBuffer:BUFFER-FIELD("strkode"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
      IF AVAIL artlag THEN 
          cTmpData[9] = STRING(artlag.lagant).
      ELSE
          cTmpData[9] = "".
      IF cTmpData[4] = "" THEN DO:
          FIND farg OF artbas NO-LOCK NO-ERROR.
          IF AVAIL farg THEN
              cTmpData[4] = farg.farbeskr.
      END.
      ASSIGN iLevSumantal = iLevSumantal + iAntall
             dLevSum      = dLevSum      + dKost.
      IF NOT AVAIL TT_LevSum OR TT_LevSum.Levnr <> LevBas.Levnr THEN DO:
          CREATE TT_LevSum.
          ASSIGN  TT_LevSum.levnr   = LevBas.levnr
                  TT_LevSum.levnavn = levBas.levnamn.
      END.
      ASSIGN TT_LevSum.levant = TT_LevSum.levant + iAntall
             TT_LevSum.lsum   = TT_LevSum.lsum   + dKost.

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
  RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(cButiker),10).
  
  RUN pdf_text_to IN h_PDFinc  ("Spdf",  "Side: "
                           + STRING(pdf_page("Spdf") + (IF lSumPageInserted = TRUE THEN 0 ELSE iAntPagesLev))
                                + " (" + STRING(iAntPages) + ")", 120).
/*                                 + " (" + pdf_TotalPages("Spdf") + ")", 138). */

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
  DEFINE VARIABLE iLPos1 AS INTEGER INITIAL 13    NO-UNDO.
  DEFINE VARIABLE iLPos2 AS INTEGER INITIAL 80    NO-UNDO.
  DEFINE VARIABLE iLPos3 AS INTEGER INITIAL 213    NO-UNDO.
  DEFINE VARIABLE iPos1 AS INTEGER INITIAL 34    NO-UNDO.
  DEFINE VARIABLE iPos2 AS INTEGER INITIAL 105    NO-UNDO.
  DEFINE VARIABLE iPos3 AS INTEGER INITIAL 225    NO-UNDO.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
/*   RUN pdf_text_to IN h_PDFinc ("Spdf",TRIM(cTmpData[ii]),124) */
  IF cType = "O" THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf","Ordre",iLpos1).
  IF cType = "F" THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf","Forslag",iLpos1).
  RUN pdf_text_at IN h_PDFinc ("Spdf",": " + STRING(PlListeHode.PlListeId),iPos1).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Sendt dato",iLpos2).
  RUN pdf_text_at IN h_PDFinc ("Spdf",": " + STRING(TODAY),iPos2).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","Side",iLpos3).                                  */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",": " + STRING(pdf_page("Spdf")),iPos3).          */
  
  RUN pdf_skip IN h_PDFinc ("Spdf").
/*   RUN pdf_text_refline  IN h_PDFinc    ("Spdf","AT"). */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf","Butikk",iLpos1).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
  RUN pdf_text_at IN h_PDFinc ("Spdf",Butiker.butnamn,iLpos1).
  IF lSumPage = TRUE THEN DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
      RUN pdf_text_at IN h_PDFinc ("Spdf","LEVERANDØROVERSIKT",48).
  END.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
  RUN pdf_skip IN h_PDFinc ("Spdf").

  IF lSumPage = FALSE THEN DO:
      RUN pdf_skip IN h_PDFinc ("Spdf").
      RUN pdf_text_at IN h_PDFinc ("Spdf","Leverandør",iLpos1).
      RUN pdf_skip IN h_PDFinc ("Spdf").
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(hBuffer:BUFFER-FIELD("Levnamn"):BUFFER-VALUE()),iLpos1).
      RUN pdf_skip IN h_PDFinc ("Spdf").

      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
      RUN pdf_skip IN h_PDFinc ("Spdf").
  END.


/*   RUN pdf_text_at IN h_PDFinc ("Spdf",STRING(TODAY),6).       */
/*   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24). */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",cTitle,40).             */
/*   RUN pdf_skip IN h_PDFinc ("Spdf").                          */
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin("Spdf") + 6, pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 17 , pdf_TextY("Spdf") + 5, 1).
/*   RUN pdf_line IN h_PDFinc ("Spdf", pdf_LeftMargin("Spdf"), pdf_TextY("Spdf") + 5, pdf_PageWidth("Spdf") - 20 , */
/*                  pdf_TextY("Spdf") + 5, 1).                                                                     */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",138),iCols[1]). */
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN COlLabels.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkrivPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivPDF Procedure 
PROCEDURE SkrivPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE pcRappFil    AS CHAR    NO-UNDO.
  DEFINE VARIABLE iRapportValg AS INTEGER NO-UNDO.
  DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE I2 AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLevSumAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dLevSumTOT    AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iAntLev       AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iLastLevnr AS INTEGER     NO-UNDO.
  ASSIGN iCols[1] = 0
         iCols[2] = 30  
         iCols[3] = 67
         iCols[4] = 120
         iCols[5] = 160
         iCols[6] = 186   
         iCols[7] = 0  
         iCols[8] = 0 
         iCols[9] = 0   
         iCols[10] = 0   
         iCols[11] = 0
/*          iCols[11] = 0   */
/*          iCols[12] = 0   */
/*          iCols[13] = 240 */
      .

ASSIGN 
    iToRight[1] = 12
    iToRight[2] = 0
    iToRight[3] = 0
    iToRight[4] = 0
    iToRight[5] = 0
    iToRight[6] = 0
    iToRight[7] = 93
    iToRight[8] = 102
    iToRight[9] = 108
    iToRight[10] = 114
    iToRight[11] = 123
/*     iToRight[11] = 95  */
/*     iToRight[12] = 100 */
/*     iToRight[13] = 0   */
    .
  ASSIGN iLevCols[1] = 0
         iLevCols[2] = 30  
         iLevCols[3] = 0
         iLevCols[4] = 0
         iLevToRight[1] = 12
         iLevToRight[2] = 0
         iLevToRight[3] = 48
         iLevToRight[4] = 58.

CREATE BUFFER hBuffer FOR TABLE hTempTable.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hbuffer).
hQuery:QUERY-PREPARE("for each " + hBuffer:NAME + " NO-LOCK by levnr by beskr by artikkelnr by StrSeq").
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF iLastLevnr <> INT(hBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE()) THEN
        iAntLev = iAntLev + 1.
    iLastLevnr = INT(hBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE()).
    hQuery:GET-NEXT().
END.
/* Hur många sidor blir det */
/* !! */ iAntLev = iAntLev.
iAntPages = 1.
i2 = iAntlev.
DO WHILE i2 - 47 > 0:
    i2 = i2 - 47.
    iAntPages = iAntPages + 1.
END.
ASSIGN iAntPagesLev = iAntPages.
       iAntPages    = iAntPages + iAntLev.
hQuery:GET-FIRST().
IF hBuffer:AVAIL THEN DO:
  FIND PlListeHode WHERE PlListeHode.PlListeId = DECI(hBuffer:BUFFER-FIELD("PlListeId"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
  IF AVAIL PlListeHode THEN DO:
      FIND butiker WHERE butiker.butik = PlListeHode.FraButikkNr NO-LOCK NO-ERROR.
  END.
  ELSE
      LEAVE.
  IF AVAIL butiker THEN
      cButiker = Butiker.butnamn.
  ELSE
      LEAVE.
      IF cType = "O" THEN
          ASSIGN pcRappFil = SESSION:TEMP-DIR + "Ordre" + "_" + STRING(hBuffer:BUFFER-FIELD("PlListeId"):BUFFER-VALUE()) + ".pdf".
      ELSE IF cType = "F" THEN
          ASSIGN pcRappFil = SESSION:TEMP-DIR + "Forslag" + "_" + STRING(hBuffer:BUFFER-FIELD("PlListeId"):BUFFER-VALUE()) + ".pdf".
  cOrdreUtFil = pcRappfil.
  RUN pdf_new IN h_PDFinc ("Spdf",pcRappFil).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Portrait").
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
/*   RUN new_page. */
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
/*   RUN pdf_new_page IN h_PDFinc ("Spdf"). */
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
      lOK = TRUE.
      IF cType = "O" THEN DO:
          IF hBuffer:BUFFER-FIELD("KostBekreftet"):BUFFER-VALUE() <= 0 THEN
              lOK = FALSE.
          IF NOT hBuffer:BUFFER-FIELD("AntallPlukket"):BUFFER-VALUE() > 0 THEN
              lOK = FALSE.
      END.
      ELSE DO:
          IF hBuffer:BUFFER-FIELD("KostForslag"):BUFFER-VALUE() <= 0 THEN
              lOK = FALSE.
          IF NOT hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE() > 0 THEN
              lOK = FALSE.
      END.
      FIND Artbas WHERE artbas.artikkelnr = DECI(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
      IF NOT AVAIL Artbas THEN
          lOK = FALSE.
      FIND levbas OF artbas NO-LOCK NO-ERROR.
      IF NOT AVAIL Levbas THEN
          lOK = FALSE.
      IF lOK = TRUE THEN DO:
          FIND artpris WHERE artpris.artikkelnr = Artbas.artikkelnr AND
                             artpris.profilnr = butiker.profilnr NO-LOCK NO-ERROR.
          IF NOT AVAIL artpris THEN
              FIND artpris WHERE artpris.artikkelnr = Artbas.artikkelnr AND
                                 artpris.profilnr = iCLProfilNr NO-LOCK NO-ERROR.
          IF INT(hBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE()) <> iLevnr THEN DO:
               IF iLevSumantal <> 0 AND dLevSum <> 0 THEN DO:
                   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",7).
                   RUN pdf_text_to IN h_PDFinc ("Spdf","SUM",iToRight[9]).
                   RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(iLevSumantal),iToRight[10]).
                   RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dLevSum,">>>,>>9.99"),iToRight[11]).
                   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
               END.
              RUN pdf_new_page IN h_PDFinc ("Spdf").
              ASSIGN iLevSumantal = 0 
                     dLevSum      = 0.
              iLevNr = hBuffer:BUFFER-FIELD("LevNr"):BUFFER-VALUE().
          END.
    /*       RUN pdf_skip IN h_PDFinc ("Spdf").                  */
    /*       RUN pdf_text_refline  IN h_PDFinc    ("Spdf","AT"). */
    /*       RUN pdf_skip IN h_PDFinc ("Spdf").                  */
    /*       RUN pdf_skip IN h_PDFinc ("Spdf").                  */
    /*       RUN pdf_text_refline  IN h_PDFinc    ("Spdf","TO"). */
    /*       RUN pdf_skip IN h_PDFinc ("Spdf").                  */
          RUN getDataLinje.
    /*       DO i2 = 1 TO 60: !!! 46 datarader*/
          DO ii = 1 TO iAntFelt:
              IF iCols[ii] <> 0 THEN
                  RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(cTmpData[ii]),iCols[ii]).
              ELSE
                  RUN pdf_text_to IN h_PDFinc ("Spdf",TRIM(cTmpData[ii]),iToRight[ii]).
          END.
          RUN pdf_skip    IN h_PDFinc ("Spdf").
    /*       END. */
      END.
      hQuery:GET-NEXT().
  END.
  IF iLevSumantal <> 0 AND dLevSum <> 0 THEN DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",7).
      RUN pdf_text_to IN h_PDFinc ("Spdf","SUM",iToRight[9]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(iLevSumantal),iToRight[10]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dLevSum,">>>,>>9.99"),iToRight[11]).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
  END.
  IF CAN-FIND(FIRST TT_LevSum) THEN DO:
      lSumPage = TRUE.
      RUN pdf_insert_page IN h_PDFinc ("Spdf",1,"BEFORE").
      ASSIGN lSumPageInserted = TRUE
             iPGnumLev        = 1
             i2               = 0.
      FOR EACH TT_LevSum:
          ASSIGN iLevSumAntall = iLevSumAntall + TT_LevSum.levant
                 dLevSumTOT    = dLevSumTOT    + TT_LevSum.lsum.
          RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(TT_LevSum.levnr),iLevToRight[1]).
          RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(TT_LevSum.levnavn),iLevCols[2]).
          RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(TT_LevSum.levant),iLevToRight[3]).
          RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(TT_LevSum.lsum,">>>,>>9.99"),iLevToRight[4]).
/*           RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(999999),iLevToRight[1]).                                  */
/*           RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM("Wintersteiger Norge, Board & Skiteknikk AS"),iLevCols[2]). */
/*           RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(108),iLevToRight[3]).                                     */
/*           RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(999999,">>>,>>9.99"),iLevToRight[4]).                     */
          i2 = i2 + 1.
          RUN pdf_skip    IN h_PDFinc ("Spdf").
          IF i2 MOD 47 = 0 AND i2 < iAntlev THEN DO:
              iPGnumLev = iPGnumLev + 1.
              RUN pdf_insert_page IN h_PDFinc ("Spdf",iPGnumLev,"BEFORE").
          END.
      END.
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",7).
      RUN pdf_text_at IN h_PDFinc ("Spdf","SUM",40).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(iLevSumAntall),iLevToRight[3]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dLevSumTOT,">>>,>>9.99"),iLevToRight[4]).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",7).
      RUN pdf_skip    IN h_PDFinc ("Spdf").
  END.
/*   RUN SumRad. */
  RUN pdf_close IN h_PDFinc ("Spdf").
END.

  hQuery:QUERY-CLOSE().
  DELETE OBJECT hBuffer.
  DELETE OBJECT hQuery.
  DELETE PROCEDURE h_PDFinc.
  hQuery = ?.
  hBuffer = ?.
/*  RUN browse2pdf\viewxmldialog.w (pcRappFil,"TELLING"). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SumRad) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SumRad Procedure 
PROCEDURE SumRad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cBelopp AS CHAR EXTENT 14    NO-UNDO.
  DEFINE VARIABLE ii      AS INTEGER     NO-UNDO.
/*     ASSIGN cBelopp[6] = STRING(iAntallPar)                                           */
/*            cBelopp[7] = STRING(iAntallTalt)                                          */
/*            cBelopp[9] = STRING(dSalgsVerdi,"->>,>>>,>>9.99")                         */
/*            cBelopp[11] = string(iAntDiff)                                            */
/*            cBelopp[12] = STRING(dVerdiDiff,"->>,>>>,>>9.99")                         */
/*            cBelopp[13] = STRING(dOpprVerdi,"->>,>>>,>>9.99")                         */
/*            cBelopp[14] = STRING(dOpptVerdi,"->>,>>>,>>9.99").                        */
/*                                                                                      */
/*     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).                      */
/*     DO ii = 6 TO 14:                                                                 */
/*         IF CAN-DO("6,7,9,11,12,13,14",STRING(11)) THEN DO:                           */
/*             IF iCols[ii] <> 0 THEN                                                   */
/*                 RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(cBelopp[ii]),iCols[ii]).    */
/*             ELSE                                                                     */
/*                 RUN pdf_text_to IN h_PDFinc ("Spdf",TRIM(cBelopp[ii]),iToRight[ii]). */
/*         END.                                                                         */
/*     END.                                                                             */
/*     RUN pdf_skip    IN h_PDFinc ("Spdf").                                            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

