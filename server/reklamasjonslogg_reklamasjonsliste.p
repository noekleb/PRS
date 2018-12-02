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

DEF INPUT  PARAM icParam     AS CHAR  NO-UNDO. 
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR  NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR  NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO. 

DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fReklamasjonsNr AS INT    NO-UNDO.
DEF VAR iStatus         AS INT    NO-UNDO.

DEFINE VARIABLE cFilNavn       AS CHARACTER            NO-UNDO.
DEFINE VARIABLE iCols          AS INTEGER   EXTENT 15  NO-UNDO.
DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iToRight       AS INTEGER   EXTENT 15  NO-UNDO.
DEFINE VARIABLE iColsSko       AS INTEGER   EXTENT 15  NO-UNDO.
DEFINE VARIABLE cColLabelsSko  AS CHARACTER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iToRightSko    AS INTEGER   EXTENT 15  NO-UNDO.
DEFINE VARIABLE cExtraCLabel   AS CHARACTER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER              NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER              NO-UNDO.
DEFINE VARIABLE iPageHeight    AS INTEGER              NO-UNDO.
DEFINE VARIABLE iPageWidth     AS INTEGER              NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER              NO-UNDO.
DEFINE VARIABLE iCLprofilnr    AS INTEGER              NO-UNDO.
DEFINE VARIABLE cFirmanavn     AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cAdress        AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cPostadress    AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cSprak         AS CHARACTER            NO-UNDO.
DEFINE VARIABLE cSkomodus      AS CHARACTER            NO-UNDO.
DEFINE BUFFER artprisbut FOR butiker.
DEFINE TEMP-TABLE tt_reklamasjon NO-UNDO
    FIELD levnr AS INTE
    FIELD reklamasjonsnr AS DECI
    FIELD linjenr AS INTE.

DEFINE BUFFER btt_reklamasjon FOR tt_reklamasjon.
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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
FIND bruker WHERE Bruker.BrukerID = USERID("skotex") NO-LOCK NO-ERROR.
IF AVAIL bruker THEN
    cSprak = Bruker.Lng.
{syspara.i 1 1 54 cSkomodus}

RUN ByggTT.

IF CAN-FIND(FIRST tt_reklamasjon) THEN DO:
    {syspara.i 5 1 1 iCL INT}
    FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
    iCLprofilnr = Butiker.profilnr.
    FIND post WHERE post.postnr = butiker.BuPonr NO-LOCK NO-ERROR.
    cAdress     = IF AVAIL butiker THEN Butiker.BuAdr ELSE "".
    cPostadress = (IF AVAIL butiker THEN Butiker.BuPoNr ELSE "") + IF AVAIL post THEN " " + Post.Beskrivelse ELSE "".
    {syspara.i 1 1 100 cFirmaNavn}
    IF cSkomodus = "1" THEN
        RUN RapportPDFSko.
    ELSE
        RUN RapportPDF.
    ASSIGN 
      ocReturn = cFilnavn
      obOk     = TRUE
    .
END.
IF VALID-HANDLE(h_PDFinc) THEN DO:
        DELETE PROCEDURE h_PDFinc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT Procedure 
PROCEDURE ByggTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hQuery          AS HANDLE NO-UNDO.
DEF VAR fReklamasjonsNr AS INT    NO-UNDO.
DEF VAR iStatus         AS INT    NO-UNDO.

ASSIGN
  fReklamasjonsNr = DEC(ENTRY(1,icParam,';'))
.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FOR EACH reklamasjonslinje WHERE reklamasjonslinje.reklamasjonsnr =
                   dec(ihBuffer:BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE) AND 
                              reklamasjonslinje.linjenr = INT(ihBuffer:BUFFER-FIELD('Linjenr'):BUFFER-VALUE) NO-LOCK.
        FIND reklamasjonslogg WHERE reklamasjonslogg.reklamasjonsnr = reklamasjonslinje.reklamasjonsnr NO-LOCK.
        FIND Artbas WHERE artbas.artikkelnr = reklamasjonslinje.artikkelnr NO-LOCK NO-ERROR.
        IF NOT AVAIL reklamasjonslinje THEN
            NEXT.
        IF CAN-FIND(tt_reklamasjon WHERE tt_reklamasjon.levnr               = reklamasjonslogg.levnr           AND
                                             tt_reklamasjon.reklamasjonsnr  = reklamasjonslinje.reklamasjonsnr AND
                                             tt_reklamasjon.linjenr         = reklamasjonslinje.linjenr) THEN
            NEXT.
        CREATE tt_reklamasjon.
        ASSIGN tt_reklamasjon.levnr          = reklamasjonslogg.levnr
               tt_reklamasjon.reklamasjonsnr = reklamasjonslinje.reklamasjonsnr
               tt_reklamasjon.linjenr        = reklamasjonslinje.linjenr.
    END.

    hQuery:GET-NEXT().
END.

hQuery:QUERY-CLOSE().
DELETE OBJECT hQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ColLabels) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColLabels Procedure 
PROCEDURE ColLabels :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEFINE VARIABLE iX AS INTEGER     NO-UNDO.        */
/*   MESSAGE pdf_PointSize ("Spdf")                    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","",iCols[1]). */
/*   iX = pdf_TextX("Spdf").                           */
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",85),iCols[1]). */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                       */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  DO ii = 1 TO EXTENT(iCols):
      IF iCols[ii] = 0 OR iCols[ii] = ? THEN
          LEAVE.
      RUN pdf_text_at IN h_PDFinc ("Spdf",cColLabels[ii],iCols[ii]).
  END.
  RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",138),iCols[1]).
  
/*   RUN pdf_line IN h_PDFinc  ("Spdf", iX, 720, pdf_PageWidth("Spdf") - 61 , 720, 1). */
  
  
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ColLabelsSko) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColLabelsSko Procedure 
PROCEDURE ColLabelsSko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEFINE VARIABLE iX AS INTEGER     NO-UNDO.        */
/*   MESSAGE pdf_PointSize ("Spdf")                    */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.            */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","",iColsSko[1]). */
/*   iX = pdf_TextX("Spdf").                           */
DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",85),iColsSko[1]). */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                       */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  DO ii = 1 TO EXTENT(iColsSko):
      IF iColsSko[ii] = 0 OR iColsSko[ii] = ? THEN
          LEAVE.
      RUN pdf_text_at IN h_PDFinc ("Spdf",cColLabelsSko[ii],iColsSko[ii]).
  END.
  RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",142),iColsSko[1]).
  
/*   RUN pdf_line IN h_PDFinc  ("Spdf", iX, 720, pdf_PageWidth("Spdf") - 61 , 720, 1). */
  
  
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

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

  RUN pdf_text_to IN h_PDFinc ("Spdf",TODAY,15).
  RUN pdf_text_to IN h_PDFinc  ("Spdf", cFirmanavn, 60).
  IF cSprak = "SE" THEN
      RUN pdf_text_to IN h_PDFinc  ("Spdf",  "Sida: "
                               + STRING(pdf_page("Spdf"))
                               + " (" + pdf_TotalPages("Spdf") + ")", 135).
  ELSE
      RUN pdf_text_to IN h_PDFinc  ("Spdf",  "Side: "
                               + STRING(pdf_page("Spdf"))
                               + " (" + pdf_TotalPages("Spdf") + ")", 135).

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
  DEFINE VARIABLE cNamn AS CHARACTER   NO-UNDO.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).

/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */

    /* Collabels */
/*   RUN pdf_text_align IN h_PDFinc ("Spdf","Reservasjon nettbutikk","CENTER", iLeftMargin, iRMarginPos). */
  IF cSprak = "SE" THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf","Reklamationslista",50).
  ELSE
      RUN pdf_text_at IN h_PDFinc ("Spdf","Reklamasjonsliste",50).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",cOrdrenrTekst,155). */

  RUN pdf_skip IN h_PDFinc ("Spdf").
  IF cSprak = "SE" THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf","Leverantör:",20).
  ELSE
      RUN pdf_text_at IN h_PDFinc ("Spdf","Leverandør:",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",STRING(tt_reklamasjon.levnr) + " " + (IF AVAIL Levbas THEN Levbas.levnamn ELSE "Ukjent"),45).
  IF AVAIL butiker THEN
      cNamn = IF butiker.butfirmanavn <> "" THEN butiker.butfirmanavn ELSE butiker.Butnamn.
  ELSE
      cNamn = "Ukjent".
  RUN pdf_text_at IN h_PDFinc ("Spdf",cNamn,130).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Levbas THEN Levbas.levadr ELSE ""),45).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cAdress,130).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",TODAY,45). */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Levbas THEN LevBas.levponr + " " + LevBas.levpadr ELSE ""),45).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cPostadress,130).

  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Levbas THEN LevBas.levland + " (" + LevBas.valkod + ")" ELSE ""),45).
  RUN pdf_skip IN h_PDFinc ("Spdf").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF Procedure 
PROCEDURE RapportPDF PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iSumAntall      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dSumVk          AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumPris        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumReklamtotal AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iOldPage        AS INTEGER     NO-UNDO.
  ASSIGN iCols[1] = 10   /* vg      */
         iCols[2] = 19   /* lopnr   */
         iCols[3] = 33   /* levart  */
         iCols[4] = 68   /* varetxt */
         iCols[5] = 112  /* storl   */
         iCols[6] = 124  /* butik   */
         iCols[7] = 134 /* kassa */
         iCols[8] = 145 /* bong  */
         iCols[9] = 161 /* antall  */
         iCols[10] = 170 /* I.pris  */
         iCols[11] = 185 /* pris  */
         iCols[12] = 199 /* Sum kost  */
         iCols[13] = 217 /* fkode  */
         iCols[14] = 229. /* feiltxt  */
  IF cSprak = "SE" THEN
      ASSIGN cColLabels[1]  = "Vg" /* vg      */
             cColLabels[2]  = "/Löpnr" /* lopnr   */
             cColLabels[3]  = "Levart.nr" /* bestnr  */
             cColLabels[4]  = "Varutext" /* bongtxt */
             cColLabels[5]  = "Storl" /* farve   */
             cColLabels[6]  = "But" /* storl   */
             cColLabels[7]  = "Ant" /* antall  */
             cColLabels[8]  = "I.pris" /* antall  */
             cColLabels[9]  = "Pris" /* antall  */
             cColLabels[10] = "Rabatt" /* antall  */
             cColLabels[11] = "Utgift" /* antall  */
             cColLabels[12] = "Sum.kost" /* antall  */
             cColLabels[13] = "Kod" /* antall  */
             cColLabels[14] = "Feltext". /* antall  */
  ELSE
      ASSIGN cColLabels[1]  = "Vg" /* vg      */
             cColLabels[2]  = "/Løpnr" /* lopnr   */
             cColLabels[3]  = "Levart.nr" /* bestnr  */
             cColLabels[4]  = "Varetekst" /* bongtxt */
             cColLabels[5]  = "Storl" /* farve   */
             cColLabels[6]  = "But" /* storl   */
             cColLabels[7]  = "Ant" /* antall  */
             cColLabels[8]  = "I.pris" /* antall  */
             cColLabels[9]  = "Pris" /* antall  */
             cColLabels[10] = "Rabatt" /* antall  */
             cColLabels[11] = "Utgift" /* antall  */
             cColLabels[12] = "Sum.kost" /* antall  */
             cColLabels[13] = "Kode" /* antall  */
             cColLabels[14] = "Feiltekst". /* antall  */
  ASSIGN iToRight[6]  = 55
         iToRight[7]  = 59
         iToRight[8]  = 65
         iToRight[9]  = 71
         iToRight[10] = 76
         iToRight[11] = 82
         iToRight[12] = 90
         iToRight[13] = 95
         .


  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  cFilNavn = SESSION:TEMP-DIR + "Reklamasjonsliste" + "_" + STRING(TIME) + ".pdf".
    /* skapa ett utlägg pr butik */
  RUN pdf_new IN h_PDFinc ("Spdf",cFilNavn).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").

  iPageHeight = pdf_PageHeight ("Spdf").
  iPageWidth  = pdf_PageWidth ("Spdf").
  
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
  RUN new_page.
  iColLabelPage = 1.
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
/*           "RIGHT" pdf_RightMargin ("Spdf") */
/*   RUN PageHeader. */
/*   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10). */
    
/*                                                   */
/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */
  
/*   RUN ColLabels. */
/*   RUN pdf_set_TextY IN h_PDFinc ("Spdf",680). */
    /* Collabels */
iColLabelPage = 1.
FOR EACH tt_reklamasjon BREAK BY tt_reklamasjon.levnr BY tt_reklamasjon.reklamasjonsnr BY tt_reklamasjon.Linjenr:
    FIND ReklamasjonsLinje WHERE ReklamasjonsLinje.reklamasjonsnr = tt_reklamasjon.reklamasjonsnr AND
                                 ReklamasjonsLinje.LinjeNr        = tt_reklamasjon.Linjenr NO-LOCK.
    FIND feilkode OF reklamasjonslinje NO-LOCK NO-ERROR.
  IF FIRST-OF(tt_reklamasjon.levnr) THEN DO:
      FIND levbas WHERE levbas.levnr = tt_reklamasjon.levnr NO-LOCK NO-ERROR.
      RUN PageHeader.
/*       RUN ColLabels. */
      ASSIGN iSumAntall      = 0
             dSumVk          = 0
             dSumPris        = 0
             dSumReklamtotal = 0.
  END.
  IF pdf_Page ("Spdf") > iOldPage THEN DO:
/*       iColLabelPage = iColLabelPage + 1. */
      RUN ColLabels.
      iOldPage = pdf_Page ("Spdf").
  END.

  ASSIGN iSumAntall = iSumAntall + Reklamasjonslinje.antall
         dSumVk     = dSumVk     + (Reklamasjonslinje.antall * ReklamasjonsLinje.VVarekost)
         dSumPris   = dSumPris   + (Reklamasjonslinje.antall * ReklamasjonsLinje.Pris)
         dSumReklamtotal = dSumReklamtotal + Reklamasjonslinje.ReklamTotal
      .

  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.vg,iCols[1]).
  RUN pdf_text    IN h_PDFinc ("Spdf","/").
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.lopnr,iCols[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf", SUBSTR(Reklamasjonslinje.Levkod,1,14),iCols[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",SUBSTR(Reklamasjonslinje.Varetekst,1,25),iCols[4]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.storl,iCols[5]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.butik,iToRight[6]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.antall,iToRight[7]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.VVarekost,">>>,>>9"),iToRight[8]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.Pris,">>>,>>9"),iToRight[9]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.RabKr,">>,>>9"),iToRight[10]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.ReklamUtgifter,">>,>>9"),iToRight[11]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.ReklamTotal,">,>>>,>>9"),iToRight[12]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.FeilKode),iToRight[13]).
  IF AVAIL feilkode THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf",SUBSTR(Feilkode.Beskrivelse,1,34),iCols[14]).


/*         RUN pdf_text_at IN h_PDFinc ("Spdf",ABS(Reklamasjonslinje.antall),iCols[7]). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  IF LAST-OF(tt_reklamasjon.levnr) THEN DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      RUN pdf_skip    IN h_PDFinc ("Spdf").
      RUN pdf_text_to IN h_PDFinc ("Spdf",iSumAntall,iToRight[7]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSUmVk,">>>,>>9"),iToRight[8]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSumPris,">>>,>>9"),iToRight[9]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSumReklamtotal,">,>>>,>>9"),iToRight[12]).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

  END.
  IF LAST-OF(tt_reklamasjon.levnr) AND NOT LAST(tt_reklamasjon.levnr) THEN DO:
      RUN pdf_new_page IN h_PDFinc ("Spdf").
  END.

END.
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_to IN h_PDFinc ("Spdf","T",6).            */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","A",10).           */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_to IN h_PDFinc ("Spdf","T",7).            */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","A",12).           */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_to IN h_PDFinc ("Spdf","T",82).           */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","A",192).          */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12). */
/*   RUN pdf_text_xy IN h_PDFinc ("Spdf","10 100",10,100).  */
/*   RUN pdf_text_xy IN h_PDFinc ("Spdf","40 200",40,200).  */
/*   RUN pdf_text_to IN h_PDFinc ("Spdf","T-",5).           */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").                  */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","A",8).            */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").            */
/*   RUN pdf_text_to IN h_PDFinc ("Spdf","T123",121). */
/*   RUN pdf_skip    IN h_PDFinc ("Spdf").            */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","A",285).    */

 RUN pdf_close IN h_PDFinc ("Spdf").
/*  RUN SendEmail IN THIS-PROCEDURE. */
/*  RUN browse2pdf\viewxmldialog.w (cFilNavn,"WEBORDRE"). */
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDFSko) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDFSko Procedure 
PROCEDURE RapportPDFSko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii              AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr        AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iSumAntall      AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dSumVk          AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumPris        AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumReklamtotal AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iOldPage        AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cNotat          AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE dInpris AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dRab1%  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dRab2%  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dKoeff1 AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dKoeff2 AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSum    AS DECIMAL     NO-UNDO.


  DEFINE VARIABLE cTyp AS CHARACTER   NO-UNDO.
  ASSIGN iColsSko[1] = 10   /* vg      */
         iColsSko[2] = 19   /* lopnr   */
         iColsSko[3] = 31   /* levart  */
         iColsSko[4] = 88   /* levfärg  */
         iColsSko[5] = 116  /* storl   */
         iColsSko[6] = 124  /* butik   */
         iColsSko[7] = 134 /* kassa */
         iColsSko[8] = 145 /* bong  */
         iColsSko[9] = 161 /* antall  */
      iColsSko[10] = 178   /* rab2 %  */ 
      iColsSko[11] = 195 /* Sum kost  */
      iColsSko[12] = 215 /* fkode  */   
      iColsSko[13] = 224. /* feiltxt  */
      iColsSko[14] = 268. /* feiltxt  */
      iColsSko[15] = 286.
/*          iColsSko[9] = 185 /* pris  */      */
/*          iColsSko[10] = 199 /* Sum kost  */ */
/*          iColsSko[11] = 217 /* fkode  */    */
/*          iColsSko[12] = 229. /* feiltxt  */ */
  IF cSprak = "SE" THEN DO:
      ASSIGN cColLabelsSko[1]  = "Vg" /* vg      */
             cColLabelsSko[2]  = "/Löpnr" /* lopnr   */
             cColLabelsSko[3]  = "Lev.artnr" /* bestnr  */
             cColLabelsSko[4]  = "Lev.färgkod" /* bestnr  */
             cColLabelsSko[5]  = "Strl" /* farve   */
             cColLabelsSko[6]  = "But" /* storl   */
             cColLabelsSko[7]  = "Ant" /* antall  */
             cColLabelsSko[8]  = "Val.pris" /* antall  */
             cColLabelsSko[9]  = "Rab1%" /* antall  */
             cColLabelsSko[10] = "Rab2%" /* antall  */
             cColLabelsSko[11] = "Sum.kost" /* antall  */
             cColLabelsSko[12] = "Kod" /* antall  */
             cColLabelsSko[13] = "Feltext" /* antall  */
             cColLabelsSko[14] = "Notat". /* antall  */
      
      ASSIGN iToRightSko[6]  = 55
             iToRightSko[7]  = 59
             iToRightSko[8]  = 67
             iToRightSko[9]  = 73
             iToRightSko[10] = 80
             iToRightSko[11] = 89
             iToRightSko[12] = 94.
  END.
  ELSE DO:
      ASSIGN cColLabelsSko[1]  = "Vg" /* vg      */
             cColLabelsSko[2]  = "/Løpnr" /* lopnr   */
             cColLabelsSko[3]  = "Levart.nr" /* bestnr  */
             cColLabelsSko[4]  = "Storl" /* farve   */
             cColLabelsSko[5]  = "But" /* storl   */
             cColLabelsSko[6]  = "Ant" /* antall  */
             cColLabelsSko[7]  = "I.pris" /* antall  */
             cColLabelsSko[8]  = "Pris" /* antall  */
             cColLabelsSko[9] = "Utgift" /* antall  */
             cColLabelsSko[10] = "Sum.kost" /* antall  */
             cColLabelsSko[11] = "Kode" /* antall  */
             cColLabelsSko[12] = "Feiltekst" /* antall  */
             cColLabelsSko[13] = "Notat". /* antall  */
      ASSIGN iToRightSko[5]  = 55
             iToRightSko[6]  = 59
             iToRightSko[7]  = 65
             iToRightSko[8]  = 71
             iToRightSko[9] = 78
             iToRightSko[10] = 86
             iToRightSko[11] = 90.
  END.

  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  cFilNavn = SESSION:TEMP-DIR + "Reklamasjonsliste" + "_" + STRING(TIME) + ".pdf".
    /* skapa ett utlägg pr butik */
  RUN pdf_new IN h_PDFinc ("Spdf",cFilNavn).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Landscape").

  iPageHeight = pdf_PageHeight ("Spdf").
  iPageWidth  = pdf_PageWidth ("Spdf").
  
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
  RUN new_page.
  iColLabelPage = 1.
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
/*           "RIGHT" pdf_RightMargin ("Spdf") */
/*   RUN PageHeader. */
/*   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10). */
    
/*                                                   */
/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */
  
/*   RUN ColLabels. */
/*   RUN pdf_set_TextY IN h_PDFinc ("Spdf",680). */
    /* Collabels */
iColLabelPage = 1.
FOR EACH tt_reklamasjon BREAK BY tt_reklamasjon.levnr BY tt_reklamasjon.reklamasjonsnr BY tt_reklamasjon.Linjenr:
    FIND ReklamasjonsLinje WHERE ReklamasjonsLinje.reklamasjonsnr = tt_reklamasjon.reklamasjonsnr AND
                                 ReklamasjonsLinje.LinjeNr        = tt_reklamasjon.Linjenr NO-LOCK.
    FIND feilkode OF reklamasjonslinje NO-LOCK NO-ERROR.
  IF FIRST-OF(tt_reklamasjon.levnr) THEN DO:
      FIND levbas WHERE levbas.levnr = tt_reklamasjon.levnr NO-LOCK NO-ERROR.
      RUN PageHeader.
/*       RUN ColLabels. */
      ASSIGN cExtraCLabel[8]  = IF AVAIL levbas THEN LevBas.valkod ELSE "---"
             cExtraCLabel[11] = "SEK".
      ASSIGN iSumAntall      = 0
             dSumVk          = 0
             dSumPris        = 0
             dSumReklamtotal = 0.
  END.
  IF pdf_Page ("Spdf") > iOldPage THEN DO:
/*       iColLabelPage = iColLabelPage + 1. */
      RUN ColLabelsSko.
      iOldPage = pdf_Page ("Spdf").
  END.
  RELEASE artpris.
  FIND artprisbut WHERE artprisbut.butik = Reklamasjonslinje.butik NO-LOCK NO-ERROR.
  IF AVAIL artprisbut THEN
      FIND artpris WHERE artpris.artikkelnr = Reklamasjonslinje.artikkelnr AND artpris.profilnr = artprisbut.profilnr NO-LOCK NO-ERROR.
  IF NOT AVAIL artpris THEN
      FIND artpris WHERE artpris.artikkelnr = Reklamasjonslinje.artikkelnr AND artpris.profilnr = iCLprofilnr NO-LOCK NO-ERROR.
  FIND artpris WHERE artpris.artikkelnr = Reklamasjonslinje.artikkelnr AND artpris.profilnr = iCLProfilnr NO-LOCK NO-ERROR.
  IF NOT AVAIL artpris THEN
      FIND FIRST artpris NO-LOCK NO-ERROR.
  ASSIGN iSumAntall = iSumAntall + Reklamasjonslinje.antall.
/*                                                                                        */
/*          dSumVk     = dSumVk     + (Reklamasjonslinje.antall * ArtPris.ValPris[1])     */
/*          dSumPris   = dSumPris   + (Reklamasjonslinje.antall * ReklamasjonsLinje.Pris) */
/*          dSumReklamtotal = dSumReklamtotal + Reklamasjonslinje.ReklamTotal             */
      .
  cTyp = IF Reklamasjonslinje.TTId = 62 THEN "(RK)" ELSE STRING(Reklamasjonslinje.TTId = 3,"(KR)/(LR)").
  cNotat = TRIM(SUBSTR(ReklamasjonsLinje.FeilNotat,1,10)).
  ASSIGN dInpris = 0
         dRab1%  = 0
         dRab2%  = 0
         dSum    = 0.
  IF Reklamasjonslinje.TTId = 62 THEN
      assign dSum = Reklamasjonslinje.ReklamTotal
             dSumReklamtotal = dSumReklamtotal + dSum.
  ELSE
      ASSIGN dInpris = ArtPris.ValPris[1]
             dRab1%  = ArtPris.Rab1%[1]
             dRab2%  = ArtPris.Rab2%[1]
             dKoeff1 = 1 - (dRab1% / 100) 
             dKoeff2 = 1 - (dRab2% / 100) 
             dSum    = Reklamasjonslinje.Antall * (ArtPris.ValPris[1] * dKoeff1 * dKoeff2)
             dSum = ROUND(dSum,2)
             dSumReklamtotal = dSumReklamtotal + dSum.


  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.vg,iColsSko[1]).
  RUN pdf_text    IN h_PDFinc ("Spdf","/").
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.lopnr,iColsSko[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf", SUBSTR(Reklamasjonslinje.Levkod,1,24),iColsSko[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf", SUBSTR(Reklamasjonslinje.Levfargkod,1,12),iColsSko[4]).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",SUBSTR(Reklamasjonslinje.Varetekst,1,25),iColsSko[4]). */
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.storl,iColsSko[5]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.butik,iToRightSko[6]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.antall,iToRightSko[7]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dInpris,">>>,>>9.99"),iToRightSko[8]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ROUND(dRab1%,2),">>9.99"),iToRightSko[9]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ROUND(dRab2%,2),">>9.99"),iToRightSko[10]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSum,">,>>>,>>9.99"),iToRightSko[11]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",IF ReklamasjonsLinje.FeilKode = 0 THEN "" ELSE STRING(ReklamasjonsLinje.FeilKode),iToRightSko[12]).
  IF AVAIL feilkode THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf",SUBSTR(Feilkode.Beskrivelse,1,34),iColsSko[13]).
  ELSE IF ReklamasjonsLinje.FeilKode = 0 THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf", "Övrigt fel",iColsSko[13]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cNotat,iColsSko[14]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cTyp,iColsSko[15]).


/*         RUN pdf_text_at IN h_PDFinc ("Spdf",ABS(Reklamasjonslinje.antall),iColsSko[7]). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  IF LAST-OF(tt_reklamasjon.levnr) THEN DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      RUN pdf_skip    IN h_PDFinc ("Spdf").
      RUN pdf_text_to IN h_PDFinc ("Spdf",iSumAntall,iToRightSko[7]).
/*       RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSUmVk,">>>,>>9"),iToRightSko[8]).   */
/*       RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSumPris,">>>,>>9"),iToRightSko[9]). */
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSumReklamtotal,">,>>>,>>9.99"),iToRightSko[11]).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

  END.
  IF LAST-OF(tt_reklamasjon.levnr) AND NOT LAST(tt_reklamasjon.levnr) THEN DO:
      RUN pdf_new_page IN h_PDFinc ("Spdf").
  END.

END.

 RUN pdf_close IN h_PDFinc ("Spdf").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

