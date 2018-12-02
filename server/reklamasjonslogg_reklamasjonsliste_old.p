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

DEFINE VARIABLE cFilNavn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCols          AS INTEGER EXTENT 15  NO-UNDO.
DEFINE VARIABLE cColLabels     AS CHARACTER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iToRight       AS INTEGER EXTENT 15  NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageHeight    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iCL            AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFirmanavn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAdress        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPostadress AS CHARACTER   NO-UNDO.

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

RUN ByggTT.

IF CAN-FIND(FIRST tt_reklamasjon) THEN DO:
    {syspara.i 5 1 1 iCL INT}
    FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
    FIND post WHERE post.postnr = butiker.BuPonr NO-LOCK NO-ERROR.
    cAdress     = IF AVAIL butiker THEN Butiker.BuPadr ELSE "".
    cPostadress = (IF AVAIL butiker THEN Butiker.BuPoNr ELSE "") + IF AVAIL post THEN " " + Post.Beskrivelse ELSE "".
    {syspara.i 1 1 100 cFirmaNavn}
    
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
                   ihBuffer:BUFFER-FIELD('ReklamasjonsNr'):BUFFER-VALUE NO-LOCK.
        FIND reklamasjonslogg WHERE reklamasjonslogg.reklamasjonsnr = reklamasjonslinje.reklamasjonsnr NO-LOCK.
        FIND Artbas WHERE artbas.artikkelnr = reklamasjonslinje.artikkelnr NO-LOCK NO-ERROR.
        IF NOT AVAIL reklamasjonslinje THEN
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
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).

/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */

    /* Collabels */
/*   RUN pdf_text_align IN h_PDFinc ("Spdf","Reservasjon nettbutikk","CENTER", iLeftMargin, iRMarginPos). */
  RUN pdf_text_at IN h_PDFinc ("Spdf","Reklamasjonsliste",50).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",cOrdrenrTekst,155). */

  RUN pdf_skip IN h_PDFinc ("Spdf").
  
  RUN pdf_text_at IN h_PDFinc ("Spdf","Leverandør:",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",STRING(tt_reklamasjon.levnr) + " " + (IF AVAIL Levbas THEN Levbas.levnamn ELSE "Ukjent"),45).

  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Butiker THEN Butiker.butnamn ELSE "Ukjent"),130).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Levbas THEN Levbas.levadr ELSE ""),45).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cAdress,130).
/*   RUN pdf_text_at IN h_PDFinc ("Spdf",TODAY,45). */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Levbas THEN LevBas.levponr + " " + LevBas.levpadr ELSE ""),45).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cPostadress,130).

  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf",(IF AVAIL Levbas THEN LevBas.levland ELSE ""),45).
  RUN pdf_skip IN h_PDFinc ("Spdf").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-RapportPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF Procedure 
PROCEDURE RapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iSumAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dSumVk     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumPris   AS DECIMAL     NO-UNDO.

  ASSIGN iCols[1] = 10   /* vg      */
         iCols[2] = 19   /* lopnr   */
         iCols[3] = 33   /* levart  */
         iCols[4] = 68   /* varetxt */
         iCols[5] = 112  /* storl   */
         iCols[6] = 128  /* butik   */
         iCols[7] = 141 /* kassa */
         iCols[8] = 155 /* bong  */
         iCols[9] = 168 /* antall  */
         iCols[10] = 186 /* I.pris  */
         iCols[11] = 205 /* pris  */
         iCols[12] = 215 /* fkode  */
         iCols[13] = 229. /* feiltxt  */
  ASSIGN cColLabels[1]  = "Vg" /* vg      */
         cColLabels[2]  = "/Løpnr" /* lopnr   */
         cColLabels[3]  = "Levart.nr" /* bestnr  */
         cColLabels[4]  = "Varetekst" /* bongtxt */
         cColLabels[5]  = "Storl" /* farve   */
         cColLabels[6]  = "Butik" /* storl   */
         cColLabels[7]  = "Kassa" /* antall  */
         cColLabels[8]  = "Bong" /* antall  */
         cColLabels[9]  = "Antall" /* antall  */
         cColLabels[10] = "I.pris" /* antall  */
         cColLabels[11] = "Pris" /* antall  */
         cColLabels[12] = "Fkode" /* antall  */
         cColLabels[13] = "Feiltekst". /* antall  */
  ASSIGN iToRight[6]  = 58
         iToRight[7]  = 64
         iToRight[8]  = 70
         iToRight[9]  = 75
         iToRight[10] = 82
         iToRight[11] = 89
         iToRight[12] = 95.


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
         iRMarginPos = pdf_PageWidth ("Spdf") - iLeftMargin.
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
FOR EACH tt_reklamasjon BREAK BY tt_reklamasjon.levnr:
    FIND ReklamasjonsLinje WHERE ReklamasjonsLinje.reklamasjonsnr = tt_reklamasjon.reklamasjonsnr AND
                                 ReklamasjonsLinje.LinjeNr        = tt_reklamasjon.Linjenr NO-LOCK.
    FIND feilkode OF reklamasjonslinje NO-LOCK NO-ERROR.
  IF FIRST-OF(tt_reklamasjon.levnr) THEN DO:
      FIND levbas WHERE levbas.levnr = tt_reklamasjon.levnr NO-LOCK NO-ERROR.
      RUN PageHeader.
      RUN ColLabels.
      ASSIGN iSumAntall = 0
             dSumVk     = 0
             dSumPris   = 0.
  END.
  IF pdf_Page ("Spdf") > iColLabelPage THEN DO:
/*       iColLabelPage = iColLabelPage + 1. */
/*       RUN ColLabels.                     */
  END.
  
/*
         ReklamasjonsLinje.Varetekst              
         ReklamasjonsLinje.Dato
         ReklamasjonsLinje.Storl
         ReklamasjonsLinje.BongId
         ReklamasjonsLinje.Antall
         ReklamasjonsLinje.VVarekost
         ReklamasjonsLinje.LevKod
         ReklamasjonsLinje.AkseptertVerdi  
         ReklamasjonsLinje.ArtikkelNr 
         ReklamasjonsLinje.Beskr 
         ReklamasjonsLinje.BongId 
         ReklamasjonsLinje.BongLinjeNr 
         ReklamasjonsLinje.BrukerID 
         ReklamasjonsLinje.Butik 
         ReklamasjonsLinje.Dato  
         ReklamasjonsLinje.ETid 
         ReklamasjonsLinje.FeilKode 
         ReklamasjonsLinje.FeilNotat 
         ReklamasjonsLinje.ForsNr 
         ReklamasjonsLinje.KassaNr 
         ReklamasjonsLinje.LevFargKod  
         ReklamasjonsLinje.LinjeNr  
         ReklamasjonsLinje.Mva 
         ReklamasjonsLinje.Pris 
         ReklamasjonsLinje.RabKr 
         ReklamasjonsLinje.ReklamTotal 
         ReklamasjonsLinje.ReklamUtgifter 
         ReklamasjonsLinje.ReklamVerdi 
         ReklamasjonsLinje.SelgerNr 
         ReklamasjonsLinje.SolgtBongId 
         ReklamasjonsLinje.SolgtDato 
         ReklamasjonsLinje.SolgtForsNr 
         ReklamasjonsLinje.SolgtIButikk  
         ReklamasjonsLinje.SubtotalRab  
*/

  ASSIGN iSumAntall = iSumAntall + Reklamasjonslinje.antall
         dSumVk     = dSumVk     + (Reklamasjonslinje.antall * ReklamasjonsLinje.VVarekost)
         dSumPris   = dSumPris   + (Reklamasjonslinje.antall * ReklamasjonsLinje.Pris).
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.vg,iCols[1]).
  RUN pdf_text    IN h_PDFinc ("Spdf","/").
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.lopnr,iCols[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf", SUBSTR(Reklamasjonslinje.Levkod,1,14),iCols[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",SUBSTR(Reklamasjonslinje.Varetekst,1,25),iCols[4]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",Reklamasjonslinje.storl,iCols[5]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.butik,iToRight[6]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.kassanr,iToRight[7]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",ReklamasjonsLinje.BongId,iToRight[8]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",Reklamasjonslinje.antall,iToRight[9]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.VVarekost,">>,>>9.99"),iToRight[10]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.Pris,">>,>>9.99"),iToRight[11]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(ReklamasjonsLinje.FeilKode),iToRight[12]).
  IF AVAIL feilkode THEN
      RUN pdf_text_at IN h_PDFinc ("Spdf",SUBSTR(Feilkode.Beskrivelse,1,34),iCols[13]).


/*         RUN pdf_text_at IN h_PDFinc ("Spdf",ABS(Reklamasjonslinje.antall),iCols[7]). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  IF LAST-OF(tt_reklamasjon.levnr) THEN DO:
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      RUN pdf_skip    IN h_PDFinc ("Spdf").
      RUN pdf_text_to IN h_PDFinc ("Spdf",iSumAntall,iToRight[9]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSUmVk,">>,>>9.99"),iToRight[10]).
      RUN pdf_text_to IN h_PDFinc ("Spdf",STRING(dSumPris,">>,>>9.99"),iToRight[11]).
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

