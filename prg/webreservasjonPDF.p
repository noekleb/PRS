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
DEFINE INPUT  PARAMETER db_id AS DECIMAL     NO-UNDO.
DEFINE VARIABLE rTelleHode AS ROWID      NO-UNDO.
DEFINE VARIABLE cQry AS CHARACTER  NO-UNDO.
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
/* DEFINE VARIABLE iCols  AS INTEGER  EXTENT 14 INITIAL           */
/* /*     [6,10,16,33,37,44,54,59,64,73,82,87,96,105] NO-UNDO. */ */
/*     [6,12,29,32,38,54,60,65,67,76,82,87,96,104] NO-UNDO.       */
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


DEFINE VARIABLE iFrabutikk     AS INTEGER     NO-UNDO.
DEFINE VARIABLE cOrdrenrTekst  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWebButEmail   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWebbutikknavn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilNavn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCols          AS INTEGER EXTENT 10  NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos    AS INTEGER     NO-UNDO.
/* smtpoppsett */
DEFINE VARIABLE cMailhub  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDoAUTH   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAuthType AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cUser     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPassword AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE TT_reserver NO-UNDO
    FIELD butikknr AS INTE
    FIELD navn     AS CHAR
    FIELD nummer   AS INTE
    FIELD email    AS CHAR
    INDEX butikknr IS PRIMARY butikknr.
DEFINE TEMP-TABLE TT_bonglinje NO-UNDO LIKE bonglinje
    FIELD nummer AS INTE
    FIELD bestillingsnr AS CHAR FORMAT "x(15)"
    FIELD farve         AS CHAR FORMAT "x(15)"
    INDEX nummer IS PRIMARY nummer.

{runlib.i}
{ pdf_inc.i "NOT SUPER"}

/* pdf_PageHeader (<stream name>,THIS-PROCEDURE:HANDLE, <Procedure Name>). */

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
         HEIGHT             = 19.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* RUN InitLabels för språkhantering */ 
    FIND bonghode WHERE bonghode.b_id = db_Id NO-LOCK NO-ERROR.
    IF NOT AVAIL bonghode THEN
        RETURN "FEIL".
/*     FIND FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND               */
/*         bonglinje.ttid = 87 AND bonglinje.antall = 9 NO-LOCK NO-ERROR.          */
/*     IF NOT AVAIL bonglinje THEN                                                 */
/*         RETURN "FEIL".                                                          */
/*     IF NOT CAN-FIND(butiker WHERE butiker.butik = INT(bonglinje.linjesum)) THEN */
/*         RETURN "FEIL".                                                          */
/*     iFrabutikk = bonglinje.linjesum. */
    FIND FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND
        bonglinje.ttid = 95 NO-LOCK NO-ERROR.
    IF AVAIL bonglinje THEN
        cOrdrenrTekst = bonglinje.bongtekst.
/*     FIND TelleHode WHERE ROWID(TelleHode) = rTelleHode NO-LOCK NO-ERROR. */
/*     IF TelleHode.TTId = 8 THEN                                           */
/*         ASSIGN cRub[9] = "Nedskr".                                       */
/*     IF AVAIL TelleHode THEN DO:                                          */
    IF AVAIL bonghode THEN DO:
        FIND butiker WHERE butiker.butik = Bonghode.butikknr NO-LOCK.
        ASSIGN cWebButEmail   = butiker.ePostAdresse
               cWebbutikknavn = butiker.butnamn.

        {syspara.i 1 1 100 cKundenavn}
        {syspara.i 1 1 101 cPolygon}
        {syspara.i 5 1 1 iCl INT}

        {syspara.i 50 50 1 cMailhub }
        {syspara.i 50 50 2 cDoAUTH  }
        {syspara.i 50 50 3 cAuthType}
        {syspara.i 50 50 4 cUser    }
        {syspara.i 50 50 5 cPassword}



/*         FIND TransType WHERE TransType.TTId = TelleHode.TTId NO-LOCK NO-ERROR. */
/*         IF AVAIL TransType THEN                                                */
        ASSIGN cTitle = "WEBRESERVASJON".
        FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
        ASSIGN iCLProfilNr = Butiker.ProfilNr.
/*         FIND Butiker WHERE Butiker.Butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR. */
/*         ASSIGN cButNamn = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".          */
        RUN ByggTT.
        IF CAN-FIND(FIRST tt_reserver) THEN
            RUN reservasjon.
    END.
    ELSE DO:
        RETURN "FEIL".
    END.
RETURN RETURN-VALUE.

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
DEFINE VARIABLE iNummer AS INTEGER     NO-UNDO.
    FOR EACH bonglinje NO-LOCK WHERE bonglinje.b_id = bonghode.b_id:
        IF bonglinje.ttid <> 6 THEN
            NEXT.
        IF bonglinje.antall >= 0 THEN
            NEXT.
        FIND tt_reserver WHERE tt_reserver.butikknr = BongLinje.MButikkNr NO-ERROR.
        IF NOT AVAIL tt_reserver THEN DO:
            FIND butiker WHERE butiker.butik = BongLinje.MButikkNr NO-LOCK NO-ERROR.
            IF NOT AVAIL butiker THEN
                NEXT.
            iNummer = inummer + 1.
            CREATE tt_reserver.
            ASSIGN tt_reserver.butikknr = BongLinje.MButikkNr
                   tt_reserver.navn     = butiker.butnamn
                   tt_reserver.nummer   = iNummer
                   tt_reserver.email    = TRIM(butiker.ePostAdresse).
        END.
        RELEASE farg.
        FIND artbas WHERE artbas.artikkelnr = DECI(bonglinje.artikkelnr) NO-LOCK NO-ERROR.
        FIND strekkode WHERE strekkode.kode = bonglinje.strekkode NO-LOCK NO-ERROR.
        IF AVAIL artbas AND TRIM(ArtBas.LevFargKod) = "" THEN
            FIND farg OF artbas NO-LOCK NO-ERROR.
        CREATE tt_bonglinje.
        BUFFER-COPY bonglinje TO tt_bonglinje.
        ASSIGN tt_bonglinje.nummer        = tt_reserver.nummer
               tt_bonglinje.bestillingsnr = IF AVAIL strekkode THEN strekkode.Bestillingsnummer ELSE ""
               tt_bonglinje.bestillingsnr = IF TRIM(tt_bonglinje.bestillingsnr) = "" AND AVAIL artbas THEN artbas.levkod ELSE tt_bonglinje.bestillingsnr
               tt_bonglinje.bestillingsnr = SUBSTR(tt_bonglinje.bestillingsnr,1,15)
               tt_bonglinje.farve         = IF AVAIL farg THEN farg.farbeskr ELSE IF AVAIL artbas THEN artbas.levfargkod ELSE ""
               tt_bonglinje.farve         = IF TRIM(tt_bonglinje.farve) = "" AND AVAIL artbas THEN artbas.levfargkod ELSE tt_bonglinje.farve
               tt_bonglinje.farve         = SUBSTR(tt_bonglinje.farve,1,15)
               tt_bonglinje.BongTekst     = IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE tt_BongLinje.BongTekst.
    END.
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
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",85),iCols[1]).
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf","Vg",iCols[1]).
  RUN pdf_text    IN h_PDFinc ("Spdf","/").
  RUN pdf_text_at IN h_PDFinc ("Spdf","Løpnr",iCols[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Levart.nr",iCols[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Bongtekst",iCols[4]).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Farve",iCols[5]).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Strl",iCols[6]).
  RUN pdf_text_at IN h_PDFinc ("Spdf","Antall",iCols[7]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",FILL("_",85),iCols[1]).
  
/*   RUN pdf_line IN h_PDFinc  ("Spdf", iX, 720, pdf_PageWidth("Spdf") - 61 , 720, 1). */
  
  
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_skip    IN h_PDFinc ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).

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
                           + " (" + pdf_TotalPages("Spdf") + ")", 97).

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
  RUN pdf_text_at IN h_PDFinc ("Spdf","Reservasjon nettbutikk",20).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cOrdrenrTekst,155).

/*   RUN pdf_text    IN h_PDFinc ("Spdf","/").                */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","Løpnr",iCols[2]).   */
/*   RUN pdf_text_at IN h_PDFinc ("Spdf","Best.nr",iCols[3]). */
  RUN pdf_skip IN h_PDFinc ("Spdf").
  
  RUN pdf_text_at IN h_PDFinc ("Spdf","Nettbutikk:",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",cWebbutikknavn,45).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf","Lagerbutikk:",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",tt_reserver.navn,45).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text_at IN h_PDFinc ("Spdf","Dato:",20).
  RUN pdf_text_at IN h_PDFinc ("Spdf",Bonghode.dato,45).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_skip IN h_PDFinc ("Spdf").



/*
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
  RUN pdf_text IN h_PDFinc ("Spdf",STRING(TODAY)).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",24).
  RUN pdf_text_center IN h_PDFinc ("Spdf",cTitle,200,580).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",12).
  RUN pdf_line IN h_PDFinc ("Spdf",10,550,780,550,0.5).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",9).
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_skip IN h_PDFinc ("Spdf").
    /* /*   PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P8><R+2>".                                 */ */
/* /*   PUT UNFORMATTED cColLabelString SKIP.                                                                    */ */

  RUN pdf_set_TextX IN h_PDFinc ("Spdf", 100).
  RUN pdf_text IN h_PDFinc ("Spdf","X1").
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_set_TextY IN h_PDFinc ("Spdf", 100).
  RUN pdf_text IN h_PDFinc ("Spdf","Y1").
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_set_TextX IN h_PDFinc ("Spdf", 200).
  RUN pdf_text IN h_PDFinc ("Spdf","X2").
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_set_TextY IN h_PDFinc ("Spdf", 200).
  RUN pdf_text IN h_PDFinc ("Spdf","Y2").
  RUN pdf_skip IN h_PDFinc ("Spdf").
  RUN pdf_text IN h_PDFinc ("Spdf","Efter XY").



DEFINE FRAME PageHeader
   HEADER
      "<ALIGN=BASE><FArial><R3><P12><B><C6>" STRING(TODAY)
      "<P12></B><C110><P9>" PAGE-NUMBER FORMAT ">>>>" SKIP
      "<R4><C6><B>Butikk:" cButNamn "</B>" SKIP
      "<R5><C6><FROM><R5><C113><LINE>" SKIP
      WITH PAGE-TOP STREAM-IO WIDTH 255.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Reservasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reservasjon Procedure 
PROCEDURE Reservasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iNummer AS INTEGER     NO-UNDO.
    FOR EACH tt_reserver:
        /* fixa filnamn */
        RUN reservasjonPDF.
/*         RUN test. */
/*         maila */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reservasjonPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reservasjonPDF Procedure 
PROCEDURE reservasjonPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hQuery       AS HANDLE NO-UNDO.
  DEF VAR hBuffer      AS HANDLE NO-UNDO.
  DEF VAR iRapportValg AS INTEGER NO-UNDO.
  DEF VAR lOK          AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage AS INTEGER     NO-UNDO.
  ASSIGN iCols[1] = 20   /* vg      */
         iCols[2] = 29   /* lopnr   */
         iCols[3] = 42   /* bestnr  */
         iCols[4] = 76   /* bongtxt */
         iCols[5] = 126  /* farve   */
         iCols[6] = 153  /* storl   */
         iCols[7] = 179. /* antall  */

  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  IF NUM-ENTRIES(cOrdrenrTekst," ") = 2 THEN
      cOrdreNr = ENTRY(2,cOrdrenrTekst," ").
  ELSE
      cOrdreNr = string(TIME).
  cFilNavn = SESSION:TEMP-DIR + "Weborder" + "_" + cOrdreNr + "_" + STRING(tt_reserver.butikknr) + ".pdf".
    /* skapa ett utlägg pr butik */
  RUN pdf_new IN h_PDFinc ("Spdf",cFilNavn).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Portrait").
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
  RUN new_page.
  iColLabelPage = 1.
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
/*           "RIGHT" pdf_RightMargin ("Spdf") */
  RUN PageHeader.
/*   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10). */
    
/*                                                   */
/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */
  
  RUN ColLabels.
/*   RUN pdf_set_TextY IN h_PDFinc ("Spdf",680). */
    /* Collabels */
FOR EACH tt_bonglinje WHERE tt_bonglinje.nummer =  tt_reserver.nummer:
  IF pdf_Page ("Spdf") > iColLabelPage THEN DO:
      iColLabelPage = iColLabelPage + 1.
      RUN ColLabels.
  END.
  RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.varegr,iCols[1]).
  RUN pdf_text    IN h_PDFinc ("Spdf","/").
  RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.lopenr,iCols[2]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.bestillingsnr,iCols[3]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.bongtekst,iCols[4]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.farve,iCols[5]).
  RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(tt_bonglinje.storrelse),iCols[6]).
  RUN pdf_text_to IN h_PDFinc ("Spdf",ABS(tt_bonglinje.antall),80).
/*         RUN pdf_text_at IN h_PDFinc ("Spdf",ABS(tt_bonglinje.antall),iCols[7]). */
  RUN pdf_skip    IN h_PDFinc ("Spdf").
END.

 RUN pdf_close IN h_PDFinc ("Spdf").
 RUN SendEmail IN THIS-PROCEDURE.
/*  RUN browse2pdf\viewxmldialog.w (cFilNavn,"WEBORDRE"). */
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendEmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendEmail Procedure 
PROCEDURE SendEmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lMailOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cMailTo AS CHARACTER   NO-UNDO.
    

    IF SEARCH(cFilnavn) = ? THEN
        cFilNavn = "".

    cMailTo = IF tt_reserver.email <> "" AND NUM-ENTRIES(tt_reserver.email,"@") = 2 THEN tt_reserver.email ELSE cWebButEmail.

    RUN prssmtpmailv5_7a.p (
    /*mailhub    */   cMailhub,
    /*EmailTo    */   cMailTo,
    /*EmailFrom  */   cWebButEmail,
    /*EmailCC    */   "",
    /*Attachments*/   IF cFilNavn = "" THEN "" ELSE ENTRY(NUM-ENTRIES(cFilNavn,"\"),cFilNavn,"\"),
    /*LocalFiles */   IF cFilNavn = "" THEN "" ELSE cFilNavn,
    /*Subject    */   "Reservasjon nettbutikk",
    /*Body       */   "",
    /*MIMEHeader */   "",
    /*BodyType   */   "",
    /*Importance */   0,
    /*L_DoAUTH   */   IF cDoAUTH = "1" THEN TRUE ELSE FALSE,
    /*C_AuthType */   cAuthType,
    /*C_User     */   cUser,
    /*C_Password */   cPassword,
    /*oSuccessful*/  OUTPUT lMailOK,
    /*vMessage   */  OUTPUT cMessage) NO-ERROR.
    IF lMailOK = TRUE AND cFilNavn <> "" THEN
        OS-DELETE VALUE(cFilNavn).
/*                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-test) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test Procedure 
PROCEDURE test :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hQuery       AS HANDLE NO-UNDO.
  DEF VAR hBuffer      AS HANDLE NO-UNDO.
  DEF VAR iRapportValg AS INTEGER NO-UNDO.
  DEF VAR lOK          AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
  ASSIGN iCols[1] = 20   /* vg      */
         iCols[2] = 29   /* lopnr   */
         iCols[3] = 42   /* bestnr  */
         iCols[4] = 76   /* bongtxt */
         iCols[5] = 126  /* farve   */
         iCols[6] = 153  /* storl   */
         iCols[7] = 179. /* antall  */

  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  IF NUM-ENTRIES(cOrdrenrTekst," ") = 2 THEN
      cOrdreNr = ENTRY(2,cOrdrenrTekst," ").
  ELSE
      cOrdreNr = string(TIME).
  cFilNavn = SESSION:TEMP-DIR + "Weborder" + "_" + cOrdreNr + "_" + STRING(tt_reserver.butikknr) + ".pdf".
    /* skapa ett utlägg pr butik */
  RUN pdf_new IN h_PDFinc ("Spdf",cFilNavn).
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter").
  RUN pdf_set_BottomMargin IN h_PDFinc ("Spdf", 60).
  RUN pdf_set_PaperType IN h_PDFinc ("Spdf","A4").
    /*   RUN LoadFonts. */
  RUN pdf_set_Orientation IN h_PDFinc ("Spdf","Portrait").
  RUN new_page.
  ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
         iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
/*           "RIGHT" pdf_RightMargin ("Spdf") */
  RUN PageHeader.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    
/*                                                   */
/*     PUT UNFORMATTED "Weborder reservasjon" SKIP   */
/*         "Webbutikk  :" cWebbutikknavn        SKIP */
/*         "Lagerbutikk:" tt_reserver.navn SKIP      */
/*         "Notat      :" cOrdrenrTekst SKIP         */
/*         .                                         */
  RUN ColLabels.
    /* Collabels */
  DO ii  = 1 TO 38:
      FOR EACH tt_bonglinje WHERE tt_bonglinje.nummer =  tt_reserver.nummer:
          iRow = irow + 1.
          RUN pdf_text_at IN h_PDFinc ("Spdf",pdf_TextX("Spdf"),iCols[1]).
          RUN pdf_text    IN h_PDFinc ("Spdf","/").
          RUN pdf_text_at IN h_PDFinc ("Spdf",iRow,iCols[2]).
          RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.bestillingsnr,iCols[3]).
          RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.bongtekst,iCols[4]).
          RUN pdf_text_at IN h_PDFinc ("Spdf",tt_bonglinje.farve,iCols[5]).
          RUN pdf_text_at IN h_PDFinc ("Spdf",TRIM(tt_bonglinje.storrelse),iCols[6]).
          RUN pdf_text_to IN h_PDFinc ("Spdf",ABS(tt_bonglinje.antall),80).
  /*         RUN pdf_text_at IN h_PDFinc ("Spdf",ABS(tt_bonglinje.antall),iCols[7]). */
          RUN pdf_skip    IN h_PDFinc ("Spdf").
      END.
  END.

 RUN pdf_close IN h_PDFinc ("Spdf").
 RUN SendEmail IN THIS-PROCEDURE.
 RUN browse2pdf\viewxmldialog.w (cFilNavn,"WEBORDRE").
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

