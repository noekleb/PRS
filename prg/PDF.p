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


DEFINE VARIABLE db_id AS DECIMAL     NO-UNDO.
DEFINE VARIABLE rTelleHode AS ROWID      NO-UNDO.
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
    MESSAGE cParam
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN.
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

/*         FIND TransType WHERE TransType.TTId = TelleHode.TTId NO-LOCK NO-ERROR. */
/*         IF AVAIL TransType THEN                                                */
        ASSIGN cTitle = "WEBRESERVASJON".
        FIND Butiker WHERE Butiker.Butik = iCL NO-LOCK NO-ERROR.
        ASSIGN iCLProfilNr = Butiker.ProfilNr.
/*         FIND Butiker WHERE Butiker.Butik = INT(TelleHode.ButikkListe) NO-LOCK NO-ERROR. */
/*         ASSIGN cButNamn = IF AVAIL Butiker THEN Butiker.ButNamn ELSE "Ukjent".          */
        RUN ByggTT.
        IF CAN-FIND(FIRST tt_reserver) THEN
            RUN skrivPDF.
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

&IF DEFINED(EXCLUDE-ByggTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT Procedure 
PROCEDURE ByggTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iNummer AS INTEGER     NO-UNDO.
/* DEFINE TEMP-TABLE TT_reserver NO-UNDO                 */
/*     FIELD butikknr AS INTE                            */
/*     FIELD nummer AS INTE                              */
/*     INDEX butikknr IS PRIMARY butikknr.               */
/* DEFINE TEMP-TABLE TT_bonglinje NO-UNDO LIKE bonglinje */
/*     FIELD nummer AS INTE                              */
/*     INDEX nummer IS PRIMARY nummer.                   */
/*     /* bonglinjer med överföring. Identifiera  */     */
   
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
               tt_bonglinje.farve         = IF TRIM(tt_bonglinje.farve) = "" THEN artbas.levfargkod ELSE tt_bonglinje.farve
               tt_bonglinje.farve         = SUBSTR(tt_bonglinje.farve,1,15).
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

&IF DEFINED(EXCLUDE-Reservasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reservasjon Procedure 
PROCEDURE Reservasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iNummer AS INTEGER     NO-UNDO.
/* 
DEFINE TEMP-TABLE TT_reserver NO-UNDO
    FIELD butikknr AS INTE
    FIELD navn     AS CHAR
    FIELD nummer   AS INTE
    FIELD email    AS CHAR
    INDEX butikknr IS PRIMARY butikknr.
DEFINE TEMP-TABLE TT_bonglinje NO-UNDO LIKE bonglinje
    FIELD nummer AS INTE
    FIELD bestillingsnr AS CHAR
    FIELD farve         AS CHAR
    INDEX nummer IS PRIMARY nummer.
 
 */

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

/*     cMailTo = IF tt_reserver.email <> "" AND NUM-ENTRIES(tt_reserver.email,"@") = 2 THEN tt_reserver.email ELSE cWebButEmail. */
    cMailTo = "ken1@polygonsoftware.no".

        RUN smtpmailv5_7a.p (
    /*mailhub    */   "mail.polygonsoftware.no",
    /*EmailTo    */   cMailTo,
    /*EmailFrom  */   "cWebButEmail",
    /*EmailCC    */   "",
    /*Attachments*/   IF cFilNavn = "" THEN "" ELSE ENTRY(NUM-ENTRIES(cFilNavn,"\"),cFilNavn,"\"),
    /*LocalFiles */   IF cFilNavn = "" THEN "" ELSE cFilNavn,
    /*Subject    */   "Item - Hg analys " + STRING(TODAY) + (IF cFilNavn = "" THEN "- Alla OK" ELSE ""),
    /*Body       */   "",
    /*MIMEHeader */   "",
    /*BodyType   */   "",
    /*Importance */   0,
    /*L_DoAUTH   */   TRUE,
    /*C_AuthType */   "",
    /*C_User     */   "ken1@polygonsoftware.no",
    /*C_Password */   "540212",
    /*oSuccessful*/  OUTPUT lMailOK,
    /*vMessage   */  OUTPUT cMessage) NO-ERROR.
/*     IF cFilNavn <> "" THEN         */
/*         OS-DELETE VALUE(cFilNavn). */
/*                                    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-skrivPDF) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivPDF Procedure 
PROCEDURE skrivPDF :
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
/*  RUN SendEmail IN THIS-PROCEDURE. */
/*  RUN browse2pdf\viewxmldialog.w (cFilNavn,"WEBORDRE"). */
  /* Sender filen til visning og utskrift. */
/*   RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). */
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellelisteXPrintORG) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellelisteXPrintORG Procedure 
PROCEDURE TellelisteXPrintORG :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcRappFil    AS CHAR    NO-UNDO.
  DEF VAR iRapportValg AS INTEGER NO-UNDO.
  /* Hær sætter vi styrinfo før varje rad och æven rubrikstrængen */
/*   MESSAGE "Telleliste uten '0' lager og '0' talt?" SKIP               */
/*           "(Nej = Alle)"                                              */
/*       VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lSvar AS logi. */
  
  RUN gvelgutskrtell.w (OUTPUT iRapportValg).
  if iRapportValg = 0 then
    return no-apply.
  
  RUN InitPrintString.
IF VALID-HANDLE(wLibHandle) THEN
     RUN GetTempFileName IN wLibHandle ("STelleListe", "xpr", OUTPUT pcRappFil). 
  /* Åpner stream til skriverfil. */
  OUTPUT TO VALUE(pcRappFil) PAGED PAGE-SIZE VALUE(65).
  PUT CONTROL '<PDF-OUTPUT=' + REPLACE(pcRappFil,"xpr","pdf") + '>'.
  PUT CONTROL '<PREVIEW=ZoomToWidth><OLANDSCAPE>'.
  VIEW FRAME PageHeader.
  PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
  PUT UNFORMATTED cColLabelString SKIP.
  FOR EACH TelleLinje OF TelleHode  NO-LOCK BREAK BY TelleLinje.ArtikkelNr:
      /* iRapportValg: 10=alla,11=inga 0-0,20=bara negativ diff ,30=bara positiv diff */
      IF iRapportValg = 11 AND TelleLinje.AntallPar = 0 AND TelleLinje.AntallTalt = 0 THEN
          NEXT.
      ELSE IF iRapportValg = 20 AND TelleLinje.AntallDiff >= 0 THEN
          NEXT.
      ELSE IF iRapportValg = 30 AND TelleLinje.AntallDiff <= 0 THEN
          NEXT.
/*       IF lSvar = TRUE AND TelleLinje.AntallPar = 0 AND TelleLinje.AntallTalt = 0 THEN */
/*           NEXT.                                                                       */
      ASSIGN iAntDiff    = iAntDiff    + TelleLinje.AntallDiff
             dVerdiDiff  = dVerdiDiff  + TelleLinje.VerdiDiff
             iAntallPar  = iAntallPar  + TelleLinje.AntallPar 
             iAntallTalt = iAntallTalt + TelleLinje.AntallTalt
             dNedskrevet = dNedskrevet + TelleLinje.Nedskrevet
             dOpprVerdi  = dOpprVerdi  + TelleLinje.OpprVerdi 
             dOpptVerdi  = dOpptVerdi  + TelleLinje.OpptVerdi.
      PUT UNFORMATTED getDataLinje() SKIP.
      IF LINE-COUNTER = 40 AND NOT LAST(TelleLinje.ArtikkelNr) THEN DO:
          RUN PFooter.
          PAGE.
          VIEW FRAME PageHeader.
          PUT UNFORMATTED  "<R4><B><C1><CENTER=C110><P24>" cTitle "</B><P10><R+2>".
          PUT UNFORMATTED cColLabelString SKIP.
      END.
  END.
  PUT UNFORMATTED SumRad() SKIP.
  RUN PFooter.
  OUTPUT CLOSE.
  OUTPUT TO TERMINAL.

  /* Klargjør rapportfilnavnet */
  ASSIGN FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
  RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 
   
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

