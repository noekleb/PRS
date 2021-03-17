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

DEFINE INPUT  PARAMETER iKampanjeId AS INT NO-UNDO.
DEFINE INPUT  PARAMETER iType       AS INTEGER    NO-UNDO. /* 1 = enkel prislista, 2 = full kampanjelista */
DEF VAR cFirma AS CHAR NO-UNDO.
DEFINE VARIABLE cTitel AS CHARACTER   NO-UNDO.
  DEF VAR pcDato       AS CHAR   NO-UNDO.
  DEF VAR pcTid        AS CHAR   NO-UNDO.
  DEF VAR pcBrukerId   AS CHAR   NO-UNDO.
  DEF VAR pcLabel      AS CHAR   NO-UNDO.
  DEFINE VARIABLE iKampCol AS INTEGER EXTENT 11    NO-UNDO.
  DEFINE VARIABLE iPrisCol AS INTEGER EXTENT  6    NO-UNDO.
  DEFINE VARIABLE cLng AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cLabel AS CHARACTER   NO-UNDO.

{pdf_inc.i "THIS-PROCEDURE"}.
{proclib.i}
{xPrint.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByttElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ByttElement Procedure 
FUNCTION ByttElement RETURNS CHARACTER
  ( input ipSkjerm as char,
    input ipElement as int,
    input ipNyttElement as char,
    input ipDelimiter as char)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getOKtext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getOKtext Procedure 
FUNCTION getOKtext RETURNS CHARACTER
  ( INPUT cText AS CHAR, INPUT iAvailBredd AS INTE )  FORWARD.

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

{syspara.i 1 1 101 cFirma}
cFirma = FILL(" ",50 - LENGTH(cFirma)) + cFirma.
FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK.
cLng = bruker.lng.
IF iType = 1 THEN DO:
    cTitel = STRING(clNg = "SE","P R I S L I S T A/P R I S L I S T E").
    cLabel = STRING(cLng = "SE","Vg,Lpnr,Lev.art.nr,Varutekst,Lev.färg,Pris/Vg,Lpnr,Lev.art.nr,Varetekst,Lev.farge,Pris").
    ASSIGN iPrisCol[1] =  60
           iPrisCol[2] =  95
           iPrisCol[3] = 100
           iPrisCol[4] = 205
           iPrisCol[5] = 370
           iPrisCol[6] = 550.
    RUN PDFPrintPrisliste (INPUT iKampanjeId).
END.
ELSE IF iType = 2 THEN DO:
    cTitel = STRING(clNg = "SE","K A M P A N J/K A M P A N J E").
    cLabel = STRING(cLng = "SE","Vg,Lpnr,Lev.art.nr,Varutext,Levfärg,B,Varukost,Pris,TbKr,Tb%,Fel/Vg,Lpnr,Lev.art.nr,Varetekst,Levfarge,B,Varekost,Pris,DbKr,Db%,Feil").
      ASSIGN iKampCol[1]  = 40
             iKampCol[2]  = 65
             iKampCol[3]  = 85
             iKampCol[4]  = 160
             iKampCol[5]  = 300
             iKampCol[6]  = 400
             iKampCol[7]  = 475
             iKampCol[8]  = 525
             iKampCol[9]  = 575
             iKampCol[10] = 630
             iKampCol[11] = 700.
    RUN PDFPrintKampanje (INPUT iKampanjeId).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-PDFPageFooter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageFooter Procedure 
PROCEDURE PDFPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cSidTxt AS CHARACTER   NO-UNDO.

/*   DEFINE FRAME PageBottom                                                                                */
/*       HEADER                                                                                             */
/*       "<C4><FROM><C80><LINE>" SKIP                                                                       */
/*       "<C4><FTimes New Roman><P10>" pcBrukerId FORMAT "x(12)" pcDato pcTid "<C47>" cFirma FORMAT "x(50)" */
/*       WITH PAGE-BOTTOM STREAM-IO WIDTH 255.                                                              */



  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
/*  RUN pdf_set_dash IN h_PDFinc ("Spdf",1,0).*/
  RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"), pdf_BottomMargin ("Spdf"), pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , pdf_BottomMargin ("Spdf"), 0.5).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).  
  RUN pdf_text_xy_dec ("Spdf",STRING(TODAY),pdf_LeftMargin ("Spdf"),pdf_BottomMargin ("Spdf") - 14).
  RUN pdf_text_xy_dec ("Spdf",cFirma,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 350,pdf_BottomMargin ("Spdf") - 14).
  cSidTxt = TRIM("Sida: " + STRING(pdf_page("Spdf")) + " (" + pdf_TotalPages("Spdf") + ")").

  RUN pdf_text_xy_dec ("Spdf",cSidTxt,pdf_Pagewidth ("Spdf") - pdf_LeftMargin ("Spdf") - 50,pdf_BottomMargin ("Spdf") - 14).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPageHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPageHeader Procedure 
PROCEDURE PDFPageHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE VARIABLE cTxt1 AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cTxt2 AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
   DEFINE VARIABLE pcStatus AS CHARACTER   NO-UNDO.
   DEFINE VARIABLE cKampTyp AS CHARACTER   NO-UNDO.
  IF KampanjeHode.Komplett THEN
      pcStatus = STRING(clNg = "SE","Aktiverad/Aktivert").
  ELSE IF KampanjeHode.Aktivert THEN
    pcStatus = STRING(clNg = "SE","Delvis aktiverad/Delhvis aktivert").
  ELSE
    pcStatus = STRING(clNg = "SE","Inte aktiverad/Ikke aktivert").
   IF KampanjeHode.NormalPris THEN
       cKampTyp = STRING(clNg = "SE","Prisändring aktiveras /Prisendring aktiveres ").
   ELSE
       cKampTyp = STRING(clNg = "SE","Kampanjperiod         /Kampanjeperiode       ").
/*    cTxt1 = STRING(KampanjeHode.NormalPris,"Prisendring aktiveres /Kampanjeperiode       ") + */
   cTxt1 = cKampTyp +
          STRING(KampanjeHode.StartDato) + " " + STRING(KampanjeHode.AktiveresTid,"HH:MM") +
          (IF KampanjeHode.NormalPris
          THEN ""
          ELSE " - " + string(KampanjeHode.SluttDato) + " " + string(KampanjeHode.GyldigTilTid,"HH:MM")) + "          " +
        (IF KampanjeHode.NormalPris
            THEN ""
            ELSE STRING(pcStatus)).
   
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",18).
   RUN pdf_text_xy_dec ("Spdf",cTitel, pdf_PageWidth("Spdf") / 2 - (bredd(cTitel) / 2),pdf_PageHeight("Spdf") - 30).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
   RUN pdf_text_xy_dec ("Spdf",string(KampanjeHode.KampanjeId) + " " + KampanjeHode.Beskrivelse,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 45).
   RUN pdf_text_xy_dec ("Spdf",cTxt1,pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 60).
   RUN pdf_line IN h_PDFinc  ("Spdf", pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - 65, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") ,pdf_PageHeight("Spdf") - 65, 0.5).
   RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",9).

   IF iType = 1 THEN DO:
       RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cLabel),iPrisCol[1] - bredd(ENTRY(1,cLabel)),pdf_PageHeight("Spdf") - 80).
       RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cLabel),iPrisCol[2] - bredd(ENTRY(2,cLabel)),pdf_PageHeight("Spdf") - 80).
        DO ii = 3 TO NUM-ENTRIES(cLabel) - 1:
            RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,cLabel),iPrisCol[ii],pdf_PageHeight("Spdf") - 80).
        END.
        RUN pdf_text_xy_dec ("Spdf",ENTRY(NUM-ENTRIES(cLabel),cLabel),iPrisCol[NUM-ENTRIES(cLabel)] - bredd(ENTRY(NUM-ENTRIES(cLabel),cLabel)),pdf_PageHeight("Spdf") - 80).

   END.
   ELSE DO:
       RUN pdf_text_xy_dec ("Spdf",ENTRY(1,cLabel),iKampCol[1] - bredd(ENTRY(1,cLabel)),pdf_PageHeight("Spdf") - 80).
       RUN pdf_text_xy_dec ("Spdf",ENTRY(2,cLabel),iKampCol[2] - bredd(ENTRY(2,cLabel)),pdf_PageHeight("Spdf") - 80).
        DO ii = 3 TO 6:
            RUN pdf_text_xy_dec ("Spdf",ENTRY(ii,cLabel),iKampCol[ii],pdf_PageHeight("Spdf") - 80).
        END.
        RUN pdf_text_xy_dec ("Spdf",ENTRY(7,cLabel),iKampCol[7] - bredd(ENTRY(7,cLabel)),pdf_PageHeight("Spdf") - 80).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(8,cLabel),iKampCol[8] - bredd(ENTRY(8,cLabel)),pdf_PageHeight("Spdf") - 80).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(9,cLabel),iKampCol[9] - bredd(ENTRY(9,cLabel)),pdf_PageHeight("Spdf") - 80).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(10,cLabel),iKampCol[10] - bredd(ENTRY(10,cLabel)),pdf_PageHeight("Spdf") - 80).
        RUN pdf_text_xy_dec ("Spdf",ENTRY(11,cLabel),iKampCol[11],pdf_PageHeight("Spdf") - 80).
/*         DO ii = 30 TO 780:                                                           */
/*             IF ii MOD 10 = 0 THEN DO:                                                */
/*                 IF ii MOD 100 = 0 THEN                                               */
/*                     RUN pdf_text_xy_dec ("Spdf","|",ii,pdf_PageHeight("Spdf") - 85). */
/*                 ELSE                                                                 */
/*                     RUN pdf_text_xy_dec ("Spdf","^",ii,pdf_PageHeight("Spdf") - 85). */
/*             END.                                                                     */
/*         END.                                                                         */
   END.

  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPrintKampanje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPrintKampanje Procedure 
PROCEDURE PDFPrintKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piKampanjeId AS INT NO-UNDO.
    
  DEF VAR pcBildeFil   AS CHAR   NO-UNDO.
  DEF VAR piCopies     AS INT    NO-UNDO.
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.
  DEF VAR pcRowIdent   AS CHAR   NO-UNDO.
  DEF VAR pcRowValues  AS CHAR   NO-UNDO.
  DEF VAR pcColValues  AS CHAR   NO-UNDO.
  DEF VAR pcSkadeListe AS CHAR   NO-UNDO.
  DEF VAR ph_Dummy     AS HANDLE NO-UNDO.
  DEF VAR pcRegNr      AS CHAR   NO-UNDO.
  DEF VAR piLoop       AS INT    NO-UNDO.
  DEF VAR pcTekst      AS CHAR   NO-UNDO.
  DEF VAR piRad        AS dec    NO-UNDO.
  DEF VAR pcStatus     AS CHAR   NO-UNDO.
  DEF VAR pcDato       AS CHAR   NO-UNDO.
  DEF VAR pcTid        AS CHAR   NO-UNDO.
  DEF VAR pcBrukerId   AS CHAR   NO-UNDO.
  DEF VAR pcSkjerm     AS CHAR   NO-UNDO.
  DEF VAR pcLabel      AS CHAR   NO-UNDO.

  DEF VAR iRad         AS INTE INIT 5 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.
  DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
  DEF VAR iSolgtAnt    AS INTE    NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.
  DEF VAR h_PrisKo     AS HANDLE  NO-UNDO.
  DEFINE VARIABLE dVarekost AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dPris     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dDBKr     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dMVa%     AS DECIMAL     NO-UNDO.

  FIND KampanjeHode NO-LOCK WHERE
      KampanjeHode.KampanjeId = piKampanjeId NO-ERROR.
  IF NOT AVAILABLE KampanjeHode THEN
      RETURN "Finner ikke kampanjehode.".

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  ASSIGN
    piCopies   = 1
    pcDato     = STRING(TODAY)
    pcTid      = STRING(TIME,"HH:MM:SS")
    pcBrukerId = USERID("SkoTex")
    pcLabel    = "Vg,Lpnr,Lev.art.nr,Varetekst,Lev.farge,B,Varekost,Pris,DbKr,Db%,Feil"
    pcLabel    = pcLabel + ",,,,,,,,,,,,,,,,,,,,"
    .
  STATUS DEFAULT "Skriver ut, venligst vent...".


  /* Henter tempfilnavn */
  if valid-handle(h_proclib) then
    run GetTempFileName in h_proclib ("Kampanje", "xpr", output pcRappFil). 
  
  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 20).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
  RUN pdf_set_Orientation ("Spdf", "portrait").
  iY = 100.
  RUN pdf_new_page ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",9).


  IF KampanjeHode.Komplett THEN
      pcStatus = "Aktivert".
  ELSE IF KampanjeHode.Aktivert THEN
      pcStatus = "Delhvis aktivert".
  ELSE
      pcStatus = "Ikke aktivert".


FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK
     BREAK 
     BY KampanjeLinje.ProfilNr
     BY KampanjeLinje.Vg
     BY KampanjeLinje.LopNr:

     IF FIRST-OF(KampanjeLinje.ProfilNr) THEN
     DO:
         FIND PrisProfil NO-LOCK WHERE
             PrisProfil.ProfilNr = KampanjeLinje.ProfilNr NO-ERROR.
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
           RUN pdf_text_xy_dec ("Spdf","Prisprofil: " + STRING(KampanjeLinje.ProfilNr) + " " + (IF AVAILABLE PrisProfil THEN PrisProfil.KortNavn ELSE ""),pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
/*            RUN pdf_text_xy_dec ("Spdf",STRING(pdf_PageHeight ("Spdf") - iY),pdf_LeftMargin ("Spdf") - 15,pdf_PageHeight("Spdf") - iY). */
           iY = iY + 20.

     END.
     IF FIRST-OF(KampanjeLinje.Vg) THEN
     DO:
         iY = iY + 5.
         FIND VarGr NO-LOCK WHERE
             VarGr.Vg = KampanjeLinje.Vg NO-ERROR.
         RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
         RUN pdf_text_xy_dec ("Spdf","Varegruppe: " + STRING(KampanjeLinje.Vg) + " " + (IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ""),pdf_LeftMargin ("Spdf"),pdf_PageHeight("Spdf") - iY).
/*      RUN pdf_text_xy_dec ("Spdf",STRING(pdf_PageHeight ("Spdf") - iY),pdf_LeftMargin ("Spdf") - 15,pdf_PageHeight("Spdf") - iY). */
         RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
         iY = iY + 20.
     END.

     ASSIGN 
       cRader   = cRader + "," + STRING(iRad + 1)
       pcSkjerm = ""
       .

     FIND ArtBas NO-LOCK where
         ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
     FIND VarGr  OF ArtBas        NO-LOCK NO-ERROR.
     FIND Moms   OF VarGr         NO-LOCK NO-ERROR.
     FIND Valuta OF ArtBas        NO-LOCK NO-ERROR.

     /* Bygge skjerm streng ut fra aktiv kalkyle */
     RUN InitKalkyle IN h_PrisKo
           (recid(ArtBas),
            KampanjeLinje.ProfilNr, 
            INPUT-OUTPUT pcSkjerm,     
            Moms.MomsProc,   
            Valuta.ValKurs,    
            18,     
            (IF AVAILABLE ArtPris
               THEN ArtPris.Tilbud
               ELSE FALSE)).     

     /* Oppdaterer strengen med den nye prisen. */
     pcSkjerm = ByttElement(input pcSkjerm,
                     input 18,
                     input (IF KampanjeHode.NormalPris
                             THEN string(KampanjeLinje.Pris[1])
                             ELSE STRING(KampanjeLinje.Pris[2])) ,
                     input ";").       

     IF KampanjeHode.NormalPris = FALSE THEN
     TILB-OPPDAT-STRENG:
     DO:
       /* Tilbud fra */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 23,
                             input string(KampanjeHode.StartDato),
                             input ";").
       /* Tilbud fra tid */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 24,
                             input string(KampanjeHode.AktiveresTid),
                             input ";").
       /* Tilbud til */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 25,
                             input string(KampanjeHode.SluttDato),
                             input ";").
       /* Tilbud til tid */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 26,
                             input string(KampanjeHode.GyldigTilTid),
                             input ";").

     END. /* TILB-OPPDAT-STRENG */
     ELSE 
     NORM-OPPDAT-STRENG:
     DO:
       /* Fra */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 23,
                             input string(KampanjeHode.StartDato),
                             input ";").
       /* Til tid */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 24,
                             input STRING(KampanjeHode.AktiveresTid),
                             input ";").
     END. /* NORM-OPPDAT-STRENG */

     ASSIGN dVarekost = DECI(ENTRY(13,pcSkjerm,";"))
            dPris     = IF Kampanjehode.normalpris THEN KampanjeLinje.Pris[1] ELSE KampanjeLinje.Pris[2]
            dMVa%     = DECI(ENTRY(15,pcSkjerm,";")).
    dDBKr = dPris - ROUND(dPris * dMVa% / (100 + dMva%),2) - dVarekost.


/* MESSAGE  dVarekost dPris dMVa% dDBKr SKIP                                                            */
/*          ENTRY(13,pcSkjerm,";") ENTRY(15,pcSkjerm,";") ENTRY(16,pcSkjerm,";") ENTRY(17,pcSkjerm,";") */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                               */
/*      RUN pdf_text_xy_dec ("Spdf",STRING(pdf_PageHeight ("Spdf") - iY),pdf_LeftMargin ("Spdf") - 15,pdf_PageHeight("Spdf") - iY). */

     RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.Vg),iKampCol[1] - bredd(STRING(KampanjeLinje.Vg)),pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.LopNr),iKampCol[2] - bredd(STRING(KampanjeLinje.LopNr)),pdf_PageHeight("Spdf") - iY).
/*      RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN Artbas.LevKod ELSE ""),iKampCol[3],pdf_PageHeight("Spdf") - iY). */
     RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN getOKtext(Artbas.LevKod,iKampCol[4] - iKampCol[3] - 2) ELSE ""),iKampCol[3],pdf_PageHeight("Spdf") - iY).
/*      RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN SUBSTR(Artbas.Beskr,1,25) ELSE ""),iKampCol[4],pdf_PageHeight("Spdf") - iY). */
     RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN getOKtext(Artbas.Beskr,iKampCol[5] - iKampCol[4] - 2) ELSE ""),iKampCol[4],pdf_PageHeight("Spdf") - iY).
/*      RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN Artbas.LevFargKod ELSE ""),iKampCol[5],pdf_PageHeight("Spdf") - iY). */
     RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN getOKtext(Artbas.Levfargkod,iKampCol[6] - iKampCol[5] - 60) ELSE ""),iKampCol[5],pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.Behandlet,"*/"),iKampCol[6],pdf_PageHeight("Spdf") - iY).
     


/*      RUN pdf_text_xy_dec ("Spdf",STRING(DEC(ENTRY(13,pcSkjerm,";")),">>,>>9.99"),iKampCol[7] - bredd(STRING(DEC(ENTRY(13,pcSkjerm,";")),">>,>>9.99")),pdf_PageHeight("Spdf") - iY). */
/*      RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.Pris[2],">>,>>9.99"),iKampCol[8] - bredd(STRING(KampanjeLinje.Pris[2],">>,>>9.99")),pdf_PageHeight("Spdf") - iY). */
/*      RUN pdf_text_xy_dec ("Spdf",STRING(DEC(ENTRY(16,pcSkjerm,";")),">>,>>9.99"),iKampCol[9] - bredd(STRING(DEC(ENTRY(16,pcSkjerm,";")),">>,>>9.99")),pdf_PageHeight("Spdf") - iY). */
     RUN pdf_text_xy_dec ("Spdf",STRING(dVarekost,">>,>>9.99"),iKampCol[7] - bredd(STRING(dVarekost,">>,>>9.99")),pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",STRING(dPris,">>,>>9.99"),iKampCol[8] - bredd(STRING(dPris,">>,>>9.99")),pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",STRING(dDBKr,"->>,>>9.99"),iKampCol[9] - bredd(STRING(dDBKr,"->>,>>9.99")),pdf_PageHeight("Spdf") - iY).
/*                                                                                                                                                                                      */
/*      RUN pdf_text_xy_dec ("Spdf",STRING(DEC(ENTRY(17,pcSkjerm,";")),">>,>>9.99"),iKampCol[10] - bredd(STRING(DEC(ENTRY(17,pcSkjerm,";")),">>,>>9.99")),pdf_PageHeight("Spdf") - iY). */
/*      RUN pdf_text_xy_dec ("Spdf",ENTRY(1,KampanjeLinje.FeilKode,CHR(1)),iKampCol[11],pdf_PageHeight("Spdf") - iY).                                                                   */

     iY = iY + 15.
     IF iY > 720 THEN DO:
         iY = 100.
         RUN pdf_new_page ("Spdf").
         RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
     END.
     ASSIGN 
       iRad = iRad + 1
       cRader = cRader + "," + STRING(iRad + 1)
       .
  END.

  RUN pdf_close ("Spdf").

    RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PDFPrintPrisliste) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFPrintPrisliste Procedure 
PROCEDURE PDFPrintPrisliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piKampanjeId AS INT NO-UNDO.
    
  DEF VAR pcBildeFil   AS CHAR   NO-UNDO.
  DEF VAR piCopies     AS INT    NO-UNDO.
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.
  DEF VAR pcRowIdent   AS CHAR   NO-UNDO.
  DEF VAR pcRowValues  AS CHAR   NO-UNDO.
  DEF VAR pcColValues  AS CHAR   NO-UNDO.
  DEF VAR pcSkadeListe AS CHAR   NO-UNDO.
  DEF VAR ph_Dummy     AS HANDLE NO-UNDO.
  DEF VAR pcRegNr      AS CHAR   NO-UNDO.
  DEF VAR piLoop       AS INT    NO-UNDO.
  DEF VAR pcTekst      AS CHAR   NO-UNDO.
  DEF VAR piRad        AS dec    NO-UNDO.
  DEF VAR pcStatus     AS CHAR   NO-UNDO.

  DEF VAR iRad         AS INTE INIT 5 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.

  DEF VAR iSolgtAnt    AS INTE    NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.
  DEF VAR h_PrisKo     AS HANDLE  NO-UNDO.
  DEF VAR cTekst1      AS CHAR    FORMAT "x(6)" NO-UNDO.
  DEFINE VARIABLE iY   AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cListTitel AS CHARACTER FORMAT "x(35)" NO-UNDO.
  DEFINE VARIABLE cPrintText AS CHARACTER   NO-UNDO.
  FIND KampanjeHode NO-LOCK WHERE
      KampanjeHode.KampanjeId = piKampanjeId NO-ERROR.
  IF NOT AVAILABLE KampanjeHode THEN
      RETURN "Finner ikke kampanjehode.".
  
ASSIGN
    piCopies   = 1
    pcDato     = STRING(TODAY)
    pcTid      = STRING(TIME,"HH:MM:SS")
    pcBrukerId = USERID("SkoTex")
    pcLabel    = "  Vg,LpNr,Pris,Lev.art.nr,Varetekst,Lev.farge" 
    .
  /* Henter tempfilnavn */
  if valid-handle(h_proclib) then
    run GetTempFileName in h_proclib ("KampPris", "pdf", output pcRappFil). 
  
  RUN pdf_new ("Spdf",pcRappFil).
  pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageHeader").
  pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PDFPageFooter").
  RUN pdf_set_PaperType ("Spdf","A4").
  RUN pdf_set_LeftMargin ("Spdf", 30).
  RUN pdf_set_BottomMargin ("Spdf", 40).
  RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf", 13).
  iY = 94.
  RUN pdf_new_page ("Spdf").
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",9).
  FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK
     BY KampanjeLinje.Vg
     BY KampanjeLinje.LopNr:

     IF pdf_PageHeight("Spdf") - iY < pdf_BottomMargin("Spdf") THEN DO:
         iY = 94.
           RUN pdf_new_page ("Spdf").
           RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",9).
     END.
     FIND ArtBas NO-LOCK where
         ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.

     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",9).
     RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.Vg),iPrisCol[1] - bredd(STRING(KampanjeLinje.Vg)),pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.LopNr),iPrisCol[2] - bredd(STRING(KampanjeLinje.LopNr)),pdf_PageHeight("Spdf") - iY).
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",9).
     RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN getOKtext(Artbas.LevKod,iPrisCol[4] - iPrisCol[3] - 2) ELSE ""),iPrisCol[3],pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN getOKtext(Artbas.Beskr,iPrisCol[5] - iPrisCol[4] - 2) ELSE ""),iPrisCol[4],pdf_PageHeight("Spdf") - iY).
     RUN pdf_text_xy_dec ("Spdf",(IF AVAIL Artbas THEN getOKtext(Artbas.Levfargkod,iPrisCol[6] - iPrisCol[5] - 60) ELSE ""),iPrisCol[5],pdf_PageHeight("Spdf") - iY).
     RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",9).
     RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.Pris[2],">>,>>9.99"),iPrisCol[6] - bredd(STRING(KampanjeLinje.Pris[2],">>,>>9.99")),pdf_PageHeight("Spdf") - iY).
     iY = iY + 15.
/*      RUN pdf_text_xy_dec ("Spdf",STRING(333330),iPrisCol[1] - bredd(STRING(333330)),pdf_PageHeight("Spdf") - iY). */
/*      RUN pdf_text_xy_dec ("Spdf",STRING(66667),iPrisCol[2] - bredd(STRING(66667)),pdf_PageHeight("Spdf") - iY). */
/*      RUN pdf_text_xy_dec ("Spdf","ANANAS DULCE GOLD",iPrisCol[3],pdf_PageHeight("Spdf") - iY).         */
/*      RUN pdf_text_xy_dec ("Spdf","BBBBBBBBBBBBBBBBBBBBBBBBC",iPrisCol[4],pdf_PageHeight("Spdf") - iY). */
/*      RUN pdf_text_xy_dec ("Spdf","GRØNN IND HVIT REM",iPrisCol[5],pdf_PageHeight("Spdf") - iY).        */
/*      RUN pdf_text_xy_dec ("Spdf",STRING(KampanjeLinje.Pris[2],">>,>>9.99"),iPrisCol[6] - bredd(STRING(KampanjeLinje.Pris[2],">>,>>9.99")),pdf_PageHeight("Spdf") - iY). */
  END.

RUN pdf_close ("Spdf").

  RUN browse2pdf\viewxmldialog.w (pcRappFil,"Polygon Retail Solutions").


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintKampanje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintKampanje Procedure 
PROCEDURE PrintKampanje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piKampanjeId AS INT NO-UNDO.
    
  DEF VAR pcBildeFil   AS CHAR   NO-UNDO.
  DEF VAR piCopies     AS INT    NO-UNDO.
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.
  DEF VAR pcRowIdent   AS CHAR   NO-UNDO.
  DEF VAR pcRowValues  AS CHAR   NO-UNDO.
  DEF VAR pcColValues  AS CHAR   NO-UNDO.
  DEF VAR pcSkadeListe AS CHAR   NO-UNDO.
  DEF VAR ph_Dummy     AS HANDLE NO-UNDO.
  DEF VAR pcRegNr      AS CHAR   NO-UNDO.
  DEF VAR piLoop       AS INT    NO-UNDO.
  DEF VAR pcTekst      AS CHAR   NO-UNDO.
  DEF VAR piRad        AS dec    NO-UNDO.
  DEF VAR pcStatus     AS CHAR   NO-UNDO.
  DEF VAR pcDato       AS CHAR   NO-UNDO.
  DEF VAR pcTid        AS CHAR   NO-UNDO.
  DEF VAR pcBrukerId   AS CHAR   NO-UNDO.
  DEF VAR pcSkjerm     AS CHAR   NO-UNDO.
  DEF VAR pcLabel      AS CHAR   NO-UNDO.

  DEF VAR iRad         AS INTE INIT 5 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.

  DEF VAR iSolgtAnt    AS INTE    NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.
  DEF VAR h_PrisKo     AS HANDLE  NO-UNDO.

  FIND KampanjeHode NO-LOCK WHERE
      KampanjeHode.KampanjeId = piKampanjeId NO-ERROR.
  IF NOT AVAILABLE KampanjeHode THEN
      RETURN "Finner ikke kampanjehode.".

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN prisko.p PERSISTENT SET h_PrisKo.

  ASSIGN
    piCopies   = 1
    pcDato     = STRING(TODAY)
    pcTid      = STRING(TIME,"HH:MM:SS")
    pcBrukerId = USERID("SkoTex")
    pcLabel    = " Vg,Lpnr,Lev.art.nr,Varetekst,Lev.farge,B,Varekost,Pris,DbKr,Db%,Feil" 
    pcLabel    = pcLabel + ",,,,,,,,,,,,,,,,,,,,"
    .
  STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
  if valid-handle(h_proclib) then
    run GetTempFileName in h_proclib ("Kampanje", "xpr", output pcRappFil). 
  
  /* Åpner stream til skriverfil. */
  output TO value(pcRappFil) PAGED page-size 67.

  put control '<PREVIEW=ZoomToWidth>'.
  /*
  put control "<PrinterSetup>". /* xPrint will display the Printer Setup Box */
  */

  IF KampanjeHode.Komplett THEN
      pcStatus = "Aktivert".
  ELSE IF KampanjeHode.Aktivert THEN
      pcStatus = "Delhvis aktivert".
  ELSE
      pcStatus = "Ikke aktivert".

  Define Frame PageTopp
      header
        "<C4><P18><FTimes New Roman><B>K A M P A N J E" 
        "</B><P12>"
        "<C75><P10><FArial><B>" Page-Number format ">>"  "/ <#Pages></B>" SKIP(1)

        "<C4><FTimes New Roman><P12><B>" string(KampanjeHode.KampanjeId) KampanjeHode.Beskrivelse "</B>" SKIP
        "<C4><P10><B>" 
        (IF KampanjeHode.NormalPris
           THEN "Prisendring aktiveres"
           ELSE "Kampanjeperiode") FORMAT "x(21)"
        string(KampanjeHode.StartDato) + " " + STRING(KampanjeHode.AktiveresTid,"HH:MM") FORMAT "x(15)"
        
        (IF KampanjeHode.NormalPris
          THEN ""
          ELSE "- " + string(KampanjeHode.SluttDato) + " " + string(KampanjeHode.GyldigTilTid,"HH:MM")) FORMAT "x(20)"
        (IF KampanjeHode.NormalPris
            THEN ""
            ELSE STRING(pcStatus)) FORMAT "x(20)"
        SKIP

        "<C3><B><FROM><C80><LINE>" SKIP
         
        "<P8><FAreal New>"   
        "<C3>"  entry(1,pcLabel) FORMAT "x(3)" 
        "<C9>"  entry(2,pcLabel) FORMAT "x(4)"
        "<C12>" "Lev.art.nr"
        "<C20>" entry(3,pcLabel) FORMAT "x(17)" 
        "<C41>" "Lev.farge"
        "<C51>" entry(4,pcLabel) FORMAT "x(1)"
        "<C51>" entry(5,pcLabel) FORMAT "x(12)"
        "<C58>" entry(6,pcLabel) FORMAT "x(11)"
        "<C63>" entry(7,pcLabel) FORMAT "x(10)"
        "<C69>" entry(8,pcLabel) FORMAT "x(8)" 
        "<C75>" entry(9,pcLabel) FORMAT "x(4)" 
        SKIP
        
        "<C3><FROM><C6><LINE>" 
        "<C9><FROM><C11><LINE>"
        "<C12><FROM><C19><LINE>"      
        "<C20><FROM><C39><LINE>"
        "<C41><FROM><C49><LINE>"
        "<C51><FROM><C52><LINE>"
        "<C53><FROM><C57><LINE>"
        "<C59><FROM><C63><LINE>"
        "<C65><FROM><C69><LINE>" 
        "<C71><FROM><C74><LINE>"
        "<C75><FROM><C78><LINE>"
        "</B>" 
        SKIP
      with page-Top stream-io width 255.



  DEFINE FRAME PageBottom
      HEADER
      "<C4><FROM><C80><LINE>" SKIP
      "<C4><FTimes New Roman><P10>" pcBrukerId FORMAT "x(12)" pcDato pcTid "<C47>" cFirma FORMAT "x(50)"
      WITH PAGE-BOTTOM STREAM-IO WIDTH 255.

  view frame PageTopp.
  VIEW FRAME PageBottom.

  FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK
     BREAK 
     BY KampanjeLinje.ProfilNr
     BY KampanjeLinje.Vg
     BY KampanjeLinje.LopNr:

     IF FIRST-OF(KampanjeLinje.ProfilNr) THEN
     DO:
         FIND PrisProfil NO-LOCK WHERE
             PrisProfil.ProfilNr = KampanjeLinje.ProfilNr NO-ERROR.
         PUT UNFORMATTED
           SKIP(1)
           "<C4><FTimes New Roman><B>Prisprofil: " KampanjeLinje.ProfilNr " " (IF AVAILABLE PrisProfil
                                                           THEN PrisProfil.KortNavn
                                                           ELSE "").
     END.
     IF FIRST-OF(KampanjeLinje.Vg) THEN
     DO:
         FIND VarGr NO-LOCK WHERE
             VarGr.Vg = KampanjeLinje.Vg NO-ERROR.
         PUT UNFORMATTED
           SKIP(1)
           "<C4><FTimes New Roman><B>Varegruppe: " KampanjeLinje.Vg " " (IF AVAILABLE VarGr
                                                      THEN VarGr.VgBeskr
                                                      ELSE "") SKIP(1).
     END.

     ASSIGN 
       cRader   = cRader + "," + STRING(iRad + 1)
       pcSkjerm = ""
       .

     FIND ArtBas NO-LOCK where
         ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
     FIND VarGr  OF ArtBas        NO-LOCK NO-ERROR.
     FIND Moms   OF VarGr         NO-LOCK NO-ERROR.
     FIND Valuta OF ArtBas        NO-LOCK NO-ERROR.

     /* Bygge skjerm streng ut fra aktiv kalkyle */
     RUN InitKalkyle IN h_PrisKo
           (recid(ArtBas),
            KampanjeLinje.ProfilNr, 
            INPUT-OUTPUT pcSkjerm,     
            Moms.MomsProc,   
            Valuta.ValKurs,    
            18,     
            (IF AVAILABLE ArtPris
               THEN ArtPris.Tilbud
               ELSE FALSE)).     

     /* Oppdaterer strengen med den nye prisen. */
     pcSkjerm = ByttElement(input pcSkjerm,
                     input 18,
                     input (IF KampanjeHode.NormalPris
                             THEN string(KampanjeLinje.Pris[1])
                             ELSE STRING(KampanjeLinje.Pris[2])) ,
                     input ";").       

     IF KampanjeHode.NormalPris = FALSE THEN
     TILB-OPPDAT-STRENG:
     DO:
       /* Tilbud fra */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 23,
                             input string(KampanjeHode.StartDato),
                             input ";").
       /* Tilbud fra tid */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 24,
                             input string(KampanjeHode.AktiveresTid),
                             input ";").
       /* Tilbud til */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 25,
                             input string(KampanjeHode.SluttDato),
                             input ";").
       /* Tilbud til tid */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 26,
                             input string(KampanjeHode.GyldigTilTid),
                             input ";").

     END. /* TILB-OPPDAT-STRENG */
     ELSE 
     NORM-OPPDAT-STRENG:
     DO:
       /* Fra */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 23,
                             input string(KampanjeHode.StartDato),
                             input ";").
       /* Til tid */
       pcSkjerm = ByttElement(input pcSkjerm,
                             input 24,
                             input STRING(KampanjeHode.AktiveresTid),
                             input ";").
     END. /* NORM-OPPDAT-STRENG */
     
     PUT
       /*"<P8><FCourier New></B>"*/   
       "<P8><FAreal></B>"   
       "<C3>"  KampanjeLinje.Vg FORMAT "zzzzz9" 
       "<C9>"  KampanjeLinje.LopNr FORMAT "zzz9"
       "<C12>" ArtBAs.LevKod FORMAT "x(10)"
       "<C20>" ArtBas.Beskr FORMAT "x(20)"   
       "<C41>" ArtBAs.LevFargKod FORMAT "x(10)"
       "<C51>" (IF KampanjeLinje.Behandlet
                           THEN "*"
                           ELSE " ") FORMAT "x"
       "<C53>"  dec(entry(13,pcSkjerm,";")) FORMAT "->>>>9.99"
/*        "<C43>"  KampanjeLinje.Pris[IF KampanjeHode.NormalPris */
/*                        THEN 1 ELSE 2] FORMAT ">,>>>,>>9.99"   */
       "<C59>"  KampanjeLinje.Pris[2] FORMAT ">>>>9.99"
       "<C65>"  dec(entry(16,pcSkjerm,";")) FORMAT "->>>9.99"
       "<C70>"  dec(entry(17,pcSkjerm,";")) FORMAT "->>>9.99"
       "<C75>"  ENTRY(1,KampanjeLinje.FeilKode,CHR(1))
       SKIP
       .
       
     ASSIGN 
       iRad = iRad + 1
       cRader = cRader + "," + STRING(iRad + 1)
       .
  END.

  /* Lukker stream */
  OUTPUT CLOSE.

  /* Klargjør rapportfilnavnet */
  ASSIGN
    FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

  STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PrintPrisliste) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintPrisliste Procedure 
PROCEDURE PrintPrisliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piKampanjeId AS INT NO-UNDO.
    
  DEF VAR pcBildeFil   AS CHAR   NO-UNDO.
  DEF VAR piCopies     AS INT    NO-UNDO.
  DEF VAR pcRappFil    AS CHAR   NO-UNDO.
  DEF VAR pcRowIdent   AS CHAR   NO-UNDO.
  DEF VAR pcRowValues  AS CHAR   NO-UNDO.
  DEF VAR pcColValues  AS CHAR   NO-UNDO.
  DEF VAR pcSkadeListe AS CHAR   NO-UNDO.
  DEF VAR ph_Dummy     AS HANDLE NO-UNDO.
  DEF VAR pcRegNr      AS CHAR   NO-UNDO.
  DEF VAR piLoop       AS INT    NO-UNDO.
  DEF VAR pcTekst      AS CHAR   NO-UNDO.
  DEF VAR piRad        AS dec    NO-UNDO.
  DEF VAR pcStatus     AS CHAR   NO-UNDO.

  DEF VAR iRad         AS INTE INIT 5 NO-UNDO.
  DEF VAR iButik       AS INTE INIT ? NO-UNDO.
  DEF VAR iKasse       AS INTE INIT ? NO-UNDO.
  DEF VAR cRader       AS CHAR        NO-UNDO.
  DEF VAR cCols        AS CHAR        NO-UNDO.

  DEF VAR iSolgtAnt    AS INTE    NO-UNDO.
  DEF VAR dSolgtVerdi  AS DECI    NO-UNDO.
  DEF VAR iAntKunder   AS INTE    NO-UNDO.
  DEF VAR h_PrisKo     AS HANDLE  NO-UNDO.
  DEF VAR cTekst1      AS CHAR    FORMAT "x(6)" NO-UNDO.

  DEFINE VARIABLE cListTitel AS CHARACTER FORMAT "x(35)" NO-UNDO.

  FIND KampanjeHode NO-LOCK WHERE
      KampanjeHode.KampanjeId = piKampanjeId NO-ERROR.
  IF NOT AVAILABLE KampanjeHode THEN
      RETURN "Finner ikke kampanjehode.".
  
  ASSIGN cListTitel = IF iType = 1 
                        THEN "P R I S L I S T E"
                        ELSE "K A M P A N J E".

  ASSIGN
    pcDato     = STRING(TODAY)
    pcTid      = STRING(TIME,"HH:MM:SS")
    pcBrukerId = USERID("SkoTex")
    pcLabel    = "   Vg,LpNr,Pris,Lev.art.nr,Varetekst,Lev.farge" 
    .
  STATUS DEFAULT "Skriver ut, venligst vent...".

  /* Henter tempfilnavn */
  if valid-handle(h_proclib) then
    run GetTempFileName in h_proclib ("KampPris", "xpr", output pcRappFil). 
  
  /* Åpner stream til skriverfil. */
  output TO value(pcRappFil) PAGED page-size 67.

  put control '<PREVIEW=ZoomToWidth>'.
  /*
  put control "<PrinterSetup>". /* xPrint will display the Printer Setup Box */
  */

  Define Frame PageTopp
      header
        "<C4><P18><FTimes New Roman><B>" cListTitel
        "</B><P12>"
        "<C75><P10><FArial><B>" Page-Number format ">>"  "/ <#Pages></B>" SKIP(1)

        "<C4><FTimes New Roman><P12><B>" string(KampanjeHode.KampanjeId) KampanjeHode.Beskrivelse "</B>" SKIP
        "<C4><P10><B>" 
        (IF KampanjeHode.NormalPris
           THEN "Prisendring aktiveres"
           ELSE "Kampanjeperiode") FORMAT "x(21)"
        string(KampanjeHode.StartDato) + " " + STRING(KampanjeHode.AktiveresTid,"HH:MM") FORMAT "x(15)"
        
        (IF KampanjeHode.NormalPris
          THEN ""
          ELSE "- " + string(KampanjeHode.SluttDato) + " " + string(KampanjeHode.GyldigTilTid,"HH:MM")) FORMAT "x(20)"
        (IF KampanjeHode.NormalPris
            THEN ""
            ELSE STRING(pcStatus)) FORMAT "x(20)"
        SKIP

        "<C3></B><FROM><C80><LINE>" SKIP
         
        "<P9><Fcourier New><B>"   
        "<C3>" "  Vg: Lpnr: Lev.art.nr:          Varetekst:                     Lev.farge:                Pris:"
        "</B>"      
        SKIP
        
      with page-Top stream-io width 255.

  DEFINE FRAME PageBottom
      HEADER
      "<C4><FROM><C80><LINE>" SKIP
      "<C4><FTimes New Roman><P10>" pcBrukerId FORMAT "x(12)" pcDato pcTid "<C47>" cFirma FORMAT "x(50)"
      WITH PAGE-BOTTOM STREAM-IO WIDTH 255.

  view frame PageTopp.
  VIEW FRAME PageBottom.

  FOR EACH KampanjeLinje OF KampanjeHode NO-LOCK
     BY KampanjeLinje.Vg
     BY KampanjeLinje.LopNr:

     FIND ArtBas NO-LOCK where
         ArtBas.ArtikkelNr = KampanjeLinje.ArtikkelNr NO-ERROR.
     
     PUT
       "<P9><Fcourier New></B>"   
       "<C3>"  KampanjeLinje.Vg                                 FORMAT "zzzzz9" 
       " "     KampanjeLinje.LopNr                              FORMAT "zzzz9"
       " "     (IF AVAIL Artbas THEN Artbas.LevKod ELSE "")     FORMAT "x(20)"
       " "     (IF AVAIL Artbas THEN Artbas.Beskr ELSE "")      FORMAT "x(30)"
       " "     (IF AVAIL Artbas THEN Artbas.LevFargKod ELSE "") FORMAT "x(20)"
       " "     KampanjeLinje.Pris[2] FORMAT ">>>,>>9.99"
       "</B>"
       SKIP
       .
       
/*      ASSIGN                                     */
/*        iRad = iRad + 1                          */
/*        cRader = cRader + "," + STRING(iRad + 1) */
/*        .                                        */
  END.

  /* Lukker stream */
  OUTPUT CLOSE.

  /* Klargjør rapportfilnavnet */
  ASSIGN
    FILE-INFO:File-NAME = pcRappFil.
    
  /* Sender filen til visning og utskrift. */
 RUN PrintPDF(FILE-INFO:FULL-PATHNAME, 'POLYGON SOFTWARE AS', 'A1a9T4h4e2h_mqe2mbka' ). 

  STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-bredd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd Procedure 
FUNCTION bredd RETURNS DECIMAL
    ( INPUT cText AS CHARACTER ):
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/

    RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByttElement) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ByttElement Procedure 
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

&ENDIF

&IF DEFINED(EXCLUDE-getOKtext) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getOKtext Procedure 
FUNCTION getOKtext RETURNS CHARACTER
  ( INPUT cText AS CHAR, INPUT iAvailBredd AS INTE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  REPEAT WHILE bredd(cText) > iAvailBredd:
      cText = SUBSTR(cText,1,LENGTH(cText) - 1).
  END.
  RETURN cText.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

