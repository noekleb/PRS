/* Utskrift av medlemsrabattsjekker. 
   Parameter:  
   Opprettet: 5.4.2011             
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cFlagg          AS CHAR       NO-UNDO.
DEFINE VARIABLE iCL             AS INT        NO-UNDO.
DEFINE VARIABLE cTitle          AS CHARACTER INIT "RABATTSJEKK" NO-UNDO.
DEFINE VARIABLE iRad            AS INTEGER    NO-UNDO.
DEFINE VARIABLE cColLabelString AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dYBas           AS INTEGER    NO-UNDO.
DEFINE VARIABLE dY              AS INTEGER    NO-UNDO. /* LinjeNr */
DEFINE VARIABLE iColLbl         AS INTEGER    EXTENT 20 NO-UNDO. /* Kollonne */
DEFINE VARIABLE cPrintString    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKundenavn      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cButNamn LIKE Butiker.ButNamn NO-UNDO.
DEFINE VARIABLE lDirekte        AS LOG        NO-UNDO.
DEFINE VARIABLE cFilNavn        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrinter        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE     NO-UNDO.
DEFINE VARIABLE iPageHeight     AS INTEGER    NO-UNDO.
DEFINE VARIABLE iPageWidth      AS INTEGER    NO-UNDO.
DEFINE VARIABLE pcRappFil       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSidNr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE iRadNr          AS INTEGER    NO-UNDO.
DEFINE VARIABLE iLoop           AS INTEGER    NO-UNDO.
DEFINE VARIABLE iAntEks         AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSideNr         AS INTEGER    NO-UNDO.
DEFINE VARIABLE iLeftCol        AS INTEGER    NO-UNDO.
DEFINE VARIABLE cTekst          AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE VARIABLE cTxt            AS CHARACTER  NO-UNDO.
{runlib.i}
{pdf_inc.i "THIS-PROCEDURE"}
{incl/DevMode.i}
{incl/CustDevMode.i}

{syspara.i 5 1 1 iCL INT}
{syspara.i 1 1 100 cKundenavn}
{syspara.i 1 1 101 cPolygon}

FUNCTION Bredd RETURNS DECIMAL 
	( INPUT cText AS CHARACTER ) FORWARD.

FUNCTION getRapPrinter RETURNS CHARACTER 
    ( INPUT ipcPrinter AS CHARACTER ) FORWARD.


    ASSIGN
        lDirekte   = FALSE /* Utskrift til skjerm */
        iAntEks    = 1 /* Antall eksemplarer av utskriften */
        iLeftCol   = 50
        cTekst[ 1] = 'Gjelder som rabatt i alle våre butikker.' 
        cTekst[ 2] = 'Kan ikke byttes mot kontanter.' 
        cTekst[ 3] = '' 
        cTekst[ 4] = 'Gyldig t.o.m.' 
        cTekst[ 5] = 'kroner'
        cTekst[ 6] = 'Serienr.'
        cTekst[ 7] = 'Rabattsjekker som mistes erstattes ikke. Innløst rabattsjekk regnes som brukt. Gis ikke tilbake ved retur av varer.'
        cTekst[ 8] = 'Medlemsnummer'
        cFlagg     = ENTRY(1,icParam,'|').

{syspara.i 14 3  1 cTxt}
cTekst[1] = cTxt.
{syspara.i 14 3  2 cTxt}
cTekst[2] = cTxt.
{syspara.i 14 3  3 cTxt}
cTekst[3] = cTxt.
{syspara.i 14 3  4 cTxt}
cTekst[4] = cTxt.
{syspara.i 14 3  5 cTxt}
cTekst[5] = cTxt.
{syspara.i 14 3  6 cTxt}
cTekst[6] = cTxt.
{syspara.i 14 3  7 cTxt}
cTekst[7] = cTxt.
{syspara.i 14 3  8 cTxt}
cTekst[8] = cTxt.

/* Henter standardskriver når det skrives direkte til skriver. */
IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN
    RETURN.

cFilNavn = SESSION:TEMP-DIR + "RABATTSJEKK" + "_" + STRING(TIME) + ".pdf".

/* Oppsett av utskriftssesjon */
RUN pdf_new ("Spdf",cFilNavn).
RUN pdf_set_BottomMargin ("Spdf", 20).
RUN pdf_set_PaperType ("Spdf","A4").
iPageHeight = pdf_PageHeight ("Spdf").
iPageWidth  = pdf_PageWidth ("Spdf").
RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
RUN pdf_set_Orientation ("Spdf","portrait").

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK BY MedRabSjekk.MedlemsNr").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN 
    ocReturn = ''
    obOk     = TRUE
    iCount = 0
    iSidNr = 0
    dYBas  = 220
    .

  UTSKRIFT:
  DO:
    FIND FIRST MedRabSjekk WHERE MedRabSjekk.RabSjekkId = DEC(ihBuffer:BUFFER-FIELD('RabSjekkId'):BUFFER-VALUE)
                         NO-LOCK NO-ERROR.
    
    IF AVAIL MedRabSjekk THEN
    AVAIL_BLOKK:
    DO:
      FIND Medlem OF MedRabSjekk NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Medlem THEN 
        LEAVE AVAIL_BLOKK.
      FIND RabSjekkType OF MedRabSjekk NO-ERROR.
      IF NOT AVAILABLE RabSjekkType THEN 
        LEAVE AVAIL_BLOKK.
      FIND Kunde OF medlem NO-LOCK NO-ERROR.
      IF AVAIL Kunde THEN
          FIND FIRST kundekort OF kunde NO-LOCK NO-ERROR.
      ELSE
          RELEASE kundekort.
        
      ASSIGN
        iCount       = iCount  + 1
        iSidNr       = iSideNr + 1.
        
      RUN pdf_new_page ("Spdf").
      RUN RitaRamar. /* pt kvar att göra */
      
      /* Venstre side av sjekken - venstrestillte felt */
      ASSIGN dY = dYBas iColLbl[1] = 50.   
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).
      RUN pdf_text_xy_dec ("Spdf",RabSjekkType.RabSjekkTypeBeskrivelse,iColLbl[1],dY).
      ASSIGN dY = dYBas - 14 iColLbl[1] = 50.         
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
      ASSIGN dY = dYBas - (2 * 14).      
      RUN pdf_text_xy_dec ("Spdf",cTekst[1],iColLbl[1],dY).
      ASSIGN dY = dYBas - (3 * 14).      
      RUN pdf_text_xy_dec ("Spdf",cTekst[2],iColLbl[1],dY).
      ASSIGN dY = dYBas - (4 * 14).      
      RUN pdf_text_xy_dec ("Spdf",cTekst[3],iColLbl[1],dY).
      ASSIGN dY = dYBas - (5 * 14).      
      RUN pdf_text_xy_dec ("Spdf",cTekst[4] + ' ' + STRING(MedRabSjekk.DatoGyldig,'99/99/99'),iColLbl[1],dY).                 
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).
      ASSIGN dY = dYBas - (8 * 14).      
      RUN pdf_text_xy_dec ("Spdf",TRIM(STRING(MedRabSjekk.Belop,"->>,>>>,>>9")) + ' ' + cTekst[5],iColLbl[1],dY).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
      ASSIGN dY = dYBas - (10 * 14).      
      RUN pdf_text_xy_dec ("Spdf",cTekst[6] + ' ' + STRING(MedRabSjekk.RabSjekkSerieNr),iColLbl[1],dY).
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",8).
      ASSIGN dY = dYBas - (12 * 14).      
      RUN pdf_text_xy_dec ("Spdf",cTekst[7],iColLbl[1],dY).

      /* Høyresiden av sjekken - høyrestillte felt */            
      ASSIGN
        dY         = dYBas
        iColLbl[1] = 550
        cTxt       = cKundenavn.   
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",24).
      RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1] - bredd(cTxt),dY).

      /* Medlemsnummertext */
      ASSIGN
        dY         = dYBas - (10 * 14)
/*         iColLbl[1] = 550 */
          iColLbl[1] = 350
        cTxt       = cTekst[8].   
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",14).
      RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1] - bredd(cTxt),dY).

      /* Medlemmens namn */
      ASSIGN
        dY         = dYBas - (10 * 14)
        iColLbl[1] = 550
        cTxt       = TRIM(Medlem.fornavn) + (IF TRIM(Medlem.etternavn) <> "" THEN " " ELSE "") + TRIM(Medlem.etternavn).   
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",14).
      RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1] - bredd(cTxt),dY).

      /* Medlemsnummer */
      ASSIGN
        dY           = dYBas - (11 * 14)
          iColLbl[1] = 350
        cTxt         = TRIM(STRING(MedRabSjekk.MedlemsNr)).   
      RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
      RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1] - bredd(cTxt),dY).
      /* Kundkortsnummer */
      IF AVAIL kundekort THEN DO:
          ASSIGN
            dY           = dYBas - (11 * 14)
            iColLbl[1] = 550
            cTxt         = TRIM(Kundekort.kortnr).   
          RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",10).
          RUN pdf_text_xy_dec ("Spdf",cTxt,iColLbl[1] - bredd(cTxt),dY).
      END.

      DO TRANSACTION:
          FIND CURRENT MedRabSjekk EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE MedRabSjekk THEN
          DO:
              ASSIGN
                  MedRabSjekk.DatoSkrevet = TODAY
                  MedRabSjekk.TidSkrevet  = TIME.
              FIND CURRENT MedRabSjekk NO-LOCK NO-ERROR.
              
          END.
      END. /* TRANSACTION */

      /* Flagger resultatstatus. */
      obOk = NOT ERROR-STATUS:ERROR.
      IF NOT obOk THEN
      DO:
        ocReturn = ERROR-STATUS:GET-MESSAGE(1).
        LEAVE.
      END.
    END. /* AVAIL_BLOKK */
  END. /* UTSKRIFT */

  IF AVAIL MedRabSjekk THEN RELEASE MedRabSjekk.
  hQuery:GET-NEXT().
END.

RUN pdf_close ("Spdf").

IF lDirekte = FALSE THEN 
  DO:
    IF obOk THEN ocReturn = cFilNavn.
    /*RUN browse2pdf\viewxmldialog.w (cFilNavn,"RABATTSJEKK").*/
  END.
ELSE 
  DO iLoop = 1 TO iAntEks:
    OS-COMMAND SILENT VALUE(".\cmd\PrintPdf " + cFilnavn + ' "' + cPrinter + '"').
  END.


/* **********************  Internal Procedures  *********************** */


PROCEDURE RitaRamar:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

        RUN pdf_stroke_fill IN h_PDFinc ("Spdf",.92,.92,.92).
        RUN pdf_rect IN h_PDFinc ("Spdf", iLeftcol - 10, 40, 515, 220,0.1).

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  å - 229 - 97
            ä - 228 - 97
            ö - 246 - 111
            Å - 197 - 65
            Ä - 196 - 65
            Ö - 214 - 79    
------------------------------------------------------------------------------*/
  ASSIGN 
    cText = REPLACE(cText,CHR(229),CHR(97))
    cText = REPLACE(cText,CHR(228),CHR(97))
    cText = REPLACE(cText,CHR(246),CHR(111))
    cText = REPLACE(cText,CHR(197),CHR(65))
    cText = REPLACE(cText,CHR(196),CHR(65))
    cText = REPLACE(cText,CHR(214),CHR(79))
    .
  RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

FUNCTION getRapPrinter RETURNS CHARACTER
  ( INPUT ipcPrinter AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN IF ipcPrinter <> "" THEN ipcPrinter ELSE
      IF DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") <> "" THEN
          DYNAMIC-FUNCTION("getAttribute",SESSION,"SE_PRINTER") ELSE SESSION:PRINTER-NAME.   /* Function return value. */
END FUNCTION.
