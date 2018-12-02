DEFINE INPUT  PARAMETER dKampId AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dPris AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dUtleggArtnr                               AS DECIMAL    NO-UNDO.
DEFINE VARIABLE dUtleggArtNrTMP                            AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cAktiv AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lChoice AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE TT_image NO-UNDO
    FIELD image_name    AS CHARACTER
 INDEX obj_name AS PRIMARY
          image_name.

{ pdf_inc.i "THIS-PROCEDURE"}


FIND KampanjeMixMatch WHERE KampanjeMixMatch.KampId = dKampId /* dKampId */ NO-LOCK NO-ERROR.
IF NOT AVAILABLE KampanjeMixMatch THEN
  RETURN.
MESSAGE "Vill Du ha etiketter? "
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE lChoice.
CASE lChoice:
    WHEN TRUE THEN DO:
/*      MESSAGE "Skriver Etiketter!"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      RUN SkrivEtikketter.
      RETURN.
    END.
    WHEN FALSE THEN
        PAUSE 1.
/*     MESSAGE "Skriver lista!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    OTHERWISE RETURN.
END CASE.

OUTPUT TO "CLIPBOARD".
                    
IF AVAIL KampanjeMixMatch THEN DO:
    PUT UNFORMATTED "Kampid"     CHR(9)
                    "Namn"       CHR(9)
                    "Notat"      CHR(9)
                    "Startdatum" CHR(9)
                    "Starttid"   CHR(9)
                    "Slut datum" CHR(9)
                    "Slut tid"   CHR(9)
                    "Aktiv"       CHR(9)
                    "Sänt datum" CHR(9)
                    "Sänt tid"   SKIP.

    PUT UNFORMATTED KampanjeMixMatch.KampId        CHR(9)
                    KampanjeMixMatch.KampNavn      CHR(9)
                    KampanjeMixMatch.KampanjeNotat CHR(9)
                    "'" KampanjeMixMatch.KampStartDato CHR(9)
                    STRING(KampanjeMixMatch.KampStartTid,"HH:MM") CHR(9)
                    "'" KampanjeMixMatch.KampSluttDato CHR(9)
                    STRING(KampanjeMixMatch.KampSluttTid,"HH:MM") CHR(9)
                    STRING(KampanjeMixMatch.KampKlar,"Ja/Nej")      CHR(9)
                    "'" KampanjeMixMatch.KampSendtDato CHR(9)
                    STRING(KampanjeMixMatch.KampSendtTid,"HH:MM") SKIP CHR(9) SKIP.
    
    FOR EACH KampanjeTilbud OF KampanjeMixMatch NO-LOCK:
        FIND kampanjetilbtype OF KampanjeTilbud NO-LOCK NO-ERROR.

        PUT UNFORMATTED chr(9) skip
                        KampanjeTilbud.KampId                   CHR(9)
                        KampanjeTilbud.KampTilbId               CHR(9)
                        KampanjeTilbud.HapHourId                CHR(9)
                        IF AVAIL Kampanjetilbtype THEN Kampanjetilbtype.KampTilbTypeNavn ELSE " " CHR(9)
                        "Pris: " STRING(KampanjeTilbud.KampTilbBelop,">>>>9.99")            CHR(9)
                        IF KampanjeTilbud.KamptilbGrenseAntall > 0 THEN "Max: " + STRING(KampanjeTilbud.KamptilbGrenseAntall) ELSE ""     CHR(9)
                        "Gräns: " string(KampanjeTilbud.KamptilbGrenseAntallBruk,"J/N") CHR(9)
                        "Text: " KampanjeTilbud.KampTilbKvitteringstekst CHR(9)
                        "Namn: " KampanjeTilbud.KampTilbNavn             CHR(9)
                        KampanjeTilbud.KamptilbNotat            CHR(9)
                        "Stegvis: " string(KampanjeTilbud.KampTilbOkning,"J/N")        CHR(9)
                        KampanjeTilbud.KamptilbPopUpTekst       CHR(9)
                        "Popup: " STRING(KampanjeTilbud.KamptilbPopUpTekstBruk,"J/N")  CHR(9)
                        "Prop fördeln: " string(KampanjeTilbud.KampTilbPropBetalFor,"J/N")      CHR(9)
                        ENTRY(KampanjeTilbud.KampTilbTypeId,"pay,save,percent,payfor") SKIP.
         IF KampanjeTilbud.HapHourId <> 0 THEN DO:
             FIND HappyHourHode OF KampanjeTilbud NO-LOCK NO-ERROR.
             IF NOT AVAIL HappyHourHode THEN
                 PUT UNFORMATTED "HH-hode saknas" SKIP.
             ELSE IF NOT can-find(FIRST HappyHourPeriode OF HappyHourHode) THEN
                 PUT UNFORMATTED "HH-periode saknas" SKIP.
             ELSE DO:
                 FOR EACH HappyHourPeriode OF HappyHourHode NO-LOCK:
                     PUT UNFORMATTED HappyHourPeriode.HapHourId             CHR(9)
                                     HappyHourPeriode.HapHourPerId          CHR(9)
                                     STRING(HappyHourPeriode.HapHourPerStartTid,"HH:MM:SS")    CHR(9)
                                     STRING(HappyHourPeriode.HapHourPerSluttTid,"HH:MM:SS")    CHR(9)
                                     HappyHourPeriode.HapHourPerUkedagListe SKIP.
                 END.
             END.
         END.
         FOR EACH KampanjeTilbArtikkel OF Kampanjetilbud NO-LOCK:
             FIND Produktfamilie OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
             IF NOT AVAIL Produktfamilie THEN
                 PUT UNFORMATTED "Ingen prodfam" SKIP.
             ELSE DO:
                 FIND KampRabattType OF KampanjeTilbArtikkel NO-LOCK NO-ERROR.
                 PUT UNFORMATTED  KampanjeTilbArtikkel.KampTilbArtId CHR(9)
                                 "Prodfam: " STRING(KampanjeTilbArtikkel.ProdFamId)CHR(9)
                                  ProduktFamilie.ProdFamNavn         CHR(9)
                                  "Minantal: " STRING(KampanjeTilbArtikkel.KampTilbArtMinAntall,">>>>>>>9.99") CHR(9)
                                  "Belopp: "   STRING(KampanjeTilbArtikkel.KampTilbArtBelop,">>>>>9.99") CHR(9)
                                  "Rabtyp: "    IF AVAIL KampRabattType THEN KampRabattType.KampRabattTypeNavn ELSE "" SKIP.
                 IF KampanjeTilbArtikkel.KampTilbArtId <> 0 THEN DO:
                     FIND Artbas WHERE artbas.artikkelnr = KampanjeTilbArtikkel.KampTilbArtId NO-LOCK NO-ERROR.
                     IF NOT AVAIL Artbas THEN
                         PUT UNFORMATTED "Artikkel saknas" CHR(9) KampanjeTilbArtikkel.ProdFamId chr(9) KampanjeTilbArtikkel.KampTilbArtId SKIP.
                     ELSE DO:
                         FIND FIRST strekkode OF artbas NO-LOCK NO-ERROR.
/*                          IF NOT AVAIL strekkode THEN                                                                                              */
/*                              PUT UNFORMATTED "Ingen EAN" CHR(9) KampanjeTilbArtikkel.ProdFamId chr(9) Artbas.artikkelnr chr(9) artbas.beskr SKIP. */
/*                          ELSE DO:                                                                                                                 */
                             dPris = 0.
                             FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
                             IF AVAIL artpris THEN
                                 dPris = IF artpris.tilbud THEN artpris.pris[2] ELSE artpris.pris[1].
                             IF artbas.aktivert = FALSE OR artbas.aktivert = FALSE THEN
                                 cAktiv = "DEAKTIVERAD".
                             ELSE
                                 cAktiv = "".
                             PUT UNFORMATTED artbas.artikkelnr CHR(9) artbas.beskr CHR(9) cAktiv chr(9) "Pris: " string(dPris,">>>>9.99") CHR(9) STRING(AVAIL Strekkode,"EAN/INGEN EAN") SKIP. 
/*                          END. */
                     END.
                 END.
                 FOR EACH produktfammedlem OF produktfamilie NO-LOCK:
                     dUtleggArtnr = ProduktFamMedlem.ProdFamArtikkelNr.
                     FIND artbas WHERE artbas.artikkelnr = ProduktFamMedlem.ProdFamArtikkelNr NO-LOCK NO-ERROR.
                     IF NOT AVAIL artbas THEN
                         PUT UNFORMATTED "Artikel saknas" CHR(9) KampanjeTilbArtikkel.ProdFamId chr(9) ProduktFamMedlem.ProdFamArtikkelNr SKIP.
                     ELSE DO:
                         FIND FIRST strekkode OF artbas NO-LOCK NO-ERROR.
                         IF NOT AVAIL strekkode THEN
                             PUT UNFORMATTED "Ingen EAN" CHR(9) KampanjeTilbArtikkel.ProdFamId chr(9) Artbas.artikkelnr chr(9) artbas.beskr SKIP.
                         ELSE DO:
                             FIND vargr OF artbas NO-LOCK NO-ERROR.
                             IF AVAIL vargr THEN 
                                 FIND huvgr OF vargr NO-LOCK NO-ERROR.
                             IF AVAIL huvgr AND huvgr.avdelingnr = 1 THEN DO:
                                 FOR EACH strekkode OF artbas WHERE TRIM(strekkode.bestillingsnummer) <> "":
                                     dUtleggArtNrTMP = DECI(strekkode.bestillingsnummer) NO-ERROR.
                                     IF NOT ERROR-STATUS:ERROR THEN
                                         LEAVE.
                                     ELSE
                                         dUtleggArtNrTMP.
                                 END.
                             END.
                             IF dUtleggArtNrTMP > 0 THEN
                                 dUtleggArtnr = dUtleggArtNrTMP.
                             dPris = 0.
                             IF artbas.aktivert = FALSE OR artbas.aktivert = FALSE THEN
                                 cAktiv = "DEAKTIVERAD".
                             ELSE
                                 cAktiv = "".
                             FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
                             IF AVAIL artpris THEN
                                 dPris = IF artpris.tilbud THEN artpris.pris[2] ELSE artpris.pris[1].
                             PUT UNFORMATTED artbas.artikkelnr CHR(9) artbas.beskr CHR(9) "Pris: " string(dPris,">>>>9.99") chr(9) cAktiv SKIP. 
                         END.
                     END.
                 END.
             END.
         END.
    END.
END.
OUTPUT CLOSE.    
MESSAGE "Data i clipboard"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
RETURN.

PROCEDURE SkrivEtikketter:
DEFINE VARIABLE iMaxTkn      AS INTEGER     NO-UNDO.
DEFINE VARIABLE lBold        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iLeft        AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRight       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cTxt         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iTknStr      AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFilNamn     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLeftMargin  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iRMarginPos  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lFirst       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dPos         AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iPrisKr      AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPrisOren    AS INTEGER     NO-UNDO.
DEFINE VARIABLE dKrWidth     AS DECIMAL     NO-UNDO.
DEFINE VARIABLE stDate       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE slDate       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iPageHeight  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPageWidth   AS INTEGER     NO-UNDO.
DEFINE VARIABLE lRed         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lBild        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cBildefil    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBildNamn    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iImageHeight AS INTEGER     NO-UNDO.
DEFINE VARIABLE iImageWidth  AS INTEGER     NO-UNDO.
DEFINE VARIABLE lOrdPris     AS LOGICAL     NO-UNDO.
DEFINE VARIABLE dOrdPris     AS DECIMAL     NO-UNDO.

ASSIGN lFirst = TRUE.
ASSIGN stDate = STRING(YEAR(KampanjeMixMatch.KampStartDato),"9999") + "-" 
              + STRING(MONTH(KampanjeMixMatch.KampStartDato),"99") + "-" 
              + STRING(DAY(KampanjeMixMatch.KampStartDato),"99").

ASSIGN slDate = STRING(YEAR(KampanjeMixMatch.KampSluttDato),"9999") + "-" 
              + STRING(MONTH(KampanjeMixMatch.KampSluttDato),"99") + "-" 
              + STRING(DAY(KampanjeMixMatch.KampSluttDato),"99").
/*MESSAGE "iLeftMargin " iLeftMargin SKIP "iRMarginPos " iRMarginPos
    VIEW-AS ALERT-BOX INFO BUTTONS OK. */

/*MESSAGE "Vill Du ha rött belopp?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice.
IF lChoice = TRUE THEN
  ASSIGN lRed = TRUE.
ELSE
  ASSIGN lRed = FALSE.*/

FOR EACH KampanjeTilbud OF KampanjeMixMatch NO-LOCK:

    MESSAGE "Vill Du ha etikett för " SKIP KampanjeTilbud.KampTilbNavn "?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lChoice.
    IF lChoice = FALSE THEN
      NEXT.
      IF lFirst = TRUE THEN
      DO:
          ASSIGN lFirst = FALSE.
          ASSIGN cFilNamn = SESSION:TEMP-DIR + "MixMatch.pdf".
          RUN pdf_new ("Spdf",cFilNamn).
          RUN pdf_set_BottomMargin ("Spdf", 60).
          RUN pdf_set_PaperType ("Spdf","A4").
          RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13).
                         /*   RUN LoadFonts. */
          RUN pdf_set_Orientation ("Spdf","portrait").

          ASSIGN iLeftMargin = pdf_LeftMargin ("Spdf") 
                 iRMarginPos =  pdf_PageWidth ("Spdf") - iLeftMargin.
          iPageHeight = pdf_PageHeight ("Spdf").
          iPageWidth  = pdf_PageWidth ("Spdf").
      END.
/*MESSAGE "iTknStr " iTknStr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

  RUN pdf_new_page ("Spdf").
  RUN pdf_stroke_fill ("Spdf",1.0,1.0,1.0).
  RUN pdf_rect2 ("Spdf", 8, 20, iPageWidth - 12, iPageHeight - 40,2.0). /* Left,Bottum,Width,Hight,Thickness */
  RUN pdf_stroke_fill ("Spdf",1.0,1.0,1.0).
  {syspara.i 5 20 200 cBildeFil}
  ASSIGN lBild = FALSE.
/*  ASSIGN cBildefil = "c:\appdir\se\icon\TEMPO20Logga.jpg".*/
  ASSIGN cBildefil = REPLACE(cBildefil,".bmp",".jpg").
  IF cBildefil <> "" AND SEARCH(cBildefil) <> ? THEN
  DO:
    ASSIGN cBildNamn = cBildefil.
    IF NOT CAN-FIND(FIRST TT_image
           WHERE TT_image.image_name = cBildNamn NO-LOCK) 
    THEN DO:
      RUN pdf_load_image ("Spdf",cBildNamn,cBildefil).
      CREATE TT_image.
      ASSIGN TT_image.image_name = cBildNamn.
    END.
    iImageHeight = pdf_imageDim ("Spdf",cBildNamn,"HEIGHT").
    iImageWidth = pdf_imageDim ("Spdf",cBildNamn,"WIDTH").

    IF iImageWidth >= iImageHeight THEN
      ASSIGN iImageHeight = 200 * (iImageHeight / iImageWidth)
             iImageWidth = 200.
    ELSE
      ASSIGN iImageWidth = 180 * (iImageWidth / iImageHeight)
             iImageHeight = 180.

    RUN pdf_place_image ("Spdf",cBildNamn,200,140,iImageWidth,iImageHeight).
    ASSIGN lBild = TRUE.
  END.

  ASSIGN cTxt = TRIM(KampanjeTilbud.KampTilbNavn)
         iMaxTkn = LENGTH(cTxt)
         lBold = TRUE
         iLeft = iLeftMargin
         iRight = iRMarginPos - 35
         iTknStr = 200.

  RUN PDFGetMaxStr (iMaxTkn,lBold,iLeft,iRight,cTxt,INPUT-OUTPUT iTknStr).
  RUN pdf_text_xy_dec ("Spdf",cTxt,iLeft + 5,600).

  ASSIGN cTxt = "Köp " + STRING(KampanjeTilbud.KamptilbGrenseAntall) + " för " + STRING(KampanjeTilbud.KampTilbBelop,">>>>9.99") + " Kr.".
  ASSIGN iMaxTkn = LENGTH(cTxt).
  ASSIGN lBold = FALSE.
  ASSIGN iTknStr = 80.
  ASSIGN dPos = iLeft + iMaxTkn.
/*  RUN PDFGetMaxStr (iMaxTkn,lBold,iLeft,iRight,cTxt,INPUT-OUTPUT iTknStr).*/
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",40).
  RUN pdf_text_xy_dec ("Spdf",cTxt,100,530).

  ASSIGN iPrisKr = TRUNC(KampanjeTilbud.KampTilbBelop,0).
  ASSIGN iPrisOren = (KampanjeTilbud.KampTilbBelop - iPrisKr) * 100.

  RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).

  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",100).
  IF lRed = TRUE THEN
    RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,.0,.0).
  RUN pdf_text_xy_dec ("Spdf",STRING(iPrisOren,"99"),450,400).
  IF iPrisKr > 99 THEN
    RUN pdf_text_xy_dec ("Spdf","Kr",460,300).
  ELSE
    RUN pdf_text_xy_dec ("Spdf","Kr",460,250).
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",300).
  IF iPrisKr > 999 THEN
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",200).
  ELSE IF iPrisKr > 99 THEN
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",250).
  dKrWidth = pdf_text_widthdec ("Spdf",TRIM(STRING(iPrisKr))).
  IF iPrisKr > 999 THEN
    RUN pdf_text_xy_dec ("Spdf",STRING(iPrisKr),450 - dKrWidth,330).
  ELSE IF iPrisKr > 99 THEN
    RUN pdf_text_xy_dec ("Spdf",STRING(iPrisKr),450 - dKrWidth,300).
  ELSE
    RUN pdf_text_xy_dec ("Spdf",STRING(iPrisKr),450 - dKrWidth,250).
  RUN pdf_stroke_fill IN h_PDFinc ("Spdf",1.0,1.0,1.0).

  ASSIGN lOrdPris = FALSE.
  FIND FIRST KampanjeTilbArtikkel WHERE KampanjeTilbud.KampId = KampanjeTilbArtikkel.KampId AND
             KampanjeTilbud.KampTilbId = KampanjeTilbArtikkel.KampTilbId NO-LOCK.
  IF AVAILABLE KampanjeTilbArtikkel THEN
  DO:
    FIND FIRST Artpris WHERE KampanjeTilbArtikkel.KampTilbArtId = Artpris.ArtikkelNr NO-LOCK.
    IF AVAILABLE Artpris THEN
    DO:
      ASSIGN lOrdPris = TRUE.
      ASSIGN dOrdPris = Artpris.pris[1] * KampanjeTilbud.KamptilbGrenseAntall.
    END.
  END.

  IF lOrdPris = TRUE THEN
  DO:
    ASSIGN cTxt = "Ordinarie pris " + STRING(dOrdPris,">>>>9.99") + " Kr.".
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",30).
    RUN pdf_text_xy_dec ("Spdf",cTxt,iLeft + 100,130).
  END.

  ASSIGN cTxt = "Gäller " + stDate + " t.o.m. " + slDate.
  RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",30).
  RUN pdf_text_xy_dec ("Spdf",cTxt,iLeft + 50,30).



END.

IF lFirst = TRUE THEN
  RETURN.
RUN pdf_close ("Spdf").

RUN browse2pdf\viewxmldialog.w (cFilNamn,"MixMatch").

END PROCEDURE.

PROCEDURE PDFGetMaxStr:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER iMaxTkn AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER lBold   AS LOGICAL   NO-UNDO.
DEFINE INPUT        PARAMETER iLeft   AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER iRight  AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER cTxt    AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iTknStr AS INTEGER   NO-UNDO.
DEFINE VARIABLE               iAntTkn AS INTEGER   NO-UNDO.

REPEAT:
  IF lBold = TRUE THEN
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",iTknStr).
  ELSE
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",iTknStr).
  iAntTkn = pdf_GetNumFittingChars ("Spdf",cTxt,iLeft,iRight).                        
/*         MESSAGE "iTknStr" iTknStr SKIP "iAntTkn" iAntTkn SKIP "iMaxTkn " iMaxTkn
             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF iAntTkn >= iMaxTkn THEN
    LEAVE.
  ASSIGN iTknStr = iTknStr - 1.
END.

END PROCEDURE.

