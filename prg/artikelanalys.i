    IF lNullposter = FALSE AND TT_ArtLager.AntSolgt = 0 AND TT_ArtLager.KjopAnt = 0 THEN 
    DO:
      NEXT.
    END.
    IF lNullager = FALSE AND TT_ArtLager.LagAnt = 0 THEN 
    DO:
      NEXT.
    END.
          
    IF lFirst = TRUE OR TT_ArtLager.Vg <> VgSpar THEN
    DO:
      IF NOT lFirst THEN
        RUN PDFPrintVg (INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN iTest = 1.
/* RUN pdf_text_xy_dec ("Spdf","-A-" + STRING(doRad),400,doRad - 20). */
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN lFirst = FALSE
               VgSpar = TT_ArtLager.Vg.
        FIND FIRST VarGr WHERE VarGr.Vg = TT_ArtLager.Vg NO-LOCK NO-ERROR.
        IF AVAILABLE VarGr THEN
          RUN pdf_text_xy_dec ("Spdf",STRING(VarGr.Vg) + " " + VarGr.VgBeskr,iLM,doRad).
        ELSE
          RUN pdf_text_xy_dec ("Spdf",STRING(TT_ArtLager.Vg) + " *Saknas*",iLM,doRad).
/*        RUN pdf_text_xy_dec ("Spdf"," F.Pr.                 I.Pr       V.Pr",iLM + 400,doRad). */
    END.
    ASSIGN doRad = doRad - 15.
IF doRad < 115 THEN
/*     IF doRad < 90 THEN */
    DO:
       ASSIGN iTest = 2.
/* RUN pdf_text_xy_dec ("Spdf","-B-" + STRING(doRad),400,doRad - 20). */
       RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
       doRad = doRad - 15.
    END.
/* RUN pdf_text_xy_dec ("Spdf","-B2-" + STRING(doRad),250,doRad + 15). */

    RUN pdf_text_xy_dec ("Spdf"," F.Pr.                 I.Pr       V.Pr",430,doRad + 15).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM, doRad + 10, pdf_PageWidth("Spdf") - pdf_LeftMargin ("Spdf") , doRad + 10, 0.5).
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    RUN pdf_text_xy_dec ("Spdf",STRING(TT_ArtLager.Vg) + "/" + STRING(TT_ArtLager.Lopnr),iLM,doRad).
    RUN pdf_text_xy_dec ("Spdf",TT_ArtLager.Firstinlev,iLM,doRad - 15).
    RUN pdf_text_xy_dec ("Spdf",TT_ArtLager.BestDato,iLM,doRad - 25).
    RUN pdf_text_xy_dec ("Spdf",TT_ArtLager.LevArtNr,iLM + 40,doRad).
    FIND FIRST LevBas WHERE LevBas.levnr = TT_ArtLager.LevNr NO-LOCK NO-ERROR.
    IF LENGTH(TRIM(LevBas.Levnamn)) > 20 THEN
      RUN pdf_text_xy_dec ("Spdf",STRING(TT_ArtLager.LevNr) + " " + SUBSTRING(LevBas.LevNamn,1,20) + ">",iLM + 110,doRad).
    ELSE
      RUN pdf_text_xy_dec ("Spdf",STRING(TT_ArtLager.LevNr) + " " + LevBas.LevNamn,iLM + 110,doRad).
    FIND FIRST Farg WHERE Farg.Farg = TT_ArtLager.Farg NO-LOCK NO-ERROR.
    FIND FIRST Material WHERE Material.MatKod = TT_ArtLager.MatKod NO-LOCK NO-ERROR.
    ASSIGN cWrk = Farg.FarBeskr + "/" + Material.MatBeskr
           cWrk = TRIM(cWrk).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 300,doRad).
    ASSIGN cWrk = TRIM(STRING(TT_ArtLager.PrisOrdi,"zzzzz9.99") + " SEK/" + STRING(TT_ArtLager.PrisTilb,"zzzzz9.99") + STRING(TT_ArtLager.ValPris,"zzzzz9.99")).
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM + 390,doRad).
    ASSIGN TT_ArtLager.BildeFil = REPLACE(TT_ArtLager.BildeFil,".bmp",".jpg").
    IF TT_ArtLager.BildeFil <> "" AND SEARCH(TT_ArtLager.BildeFil) <> ? THEN
    DO:
      IF doRad < 95 THEN DO:
/* RUN pdf_text_xy_dec ("Spdf","-C-" + STRING(doRad),400,doRad - 20). */
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      END.
      ASSIGN cBildNamn = "mini" + STRING(TT_ArtLager.ArtikkelNr).
      IF NOT CAN-FIND(FIRST TT_image WHERE TT_image.image_name = cBildNamn NO-LOCK) THEN
      DO:
        RUN pdf_load_image ("Spdf",cBildNamn,TT_ArtLager.BildeFil).
        CREATE TT_image.
        ASSIGN TT_image.IMAGE_name = cBildNamn.
      END.
      ASSIGN iImageHeight = pdf_imageDim ("Spdf",cBildNamn,"HEIGHT").
      ASSIGN iImageWidth = pdf_imageDim ("Spdf",cBildNamn,"WIDTH").

      IF iImageWidth >= iImageHeight THEN
/*           h = 50                   */
/*           b = 50 * b/h dock MAX 70 */
/*           IF b > 70  THEN          */
          ASSIGN iImageWidth = 50 * iImageWidth / iImageHeight
                 iImageHeight = 50.

/*         ASSIGN iImageHeight = 50 * (iImageHeight / iImageWidth)  /* 80 */ */
/*                iImageWidth = 50.                                          */
      ELSE
        ASSIGN iImageWidth = 50 * (iImageWidth / iImageHeight)   /* 60 */
               iImageHeight = 50.
      ASSIGN doRad = doRad - 25
             dY = pdf_PageHeight ("Spdf") - doRad + 52. /* 55,50 */
      RUN pdf_place_image ("Spdf",cBildNamn,iLM,dY,iImageWidth,iImageHeight).
      ASSIGN doRad = doRad - 40.
      ASSIGN doRad2 = doRad - 30.
    END.
    ELSE
      doRad = doRad - 60.
          
    RUN pdf_text_xy_dec ("Spdf","But",iLM + 100,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Akt.Pris",iLM + 140,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Inlev",iLM + 220,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Sålt",iLM + 260,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Lag",iLM + 300,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Utf %",iLM + 340,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Förs.Bel",iLM + 400,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Verk%",iLM + 460,doRad + 45).
    RUN pdf_text_xy_dec ("Spdf","Kalk%",iLM + 505,doRad + 45).

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, doRad + 40, pdf_PageWidth("Spdf") - iLM, doRad + 40, 0.5).

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    ASSIGN dY = doRad + 35
           dTotSalg = 0
           dTotKost = 0.
    FOR EACH TT_KjopSalgLager USE-INDEX ArtButTyp WHERE TT_KjopSalgLager.ArtikkelNr = TT_ArtLager.ArtikkelNr NO-LOCK:
      IF lNullager = FALSE AND TT_KjopSalgLager.iLagant = 0 THEN
        NEXT.
      RUN PDFAddVg.
      RUN PDFAddHg.
      RUN PDFAddAvd.
      RUN PDFAddTot.
      IF dY < 60 THEN
      DO:
        ASSIGN iTest = 3.
/* RUN pdf_text_xy_dec ("Spdf","-D-" + STRING(doRad),400,doRad - 20). */
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN dY = doRad.
      END.
      ASSIGN dY = dY - 13
           cWrk = STRING(TT_KjopSalgLager.Butik,"z9")
           iLM2 = iLM + 115.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).             
      ASSIGN cWrk = STRING(TT_ArtLager.AktPris,">>>,>>9.99")
             iLM2 = iLM + 180.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN cWrk = STRING(TT_KjopSalgLager.iKjopAnt,"-zzzz9")
             iLM2 = iLM + 240.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN cWrk = STRING(TT_KjopSalgLager.iAntsolgt,"-zzzz9")
             iLM2 = iLM + 280.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).  
      ASSIGN cWrk = STRING(TT_KjopSalgLager.iLagant,"-zzzz9")
             iLM2 = iLM + 320.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ASSIGN iUtf% = (TT_KjopSalgLager.iAntsolgt / TT_KjopSalgLager.iKjopAnt) * 100
              iLM2 = iLM + 355.
      IF TT_KjopSalgLager.iKjopAnt <> 0 THEN
      DO:
        ASSIGN cWrk = STRING(iUtf%,"-zzzz9").
        RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 345.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dForsBel = TT_KjopSalgLager.VerdiSolgt 
             dTotSalg = dTotSalg + dForsBel
             dTotKost = dTotKost + TT_KjopSalgLager.SVK 
                  iLM2 = iLM + 440
                  cWrk = STRING(dForsBel,"-zzz,zz9.99").
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      IF TT_KjopSalgLager.iAntsolgt <> 0 THEN
      DO:
        ASSIGN dVerk% = ((dForsBel - TT_KjopSalgLager.SVK ) / (dForsBel )) * 100
                 cWrk = STRING(dVerk%,"-zzz9.99")
                 iLM2 = iLM + 490.
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      END.
      ELSE
      DO:
        ASSIGN iLM2 = iLM + 475.
        RUN pdf_text_xy_dec ("Spdf","****",iLM2,dY).
      END.
      ASSIGN dForsBel = (TT_ArtLager.PrisOrdi * 0.8) * TT_KjopSalgLager.iAntSolgt
             dKalk%   =  ((dForsBel - (TT_ArtLager.VVarekost * TT_KjopSalgLager.iAntSolgt)) / dForsBel) * 100
                 iLM2 = iLM + 535
                 cWrk = STRING(dKalk%,"-zzz9.99").
      IF TT_KjopSalgLager.iAntsolgt <> 0 THEN
        RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),dY).
      ELSE
      DO:
        RUN pdf_text_xy_dec ("Spdf","****",iLM2 - 15,dY).
      END.
    END.
    ASSIGN doRad = dY - 2.

    IF doRad <  60 THEN
    DO:
      ASSIGN iTest = 4.
/* RUN pdf_text_xy_dec ("Spdf","-E-" + STRING(doRad),400,doRad - 20). */
      RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      ASSIGN dY = doRad.
    END.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica",8).
    RUN pdf_line IN h_PDFinc  ("Spdf", iLM + 100, doRad, pdf_PageWidth("Spdf") - iLM, doRad, 0.5).

    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
    ASSIGN doRad = doRad - 13.
    RUN pdf_text_xy_dec ("Spdf","Totalt:",iLM + 100,doRad).
    ASSIGN cWrk = STRING(TT_ArtLager.KjopAnt,"-zzzz9")
           iLM2 = iLM + 240.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),doRad).
    ASSIGN cWrk = STRING(TT_ArtLager.Antsolgt,"-zzzz9")
           iLM2 = iLM + 280.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),doRad).  
    ASSIGN cWrk = STRING(TT_ArtLager.Lagant,"-zzzz9")
           iLM2 = iLM + 320.
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),doRad).
    ASSIGN iUtf% = (TT_ArtLager.Antsolgt / TT_ArtLager.KjopAnt) * 100
            cWrk = STRING(iUtf%,"-zzzz9")
            iLM2 = iLM + 355.
    IF TT_ArtLager.KjopAnt <> 0 THEN
    DO:
      RUN pdf_text_xy_dec ("Spdf",cWrk + " %",iLM2 - bredd(cWrk),doRad).
    END.

    ASSIGN iLM2 = iLM + 440
           cWrk = STRING(dTotSalg,"-zzz,zz9.99").
    RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),doRad).

    IF TT_ArtLager.Antsolgt <> 0 THEN
    DO:
      ASSIGN dVerk% = ((dTotSalg - dTotKost) / (dTotSalg)) * 100
               cWrk = STRING(dVerk%,"-zzz9.99")
               iLM2 = iLM + 490.
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),doRad).
      ASSIGN dForsBel = ((TT_ArtLager.PrisOrdi * TT_ArtLager.AntSolgt) * 0.8)
             dKalk%   =  ((dForsBel - (TT_ArtLager.VVarekost * TT_ArtLager.AntSolgt)) / dForsBel) * 100
                 iLM2 = iLM + 535
                 cWrk = STRING(dKalk%,"-zzz9.99").
      RUN pdf_text_xy_dec ("Spdf",cWrk,iLM2 - bredd(cWrk),doRad).
    END.
IF doRad < 115 THEN DO:
    RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
    doRad2 = doRad.
END.
/* Visa storlekar    */
    IF lVisStr = TRUE THEN
    DO:
      IF (doRad - 13) < 60 THEN
      DO:
        ASSIGN iTest = 5.
/* RUN pdf_text_xy_dec ("Spdf","-F-" + STRING(doRad),400,doRad - 20). */
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        ASSIGN dY = doRad.
      END.
      ASSIGN lFirst2 = TRUE.
      DO iind = 1 TO 31:
      ASSIGN iTabTotSalg[iind] = 0
                iTabTotLag[iind] = 0.
      END.
      FOR EACH TT_KjopSalgLager USE-INDEX ArtButTyp WHERE TT_KjopSalgLager.ArtikkelNr = TT_ArtLager.ArtikkelNr NO-LOCK:
        IF lNullager = FALSE AND TT_KjopSalgLager.iLagant = 0 THEN
          NEXT.
        IF lFirst2 = TRUE THEN
        DO:
          IF (doRad - 13) < 70 /* 60 */ THEN
          DO:
            ASSIGN iTest = 6.
/* RUN pdf_text_xy_dec ("Spdf","-G-" + STRING(doRad),400,doRad - 20). */
            RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
          END.

            ASSIGN doRad = doRad - 13
                    iLM2 = iLM + 40.
           IF doRad2 < doRad THEN
               ASSIGN doRad = doRad2.

/*                 IF doRad2 < doRad THEN   */
/*                   ASSIGN doRad = doRad2. */
/*            MESSAGE TT_KjopSalgLager.Storl
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            MESSAGE NUM-ENTRIES(TT_KjopSalgLager.VisEntry,",") TT_KjopSalgLager.VisEntry
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            DO iind = 1 TO NUM-ENTRIES(TT_KjopSalgLager.VisEntry,","):
              IF ENTRY(iind,TT_KjopSalgLager.VisEntry,",") = "1" THEN
              DO:
                ASSIGN iLM2 = iLM2 + 22.
                RUN pdf_text_xy_dec ("Spdf",ENTRY(iind,TT_KjopSalgLager.Storl,","),iLM2,doRad).
              END.
            END.
/* RUN pdf_text_xy_dec ("Spdf","-G2-" + STRING(doRad),400,doRad). */
            ASSIGN cVisEntry = TT_KjopSalgLager.VisEntry
                     lFirst2 = FALSE
                        iLM3 = iLM2 + 22.
        END.
        ASSIGN iTotRadS = 0.
               iTotRadL = 0.
        DO iind = 1 TO NUM-ENTRIES(cVisEntry,","):
          IF ENTRY(iind,cVisEntry,",") = "1" THEN
            ASSIGN cTabSalg[iind] = ENTRY(iind,TT_KjopSalgLager.Antsolgt,",")
                    cTabLag[iind] = ENTRY(iind,TT_KjopSalgLager.Lagant,",")
                         iTotRadS = iTotRadS + INT(ENTRY(iind,TT_KjopSalgLager.Antsolgt,","))
                         iTotRadL = iTotRadL + INT(ENTRY(iind,TT_KjopSalgLager.Lagant,",")).
        END.
        IF (doRad - 13) < 60 THEN
        DO:
           ASSIGN iTest = 7.
/* RUN pdf_text_xy_dec ("Spdf","-H-" + STRING(doRad),400,doRad - 20). */
           RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        END.

         ASSIGN doRad = doRad - 13
                 iLM2 = iLM + 40.
                 cWrk = STRING(TT_KjopSalgLager.Butik,"z9").
        RUN pdf_text_xy_dec ("Spdf",cWrk,(iLM + 15) - bredd(cWrk),doRad).
        RUN pdf_text_xy_dec ("Spdf","Sålt:",iLM + 20,doRad).
        DO iind = 1 TO NUM-ENTRIES(cVisEntry,","):
          IF ENTRY(iind,cVisEntry,",") = "1" THEN
          DO:
            ASSIGN iLM2 = iLM2 + 22.
            RUN pdf_text_xy_dec ("Spdf",cTabSalg[iind],iLM2,doRad).
            IF cTabSalg[iind] <> "" THEN
              ASSIGN iTabTotSalg[iind] = iTabTotSalg[iind] + INT(cTabSalg[iind]).
          END.
        END.
        RUN pdf_text_xy_dec ("Spdf","=" + STRING(iTotRadS),iLM3,doRad).
        IF (doRad - 13) < 60 THEN
        DO:
          ASSIGN iTest = 8.
/* RUN pdf_text_xy_dec ("Spdf","-I-" + STRING(doRad),400,doRad - 20). */
          RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
        END.

        ASSIGN doRad = doRad - 13
                iLM2 = iLM + 40.
        RUN pdf_text_xy_dec ("Spdf","Lager:",iLM + 20,doRad).
        DO iind = 1 TO NUM-ENTRIES(cVisEntry,","):
          IF ENTRY(iind,cVisEntry,",") = "1" THEN
          DO:
            ASSIGN iLM2 = iLM2 + 22.
            RUN pdf_text_xy_dec ("Spdf",cTabLag[iind],iLM2,doRad).
            IF cTabLag[iind] <> "" THEN
              ASSIGN iTabTotLag[iind] = iTabTotLag[iind] + INT(cTabLag[iind]).
          END.
        END.
        RUN pdf_text_xy_dec ("Spdf","=" + STRING(iTotRadL),iLM3,doRad).
        ASSIGN doRad = doRad - 5.
      END.

      IF (doRad - 13) < 60 THEN
      DO:
        ASSIGN iTest = 9.
/* RUN pdf_text_xy_dec ("Spdf","-J-" + STRING(doRad),400,doRad - 20). */
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      END.

      ASSIGN doRad = doRad - 13
              iLM2 = iLM + 40.
      RUN pdf_text_xy_dec ("Spdf","TOT",iLM - 5,doRad).
      RUN pdf_text_xy_dec ("Spdf","Sålt:",iLM + 20,doRad).
      ASSIGN iTotRadS = 0
             iTotRadl = 0.
      DO iind = 1 TO NUM-ENTRIES(cVisEntry,","):
        IF ENTRY(iind,cVisEntry,",") = "1" THEN
        DO:
          ASSIGN iLM2 = iLM2 + 22.
          IF iTabTotSalg[iind] <> 0 THEN
          DO:
            RUN pdf_text_xy_dec ("Spdf",iTabTotSalg[iind],iLM2,doRad).
            ASSIGN iTotRadS = iTotRadS + iTabTotSalg[iind].
          END.
        END.
      END.
      IF iLM3 <= 0 THEN
        ASSIGN iLM3 = 224.
/*        MESSAGE "iLM3-23B " iLM3
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      RUN pdf_text_xy_dec ("Spdf","=" + STRING(iTotRadS),iLM3,doRad).
      IF (doRad - 13) < 60 THEN
      DO:
        ASSIGN iTest = 10.
/* RUN pdf_text_xy_dec ("Spdf","-K-" + STRING(doRad),400,doRad - 20). */
        RUN PDFNysida(INPUT-OUTPUT doRad,INPUT iLM).
      END.

      ASSIGN doRad = doRad - 13
              iLM2 = iLM + 40.
      RUN pdf_text_xy_dec ("Spdf","Lager:",iLM + 20,doRad).
      DO iind = 1 TO NUM-ENTRIES(cVisEntry,","):
        IF ENTRY(iind,cVisEntry,",") = "1" THEN
        DO:
          ASSIGN iLM2 = iLM2 + 22.
          IF iTabTotLag[iind] <> 0 THEN
          DO:
            RUN pdf_text_xy_dec ("Spdf",iTabTotLag[iind],iLM2,doRad).
            ASSIGN iTotRadL = iTotRadL + iTabTotLag[iind].
          END.
        END.
      END.
      IF iLM3 <= 0 THEN
        ASSIGN iLM3 = 224.
/*        MESSAGE "iLM3-24 " iLM3
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      RUN pdf_text_xy_dec ("Spdf","=" + STRING(iTotRadL),iLM3,doRad).
      ASSIGN doRad = doRad - 13.  /* 5  */
    END.
    ELSE
      ASSIGN doRad = doRad - 13.
  END.
