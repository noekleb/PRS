DEF VAR iAntall AS INT NO-UNDO.
DEF VAR iTotalt AS INT NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR wPris   AS DEC NO-UNDO.
DEF VAR wDb     AS DEC NO-UNDO.
DEF VAR wVVkost AS DEC NO-UNDO.

FORM
    cTekst FORMAT "x(70)"
    WITH FRAME g.

PAUSE 0.
cTekst = "Teller linjer...".
DISPLAY cTekst WITH FRAME g.

/* FOR EACH BongHode NO-LOCK:  */
/*   ASSIGN                    */
/*       iTotalt = iTotalt + 1 */
/*       .                     */
/* END.                        */

PAUSE 0.
cTekst = "Leser bonglinjer...".
display cTekst WITH FRAME g.

BONGHODE:
FOR EACH BongHode NO-LOCK:
    ASSIGN
      iAntall = iAntall + 1
      .
/*   cTekst = "Butikk/BongHode:" + string(BongHode.butikkNr) + "/" + STRING(BongHode.BongNr). */
/*   PAUSE 0.                                                                                 */
/*   DISPLAY cTekst WITH FRAME g.                                                             */
  
  BONGLINJE:
  FOR EACH BongLinje WHERE
    BongLinje.B_Id = BongHode.B_Id:
      
   /* Skipper bonglinjer vi ikke skal ha. */
   IF BongLinje.ArtikkelNr <> "" THEN 
       NEXT BONGLINJE.
   IF NOT CAN-DO("001,002,003,010,011,012",STRING(BongLinje.TTID,"999")) THEN
       NEXT BONGLINJE.

    IF iAntall MODULO 500 = 0 THEN
    DO:
        PAUSE 0.
        cTekst = "Behandler bong " + STRING(iAntall) + " av " + 
                STRING(iTotalt) + ". " +
                "Butikk/BongHode:" + string(BongHode.butikkNr) + "/" + STRING(BongHode.BongNr) + ".".
        DISPLAY cTekst  WITH FRAME g.
    END.

    FIND VarGr NO-LOCK WHERE
        VarGr.Vg = Bonglinje.VareGr NO-ERROR.
    IF NOT AVAILABLE VarGr THEN
        FIND VarGr NO-LOCK WHERE
        VarGr.Vg = 1499 NO-ERROR.
    IF AVAILABLE VarGr THEN
    VARGR:
    DO:
      FIND FIRST ArtBas NO-LOCK WHERE
          ArtBas.Vg = BongLinje.VareGr and
          ArtBas.OPris = TRUE NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          FIND FIRST ArtBas NO-LOCK WHERE
              ArtBas.Vg = 1499 and
              ArtBas.OPris = TRUE NO-ERROR.

      ASSIGN
          wPris   = (BongLinje.LinjeSum -                  
                     BongLinje.MvaKr -                
                     BongLinje.SubTotalRab -          
                     BongLinje.LinjeRab)
          wvvKost = ((BongLinje.LinjeSum -                  
                     BongLinje.MvaKr -                
                     BongLinje.SubTotalRab -          
                     BongLinje.LinjeRab) * VarGr.Kost_Proc) / 100
          wDb     = wPris - wVVKost

            BongLinje.VVareKost  = wVVKost
            BongLinje.ArtikkelNr = IF BongLinje.ArtikkelNr = ""
                                     THEN STRING(ArtBas.ArtikkelNr)
                                     ELSE bongLinje.ArtikkelNr
            .

/* MESSAGE  "ArtBas:" ArtBAs.ArtikkelNr        */
/*                    artBAs.Vg                */
/*                    ArtBas.Beskr             */
/*                    ArtBas.OPris SKIP(1)     */
/*     "BongTekst:" BongLinje.BongTekst skip   */
/*     "ArtikkelNr:" BongLinje.ArtikkelNr skip */
/*     "VVareKost:" bongLinje.VVareKost SKIP   */
/*     "NyVVAreKost:" wvvKost SKIP             */
/*     "Mva:" BongLinje.MvaKr SKIP             */
/*     "Pris:" STRING(BongLinje.LinjeSum) SKIP */
/*     "DB%:" (wDb * 100) / wPris SKIP         */
/*     "Utregnet pris:"  wPris wDb             */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.      */

    END. /* VARGR */
  END. /* BONGLINJE */
  
END. /* BONGHODE */

