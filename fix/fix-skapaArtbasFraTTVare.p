/* /* ALLA TT_Vare: Skapa strekkode */                                   */
DEFINE BUFFER bArtbas FOR Artbas.
/* FOR EACH TT_Vare:                                                     */
/*     CREATE Strekkode.                                                 */
/*     ASSIGN StrekKode.Kode       = STRING(TT_Vare.Ean,"9999999999999") */
/*            StrekKode.ArtikkelNr = TT_Vare.Modell                      */
/*            StrekKode.StrKode    = TT_Vare.Storrelsesnr                */
/*            StrekKode.KodeType   = 1                                   */
/*            StrekKode.VareId     = TT_Vare.Modell                      */
/*            StrekKode.HovedNr    = TT_Vare.Storrelsesnr = 0.           */
/*            StrekKode.IKasse     = TRUE.                               */
/*     RELEASE strekkode.                                                */
/* END.                                                                  */
FOR EACH TT_vare NO-LOCK BREAK BY modell:
  IF FIRST-OF(modell) THEN DO:
      FIND VarGr WHERE VarGr.Vg = TT_Vare.Hg NO-LOCK.
      FIND LAST bArtbas WHERE bArtbas.Vg = TT_Vare.hgr USE-INDEX vglopnr NO-LOCK NO-ERROR.
     CREATE Artbas.
     ASSIGN
            ArtBas.AktivDato      = TODAY
            ArtBas.Aktivert       = TRUE
            ArtBas.ArtikkelNr     = TT_Vare.modell
            ArtBas.Beskr          = TT_Vare.varetekst
            ArtBas.BongTekst      = TT_Vare.bong
            ArtBas.Farg           = TT_Vare.Fargenr
            ArtBas.Hg             = Vargr.hg
            ArtBas.IKasse         = TRUE
            ArtBas.KjentPaHK      = TT_vare.modell > 9000000
            ArtBas.KundeRabatt    = TT_Vare.krabatt
            ArtBas.LevKod         = TT_Vare.Bestnr2
            ArtBas.LevNr          = TT_Vare.Levnr
            ArtBas.LopNr          = IF AVAIL bArtBas THEN bArtbas.lopnr + 1 ELSE 1
            ArtBas.OLLager        = TRUE
            ArtBas.OPris          = FALSE
            ArtBas.SalgsEnhet     = "Stk"
            ArtBas.Storrelser     = TRUE
            ArtBas.StrTypeID      = IF tt_vare.storrelsestype = 0 THEN 2 ELSE tt_vare.storrelsestype
            ArtBas.Vg             = TT_Vare.hgr
            ArtBas.VgKat          = 1.
/*          ArtBas.AktivAv */
/*             ArtBas.Alder */
/*             ArtBas.AnbefaltPris */
/*             ArtBas.AnonseArtikkel */
/*             ArtBas.anv-id */
/*             ArtBas.BehKode */
/*             ArtBas.BildeIKasse    = TRUE */
/*             ArtBas.BildNr */
/*             ArtBas.BrukerID */
/*             ArtBas.Dato1gSendtHk */
/*             ArtBas.DivInfo */
/*             ArtBas.EDato   */
/*             ArtBas.ETid    */
/*             ArtBas.Etikett */
/*             ArtBas.Etikettekst1 */
/*             ArtBas.EtiLayout */
/*             ArtBas.foder-id */
/*             ArtBas.HKArtikkelNr */
/*             ArtBas.HkStyrt      */
/*             ArtBas.HKVareId     */
/*             ArtBas.HovedModellFarge */
/*             ArtBas.inner-id */
/*             ArtBas.inn_dato */
/*             ArtBas.Klack     */
/*             ArtBas.Kommentar */
/*             ArtBas.lager          = TRUE */
/*             ArtBas.LapTop */
/*             ArtBas.last-id  */
/*             ArtBas.LevDato1 */
/*             ArtBas.LevDato2 */
/*             ArtBas.LevFargKod */
/*             ArtBas.LokPris */
/*             ArtBas.MatKod      */
/*             ArtBas.ModellFarge */
/*             ArtBas.Notat       */
/*             ArtBas.ny_dato */
/*             ArtBas.ov-id */
/*             ArtBas.Pakke   */
/*             ArtBas.Pakkenr */
/*             ArtBas.PrisGrpNr */
/*             ArtBas.ProdNr  */
/*             ArtBas.ProvKod */
/*             ArtBas.RabKod  */
/*             ArtBas.RegistrertAv   */
/*             ArtBas.RegistrertDato */
/*             ArtBas.RegistrertTid  */
/*             ArtBas.SaSong */
/*             ArtBas.SattPaKampanje */
/*             ArtBas.SentralBestilling */
/*             ArtBas.SlaskArtikkelNr   */
/*             ArtBas.Slasket */
/*             ArtBas.slit-id */
/*             ArtBas.Tilv-Land */
/*             ArtBas.valkod    */
/*             ArtBas.VisDivInfo */
/*             ArtBas.VMId       */
    END.
END.

