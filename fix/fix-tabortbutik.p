DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.

ASSIGN iButik = 3.

FOR EACH akt_rapp WHERE akt_rapp.butik = iButik:
    DELETE Akt_rapp.
END.
FOR EACH ApnSkjema WHERE ApnSkjema.ButikkNr = iButik:
    DELETE ApnSkjema.
END.
FOR EACH ArtLag WHERE ArtLag.butik = iButik:
    DELETE ArtLag.
END.
FOR EACH ArtLok WHERE ArtLok.ButikkNr = iButik:
    DELETE ArtLok.
END.
FOR EACH BestHLev:
    DELETE BestHLev.
END.
FOR EACH BestHode:
    DELETE BestHode.
END.
FOR EACH BestKasse:
    DELETE BestKasse.
END.
FOR EACH BestLevert:
    DELETE BestLevert.
END.
FOR EACH BestLinje:
    DELETE BestLinje.
END.
FOR EACH BokforingsBilag WHERE BokforingsBilag.ButikkNr = iButik:
    DELETE BokforingsBilag.
END.
FOR EACH BestPris:
    DELETE BestPris.
END.
FOR EACH BestSort:
    DELETE BestSort.
END.
FOR EACH BestStr:
    DELETE BestStr.
END.
FOR EACH FriButik:
    DELETE Fributik.
END.

FOR EACH butikkforsalj WHERE butikkforsalj.Butik = iButik:
    DELETE butikkforsalj.
END.

FOR EACH ButikkSelger WHERE ButikkSelger.ButikkNr = iButik:
    DELETE ButikkSelger. 
END.
FOR EACH ButLok WHERE ButLok.ButikkNr = iButik:
    DELETE ButLok. 
END.

FOR EACH dags_rap WHERE dags_rap.butikk = iButik:
    DELETE dags_rap.
END.
FOR EACH etikett WHERE etikett.butik = iButik:
    DELETE etikett. 
END.
FOR EACH Gavekort WHERE Gavekort.ButikkNr = iButik:
    DELETE Gavekort. 
END.

FOR EACH KassererBilag WHERE KassererBilag.ButikkNr = iButik:
    DELETE KassererBilag.
END.
FOR EACH KassererDag WHERE KassererDag.ButikkNr = iButik:
    DELETE KassererDag.
END.
FOR EACH KassererKontanter WHERE KassererKontanter.ButikkNr = iButik:
    DELETE KassererKontanter.  
END.
FOR EACH KassererOppgj WHERE KassererOppgj.ButikkNr = iButik:
    DELETE KassererOppgj.
END.
FOR EACH KassererValuta WHERE KassererValuta.ButikkNr = iButik:
    DELETE KassererValuta.  
END.
FOR EACH kas_konter WHERE kas_konter.butikk = iButik:
    DELETE kas_konter.
END.
FOR EACH kas_logg WHERE kas_logg.butikk = iButik:
    DELETE kas_logg.  
END.
FOR EACH kas_rap WHERE kas_rap.butikk = iButik:
    DELETE kas_rap.  
END.


FOR EACH konto WHERE konto.butikk = iButik:
    DELETE konto.  
END.
FOR EACH kont_mal WHERE kont_mal.butikk = iButik:
    DELETE kont_mal.
END.
FOR EACH Kort_Spes WHERE Kort_Spes.butikk = iButik:
    DELETE Kort_Spes.
END.
FOR EACH Kunde WHERE Kunde.ButikkNr = iButik:
    DELETE Kunde.  
END.
FOR EACH KundeBetTrans WHERE KundeBetTrans.betButik = iButik:
    DELETE KundeBetTrans.  
END.
FOR EACH KundeBetTrans WHERE KundeBetTrans.Butik = iButik:
    DELETE KundeBetTrans.  
END.
FOR EACH KundeSaldo WHERE KundeSaldo.ButikkNr = iButik:
    DELETE KundeSaldo.  
END.
FOR EACH KundeTrans WHERE KundeTrans.Butik = iButik:
    DELETE KundeTrans.  
END.
FOR EACH Lager WHERE Lager.Butik = iButik:
    DELETE Lager.  
END.
FOR EACH Medlem WHERE Medlem.ButikkNr = iButik:
    DELETE Medlem.  
END.
FOR EACH MedlemBetTrans WHERE MedlemBetTrans.betButik = iButik:
    DELETE MedlemBetTrans.  
END.
FOR EACH MedlemBetTrans WHERE MedlemBetTrans.Butik = iButik:
    DELETE MedlemBetTrans.  
END.
FOR EACH MedlemSaldo WHERE MedlemSaldo.ButikkNr = iButik:
    DELETE MedlemSaldo.
END.
FOR EACH MedTrans WHERE MedTrans.Butik = iButik:
    DELETE MedTrans.
END.
FOR EACH OvBuffer WHERE OvBuffer.ButikkNrFra = iButik:
    DELETE OvBuffer.  
END.
FOR EACH OvBuffer WHERE OvBuffer.ButikkNrTil = iButik:
    DELETE OvBuffer.
END.
FOR EACH OvLinje WHERE OvLinje.FraButik = iButik:
    DELETE OvLinje.  
END.
FOR EACH OvLinje WHERE OvLinje.TilButik = iButik:
    DELETE OvLinje.
END.
FOR EACH OvOrdre WHERE OvOrdre.FraButik = iButik:
    DELETE OvOrdre. 
END.
FOR EACH OvOrdre WHERE OvOrdre.TilButik = iButik:
    DELETE OvOrdre.
END.
FOR EACH ReklamasjonsLinje WHERE ReklamasjonsLinje.Butik = iButik:
    DELETE ReklamasjonsLinje.
END.
FOR EACH StLager WHERE StLager.Butik = iButik:
    DELETE StLager.
END.
FOR EACH StLinje WHERE StLinje.Butik = iButik:
    DELETE StLinje.
END.

FOR EACH Tilgode WHERE Tilgode.ButikkNr = iButik:
    DELETE Tilgode.
END.
FOR EACH Tilgode WHERE Tilgode.FraButikkNr = iButik:
    DELETE Tilgode.
END.
FOR EACH TransLogg WHERE TransLogg.Butik = iButik:
    DELETE TransLogg.  
END.
FOR EACH z_nummer WHERE z_nummer.butikk = iButik:
    DELETE z_nummer.
END.
/* FOR EACH AnalyseLogg WHERE AnalyseLogg.ButikkNr = iButik: */
/*     DELETE AnalyseLogg.                              */
/* END.                                                 */
/* FOR EACH BongHode WHERE BongHode.ButikkNr = iButik:       */
/*     DELETE BongHode.                                 */
/* END.                                                 */
/* FOR EACH BongLinje WHERE BongLinje.ButikkNr = iButik:     */
/*     DELETE BongLinje.                                */
/* END.                                                 */
/* FOR EACH BongLinje WHERE BongLinje.MButikkNr = iButik:    */
/*     DELETE BongLinje.                                */
/* END.                                                 */
/* FOR EACH BongLinje WHERE BongLinje.ReturButikk = iButik:  */
/*     DELETE BongLinje.                                */
/* END.                                                 */
/* FOR EACH Datasett WHERE Datasett.ButikkNr = iButik:       */
/*     DELETE Datasett.                                 */
/* END.                                                 */
/* FOR EACH hgrdag WHERE hgrdag.butnr = iButik:              */
/*     DELETE hgrdag.                                   */
/* END.                                                 */
/* FOR EACH timedag WHERE timedag.butnr = iButik:            */
/*     DELETE timedag.                                  */
/* END.                                                 */
/* FOR EACH varedag WHERE varedag.butnr = iButik:            */
/*     DELETE varedag.                                  */
/* END.                                                 */
/* manuellt */
/* FOR EACH ButikkKobling WHERE ButikkKobling.Butik = iButik: */
/*     DELETE ButikkKobling.                             */
/* END.                                                  */
/* FOR EACH ButikkTilgang WHERE ButikkTilgang.Butik = iButik: */
/*     DELETE ButikkTilgang.                             */
/* END.                                                  */
/* slut manuellt */
FOR EACH Gruppe WHERE Gruppe.ButikkNr = iButik:
    DELETE Gruppe.
END.
FOR EACH Kasse WHERE Kasse.ButikkNr = iButik:
    DELETE Kasse.
END.
/* FIND butiker WHERE butiker.butik = iButik. */
/* DELETE butiker.                            */
