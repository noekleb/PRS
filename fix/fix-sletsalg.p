FORM WITH FRAME A. 
FOR EACH EODKasse:
    DELETE EODKasse.
END.

DISPLAY 'VPIArtBas' WITH FRAME A. 
PAUSE 0.
FOR EACH VPIArtBAs:
    DELETE VPIArtBas.
END.

DISPLAY 'VPIFilHode' WITH FRAME A.
PAUSE 0.
FOR EACH VPIFilHode: DELETE VPIFilHode. END.
DISPLAY 'VPIFilLinje' WITH FRAME A.
PAUSE 0.
FOR EACH VPIFilLinje: DELETE VPIFilLinje. END.
DISPLAY 'VPIFilLogg' WITH FRAME A.
PAUSE 0.
FOR EACH VPIFilLogg: DELETE VPIFilLogg. END.

/*
DISPLAY 'ArtLag' WITH FRAME A.
PAUSE 0.
FOR EACH ArtLag EXCLUSIVE-LOCK:
    DELETE ArtLag.
END.

DISPLAY 'Lager' WITH FRAME A.
PAUSE 0.
FOR EACH Lager EXCLUSIVE-LOCK:
    DELETE LAger.
END.
*/


FOR EACH Nets:
    DELETE Nets.
END.

DISPLAY 'Sale_Spes' WITH FRAME A.
PAUSE 0.
FOR EACH NON_Sale_Spes EXCLUSIVE-LOCK:
    DELETE NON_Sale_Spes.
END.

DISPLAY 'PkSdlPris' WITH FRAME A.
PAUSE 0.
FOR EACH PkSdlPris EXCLUSIVE-LOCK:
    DELETE PkSdlPris.
END.

DISPLAY 'PkSdlMottak' WITH FRAME A.
PAUSE 0.
FOR EACH PkSdlMottak EXCLUSIVE-LOCK:
    DELETE PkSdlMottak.
END.
DISPLAY 'PkSdlHode' WITH FRAME A.
PAUSE 0.
FOR EACH PKSdlHode EXCLUSIVE-LOCK:
    FOR EACH PKSDLLinje OF PKSdlHode EXCLUSIVE-LOCK:
        DELETE PkSDLLinje.
    END.
    DELETE PKSdlHode.
END.

DISPLAY 'FakturaHode' WITH FRAME A.
PAUSE 0.
FOR EACH FakturaHode EXCLUSIVE-LOCK:
    FOR EACH FakturaLinje OF fakturaHode EXCLUSIVE-LOCK:
        DELETE FakturaLinje.
    END.
    DELETE FakturaHode.
END.

DISPLAY 'LevLager' WITH FRAME A.
PAUSE 0.
FOR EACH LevLager EXCLUSIVE-LOCK:
    DELETE LevLAger.
END.
DISPLAY 'Datasett' WITH FRAME A.
PAUSE 0.
FOR EACH DataSett EXCLUSIVE-LOCK:
    DELETE Datasett.
END.
DISPLAY 'BongHode' WITH FRAME A.
PAUSE 0.
FOR EACH BongHode EXCLUSIVE-LOCK:
    FOR EACH BongLinje WHERE
        BongLinje.B_Id = BongHode.B_Id EXCLUSIVE-LOCK:
        DELETE BongLinje.
    END.
    DELETE BongHode.
END.
DISPLAY 'KOrdreHode' WITH FRAME A.
PAUSE 0.
FOR EACH KOrdreHode EXCLUSIVE-LOCK:
    FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK:
        DELETE KOrdreLinje.
    END.
    DELETE KOrdreHode.
END.

DISPLAY 'Gavekort' WITH FRAME A.
PAUSE 0.
FOR EACH Gavekort EXCLUSIVE-LOCK: DELETE Gavekort. END.
DISPLAY 'Tilgode' WITH FRAME A.
PAUSE 0.
FOR EACH Tilgode EXCLUSIVE-LOCK: DELETE Tilgode. END.
/*delete from ackforsin.*/
/*delete from akt_mal.*/
DISPLAY 'Akt_Rapp' WITH FRAME A.
PAUSE 0.
FOR EACH Akt_Rapp EXCLUSIVE-LOCK:
    DELETE Akt_Rapp.
END.
/*delete from andrlog.*/
/*delete from ant-sort.*/
/*delete from Anv-Kod.*/
/*delete from ArtBas.*/
DISPLAY 'ELogg' WITH FRAME A.
PAUSE 0.
FOR EACH ELogg EXCLUSIVE-LOCK:
    DELETE ELogg.
END.
/*delete from ArtPris.*/
/*delete from Avdeling.*/
DISPLAY 'BatchLogg' WITH FRAME A.
PAUSE 0.
FOR EACH BatchLogg EXCLUSIVE-LOCK:
    DELETE BatchLogg.
END.
FOR EACH BestHLev EXCLUSIVE-LOCK:
    DELETE BestHLev.
END.
FOR EACH BestHode EXCLUSIVE-LOCK:
    DELETE BestHode.
END.
FOR EACH BestKasse EXCLUSIVE-LOCK:
    DELETE BestKasse.
END.
FOR EACH BestLevert EXCLUSIVE-LOCK:
    DELETE BestLevert.
END.
FOR EACH BestLinje EXCLUSIVE-LOCK:
    DELETE BestLinje.
END.
FOR EACH BestPris EXCLUSIVE-LOCK:
    DELETE BestPris.
END.
FOR EACH BestSort EXCLUSIVE-LOCK:
    DELETE BestSort.
END.
FOR EACH BestStr EXCLUSIVE-LOCK:
    DELETE BestStr.
END.

DELETE FROM VarebehBestHode.
DELETE FROM VarebehBestLinje.
DELETE FROM VareBeHLinje.
DELETE FROM VAreBeHHode.
DELETE FROM VareBeHPris.
DELETE FROM VAreBeHLinjeTrans.
DELETE FROM VAreBeHLinjeTHode.
DELETE FROM VAreBokHode.
DELETE FROM VareBokLinje.
DELETE FROM VAreBokTemaHode.
DELETE FROM VareBokTemaLinje.

DISPLAY 'FriButikk' WITH FRAME A.
PAUSE 0.
FOR EACH Fributik EXCLUSIVE-LOCK:
    DELETE fributik.
END.
/*delete from BildeData.*/
/*delete from Bilderegister.*/
/*delete from Bruker.*/
/*delete from BrukerGrp.*/
/*delete from Butiker.*/
/*delete from butikkforsalj.*/
/*delete from ButikkKobling.*/
/*delete from ButikkTeam.*/
/*delete from ButikkTilgang.*/
/*delete from dags_lbl.*/
DISPLAY 'Dags_rapp' WITH FRAME A.
PAUSE 0.
FOR EACH Dags_Rap EXCLUSIVE-LOCK:
    DELETE Dags_rap.
END.

/*delete from EkstVPIFil.*/
/*delete from EkstVPILev.*/
/*delete from Erstattningsvare.*/
DISPLAY 'Etikett' WITH FRAME A.
PAUSE 0.
FOR EACH Etikett EXCLUSIVE-LOCK:
    DELETE Etikett.
END.

/*delete from Farg.*/
/*delete from Feilkode.*/
/*delete from FilArkiv.*/
/*delete from foder.*/
/*delete from Forsalj.*/
/*delete from forsin.*/

/*delete from Fylke.*/
/*delete from Gruppe.*/
/*delete from Handtering.*/
/*delete from HjelpMap.*/
/*
DISPLAY 'HPrisko' WITH FRAME A.
PAUSE 0.
FOR EACH HPrisKo EXCLUSIVE-LOCK:
    DELETE HPrisKo.
END.
*/
DISPLAY 'Prisko' WITH FRAME A.
FOR EACH Prisko EXCLUSIVE-LOCK:
    DELETE PrisKo.
END.

/*delete from HT-Type.*/
/*delete from HuvGr.*/
/*delete from ImpKonv.*/
/*delete from InnerSula.*/
/*delete from Innkjopsgrupper.*/
DISPLAY 'Jobb' WITH FRAME A.
PAUSE 0.
FOR EACH Jobb EXCLUSIVE-LOCK:
    DELETE Jobb.
END.
/*
FOR EACH KampanjeHode:
    DELETE KampanjeHode.
END.
FOR EACH KampanjeLinje:
    DELETE KampanjeLinje.
END.
*/
DISPLAY 'Kas_Konter' WITH FRAME A.
PAUSE 0.
FOR EACH Kas_Konter EXCLUSIVE-LOCK:
    DELETE Kas_Konter.
END.
/*delete from Kasse.*/
DISPLAY 'Kas_Logg' WITH FRAME A.
PAUSE 0.
FOR EACH Kas_Logg EXCLUSIVE-LOCK:
    DELETE Kas_Logg.
END.
DISPLAY 'Kas_Rap' WITH FRAME A.
PAUSE 0.
FOR EACH Kas_Rap EXCLUSIVE-LOCK:
    DELETE Kas_Rap.
END.
DISPLAY 'KortSpes' WITH FRAME A.
PAUSE 0.
FOR EACH Kort_Spes EXCLUSIVE-LOCK:
    DELETE Kort_Spes.
END.

/*delete from KatalogArkiv.*/
/*delete from Kategori.*/
/*delete from klack.*/
/*delete from Kommune.*/
DISPLAY 'Konto' WITH FRAME A.
PAUSE 0.
FOR EACH konto EXCLUSIVE-LOCK:
    DELETE konto.
END.
/*delete from KontoTabell.*/
DISPLAY 'Kont_Mal' WITH FRAME A.
PAUSE 0.
FOR EACH kont_mal EXCLUSIVE-LOCK:
    DELETE kont_mal.
END.
/*delete from KonvReg.*/
/*delete from Kravkode.*/
/*delete from Kunde.*/
DISPLAY 'KundeBetTrans' WITH FRAME A.
PAUSE 0.
FOR EACH KundeBetTrans EXCLUSIVE-LOCK:
    DELETE KundeBetTrans.
END.
/*delete from KundeGruppe.*/
/*delete from KundeKort.*/


DISPLAY 'Kundesaldo' WITH FRAME A.
PAUSE 0.
FOR EACH KundeSaldo EXCLUSIVE-LOCK:
    DELETE KundeSaldo.
END.
DISPLAY 'Kundetrans' WITH FRAME A.
PAUSE 0.
FOR EACH KundeTrans EXCLUSIVE-LOCK:
    DELETE KundeTrans.
END.

DISPLAY 'Lister' WITH FRAME A.
PAUSE 0.
FOR EACH Lister EXCLUSIVE-LOCK:
    DELETE Lister.
END.
DISPLAY 'ListeLinjer' WITH FRAME A.
PAUSE 0.
FOR EACH ListeLinje EXCLUSIVE-LOCK: 
    DELETE ListeLinje.
END.

DISPLAY 'StLager' WITH FRAME A.
PAUSE 0.
FOR EACH StLAger EXCLUSIVE-LOCK:
    DELETE StLager.
END.
DISPLAY 'StLinje' WITH FRAME A.
PAUSE 0.
FOR EACH StLinje EXCLUSIVE-LOCK:
    DELETE StLinje.
END.
DISPLAY 'TelleHode' WITH FRAME A.
PAUSE 0.
FOR EACH TelleHode EXCLUSIVE-LOCK:
    FOR EACH TelleLinje OF TelleHode EXCLUSIVE-LOCK:
        DELETE TelleLinje.
    END.
    DELETE TelleHode.
END.

DISPLAY 'Translogg' WITH FRAME A.
PAUSE 0.
FOR EACH Translogg EXCLUSIVE-LOCK:
    DELETE translogg.
END.
DISPLAY 'Reklamasjonslogg' WITH FRAME A.
PAUSE 0.
FOR EACH ReklamasjonsLinje EXCLUSIVE-LOCK:
    DELETE ReklamasjonsLinje.
END.
DISPLAY 'Reklamasjonslogg' WITH FRAME A.
PAUSE 0.
FOR EACH ReklamasjonsLogg EXCLUSIVE-LOCK:
    DELETE Reklamasjonslogg.
END.
DISPLAY 'HT-FilHode' WITH FRAME A.
PAUSE 0.
FOR EACH HT-FilHode EXCLUSIVE-LOCK:
    FOR EACH HT-FilLinje OF HT-FilHode EXCLUSIVE-LOCK:
        DELETE HT-FilLinje.
    END.
    DELETE HT-FilHode.
END.

DISPLAY 'KassererOppgj' WITH FRAME A.
PAUSE 0.
FOR EACH KassererOppgj EXCLUSIVE-LOCK:
    DELETE KassererOppgj.
END.
DISPLAY 'Kassererbilag' WITH FRAME A.
PAUSE 0.
FOR EACH kassererBilag EXCLUSIVE-LOCK.
    DELETE kassererBilag.
END.
DISPLAY 'Kassererkontanter' WITH FRAME A.
PAUSE 0.
FOR EACH kassererkontanter EXCLUSIVE-LOCK.
    DELETE kassererkontanter.
END.
DISPLAY 'Kasserervaluta' WITH FRAME A.
PAUSE 0.
FOR EACH kasserervaluta EXCLUSIVE-LOCK.
    DELETE kasserervaluta.
END.

DISPLAY 'AnalyseLogg' WITH FRAME A.
PAUSE 0.
FOR EACH AnalyseLogg EXCLUSIVE-LOCK:
    DELETE AnalyseLogg.
END.

DISPLAY 'z_nummer' WITH FRAME A.
PAUSE 0.
FOR EACH z_nummer EXCLUSIVE-LOCK:
    DELETE z_nummer.
END.
DISPLAY 'Timedag' WITH FRAME A.
PAUSE 0.
FOR EACH timedag EXCLUSIVE-LOCK:
    DELETE timedag.
END.
DISPLAY 'Varedag' WITH FRAME A.
PAUSE 0.
FOR EACH varedag EXCLUSIVE-LOCK:
    DELETE varedag.
END.
DISPLAY 'HgrDag' WITH FRAME A.
PAUSE 0.
FOR EACH hgrdag EXCLUSIVE-LOCK:
    DELETE hgrdag.
END.
DISPLAY 'Bokføringsbilag' WITH FRAME A.
PAUSE 0.
FOR EACH bokforingsbilag EXCLUSIVE-LOCK:
    DELETE bokforingsbilag.
END.
FOR EACH bokforingsKorrbilag EXCLUSIVE-LOCK:
    DELETE bokforingsKorrbilag.
END.
FOR EACH bokforingsVisning EXCLUSIVE-LOCK:
    DELETE bokforingsVisning.
END.

DISPLAY 'Filer' WITH FRAME A.
PAUSE 0.
FOR EACH Filer EXCLUSIVE-LOCK:
    FOR EACH Fillinjer OF filer EXCLUSIVE-LOCK:
        DELETE Fillinjer.
    END.
    FOR EACH FilLogg OF Filer EXCLUSIVE-LOCK:
        DELETE Fillogg.
    END.

    DELETE Filer.
END.

DISPLAY 'Kundereskontr' WITH FRAME A.
PAUSE 0.
FOR EACH Kundereskontr EXCLUSIVE-LOCK:
    DELETE Kundereskontr.
END.
DISPLAY 'Kundereskobling' WITH FRAME A.
PAUSE 0.
FOR EACH KundeResKobling EXCLUSIVE-LOCK:
    DELETE KundeResKobling.
END.
DISPLAY 'Kunde' WITH FRAME A.
PAUSE 0.
FOR EACH Kunde EXCLUSIVE-LOCK:
    ASSIGN
        Kunde.Kundesaldo = 0
        Kunde.ForsteKjop = ?
        Kunde.SisteKjop  = ?
        .
END.
DISPLAY 'KOrdreHode' WITH FRAME A.
PAUSE 0.
FOR EACH KOrdreHode EXCLUSIVE-LOCK:
    FOR EACH KOrdreLinje OF KOrdreHode EXCLUSIVE-LOCK:
        DELETE KOrdreLinje.
    END.
    DELETE KOrdreHode.
END.


DISPLAY 'MedlemSaldo' WITH FRAME A.
PAUSE 0.
FOR EACH MedlemSaldo EXCLUSIVE-LOCK:
    DELETE MedlemSaldo.
END.

DISPLAY 'MedTrans' WITH FRAME A.
PAUSE 0.
FOR EACH MedTrans EXCLUSIVE-LOCK:
    DELETE MedTrans.
END.

DISPLAY 'Ordre' WITH FRAME A.
PAUSE 0.
FOR EACH Ordre EXCLUSIVE-LOCK:
    DELETE Ordre.
END.

DISPLAY 'OvArt' WITH FRAME A.
PAUSE 0.
FOR EACH OVart EXCLUSIVE-LOCK:
    DELETE OVArt.
END.

DISPLAY 'OvBuffer' WITH FRAME A.
PAUSE 0.
FOR EACH OvBuffer EXCLUSIVE-LOCK:
    DELETE OvBuffer.
END.

DISPLAY 'OvBunt' WITH FRAME A.
PAUSE 0.
FOR EACH OvBunt EXCLUSIVE-LOCK:
    DELETE OvBunt.
END.

DISPLAY 'OvLinje' WITH FRAME A.
PAUSE 0.
FOR EACH OvLinje EXCLUSIVE-LOCK:
    DELETE OvLinje.
END.

DISPLAY 'OvLink' WITH FRAME A.
PAUSE 0.
FOR EACH OvLink EXCLUSIVE-LOCK:
    DELETE OvLink.
END.

DISPLAY 'OvOrdre' WITH FRAME A.
PAUSE 0.
FOR EACH OvOrdre EXCLUSIVE-LOCK:
    DELETE OvOrdre.
END.


