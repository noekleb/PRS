FORM WITH FRAME A.
/*
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

*/

DISPLAY 'ArtLag' WITH FRAME A.
PAUSE 0.
FOR EACH ArtLag:
    DELETE ArtLag.
END.

DISPLAY 'Lager' WITH FRAME A.
PAUSE 0.
FOR EACH Lager:
    DELETE LAger.
END.

FOR EACH Nets:
    DELETE Nets.
END.

DISPLAY 'Sale_Spes' WITH FRAME A.
PAUSE 0.
FOR EACH NON_Sale_Spes:
    DELETE NON_Sale_Spes.
END.

DISPLAY 'PkSdlHode' WITH FRAME A.
PAUSE 0.
FOR EACH PKSdlHode:
    FOR EACH PKSDLLinje OF PKSdlHode:
        DELETE PkSDLLinje.
    END.
    DELETE PKSdlHode.
END.
DISPLAY 'PkSdlPris' WITH FRAME A.
PAUSE 0.
FOR EACH PkSdlPris:
    DELETE PkSdlPris.
END.

DISPLAY 'PkSdlMottak' WITH FRAME A.
PAUSE 0.
FOR EACH PkSdlMottak:
    DELETE PkSdlMottak.
END.

DISPLAY 'FakturaHode' WITH FRAME A.
PAUSE 0.
FOR EACH FakturaHode:
    FOR EACH FakturaLinje OF fakturaHode:
        DELETE FakturaLinje.
    END.
    DELETE FakturaHode.
END.

DISPLAY 'LevLager' WITH FRAME A.
PAUSE 0.
FOR EACH LevLager:
    DELETE LevLAger.
END.
DISPLAY 'Datasett' WITH FRAME A.
PAUSE 0.
FOR EACH DataSett:
    DELETE Datasett.
END.
DISPLAY 'BongHode' WITH FRAME A.
PAUSE 0.
FOR EACH BongHode:
    FOR EACH BongLinje WHERE
        BongLinje.B_Id = BongHode.B_Id:
        DELETE BongLinje.
    END.
    DELETE BongHode.
END.
DISPLAY 'KOrdreHode' WITH FRAME A.
PAUSE 0.
FOR EACH KOrdreHode:
    FOR EACH KOrdreLinje OF KOrdreHode:
        DELETE KOrdreLinje.
    END.
    DELETE KOrdreHode.
END.

DISPLAY 'Gavekort' WITH FRAME A.
PAUSE 0.
FOR EACH Gavekort: DELETE Gavekort. END.
DISPLAY 'Tilgode' WITH FRAME A.
PAUSE 0.
FOR EACH Tilgode: DELETE Tilgode. END.
/*delete from ackforsin.*/
/*delete from akt_mal.*/
DISPLAY 'Akt_Rapp' WITH FRAME A.
PAUSE 0.
FOR EACH Akt_Rapp:
    DELETE Akt_Rapp.
END.
/*delete from andrlog.*/
/*delete from ant-sort.*/
/*delete from Anv-Kod.*/
/*delete from ArtBas.*/
DISPLAY 'ELogg' WITH FRAME A.
PAUSE 0.
FOR EACH ELogg:
    DELETE ELogg.
END.
/*delete from ArtPris.*/
/*delete from Avdeling.*/
DISPLAY 'BatchLogg' WITH FRAME A.
PAUSE 0.
FOR EACH BatchLogg:
    DELETE BatchLogg.
END.
/*delete from Behandlingskode.*/
DO TRANSACTION:
  DELETE FROM BestHLev.
  DELETE FROM BestHode.
  DELETE FROM BestKasse.
  DELETE FROM BestLevert.
  DELETE FROM BestLinje.
  DELETE FROM BestPris.
  DELETE FROM BestSort.
  DELETE FROM BestStr.
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
FOR EACH Fributik:
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
FOR EACH Dags_Rap:
    DELETE Dags_rap.
END.

/*delete from EkstVPIFil.*/
/*delete from EkstVPILev.*/
/*delete from Erstattningsvare.*/
DISPLAY 'Etikett' WITH FRAME A.
PAUSE 0.
FOR EACH Etikett:
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
DISPLAY 'HPrisko' WITH FRAME A.
PAUSE 0.
FOR EACH HPrisKo:
    DELETE HPrisKo.
END.

DISPLAY 'Prisko' WITH FRAME A.
FOR EACH Prisko:
    DELETE PrisKo.
END.

/*delete from HT-Type.*/
/*delete from HuvGr.*/
/*delete from ImpKonv.*/
/*delete from InnerSula.*/
/*delete from Innkjopsgrupper.*/
DISPLAY 'Jobb' WITH FRAME A.
PAUSE 0.
FOR EACH Jobb:
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
FOR EACH Kas_Konter:
    DELETE Kas_Konter.
END.
/*delete from Kasse.*/
DISPLAY 'Kas_Logg' WITH FRAME A.
PAUSE 0.
FOR EACH Kas_Logg:
    DELETE Kas_Logg.
END.
DISPLAY 'Kas_Rap' WITH FRAME A.
PAUSE 0.
FOR EACH Kas_Rap:
    DELETE Kas_Rap.
END.
DISPLAY 'KortSpes' WITH FRAME A.
PAUSE 0.
FOR EACH Kort_Spes:
    DELETE Kort_Spes.
END.

/*delete from KatalogArkiv.*/
/*delete from Kategori.*/
/*delete from klack.*/
/*delete from Kommune.*/
DISPLAY 'Konto' WITH FRAME A.
PAUSE 0.
FOR EACH konto:
    DELETE konto.
END.
/*delete from KontoTabell.*/
DISPLAY 'Kont_Mal' WITH FRAME A.
PAUSE 0.
FOR EACH kont_mal:
    DELETE kont_mal.
END.
/*delete from KonvReg.*/
/*delete from Kravkode.*/
/*delete from Kunde.*/
DISPLAY 'KundeBetTrans' WITH FRAME A.
PAUSE 0.
FOR EACH KundeBetTrans:
    DELETE KundeBetTrans.
END.
/*delete from KundeGruppe.*/
/*delete from KundeKort.*/


DISPLAY 'Kundesaldo' WITH FRAME A.
PAUSE 0.
FOR EACH KundeSaldo:
    DELETE KundeSaldo.
END.
DISPLAY 'Kundetrans' WITH FRAME A.
PAUSE 0.
FOR EACH KundeTrans:
    DELETE KundeTrans.
END.

DISPLAY 'Lister' WITH FRAME A.
PAUSE 0.
FOR EACH Lister:
    DELETE Lister.
END.
DISPLAY 'ListeLinjer' WITH FRAME A.
PAUSE 0.
FOR EACH ListeLinje: 
    DELETE ListeLinje.
END.

DISPLAY 'StLager' WITH FRAME A.
PAUSE 0.
FOR EACH StLAger:
    DELETE StLager.
END.
DISPLAY 'StLinje' WITH FRAME A.
PAUSE 0.
FOR EACH StLinje:
    DELETE StLinje.
END.
DISPLAY 'TelleHode' WITH FRAME A.
PAUSE 0.
FOR EACH TelleHode:
    FOR EACH TelleLinje OF TelleHode:
        DELETE TelleLinje.
    END.
    DELETE TelleHode.
END.

DISPLAY 'Translogg' WITH FRAME A.
PAUSE 0.
FOR EACH Translogg:
    DELETE translogg.
END.
DISPLAY 'Reklamasjonslogg' WITH FRAME A.
PAUSE 0.
FOR EACH ReklamasjonsLinje:
    DELETE ReklamasjonsLinje.
END.
DISPLAY 'Reklamasjonslogg' WITH FRAME A.
PAUSE 0.
FOR EACH ReklamasjonsLogg:
    DELETE Reklamasjonslogg.
END.
DISPLAY 'HT-FilHode' WITH FRAME A.
PAUSE 0.
FOR EACH HT-FilHode:
    FOR EACH HT-FilLinje OF HT-FilHode:
        DELETE HT-FilLinje.
    END.
    DELETE HT-FilHode.
END.

DISPLAY 'KassererOppgj' WITH FRAME A.
PAUSE 0.
FOR EACH KassererOppgj:
    DELETE KassererOppgj.
END.
DISPLAY 'Kassererbilag' WITH FRAME A.
PAUSE 0.
FOR EACH kassererBilag.
    DELETE kassererBilag.
END.
DISPLAY 'Kassererkontanter' WITH FRAME A.
PAUSE 0.
FOR EACH kassererkontanter.
    DELETE kassererkontanter.
END.
DISPLAY 'Kasserervaluta' WITH FRAME A.
PAUSE 0.
FOR EACH kasserervaluta.
    DELETE kasserervaluta.
END.

DISPLAY 'AnalyseLogg' WITH FRAME A.
PAUSE 0.
FOR EACH AnalyseLogg:
    DELETE AnalyseLogg.
END.

DISPLAY 'z_nummer' WITH FRAME A.
PAUSE 0.
FOR EACH z_nummer:
    DELETE z_nummer.
END.
DISPLAY 'Timedag' WITH FRAME A.
PAUSE 0.
FOR EACH timedag:
    DELETE timedag.
END.
DISPLAY 'Varedag' WITH FRAME A.
PAUSE 0.
FOR EACH varedag:
    DELETE varedag.
END.
DISPLAY 'HgrDag' WITH FRAME A.
PAUSE 0.
FOR EACH hgrdag:
    DELETE hgrdag.
END.
DISPLAY 'Bokføringsbilag' WITH FRAME A.
PAUSE 0.
FOR EACH bokforingsbilag:
    DELETE bokforingsbilag.
END.

DISPLAY 'Filer' WITH FRAME A.
PAUSE 0.
FOR EACH Filer:
    FOR EACH Fillinjer OF filer:
        DELETE Fillinjer.
    END.
    FOR EACH FilLogg OF Filer:
        DELETE Fillogg.
    END.

    DELETE Filer.
END.

DISPLAY 'Kundereskontr' WITH FRAME A.
PAUSE 0.
FOR EACH Kundereskontr:
    DELETE Kundereskontr.
END.
DISPLAY 'Kundereskobling' WITH FRAME A.
PAUSE 0.
FOR EACH KundeResKobling:
    DELETE KundeResKobling.
END.
DISPLAY 'Kunde' WITH FRAME A.
PAUSE 0.
FOR EACH Kunde:
    ASSIGN
        Kunde.Kundesaldo = 0
        Kunde.ForsteKjop = ?
        Kunde.SisteKjop  = ?
        .
END.
DISPLAY 'KOrdreHode' WITH FRAME A.
PAUSE 0.
FOR EACH KOrdreHode:
    FOR EACH KOrdreLinje OF KOrdreHode:
        DELETE KOrdreLinje.
    END.
    DELETE KOrdreHode.
END.


DISPLAY 'MedlemSaldo' WITH FRAME A.
PAUSE 0.
FOR EACH MedlemSaldo:
    DELETE MedlemSaldo.
END.

DISPLAY 'MedTrans' WITH FRAME A.
PAUSE 0.
FOR EACH MedTrans:
    DELETE MedTrans.
END.

DISPLAY 'Ordre' WITH FRAME A.
PAUSE 0.
FOR EACH Ordre:
    DELETE Ordre.
END.

DISPLAY 'OvArt' WITH FRAME A.
PAUSE 0.
FOR EACH OVart:
    DELETE OVArt.
END.

DISPLAY 'OvBuffer' WITH FRAME A.
PAUSE 0.
FOR EACH OvBuffer:
    DELETE OvBuffer.
END.

DISPLAY 'OvBunt' WITH FRAME A.
PAUSE 0.
FOR EACH OvBunt:
    DELETE OvBunt.
END.

DISPLAY 'OvLinje' WITH FRAME A.
PAUSE 0.
FOR EACH OvLinje:
    DELETE OvLinje.
END.

DISPLAY 'OvLink' WITH FRAME A.
PAUSE 0.
FOR EACH OvLink:
    DELETE OvLink.
END.

DISPLAY 'OvOrdre' WITH FRAME A.
PAUSE 0.
FOR EACH OvOrdre:
    DELETE OvOrdre.
END.


