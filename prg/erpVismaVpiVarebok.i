&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

            /* Endringskode */
            iCode
            /* Artikkelidentitet og benevning */
            string(Artbas.ArtikkelNr) + TRIM(STRING(Strekkode.StrKode,">999"))
            string(Artbas.ArtikkelNr)
            VarebokLinje.AvdelingNr /*IF AVAILABLE Avdeling THEN Avdeling.AvdelingNr ELSE 0*/
            VarebokLinje.Hg /*IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0*/
            VarebokLinje.Vg /*IF AVAILABLE ArtBas THEN ArtBas.Vg ELSE 0*/
            IF Artbas.LopNr <> ? THEN ArtBas.LopNr ELSE 0     
            VarebokLinje.Sasong /*ArtBas.Sasong*/
            ArtBas.Farg
            ArtBas.Klack
            ArtBas.MatKod
            VarebokLinje.Beskr /*ArtBas.Beskr*/
            VareBokLinje.LevNr /*ArtBas.LevNr*/
            VareBokLinje.LevKod /*ArtBas.LevKod*/
            ArtBas.Tilv-Land
            ArtBas.Kommentar
            ArtBas.ov-id
            ArtBas.last-id
            ArtBas.foder-id
            ArtBas.inner-id
            ArtBas.slit-id
            ArtBas.anv-id
            ArtBas.RabKod
            ArtBas.ProvKod
            ArtBas.valkod
            ArtBas.ny_dato
            ArtBas.inn_dato
            ArtBas.lager
            ArtBas.VMId
            IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ''
            Vareboklinje.LevFargKod /*ArtBas.LevFargKod*/
            replace(cNotat1,";"," ")
            ArtBas.BongTekst
            ArtBas.AnonseArtikkel
            ArtBas.VgKat
            ArtBas.StrTypeID
            VarebokLinje.ProdNr /*ArtBas.ProdNr*/
            IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE ''
            ArtBas.EDato
            ArtBas.ETid
            ArtBas.BrukerID
            ArtBas.RegistrertDato
            ArtBas.RegistrertTid
            ArtBas.RegistrertAv
            ArtBas.BildNr
            ArtBas.AktivDato
            ArtBas.AktivAv
            ArtBas.Storrelser
            ArtBas.LapTop
            VareBokLinje.LevDato1 /*ArtBas.LevDato1*/
            VarebokLinje.LevDato2 /*ArtBas.LevDato2*/
            ArtBas.SattPaKampanje
            ArtBas.OPris
            ArtBas.OLLager
            ArtBas.BildeIKasse
            ArtBas.Pakke
            ArtBas.Alder
            ArtBas.HkStyrt
            ArtBas.LokPris
            ArtBas.IKasse
            ArtBas.HKVareId
            ArtBas.KjentPaHK
            ArtBas.BehKode
            ArtBas.Pakkenr            
            ArtBas.AnbefaltPris
            ArtBas.KundeRabatt
            ArtBas.Etikett
            ArtBas.SalgsEnhet
            ArtBas.Slasket
            ArtBas.SlaskArtikkelNr
            ArtBas.ModellFarge
            ArtBas.SentralBestilling
            ArtBas.PrisGrpNr
            ArtBas.HKArtikkelNr
            ArtBas.HovedModellFarge
            ArtBas.Dato1gSendtHk
            ArtBas.Etikettekst1
            ArtBas.EtiLayout
            ArtBas.LinkVareNr
            ArtBas.Mengde
            ArtBas.ManRabIKas
            ArtBas.ArtSlag
            ArtBas.IndividType
            ArtBas.Pant
            ArtBas.BestForslag
            ArtBas.GarantiKl
            VareBokLinje.LevDato3 /*ArtBas.LevDato3*/
            VarebokLinje.LevDato4 /*ArtBas.LevDato4 */
            cNotat3 /* ArtBas.LinjeMerknad */

            ArtBas.KatalogPris
            ArtBas.forhRab%
            ArtBas.supRab%
            ArtBas.VPIDato
            ArtBas.KjedeVare
            ArtBas.VPIBildeKode
            ArtBas.StrKode1
            ArtBas.StrKode2
            ArtBas.LevVareTekst
            ArtBas.AntIPakn
            ArtBas.FrittTillegg
            replace(cNotat2,";"," ") /* ArtBas.VareFakta */
            ArtBas.Lokasjon
            ArtBas.KonvFaktEtikett
            ArtBas.Gjennomfaktureres
            ArtBas.KjedeRab%
            ArtBas.KjedeInnkPris
            ArtBas.Depositum
            ArtBas.Medlemsutbytte
            '' /*ArtBas.UtvidetSok*/
            ArtBas.HoyLavMva
            ArtBas.Etikettekst2
            ArtBas.WebButikkArtikkel
            ArtBas.RAvdNr
            ArtBas.SanertDato            
            /* Strekkode */
            StrekKode.Kode
            StrekKode.KodeType
            StrekKode.StrKode   
            IF AVAILABLE StrKonv THEN trim(StrKonv.Storl) ELSE ""
            StrekKode.Bestillingsnummer
            StrekKode.IKasse
            StrekKode.HovedNr
            StrekKode.EDato
            StrekKode.ETid
            StrekKode.BrukerID
            StrekKode.RegistrertDato
            StrekKode.RegistrertTid
            StrekKode.RegistrertAv
            /* Pris */
            ArtPris.ProfilNr
            /* Normalpris */
            ArtPris.ValPris[1]
            ArtPris.InnkjopsPris[1]      
            ArtPris.Rab1Kr[1]            
            ArtPris.Rab1%[1]             
            ArtPris.Rab2Kr[1]            
            ArtPris.Rab2%[1]             
            ArtPris.Frakt[1]             
            ArtPris.Frakt%[1]            
            ArtPris.DivKostKr[1]         
            ArtPris.DivKost%[1]          
            ArtPris.Rab3Kr[1]            
            ArtPris.Rab3%[1]             
            ArtPris.DBKr[1]              
            ArtPris.DB%[1]               
            ArtPris.Pris[1]              
            ArtPris.EuroPris[1]
            ArtPris.EuroManuel
            ArtPris.AktivFraDato      
            ArtPris.AktivFraTid 
            ArtPris.TilbudTimestyrt
            ArtPris.VareKost[1]          
            ArtPris.MvaKr[1]             
            ArtPris.Mva%[1] 
            (IF AVAILABLE Moms THEN Moms.Momskod ELSE 0)
            ArtPris.EDato
            ArtPris.ETid
            ArtPris.BrukerID
            ArtPris.RegistrertDato
            ArtPris.RegistrertTid
            ArtPris.RegistrertAv
            /* Varebok */
            VareBokLinje.KjedeVare
            VareBokLinje.Gjennomfaktureres
            (if available STrTStr then StrTStr.SeqNr else 0)
            VareBokLinje.InnkjopsPris
            VareBokLinje.VareKost
            VareBokLinje.forhRab%
            VareBokLinje.supVareKost
            VareBokLinje.supRab%
            VareBokLinje.Pris
            VareBokLinje.AnbefaltPris
            VareBokLinje.KampanjePris
            VareBokLinje.KjedeInnkPris
            VareBokLinje.Sortimentkoder
            VareBokLinje.Kampanjeuker
            VareBokLinje.Kampanjestotte
            Vareboklinje.Lagerkoder
            VareBokLinje.SaSong
            (if available bSasong then bSasong.SasBeskr else '')
            VareBokLinje.VareBokNr
            /* Messeregister. */
            IF AVAILABLE Messe THEN string(Messe.MesseNr) ELSE ''
            IF AVAILABLE Messe THEN replace(MesseBeskrivelse,";"," ") ELSE ''
            IF AVAILABLE Messe THEN string(Messe.FraDato) ELSE ''
            IF AVAILABLE Messe THEN STRING(Messe.TilDato) ELSE ''
            IF AVAILABLE Avdeling THEN Avdeling.AvdelingNavn ELSE ''
            IF AVAILABLE HuvGr    THEN HuvGr.HgBeskr ELSE ''
            IF AVAILABLE VarGr    THEN VarGr.VgBeskr ELSE ''
            Vareboklinje.LevDato1
            Vareboklinje.KjedeValutaPris
            Vareboklinje.KjedeProdusent
            Strekkode.ERPNr
            Vareboklinje.KjedeSupRab%
            Vareboklinje.KjedeSupInnkPris
            ArtBas.EkstStrTypeNavn
            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


