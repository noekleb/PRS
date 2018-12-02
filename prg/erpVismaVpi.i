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
            IF AVAILABLE StrKonv THEN trim(StrKonv.Storl) ELSE ""
            Artbas.LopNr             
            Artbas.Beskr             
            Artbas.BongTekst
    
            /* Leverandørinformasjon */
            Artbas.LevNr             
            IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE ""
            Artbas.LevKod            
            Artbas.LevFargKod    
            Artbas.LevVareTekst      
    
            /* Varestruktur */
              /* Avdeling */
              IF AVAILABLE Avdeling THEN Avdeling.AvdelingNr ELSE 0
              Avdeling.AvdelingNavn WHEN AVAILABLE Avdeling
    
              /* Hovedgruppe */
              IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0
              IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE ""
    
              /* Varegruppe */
              IF AVAILABLE ArtBAs THEN ArtBas.Vg ELSE 0
              IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ""
              IF AVAILABLE VarGr THEN VarGr.MomsKod ELSE 0
              IF AVAILABLE VarGr THEN VarGr.Kost_Proc ELSE 0
    
            /* Artikkeltype */
            Artbas.ArtSlag           
            Artbas.IndividType       
            Artbas.OPris             
            Artbas.lager             
            Artbas.Storrelser        
            Artbas.StrTypeID   
            ArtBas.KjedeRab%
            ArtBas.KjedeInnkPris
            ArtBas.Gjennomfaktureres
    
            /* Strekkode og størrelse */
            Strekkode.Kode              
            Strekkode.StrKode           
            Strekkode.KodeType          
            Strekkode.HovedNr           
            Strekkode.IKasse            
            IF AVAILABLE StrKonv THEN trim(StrKonv.Storl) ELSE ""             
            (IF trim(Strekkode.Bestillingsnummer) = "" 
                THEN ArtBas.LevKod
                ELSE Strekkode.Bestillingsnummer)

            /* Koblede registre */
            Artbas.VgKat             
            Artbas.GarantiKl         
            Artbas.ProdNr 
            IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE ""
            Artbas.VMId    
            IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ""
            Artbas.SaSong            
            IF AVAILABLE Sasong THEN Sasong.SasBeskr ELSE ""
            Artbas.Farg              
            IF AVAILABLE Farg THEN Farg.FarBeskr ELSE ""
            Artbas.MatKod            
            IF AVAILABLE Material THEN Material.MatBeskr ELSE ""
            Artbas.valkod            
            IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 0
    
            /* Leverandørbetingelser */
            Artbas.LevDato1          
            Artbas.LevDato2          
            Artbas.LevDato3          
            Artbas.LevDato4          
            cNotat3 /*Artbas.LinjeMerknad */
            Artbas.KatalogPris       
            Artbas.forhRab%          
            Artbas.supRab%           
            Artbas.VPIDato           
            Artbas.KjedeVare         
            Artbas.VPIBildeKode      
            Artbas.AntIPakn          
            Artbas.FrittTillegg      
            cNotat2 /*Artbas.VareFakta */
            Artbas.StrKode1          
            Artbas.StrKode2          
    
            /* Sist endret */
            Artbas.EDato             
            string(Artbas.ETid,"HH:MM:SS")              
            Artbas.BrukerID          
    
            /* opprettet */
            Artbas.RegistrertDato    
            string(Artbas.RegistrertTid,"HH:MM:SS")     
            Artbas.RegistrertAv      
            
            /* Diverse artikkelinformasjon */
            Artbas.ny_dato           
            Artbas.inn_dato          
            cNotat1 /*Artbas.Notat */
            Artbas.AnonseArtikkel    
            Artbas.AktivDato         
            Artbas.AktivAv           
            Artbas.Aktivert          
            Artbas.SattPaKampanje    
            Artbas.OLLager           
            Artbas.BildeIKasse       
            Artbas.Pakke             
            Artbas.Alder             
            Artbas.HkStyrt           
            Artbas.LokPris           
            Artbas.IKasse            
            Artbas.KjentPaHK         
            Artbas.BehKode           
            Artbas.Pakkenr           
            Artbas.AnbefaltPris      
            Artbas.KundeRabatt       
            Artbas.Etikett           
            Artbas.SalgsEnhet        
            Artbas.ModellFarge       
            Artbas.SentralBestilling 
            Artbas.PrisGrpNr         
            Artbas.HovedModellFarge  
            Artbas.Etikettekst1      
            Artbas.EtiLayout         
            Artbas.LinkVareNr        
            Artbas.Mengde            
            Artbas.ManRabIKas        
            Artbas.Kommentar         
    
            /* Priser */
            ArtPris.Tilbud            
            ArtPris.AktivFraDato      
            ArtPris.AktivFraTid       
            ArtPris.TilbudFraDato     
            ArtPris.TilbudTilDato     
            ArtPris.TilbudFraTid      
            ArtPris.TilbudTilTid      
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
            ArtPris.VareKost[1]          
            ArtPris.MvaKr[1]             
            ArtPris.Mva%[1]              
            ArtPris.DBKr[1]              
            ArtPris.DB%[1]               
            ArtPris.Pris[1]              
            /* Tilbudspris */
            ArtPris.ValPris[2]           
            ArtPris.InnkjopsPris[2]      
            ArtPris.Rab1Kr[2]            
            ArtPris.Rab1%[2]             
            ArtPris.Rab2Kr[2]            
            ArtPris.Rab2%[2]             
            ArtPris.Frakt[2]             
            ArtPris.Frakt%[2]            
            ArtPris.DivKostKr[2]         
            ArtPris.DivKost%[2]          
            ArtPris.Rab3Kr[2]            
            ArtPris.Rab3%[2]             
            ArtPris.VareKost[2]          
            ArtPris.MvaKr[2]             
            ArtPris.Mva%[2]              
            ArtPris.DBKr[2]              
            ArtPris.DB%[2]               
            ArtPris.Pris[2]  
            /* Nye felt */
            ArtBas.RAvdNr
            ArtBas.KjedeValutaPris
            ArtBas.KjedeProdusent
            Strekkode.ERPNr
            ArtBas.KjedeSupRab%
            ArtBas.KjedeSupInnkPris
            ArtBas.EkstStrTypeNavn
            .
            /* Skal ikke overføres */
            /*
            Artbas.HKArtikkelNr      
            Artbas.Klack             
            Artbas.Tilv-Land         
            LapTop            logi         i     Ja/Nei
            Dato1gSendtHk     date               99/99/99
            HKVareId          inte         i     >>>>>>9
            Slasket           logi         i     yes/no
            SlaskArtikkelNr   deci-2       i     >>>>>>>>>>>>9
            BildNr            inte         i     >>>>>>9
            ov-id             inte               z9
            last-id           inte               z9
            foder-id          inte               >9
            inner-id          inte               z9
            slit-id           inte               z9
            anv-id            inte               z9
            RabKod            inte               z9
            ProvKod           inte               z9
            Pant              logi         i     yes/no
            BestForslag       logi         i     yes/no
            DivInfo           char[20]           X(30)
            VisDivInfo        logi[20]           yes/no
            */

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


