&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
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

DEFINE OUTPUT PARAMETER ocRetur AS CHARACTER  NO-UNDO.
DEFINE VAR oc2Retur AS CHARACTER  NO-UNDO.

DEF VAR cTmpFilNavn AS CHAR NO-UNDO.
DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR cGetFilnavn AS CHAR NO-UNDO.
DEF VAR iAntEksport AS INTEGER    NO-UNDO.
DEF VAR iCl      AS INT  NO-UNDO.
DEF VAR iKode       AS INT INITIAL 1 NO-UNDO.
DEF VAR iStrKode     AS INT  NO-UNDO.
DEF VAR cMottager AS CHAR NO-UNDO.
DEF VAR piLoop    AS INT NO-UNDO.
DEF VAR iTotAntEksport AS INT NO-UNDO.

DEFINE VARIABLE cLevAdresse1 LIKE Ordre.LevAdresse1 NO-UNDO.
DEFINE VARIABLE cLevAdresse2 LIKE Ordre.LevAdresse2 NO-UNDO.
DEFINE VARIABLE cLevPostNr   LIKE Ordre.LevPostNr   NO-UNDO.
DEFINE VARIABLE cLevPostBoks LIKE Ordre.LevPostBoks NO-UNDO.
DEFINE VARIABLE cLevTelefon LIKE Ordre.LevTelefon   NO-UNDO.
DEFINE VARIABLE cLevKontakt LIKE Ordre.LevKontakt   NO-UNDO.

DEFINE TEMP-TABLE TT_ELogg NO-UNDO LIKE ELogg
    FIELD VismaKlient AS INT. /* 1=Visma Karlsson klient, 2=Visma Sport1 klient */
DEFINE BUFFER   bTT_Elogg FOR TT_Elogg.

DEF STREAM Ut.

DEF TEMP-TABLE tt_BestStr LIKE BestStr.

/* Denne brukes bare som mellomdefinisjon */
DEF TEMP-TABLE ttt_eksport
    FIELD iRectype AS INT
    FIELD OrdreNr LIKE Ordre.OrdreNr               
    FIELD LevNr LIKE Ordre.LevNr                 
    FIELD POrdreStatus LIKE Ordre.OrdreStatus           
    FIELD EkstId LIKE Ordre.EkstId                
    FIELD LevMerknad LIKE Ordre.LevMerknad            
    FIELD SendtDato LIKE Ordre.SendtDato             
    FIELD Merknad LIKE Ordre.Merknad               
    FIELD cLevAdresse1 AS CHAR FORMAT "x(30)"         
/*10*/ FIELD cLevAdresse2 AS CHAR FORMAT "x(30)"         
    FIELD cLevPostNr AS CHAR FORMAT "x(30)"          
    FIELD cLevPostBoks AS CHAR FORMAT "x(30)"         
    FIELD cLevTelefon AS CHAR FORMAT "x(30)"           
    FIELD cLevKontakt AS CHAR FORMAT "x(30)"           
    FIELD cNotat1 LIKE Ordre.Notat
    FIELD Hasteordre LIKE Ordre.Hasteordre
    /* Bestillingshode */
    FIELD BestNr LIKE BestHode.BestNr                
    FIELD Bestillingsdato LIKE BestHode.BestillingsDato       
    FIELD BestStat LIKE BestHode.BestStat              
/*20*/ FIELD cNotat2 LIKE BestHode.Merknad 
    FIELD Beskrivelse LIKE BestHode.Beskrivelse           
    FIELD DirekteLev LIKE BestHode.DirekteLev            
    FIELD ArtikkelNr AS CHAR FORMAT "x(30)" 
    FIELD Beskr LIKE ArtBas.Beskr
    FIELD LevKod LIKE ArtBas.LevKod                
    FIELD LevFargKod LIKE ArtBas.LevFargKod 
    FIELD BOrdreNr LIKE BestHode.OrdreNr               
    FIELD BestType LIKE BestHode.BestType              
    FIELD BSendtDato LIKE BestHode.SendtDato             
/*30*/ FIELD LevDato LIKE BestHode.LevDato               
    FIELD KjedeAvtale LIKE BestHode.KjedeAvtale           
    FIELD EkstOrdreNr LIKE BestHode.EkstOrdreNr           
    /* Kalkyle */
    FIELD ValPris LIKE BestPris.ValPris               
    FIELD InnkjopsPris LIKE BestPris.InnkjopsPris          
    FIELD Rab1Kr LIKE BestPris.Rab1Kr                
    FIELD Rab1% LIKE BestPris.Rab1%                 
    FIELD Rab2Kr LIKE BestPris.Rab2Kr                
    FIELD Rab2% LIKE BestPris.Rab2%                 
    FIELD Frakt LIKE BestPris.Frakt                 
/*40*/ FIELD Frakt% LIKE BestPris.Frakt%                
    FIELD DivKostKr LIKE BestPris.DivKostKr             
    FIELD DivKost% LIKE BestPris.DivKost%              
    FIELD Rab3Kr LIKE BestPris.Rab3Kr                
    FIELD Rab3% LIKE BestPris.Rab3%                 
    FIELD DbKr LIKE BestPris.DBKr                  
    FIELD Db% LIKE BestPris.DB%                   
    FIELD Pris LIKE BestPris.Pris  
    FIELD KjedeInnkPris LIKE VareBehLinje.KjedeInnkPris
    /* Bestillte størrelser */
    FIELD BButik LIKE BestStr.Butik                 
/*50*/ FIELD Storl LIKE BestStr.Storl
/*51*/ FIELD Bestilt LIKE BestStr.Bestilt               
    /* Strekkode */
/*52*/ FIELD Kode LIKE Strekkode.Kode
/*53*/ FIELD Bestillingsnummer LIKE Strekkode.Bestillingsnummer
/*54*/ FIELD VareKost LIKE BestPris.Varekost
    /* VPI informasjon */
/*55*/ FIELD LopNr LIKE Artbas.LopNr             
/*56*/ FIELD ABeskr LIKE Artbas.Beskr
/*57*/ FIELD BongTekst LIKE Artbas.BongTekst
    /* Leverandørinformasjon */
/*58*/ FIELD ALevNr LIKE Artbas.LevNr             
/*59*/ FIELD LevNamn LIKE LevBas.LevNamn
/*60*/ FIELD ALevKod LIKE Artbas.LevKod            
    FIELD ALevFargKod LIKE Artbas.LevFargKod    
    FIELD LevVareTekst LIKE Artbas.LevVareTekst      
    /* Varestruktur */
    /* Avdeling */
    FIELD AvdelingNr LIKE Avdeling.AvdelingNr
    FIELD AvdelingNavn LIKE Avdeling.AvdelingNavn
    /* Hovedgruppe */
    FIELD Hg LIKE VarGr.Hg
    FIELD HgBeskr LIKE HuvGr.HgBeskr
    /* Varegruppe */
    FIELD Vg LIKE ArtBas.Vg
    FIELD VgBeskr LIKE VarGr.VgBeskr
    FIELD MomsKod LIKE VarGr.MomsKod
    FIELD Kost_Proc LIKE VarGr.Kost_Proc
    /* Artikkeltype */
    FIELD ArtSlag LIKE Artbas.ArtSlag           
    FIELD IndividType LIKE Artbas.IndividType       
    FIELD OPris LIKE Artbas.OPris             
    FIELD Lager LIKE Artbas.lager             
    FIELD Storrelser LIKE Artbas.Storrelser        
    FIELD StrTypeId LIKE Artbas.StrTypeID   
    FIELD KjedeRab% LIKE ArtBas.KjedeRab%
    FIELD AKjedeInnkPris LIKE ArtBas.KjedeInnkPris
    FIELD Gjennomfaktureres LIKE ArtBas.Gjennomfaktureres
    /* Strekkode og størrelse */
    FIELD Kode2 LIKE Strekkode.Kode 
    FIELD StrKode LIKE Strekkode.StrKode   
    FIELD Kodetype LIKE Strekkode.KodeType          
    FIELD HovedNr LIKE Strekkode.HovedNr           
    FIELD IKasse LIKE Strekkode.IKasse             
    FIELD SStorl LIKE StrKonv.Storl             
    /* Koblede registre */
    FIELD VgKat LIKE Artbas.VgKat             
    FIELD GarantiKl LIKE Artbas.GarantiKl         
    FIELD ProdNr LIKE Artbas.ProdNr 
    FIELD PBeskrivelse LIKE Produsent.Beskrivelse
    FIELD VMId LIKE Artbas.VMId    
    FIELD Beskrivelse2 LIKE Varemerke.Beskrivelse
    FIELD SaSong LIKE Artbas.SaSong            
    FIELD SasBeskr LIKE Sasong.SasBeskr
    FIELD Farg LIKE Artbas.Farg              
    FIELD FarBeskr LIKE Farg.FarBeskr
    FIELD MatKod LIKE Artbas.MatKod            
    FIELD MatBeskr LIKE Material.MatBeskr
    FIELD ValKod LIKE Artbas.valkod            
    FIELD ValKurs LIKE Valuta.ValKurs
    /* Leverandørbetingelser */
    FIELD LevDato1 LIKE Artbas.LevDato1          
    FIELD LevDato2 LIKE Artbas.LevDato2          
    FIELD LevDato3 LIKE Artbas.LevDato3          
    FIELD LevDato4 LIKE Artbas.LevDato4          
    FIELD c2Notat3 LIKE Artbas.LinjeMerknad 
    FIELD KatalogPris LIKE Artbas.KatalogPris       
    FIELD ForhRab% LIKE Artbas.forhRab%          
    FIELD SupRab% LIKE Artbas.supRab%           
    FIELD VPIDato LIKE Artbas.VPIDato           
    FIELD KjedeVare LIKE Artbas.KjedeVare         
    FIELD VPIBildeKode LIKE Artbas.VPIBildeKode      
    FIELD AntIPakn LIKE Artbas.AntIPakn          
    FIELD FrittTillegg LIKE Artbas.FrittTillegg      
    FIELD c2Notat2 LIKE Artbas.VareFakta 
    FIELD StrKode1 LIKE Artbas.StrKode1          
    FIELD StrKode2 LIKE Artbas.StrKode2          
    .

DEF TEMP-TABLE tt_eksport LIKE ttt_eksport
    /* Sist endret */
    FIELD EDato LIKE Artbas.EDato             
    FIELD cETid AS CHAR FORMAT "x(30)"               
    FIELD BrukerId LIKE Artbas.BrukerID          
    /* opprettet */
    FIELD RegistrertDato LIKE Artbas.RegistrertDato    
    FIELD cRegistrertTid AS CHAR FORMAT "x(30)"      
    FIELD RegistrertAv LIKE Artbas.RegistrertAv      
    /* Diverse artikkelinformasjon */
    FIELD Ny_Dato LIKE Artbas.ny_dato           
    FIELD Inn_Dato LIKE Artbas.inn_dato          
    FIELD c2Notat1 LIKE Artbas.Notat
    FIELD AnonseArtikkel LIKE Artbas.AnonseArtikkel    
    FIELD AktivDato LIKE Artbas.AktivDato         
    FIELD AktivAv LIKE Artbas.AktivAv           
    FIELD Aktivert LIKE Artbas.Aktivert          
    FIELD SattPaKampanje LIKE Artbas.SattPaKampanje    
    FIELD OLLager LIKE Artbas.OLLager           
    FIELD BildeIKasse LIKE Artbas.BildeIKasse       
    FIELD Pakke LIKE Artbas.Pakke             
    FIELD Alder LIKE Artbas.Alder             
    FIELD HkStyrt LIKE Artbas.HkStyrt           
    FIELD LokPris LIKE Artbas.LokPris           
    FIELD AIKasse LIKE Artbas.IKasse            
    FIELD KjentPaHK LIKE Artbas.KjentPaHK         
    FIELD BehKode LIKE Artbas.BehKode           
    FIELD PakkeNr LIKE Artbas.Pakkenr           
    FIELD AnbefaltPris LIKE Artbas.AnbefaltPris      
    FIELD KundeRabatt LIKE Artbas.KundeRabatt       
    FIELD Etikett LIKE Artbas.Etikett           
    FIELD SalgsEnhet LIKE Artbas.SalgsEnhet        
    FIELD ModellFarge LIKE Artbas.ModellFarge       
    FIELD SentralBestilling LIKE Artbas.SentralBestilling 
    FIELD PrisGrpNr LIKE Artbas.PrisGrpNr         
    FIELD HovedModellNr LIKE Artbas.HovedModellFarge  
    FIELD Etikettekst1 LIKE Artbas.Etikettekst1      
    FIELD EtiLayout LIKE Artbas.EtiLayout         
    FIELD LinkVare LIKE Artbas.LinkVareNr        
    FIELD Mengde LIKE Artbas.Mengde            
    FIELD ManRabIKas LIKE Artbas.ManRabIKas        
    FIELD Kommentar LIKE Artbas.Kommentar           
    /* Priser */
    FIELD Tilbud LIKE ArtPris.Tilbud            
    FIELD AktivFraDato LIKE ArtPris.AktivFraDato      
    FIELD AktivFraTid LIKE ArtPris.AktivFraTid       
    FIELD TilbudFraDato LIKE ArtPris.TilbudFraDato     
    FIELD TilbudTilDato LIKE ArtPris.TilbudTilDato     
    FIELD TilbudFraTid LIKE ArtPris.TilbudFraTid      
    FIELD TilbudTilTid LIKE ArtPris.TilbudTilTid      
    /* Normalpris */
    FIELD AValPris LIKE Prisko.ValPris
    FIELD AInnkjopsPris LIKE PrisKo.InnkjopsPris
    FIELD P1Rab1Kr LIKE PrisKo.Rab1Kr            
    FIELD P1Rab1% LIKE PrisKo.Rab1%             
    FIELD P1Rab2Kr LIKE PrisKo.Rab2Kr            
    FIELD P1Rab2% LIKE PrisKo.Rab2%             
    FIELD P1Frakt LIKE PrisKo.Frakt             
    FIELD P1Frakt% LIKE PrisKo.Frakt%            
    FIELD P1DivKostKr LIKE PrisKo.DivKostKr         
    FIELD P1DivKost% LIKE PrisKo.DivKost%          
    FIELD P1Rab3Kr LIKE PrisKo.Rab3Kr            
    FIELD p1Rab3% LIKE PrisKo.Rab3%             
    FIELD p1VareKost LIKE PrisKo.VareKost          
    FIELD p1MvaKr LIKE PrisKo.MvaKr             
    FIELD p1Mva% LIKE PrisKo.Mva%              
    FIELD p1DBKr LIKE PrisKo.DBKr              
    FIELD p1Db% LIKE PrisKo.DB%               
    FIELD p1Pris LIKE PrisKo.Pris              
    /* Tilbudspris */
    FIELD p2ValPris LIKE PrisKo.ValPris           
    FIELD p2Innkjopspris LIKE PrisKo.InnkjopsPris      
    FIELD p2Rab1Kr LIKE PrisKo.Rab1Kr            
    FIELD p2Rab1% LIKE PrisKo.Rab1%             
    FIELD p2Rab2Kr LIKE PrisKo.Rab2Kr            
    FIELD p2Rab2% LIKE PrisKo.Rab2%             
    FIELD p2Frakt LIKE  PrisKo.Frakt             
    FIELD p2Frakt% LIKE PrisKo.Frakt%            
    FIELD p2DivKostKr LIKE PrisKo.DivKostKr         
    FIELD p2DivKost% LIKE PrisKo.DivKost%          
    FIELD p2Rab3Kr LIKE PrisKo.Rab3Kr            
    FIELD p2Rab3% LIKE PrisKo.Rab3%             
    FIELD p2VareKost LIKE PrisKo.VareKost          
    FIELD p2MvaKr LIKE PrisKo.MvaKr             
    FIELD p2Mva% LIKE PrisKo.Mva%              
    FIELD p2DBKr LIKE PrisKo.DBKr              
    FIELD p2DB% LIKE PrisKo.DB%               
    FIELD p2Pris LIKE PrisKo.Pris   
    /* Nye felt som skal ut. */
    FIELD RAvdNr LIKE ArtBas.RAvdNr
    FIELD Kjedevare2 LIKE ArtBas.Kjedevare
    FIELD Gjennomfaktureres2 LIKE artbas.Gjennomfaktureres
    FIELD SeqNr AS INT
    FIELD OrdreMottaker LIKE Ordre.OrdreMottaker
    FIELD Innkjopspris2 LIKE Vareboklinje.Innkjopspris  
    FIELD Varekost2 LIKE Vareboklinje.Varekost  
    FIELD ForhRab%2 LIKE Vareboklinje.ForhRab%  
    FIELD SupVarekost LIKE Vareboklinje.SupVarekost 
    FIELD SupRab%2 LIKE Vareboklinje.SupRab% 
    FIELD Pris2 LIKE Vareboklinje.Pris 
    FIELD AnbefaltPris2 LIKE Vareboklinje.AnbefaltPris 
    FIELD Kampanjepris LIKE Vareboklinje.Kampanjepris 
    FIELD KjedeInnKPris2 LIKE Vareboklinje.KjedeInnKPris  
    FIELD SortimentKoder LIKE Vareboklinje.SortimentKoder 
    FIELD Kampanjeuker LIKE Vareboklinje.Kampanjeuker 
    FIELD Kampanjestotte LIKE Vareboklinje.Kampanjestotte 
    FIELD Sasong2 LIKE Vareboklinje.Sasong 
    FIELD SasBeskr2 AS CHAR FORMAT "x(30)"  
    FIELD Lagerkoder AS CHAR FORMAT "x(30)"
    FIELD KjedeValutaPris AS CHAR FORMAT "x(15)"
    FIELD KjedeProdusent AS CHAR FORMAT "x(30)"
    FIELD ERPNr AS CHAR FORMAT "x(30)"
    .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Procedure 
/* ************************* Included-Libraries *********************** */

{incl\devmode.i}
{incl\custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCL INT}

FIND FIRST EkstEDBSystem WHERE EkstEDBSystem.DataType = "ORD" AND EkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF NOT AVAIL EkstEDBSystem THEN DO:
    ocRetur = "ERROR - Ingen ORD-rutine aktiv".
    RETURN.
END.

ASSIGN
    cMottager = "KJEDE,GJENNOM"
    .

MOTTAGER:
DO piLoop = 1 TO NUM-ENTRIES(cMottager):
    IF DYNAMIC-FUNCTION("runProc","get_ekstedbsys_filnavn.p",EkstEDBSystem.EDBSystem,?) THEN 
        ASSIGN cGetFilnavn = DYNAMIC-FUNCTION("getTransactionMessage").
    IF NOT NUM-ENTRIES(cGetFilnavn,"|") = 3 THEN DO:
        ocRetur = "ERROR-" + cGetFilnavn.
        RETURN.
    END.
    ELSE DO:
        ASSIGN cTmpFilNavn = RIGHT-TRIM(ENTRY(1,cGetFilnavn,"|"),"\") + "\" + "TMP" + ENTRY(2,cGetFilnavn,"|")
               cFilNavn    = ENTRY(2,cGetFilnavn,"|").
        IF ENTRY(piLoop,cMottager) = "KJEDE" THEN
            cFilNavn = REPLACE(cFilNavn,"ORD","ORDK").
        ELSE IF ENTRY(piLoop,cMottager) = "GJENNOM" THEN
            cFilNavn = REPLACE(cFilNavn,"ORD","ORDG").
    END.

    /* Tømmer buffer før ny sortering bygges opp. */
    FOR EACH ttt_eksport:
      DELETE ttt_eksport.
    END.
    FOR EACH tt_Eksport:
        DELETE tt_Eksport.
    END.
    FOR EACH TT_ELogg:
        DELETE TT_Elogg.
    END.

    /* Leser alle loggede ordre og logger berørte artikler. */
    RUN KopierElogg.

    /* Nå legger vi ut ordrene. */
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = iCl NO-ERROR.
    RUN ByggTmpTabell.
    RUN EksporterOrd.
    ASSIGN
        iTotAntEksport = iTotantEksport + iantEksport
        iAntEksport    = 0
        .
END. /* MOTTAGER */


ocRetur = "OK," + String(iTotAntEksport).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-assignTmpTable) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assignTmpTable Procedure 
PROCEDURE assignTmpTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcBestillingsnr AS CHAR NO-UNDO.

IF AVAILABLE Strekkode THEN
DO:
  IF Strekkode.Bestillingsnummer <> "" THEN
      pcBestillingsnr = Strekkode.Bestillingsnummer.
  ELSE pcBestillingsnr = ArtBas.LevKod.
END.
ELSE pcBestillingsnr = ArtBas.LevKod.
ASSIGN
          tt_Eksport.iRecType        = iRectype
          /* Ordrehode */
          tt_Eksport.OrdreNr         = Ordre.OrdreNr               
          tt_Eksport.LevNr           = Ordre.LevNr                 
          tt_Eksport.POrdreStatus    = Ordre.OrdreStatus           
          tt_Eksport.EkstId          = Ordre.EkstId                
          tt_Eksport.LevMerknad      = Ordre.LevMerknad            
          tt_Eksport.SendtDato       = Ordre.SendtDato             
          tt_Eksport.Merknad         = Ordre.Merknad               
          tt_Eksport.cLevAdresse1    = cLevAdresse1          
          tt_Eksport.cLevAdresse2    = cLevAdresse2          
          tt_Eksport.cLEvPostNr      = cLevPostNr            
          tt_Eksport.cLevPostBoks    = cLevPostBoks          
          tt_Eksport.cLevTelefon     = cLevTelefon           
          tt_Eksport.cLevKontakt     = cLevKontakt           
  /*15*/  tt_Eksport.cNotat1         = cNotat1 /* Ordre.Notat */
          tt_Eksport.Hasteordre      = Ordre.HasteOrdre
          /* Bestillingshode */
          tt_Eksport.BestNr          = BestHode.BestNr                
          tt_Eksport.Bestillingsdato = BestHode.BestillingsDato       
          tt_Eksport.BestStat        = BestHode.BestStat              
          tt_Eksport.cNotat2         = cNotat2 /*BestHode.Merknad */
          tt_Eksport.Beskrivelse     = BestHode.Beskrivelse           
          tt_Eksport.DirekteLev      = BestHode.DirekteLev            
          tt_Eksport.ArtikkelNr      = STRING(BestHode.ArtikkelNr) + STRING(iStrKode,"999")
          tt_Eksport.Beskr           = ArtBas.Beskr
          tt_Eksport.LevKod          = pcBestillingsnr                
          tt_Eksport.LevFargKod      = ArtBas.LevFargKod 
          tt_Eksport.BOrdreNr        = BestHode.OrdreNr               
          tt_Eksport.BestType        = BestHode.BestType              
          tt_Eksport.BSendtDato      = BestHode.SendtDato             
          tt_Eksport.LevDato         = BestHode.LevDato               
          tt_Eksport.KjedeAvtale     = BestHode.KjedeAvtale           
  /*31*/  tt_Eksport.EkstOrdreNr     = BestHode.EkstOrdreNr           
        /* Kalkyle */          /* Kalkyle */
          tt_Eksport.ValPris         = BestPris.ValPris               
          tt_Eksport.InnkjopsPris    = BestPris.InnkjopsPris          
          tt_Eksport.Rab1Kr          = BestPris.Rab1Kr                
          tt_Eksport.Rab1%           = BestPris.Rab1%                 
          tt_Eksport.Rab2Kr          = BestPris.Rab2Kr                
          tt_Eksport.Rab2%           = BestPris.Rab2%                 
          tt_Eksport.Frakt           = BestPris.Frakt                 
          tt_Eksport.Frakt%          = BestPris.Frakt%                
          tt_Eksport.DivKostKr       = BestPris.DivKostKr             
          tt_Eksport.DivKost%        = BestPris.DivKost%              
          tt_Eksport.Rab3Kr          = BestPris.Rab3Kr                
          tt_Eksport.Rab3%           = BestPris.Rab3%                 
          tt_Eksport.DbKr            = BestPris.DBKr                  
          tt_Eksport.Db%             = BestPris.DB%                   
  /*46*/  tt_Eksport.Pris            = BestPris.Pris  
          .
        /* Kjederabatt */
        IF AVAILABLE VareBehLinje THEN
        DO:
            tt_Eksport.KjedeInnkPris   = (IF VareBehLinje.KjedeInnkPris = 0 
                                            THEN VareBehLinje.Varekost
                                            ELSE VareBehLinje.KjedeInnkPris).
        END.
        ASSIGN
          /* Bestillte størrelser */
          tt_Eksport.BButik          = TT_BestStr.Butik                 
          tt_Eksport.Storl           = TRIM(TT_BestStr.Storl)
          tt_Eksport.Bestilt         = TT_BestStr.Bestilt               
          /* Strekkode */
          tt_Eksport.Kode            = IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE ""
          tt_Eksport.Varekost        = BestPris.Varekost        
          tt_Eksport.Bestillingsnummer = IF AVAILABLE Strekkode THEN Strekkode.Bestillingsnummer ELSE ""
          /* VPI informasjon */
          tt_Eksport.LopNr           = Artbas.LopNr             
          tt_Eksport.ABeskr          = Artbas.Beskr
          tt_Eksport.BongTekst       = Artbas.BongTekst    
          /* Leverandørinformasjon */
   /*56*/ tt_Eksport.ALevNr          = Artbas.LevNr             
          tt_Eksport.LevNamn         = IF AVAILABLE LevBas THEN LevBas.LevNamn ELSE ""
          tt_Eksport.ALevKod         = pcBestillingsnr            
          tt_Eksport.ALevFargKod     = Artbas.LevFargKod    
          tt_Eksport.LevVareTekst    = Artbas.LevVareTekst      
          /* Varestruktur */
          /* Avdeling */
          tt_Eksport.AvdelingNr      = IF AVAILABLE Avdeling THEN Avdeling.AvdelingNr ELSE 0
          tt_Eksport.AvdelingNavn    = Avdeling.AvdelingNavn WHEN AVAILABLE Avdeling
          /* Hovedgruppe */
          tt_Eksport.Hg              = IF AVAILABLE VarGr THEN VarGr.Hg ELSE 0
          tt_Eksport.HgBeskr         = IF AVAILABLE HuvGr THEN HuvGr.HgBeskr ELSE ""
          /* Varegruppe */
          tt_Eksport.Vg              = IF AVAILABLE ArtBAs THEN ArtBas.Vg ELSE 0
          tt_Eksport.VgBeskr         = IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ""
          tt_Eksport.MomsKod         = IF AVAILABLE VarGr THEN VarGr.MomsKod ELSE 0
          tt_Eksport.Kost_Proc       = IF AVAILABLE VarGr THEN VarGr.Kost_Proc ELSE 0
          /* Artikkeltype */
          tt_Eksport.ArtSlag         = Artbas.ArtSlag           
          tt_Eksport.IndividType     = Artbas.IndividType       
          tt_Eksport.OPris           = Artbas.OPris             
          tt_Eksport.Lager           = Artbas.lager             
          tt_Eksport.Storrelser      = Artbas.Storrelser        
          tt_Eksport.StrTypeId       = Artbas.StrTypeID   
          tt_Eksport.KjedeRab%       = ArtBas.KjedeRab%
          .
          /* Kjederabatt */
          IF AVAILABLE VareBehLinje THEN
          DO:
              ASSIGN
              tt_Eksport.Kjedevare2         = VareBehLinje.Kjedevare
              tt_Eksport.Gjennomfaktureres2 = VareBehLinje.Gjennomfaktureres
              tt_Eksport.AKjedeInnkPris     = (IF VareBehLinje.KjedeInnkPris = 0 
                                                 THEN VareBehLinje.Varekost
                                                 ELSE VareBehLinje.KjedeInnkPris).
          END.

      ASSIGN
   /*77*/ tt_Eksport.Gjennomfaktureres = IF AVAILABLE VareBehLinje 
                                           THEN VareBehLinje.Gjennomfaktureres
                                           ELSE ArtBas.Gjennomfaktureres
          /* Strekkode og størrelse */
          tt_Eksport.Kode2           = IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE ""
          tt_Eksport.StrKode         = IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0  
          tt_Eksport.Kodetype        = IF AVAILABLE Strekkode THEN Strekkode.KodeType ELSE 0          
          tt_Eksport.HovedNr         = IF AVAILABLE Strekkode THEN Strekkode.HovedNr ELSE NO           
          tt_Eksport.IKasse          = IF AVAILABLE Strekkode THEN Strekkode.IKasse ELSE YES            
          tt_Eksport.SStorl          = IF AVAILABLE StrKonv THEN TRIM(StrKonv.Storl) ELSE ""             
          
          /* Koblede registre */
          tt_Eksport.VgKat           = Artbas.VgKat             
          tt_Eksport.GarantiKl       = Artbas.GarantiKl         
          tt_Eksport.ProdNr          = Artbas.ProdNr 
          tt_Eksport.PBeskrivelse    = IF AVAILABLE Produsent THEN Produsent.Beskrivelse ELSE ""
          tt_Eksport.VmId            = Artbas.VMId 
          tt_Eksport.Beskrivelse2    = IF AVAILABLE Varemerke THEN Varemerke.Beskrivelse ELSE ""   
          tt_Eksport.SaSong          = ArtBas.Sasong                    
          tt_Eksport.SasBeskr        = IF AVAILABLE Sasong THEN Sasong.SasBeskr ELSE ""
          tt_Eksport.Farg            = Artbas.Farg                                         
          tt_Eksport.FarBeskr        = IF AVAILABLE Farg THEN Farg.FarBeskr ELSE ""        
          tt_Eksport.MatKod          = Artbas.MatKod                                       
          tt_Eksport.MatBeskr        = IF AVAILABLE Material THEN Material.MatBeskr ELSE ""
          tt_Eksport.ValKod          = Artbas.valkod                                       
          tt_Eksport.ValKurs         = IF AVAILABLE Valuta THEN Valuta.ValKurs ELSE 0      
          
          /* Leverandørbetingelser */
          tt_Eksport.LevDato1        = Artbas.LevDato1          
          tt_Eksport.LevDato2        = Artbas.LevDato2          
          tt_Eksport.LevDato3        = Artbas.LevDato3          
          tt_Eksport.LevDato4        = Artbas.LevDato4          
          tt_Eksport.c2Notat3        = c2Notat3 /*Artbas.LinjeMerknad */
          tt_Eksport.KatalogPris     =  Artbas.KatalogPris       
          tt_Eksport.ForhRab%        = Artbas.forhRab%          
          tt_Eksport.SupRab%         = Artbas.supRab%           
          tt_Eksport.VPIDato         = Artbas.VPIDato           
          tt_Eksport.KjedeVare       = IF AVAILABLE VareBehLinje
                                         THEN VareBehLinje.KjedeVare
                                         ELSE Artbas.KjedeVare         
          tt_Eksport.VPIBildeKode    = Artbas.VPIBildeKode      
          tt_Eksport.AntIPakn        = Artbas.AntIPakn          
          tt_Eksport.FrittTillegg    = Artbas.FrittTillegg      
          tt_Eksport.c2Notat2        = c2Notat2 /*Artbas.VareFakta */
          tt_Eksport.StrKode1        = Artbas.StrKode1          
          tt_Eksport.StrKode2        = Artbas.StrKode2    
          .
        ASSIGN
          /* Sist endret */
          tt_Eksport.EDato           = Artbas.EDato             
          tt_Eksport.cETid           = STRING(Artbas.ETid,"HH:MM:SS")              
          tt_Eksport.BrukerId        = Artbas.BrukerID          
          /* opprettet */
          tt_Eksport.RegistrertDato  = Artbas.RegistrertDato    
          tt_Eksport.cRegistrertTid  = STRING(Artbas.RegistrertTid,"HH:MM:SS")     
          tt_Eksport.RegistrertAv    = Artbas.RegistrertAv      
          /* Diverse artikkelinforma sjon */
          tt_Eksport.Ny_Dato         = Artbas.ny_dato           
          tt_Eksport.Inn_Dato        = Artbas.inn_dato          
          tt_Eksport.c2Notat1        = c2Notat1 /*Artbas.Notat */
          tt_Eksport.AnonseArtikkel  = Artbas.AnonseArtikkel    
          tt_Eksport.AktivDato       = Artbas.AktivDato         
          tt_Eksport.AktivAv         = Artbas.AktivAv           
          tt_Eksport.Aktivert        = Artbas.Aktivert          
          tt_Eksport.SattPaKampanje  = Artbas.SattPaKampanje    
          tt_Eksport.OLLager         = Artbas.OLLager           
          tt_Eksport.BildeIKasse     = Artbas.BildeIKasse       
          tt_Eksport.Pakke           = Artbas.Pakke             
          tt_Eksport.Alder           = Artbas.Alder             
          tt_Eksport.HkStyrt         = Artbas.HkStyrt           
          tt_Eksport.LokPris         = Artbas.LokPris           
          tt_Eksport.AIKasse         = Artbas.IKasse            
          tt_Eksport.KjentPaHK       = Artbas.KjentPaHK         
          tt_Eksport.BehKode         = Artbas.BehKode           
          tt_Eksport.PakkeNr         = Artbas.Pakkenr           
          tt_Eksport.AnbefaltPris    = Artbas.AnbefaltPris      
          tt_Eksport.KundeRabatt     = Artbas.KundeRabatt       
          tt_Eksport.Etikett         = Artbas.Etikett           
          tt_Eksport.SalgsEnhet      = Artbas.SalgsEnhet        
          tt_Eksport.ModellFarge     = Artbas.ModellFarge       
          tt_Eksport.SentralBestilling = Artbas.SentralBestilling 
          tt_Eksport.PrisGrpNr       = Artbas.PrisGrpNr         
          tt_Eksport.HovedModellNr   = Artbas.HovedModellFarge  
          tt_Eksport.Etikettekst1    = Artbas.Etikettekst1      
          tt_Eksport.EtiLayout       = Artbas.EtiLayout         
          tt_Eksport.LinkVare        = Artbas.LinkVareNr        
          tt_Eksport.Mengde          = Artbas.Mengde            
          tt_Eksport.ManRabIKas      = Artbas.ManRabIKas        
          tt_Eksport.Kommentar       = Artbas.Kommentar 
          tt_Eksport.ERPNr           = (IF AVAILABLE Strekkode THEN Strekkode.ERPNr ELSE '')        
          /* Priser */               
          tt_Eksport.Tilbud          = ArtPris.Tilbud            
          tt_Eksport.AktivFraDato    = ArtPris.AktivFraDato      
          tt_Eksport.AktivFraTid     = ArtPris.AktivFraTid       
          tt_Eksport.TilbudFraDato   = ArtPris.TilbudFraDato     
          tt_Eksport.TilbudTilDato   = ArtPris.TilbudTilDato     
          tt_Eksport.TilbudFraTid    = ArtPris.TilbudFraTid      
          tt_Eksport.TilbudTilTid    = ArtPris.TilbudTilTid      
          /* Normalpris */
          tt_Eksport.AValPris        = ArtPris.ValPris[1]
          tt_Eksport.AInnkjopsPris   = ArtPris.InnkjopsPris[1]      
          tt_Eksport.P1Rab1Kr        = ArtPris.Rab1Kr[1]            
          tt_Eksport.P1Rab1%         = ArtPris.Rab1%[1]             
          tt_Eksport.P1Rab2Kr        = ArtPris.Rab2Kr[1]            
          tt_Eksport.P1Rab2%         = ArtPris.Rab2%[1]             
          tt_Eksport.P1Frakt         = ArtPris.Frakt[1]             
          tt_Eksport.P1Frakt%        = ArtPris.Frakt%[1]            
          tt_Eksport.P1DivKostKr     = ArtPris.DivKostKr[1]         
          tt_Eksport.P1DivKost%      = ArtPris.DivKost%[1]          
          tt_Eksport.P1Rab3Kr        = ArtPris.Rab3Kr[1]            
          tt_Eksport.p1Rab3%         = ArtPris.Rab3%[1]             
          tt_Eksport.p1VareKost      = ArtPris.VareKost[1]          
          tt_Eksport.p1MvaKr         = ArtPris.MvaKr[1]             
          tt_Eksport.p1Mva%          = ArtPris.Mva%[1]              
          tt_Eksport.p1DBKr          = ArtPris.DBKr[1]              
          tt_Eksport.p1Db%           = ArtPris.DB%[1]               
          tt_Eksport.p1Pris          = ArtPris.Pris[1]              
          /* Tilbudspris */
          tt_Eksport.p2ValPris       = ArtPris.ValPris[2]           
          tt_Eksport.p2Innkjopspris  = ArtPris.InnkjopsPris[2]      
          tt_Eksport.p2Rab1Kr        = ArtPris.Rab1Kr[2]            
          tt_Eksport.p2Rab1%         = ArtPris.Rab1%[2]             
          tt_Eksport.p2Rab2Kr        = ArtPris.Rab2Kr[2]            
          tt_Eksport.p2Rab2%         = ArtPris.Rab2%[2]             
          tt_Eksport.p2Frakt         = ArtPris.Frakt[2]             
          tt_Eksport.p2Frakt%        = ArtPris.Frakt%[2]            
          tt_Eksport.p2DivKostKr     = ArtPris.DivKostKr[2]         
          tt_Eksport.p2DivKost%      = ArtPris.DivKost%[2]          
          tt_Eksport.p2Rab3Kr        = ArtPris.Rab3Kr[2]            
          tt_Eksport.p2Rab3%         = ArtPris.Rab3%[2]             
          tt_Eksport.p2VareKost      = ArtPris.VareKost[2]          
          tt_Eksport.p2MvaKr         = ArtPris.MvaKr[2]             
          tt_Eksport.p2Mva%          = ArtPris.Mva%[2]              
          tt_Eksport.p2DBKr          = ArtPris.DBKr[2]              
          tt_Eksport.p2DB%           = ArtPris.DB%[2]               
          tt_Eksport.p2Pris          = ArtPris.Pris[2]   

        /* Nye felt */
          tt_Eksport.RAvdNr          = ArtBas.RAvdNr          
          tt_Eksport.OrdreMottaker   = Ordre.OrdreMottaker
          tt_Eksport.Sasong2         = IF AVAILABLE Sasong THEN Sasong.Sasong ELSE 0                 
          tt_Eksport.SasBeskr2       = IF AVAILABLE Sasong THEN Sasong.SasBeskr ELSE ""    
        .

    /* Henter sekvensnummer for sortering av størrelsen. */
    IF AVAILABLE ArtBas THEN
        FIND FIRST StrTstr NO-LOCK WHERE
           StrTstr.StrTypeId = ArtBAs.StrTypeId AND
           TRIM(StrTStr.SoStorl)   = trim(TT_BestStr.Storl) NO-ERROR.
    IF AVAILABLE STrTStr THEN
        tt_Eksport.SeqNr = StrTStr.SeqNr.

    IF AVAILABLE VareBokLinje THEN
        ASSIGN
        tt_Eksport.SortimentKoder  = VareBokLinje.Sortimentkoder 
        tt_Eksport.Kampanjeuker    = VarebokLinje.Kampanjeuker     
        tt_Eksport.Kampanjestotte  = VarebokLinje.Kampanjestotte 
        tt_Eksport.Sasong          = VareBokLinje.Sasong                 
        tt_Eksport.Lagerkoder      = Vareboklinje.Lagerkoder
        tt_Eksport.Innkjopspris2   = VareBokLinje.Innkjopspris     
        tt_Eksport.Varekost2       = VareBokLinje.Varekost             
        tt_Eksport.ForhRab%2       = VareBokLinje.ForhRab%             
        tt_Eksport.SupVarekost     = VareBokLinje.SupVarekost       
        tt_Eksport.SupRab%2        = VareBokLinje.SupRab%               
        tt_Eksport.Pris2           = VareBokLinje.Pris                     
        tt_Eksport.AnbefaltPris2   = VareBokLinje.AnbefaltPris     
        tt_Eksport.Kampanjepris    = VareBokLinje.Kampanjepris     
        tt_Eksport.KjedeInnKPris2  = VareBokLinje.KjedeInnKPris   
        tt_Eksport.KjedeValutaPris = Vareboklinje.KjedeValutaPris
        tt_Eksport.KjedeProdusent  = Vareboklinje.KjedeProdusent
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggTmpTabell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTmpTabell Procedure 
PROCEDURE ByggTmpTabell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR iRecType     AS INT  NO-UNDO.
DEF VAR cNotat1      AS CHAR NO-UNDO.
DEF VAR cNotat2      AS CHAR NO-UNDO.

DEF VAR c2Notat1     AS CHAR NO-UNDO.
DEF VAR c2Notat2     AS CHAR NO-UNDO.
DEF VAR c2Notat3     AS CHAR NO-UNDO.
DEFINE VARIABLE iBestButik  LIKE Butiker.Butik      NO-UNDO.
DEFINE VARIABLE iBestNr     LIKE BestHode.BestNr    NO-UNDO.
DEFINE BUFFER LevAdrButiker FOR Butiker.
/* Vare på strekkodenivå til ERP systemet. */



ASSIGN
    cLevAdresse1 = ""
    cLevAdresse2 = "" 
    cLevPostNr   = "" 
    cLevPostBoks = ""
    cLevTelefon  = ""
    cLevKontakt  = ""
    .

ORDRE:
FOR EACH TT_Elogg: 
    FOR FIRST Ordre NO-LOCK WHERE Ordre.OrdreNr = INT(TT_Elogg.Verdier),
    EACH BestHode NO-LOCK WHERE
         BestHode.OrdreNr = Ordre.OrdreNr
        BREAK BY BestHode.OrdreNr
              BY Besthode.LevDato:
    FIND BestPris NO-LOCK WHERE
         BestPris.BestNr = BestHode.BestNr AND BestPris.BestStat = BestHode.BestStat NO-ERROR.
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.

    FIND FIRST VareBehLinje NO-LOCK WHERE
        VareBehLinje.VareBehNr  = BestHode.VareBehNr AND
        VareBehLinje.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.
    IF AVAILABLE VareBehLinje THEN
        FIND VareBehHode OF VareBehLinje NO-ERROR.
    IF AVAILABLE VareBehHode THEN
        FIND VareBokLinje NO-LOCK WHERE
          VareBokLinje.VareBokNr = VareBehHode.Kilde AND
          VareBokLinje.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.

/*
IF VareBehLinje.ArtikkelNr = 9981467 OR VareBehLinje.ArtikkelNr = 9981522 THEN
MESSAGE "Funnet varebehlinje"
    BestHode.VareBehNr
    BestHode.ArtikkelNr
    VareBehLinje.VareBehNr
    VareBehLinje.ArtikkelNr
    VareBehLinje.Beskr
    VareBehLinje.ArtikkelNr
    VareBehLinje.KjedeVare
    VareBehLinje.Gjennomfaktureres
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/    
    /* Fra varehåndteringsbok */
    IF AVAILABLE VareBehLinje THEN 
    DO:
/*
        IF VareBehLinje.KjedeVare = TRUE THEN
        MESSAGE "Fra varebok" VareBehLinje.KjedeVare VareBehLinje.Gjennomfaktureres SKIP
            VareBehLinje.Beskr
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
        /* Det er kun produkter som er merket som kjedevare som skal eksporteres */
        IF (VareBehLinje.KjedeVare = TRUE OR VareBehLinje.Gjennomfaktureres = TRUE) THEN. /* Gjør ingenting. Skal være med */
        ELSE NEXT ORDRE. /* Er det ikke kjedevare eller gjennomfakturering, skal den ikke med. */
    END.
    /* Ikke varebok */
    ELSE DO:
/*
        IF ArtBas.Kjedevare = TRUE THEN
        MESSAGE "Fra artbas" ArtBAs.KjedeVare ArtBAs.Gjennomfaktureres SKIP
            VareBehLinje.Beskr
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
        /* Det er kun produkter som er merket som kjedevare som skal eksporteres */
        IF (ArtBas.KjedeVare = TRUE OR Artbas.Gjennomfaktureres = TRUE) THEN. /* Gjør ingenting. Skal være med */
        ELSE NEXT ORDRE. /* Er det ikke kjedevare eller gjennomfakturering, skal den ikke med. */
    END.
/*
MESSAGE "Gurre kjører FillTT_BestStr" 
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
*/
    RUN FillTT_BestStr. /* För att hantera direkte/ikke direkte */

    /* Slipper ev. recordlock. */
    IF AVAILABLE VarGr THEN
        RELEASE VarGr.
    IF AVAILABLE Avdeling THEN
        RELEASE Avdeling.
    IF AVAILABLE HuvGr THEN
        RELEASE HuvGr.

    /* Henter koblede poster til artikkelen hvis suppleringsbok er koblet til varebok. */
    IF AVAILABLE VareBokLinje THEN
    FRA_VAREBOKLINJE:
    DO:
        FIND VarGr NO-LOCK WHERE
            VarGr.Vg = VareBokLinje.Vg NO-ERROR.
        FIND LevBas NO-LOCK WHERE
            LevBas.LevNr = VareBokLinje.LevNr NO-ERROR.
        FIND Produsent NO-LOCK WHERE
            Produsent.ProdNr = VareBokLinje.ProdNr NO-ERROR.
        FIND Sasong NO-LOCK WHERE
            Sasong.Sasong = VareBokLinje.Sasong NO-ERROR.
    END. /* FRA_VAREBOKLINJE */

    /* Suppleringsboken er ikke koblet til varebok */
    ELSE DO:
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
        FIND LevBas NO-LOCK WHERE
            LevBas.LevNr = ArtBas.LevNr NO-ERROR.
        FIND Produsent NO-LOCK WHERE
            Produsent.ProdNr = ArtBas.ProdNr NO-ERROR.
        FIND Sasong NO-LOCK WHERE
            Sasong.Sasong = ArtBAs.Sasong NO-ERROR.
    END.

    /* Koblede poser som ikke påvirket av varebok */
    FIND Farg NO-LOCK WHERE
        Farg.Farg = ArtBas.Farg NO-ERROR.
    FIND Varemerke NO-LOCK WHERE
        Varemerke.VmId = ArtBas.VmId NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
    IF AVAILABLE HuvGr THEN
        FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    FIND ArtPris OF ArtBas NO-LOCK WHERE
        ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    FIND Material NO-LOCK WHERE
        Material.MatKod = ArtBas.MatKod NO-ERROR.
    FIND Valuta NO-LOCK WHERE
        Valuta.ValKod = ArtBas.Valkod NO-ERROR.
    IF AVAILABLE ArtBas THEN
        ASSIGN
        c2Notat1 = ArtBas.Notat
        c2Notat2 = ArtBas.VareFakta
        c2Notat3 = ArtBas.Linjemerknad
        .

    FOR EACH tt_BestStr
        BREAK
        BY TT_BestStr.Butik  
        BY TT_BestStr.Storl
        BY TT_BestStr.BestNr:
        FIND FIRST StrKonv NO-LOCK WHERE
            TRIM(StrKonv.Storl) = trim(TT_BestStr.Storl) NO-ERROR.
        IF AVAILABLE StrKonv THEN DO:
            /* Sjekker først om vi finner en med hovednummer */
            FIND FIRST Strekkode NO-LOCK WHERE
                 Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                 Strekkode.StrKode    = StrKonv.StrKode AND NOT StrekKode.Kode BEGINS "02" AND
                 Strekkode.HovedNr = TRUE NO-ERROR.
            /* Da tester vi uten hovednummer */
            FIND FIRST Strekkode NO-LOCK WHERE
                 Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                 Strekkode.StrKode    = StrKonv.StrKode AND NOT StrekKode.Kode BEGINS "02" NO-ERROR.
            IF NOT AVAIL StrekKode THEN
                FIND FIRST Strekkode NO-LOCK WHERE
                     Strekkode.ArtikkelNr = ArtBas.ArtikkelNr AND
                     Strekkode.StrKode    = StrKonv.StrKode NO-ERROR.
        END.
        /* Henter varebeh.linjen for å få frem kjederabatten. */
        FIND VareBehLinje NO-LOCK WHERE
            VareBehLinje.VareBehNr  = BestHode.VareBehNr AND
            VareBehLinje.ArtikkelNr = BestHode.ArtikkelNr NO-ERROR.
        ASSIGN 
            iAntEksport = iAntEksport + 1
            iRecType    = IF iRecType = 0 THEN 1 ELSE iRecType 
            iStrKode    = IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0
            cNotat1     = Ordre.Notat
            cNotat2     = BestHode.Merknad
            /* Stripper notatfelt for chr(10) og chr(13). */
            cNotat1  = REPLACE (cNotat1,CHR(13),"|")
            cNotat1  = REPLACE (cNotat1,CHR(10),"|")
            cNotat2  = REPLACE (cNotat2,CHR(13),"|")
            cNotat2  = REPLACE (cNotat2,CHR(10),"|")
            c2Notat1 = REPLACE (c2Notat1,CHR(13),"|")
            c2Notat1 = REPLACE (c2Notat1,CHR(10),"|")
            c2Notat2 = REPLACE (c2Notat2,CHR(13),"|")
            c2Notat2 = REPLACE (c2Notat2,CHR(10),"|")
            c2Notat3 = REPLACE (c2Notat3,CHR(13),"|")
            c2Notat3 = REPLACE (c2Notat3,CHR(10),"|")
            .

        IF iBestButik <> TT_BestStr.Butik OR iBestNr <> BestHode.Bestnr THEN DO:
            ASSIGN iBestButik = TT_BestStr.Butik
                   iBestNr    = BestHode.Bestnr.

            ASSIGN cLevAdresse1 = ""
                   cLevAdresse2 = ""
                   cLevPostNr   = ""
                   cLevPostBoks = ""
                   cLevTelefon  = ""
                   cLevKontakt  = "".
            RELEASE LevAdrButiker.
            FIND LevAdrButiker WHERE LevAdrButiker.butik = TT_BestStr.Butik NO-LOCK NO-ERROR.
            IF BestHode.Direktelev = FALSE THEN DO:
                IF AVAIL LevAdrButiker AND Butiker.clButikkNr <> 0 THEN DO:
                    FIND LevAdrButiker WHERE LevAdrButiker.butik = Butiker.clButikkNr NO-LOCK NO-ERROR.
                END.
                ELSE
                    RELEASE LevAdrButiker.
            END.
            IF AVAIL LevAdrButiker THEN
                ASSIGN cLevAdresse1 = IF TRIM(LevAdrButiker.LevAdresse1) <> "" THEN LevAdrButiker.LevAdresse1 ELSE LevAdrButiker.BuAdr
                       cLevAdresse2 = IF TRIM(LevAdrButiker.LevAdresse1) <> "" THEN LevAdrButiker.LevAdresse2 ELSE ""
                       cLevPostNr   = IF TRIM(LevAdrButiker.LevAdresse1) <> "" THEN LevAdrButiker.LevPostNr ELSE LevAdrButiker.BuPonr
                       cLevPostBoks = IF TRIM(LevAdrButiker.LevAdresse1) <> "" THEN LevAdrButiker.LevPostBoks ELSE ""
                       cLevTelefon  = IF TRIM(LevAdrButiker.LevAdresse1) <> "" THEN LevAdrButiker.LevTelefon ELSE LevAdrButiker.BuTel
                       cLevKontakt  = IF TRIM(LevAdrButiker.LevAdresse1) <> "" THEN LevAdrButiker.LevKontakt ELSE "".
            ELSE
                ASSIGN cLevAdresse1 = Ordre.LevAdresse1 
                       cLevAdresse2 = Ordre.LevAdresse2
                       cLevPostNr   = Ordre.LevPostNr  
                       cLevPostBoks = Ordre.LevPostBoks
                       cLevTelefon  = Ordre.LevTelefon 
                       cLevKontakt  = Ordre.LevKontakt.
        END.

        CREATE tt_Eksport.
        RUN assignTmpTable.

    END.
END. /* ARTIKKEL */
END.   
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterOrd) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterOrd Procedure 
PROCEDURE EksporterOrd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
      BREAK /* Byttett rekkefølge på sortering 
          BY tt_Eksport.OrdreNr
          BY tt_Eksport.LevNr
          */
          BY tt_Eksport.LevNr
          BY tt_Eksport.OrdreNr

          BY tt_Eksport.LevDato
          BY tt_Eksport.BButik
          /* Lagt inn ekstra sortering på artikkel og størrelse. */ 
          BY tt_Eksport.ArtikkelNr
          BY tt_Eksport.Storl:

------------------------------------------------------------------------------*/
DEF VAR bStreamAapen AS LOG  NO-UNDO.

EKSPORTER:
FOR EACH tt_Eksport
    BREAK BY tt_Eksport.BButik
          BY tt_Eksport.LevDato
          BY tt_Eksport.LevNr
          BY tt_Eksport.OrdreNr
          BY tt_Eksport.BestNr
          /* Lagt inn ekstra sortering på artikkel og størrelse. */ 
          BY tt_Eksport.ArtikkelNr
          BY tt_Eksport.Storl:
    IF bStreamAapen = FALSE THEN
    DO:
        OUTPUT STREAM Ut TO VALUE(cTmpFilNavn) NO-ECHO.
        bStreamAapen = TRUE.
        RUN eksportHeader.
    END.

    EXPORT STREAM Ut DELIMITER ";"
        tt_Eksport
        .

END. /* EKSPORTER */

IF bStreamAapen THEN
    OUTPUT STREAM Ut CLOSE.
OS-COMMAND SILENT VALUE("RENAME " + cTmpFilNavn + " " + cFilNavn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-eksportHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksportHeader Procedure 
PROCEDURE eksportHeader :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

EXPORT STREAM Ut DELIMITER ";"
  "iRectype"
  "Ordre.OrdreNr"
  "Ordre.LevNr"
  "Ordre.OrdreStatus"
  "Ordre.EkstId"
  "Ordre.LevMerknad"
  "Ordre.SendtDato"
  "Ordre.Merknad"
  "Ordre.LevAdresse1"
  "Ordre.LevAdresse2"
  "Ordre.LevPostNr"
  "Ordre.LevPostBoks"
  "Ordre.LevTelefon"
  "Ordre.LevKontakt"
  "Ordre.Notat"
  "Ordre.HasteOrdre"

  "BestHode.BestNr"
  "BestHode.BestillingsDato"
  "BestHode.BestStat"
  "BestHode.Merknad"
  "BestHode.Beskrivelse"
  "BestHode.DirekteLev"
  "BestHode.ArtikkelNr"
  "ArtBas.Beskr"
  "ArtBas.LevKod"
  "ArtBas.LevFargKod"
  "BestHode.OrdreNr"
  "BestHode.BestType"
  "BestHode.SendtDato"
  "BestHode.LevDato"
  "BestHode.KjedeAvtale"
  "BestHode.EkstOrdreNr"

  "BestPris.ValPris"
  "BestPris.InnkjopsPris"
  "BestPris.Rab1Kr"
  "BestPris.Rab1%"
  "BestPris.Rab2Kr"
  "BestPris.Rab2%"
  "BestPris.Frakt"
  "BestPris.Frakt%"
  "BestPris.DivKostKr"
  "BestPris.DivKost%"
  "BestPris.Rab3Kr"
  "BestPris.Rab3%"
  "BestPris.DBKr"
  "BestPris.DB%"
  "BestPris.Pris"
  "BestPris.KjedeInnkPris"

  "TT_BestStr.Butik"
  "TT_BestStr.Storl"
  "TT_BestStr.Bestilt"
  "Strekkode.Kode"
  "Strekkode.Bestillingsnummer"
  "BestPris.Varekost"
  /* VPI informasjon. Samme informasjon som legges ut i VPI recorden.   */
  /* Untatt - de tre ArtBas. feltene som er lagt ut lenger oppe i denne */
  /* recorden.                                                          */
  "Artbas.LopNr"
  "Artbas.Beskr"
  "Artbas.BongTekst"
  "Artbas.LevNr"
  "LevBas.LevNamn"
  "Artbas.LevKod"
  "Artbas.LevFargKod"
  "Artbas.LevVareTekst"
  "Avdeling.AvdelingNr"
  "Avdeling.AvdelingNavn"
  "VarGr.Hg"
  "HuvGr.HgBeskr"
  "ArtBas.Vg"
  "VarGr.VgBeskr"
  "VarGr.MomsKod"
  "VarGr.Kost_Proc"
  "Artbas.ArtSlag"
  "Artbas.IndividType"
  "Artbas.OPris"
  "Artbas.lager"
  "Artbas.Storrelser"
  "Artbas.StrTypeID"
  "ArtBas.KjedeRab%"
  "ArtBas.KjedeInnkPris"
  "ArtBas.Gjennomfaktureres"
  "Strekkode.Kode"
  "Strekkode.StrKode"
  "Strekkode.KodeType"
  "Strekkode.HovedNr"
  "Strekkode.IKasse"
  "StrKonv.Storl"
  "Artbas.VgKat"
  "Artbas.GarantiKl"
  "Artbas.ProdNr"
  "Produsent.Beskrivelse"
  "Artbas.VMId"
  "Varemerke.Beskrivelse"
  "Artbas.SaSong"
  "Sasong.SasBeskr"
  "Artbas.Farg"
  "Farg.FarBeskr"
  "Artbas.MatKod"
  "Material.MatBeskr"
  "Artbas.valkod"
  "Valuta.ValKurs"
  "Artbas.LevDato1"
  "Artbas.LevDato2"
  "Artbas.LevDato3"
  "Artbas.LevDato4"
  "Artbas.LinjeMerknad"
  "Artbas.KatalogPris"
  "Artbas.forhRab%"
  "Artbas.supRab%"
  "Artbas.VPIDato"
  "Artbas.KjedeVare"
  "Artbas.VPIBildeKode"
  "Artbas.AntIPakn"
  "Artbas.FrittTillegg"
  "Artbas.VareFakta"
  "Artbas.StrKode1"
  "Artbas.StrKode2"
  "Artbas.EDato"
  "Artbas.ETid"
  "ArtBas.EndretAv"
  "Artbas.RegistrertDato"
  "Artbas.RegistrertTid"
  "Artbas.RegistrertAv"
  "Artbas.ny_dato"
  "Artbas.inn_dato"
  "Artbas.Notat"
  "Artbas.AnonseArtikkel"
  "Artbas.AktivDato"
  "Artbas.AktivAv"
  "Artbas.Aktivert"
  "Artbas.SattPaKampanje"
  "Artbas.OLLager"
  "Artbas.BildeIKasse"
  "Artbas.Pakke"
  "Artbas.Alder"
  "Artbas.HkStyrt"
  "Artbas.LokPris"
  "Artbas.IKasse"
  "Artbas.KjentPaHK"
  "Artbas.BehKode"
  "Artbas.Pakkenr"
  "Artbas.AnbefaltPris"
  "Artbas.KundeRabatt"
  "Artbas.Etikett"
  "Artbas.SalgsEnhet"
  "Artbas.ModellFarge"
  "Artbas.SentralBestilling"
  "Artbas.PrisGrpNr"
  "Artbas.HovedModellFarge"
  "Artbas.Etikettekst1"
  "Artbas.EtiLayout"
  "Artbas.LinkVareNr"
  "Artbas.Mengde"
  "Artbas.ManRabIKas"
  "Artbas.Kommentar"
  "ArtPris.Tilbud"
  "ArtPris.AktivFraDato"
  "ArtPris.AktivFraTid"
  "ArtPris.TilbudFraDato"
  "ArtPris.TilbudTilDato"
  "ArtPris.TilbudFraTid"
  "ArtPris.TilbudTilTid"
  "ArtPris.ValPris[1]"
  "ArtPris.InnkjopsPris[1]"
  "ArtPris.Rab1Kr[1]"
  "ArtPris.Rab1%[1]"
  "ArtPris.Rab2Kr[1]"
  "ArtPris.Rab2%[1]"
  "ArtPris.Frakt[1]"
  "ArtPris.Frakt%[1]"
  "ArtPris.DivKostKr[1]"
  "ArtPris.DivKost%[1]"
  "ArtPris.Rab3Kr[1]"
  "ArtPris.Rab3%[1]"
  "ArtPris.VareKost[1]"
  "ArtPris.MvaKr[1]"
  "ArtPris.Mva%[1]"
  "ArtPris.DBKr[1]"
  "ArtPris.DB%[1]"
  "ArtPris.Pris[1]"
  "ArtPris.ValPris[2]"
  "ArtPris.InnkjopsPris[2]"
  "ArtPris.Rab1Kr[2]"
  "ArtPris.Rab1%[2]"
  "ArtPris.Rab2Kr[2]"
  "ArtPris.Rab2%[2]"
  "ArtPris.Frakt[2]"
  "ArtPris.Frakt%[2]"
  "ArtPris.DivKostKr[2]"
  "ArtPris.DivKost%[2]"
  "ArtPris.Rab3Kr[2]"
  "ArtPris.Rab3%[2]"
  "ArtPris.VareKost[2]"
  "ArtPris.MvaKr[2]"
  "ArtPris.Mva%[2]"
  "ArtPris.DBKr[2]"
  "ArtPris.DB%[2]"
  "ArtPris.Pris[2]"
  "ArtBas.RAvdNr"  /* Vareområde */
  "VareBokLinje.Kjedevare"
  "VarebokLinje.Gjennomfaktureres"
  "StrTStr.SeqNr"  /* For sortering av størrelser */
  "Ordre.OrdreMottaker"

  "Vareboklinje.Innkjopspris"   /* Engros */
  "Vareboklinje.Varekost"       /* Netti.inn.forh*/
  "VarebokLinje.ForhRab%"       /* Rab.forh */
  "VarebokLinje.SupVarekost"    /* Netto innpris supplering */
  "Vareboklinje.SupRab%"        /* Supl.rab */
  "VarebokLinje.Pris"           /* Markedspris */
  "Vareboklinje.AnbefaltPris"   /* Veil.pris */
  "Vareboklinje.Kampanjepris"   /* Kampanje */
  "VarebokLinje.KjedeInnKPris"  /* Kj.innk.pr */
  "VarebokLinje.SortimentKoder" /* Sort (Eks. B1, G) */
  "Vareboklinje.Kampanjeuker"   /* Kamp.uke (Eks. 32, 36,49) */
  "Vareboklinje.Kampanjestotte" /* Støtte (Eks. Magasin, Radio, TV, VG) */
  "VarebokLinje.Sesong"         /* Sesongkoder */
  "Vareboklinje.SasBeskr"       /* Sesong */
  "Vareboklinje.Lagerkoder"     /* Lager (Eks. L1, L2) */
  "Vareboklinje.KjedeValutaPris"
  "Vareboklinje.KjedeProdusent"
  "Strekkode.ERPNr"
  .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FillTT_BestStr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillTT_BestStr Procedure 
PROCEDURE FillTT_BestStr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE TT_BestStr.

    FOR EACH BestStr NO-LOCK WHERE
         BestStr.BestNr = BestHode.BestNr AND BestStr.BestStat = BestHode.BestStat:
        RELEASE TT_BestStr.
        IF BestHode.DirekteLev THEN
            BUFFER-COPY BestStr TO TT_BestStr.
        ELSE DO:
            FIND TT_BestStr WHERE TT_BestStr.BestNr   = BestStr.BestNr   AND
                                  TT_BestStr.Butik    = iCL              AND
                                  TT_BestStr.Storl    = BestStr.Storl    AND
                                  TT_BestStr.BestStat = BestStr.BestStat NO-ERROR.
            IF AVAIL TT_BestStr THEN
                ASSIGN TT_BestStr.Bestilt = TT_BestStr.Bestilt + BestStr.Bestilt.
            ELSE DO:
                BUFFER-COPY BestStr EXCEPT BestStr.Butik TO TT_BestStr
                    ASSIGN TT_BestStr.Butik = iCL.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KopierElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierElogg Procedure 
PROCEDURE KopierElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bElogg   FOR Elogg.
    DEFINE BUFFER erpELogg FOR Elogg.

    DEFINE VARIABLE iTst AS INTEGER    NO-UNDO.
    DO:
        FOR EACH ELogg WHERE ELogg.TabellNavn     = "Ordre"   AND
                             ELogg.EksterntSystem = "ERP" AND 
                             Elogg.EndringsType   = 1       NO-LOCK:
            FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.
            IF NOT AVAIL bElogg THEN
                NEXT.
            ASSIGN iTst = INT(bELogg.Verdier) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE bElogg.
                NEXT.
            END.
            /* Sjekker på mottager for gjennomfakturerte og kjedeleverte */
            IF CAN-FIND(Ordre WHERE ordre.OrdreNr = iTst AND 
                        Ordre.ordrestatus > 1 AND
                        Ordre.OrdreMottaker = ENTRY(piLoop,cMottager)) THEN
                BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.
            ELSE
                NEXT.
/*             FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR. */
/*             IF AVAIL bElogg THEN                                                       */
            DELETE bELogg.
            IF AVAILABLE TT_Elogg THEN
                RELEASE TT_ELogg.
        
/* Old kod -> skapade problem */
/*         FOR EACH ELogg WHERE ELogg.TabellNavn = "Ordre" AND                               */
/*                              ELogg.EksterntSystem = "ERP" NO-LOCK:                        */
/*             ASSIGN iTst = INT(ELogg.Verdier) NO-ERROR.                                    */
/*             IF NOT ERROR-STATUS:ERROR AND CAN-FIND(Ordre WHERE ordre.OrdreNr = iTst) THEN */
/*                 BUFFER-COPY ELogg TO TT_ELogg NO-ERROR.                                   */
/*             FIND bElogg WHERE ROWID(bElogg) = ROWID(Elogg) EXCLUSIVE NO-WAIT NO-ERROR.    */
/*             IF AVAIL bElogg THEN                                                          */
/*                 DELETE bELogg.                                                            */
/*             IF AVAILABLE TT_Elogg THEN                                                    */
/*                 RELEASE TT_ELogg.                                                         */




            /* Logger artikklene i ordrene for overføring til ERP. */
/*             FIND Ordre NO-LOCK WHERE                                                     */
/*                 Ordre.OrdreNr = iTst NO-ERROR.                                           */
/*             IF AVAILABLE Ordre THEN                                                      */
/*             DO:                                                                          */
/*                 ERP-LOGG:                                                                */
/*                 FOR EACH BestHode OF Ordre NO-LOCK:                                      */
/*                     FIND erpELogg WHERE                                                  */
/*                          erpELogg.TabellNavn     = "ArtBas" AND                          */
/*                          erpELogg.EksterntSystem = "ERP"    AND                          */
/*                          erpELogg.Verdier        = STRING(BestHode.ArtikkelNr) NO-ERROR. */
/*                     IF NOT AVAIL erpElogg THEN DO:                                       */
/*                         CREATE erpElogg.                                                 */
/*                         ASSIGN erpELogg.TabellNavn     = "ArtBas"                        */
/*                                erpELogg.EksterntSystem = "ERP"                           */
/*                                erpELogg.Verdier        = STRING(BestHode.ArtikkelNr).    */
/*                     END.                                                                 */
/*                     ASSIGN erpELogg.EndringsType = 1                                     */
/*                            erpELogg.Behandlet    = FALSE.                                */
/*                     RELEASE erpELogg.                                                    */
/*                 END. /* ERP-LOGG */                                                      */
/*             END.                                                                         */
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

