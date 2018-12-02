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


DEFINE INPUT  PARAMETER cEDBSystem         AS CHAR       NO-UNDO.
DEFINE INPUT  PARAMETER cSentrallagerliste AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER iSendBilder        AS INT        NO-UNDO.
DEFINE OUTPUT PARAMETER iAntSlett          AS INTEGER    NO-UNDO.
DEFINE OUTPUT PARAMETER iAntNyEndre        AS INTEGER    NO-UNDO.

DEFINE VARIABLE cSendesDir AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cHKinst    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lHK        AS LOG        NO-UNDO.
DEFINE VARIABLE iCL        AS INT        NO-UNDO.

DEFINE TEMP-TABLE TT_Elogg     NO-UNDO LIKE Elogg.
DEFINE TEMP-TABLE TT_VPIArtBas NO-UNDO LIKE VPIArtBas .

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
         HEIGHT             = 21.48
         WIDTH              = 104.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* HK installasjon */
{syspara.i 1 1 18 cHKinst}
IF CAN-DO("Ja,yes,true,1",cHKInst) 
    THEN lHK = TRUE.
ELSE lHK = FALSE.

/* Eksportkatalog */
{syspara.i 1 1 58 cSendesDir}
IF cSendesDir = '' THEN
DO:
    {syspara.i 1 1 51 cSendesDir}
END.
IF cSendesDir = "" THEN "C:\home\lindbak\sendes\".

/* Sentrallager */
{syspara.i 5 1 1 iCL INT}

/* Initierer butikkliste hvis det er sendt inn blank liste. */
IF cSentrallagerliste = "" THEN
    RUN initButikkListe.

RUN StartEksport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-initButikkListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initButikkListe Procedure 
PROCEDURE initButikkListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Default initiering på HK */        
IF lHK = TRUE THEN
DO:
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Sentrallager = TRUE AND
        Butiker.ApningsDato <> ? AND
        Butiker.NedlagtDato = ? 
        BY Butiker.butik:
        ASSIGN
            cSentrallagerliste = cSentrallagerliste + 
                                 (IF cSentrallagerliste = "" THEN "" ELSE ",") + 
                                 STRING(Butiker.Butik).

    END.
END.

/* Default initiering i butikk */
ELSE DO:
    ASSIGN
        cSentrallagerliste = STRING(iCL).
        
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
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntSlett   AS INTEGER    NO-UNDO.
  DEFINE OUTPUT PARAMETER iAntNyEndre AS INTEGER    NO-UNDO.

  FOR EACH ELogg WHERE 
      ELogg.TabellNavn     = cTabellNavn AND 
      ELogg.EksterntSystem = cEDBSystem:

      CREATE TT_Elogg.
      BUFFER-COPY Elogg TO TT_Elogg.
      RELEASE TT_Elogg.
      ASSIGN Elogg.Behandlet = TRUE
             iAntSlett   = iAntSlett   + IF Elogg.EndringsType = 3 THEN 1 ELSE 0
             iAntNyEndre = iAntNyEndre + IF Elogg.EndringsType = 1 AND ELogg.Verdier <> "ALLE" THEN 1 ELSE 0.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-pakkUtPakke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pakkUtPakke Procedure 
PROCEDURE pakkUtPakke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.

  DEF VAR cVerdier AS CHAR NO-UNDO.
  
  DEF BUFFER bufTT_Elogg FOR TT_Elogg.

  LOOPEN:
  FOR EACH bufTT_Elogg WHERE
      bufTT_ELogg.TabellNavn     = cTabellNavn AND
      bufTT_Elogg.EksterntSystem = cEDBSystem:

      FIND VPIArtBas NO-LOCK WHERE
          VPIArtBAs.EkstVPILevNr = INT(ENTRY(1,bufTT_Elogg.Verdier,CHR(1))) AND
          VPIArtBAs.VareNr       = (ENTRY(2,bufTT_Elogg.Verdier,CHR(1))) NO-ERROR.
      IF NOT AVAILABLE VPIArtBas THEN
          NEXT LOOPEN.

      IF VPIArtBas.Pakke = TRUE THEN
      DO:
          FOR EACH VPIPakkeLinje OF VPIArtBas NO-LOCK:
              ASSIGN cVerdier = buftt_Elogg.Verdier.
              ENTRY(2,cVerdier,CHR(1)) = STRING(VPIPakkeLinje.PKArtikkelNr).

              CREATE TT_Elogg.
              BUFFER-COPY bufTT_Elogg TO TT_Elogg
                  ASSIGN
                  tt_Elogg.Verdier = cVerdier 
                  NO-ERROR.
              /* Hvis pakken inneholder flere linjer fra samme artikkel med ulike størrelser     */
              /* skal det bare skapes elogg post for artikkelen, ikke for artikkel og størrelse. */
              IF ERROR-STATUS:ERROR AND AVAILABLE TT_Elogg THEN
                  DELETE TT_ELogg.
              RELEASE TT_Elogg.
          END.
      END.

  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendStrKonv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStrKonv Procedure 
PROCEDURE SendStrKonv :
/*------------------------------------------------------------------------------
        Purpose:                                                                                                                                          
        Notes:                                                                                                                                            
  ------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* Teller opp antall poster som har størrelser. */
  FOR EACH VPIStrekkode NO-LOCK WHERE
    VPIStrekkode.EkstVPILevNr = piEkstVPILevNr AND
    VPIStrekkode.VareNr       = pcVareNr:
      
    FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN
        ASSIGN
          piAnt = piAnt + 1
          .
  END.

  EXPORT "H" "StrKonv" 1 "1.0" piAnt.
  FOR EACH VPIStrekkode NO-LOCK WHERE
    VPIStrekkode.EkstVPILevNr = piEkstVPILevNr AND
    VPIStrekkode.VareNr       = pcVareNr:
    
    FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = VPIStrekkode.StrKode NO-ERROR.
    
    IF AVAILABLE StrKonv THEN 
    EXPORT     
      StrKonv.StrKode       
      StrKonv.Storl         
      StrKonv.Merknad       
      StrKonv.EDato         
      StrKonv.ETid          
      StrKonv.BrukerID      
      StrKonv.RegistrertDato
      StrKonv.RegistrertTid 
      StrKonv.RegistrertAv
      .
    
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendStrTstr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStrTstr Procedure 
PROCEDURE SendStrTstr :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  FIND VPIArtBas NO-LOCK WHERE
    VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
    VPIArtBas.VareNr       = pcVareNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN 
  DO:

    /* Teller opp antall poster som har størrelser. */
    FOR EACH StrTStr NO-LOCK WHERE
      StrTStr.StrTypeId = VPIArtBAs.StrTypeId:
      ASSIGN
        piAnt = piAnt + 1
        .
    END.

    EXPORT "H" "StrTstr" 1 "1.0" piAnt.
    FOR EACH StrTstr NO-LOCK WHERE
      StrTstr.StrTypeId = VPIArtBas.StrTypeId:
    
      EXPORT     
        StrTStr.StrTypeID     
        StrTStr.SoStorl       
        StrTStr.SeqNr         
        StrTStr.EDato         
        StrTStr.ETid          
        StrTStr.BrukerID      
        StrTStr.RegistrertDato
        StrTStr.RegistrertTid 
        StrTStr.RegistrertAv  
        .
    
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendStrType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendStrType Procedure 
PROCEDURE SendStrType :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  FIND VPIArtBas NO-LOCK WHERE
    VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND
    VPIArtBas.VareNr       = pcVareNr NO-ERROR.
  IF AVAILABLE VPIArtBas THEN 
  DO:
    piAnt = 0.
    FOR EACH StrType NO-LOCK WHERE
      StrType.StrTypeId = VPIArtBas.StrTypeId:
      ASSIGN
         piAnt = piAnt + 1.
    END.

    EXPORT "H" "StrType" 1 "1.0" piAnt.
    FOR EACH StrType NO-LOCK WHERE
      StrType.StrTypeId = VPIArtBas.StrTypeId:
    
      EXPORT     
        StrType.StrTypeID     
        StrType.Beskrivelse   
        StrType.Intervall     
        StrType.Fordeling     
        StrType.KortNavn      
        StrType.EDato         
        StrType.ETid          
        StrType.BrukerID      
        StrType.RegistrertDato
        StrType.RegistrertTid 
        StrType.RegistrertAv  
        StrType.AlfaFordeling
        StrType.Hg
        StrType.AvdelingNr
        .
    
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendVPIArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPIArtBas Procedure 
PROCEDURE sendVPIArtBas :
/*------------------------------------------------------------------------------
  Purpose:     VPIArtBas.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   VPIArtBas
  &SCOPED-DEFINE KeyFelt1 EkstVPILevNr
  &SCOPED-DEFINE KeyFelt2 VareNr
  
  DEFINE VARIABLE cTabellNavn  AS CHARACTER  INIT "{&Tabell}" NO-UNDO.
  DEFINE VARIABLE piLopeNr     AS INT        NO-UNDO.
  DEF    VAR      plArtikkelNr AS DEC        NO-UNDO.
  DEFINE VARIABLE piAntElogg   AS INTEGER    NO-UNDO.
  
  ASSIGN
      piLopeNr     = ? /* Det er alltid ? som skal eksporteres. */
      plArtikkelNr = 0
      piAntElogg   = 0
      .

  EKSPORTER:
  DO:
  
  IF iAntSlett > 0 THEN DO:
      EXPORT "H" cTabellNavn 3 "1.0" iAntSlett.
      CREATE TT_{&Tabell}.
      FOR EACH TT_ELogg WHERE TT_ELogg.TabellNavn = cTabellNavn AND TT_ELogg.EksterntSystem = cEDBSystem
                          AND TT_Elogg.EndringsType = 3:

          ASSIGN 
            TT_{&Tabell}.{&KeyFelt1} = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1)))
            TT_{&Tabell}.{&KeyFelt2} = (ENTRY(2,TT_Elogg.Verdier,CHR(1)))
            .
          EXPORT 
            TT_{&Tabell}.EkstVPILevNr      
            TT_{&Tabell}.VareNr            
            TT_{&Tabell}.Hg                
            TT_{&Tabell}.Vg                
            piLopeNr /*TT_{&Tabell}.LopNr */            
            TT_{&Tabell}.SaSong            
            TT_{&Tabell}.Farg              
            TT_{&Tabell}.Klack             
            TT_{&Tabell}.MatKod            
            TT_{&Tabell}.BildNr            
            TT_{&Tabell}.Beskr             
            TT_{&Tabell}.LevNr             
            TT_{&Tabell}.LevKod            
            TT_{&Tabell}.Tilv-Land         
            TT_{&Tabell}.Kommentar         
            TT_{&Tabell}.Ov-Id             
            TT_{&Tabell}.Last-Id           
            TT_{&Tabell}.Foder-Id          
            TT_{&Tabell}.Inner-Id          
            TT_{&Tabell}.Slit-Id           
            TT_{&Tabell}.Anv-Id            
            TT_{&Tabell}.RabKod            
            TT_{&Tabell}.ProvKod           
            TT_{&Tabell}.ValKod            
            TT_{&Tabell}.Lager             
            TT_{&Tabell}.VMId              
            TT_{&Tabell}.LevFargKod        
            TT_{&Tabell}.Notat             
            TT_{&Tabell}.BongTekst         
            TT_{&Tabell}.AnonseArtikkel    
            TT_{&Tabell}.VgKat             
            TT_{&Tabell}.StrTypeID         
            TT_{&Tabell}.ProdNr            
            TT_{&Tabell}.EDato             
            TT_{&Tabell}.ETid              
            TT_{&Tabell}.BrukerID          
            TT_{&Tabell}.RegistrertDato    
            TT_{&Tabell}.RegistrertTid     
            TT_{&Tabell}.RegistrertAv      
            (IF TT_{&Tabell}.EkstVPILevNr < 100
               THEN TT_{&Tabell}.ArtikkelNr
               ELSE plArtikkelNr)       
            TT_{&Tabell}.Storrelser        
            TT_{&Tabell}.LevDato1          
            TT_{&Tabell}.LevDato2          
            TT_{&Tabell}.DivInfo[ 1]       
            TT_{&Tabell}.DivInfo[ 2]       
            TT_{&Tabell}.DivInfo[ 3]       
            TT_{&Tabell}.DivInfo[ 4]       
            TT_{&Tabell}.DivInfo[ 5]       
            TT_{&Tabell}.DivInfo[ 6]       
            TT_{&Tabell}.DivInfo[ 7]       
            TT_{&Tabell}.DivInfo[ 8]       
            TT_{&Tabell}.DivInfo[ 9]       
            TT_{&Tabell}.DivInfo[10]       
            TT_{&Tabell}.DivInfo[11]       
            TT_{&Tabell}.DivInfo[12]       
            TT_{&Tabell}.DivInfo[13]       
            TT_{&Tabell}.DivInfo[14]       
            TT_{&Tabell}.DivInfo[15]       
            TT_{&Tabell}.DivInfo[16]       
            TT_{&Tabell}.DivInfo[17]       
            TT_{&Tabell}.DivInfo[18]       
            TT_{&Tabell}.DivInfo[19]       
            TT_{&Tabell}.DivInfo[20]       
            TT_{&Tabell}.VisDivInfo[ 1]    
            TT_{&Tabell}.VisDivInfo[ 2]    
            TT_{&Tabell}.VisDivInfo[ 3]    
            TT_{&Tabell}.VisDivInfo[ 4]    
            TT_{&Tabell}.VisDivInfo[ 5]    
            TT_{&Tabell}.VisDivInfo[ 6]    
            TT_{&Tabell}.VisDivInfo[ 7]    
            TT_{&Tabell}.VisDivInfo[ 8]    
            TT_{&Tabell}.VisDivInfo[ 9]    
            TT_{&Tabell}.VisDivInfo[10]    
            TT_{&Tabell}.VisDivInfo[11]    
            TT_{&Tabell}.VisDivInfo[12]    
            TT_{&Tabell}.VisDivInfo[13]    
            TT_{&Tabell}.VisDivInfo[14]    
            TT_{&Tabell}.VisDivInfo[15]    
            TT_{&Tabell}.VisDivInfo[16]    
            TT_{&Tabell}.VisDivInfo[17]    
            TT_{&Tabell}.VisDivInfo[18]    
            TT_{&Tabell}.VisDivInfo[19]    
            TT_{&Tabell}.VisDivInfo[20]    
            TT_{&Tabell}.SattPaKampanje    
            TT_{&Tabell}.OPris             
            TT_{&Tabell}.OLLager           
            TT_{&Tabell}.BildeIKasse       
            TT_{&Tabell}.Pakke             
            TT_{&Tabell}.Alder             
            TT_{&Tabell}.HkStyrt           
            TT_{&Tabell}.LokPris           
            TT_{&Tabell}.IKasse            
            TT_{&Tabell}.ArtikkelNr /* TT_{&Tabell}.HKVareId */         
            TT_{&Tabell}.KjentPaHK         
            TT_{&Tabell}.BehKode           
            TT_{&Tabell}.Pakkenr           
            TT_{&Tabell}.HandKode          
            TT_{&Tabell}.AnbefaltPris      
            TT_{&Tabell}.KundeRabatt       
            TT_{&Tabell}.Etikett           
            TT_{&Tabell}.SalgsEnhet        
            TT_{&Tabell}.Oppdatert         
            TT_{&Tabell}.LokArtikkelNr     
            TT_{&Tabell}.ModellFarge       
            TT_{&Tabell}.SentralBestilling 
            TT_{&Tabell}.PrisGrpNr         
            TT_{&Tabell}.HovedModellFarge  
            TT_{&Tabell}.StrKode1  
            TT_{&Tabell}.StrKode2  
            TT_{&Tabell}.LevVareTekst 
            TT_{&Tabell}.Gjennomfaktureres
            TT_{&Tabell}.KjedeVare
            0 /*TT_{&Tabell}.KorrArtikkelNr*/
            TT_{&Tabell}.UtvidetSok
            TT_{&Tabell}.Etikettekst1
            TT_{&Tabell}.Etikettekst2
            /* Nye felt 17/4-08 */
            TT_{&Tabell}.forhRab%
            TT_{&Tabell}.suppRab%
            TT_{&Tabell}.KatalogPris
            TT_{&Tabell}.Linjemerknad
            TT_{&Tabell}.LevDato3
            TT_{&Tabell}.LevDato4
            TT_{&Tabell}.VPIDato
            TT_{&Tabell}.VPIBildeKode
            TT_{&Tabell}.LinkVareNr
            TT_{&Tabell}.Mengde
            TT_{&Tabell}.ManRabIKas
            TT_{&Tabell}.ArtSlag
            TT_{&Tabell}.IndividType
            TT_{&Tabell}.Pant
            TT_{&Tabell}.BestForslag
            TT_{&Tabell}.GarantiKl
            TT_{&Tabell}.AntIPkn
            TT_{&Tabell}.BehStatus
            TT_{&Tabell}.ArtStatus
            TT_{&Tabell}.KorrStatus
            TT_{&Tabell}.Lokasjon
            TT_{&Tabell}.RAvdNr
            .
        DELETE TT_ELogg.
      END.
  END.
  IF iAntNyEndre > 0 THEN 
      NY_ENDRE:
      DO:
     FOR_TTELOGG: 
     FOR EACH TT_ELogg WHERE 
         TT_ELogg.TabellNavn     = cTabellNavn AND 
         TT_ELogg.EksterntSystem = cEDBSystem AND 
         TT_Elogg.EndringsType   = 1:
  
          piAntElogg = piAntElogg + 1.
          
          FIND {&Tabell} WHERE 
            {&Tabell}.{&KeyFelt1} = INT(ENTRY(1,TT_Elogg.Verdier,CHR(1))) AND
            {&Tabell}.{&KeyFelt2} = (ENTRY(2,TT_Elogg.Verdier,CHR(1))) 
            NO-LOCK NO-ERROR.

          IF AVAIL {&Tabell} THEN
          EKSPORT_TABELL:
          DO:
            EXPORT "H" cTabellNavn 1 "1.0" 1.
            EXPORT 
            {&Tabell}.EkstVPILevNr      
            {&Tabell}.VareNr            
            {&Tabell}.Hg                
            {&Tabell}.Vg                
            {&Tabell}.LopNr             
            {&Tabell}.SaSong            
            {&Tabell}.Farg              
            {&Tabell}.Klack             
            {&Tabell}.MatKod            
            {&Tabell}.BildNr            
            {&Tabell}.Beskr             
            {&Tabell}.LevNr             
            {&Tabell}.LevKod            
            {&Tabell}.Tilv-Land         
            {&Tabell}.Kommentar         
            {&Tabell}.Ov-Id             
            {&Tabell}.Last-Id           
            {&Tabell}.Foder-Id          
            {&Tabell}.Inner-Id          
            {&Tabell}.Slit-Id           
            {&Tabell}.Anv-Id            
            {&Tabell}.RabKod            
            {&Tabell}.ProvKod           
            {&Tabell}.ValKod            
            {&Tabell}.Lager             
            {&Tabell}.VMId              
            {&Tabell}.LevFargKod        
            {&Tabell}.Notat             
            {&Tabell}.BongTekst         
            {&Tabell}.AnonseArtikkel    
            {&Tabell}.VgKat             
            {&Tabell}.StrTypeID         
            {&Tabell}.ProdNr            
            {&Tabell}.EDato             
            {&Tabell}.ETid              
            {&Tabell}.BrukerID          
            {&Tabell}.RegistrertDato    
            {&Tabell}.RegistrertTid     
            {&Tabell}.RegistrertAv      
            {&Tabell}.ArtikkelNr        
            {&Tabell}.Storrelser        
            {&Tabell}.LevDato1          
            {&Tabell}.LevDato2          
            {&Tabell}.DivInfo[ 1]       
            {&Tabell}.DivInfo[ 2]       
            {&Tabell}.DivInfo[ 3]       
            {&Tabell}.DivInfo[ 4]       
            {&Tabell}.DivInfo[ 5]       
            {&Tabell}.DivInfo[ 6]       
            {&Tabell}.DivInfo[ 7]       
            {&Tabell}.DivInfo[ 8]       
            {&Tabell}.DivInfo[ 9]       
            {&Tabell}.DivInfo[10]       
            {&Tabell}.DivInfo[11]       
            {&Tabell}.DivInfo[12]       
            {&Tabell}.DivInfo[13]       
            {&Tabell}.DivInfo[14]       
            {&Tabell}.DivInfo[15]       
            {&Tabell}.DivInfo[16]       
            {&Tabell}.DivInfo[17]       
            {&Tabell}.DivInfo[18]       
            {&Tabell}.DivInfo[19]       
            {&Tabell}.DivInfo[20]       
            {&Tabell}.VisDivInfo[ 1]    
            {&Tabell}.VisDivInfo[ 2]    
            {&Tabell}.VisDivInfo[ 3]    
            {&Tabell}.VisDivInfo[ 4]    
            {&Tabell}.VisDivInfo[ 5]    
            {&Tabell}.VisDivInfo[ 6]    
            {&Tabell}.VisDivInfo[ 7]    
            {&Tabell}.VisDivInfo[ 8]    
            {&Tabell}.VisDivInfo[ 9]    
            {&Tabell}.VisDivInfo[10]    
            {&Tabell}.VisDivInfo[11]    
            {&Tabell}.VisDivInfo[12]    
            {&Tabell}.VisDivInfo[13]    
            {&Tabell}.VisDivInfo[14]    
            {&Tabell}.VisDivInfo[15]    
            {&Tabell}.VisDivInfo[16]    
            {&Tabell}.VisDivInfo[17]    
            {&Tabell}.VisDivInfo[18]    
            {&Tabell}.VisDivInfo[19]    
            {&Tabell}.VisDivInfo[20]    
            {&Tabell}.SattPaKampanje    
            {&Tabell}.OPris             
            {&Tabell}.OLLager           
            {&Tabell}.BildeIKasse       
            {&Tabell}.Pakke             
            {&Tabell}.Alder             
            {&Tabell}.HkStyrt           
            {&Tabell}.LokPris           
            {&Tabell}.IKasse            
            {&Tabell}.HKVareId          
            {&Tabell}.KjentPaHK         
            {&Tabell}.BehKode           
            {&Tabell}.Pakkenr           
            {&Tabell}.HandKode          
            {&Tabell}.AnbefaltPris      
            {&Tabell}.KundeRabatt       
            {&Tabell}.Etikett           
            {&Tabell}.SalgsEnhet        
            {&Tabell}.Oppdatert         
            {&Tabell}.LokArtikkelNr     
            {&Tabell}.ModellFarge       
            {&Tabell}.SentralBestilling 
            {&Tabell}.PrisGrpNr         
            {&Tabell}.HovedModellFarge  
            {&Tabell}.StrKode1  
            {&Tabell}.StrKode2  
            {&Tabell}.LevVareTekst  
            {&Tabell}.Gjennomfaktureres
            {&Tabell}.KjedeVare
            0 /*{&Tabell}.KorrArtikkelNr*/
            {&Tabell}.UtvidetSok
            {&Tabell}.Etikettekst1
            {&Tabell}.Etikettekst2
            /* Nye felt 17/4-08 */
            {&Tabell}.forhRab%
            {&Tabell}.suppRab%
            {&Tabell}.KatalogPris
            {&Tabell}.Linjemerknad
            {&Tabell}.LevDato3
            {&Tabell}.LevDato4
            {&Tabell}.VPIDato
            {&Tabell}.VPIBildeKode
            {&Tabell}.LinkVareNr
            {&Tabell}.Mengde
            {&Tabell}.ManRabIKas
            {&Tabell}.ArtSlag
            {&Tabell}.IndividType
            {&Tabell}.Pant
            {&Tabell}.BestForslag
            {&Tabell}.GarantiKl
            {&Tabell}.AntIPkn
            {&Tabell}.BehStatus
            {&Tabell}.ArtStatus
            {&Tabell}.KorrStatus
            {&Tabell}.Lokasjon
            {&Tabell}.RAvdNr
            {&Tabell}.EkstStrTypeNavn
             .
            RUN SendVPIArtPris          ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).            
            RUN SendVPIStrekkode        ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).            
            RUN SendVPIBildeRegister    ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).            
            RUN SendVPIBildeData        ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).            
            RUN SendVPIErstattningsvare ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).            
            RUN SendVPIPakkelinje       ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr). 
            RUN SendStrType             ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).            
            RUN SendStrTstr             ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).                                               
            RUN SendStrKonv             ({&Tabell}.EkstVPILevNr,{&Tabell}.VareNr).
            
            DELETE TT_ELogg.
            IF piAntElogg > 1000 THEN 
                LEAVE EKSPORTER.
          END. /* EKSPORT_TABELL */
      END. /* FOR_TTELOGG */
  END. /* NY_ENDRE */
  END. /* EKSPORTER*/
  
  &UNDEFINE Tabell
  &UNDEFINE KeyFelt1
  &UNDEFINE KeyFelt2
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendVPIArtPris) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPIArtPris Procedure 
PROCEDURE sendVPIArtPris :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* Teller opp antall poster. */
  FOR EACH VPIArtPris NO-LOCK WHERE
    VPIArtPris.EkstVPILevNr = piEkstVPILevNr AND
    VPIArtPris.VareNr       = pcVareNr:
    ASSIGN
      piAnt = piAnt + 1
      .
  END.

  EXPORT "H" "VPIArtPris" 1 "1.0" piAnt.
  FOR EACH VPIArtPris NO-LOCK WHERE
    VPIArtPris.EkstVPILevNr = piEkstVPILevNr AND
    VPIArtPris.VareNr       = pcVareNr:
    EXPORT 
      VPIArtPris.EkstVPILevNr    
      VPIArtPris.VareNr          
      VPIArtPris.EDato           
      VPIArtPris.ETid            
      VPIArtPris.BrukerID        
      VPIArtPris.RegistrertDato  
      VPIArtPris.RegistrertTid   
      VPIArtPris.RegistrertAv    
      VPIArtPris.ProfilNr        
      VPIArtPris.ArtikkelNr      
      VPIArtPris.Tilbud          
      VPIArtPris.VareKost[1]     
      VPIArtPris.VareKost[2]     
      VPIArtPris.MvaKr[1]        
      VPIArtPris.MvaKr[2]        
      VPIArtPris.LevNr           
      VPIArtPris.EuroManuel      
      VPIArtPris.ValPris[1]      
      VPIArtPris.ValPris[2]      
      VPIArtPris.Rab1Kr[1]       
      VPIArtPris.Rab1Kr[2]       
      VPIArtPris.Rab1%[1]        
      VPIArtPris.Rab1%[2]        
      VPIArtPris.Rab2Kr[1]       
      VPIArtPris.Rab2Kr[2]       
      VPIArtPris.Rab2%[1]        
      VPIArtPris.Rab2%[2]        
      VPIArtPris.Frakt[1]        
      VPIArtPris.Frakt[2]        
      VPIArtPris.Frakt%[1]       
      VPIArtPris.Frakt%[2]       
      VPIArtPris.DivKostKr[1]    
      VPIArtPris.DivKostKr[2]    
      VPIArtPris.DivKost%[1]     
      VPIArtPris.DivKost%[2]     
      VPIArtPris.Rab3Kr[1]       
      VPIArtPris.Rab3Kr[2]       
      VPIArtPris.Rab3%[1]        
      VPIArtPris.Rab3%[2]        
      VPIArtPris.DBKr[1]         
      VPIArtPris.DBKr[2]         
      VPIArtPris.DB%[1]          
      VPIArtPris.DB%[2]          
      VPIArtPris.EuroPris[1]     
      VPIArtPris.EuroPris[2]     
      VPIArtPris.InnkjopsPris[1] 
      VPIArtPris.InnkjopsPris[2] 
      VPIArtPris.Mva%[1]         
      VPIArtPris.Mva%[2]         
      VPIArtPris.Pris[1]         
      VPIArtPris.Pris[2]         
      VPIArtPris.AktivFraDato    
      VPIArtPris.AktivFraTid     
      VPIArtPris.TilbudFraDato   
      VPIArtPris.TilbudTilDato   
      VPIArtPris.TilbudFraTid    
      VPIArtPris.TilbudTilTid    
      VPIArtPris.TilbudTimeStyrt 
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendVPIBildeData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPIBildeData Procedure 
PROCEDURE sendVPIBildeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* NB NB NB VPI fil til butikk ble for stor. */
  /* Vi tar derfor ikke med bilder.            */
  IF iSendBilder = 0 THEN RETURN.

  /* Teller opp antall poster. */
  FOR EACH VPIBildeData NO-LOCK WHERE
    VPIBildeData.EkstVPILevNr = piEkstVPILevNr AND
    VPIBildeData.VareNr       = pcVareNr:
    ASSIGN
      piAnt = piAnt + 1
      .
  END.

  EXPORT "H" "VPIBildeData" 1 "1.0" piAnt.
  FOR EACH VPIBildeData NO-LOCK WHERE
    VPIBildeData.EkstVPILevNr = piEkstVPILevNr AND
    VPIBildeData.VareNr       = pcVareNr:
    EXPORT
        VPIBildeData.EkstVPILevNr
        VPIBildeData.VareNr
        VPIBildeData.BildNr
        VPIBildeData.Teller
        VPIBildeData.RawData
        VPIBildeData.EDato
        VPIBildeData.ETid
        VPIBildeData.BrukerID
        VPIBildeData.RegistrertDato
        VPIBildeData.RegistrertTid
        VPIBildeData.RegistrertAv
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendVPIBildeRegister) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPIBildeRegister Procedure 
PROCEDURE sendVPIBildeRegister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* NB NB NB VPI fil til butikk ble for stor. */
  /* Vi tar derfor ikke med bilder.            */
  IF iSendBilder = 0 THEN RETURN.

  /* Teller opp antall poster. */
  FOR EACH VPIBildeRegister NO-LOCK WHERE
    VPIBildeRegister.EkstVPILevNr = piEkstVPILevNr AND
    VPIBildeRegister.VareNr       = pcVareNr:
    ASSIGN
      piAnt = piAnt + 1
      .
  END.

  EXPORT "H" "VPIBildeRegister" 1 "1.0" piAnt.
  FOR EACH VPIBildeRegister NO-LOCK WHERE
    VPIBildeRegister.EkstVPILevNr = piEkstVPILevNr AND
    VPIBildeRegister.VareNr       = pcVareNr:
    EXPORT
        VPIBildeRegister.EkstVPILevNr
        VPIBildeRegister.VareNr
        VPIBildeRegister.BildNr
        VPIBildeRegister.Merknad
        VPIBildeRegister.Tekst
        VPIBildeRegister.FilNavn
        VPIBildeRegister.RegistrertDato
        VPIBildeRegister.Dato
        VPIBildeRegister.Notat
        VPIBildeRegister.LevArtNr
        VPIBildeRegister.LevNr
        VPIBildeRegister.RegistrertTid
        VPIBildeRegister.Tid
        VPIBildeRegister.Sted
        VPIBildeRegister.EDato
        VPIBildeRegister.ETid
        VPIBildeRegister.BrukerID
        VPIBildeRegister.EksterntID
        VPIBildeRegister.RegistrertAv
        VPIBildeRegister.DokumentNr
        VPIBildeRegister.Bytes
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendVPIErstattningsvare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPIErstattningsvare Procedure 
PROCEDURE sendVPIErstattningsvare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* Teller opp antall poster. */
  FOR EACH VPIErstattningsvare NO-LOCK WHERE
    VPIErstattningsvare.EkstVPILevNr = piEkstVPILevNr AND
    VPIErstattningsvare.VareNr       = pcVareNr:
    ASSIGN
      piAnt = piAnt + 1
      .
  END.

  EXPORT "H" "VPIErstattningsvare" 1 "1.0" piAnt.
  FOR EACH VPIErstattningsvare NO-LOCK WHERE
    VPIErstattningsvare.EkstVPILevNr = piEkstVPILevNr AND
    VPIErstattningsvare.VareNr       = pcVareNr:
    EXPORT 
      VPIErstattningsvare.ErstattId    
      VPIErstattningsvare.ArtikkelNr   
      VPIErstattningsvare.EkstVPILevNr 
      VPIErstattningsvare.VareNr       
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sendVPIPakkelinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendVPIPakkelinje Procedure 
PROCEDURE sendVPIPakkelinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* Teller opp antall poster. */
  FOR EACH VPIPakkelinje NO-LOCK WHERE
    VPIPakkelinje.EkstVPILevNr = piEkstVPILevNr AND
    VPIPakkelinje.VareNr       = pcVareNr:
    ASSIGN
      piAnt = piAnt + 1
      .
  END.

  EXPORT "H" "VPIPakkelinje" 1 "1.0" piAnt.
  FOR EACH VPIPakkelinje NO-LOCK WHERE
    VPIPakkelinje.EkstVPILevNr = piEkstVPILevNr AND
    VPIPakkelinje.VareNr       = pcVareNr:
    EXPORT 
      VPIPakkelinje.ArtikkelNr     
      VPIPakkelinje.PkArtikkelNr   
      VPIPakkelinje.StrKode           
      VPIPakkelinje.Antall         
      VPIPakkelinje.EDato          
      VPIPakkelinje.ETid           
      VPIPakkelinje.BrukerID       
      VPIPakkelinje.RegistrertDato 
      VPIPakkelinje.RegistrertTid  
      VPIPakkelinje.RegistrertAv   
      VPIPakkelinje.Pakkenr        
      VPIPakkelinje.EkstVPILevNr   
      VPIPakkelinje.VareNr  
      VPIPakkeLinje.VareKost
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SendVPIStrekkode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SendVPIStrekkode Procedure 
PROCEDURE SendVPIStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piEkstVPILevNr AS INT  NO-UNDO.
  DEF INPUT PARAMETER pcVareNR       AS CHAR NO-UNDO.

  DEF VAR piAnt AS INT NO-UNDO.

  /* Teller opp antall poster. */
  FOR EACH VPIStrekkode NO-LOCK WHERE
    VPIStrekkode.EkstVPILevNr = piEkstVPILevNr AND
    VPIStrekkode.VareNr       = pcVareNr:
    ASSIGN
      piAnt = piAnt + 1
      .
  END.

  EXPORT "H" "VPIStrekkode" 1 "1.0" piAnt.
  FOR EACH VPIStrekkode NO-LOCK WHERE
    VPIStrekkode.EkstVPILevNr = piEkstVPILevNr AND
    VPIStrekkode.VareNr       = pcVareNr:
    EXPORT 
      VPIStrekkode.EkstVPILevNr    
      VPIStrekkode.VareNr          
      VPIStrekkode.Kode          
      VPIStrekkode.StrKode       
      VPIStrekkode.KodeType      
      VPIStrekkode.VareId        
      VPIStrekkode.HovedNr       
      VPIStrekkode.EDato         
      VPIStrekkode.ETid          
      VPIStrekkode.BrukerID      
      VPIStrekkode.RegistrertDato
      VPIStrekkode.RegistrertTid 
      VPIStrekkode.RegistrertAv  
      VPIStrekkode.EkstStorl     
      VPIStrekkode.Storl
      VPIStrekkode.Bestillingsnummer
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SlettElogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettElogg Procedure 
PROCEDURE SlettElogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cTabellNavn AS CHARACTER  NO-UNDO.

  FOR EACH ELogg WHERE 
      ELogg.TabellNavn     = cTabellNavn AND 
      ELogg.EksterntSystem = cEDBSystem AND
      Elogg.Behandlet      = TRUE:

      DELETE ELogg.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-StartEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartEksport Procedure 
PROCEDURE StartEksport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount             AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cFilnavn           AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cNumericFormat     AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cDateFormat        AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cButListe          AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE piLoop             AS INT        NO-UNDO.

  DEF BUFFER bufButiker FOR Butiker.
  ASSIGN cNumericFormat         = SESSION:NUMERIC-FORMAT
         cDateFormat            = SESSION:DATE-FORMAT
         SESSION:NUMERIC-FORMAT = "EUROPEAN"
         SESSION:DATE-FORMAT    = "dmy".

  /* Ingen butikker å sende til ??? */
  IF cSentrallagerliste = "" THEN 
      RETURN.


  EMPTY TEMP-TABLE TT_Elogg.  
  EMPTY TEMP-TABLE TT_VPIArtBas.

  RUN KopierElogg ('VPIArtBas',OUTPUT iAntSlett,OUTPUT iAntNyEndre).
  RUN pakkUtPakke  ('VPIArtBas').

  EKSPORTLOOP:
  DO WHILE CAN-FIND(FIRST TT_ELogg):

      ASSIGN 
          piLoop = piLoop + 1
          cFilnavn = (IF lHK THEN "HKVPI"
                      ELSE "POSVPI") + 
                     STRING(YEAR(TODAY)) + 
                     STRING(MONTH(TODAY),"99") + 
                     STRING(DAY(TODAY),"99") + 
                     "_" + 
                     REPLACE(STRING(TIME,"HH:MM:SS"),":","") + 
                     "_" + STRING(piLoop)
          cFilnavn = RIGHT-TRIM(cSendesDir,"\") + "\" + cFilnavn.
      OUTPUT TO VALUE(cFilnavn) APPEND.
      RUN SendVPIArtBas.
      OUTPUT CLOSE.

      PAUSE 1 NO-MESSAGE. /* Denne er lagt inn for å sikre at filene kommer ut. Tas den bort, kommer bare noen filer ut hvis det sendes mange filer. */
      
      FILE-INFO:FILE-NAME = cFilnavn.
      IF FILE-INFO:FILE-SIZE > 0 THEN 
      DO:
          DO iCount = 1 TO NUM-ENTRIES(cSentrallagerliste):
              FIND bufButiker NO-LOCK WHERE
                  bufButiker.Butik = INT(ENTRY(iCount,cSentrallagerliste)) NO-ERROR.
              IF NOT AVAILABLE bufButiker THEN
                  NEXT.
              OS-COPY VALUE(cFilnavn) VALUE(cFilnavn + "." + ENTRY(iCount,cSentrallagerliste)).
          END.
          OS-DELETE VALUE(cFilnavn).
      END.
      ELSE DO: 
        OS-DELETE VALUE(cFilnavn).
      END.
  END. /* EKSPORTLOOP */

  ASSIGN SESSION:NUMERIC-FORMAT = cNumericFormat
         SESSION:DATE-FORMAT    = cDateFormat.
  RUN SlettElogg ('VPIArtBas').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

