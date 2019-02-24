&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : sysinit.p
    Purpose     : Initiering av systemregistre som alltid har faste verdier.

    Syntax      : run sysinit.p

    Description :

    Author(s)   :  Tom Nøkleby
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR cVersion          AS CHAR NO-UNDO.
DEF VAR dDato             AS DATE NO-UNDO.
DEF VAR piLoop            AS INT  NO-UNDO.
DEF VAR iCl               AS INT  NO-UNDO.
DEF VAR iOpprettKundekort AS INT  NO-UNDO.
DEF VAR lHK               AS LOG  NO-UNDO.
DEF VAR cTekst            AS CHAR NO-UNDO.

DEFINE BUFFER clButiker   FOR Butiker.
DEFINE BUFFER bVPIFilType FOR VPIFilType.
DEFINE BUFFER bEkstVPIFil FOR EkstVPIFil.
DEFINE BUFFER bSysHode    FOR SysHode.

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
         HEIGHT             = 45.71
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN skoversion.w (OUTPUT cVersion, OUTPUT dDato).

{syspara.i 5 1 1 iCl INT}
{syspara.i 1 1 18 cTekst}
IF CAN-DO("J,Ja,Yes,True,1",cTekst) THEN
    lHK = TRUE.
ELSE
    lHK = FALSE.

FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL.

/* Oppdatering av parametrene skal bare kjøres en gang pr. versjon. */           
FIND FIRST syspara NO-LOCK WHERE
    syspara.syshid = 1 AND
    syspara.sysgr  = 101 AND
    syspara.Parameter1 = cVersion NO-ERROR.
IF AVAILABLE SysPara THEN
    RETURN.
ELSE DO TRANSACTION:
        FIND LAST SysPara NO-LOCK WHERE
            SysPara.SysHId = 1 AND
            SysPara.SysGr  = 101 USE-INDEX SysPara NO-ERROR.
        IF AVAILABLE SysPara THEN
            piLoop = SysPAra.ParaNr + 1.
        ELSE
            piLoop = 1.
        CREATE SysPara.
        ASSIGN
            syspara.syshid      = 1 
            syspara.sysgr       = 101 
            sysPara.ParaNr      = piLoop 
            SysPara.Beskrivelse = "SE Versjonsnummer"
            syspara.Parameter1  = cVersion 
            SysPara.Parameter2  = STRING(dDato) + " " + STRING(TIME,"HH:MM:SS")
            NO-ERROR.
            .
        RELEASE SysPara.
    END. /* TRANSACTION */

RUN sysinit2.p.
RUN sjekk_og_fiks_strtypeid_0.
RUN initbbskorttabell.p.
RUN Initiering.
RUN SysParaBudsjett.
/*RUN slettGamleSyspara.*/
RUN setPubBruker.
RUN setHlpFil.
RUN setPakningsenhet.
RUN SysParaVareSlag.
RUN setSysParaMeny.
RUN setSysparaBatch.
RUN setGavekorttype.
RUN setTransaksjonstyper.
RUN setGyldighetTilgode.
RUN setBetalingsbetingelser.
RUN setBilagsart.
RUN setBilagstype.
RUN setPurretrinn.
RUN setLeveringsform.
RUN setFakturalayout.
RUN setKOrdreLayout.
RUN setFakturaEksport.
RUN setGarantiklasser.
RUN sjekkFaktura.
RUN SjekkBatchStatus.
RUN setEtikettFraKasse.
RUN setEtikettBatchFraKasse.
RUN setEDBDataType.
RUN setEkstEDBSystem.
RUN setBestillingStatus.
RUN setOrdreStatus.
RUN setFakturatekst.
RUN setHarbutikksystem. /* Systemparameter 1 101 1 */
RUN setKassemodell.
RUN setVPIFil.
RUN setVPIFil2.
RUN setFakturaparametre.
RUN setSysparaMottakskontroll.
RUN setOppdaterEkstErp.
RUN setVareBokParam.
RUN setKundeordrestatus.
/* TN 30/12-18 Setter ny kundeordrestatus */
RUN OpprettNyeKundeordrestatus.p.

RUN SysParaOverforingsbilag.
RUN setSysParaOverforing.
RUN SysParaTilfeldigVare.
RUN setSysParaBestillingVedNyArtikkel.
RUN setInitAvdeling.
RUN initTommeRegistre.
RUN setSysPara2av5Interleave.
RUN setStartArtikelSokFelt.
RUN setHandteringskode.
RUN setKundespesifike.
RUN SysParaSuppleringsOrdre.
RUN slettStrKonv.
RUN setSysParaSuppleringsordre.
RUN setSysParaMedlem.
RUN setSysParaPakkseddel.
RUN setKamStatistikk.
RUN setSysParaBehandlingskode.
RUN setKupongtyper.
RUN setKampanjetilbTyper.
RUN setKampanjeRabattTyper.
RUN setKampanjeParametre.
RUN setSysParaEtikett82.
RUN setTeamType.
RUN setVPIBehandlingsStatus.
RUN setKorrVPIBehandlingsStatus.
RUN setVPIMottakStatus.
RUN setVPIMottakTyper.
RUN setKorrVPILev.
RUN setSysParaSlettArtikkel.
RUN setHTType.
RUN setSysParaSkomodus.
RUN setOverforingsparametreArtikkelkort.
RUN setSyspareOrdreBestilling.
RUN setKundeButikkNr.
RUN initAvJukebox.
RUN initPlListeType.
RUN setRapportgenParametre.
RUN setArtkortfaner.
RUN slettJBoxUserSetting.
RUN setArtkortEgenskaper.
RUN initRegnskapsavdeling.
RUN initArtikkelVPIInformasjon.
RUN initImpHode.
RUN setKundeordreKasse.
RUN setVPIMottakFraHK.
RUN setVPIImport.
RUN setLoggingAvBatchserver.
RUN initReklamasjonslinjeVgLopnr.
RUN setSysParaVaretellingslister.
RUN setSysParaAnalyser.
RUN setPRSPos_Parametre.
RUN setFilKontrollParametre.
RUN setSysparaGant.
RUN setPRSOverforing.
RUN setLinkVareant.
RUN setOnLineLeverandor.
RUN setMayFlower.
RUN setSysParaNets_MainCard.
RUN setSysParaEksportPricat.

RUN setSysParaPakkseddelmottak.
RUN setSysParaKreditkortNavn.
RUN setSysParaHTutlegg.
RUN setSysParaSmtpmail.
RUN setsysparaReklamasjon.
RUN setSysParaEANNrSerie.
RUN setSysParaABCArtikkelrapport.
RUN setSysParaNettbutikk.
RUN setSysparaVareType.
RUN setSysparaTilHK.
RUN setSalgsEnhet.
RUN setJamforEnhet.
RUN setSysParaEANHandtering.
RUN initArtBasUtgatt.
RUN setSysParaLeverandor.
RUN setKasseDatoImportFormat.
RUN setSysparaExcelEkstent.
RUN setSysparaDampbageriet.
RUN setSysParaOn-LineKassarapport.
RUN setSysparaEksportImportKasse.
RUN setKundeSpesifikkeParametre.
RUN setEkstVPITabellSystem.
RUN setSIETransParam.
RUN setBokforingsbilag.
RUN setMedlemsrabatt.
RUN setRabSjekkType.
RUN setTimeGripParam.
RUN setPrisregister.
RUN setBilderegister.
RUN setVagabondEksport.
RUN rensButikkKobling.
RUN setXmlVPIImport.
RUN setButikkGLNNr.
/* selvstendige setup rutiner. */
RUN setPRSImportSetup.p.

/* Fix rutiner */
RUN settAktiveTranstyper.p.
RUN fixFakturaHodeBilagstype.
RUN NullstillVaretellingimport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-fixFakturaHodeBilagstype) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixFakturaHodeBilagstype Procedure 
PROCEDURE fixFakturaHodeBilagstype :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cListe AS CHAR NO-UNDO.
DEF VAR cNavn  AS CHAR NO-UNDO.

ASSIGN
    cListe = "1,2,3,4,5,6,10,11"
    cNavn  = "Faktura,Kreditnota,Innbetaling,aKonto,Utbetaling,Debet korr.postering,Purring,Purregebyr"
    .

DO piLoop = 1 TO NUM-ENTRIES(cListe):
  FOR EACH FakturaHode WHERE
    FakturaHode.BTTEkst = entry(piLoop,cNavn):
    
    IF FakturaHode.BilagsType <> int(ENTRY(piLoop,cListe)) THEN 
      FakturaHode.BilagsType = int(ENTRY(piLoop,cListe)).
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initArtBasUtgatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initArtBasUtgatt Procedure 
PROCEDURE initArtBasUtgatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE*/
/*    ArtBas.Aktivert = FALSE:          */
/*    ASSIGN                            */
/*      ArtBas.Utgatt     = TRUE        */
/*      ArtBas.UtgattDato = AktivDato   */
/*      .                               */
/*  END.                                */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initArtikkelVPIInformasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initArtikkelVPIInformasjon Procedure 
PROCEDURE initArtikkelVPIInformasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Initiering av VPI informasjonen skal ikke kjøres på HK. */
  IF lHK THEN RETURN.

  /* TN 26/11-08 Dette er unødvending. HK VPI import fikser dette.
  /* Frisker opp VPI informasjonen i artikelkortet fra VPI registeret. */
  FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
      ArtBas.ArtikkelNr >= 8500000:

      FIND VPIArtBas NO-LOCK WHERE
          VPIArtBas.EkstVPILevNr = 1 AND
          VPIArtBas.VareNr       = STRING(ArtBas.ArtikkelNr) NO-ERROR.
      IF AVAILABLE VPIArtBas THEN
      DO:
          ASSIGN
              ArtBas.AnbefaltPris = VPIArtBas.AnbefaltPris
              ArtBas.KatalogPris  = VPIArtBas.KatalogPris[1]
              ArtBas.forhRab%     = VPIArtBas.forhRab%[1]
              ArtBas.supRab%      = VPIArtBas.suppRab%[1]
              ArtBas.VPIDato      = VPIArtBas.VPIDato
              .
      END.
  END.
  */
                                                                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initAvJukebox) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initAvJukebox Procedure 
PROCEDURE initAvJukebox :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:

      /* Company settes lik sentrallager. */
      FIND FIRST JBoxCompany EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE JBoxCompany THEN
      DO:
          CREATE JBoxCompany.
          ASSIGN
              JBoxCompany.iJBoxCompanyId = 1
              JBoxCompany.CL             = iCL
              JBoxCompany.cCompanyName   = IF AVAILABLE clButiker
                                             THEN clButiker.ButNamn
                                             ELSE "Ukjent butikk"
              .
          RELEASE JBoxCompany.
      END.

      /* Oppretter Tomn */
      IF NOT CAN-FIND(JBoxUser WHERE
                      JBoxUser.cJBoxUserId = "tomn") THEN
      DO:
          CREATE JBoxUser.
          ASSIGN
              JBoxUser.cJBoxUserId = "tomn"
              JBoxUser.cUserName   = "Tom"
              .
      END.

      /* Alle eksisterende brukere skal også logges */
      FOR EACH Bruker NO-LOCK:
          IF NOT CAN-FIND(FIRST JBoxUser WHERE 
                          JBoxUser.cJBoxUserId = Bruker.BrukerId) THEN
          DO:
              CREATE JBoxUser.
              ASSIGN
                  JBoxUser.cJBoxUserId = Bruker.BrukerId
                  JBoxUser.cUserName   = Bruker.Navn
                  .
          END.
      END.

      /* Oppretter kobling til Company */
      FOR EACH JBoxUser NO-LOCK:
          IF NOT CAN-FIND(FIRST JBoxCompanyUser WHERE
                          JBoxCompanyUser.iJBoxCompanyId = iCL AND
                          JBoxCompanyUser.cJBoxUserId    = JBoxUser.cJBoxUserId)  THEN
          DO:
              CREATE JBoxCompanyUser.
              ASSIGN
                  JBoxCompanyUser.iJBoxCompanyId = iCL
                  JBoxCompanyUser.cJBoxUserId    = JBoxUser.cJBoxUserId
                  .
          END.
      END.

      /* Oppretter brukergruppe for Kasse */
      IF NOT CAN-FIND(FIRST JBoxUserGroup) THEN
      DO:
          CREATE JBoxUserGroup.
          ASSIGN
              JBoxUserGroup.iJboxUserGroupId = 1
              JBoxUserGroup.cUserGroupName   = "Kasse"
              .
      END.

/*       /* Legger alle brukere i brukergruppe "Kasse". */                                      */
/*       FOR EACH JBoxUser NO-LOCK:                                                             */
/*           IF NOT CAN-FIND(FIRST JBoxUserGroupMembers WHERE                                   */
/*                           JBoxUserGroupMembers.iJboxUserGroupId = 1 AND                      */
/*                           JBoxUserGroupMembers.cJBoxUserId      = jBoxUser.cJBoxUserId) THEN */
/*           DO:                                                                                */
/*               CREATE JBoxUserGroupMembers.                                                   */
/*               ASSIGN                                                                         */
/*                   JBoxUserGroupMembers.iJboxUserGroupId = 1                                  */
/*                   JBoxUserGroupMembers.cJBoxUserId      = jBoxUser.cJBoxUserId               */
/*                   .                                                                          */
/*           END.                                                                               */
/*       END.                                                                                   */

      /* Kobler Brukergruppen til en meny. */
      IF NOT CAN-FIND(FIRST JBoxUserMenu WHERE
                      JBoxUserMenu.iJboxUserGroupId = 1 AND
                      JBoxUserMenu.cJBoxUserId      = "")THEN
      DO:
          CREATE JBoxUserMenu.
          ASSIGN
              JBoxUserMenu.iJboxUserGroupId = 1 
              JBoxUserMenu.cJBoxUserId      = ""
              JBoxUserMenu.iJBoxMenuId      = IF lHK THEN 1 ELSE 376
              .
      END.


  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Initiering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Procedure 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
    
DEF BUFFER bIndType FOR IndType.

ASSIGN
    cTekst = "Normal,Sykkel,Våpen,Elektro"
    .
DO FOR bIndType TRANSACTION:
    DO piLoop = 0 TO 3:
        IF NOT CAN-FIND(IndType WHERE
                        IndType.IndividType = piLoop) THEN
        DO:
            CREATE bIndType.
            ASSIGN
                bIndType.individType  = piLoop
                bIndType.IndividBeskr = ENTRY(piLoop + 1,cTekst)
                bIndtype.SerieNrReg   = (IF piLoop = 0 
                                            THEN 1 /* Ingen */
                                         ELSE IF piLoop = 1
                                           THEN 3 /* Spørsmål */
                                         ELSE IF piLoop = 2 
                                           THEN 2 /* Tvang */
                                         ELSE IF piLoop = 3 
                                           THEN 3 /* Spørsmål */ 
                                         ELSE 1)
                .
            RELEASE bIndType.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initImpHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initImpHode Procedure 
PROCEDURE initImpHode :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cListe AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cEDB-System AS CHARACTER NO-UNDO.
  
  ASSIGN
    cListe = 'Axfood,Hugin,MegaDisk,Menigo,Sport,Gant Global'
    .      
  
  DO piLoop = 1 TO NUM-ENTRIES(cListe):
    cEDB-System = ENTRY(piLoop,cListe).
    IF NOT CAN-FIND(ImpHode WHERE
                    ImpHode.EDB-System = cEDB-System) THEN
    DO:
        CREATE ImpHode.
        ASSIGN
            ImpHode.EDB-System        = cEDB-System
            ImpHode.SystemBeskrivelse = 'Opprettet av Sysinit.p.'
            .
        RELEASE ImpHode.
    END.
  END.
  
  FOR EACH ImpKonv NO-LOCK
      BREAK BY ImpKonv.EDB-System:
      IF FIRST-OF(ImpKonv.EDB-System) AND TRIM(ImpKonv.EDB-System) <> '' THEN
      DO:
          IF NOT CAN-FIND(ImpHode WHERE
                          ImpHode.EDB-System = ImpKonv.EDB-System) THEN
          DO:
              CREATE ImpHode.
              ASSIGN
                  ImpHode.EDB-System        = ImpKonv.EDB-System
                  ImpHode.SystemBeskrivelse = 'Opprettet av Sysinit.p.'
                  .
              RELEASE ImpHode.
          END.
      END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initPlListeType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initPlListeType Procedure 
PROCEDURE initPlListeType :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DO TRANSACTION:
    IF NOT CAN-FIND(plListeType WHERE PlListeType.PlLType = 1) THEN 
      DO:
        CREATE plListeType.
        ASSIGN plListeType.PlLType = 1
               plListeType.plNavn = 'Plukkliste'.
      END.
    IF NOT CAN-FIND(plListeType WHERE PlListeType.PlLType = 2) THEN 
      DO:
        CREATE plListeType.
        ASSIGN plListeType.PlLType = 2
               plListeType.plNavn = 'Ordreforslag'.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initRegnskapsavdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initRegnskapsavdeling Procedure 
PROCEDURE initRegnskapsavdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH ArtBas NO-LOCK WHERE
    NOT CAN-FIND(Regnskapsavdeling OF ArtBas):
    DO TRANSACTION:
        CREATE Regnskapsavdeling.
        ASSIGN
            Regnskapsavdeling.RAvdNr = ArtBas.RAvdNr
            Regnskapsavdeling.RAvdBeskrivelse = "Automatisk opprettet (Sysinit.p)"
            .
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initReklamasjonslinjeVgLopnr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initReklamasjonslinjeVgLopnr Procedure 
PROCEDURE initReklamasjonslinjeVgLopnr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Reklamasjonslinje EXCLUSIVE-LOCK WHERE
      Reklamasjonslinje.Vg = 0 AND
      Reklamasjonslinje.LopNr = 0:
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = Reklamasjonslinje.ArtikkelNR NO-ERROR.
      IF AVAILABLE ArtBas THEN
          ASSIGN
          Reklamasjonslinje.Vg    = ArtBas.Vg
          Reklamasjonslinje.LopNr = ArtBas.LopNr
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initTommeRegistre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTommeRegistre Procedure 
PROCEDURE initTommeRegistre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(FIRST Anv-Kod) THEN
  DO TRANSACTION:
      CREATE Anv-Kod.
      RELEASE Anv-Kod.
  END.

  IF NOT CAN-FIND(FIRST Farg) THEN
  DO TRANSACTION:
    CREATE Farg.
    RELEASE Farg.
  END.

  IF NOT CAN-FIND(FIRST Material) THEN
  DO TRANSACTION:
    CREATE Material.
    RELEASE Material.
  END.

  IF NOT CAN-FIND(FIRST Klack) THEN
  DO TRANSACTION:
    CREATE Klack.
    RELEASE Klack.
  END.

  IF NOT CAN-FIND(FIRST Varemerke) THEN
  DO TRANSACTION:
    CREATE Varemerke.
    RELEASE Varemerke.
  END.

  IF NOT CAN-FIND(FIRST Produsent) THEN
  DO TRANSACTION:
    CREATE Produsent.
    RELEASE Produsent.
  END.

  IF NOT CAN-FIND(FIRST Ovandel) THEN
  DO TRANSACTION:
    CREATE Ovandel.
    RELEASE Ovandel.
  END.

  IF NOT CAN-FIND(FIRST Last-Sko) THEN
  DO TRANSACTION:
    CREATE Last-Sko.
    RELEASE Last-Sko.
  END.

  IF NOT CAN-FIND(FIRST Foder) THEN
  DO TRANSACTION:
    CREATE Foder.
    RELEASE Foder.
  END.
  
  IF NOT CAN-FIND(FIRST InnerSula) THEN
  DO TRANSACTION:
    CREATE InnerSula.
    RELEASE InnerSula.
  END.

  IF NOT CAN-FIND(FIRST SlitSula) THEN
  DO TRANSACTION:
    CREATE SlitSula.
    RELEASE SlitSula.
  END.

  IF NOT CAN-FIND(FIRST Rabatt) THEN
  DO TRANSACTION:
    CREATE Rabatt.
    RELEASE Rabatt.
  END.
  
  IF NOT CAN-FIND(FIRST Prov) THEN
  DO TRANSACTION:
    CREATE Prov.
    RELEASE Prov.
  END.
  
  IF NOT CAN-FIND(FIRST PrisGruppe) THEN
  DO TRANSACTION:
    CREATE PrisGruppe.
    RELEASE PrisGruppe.
  END.
  



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initUtvidetsok) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initUtvidetsok Procedure 
PROCEDURE initUtvidetsok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
/*IF CAN-FIND(FIRST ArtBas WHERE
            ArtBas.Utvidetsok = "") THEN*/
DO:
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE
        ArtBas.UtvidetSok = ".":
        {w_artbas.i}
    END.
END.

/*IF CAN-FIND(FIRST VareBokLinje WHERE
            VareBokLinje.Utvidetsok = "") THEN */
DO:
    FOR EACH VareBokLinje EXCLUSIVE-LOCK:
        VarebokLinje.UtvidetSok = ".".
    END.
    FOR EACH VareBehLinje EXCLUSIVE-LOCK:
        VareBehLinje.UtvidetSok = ".".
    END.
END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-initUtvidetSok2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initUtvidetSok2 Procedure 
PROCEDURE initUtvidetSok2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*
/*IF CAN-FIND(FIRST VPIArtBas WHERE
            VPIArtBas.EkstVPILevNr > 0 AND
            VPIArtBas.Utvidetsok = "") THEN */
DO:
    FOR EACH vpiArtBas EXCLUSIVE-LOCK WHERE
        vpiArtBas.EkstVPILevNr > 0 AND
        vpiArtBas.UtvidetSok = ".":
        {w_artbas.i &vpi = "vpi"}
    END.
END.
*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-NullstillVaretellingimport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NullstillVaretellingimport Procedure 
PROCEDURE NullstillVaretellingimport :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
    DO TRANSACTION:
        FIND FIRST EkstVPILev EXCLUSIVE-LOCK WHERE 
            EkstVPILev.EkstVPILEvNr = 893 NO-ERROR.
        IF AVAILABLE EkstVPILEv THEN DO:
            EkstVPILEv.AktivLev = FALSE.
            RELEASE EkstVPILEv.
        END.
        FIND FIRST EkstVPIFil WHERE
              EkstVPIFil.EkstVPILevNr = 893 AND
              EkstVPIFil.VPIFilNr = 1 AND
              EKSTVPIFil.VPIFilType = 8 AND
              EkstVPIFil.VPIFilNavn = "TELLING" AND
              EkstVPIFil.VPIFilAktiv = TRUE NO-ERROR.
        IF AVAILABLE EkstVPIFil THEN
            EkstVPIFil.VPIFilAktiv = FALSE.
        RELEASE EkstVPIFil.

        FIND FIRST EkstVPILev EXCLUSIVE-LOCK WHERE 
            EkstVPILev.EkstVPILEvNr = 894 NO-ERROR.
        IF AVAILABLE EkstVPILEv THEN DO:
            EkstVPILEv.AktivLev = FALSE.
            RELEASE EkstVPILEv.
        END.
        FIND FIRST EkstVPIFil WHERE
              EkstVPIFil.EkstVPILevNr = 894 AND
              EkstVPIFil.VPIFilNr = 1 AND
              EKSTVPIFil.VPIFilType = 8 AND
              EkstVPIFil.VPIFilNavn = "VARETRAN" AND
              EkstVPIFil.VPIFilAktiv = TRUE NO-ERROR.
        IF AVAILABLE EkstVPIFil THEN
            EkstVPIFil.VPIFilAktiv = FALSE.
        RELEASE EkstVPIFil.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-opprettMapping) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettMapping Procedure
PROCEDURE opprettMapping:
  /*------------------------------------------------------------------------------
      Purpose:                                      
      Notes: RUN opprettMapping (cEDB-System,'LevBas','999999',trim(ENTRY(6,pcLinje),'"')).                                                  
  ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cEDB-System AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cTabell     AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cInterntId  AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER cEksterntId AS CHARACTER NO-UNDO.

  IF NOT CAN-FIND(ImpKonv WHERE
                  ImpKonv.EDB-System = cEDB-System AND
                  ImpKonv.Tabell     = cTabell AND
                  ImpKonv.InterntId  = cInterntId AND
                  ImpKonv.EksterntId = cEksterntId) THEN 
  DO:
    CREATE ImpKonv.
    ASSIGN
      ImpKonv.EDB-System = cEDB-System
      ImpKonv.Tabell     = cTabell
      ImpKonv.InterntId  = cInterntId
      ImpKonv.EksterntId = cEksterntId. 
  END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-opprettStDef) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opprettStDef Procedure 
PROCEDURE opprettStDef :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER pStTypeId LIKE StType.StTypeId NO-UNDO.

  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "AAR") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "AAR"
          StDef.Beskrivelse = "År"
          .
  END.
  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "MANED") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "MANED"
          StDef.Beskrivelse = "Måned"
          .
  END.
  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "UKE") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "UKE"
          StDef.Beskrivelse = "Uke"
          .
  END.
  IF NOT CAN-FIND(StDef WHERE
                  StDef.StTypeId = StType.StTypeId AND
                  StDef.PerId    = "DAG") THEN
  DO:
      CREATE StDef.
      ASSIGN
          StDef.StTypeId    = StType.StTypeId
          StDef.PerId       = "DAG"
          StDef.Beskrivelse = "Dag"
          .
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-rensButikkKobling) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rensButikkKobling Procedure
PROCEDURE rensButikkKobling:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

FOR EACH ButikkKobling EXCLUSIVE-LOCK WHERE
  NOT CAN-FIND(Butiker WHERE Butiker.Butik = ButikkKobling.Butik):
    DELETE ButikkKobling.
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setArtkortEgenskaper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setArtkortEgenskaper Procedure 
PROCEDURE setArtkortEgenskaper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.

/* IF NOT CAN-FIND(FIRST SysPara where */
/*     SysPara.SysHId = 2 and          */
/*     SysPara.SysGr  = 5 AND          */
/*     SysPara.ParaNr = 10) THEN       */
FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 5 AND
        bSysPara.ParaNr = 10 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 5 
        bSysPara.ParaNr      = 10
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Vis materialkode"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
/*
ELSE DO TRANSACTION:
    FIND CURRENT bSysPara EXCLUSIVE-LOCK.
    ASSIGN
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Vis materialkode"
        .
    RELEASE bSysPara.
END.
*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 13 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 13
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = '1'
        bSysPara.Hjelpetekst1 = '0-Ingen, 1-Pr.str, 2-Hylle'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Sett antall etiketter"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 22 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 22
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Nordisk håndtering av EAN 20 koder"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 23 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 23
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, >0-<Prefix> (Eks. 7388 for Sverige)'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Prefiks pris i kode på tidskrifter"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 200 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 200
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Initier VPIBildekode felt"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 24 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 24
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Slå av init av bildekode"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 25 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 25
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Aktiver passord på lagertransaksjoner"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 26 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 26
        bSysPara.Parameter1  = '1'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Sperr gen. av EANkoder på kjedeleverandør"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 29 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 29
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Kopier varetekst til bongtekst og etikettekst"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 30 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 30
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Tillat endring av kjedevare"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 39 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 39
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Etikettflagg skal ikke settes"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 31 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 31
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Tillat endring av gj.fakturert"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 40 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 40
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Kopier HK priskø til alle profiler"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 41 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 41
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Tvang på etikettutskrift"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 42 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 42
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Sett etikettflagg på HK prisprofil"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 43 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 43
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = 'Det er bare artikkelinfo. endringer som undertrykkes. Ikke pris og strekkodeendringer.'
        bSysPara.Beskrivelse = "Undertrykk utlegg til kasse for artikkelendringer"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 44 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 44
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = 'Det er bare artikkelinfo. endringer som undertrykkes. Ikke pris og strekkodeendringer.'
        bSysPara.Beskrivelse = "Lokal pris skal ikke slettes selv om den er lik HK kalkyle."
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 45 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 45
        bSysPara.Beskrivelse = "PLU nummer på etikett vare."
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = 'Komma separert lsite med PLU nummer (Strekkoder).'
        bSysPara.Hjelpetekst2 = 'PLU nummer (Strekkode) på artikkel som representerer etikett.'
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 46 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 46
        bSysPara.Beskrivelse = "Passordtvang ved sanering av artikkel."
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja.'
        bSysPara.Hjelpetekst2 = ''
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

FIND FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 47 NO-LOCK NO-ERROR.
IF NOT AVAILABLE bSysPara THEN
DO TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 4 
        bSysPara.ParaNr      = 47
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        bSysPara.Beskrivelse = "Kopier HK innpris til alle lokale prisprofiler"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setArtkortfaner) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setArtkortfaner Procedure 
PROCEDURE setArtkortfaner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 2 AND
    SysPara.SysGr  = 5 AND
    SysPara.ParaNr = 104) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 5 
        bSysPara.ParaNr      = 104
        bSysPara.Beskrivelse = "Angivelse av lagerantall i foreklet artikkelkort"
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja'
        bSysPara.Hjelpetekst2 = ''
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 2 AND
    SysPara.SysGr  = 5 AND
    SysPara.ParaNr = 103) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 5 
        bSysPara.ParaNr      = 103
        bSysPara.Beskrivelse = "Bruk forenklet artikkelkort ved ny artikkel"
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Standard, 1-Servicehandel, 2-Faghandel'
        bSysPara.Hjelpetekst2 = ''
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
ELSE DO FOR bSysPara TRANSACTION:
    FIND bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 5 AND
        bSysPara.ParaNr = 103 NO-ERROR.
    IF AVAILABLE SysPara THEN 
    DO:
        bSysPara.Hjelpetekst1 =  '0-Standard, 1-Servicehandel, 2-Faghandel'.
        RELEASE bSysPara. 
    END.
END.

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 2 AND
    SysPara.SysGr  = 5 AND
    SysPara.ParaNr = 102) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 5 
        bSysPara.ParaNr      = 102
        bSysPara.Beskrivelse = "TAB ordning i Artikkelkort"
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Hjelpetekst1 = '0-Standard, 1-Servicehandel'
        bSysPara.Hjelpetekst2 = ''
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 2 AND
    SysPara.SysGr  = 5 AND
    SysPara.ParaNr = 101) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 2 
        bSysPara.SysGr       = 5 
        bSysPara.ParaNr      = 101
        bSysPara.Parameter1  = 'E,E,E,E,D,D,D,D,D,D,D,D'
        bSysPara.Parameter2  = 'E,E,E,E,D,D,D,D,D,D,D,D'
        bSysPara.Hjelpetekst1 = 'De fire första måste vara E'
        bSysPara.Hjelpetekst2 = 'E=Enabel/Vis, D=Skjul'
        bSysPara.Beskrivelse = "Faner i Artikkelkort"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF CAN-FIND(SysPara WHERE
        SysPara.SysHId = 2 AND
        SysPara.SysGr  = 5 AND
        SysPara.ParaNr = 100 AND NUM-ENTRIES(SysPara.Parameter1) = 11) THEN DO TRANSACTION:

    FIND SysPara WHERE
         SysPara.SysHId = 2 AND
         SysPara.SysGr  = 5 AND
         SysPara.ParaNr = 100.
    ASSIGN ENTRY(2,SysPara.Parameter1) = "Tillegg vare," + ENTRY(2,SysPara.Parameter1).
    FIND SysPara WHERE
        SysPara.SysHId      = 2 AND
        SysPara.SysGr       = 5 AND
        SysPara.ParaNr      = 101.
    ASSIGN SysPara.Parameter1   = "E," + SysPara.Parameter1
           SysPara.Parameter2   = "E," + SysPara.Parameter1
           SysPara.Hjelpetekst1 = 'De fyra första måste vara E'.
    RELEASE SysPara.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBestillingStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBestillingStatus Procedure 
PROCEDURE setBestillingStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pcTekst AS CHAR NO-UNDO.        
    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcFarge AS CHAR NO-UNDO.

    DEF BUFFER bSysPara   FOR SysPara.
    DEF BUFFER bSysGruppe FOR SysGruppe.

    ASSIGN
        pcTekst = "BESTILLINGSFORSLAG,BESTILLING,PÅ ORDRE,ORDRE SENDT,DEL-LEVEVERT,FULL-LEVERT,NEG-INNLEV,,MAKULERT"
        pcFarge = "255,255,255|0,128,255|255,255,0|255,128,64|255,128,192|128,128,0|255,0,0|255,0,0|255,0,0"
        .

    DO FOR bSysPara TRANSACTION:
        LOOP:
        DO piLoop = 1 TO 9:
            IF NOT CAN-FIND(FIRST bSysPara WHERE
                bSysPara.SysHId = 5 AND
                bSysPara.SysGr  = 2 AND
                bSysPara.ParaNr = piLoop) THEN
            DO:
                CREATE bSysPara.
                ASSIGN  
                    bSysPara.SysHId      = 5 
                    bSysPara.SysGr       = 2 
                    bSysPara.ParaNr      = piLoop
                    bSysPara.Parameter1  = ENTRY(piLoop,pcTekst)
                    bSysPara.Parameter2  = ENTRY(piLoop,pcFarge,"|")
                    bSysPara.Beskrivelse = ENTRY(piLoop,pcTekst)
                    .
                RELEASE bSysPara.
            END.
        END. /* LOOP */

        FIND bSysPara WHERE
          bSysPara.SysHId = 5 AND
          bSysPara.SysGr  = 2 AND
          bSysPara.ParaNr = 99 EXCLUSIVE-LOCK NO-ERROR.
        IF NOT AVAILABLE bSysPara THEN
            CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 99
            bSysPara.Parameter1  = pcTekst
            bSysPara.Beskrivelse = "Ordrestatus"
            .
        RELEASE bSysPara.

    END. /* bSysPara TRANSACTION*/


/*     IF NOT CAN-FIND(SysGruppe where       */
/*         SysGruppe.SysHId = 2 and          */
/*         SysGruppe.SysGr  = 7) THEN        */
/*     DO FOR bSysGruppe TRANSACTION:        */
/*         CREATE bSysGruppe.                */
/*         ASSIGN                            */
/*             bSysGruppe.SysHId = 2         */
/*             bSysGruppe.SysGr  = 7         */
/*             Beskrivelse      = "Vareslag" */
/*             .                             */
/*         RELEASE bSysGruppe.               */
/*     END. /* bSysGruppe TRANSACTION */     */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBetalingsbetingelser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBetalingsbetingelser Procedure 
PROCEDURE setBetalingsbetingelser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.
            
  DO TRANSACTION:
      DO piLoop = 0 TO 8:
          IF CAN-FIND(FIRST Betalingsbetingelser WHERE 
                      Betalingsbetingelser.BetBet = piLoop) THEN
              NEXT.
          
          CREATE Betalingsbetingelser.
          ASSIGN
              Betalingsbetingelser.BetBet   = piLoop
              .
          CASE piLoop:
              WHEN 0 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = ""
                      Betalingsbetingelser.AntKredittDager = 0
                      Betalingsbetingelser.FriLevMnd       = 0
                      .
              WHEN 1 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Netto 10 dager"
                      Betalingsbetingelser.AntKredittDager = 10
                      Betalingsbetingelser.FriLevMnd       = 0
                      .
              WHEN 2 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Netto 15 dager"
                      Betalingsbetingelser.AntKredittDager = 15
                      Betalingsbetingelser.FriLevMnd       = 0
                      .
              WHEN 3 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Netto 20 dager"
                      Betalingsbetingelser.AntKredittDager = 20
                      Betalingsbetingelser.FriLevMnd       = 0
                      .
              WHEN 4 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Netto 30 dager"
                      Betalingsbetingelser.AntKredittDager = 30
                      Betalingsbetingelser.FriLevMnd       = 0
                      .
              WHEN 5 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Fri mnd,netto 10 dager"
                      Betalingsbetingelser.AntKredittDager = 10
                      Betalingsbetingelser.FriLevMnd       = 1
                      .
              WHEN 6 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Fri mnd,netto 15 dager"
                      Betalingsbetingelser.AntKredittDager = 15
                      Betalingsbetingelser.FriLevMnd       = 1
                      .
              WHEN 7 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Fri mnd,netto 20 dager"
                      Betalingsbetingelser.AntKredittDager = 20
                      Betalingsbetingelser.FriLevMnd       = 1
                      .
              WHEN 8 THEN
                  ASSIGN
                      Betalingsbetingelser.BetTekst        = "Fri mnd,netto 30 dager"
                      Betalingsbetingelser.AntKredittDager = 30
                      Betalingsbetingelser.FriLevMnd       = 1
                      .
          END CASE.


      END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBilagsart) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBilagsart Procedure 
PROCEDURE setBilagsart :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cListe AS CHAR NO-UNDO.
DEF VAR cNavn  AS CHAR NO-UNDO.

ASSIGN
    cListe = "1,2"
    cNavn  = "Varesalg,Betaling"
    .

DO piLoop = 1 TO NUM-ENTRIES(cListe):
    IF NOT CAN-FIND(Bilagsart WHERE
                    Bilagsart.BArtNr = int(ENTRY(piLoop,cListe))) THEN
    DO TRANSACTION:
        CREATE Bilagsart.
        ASSIGN
            Bilagsart.BArtNr    = int(ENTRY(piLoop,cListe))
            Bilagsart.BArtTekst = ENTRY(piLoop,cNavn)
            .
        RELEASE Bilagsart.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setBilagstype) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBilagstype Procedure 
PROCEDURE setBilagstype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cListe AS CHAR NO-UNDO.
DEF VAR cNavn  AS CHAR NO-UNDO.
DEF VAR cBruk  AS CHAR NO-UNDO.

ASSIGN
    cListe = "1,2,3,4,5,6,10,11"
    cNavn  = "Faktura,Kreditnota,Innbetaling,aKonto,Utbetaling,Debet korr.postering,Purring,Purregebyr"
    cBruk  = "3,3,2,2,2,2,1,2"
    .

DO piLoop = 1 TO NUM-ENTRIES(cListe):
    IF NOT CAN-FIND(Bilagstype WHERE
                    Bilagstype.Bilagstype = int(ENTRY(piLoop,cListe))) THEN
    DO TRANSACTION:
        CREATE Bilagstype.
        ASSIGN
            Bilagstype.Bilagstype = int(ENTRY(piLoop,cListe))
            Bilagstype.BTTekst    = ENTRY(piLoop,cNavn)
            Bilagstype.Bruk       = int(ENTRY(piLoop,cBruk))
            .

        RELEASE Bilagstype.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setBilderegister) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBilderegister Procedure
PROCEDURE setBilderegister:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/


IF NOT CAN-FIND(FIRST Bilderegister WHERE 
                      Bilderegister.BildNr = 0) THEN 
DO TRANSACTION:
  CREATE Bilderegister.
  ASSIGN
  BildeRegister.BildNr = 0.
  RELEASE Bilderegister.
END.

IF NOT CAN-FIND(SysHode WHERE
    SysHode.SysHId = 400) THEN
DO TRANSACTION:
    CREATE bSysHode.
    ASSIGN
        bSysHode.SysHId  = 400
        Beskrivelse      = "Bildeoverføringslogg"
        .
    RELEASE bSysHode.
END. /* bSysGruppe TRANSACTION */


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setBokforingsbilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBokforingsbilag Procedure 
PROCEDURE setBokforingsbilag :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

DEFINE VARIABLE piLoop   AS INT NO-UNDO.
DEFINE VARIABLE pcStreng AS CHARACTER NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 20 AND
    SysGruppe.SysGr  = 5) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 20
        bSysGruppe.SysGr  = 5
        Beskrivelse      = "Diverse"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 3:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 20 AND
            bSysPara.SysGr  = 5 AND
            bSysPara.ParaNr = piLoop) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 20 
                bSysPara.SysGr       = 5 
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = "13"
                bSysPara.Beskrivelse = "Tjenester avd. 13 pr. varegruppe"
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

/* Tekster bokføringsbilag. */
pcStreng = "Tekst"                 + CHR(1) + 
           "Konto"                 + CHR(1) + 
           "Beløp"                 + CHR(1) +
           "Mva"                   + CHR(1) +
           "Beløp u/mva"           + CHR(1) + 
           "Varesalg"              + CHR(1) + 
           "Betalt med:"           + CHR(1) + 
           "Bankkort"              + CHR(1) + 
           "Reserveløsning"        + CHR(1) +
           "D 2380"                + CHR(1) +
           "Kontant"               + CHR(1) +
           "Kassedifferens"        + CHR(1) +
           "Bankkort"              + CHR(1) +
           "Reserveløsning"        + CHR(1) +
           "Tilgodeseddler egne"   + CHR(1) +
           "Tilgodeseddler andres" + CHR(1) +
           "Gavekort Senter"       + CHR(1) +
           "Gavekort egna"         + CHR(1) +
           "Gavekort andre"        + CHR(1) +
           "Deponering"            + CHR(1) +
           "Kupong 1"              + CHR(1) +
           "Kupong 2"              + CHR(1) +
           "Deponering ut"         + CHR(1) +
           "NonSale pos"           + CHR(1) +
           "NonSale neg"           + CHR(1) +
           "Kasse endring"         + CHR(1) +
           "Kasse diff".

DO piLoop = 1 TO NUM-ENTRIES(pcStreng,CHR(1)) TRANSACTION:
   IF NOT CAN-FIND(TransBeskr WHERE  
                   TransBeskr.TTId = 900 AND 
                   TransBeskr.TBId = piLoop) THEN 
   DO:
     CREATE TransBeskr.
     ASSIGN 
       TransBeskr.TTId        = 900
       TransBeskr.TBId        = piLoop
       TransBeskr.Beskrivelse = ENTRY(piLoop,pcStreng,CHR(1)). 
   END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setButikkGLNNr) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setButikkGLNNr Procedure
PROCEDURE setButikkGLNNr:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 60) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 50
        bSysGruppe.SysGr  = 60
        Beskrivelse      = "Apoteksfiler"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setEDBDataType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEDBDataType Procedure 
PROCEDURE setEDBDataType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst       AS CHAR NO-UNDO.
  DEF VAR pcBeskrivelse AS CHAR NO-UNDO.
  DEF VAR piLoop        AS INT  NO-UNDO.

  DEF BUFFER bEDBDataType FOR EDBDataType.
  ASSIGN
      pcTekst       = "VPI,FIN,ORD,MEDW,VPIV"
      pcBeskrivelse = "Vare og prisinfo," +
                      "Finans," +
                      "Ordre," +
                      "Medlemsinitiering web," +
                      "VPI fra varebok"
      .

  IF NUM-ENTRIES(pcTekst) <> NUM-ENTRIES(pcBeskrivelse) THEN
  DO:
      MESSAGE
      PROGRAM-NAME(1)
      SKIP "Antall datatyper stemmer ikke med antall beskrivelser. " +
           "Feil i oppsett av datatyper for eksterne EDB ystemer."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  SET_DATATYPER:
  DO FOR BEDBDataType piLoop = 1 TO NUM-ENTRIES(pcTekst):
      IF NOT CAN-FIND(bEDBDataType WHERE
                      bEDBDataType.DataType = ENTRY(piLoop,pcTekst)) THEN
      DO:
          CREATE bEDBDataType.
          ASSIGN
              bEDBDataType.DataType        = ENTRY(piLoop,pcTekst)
              bEDBDataType.TypeBeskrivelse = ENTRY(piLoop,pcBeskrivelse)
              .
          RELEASE bEDBDataType.
      END.
  END. /* SET_DATATYPER */

       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEkstEDBSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEkstEDBSystem Procedure 
PROCEDURE setEkstEDBSystem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

           
IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "VisGORD") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "VisGORD"
    ekstEDBSystem.SysBeskrivelse = "Ordre til Visma Global"
    ekstEDBSystem.DataType       = "ORD"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "ORD"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "erpVismaOrdre.r"
    .
  RELEASE ekstEDBSystem.
END.
IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "VisGVPI") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "VisGVPI"
    ekstEDBSystem.SysBeskrivelse = "VPI til Visma Globa"
    ekstEDBSystem.DataType       = "VPI"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "VPI"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "erpVismaVpi.r"
    .
  RELEASE ekstEDBSystem.
END.
IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "VisVAREB") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "VisVAREB"
    ekstEDBSystem.SysBeskrivelse = "VPI fra varebok til Visma Globa"
    ekstEDBSystem.DataType       = "VPIV"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "VBVPI"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "erpVismaVpiVarebok.r"
    .
  RELEASE ekstEDBSystem.
END.
/* Retter filprefix på EDB systemoppsettet. */
ELSE DO TRANSACTION:
    FIND ekstEDBSystem WHERE
        ekstEDBSystem.EDBSystem = "VisVAREB" EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE ekstEDBSystem THEN DO:
        ASSIGN
          ekstEDBSystem.FilPrefix = "VBVPI".
        RELEASE ekstEDBSystem.
    END.
END.
IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "MedWEB") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "MedWEB"
    ekstEDBSystem.SysBeskrivelse = "Web for initiering av medlemmer"
    ekstEDBSystem.DataType       = "MEDW"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "MEDW"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "webWebMaker.r"
    .
  RELEASE ekstEDBSystem.
END.
/* Eksport laget for Butikkbageriet. Eksport av fakturahode. */
IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "EkspFAK") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "EkspFAK"
    ekstEDBSystem.SysBeskrivelse = "Eksport av fakturadata Visma Business"
    ekstEDBSystem.DataType       = "FAKTURA"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "FAK"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "eksporterfaktura_visma_business.r"
    .
  RELEASE ekstEDBSystem.
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "FAKTURA") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "FAKTURA"
          EDBDataType.TypeBEskrivelse = "Fakturaeksport"
          .
      RELEASE EDBDataType.
  END.
END.
ELSE DO TRANSACTION:
    FIND ekstEDBSystem EXCLUSIVE-LOCK WHERE
                ekstEDBSystem.EDBSystem = "EkspFAK" NO-ERROR.
    IF AVAILABLE ekstEDBSystem THEN DO:
        ekstEDBSystem.eksportRutine = "eksporterfaktura_visma_business.r".
        RELEASE ekstEDBSystem.
    END.   
END.
IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "EkspFAU") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "EkspFAU"
    ekstEDBSystem.SysBeskrivelse = "Eksport av fakturadata Uni Micro"
    ekstEDBSystem.DataType       = "FAKTURA"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "FAK"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "eksporterfaktura_unimicro.p"
    .
  RELEASE ekstEDBSystem.
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "FAKTURA") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "FAKTURA"
          EDBDataType.TypeBEskrivelse = "Fakturaeksport"
          .
      RELEASE EDBDataType.
  END.
END.

IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "EkspFAAUTO") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "EkspFAAUTO"
    ekstEDBSystem.SysBeskrivelse = "Automatisk eksport av faktura til Visma Business"
    ekstEDBSystem.DataType       = "FAKTAUTO"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "FAA"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "eksporterfaktura_holin_visma_business.p"
    .
  RELEASE ekstEDBSystem.
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "FAKTAUTO") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "FAKTAUTO"
          EDBDataType.TypeBEskrivelse = "Automatisk fakturaeksport"
          .
      RELEASE EDBDataType.
  END.
END.

IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "EkspKOAUTO") THEN
DO TRANSACTION:
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "EkspKOAUTO"
    ekstEDBSystem.SysBeskrivelse = "Automatisk eksport av kontantsalg til Visma Business"
    ekstEDBSystem.DataType       = "KONTAUTO"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "KOA"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "eksporterkontant_holin_visma_business.p"
    .
  RELEASE ekstEDBSystem.
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "KONTAUTO") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "KONTAUTO"
          EDBDataType.TypeBEskrivelse = "Automatisk kontantsalgeksport"
          .
      RELEASE EDBDataType.
  END.
END.

IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "WinEDI") THEN
DO TRANSACTION:
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "POST") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "POST"
          EDBDataType.TypeBeskrivelse = "Postetiketter"
          .
      RELEASE EDBDataType.
  END.
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "WinEDI"
    ekstEDBSystem.SysBeskrivelse = "WinEDI postetiketter"
    ekstEDBSystem.DataType       = "POST"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "P"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 9999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "ekspWinEDI.r"
    .
  RELEASE ekstEDBSystem.
END.

IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "UniFaun") THEN
DO TRANSACTION:
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "POST") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "POST"
          EDBDataType.TypeBeskrivelse = "Postetiketter"
          .
      RELEASE EDBDataType.
  END.
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "UniFaun"
    ekstEDBSystem.SysBeskrivelse = "UniFaun postetiketter"
    ekstEDBSystem.DataType       = "POST"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "POST"
    ekstEDBSystem.FilEkstent     = "xml"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 9999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "ekspUniFaun.r"
    .
  RELEASE ekstEDBSystem.
END.

IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "WebBut") THEN
DO TRANSACTION:
  IF NOT CAN-FIND(EDBDataType WHERE
                  EDBDataType.DataType = "WebBut") THEN
  DO:
      CREATE EDBDataType.
      ASSIGN
          EDBDataType.DataType        = "WebBut"
          EDBDataType.TypeBeskrivelse = "WebButikk"
          .
      RELEASE EDBDataType.
  END.
  
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "WebBut"
    ekstEDBSystem.SysBeskrivelse = "Data til Web butikk"
    ekstEDBSystem.DataType       = "WebBut"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "WB"
    ekstEDBSystem.FilEkstent     = "csv"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "webButikk.r"
    .
  RELEASE ekstEDBSystem.
END.

IF NOT CAN-FIND(ekstEDBSystem WHERE
                ekstEDBSystem.EDBSystem = "MAGENTO") THEN
DO TRANSACTION: 
  CREATE ekstEDBSystem.
  ASSIGN
    ekstEDBSystem.EDBSystem      = "MAGENTO"
    ekstEDBSystem.SysBeskrivelse = "Data til MAGENTO Nettbutikk"
    ekstEDBSystem.DataType       = "WebBut"
    ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
    ekstEDBSystem.FilPrefix      = "MA"
    ekstEDBSystem.FilEkstent     = "xml"
    ekstEDBSystem.SeqNr          = 0
    ekstEDBSystem.MaksSeq        = 99999999
    ekstEDBSystem.EkspFrekvens   = 0
    ekstEDBSystem.StartTid       = 0
    ekstEDBSystem.StoppTid       = 86340
    ekstEDBSystem.TidsIntervall  = 1
    ekstEDBSystem.Aktiv          = NO
    ekstEDBSystem.SeqvAktiv      = YES
    ekstEDBSystem.eksportRutine  = "webButMagento.r"
    .
  RELEASE ekstEDBSystem.
END.

    IF NOT CAN-FIND(ekstEDBSystem WHERE
        ekstEDBSystem.EDBSystem = "PHØNIX") THEN
    DO TRANSACTION: 
        CREATE ekstEDBSystem.
        ASSIGN
            ekstEDBSystem.EDBSystem      = "PHØNIX"
            ekstEDBSystem.SysBeskrivelse = "Data til PHØNIX Nettbutikk"
            ekstEDBSystem.DataType       = "WebBut"
            ekstEDBSystem.FilKatalog     = "c:\home\lindbak\sendes"
            ekstEDBSystem.FilPrefix      = "JSon"
            ekstEDBSystem.FilEkstent     = "JSonP"
            ekstEDBSystem.SeqNr          = 0
            ekstEDBSystem.MaksSeq        = 99999999
            ekstEDBSystem.EkspFrekvens   = 0
            ekstEDBSystem.StartTid       = 0
            ekstEDBSystem.StoppTid       = 86340
            ekstEDBSystem.TidsIntervall  = 1
            ekstEDBSystem.Aktiv          = NO
            ekstEDBSystem.SeqvAktiv      = YES
            ekstEDBSystem.eksportRutine  = "webButPhonix.r"
            .
        RELEASE ekstEDBSystem.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEkstVPITabellSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEkstVPITabellSystem Procedure 
PROCEDURE setEkstVPITabellSystem :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DO TRANSACTION:  
    IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'Butiker') THEN 
    DO:
      CREATE EkstVPITabell.
      ASSIGN
        EkstVPITabell.EkstVPILevNr = 1
        EkstVPITabell.TabellNavn   = 'Butiker'
        EkstVPITabell.TabellNr     = 0
        .
    END.
    IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'Pant') THEN 
    DO:
      CREATE EkstVPITabell.
      ASSIGN
        EkstVPITabell.EkstVPILevNr = 1
        EkstVPITabell.TabellNavn   = 'Pant'
        EkstVPITabell.TabellNr     = 1520
        .
    END.
    IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'Strekkode') THEN 
    DO:
      CREATE EkstVPITabell.
      ASSIGN
        EkstVPITabell.EkstVPILevNr = 1
        EkstVPITabell.TabellNavn   = 'Strekkode'
        EkstVPITabell.TabellNr     = 1530
        .
    END.
    IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'PLU') THEN 
    DO:
      CREATE EkstVPITabell.
      ASSIGN
        EkstVPITabell.EkstVPILevNr = 1
        EkstVPITabell.TabellNavn   = 'PLU'
        EkstVPITabell.TabellNr     = 1540
        .
    END.
    IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'Jamforenhet') THEN 
    DO:
      CREATE EkstVPITabell.
      ASSIGN
        EkstVPITabell.EkstVPILevNr = 1
        EkstVPITabell.TabellNavn   = 'Jamforenhet'
        EkstVPITabell.TabellNr     = 1550
        .
    END.
      IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'Def.Rab%') THEN 
      DO:
          CREATE EkstVPITabell.
          ASSIGN
              EkstVPITabell.EkstVPILevNr = 1
              EkstVPITabell.TabellNavn   = 'Def.Rab%'
              EkstVPITabell.TabellNr     = 1560
              .
      END.
    IF NOT CAN-FIND(FIRST EkstVPITabell WHERE EkstVPITabell.EkstVPILevNr = 1 AND EkstVPITabell.TabellNavn   = 'Returkoder') THEN 
    DO:
      CREATE EkstVPITabell.
      ASSIGN
        EkstVPITabell.EkstVPILevNr = 1
        EkstVPITabell.TabellNavn   = 'Returkoder'
        EkstVPITabell.TabellNr     = 1570
        .
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEtikettBatchFraKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEtikettBatchFraKasse Procedure 
PROCEDURE setEtikettBatchFraKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      FIND TransType NO-LOCK WHERE
          Transtype.TTId = 25 NO-ERROR.
      IF NOT AVAILABLE TransType THEN
      DO:
          CREATE Transtype.
          ASSIGN
              TransType.TTID        = 25
              TransType.BEskrivelse = "EtikettBatch"
              .
          IF NOT CAN-FIND(FIRST TransBeskr WHERE
                          TransBeskr.TTId = TransType.TTID) THEN 
          DO:
            CREATE TransBeskr.
            ASSIGN
              TransBeskr.TTID        = TransType.TTID
              TransBeskr.TBId        = 1
              TransBeskr.Beskrivelse = TransType.Beskrivelse
              .
            RELEASE TransBeskr.
          END.
          RELEASE TransType.
      END.
      FIND TransType NO-LOCK WHERE
          Transtype.TTId = 26 NO-ERROR.
      IF NOT AVAILABLE TransType THEN
      DO:
          CREATE Transtype.
          ASSIGN
              TransType.TTID        = 26
              TransType.BEskrivelse = "Varemottak pakkseddel"
              .
          IF NOT CAN-FIND(FIRST TransBeskr WHERE
                          TransBeskr.TTId = TransType.TTID) THEN 
          DO:
            CREATE TransBeskr.
            ASSIGN
              TransBeskr.TTID        = TransType.TTID
              TransBeskr.TBId        = 1
              TransBeskr.Beskrivelse = TransType.Beskrivelse
              .
            RELEASE TransBeskr.
          END.
          RELEASE TransType.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setEtikettFraKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setEtikettFraKasse Procedure 
PROCEDURE setEtikettFraKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      FIND TransType NO-LOCK WHERE
          Transtype.TTId = 24 NO-ERROR.
      IF NOT AVAILABLE TransType THEN
      DO:
          CREATE Transtype.
          ASSIGN
              TransType.TTID        = 24
              TransType.BEskrivelse = "Etikett"
              .
          IF NOT CAN-FIND(FIRST TransBeskr WHERE
                          TransBeskr.TTId = TransType.TTID) THEN 
          DO:
            CREATE TransBeskr.
            ASSIGN
              TransBeskr.TTID        = TransType.TTID
              TransBeskr.TBId        = 1
              TransBeskr.Beskrivelse = TransType.Beskrivelse
              .
            RELEASE TransBeskr.
          END.
          RELEASE TransType.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFakturaEksport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFakturaEksport Procedure 
PROCEDURE setFakturaEksport :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF VAR piLoop AS INT NO-UNDO.

    DEF BUFFER bSysPara   FOR SysPara.
    DEF BUFFER bSysGruppe FOR SysGruppe.

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 19 AND
        SysGruppe.SysGr  = 12) THEN
    DO FOR bSysGruppe TRANSACTION:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 19
            bSysGruppe.SysGr  = 12
            Beskrivelse       = "Fakturalayout"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    DO FOR bSysPara TRANSACTION:
        LOOP:
        DO piLoop = 1 TO 3:
            IF NOT CAN-FIND(FIRST bSysPara WHERE
                bSysPara.SysHId = 19 AND
                bSysPara.SysGr  = 12 AND
                bSysPara.ParaNr = piLoop) THEN
            DO:
                CREATE bSysPara.
                ASSIGN  
                    bSysPara.SysHId      = 19 
                    bSysPara.SysGr       = 12 
                    bSysPara.ParaNr      = piLoop
                    bSysPara.Parameter1  = ""
                    bSysPara.Beskrivelse = (IF piLoop = 1   THEN "Internasjonal fakturaLager"
                                        ELSE IF piLoop = 2 THEN "Faktura m/bankgiro"
                                        ELSE IF piLoop = 3 THEN "Svensk faktura"
                                        ELSE bSysPara.Beskrivels)
                    .
                RELEASE bSysPara.
            END.
        END. /* LOOP */

        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 12 AND
            bSysPara.ParaNr = 50) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 19 
                bSysPara.SysGr        = 12 
                bSysPara.ParaNr       = 50
                bSysPara.Parameter1   = "1"
                bSysPara.Beskrivelse  = 'Detaljeringsnivå av kvitto/ordre på faktura'
                bSysPara.Hjelpetekst1 = "1-Full, 2-Kvitto/ordre, 3-Total" 
                .
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 12 AND
            bSysPara.ParaNr = 52) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 19 
                bSysPara.SysGr        = 12 
                bSysPara.ParaNr       = 52
                bSysPara.Parameter1   = "1"
                bSysPara.Beskrivelse  = 'Plassering av fakturaadresse'
                bSysPara.Hjelpetekst1 = "1-Venstre, 2-Høyre" 
                .
            RELEASE bSysPara.
        END.
    
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 12 AND
            bSysPara.ParaNr = 53) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 19 
                bSysPara.SysGr        = 12 
                bSysPara.ParaNr       = 53
                bSysPara.Parameter1   = "Faktura/Proformafaktura"
                bSysPara.Beskrivelse  = 'Fakturatekst'
                bSysPara.Hjelpetekst1 = "Originaltekst: Faktura/Proformafaktura" 
                .
            RELEASE bSysPara.
        END.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFakturalayout) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFakturalayout Procedure 
PROCEDURE setFakturalayout :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 19 AND
    SysGruppe.SysGr  = 12) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 19
        bSysGruppe.SysGr  = 12
        Beskrivelse      = "Fakturalayout"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 3:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 12 AND
            bSysPara.ParaNr = piLoop) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 19 
                bSysPara.SysGr       = 12 
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = ""
                bSysPara.Beskrivelse = (IF piLoop = 1   THEN "Internasjonal fakturaLager"
                                        ELSE IF piLoop = 2 THEN "Faktura m/bankgiro"
                                        ELSE IF piLoop = 3 THEN "Svensk faktura"
                                        ELSE bSysPara.Beskrivels)
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */

    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 12 AND
        bSysPara.ParaNr = 50) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 19 
            bSysPara.SysGr        = 12 
            bSysPara.ParaNr       = 50
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = 'Detaljeringsnivå av kvitto/ordre på faktura'
            bSysPara.Hjelpetekst1 = "1-Full, 2-Kvitto/ordre, 3-Total" 
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 12 AND
        bSysPara.ParaNr = 52) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 19 
            bSysPara.SysGr        = 12 
            bSysPara.ParaNr       = 52
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = 'Plassering av fakturaadresse'
            bSysPara.Hjelpetekst1 = "1-Venstre, 2-Høyre" 
            .
        RELEASE bSysPara.
    END.
    
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 12 AND
        bSysPara.ParaNr = 53) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 19 
            bSysPara.SysGr        = 12 
            bSysPara.ParaNr       = 53
            bSysPara.Parameter1   = "Faktura/Proformafaktura"
            bSysPara.Beskrivelse  = 'Fakturatekst'
            bSysPara.Hjelpetekst1 = "Originaltekst: Faktura/Proformafaktura" 
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

LOOP2:
DO piLoop = 50 TO 51:
    IF NOT CAN-FIND(FIRST SysPara WHERE
        SysPara.SysHId = 19 AND
        SysPara.SysGr  = 12 AND
        SysPara.ParaNr = piLoop) THEN
    DO TRANSACTION:
        CREATE SysPara.
        ASSIGN  
            SysPara.SysHId       = 19
            SysPara.SysGr        = 12
            SysPara.ParaNr       = piLoop
            SysPara.Parameter1   = (IF piLoop = 50 THEN "1"
                                    ELSE "Varor på följesedel enlig bilaga.")
            SysPara.Beskrivelse  = (IF piLoop = 50   THEN "Detaljnivå av kvitto/order på faktura"
                                    ELSE "Text för 50-3")
            SysPara.Hjelpetekst1 = (IF piLoop = 50 THEN "1-Full,2-Kvitto/Order,3-Total"
                                     ELSE "").
                               
            
        RELEASE SysPara.
    END. /* TRANSACTION */
END. /* LOOP2 */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFakturaparametre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFakturaparametre Procedure 
PROCEDURE setFakturaparametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 19 AND
    SysGruppe.SysGr  = 101) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 19
        bSysGruppe.SysGr  = 101
        Beskrivelse      = "FakturaEksportParametre"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 19 AND
    bSysPara.SysGr  = 101 AND
    bSysPara.ParaNr = 1) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 19 
        bSysPara.SysGr        = 101 
        bSysPara.ParaNr       = 1
        bSysPara.Parameter1   = "1"
        bSysPara.Beskrivelse  = "Bare eksportere nye faktura"
        bSysPara.Hjelpetekst1 = "0-Alle, 1-Nye (Ikke tidligere eksportert)"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 19 AND
    bSysPara.SysGr  = 100 AND
    bSysPara.ParaNr = 3) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 19 
        bSysPara.SysGr        = 100 
        bSysPara.ParaNr       = 3
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "Bruk innkjøpspris på internoverføring"
        bSysPara.Hjelpetekst1 = "0-VVarekost, 1-Innkjøpspris"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 19 AND
    bSysPara.SysGr  = 100 AND
    bSysPara.ParaNr = 4) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 19 
        bSysPara.SysGr        = 100 
        bSysPara.ParaNr       = 4
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "Set inn farge som varespesifikasjon"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 19 AND
    bSysPara.SysGr  = 100 AND
    bSysPara.ParaNr = 5) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 19 
        bSysPara.SysGr        = 100 
        bSysPara.ParaNr       = 5
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "KIDnr. på fakturautskrift"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setFakturatekst) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFakturatekst Procedure 
PROCEDURE setFakturatekst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bFakturatekst FOR Fakturatekst.
DEF BUFFER bButiker      FOR Butiker.

IF NOT CAN-FIND(FIRST Fakturatekst) THEN
DO FOR bFakturatekst, bButiker TRANSACTION:
    CREATE bFakturatekst.
    ASSIGN
        bFakturatekst.FaktTekstNr = 1
        .
    RELEASE bFakturatekst.

    FOR EACH bButiker:
        IF bButiker.FaktTekstNr = 0 THEN
            bButiker.FaktTekstNr = 1.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-setFerskvarevekt) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFerskvarevekt Procedure
PROCEDURE setFerskvarevekt:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEFINE BUFFER bSysHode   FOR SysHode.
DEFINE BUFFER bSysGruppe FOR SysGruppe.
DEFINE BUFFER bSysPara   FOR SysPara.

IF NOT CAN-FIND(SysHode WHERE
                SysHode.SysHId = 23) THEN
DO TRANSACTION:
    CREATE bSysHode.
    ASSIGN
        bSysHode.SysHId      = 23
        bSysHode.Beskrivelse = "Ferskvarevekt"
        .
    RELEASE bSysHode.
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 23 AND
    SysGruppe.SysGr  = 1) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 23
        bSysGruppe.SysGr  = 1
        Beskrivelse      = "Ferskvarevektliste"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 23 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 1) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 23 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 1
        bSysPara.Parameter1   = '0'
        bSysPara.Parameter2   = '0'
        bSysPara.Beskrivelse  = "Ferskvarevekt"
        bSysPara.Hjelpetekst1 = "Parameter 1: 0=Ikke aktiv, 1-N=Aktiv"
        bSysPara.Hjelpetekst2 = "Parameter 2: Sekvensnr på filnavn. Oppdateres automatisk av eksportrutinen."
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setFilKontrollParametre) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFilKontrollParametre Procedure
PROCEDURE setFilKontrollParametre:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE BUFFER bSysHode   FOR SysHode.
DEFINE BUFFER bSysGruppe FOR SysGruppe.
DEFINE BUFFER bSysPara   FOR SysPara.

IF NOT CAN-FIND(SysHode WHERE
                SysHode.SysHId = 102) THEN
DO TRANSACTION:
    CREATE bSysHode.
    ASSIGN
        bSysHode.SysHId      = 102
        bSysHode.Beskrivelse = "Loggfil og filkontroll"
        .
    RELEASE bSysHode.
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 102 AND
    SysGruppe.SysGr  = 1) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 102
        bSysGruppe.SysGr  = 1
        Beskrivelse      = "VPI Loggfil kontroll"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 102 AND
        bSysPara.SysGr  = 1 AND
        bSysPara.ParaNr = 1) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 102 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "VPILog_"
            bSysPara.Parameter2   = STRING(TODAY) + '|' + STRING(TIME)
            bSysPara.Beskrivelse  = "VPI import loggfilkontroll"
            bSysPara.Hjelpetekst1 = "Prefiks settes normalt til 'VPILog_'."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 102 AND
        bSysPara.SysGr  = 1 AND
        bSysPara.ParaNr = 100) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 102 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 100
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "EMail mottagerliste"
            bSysPara.Hjelpetekst1 = "Semikolon separert liste over emailadresser som skal ta imot VPILoggen."
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setGarantiklasser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setGarantiklasser Procedure 
PROCEDURE setGarantiklasser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop  AS INT NO-UNDO.
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR pcMnd   AS CHAR NO-UNDO.

  ASSIGN
      pcTekst = "Ingen garanti,3 mnd garanti,6 mnd garanti, 12 mnd garanti,3 års garanti"
      pcMnd   = "0,3,6,12,36"
      .

  DO piLoop = 1 TO 5 TRANSACTION ON ERROR UNDO, RETRY:
    IF NOT CAN-FIND(Garanti WHERE 
                    Garanti.GarantiKl = piLoop - 1) THEN
    DO:
        CREATE Garanti.
        ASSIGN
            Garanti.GarantiKl    = piLoop - 1
            Garanti.GarantiTekst = ENTRY(piLoop,pcTekst)
            Garanti.MndAnt       = INT(ENTRY(piLoop,pcMnd))
            .
        IF AVAILABLE Garanti THEN
        DO:
            RELEASE Garanti.
        END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setGavekorttype) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setGavekorttype Procedure 
PROCEDURE setGavekorttype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DO piLoop = 0 TO 4:
    IF NOT CAN-FIND(GaveKType WHERE
                    GaveKType.IdentType = piLoop) THEN
    DO TRANSACTION:
        CREATE GaveKType.
        ASSIGN
            GaveKType.IdentType = piLoop
            GaveKType.GKTBeskrivelse = IF GaveKType.IdentType = 0
                                         THEN "Tilgode"
                                         ELSE "GavekortType " + STRING(piLoop)
            .
        RELEASE GaveKType.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setGyldighetTilgode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setGyldighetTilgode Procedure 
PROCEDURE setGyldighetTilgode :
/*------------------------------------------------------------------------------
  Purpose:     Setter gyldighetstid for tilgodelapper.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 1 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 50) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 1 
        bSysPara.SysGr       = 1 
        bSysPara.ParaNr      = 50
        bSysPara.Parameter1  = '90'
        bSysPara.Parameter2  = '90'
        bSysPara.Beskrivelse = "Gyldighetstid tilgodesedler og gavekort"
        bSysPara.Hjelpetekst1 = "Parameter 1 tilgodesedler, parameter 2 gavekort"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 1 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 63) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId      = 1 
        bSysPara.SysGr       = 1 
        bSysPara.ParaNr      = 63
        bSysPara.Parameter1  = '0'
        bSysPara.Parameter2  = ''
        bSysPara.Beskrivelse = "Ekstra kontroll av gammelt id på tilgodelapper"
        bSysPara.Hjelpetekst1 = "0-Ingen kontroll. Param.1 inneholder antall siffer i butikknr."
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 1 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 48) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 1 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 48
        bSysPara.Parameter1   = '0'
        bSysPara.Beskrivelse  = "Aktiver eksport OpenOffice"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja Åpner for eksport til OpenOffice"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 1 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 49) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 1 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 49
        bSysPara.Parameter1   = '1'
        bSysPara.Beskrivelse  = "Aktiver lagerstyring"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja Styrer opprettelse av ELogg for ArtPris ved endring av VVArekost"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setHandteringskode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setHandteringskode Procedure 
PROCEDURE setHandteringskode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
0 "Sendes sentrallager" "" ? 0 "" ? 0 ""
1 "Varen skal sendes til leverandør" "" ? 0 "" ? 0 ""
2 "Varen skal tas vare på i butikk" "" 08/04/03 53433 "sport1" 08/04/03 53433 "sport1"
3 "Varen kan kastes" "" 08/04/03 53500 "sport1" ? 0 ""
4 "Varen skal sendes til sentrallager" "" 08/04/03 53481 "sport1" ? 0 ""

------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(FIRST Handtering) THEN
  DO TRANSACTION:
      CREATE Handtering.
      ASSIGN
          Handtering.HandKode    = 0
          Handtering.Beskrivelse = "Sendes sentrallager"
          .
      CREATE Handtering.
      ASSIGN
          Handtering.HandKode    = 1
          Handtering.Beskrivelse = "Varen skal sendes til leverandør"
          .
      CREATE Handtering.
      ASSIGN
          Handtering.HandKode    = 2
          Handtering.Beskrivelse = "Varen skal tas vare på i butikk"
          .
      CREATE Handtering.
      ASSIGN
          Handtering.HandKode    = 3
          Handtering.Beskrivelse = "Varen kan kastes"
          .
      CREATE Handtering.
      ASSIGN
          Handtering.HandKode    = 4
          Handtering.Beskrivelse = "Sendes sentrallager"
          .
      RELEASE Handtering.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setHarbutikksystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setHarbutikksystem Procedure 
PROCEDURE setHarbutikksystem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
    IF INT(syspara.parameter1) = 0 OR INT(syspara.parameter1) >= 120 THEN
        ASSIGN syspara.parameter1 = "120".
------------------------------------------------------------------------------*/
    DEF BUFFER bButiker FOR Butiker.

    DO FOR bButiker TRANSACTION:

        FOR EACH bButiker WHERE
            bButiker.Apningsdato <> ? AND
            CAN-FIND(FIRST Kasse WHERE
                           Kasse.ButikkNr = bButiker.Butik):
            ASSIGN
                bButiker.harButikksystem = TRUE
                .
        END.
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setHlpFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setHlpFil Procedure 
PROCEDURE setHlpFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 1 AND
        SysPara.SysGr  = 1 AND
        SysPara.ParaNr = 2 NO-ERROR.
      IF AVAILABLE SysPara THEN
        DO:
          ASSIGN  
              SysPara.Parameter1  = ".\HLP\InfoPOS.chm"
              SysPara.Beskrivelse =  "Hjelpefil"
              .
          RELEASE SysPara.
        END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setHTType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setHTType Procedure 
PROCEDURE setHTType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
Denso BHT 5000/GEN2
-------------------
*/
IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 1) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 1
        HT-Type.Betegnelse     = "Denso BHT 5000/GEN2"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "ic"
        HT-Type.FilEkstent     = "dat"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\sendes"
        HT-Type.EkspFilPrefix  = ""
        HT-type.EkspFilEkstent = ""
        HT-Type.eksportProg    = ""
        HT-Type.Notat          = ""
        HT-Type.HTAktiv        = FALSE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.

/*
Denso 2000 v. 1.0
------------------
1-4  : Säljare
5-3  : VG 
8-4  : Löpnr
12-1 : Prisgrupp
13-4 : Storlek
17-10: Datum/tid ÅÅMMDDTTMM 
*/
IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 2) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 2
        HT-Type.Betegnelse     = "Denso 2000 v1.0"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "de"
        HT-Type.FilEkstent     = "dat"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\sendes"
        HT-Type.EkspFilPrefix  = ""
        HT-type.EkspFilEkstent = ""
        HT-Type.eksportProg    = ""
        HT-Type.Notat          = ""
        HT-Type.HTAktiv        = FALSE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.

/*
Memor 128 v. 1.0
----------------
1-1  : Postid 
2-4  : VG
5-4  : Löpnr
9-1  : Prisgrupp
10-4: Storlek
14-3: Antal 3 siffror vänsterjusterat spaceutfyllt
17-10: Datum/tid ÅÅMMDDHHMM
*/
IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 3) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 3
        HT-Type.Betegnelse     = "Memor 128 v. 1.0"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "me"
        HT-Type.FilEkstent     = "dat"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\sendes"
        HT-Type.EkspFilPrefix  = ""
        HT-type.EkspFilEkstent = ""
        HT-Type.eksportProg    = ""
        HT-Type.Notat          = ""
        HT-Type.HTAktiv        = FALSE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.

IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 6) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 6
        HT-Type.Betegnelse     = "Symbol PPT 8800"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "varetran"
        HT-Type.FilEkstent     = "<ButNr>"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\kasse"
        HT-Type.EkspFilPrefix  = "prisut"
        HT-type.EkspFilEkstent = "<ButNr>"
        HT-Type.eksportProg    = "eksportHTfil"
        HT-Type.Notat          = "Angis '<ButNr>' i ekstent feltet, vil bruker bli spurt om butikknummer når filen legges ut til håndterminalen."
        HT-Type.HTAktiv        = TRUE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.

IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 7) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 7
        HT-Type.Betegnelse     = "BxCentral  (Gml)"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "INV"
        HT-Type.FilEkstent     = "dat"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\kasse"
        HT-Type.EkspFilPrefix  = "prisut"
        HT-type.EkspFilEkstent = "<ButNr>"
        HT-Type.eksportProg    = "eksportHTfil"
        HT-Type.Notat          = "Angis '<ButNr>' i ekstent feltet, vil bruker bli spurt om butikknummer når filen legges ut til håndterminalen."
        HT-Type.HTAktiv        = FALSE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.

IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 10) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 10
        HT-Type.Betegnelse     = "BxSentral"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "INV"
        HT-Type.FilEkstent     = "<ButNr>"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\sendes"
        HT-Type.EkspFilPrefix  = "Varer"
        HT-type.EkspFilEkstent = "<ButNr>"
        HT-Type.eksportProg    = "eksportHTfil"
        HT-Type.Notat          = "Angis '<ButNr>' i ekstent feltet, vil bruker bli spurt om butikknummer når filen legges ut til håndterminalen."
        HT-Type.HTAktiv        = FALSE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.
ELSE DO TRANSACTION:
  FIND FIRST HT-Type WHERE
    HT-Type.TypeId = 10 NO-ERROR.
  IF AVAILABLE HT-Type THEN 
  DO:
    ASSIGN
      HT-type.EkspFilEkstent = "<ButNr>"
      HT-Type.FilEkstent = "<ButNr>".
    RELEASE HT-Type.
  END.
END.

IF NOT CAN-FIND(FIRST HT-Type WHERE
                HT-Type.TypeId = 12) THEN
DO TRANSACTION:
    CREATE HT-Type.
    ASSIGN
        HT-Type.TypeId = 12
        HT-Type.Betegnelse     = "CubComm"
        HT-Type.Importkatalog  = "c:\home\lindbak\ankommet"
        HT-type.FilPrefix      = "CubCom"
        HT-Type.FilEkstent     = "<ButNr>"
        HT-Type.importProg     = ""
        HT-Type.Eksportkatalog = "c:\home\lindbak\sendes"
        HT-Type.EkspFilPrefix  = "CubVare"
        HT-type.EkspFilEkstent = "<ButNr>"
        HT-Type.eksportProg    = "eksportHTfil"
        HT-Type.Notat          = "Angis '<ButNr>' i ekstent feltet, vil bruker bli spurt om butikknummer når filen legges ut til håndterminalen."
        HT-Type.HTAktiv        = TRUE
        NO-ERROR.
    IF AVAILABLE HT-Type THEN RELEASE HT-Type.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setInitAvdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setInitAvdeling Procedure 
PROCEDURE setInitAvdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piInt AS INT NO-UNDO.

IF CAN-FIND(FIRST Avdeling) THEN
    RETURN.

DO piInt = 1 TO 9:
  CREATE Avdeling.
  ASSIGN
      Avdeling.AvdelingNr = piInt
      Avdeling.AvdelingNavn = "Avdeling " + STRING(piInt)
      .
END.

FOR EACH VarGr:
    IF (HuvGr.AvdelingNr = 0 AND HuvGr.Hg < 10) THEN
        HuvGr.AvdelingNr = HuvGr.Hg.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setJamforEnhet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setJamforEnhet Procedure 
PROCEDURE setJamforEnhet :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:  
                           1    stykk
   2    kilogram
   3    liter
   4    meter
   5    kubikkmeter
   6    kvadratmeter
   7    100 meter
   8    dosering
   9    porsjon
  10    tablett
  11    behandling
  12    vask
  13    Par
  14    hg
                                
                                (stk, kg, hg, m, m2, l)"                                                                                                                                  
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE cIdLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKortNavnLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNavnLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.

DEFINE BUFFER bJamforEnhet FOR JamforEnhet.

ASSIGN
  cIdLst       = '1,2,3,4,5,6,7,8,9,10,11,12,13,14'
  cKortNavnLst = 'stk,kg,l,m,m3,m2,100m,dos,pors,tabl,beh,vask,par,hg'
  cNavnLst     = 'Stykk (stk),Kilo (kg),Liter (l),Meter (m),Kubikkmeter (m3),Kvadratmeter (m2),100 meter (100m),Dosering (dos),Porsjon (pors),Tablett (tabl),Behandling (beh),Vask (vask),Par (par),Hekto (hg)'
  .

DO FOR bJamforEnhet:
  DO piLoop = 1 TO 14:
    IF NOT CAN-FIND(bJamforEnhet WHERE
                    bJamforEnhet.JamforEnhId = piLoop) THEN 
    DO:
      CREATE bJamforEnhet.
      ASSIGN
        bJamforEnhet.JamforEnhId = piLoop
        bJamforEnhet.JamforEnhet = ENTRY(piLoop,cKortNavnLst)
        bJamforEnhet.JamforEnhtekst = ENTRY(piLoop,cNavnLst)
        .
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKampanjeParametre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKampanjeParametre Procedure 
PROCEDURE setKampanjeParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT NO-UNDO.
DEF VAR piLoop2 AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.
DEF BUFFER bSysHode   FOR SysHode.


IF NOT CAN-FIND(SysHode WHERE
                SysHode.SysHId = 17) THEN
DO TRANSACTION:
    CREATE bSysHode.
    ASSIGN
        bSysHode.SysHId      = 17
        bSysHode.Beskrivelse = "Kampanje (MixMatch)"
        .
    RELEASE bSysHode.
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 17 AND
    SysGruppe.SysGr  = 1) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 17
        bSysGruppe.SysGr  = 1
        Beskrivelse       = IF piLoop = 1 THEN "Styreparametre Kampanje"
                            ELSE ""
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 17 AND
    SysGruppe.SysGr  = 10) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 17
        bSysGruppe.SysGr  = 10
        Beskrivelse       = IF piLoop = 1 THEN "Nummerserier"
                            ELSE ""
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

/* tillatt prissavvik for produktfamilier. */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 17 AND
      syspara.sysgr  = 10 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId      = 17 
          bSysPara.SysGr       = 10 
          bSysPara.ParaNr      = 1
          bSysPara.Parameter1  = "1"
          bSysPara.Beskrivelse = "Bruk sentral nummerserie"
          .
      RELEASE bSysPara.
  END.
END.

/* tillatt prissavvik for produktfamilier. */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 17 AND
      syspara.sysgr  = 1 AND
      syspara.paranr = 3) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId      = 17 
          bSysPara.SysGr       = 1 
          bSysPara.ParaNr      = 3
          bSysPara.Parameter1  = "20"
          bSysPara.Beskrivelse = "Tillatt prisavvik +/-"
          .
      RELEASE bSysPara.
  END.
END.

/* KampanjeTilbudstyper som ikke skal brukes. */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 17 AND
      syspara.sysgr  = 1 AND
      syspara.paranr = 4) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 17 
          bSysPara.SysGr        = 1 
          bSysPara.ParaNr       = 4
          bSysPara.Parameter1   = "1,2,3,4"
          bSysPara.Beskrivelse  = "Kamp.tilb.typer som ikke skal brukes"
          bSysPara.Hjelpetekst1 = 'Kommaseparert liste med de tilb.typer som ikke skal brukes.'
          .
      RELEASE bSysPara.
  END.
END.

/* Innlegging av artikler på kampanjetilbud, skal skje uten størrelser i søk.  */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 17 AND
      syspara.sysgr  = 1 AND
      syspara.paranr = 5) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 17 
          bSysPara.SysGr        = 1 
          bSysPara.ParaNr       = 5
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Artikkelsøk med/uten størrelser"
          bSysPara.Hjelpetekst1 = '0-Med, 1-Uten.'
          .
      RELEASE bSysPara.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKampanjeRabattTyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKampanjeRabattTyper Procedure 
PROCEDURE setKampanjeRabattTyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEF VAR cTekst AS CHAR NO-UNDO.

ASSIGN
    cTekst = 'Fordelt rabatt,' +
             'På kjøpet,' + 
             'Undertrykk rabatt,' +
             'Total sum å spare,' +
             'Enhetssum å spare,' +
             'Prosent,' +          
             'Total sum å betale,' +
             'Enhetssum å betale,' +            
             'Enhetsrabatt'
    .
DO piLoop = 1 TO 9:
    IF NOT CAN-FIND(KampRabattType WHERE
                    KampRabattType.KampRabattTypeId = piLoop) THEN
    DO TRANSACTION:
        CREATE KampRabattType.
        ASSIGN
            KampRabattType.KampRabattTypeId   = piLoop
            KampRabattType.KampRabattTypeNavn = ENTRY(piLoop,cTekst)
            .
        RELEASE KampRabattType.
    END.
    ELSE DO TRANSACTION:
      FIND KampRabattType WHERE
        KampRabattType.KampRabattTypeId = piLoop NO-ERROR.
      ASSIGN
        KampRabattType.KampRabattTypeNavn = ENTRY(piLoop,cTekst)
        .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKampanjetilbTyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKampanjetilbTyper Procedure 
PROCEDURE setKampanjetilbTyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEF VAR c1Tekst AS CHAR NO-UNDO.
DEF VAR c2Tekst AS CHAR NO-UNDO.

ASSIGN
    c1Tekst = 'Kjøp X Betal Y ("Betal")|' +
              'Kjøp X Spar Y ("Spar")|' + 
              'Kjøp X Få Y% ("Prosent")|' +
              'Kjøp X Betal for Y ("Betal for")|' + 
              'Ekstrapris kampanje|' +
              'Kjøp X av vare A til pris Y|' +
              'Kjøp X av vare A, Y av vare B til pris Z'
    c2Tekst = '1|2|3|4|9|10|11'.

DO piLoop = 1 TO 7:
    IF NOT CAN-FIND(KampanjeTilbType WHERE
                    KampanjeTilbType.KampTilbTypeId = int(ENTRY(piLoop,c2Tekst,'|'))) THEN
    DO TRANSACTION:
        CREATE KampanjeTilbType.
        ASSIGN
            KampanjeTilbType.KampTilbTypeId   = int(ENTRY(piLoop,c2Tekst,'|'))
            KampanjeTilbType.KampTilbTypeNavn = ENTRY(piLoop,c1Tekst,'|')
            .
        RELEASE KampanjeTilbType.
    END.
    ELSE DO TRANSACTION:
      FIND KampanjeTilbType WHERE
        KampanjeTilbType.KampTilbTypeId = int(ENTRY(piLoop,c2Tekst,'|')) NO-ERROR.
      ASSIGN
        KampanjeTilbType.KampTilbTypeNavn = ENTRY(piLoop,c1Tekst,'|')
        .    
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKamStatistikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKamStatistikk Procedure 
PROCEDURE setKamStatistikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

               
/* Kampanjestatistikk */
DO TRANSACTION:
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "KAMPANJE") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "KAMPANJE"
          StType.Beskrivelse = "Kampanjestatistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "KAMPART") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "KAMPTILB"
          StType.Beskrivelse = "Kampanjetilbudstatistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "KAMPART") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "KAMPART"
          StType.Beskrivelse = "Kampanjeartikkelstatistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
        StType.StTypeId = "NONSALE") THEN
    DO:
        CREATE StType.
        ASSIGN
            StType.StTypeId    = "NONSALE"
            StType.Beskrivelse = "NonSale artikkelstatistikk"
            .
        RUN opprettStDef (INPUT StType.StTypeId).
  END.
  IF NOT CAN-FIND(StType WHERE
                  StType.StTypeId = "LEVERAN-SA") THEN
  DO:
      CREATE StType.
      ASSIGN
          StType.StTypeId    = "LEVERAN-SA"
          StType.Beskrivelse = "Leverandør/sesong statistikk"
          .
      RUN opprettStDef (INPUT StType.StTypeId).
  END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKasseDatoImportFormat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKasseDatoImportFormat Procedure 
PROCEDURE setKasseDatoImportFormat :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 55) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 55
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Datoformat kasse. Import og eksport."
            bSysPara.Hjelpetekst1 = "0-DMY, 1-YMD"
            .
        RELEASE bSysPara.
    END.
  END.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 57) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 57
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Tabellformat varefil til kasse (eksport)."
            bSysPara.Hjelpetekst1 = "0-51 entries, 1-57 Entries"
            .
        RELEASE bSysPara.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKassemodell) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKassemodell Procedure 
PROCEDURE setKassemodell :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       DTL 2.0 ISM Nucleus
------------------------------------------------------------------------------*/
  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  1 AND
        SysPara.SysGr  =  10 AND
        SysPara.ParaNr =  5 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  1 
              SysPara.SysGr  =  10 
              SysPara.ParaNr =  5 
              .
      END.
      ASSIGN  
          Syspara.Beskrivelse = "PRS POS 1.0"
          SysPara.Parameter1  = "PRS POS 1.0"
          .
      RELEASE SysPara.

      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  1 AND
        SysPara.SysGr  =  10 AND
        SysPara.ParaNr =  10 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  1 
              SysPara.SysGr  =  10 
              SysPara.ParaNr =  10 
              .
      END.
      ASSIGN  
          Syspara.Beskrivelse = "InfoPOS POS 8.0"
          SysPara.Parameter1  = "InfoPOS POS 8.0"
          .
      RELEASE SysPara.
 

      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  1 AND
        SysPara.SysGr  =  10 AND
        SysPara.ParaNr =  11 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  1 
              SysPara.SysGr  =  10 
              SysPara.ParaNr =  11 
              .
      END.
      ASSIGN  
          Syspara.Beskrivelse = "PRS Nettbutikk 1.0"
          SysPara.Parameter1  = "PRS Nettbutikk 1.0"
          .
      RELEASE SysPara.

      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  1 AND
        SysPara.SysGr  =  10 AND
        SysPara.ParaNr =  50 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  1 
              SysPara.SysGr  =  10 
              SysPara.ParaNr =  50 
              .
          ASSIGN  
              Syspara.Beskrivelse = "DTL 2.0 ISM Nucleus"
              SysPara.Parameter1  = "DTL 2.0 ISM Nucleus"
              .
          RELEASE SysPara.
        END.
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  1 AND
        SysPara.SysGr  =  10 AND
        SysPara.ParaNr =  51 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  1 
              SysPara.SysGr  =  10 
              SysPara.ParaNr =  51 
              .
          ASSIGN  
              Syspara.Beskrivelse = "N9 BOS Nucleus"
              SysPara.Parameter1  = "N9 BOS Nucleus"
              .
          RELEASE SysPara.
        END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKOrdreLayout) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKOrdreLayout Procedure
PROCEDURE setKOrdreLayout:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR piLoop AS INT NO-UNDO.

    DEF BUFFER bSysPara   FOR SysPara.
    DEF BUFFER bSysGruppe FOR SysGruppe.

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 19 AND
        SysGruppe.SysGr  = 13) THEN
    DO FOR bSysGruppe TRANSACTION:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 19
            bSysGruppe.SysGr  = 13
            Beskrivelse       = "KOrdreLayout"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    DO FOR bSysPara TRANSACTION:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 13 AND
            bSysPara.ParaNr = 1) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 19 
                bSysPara.SysGr        = 13 
                bSysPara.ParaNr       = 1
                bSysPara.Parameter1   = "0"
                bSysPara.Beskrivelse  = "Layout"
                bSysPara.Hjelpetekst1 = "0-Standard layout, 1-Med returseddel"
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setKorrVPIBehandlingsStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKorrVPIBehandlingsStatus Procedure 
PROCEDURE setKorrVPIBehandlingsStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bSysGruppe FOR SysGruppe.
    DEF BUFFER bSysPara   FOR SysPara.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 21 AND
            SysGruppe.SysGr  = 201) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 21
                bSysGruppe.SysGr  = 201
                Beskrivelse      = "Korr. VPI behandlingsstatus"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Ubehandlet"
                bSysPara.Parameter1  = "Ubehandlet".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Manuelt koblet"
                bSysPara.Parameter1  = "Manuelt koblet"
                bSysPara.Parameter2  = "1".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 11) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 11
                bSysPara.Beskrivelse = "Automatisk koblet"
                bSysPara.Parameter1  = "Automatisk koblet"
                bSysPara.Parameter2  = "1".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 20) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 20
                bSysPara.Beskrivelse = "Manuelt opprettet"
                bSysPara.Parameter1  = "Manuelt opprettet"
                bSysPara.Parameter2  = "1".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 21) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 21
                bSysPara.Beskrivelse = "Automatisk koblet"
                bSysPara.Parameter1  = "Automatisk koblet"
                bSysPara.Parameter2  = "1".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 79) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 79
                bSysPara.Beskrivelse = "Til oppfølging"
                bSysPara.Parameter1  = "Til oppfølging".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 80) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 80
                bSysPara.Beskrivelse = "Avvist"
                bSysPara.Parameter1  = "Avvist".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 90) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 90
                bSysPara.Beskrivelse = "Sendt"
                bSysPara.Parameter1  = "Sendt"
                bSysPara.Parameter2  = "1"
                .
            RELEASE bSysPara.
        END.
    END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKorrVPILev) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKorrVPILev Procedure 
PROCEDURE setKorrVPILev :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Oppretter nødvendige eksterne VPI leverandører */
  RUN opprett_korrvpilev.p (lHK).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKundeButikkNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKundeButikkNr Procedure 
PROCEDURE setKundeButikkNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH Kunde EXCLUSIVE-LOCK WHERE
    Kunde.ButikkNr = 0:
    Kunde.butikkNr = iCL.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKundeordreKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKundeordreKasse Procedure 
PROCEDURE setKundeordreKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bKasse FOR Kasse.
                      
  FOR EACH Butiker EXCLUSIVE-LOCK WHERE
      CAN-FIND(FIRST Kasse WHERE
               Kasse.ButikkNr = Butiker.Butik AND
               Kasse.GruppeNr = 1 AND
               Kasse.KasseNr  = 1):
      IF NOT CAN-FIND(bKasse WHERE
                      bKasse.ButikkNr = Butiker.Butik AND
                      bKasse.GruppeNr = 1 AND
                      bKasse.KasseNr  = 99) THEN
      DO:
          FIND Kasse NO-LOCK WHERE
               Kasse.ButikkNr = Butiker.Butik AND
               Kasse.GruppeNr = 1 AND
               Kasse.KasseNr  = 1 NO-ERROR.
          IF AVAILABLE Kasse THEN
          DO:
              CREATE bKasse.
              BUFFER-COPY Kasse TO bKasse
                  ASSIGN
                  bKasse.KasseNr     = 99
                  bKasse.Navn        = "Kasse 99"
                  bKasse.ElJournalId = STRING(Butiker.Butik) + ";99".
          END.

      END.

      ASSIGN
          Butiker.KasseNr = 99.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKundeordrestatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKundeordrestatus Procedure 
PROCEDURE setKundeordrestatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.
DEF VAR pcText AS CHAR NO-UNDO.
DEFINE VARIABLE pcNrLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcMailLst AS CHARACTER NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

ASSIGN
    pcText = "Opprettet,Tilbud sendt,Bekreftet,Dellevert/fakturert,Fakturert,Makulert"
    .

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 19 AND
    SysGruppe.SysGr  = 1) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 19
        bSysGruppe.SysGr  = 1
        Beskrivelse      = "Kundeordrestatus"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 19 AND
    SysGruppe.SysGr  = 3) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 19
        bSysGruppe.SysGr  = 3
        Beskrivelse      = "Hurtigordre"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */
IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 19 AND
    SysGruppe.SysGr  = 9) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 19
        bSysGruppe.SysGr       = 9
        bSysGruppe.Beskrivelse = "Kundeordre"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 9 AND
        bSysPara.ParaNr = 1) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 9 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = '0'
            bSysPara.Beskrivelse = 'Integrasjon postpakke etikettskriver'
            bSyspara.Hjelpetekst1 = "0-Ingen, 1-Consignor, 2-UniFaun"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 9 AND
        bSysPara.ParaNr = 2) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 9 
            bSysPara.ParaNr      = 2
            bSysPara.Parameter1  = '0'
            bSysPara.Beskrivelse = 'Ved ordreimport. Setting av varetekst'
            bSyspara.Hjelpetekst1 = "0-Varetekst fra nettbutikk, 1-Vg/LopNr, 2-Artikkels varetekst"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 9 AND
        bSysPara.ParaNr = 3) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 9 
            bSysPara.ParaNr      = 3
            bSysPara.Parameter1  = '0'
            bSysPara.Beskrivelse = 'Start kundeordre i Nettbutikk modus'
            bSyspara.Hjelpetekst1 = "0-Nei, 1-Ja"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 9 AND
        bSysPara.ParaNr = 10) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 9 
            bSysPara.ParaNr      = 10
            bSysPara.Parameter1  = 'Åpent kjøp'
            bSysPara.Beskrivelse = 'Åpent kjøp tekst på kundeordre'
            bSyspara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 9 AND
        bSysPara.ParaNr = 11) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 9 
            bSysPara.ParaNr      = 11
            bSysPara.Parameter1  = '0'
            bSysPara.Beskrivelse = 'Vis medlems kortnr. på kundeordre'
            bSyspara.Hjelpetekst1 = "0-Nei,1-Ja"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 9 AND
        bSysPara.ParaNr = 12) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 9 
            bSysPara.ParaNr      = 12
            bSysPara.Parameter1  = '0'
            bSysPara.Parameter2  = ''
            bSysPara.Beskrivelse = 'Vis RETUR knapp på kundeordre'
            bSyspara.Hjelpetekst1 = "0-Nei,1-Ja"
            bSysPara.Hjelpetekst2 = 'Butikkliste på butikker som får se ReturKodeRegister knappen.'
            .
        RELEASE bSysPara.
    END.

    LOOP:
    DO piLoop = 1 TO 6:
        IF NOT CAN-FIND(bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 1 AND
            bSysPara.ParaNr = piLoop * 10) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 19 
                bSysPara.SysGr       = 1 
                bSysPara.ParaNr      = piLoop * 10
                bSysPara.Parameter1  = ENTRY(piLoop,pcText)
                bSysPara.Beskrivelse = ENTRY(piLoop,pcText)
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
    
    IF NOT CAN-FIND(bSysPara WHERE
        bSysPara.SysHId = 19 AND
        bSysPara.SysGr  = 3 AND
        bSysPara.ParaNr = 1) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 19 
            bSysPara.SysGr       = 3 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = ''
            bSysPara.Beskrivelse = 'Kundenr. tilfeldig kunde'
            .
        RELEASE bSysPara.
    END.
    
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKundespesifike) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKundespesifike Procedure 
PROCEDURE setKundespesifike :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bSysGruppe FOR SysGruppe.
    DEF BUFFER bSysPara   FOR SysPara.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 200) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 200
                bSysGruppe.Beskrivelse      = "Anton stlinje art. to TimeGrip Butikkinfo"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 201) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 201
                bSysGruppe.Beskrivelse      = "Anton TimeGrip - Rapportparametre"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Eksportdir TimeGrip"
                bSysPara.Parameter1  = "c:\home\lindbak\sendes".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 2) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 2
                bSysPara.Beskrivelse = "Antall dager før idag"
                bSysPara.Parameter1  = "0".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 3) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 3
                bSysPara.Beskrivelse = "Første rapportdag"
                bSysPara.Parameter1  = "01/01/08".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 5) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 5
                bSysPara.Beskrivelse = "Overför FTP"
                bSysPara.Parameter1  = "0".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 6) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 6
                bSysPara.Beskrivelse = "Ftp-host"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 201 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 201 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Ftp-bruker"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 250) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 250
                bSysGruppe.Beskrivelse      = "Preem - Mottagsrapport"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 250 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 250 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Rapportkatalog"
                bSysPara.Parameter1  = "c:\home\lindbak\sendes".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 250 AND
                        bSysPara.ParaNr = 2) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 250 
                bSysPara.ParaNr      = 2
                bSysPara.Beskrivelse = "DTL-fördröjning-antal dagar"
                bSysPara.Parameter1  = "4".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 250 AND
                        bSysPara.ParaNr = 3) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 250 
                bSysPara.ParaNr      = 3
                bSysPara.Beskrivelse = "XML-fördröjning-antal dagar"
                bSysPara.Parameter1  = "0".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 250 AND
                        bSysPara.ParaNr = 4) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 250 
                bSysPara.ParaNr      = 4
                bSysPara.Beskrivelse = "Visa OK-butiker"
                bSysPara.Parameter1  = "N".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 250 AND
                        bSysPara.ParaNr = 5) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 250 
                bSysPara.ParaNr      = 5
                bSysPara.Beskrivelse = "Sänd email"
                bSysPara.Parameter1  = "N".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 250 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 250 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Emaillista"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 251) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 251
                bSysGruppe.Beskrivelse = "Preem - BFdagrapport"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 251 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 251 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Rapportkatalog"
                bSysPara.Parameter1  = "c:\home\lindbak\sendes".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 251 AND
                        bSysPara.ParaNr = 5) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 251 
                bSysPara.ParaNr      = 5
                bSysPara.Beskrivelse = "Sänd email"
                bSysPara.Parameter1  = "N".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 251 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 251 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Emaillista"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 252) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 252
                bSysGruppe.Beskrivelse = "Preem - Errorlog"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 252 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 252 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Rapportkatalog"
                bSysPara.Parameter1  = "c:\home\lindbak\sendes".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 252 AND
                        bSysPara.ParaNr = 5) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 252 
                bSysPara.ParaNr      = 5
                bSysPara.Beskrivelse = "Sänd email"
                bSysPara.Parameter1  = "N".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 252 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 252 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Emaillista"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 253) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 253
                bSysGruppe.Beskrivelse = "Preem - ACNielsen"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 253 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 253 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Rapportkatalog"
                bSysPara.Parameter1  = "c:\home\lindbak\sendes\ACNilesen".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 253 AND
                        bSysPara.ParaNr = 5) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 253 
                bSysPara.ParaNr      = 5
                bSysPara.Beskrivelse = "Sänd email"
                bSysPara.Parameter1  = "N".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 253 AND
                        bSysPara.ParaNr = 6) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 253 
                bSysPara.ParaNr      = 6
                bSysPara.Beskrivelse = "Filprefix"
                bSysPara.Parameter1  = "Infopos_ACN_".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 253 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 253 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Emaillista"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 270) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 270
                bSysGruppe.Beskrivelse      = "Standardoppsett dagsrapport/butikk"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */

        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 257) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 257
                bSysGruppe.Beskrivelse = "Preem - EloggServerNucleus"
                .
            RELEASE bSysGruppe.
        END. 
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 257 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 210 
                bSysPara.SysGr        = 257 
                bSysPara.ParaNr       = 1
                bSysPara.Beskrivelse  = "Kjør klargjøring av priskø"
                bSysPara.Parameter1   = "1"
                bSyspara.Hjelpetekst1 = "0-Nei, 1-Ja"
                bSyspara.Hjelpetekst2 = "Slå av klargjøring av priskø hvis denne kjøres via WinScheduler.".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 257 AND
                        bSysPara.ParaNr = 2) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 210 
                bSysPara.SysGr        = 257 
                bSysPara.ParaNr       = 2
                bSysPara.Beskrivelse  = "Kjør utlegg av xml filer"
                bSysPara.Parameter1   = "1"
                bSyspara.Hjelpetekst1 = "0-Nei, 1-Ja"
                bSyspara.Hjelpetekst2 = "Slå av utlegg av xml filer hvis denne kjøres via WinScheduler.".
            RELEASE bSysPara.
        END.

    END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKundeSpesifikkeParametre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKundeSpesifikkeParametre Procedure 
PROCEDURE setKundeSpesifikkeParametre :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

    DEF BUFFER bSysGruppe FOR SysGruppe.
    DEF BUFFER bSysPara   FOR SysPara.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 210 AND
            SysGruppe.SysGr  = 202) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 210
                bSysGruppe.SysGr  = 202
                bSysGruppe.Beskrivelse      = "Sport1"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 202 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 202 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Siste eksportdato for lager"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 210 AND
                        bSysPara.SysGr  = 202 AND
                        bSysPara.ParaNr = 2) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 210 
                bSysPara.SysGr       = 202 
                bSysPara.ParaNr      = 2
                bSysPara.Beskrivelse = "Leverandører som skal ha lager til purpleo"
                bSysPara.Parameter1  = "".
            RELEASE bSysPara.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setKupongtyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setKupongtyper Procedure 
PROCEDURE setKupongtyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEFINE VARIABLE cTekst  AS CHAR NO-UNDO.

FIND FIRST KupongEier NO-LOCK NO-ERROR.
IF NOT AVAILABLE KupongEier THEN 
DO TRANSACTION:
    CREATE KupongEier.
    ASSIGN
    KupongEier.KEierNr = 1
    KupongEier.KENavn  = 'Kupongeier'.
    FIND CURRENT Kupongeier NO-LOCK NO-ERROR.
END.

ASSIGN
    cTekst = ". (Brukes ikke)," + /* Värdeavi går ikke som kupong */           
             "B Betalcheck," + 
             "C Värdecheck," +
             "R Rabattkupong," +
             "E Elektronis kupong," + 
             "M Mobil kupong," + 
             "P Produktcheck," + 
             "V Varucheck," + 
             "C Presentkort," + 
             "? Övrigt"
             .

DO piLoop = 1 TO 10 TRANSACTION:
    FIND Kupongtype EXCLUSIVE-LOCK WHERE
         KupongType.KTypeNr = piLoop NO-ERROR.
    IF NOT AVAILABLE KupongType THEN 
    DO:
        CREATE KupongType.
        ASSIGN
        KupongType.KTypeNr   = piLoop.
    END.
    ASSIGN 
        KupongType.KTypeKode = ENTRY(1,ENTRY(piLoop,cTekst),' ')
        KupongType.KTypeNavn = ENTRY(piLoop,cTekst)
        .
    RELEASE KupongType.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setLeveringsform) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLeveringsform Procedure 
PROCEDURE setLeveringsform :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cListe AS CHAR NO-UNDO.
DEF VAR cNavn  AS CHAR NO-UNDO.

ASSIGN
    cListe = "1,2,3"
    cNavn  = "Utlevering i butikk,Postpakke,DHL"
    .

DO piLoop = 1 TO NUM-ENTRIES(cListe):
    /* Skal alltid være fast 
    IF NOT CAN-FIND(LeveringsForm WHERE
                    LeveringsForm.LevFNr = int(entry(piLoop,cListe))) THEN
    */
    DO TRANSACTION:
        FIND Leveringsform WHERE
            LeveringsForm.LevFNr = int(ENTRY(piLoop,cListe)) NO-ERROR.
        IF NOT AVAILABLE LeveringsForm THEN
        DO:
            CREATE LeveringsForm.
            ASSIGN
                LeveringsForm.LevFNr = int(ENTRY(piLoop,cListe))
                .
        END.
        ASSIGN
            LeveringsForm.LevformBeskrivelse = ENTRY(piLoop,cNavn)
            LeveringsForm.LevformMetode      = ENTRY(piLoop,cNavn)
            .
        RELEASE LeveringsForm.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setLinkVareant) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLinkVareant Procedure
PROCEDURE setLinkVareant:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
  ArtBas.LinkVareant = 0:
  ArtBas.LinkVareant = 1.    
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setLoggingAvBatchserver) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLoggingAvBatchserver Procedure 
PROCEDURE setLoggingAvBatchserver :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 200 AND
    SysGruppe.SysGr  = 200) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 200
        bSysGruppe.SysGr  = 200
        Beskrivelse      = "Logging av batchprosesser"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 200 AND
    SysPara.SysGr  = 200) THEN
DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 1:
        IF NOT CAN-FIND(FIRST SysPara WHERE
            bSysPara.SysHId = 200 AND
            bSysPara.SysGr  = 200 AND
            bSysPara.ParaNr = piLoop) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 200 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = "0"
                bSysPara.Beskrivelse = (IF      piLoop = 1 THEN "Logg Datamottaksserver"
                                        ELSE bSysPara.Beskrivels)
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setMayFlower) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMayFlower Procedure
PROCEDURE setMayFlower:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 201) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 50
        bSysGruppe.SysGr  = 201
        Beskrivelse      = "Mayflower Loyalty WebService"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 50 AND
        bSysPara.SysGr  = 201 AND
        bSysPara.ParaNr = 1) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 201 
            bSysPara.ParaNr       = 1
            bSysPara.Beskrivelse  = "Brukerid og passord til MayFlower"
            bSysPara.Parameter1   = 'polygon'
            bSysPara.Parameter2   = '2xkCXjz0ic'
            bSysPara.Hjelpetekst1 = "Brukerid"
            bSysPara.Hjelpetekst2 = "Passord"
            .
        RELEASE bSysPara.
    END.
    
    /* checkService */
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 50 AND
        bSysPara.SysGr  = 201 AND
        bSysPara.ParaNr = 10) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 201 
            bSysPara.ParaNr       = 10
            bSysPara.Beskrivelse  = "CheckService"
            bSysPara.Parameter1   = 'C:\appdir\se\WS\MayFlower\WSCheck\wsdl\CheckService.wsdl'
            bSysPara.Parameter2   = '-nohostverify'
            bSysPara.Hjelpetekst1 = "Sti og filnavn til wsdl fil"
            bSysPara.Hjelpetekst2 = "Parametre til oppkobling"
            .
        RELEASE bSysPara.
    END.

    /* TransactionService */
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 50 AND
        bSysPara.SysGr  = 201 AND
        bSysPara.ParaNr = 20) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 201 
            bSysPara.ParaNr       = 20
            bSysPara.Beskrivelse  = "TransactionService"
            bSysPara.Parameter1   = 'C:\Appdir\SE\WS\MayFlower\WSTransaction\wsdl\TransactionService.wsdl'
            bSysPara.Parameter2   = '-nohostverify'
            bSysPara.Hjelpetekst1 = "Sti og filnavn til wsdl fil"
            bSysPara.Hjelpetekst2 = "Parametre til oppkobling"
            .
        RELEASE bSysPara.
    END.

    /* MemberService */
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 50 AND
        bSysPara.SysGr  = 201 AND
        bSysPara.ParaNr = 30) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 201 
            bSysPara.ParaNr       = 30
            bSysPara.Beskrivelse  = "MemberService"
            bSysPara.Parameter1   = 'C:\Appdir\SE\WS\MayFlower\WSMember\wsdl\MemberService.wsdl'
            bSysPara.Parameter2   = '-nohostverify'
            bSysPara.Hjelpetekst1 = "Sti og filnavn til wsdl fil"
            bSysPara.Hjelpetekst2 = "Parametre til oppkobling"
            .
        RELEASE bSysPara.
    END.
END. /* TRANSACTION */
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setMedlemsrabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setMedlemsrabatt Procedure 
PROCEDURE setMedlemsrabatt :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 14 AND
    SysGruppe.SysGr  = 3) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 14
        bSysGruppe.SysGr  = 3
        Beskrivelse      = "Oppsett av tekster på rabattsjekk"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

ASSIGN
  cTekst = 'Gjelder som rabatt i alle våre butikker.' + '|' + 
           'Kan ikke byttes mot kontanter.' + '|' + 
           '' + '|' +
           'Gyldig t.o.m.' + '|' + 
           'kroner' + '|' +
           'Serienr.' + '|' +
           'Rabattsjekker som mistes erstattes ikke. Innløst rabattsjekk regnes som brukt. Gis ikke tilbake ved retur av varer.' + '|' +
           'Medlemsnummer'.

DO FOR bSysPara TRANSACTION:
  DO piLoop = 1 TO NUM-ENTRIES(cTekst,'|'):
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 14 AND
        bSysPara.SysGr  = 3 AND
        bSysPara.ParaNr = piLoop) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 3 
            bSysPara.ParaNr       = piLoop
            bSysPara.Hjelpetekst1 = ENTRY(piLoop,cTekst,'|')
            bSysPara.Parameter1   = ENTRY(piLoop,cTekst,'|')
            bSysPara.Beskrivelse  = 'Tekst ' + STRING(piLoop)
            .
        RELEASE bSysPara.
    END.
  END.
END. /* bSysPara TRANSACTION*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setOnLineLeverandor) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOnLineLeverandor Procedure
PROCEDURE setOnLineLeverandor:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE BUFFER bufOnLineLeverandor FOR OnLineLeverandor.

DO FOR bufOnLineLeverandor:
  IF NOT CAN-FIND(bufOnLineLeverandor WHERE 
                  bufOnLineLeverandor.OnLineLevNr = 1) THEN 
      DO: 
          CREATE bufOnLineLeverandor.
          ASSIGN
              bufOnLineLeverandor.OnLineLevNr    = 1
              bufOnLineLeverandor.OnLineLevBeskr = 'BlackHawk'
              .
          RELEASE bufOnLineLeverandor.
      END.    
  IF NOT CAN-FIND(bufOnLineLeverandor WHERE 
        bufOnLineLeverandor.OnLineLevNr = 2) THEN 
    DO: 
        CREATE bufOnLineLeverandor.
        ASSIGN
            bufOnLineLeverandor.OnLineLevNr    = 2
            bufOnLineLeverandor.OnLineLevBeskr = 'VärdeAvi KupongInnlösen'
            .
        RELEASE bufOnLineLeverandor.
    END.    
    IF NOT CAN-FIND(bufOnLineLeverandor WHERE 
                  bufOnLineLeverandor.OnLineLevNr = 3) THEN 
      DO: 
          CREATE bufOnLineLeverandor.
          ASSIGN
              bufOnLineLeverandor.OnLineLevNr    = 3
              bufOnLineLeverandor.OnLineLevBeskr = 'MayFlower'
              .
          RELEASE bufOnLineLeverandor.
      END.    
END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setOppdaterEkstErp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOppdaterEkstErp Procedure 
PROCEDURE setOppdaterEkstErp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bSysPara FOR SysPara.

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 200 AND
        bSysPara.SysGr  = 2 AND
        bSysPara.ParaNr = 100) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 200 
            bSysPara.SysGr        = 2 
            bSysPara.ParaNr       = 100
            bSysPara.Parameter1   = "0"
            bSysPara.Parameter2   = "c:\home\lindbak\sendes"
            bSysPara.Hjelpetekst1 = "0=Nei, 1=Ja"
            bSysPara.Hjelpetekst2 = "Filkatalog for plassering av eksportfiler."
            bSysPara.Beskrivelse  = "Overføre finans/bongdata til PRO"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 200 AND
        bSysPara.SysGr  = 2 AND
        bSysPara.ParaNr = 101) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 200 
            bSysPara.SysGr        = 2 
            bSysPara.ParaNr       = 101
            bSysPara.Parameter1   = "0"
            bSysPara.Hjelpetekst1 = "0=Nei, 1=Ja"
            bSysPara.Beskrivelse  = "Overføre finans/bongdata til Preem"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 200 AND
        bSysPara.SysGr  = 2 AND
        bSysPara.ParaNr = 102) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 200 
            bSysPara.SysGr        = 2 
            bSysPara.ParaNr       = 102
            bSysPara.Parameter1   = "0"
            bSysPara.Parameter2   = "c:\home\lindbak\sendes\;c:\home\lindbak\sendes\flyttprofiler.bat"
            bSysPara.Hjelpetekst1 = "0=Nei, 1=Ja"
            bSysPara.Hjelpetekst2 = "Semikolonseparerat, entry 1 = katalogen, entry 2 = bat-programmet"
            bSysPara.Beskrivelse  = "Flytte salgsdatafiler til Pro"
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOrdreStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOrdreStatus Procedure 
PROCEDURE setOrdreStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pcTekst AS CHAR NO-UNDO.        
    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcKode  AS CHAR NO-UNDO.

    DEF BUFFER bSysPara   FOR SysPara.
    DEF BUFFER bSysGruppe FOR SysGruppe.

    ASSIGN
        pcTekst = "Ny ordre,Sendt leverandør,Delhvis bekreftet,Bekreftet,Delhvis levert,Sluttlevert"
        pcKode  = "NY,SENDT,DBEKR,BEKR,DLEV,SLEV"
        .

    DO FOR bSysPara TRANSACTION:
        LOOP:
        DO piLoop = 1 TO 6:
            FIND FIRST bSysPara EXCLUSIVE-LOCK WHERE
                bSysPara.SysHId = 5 AND
                bSysPara.SysGr  = 3 AND
                bSysPara.ParaNr = piLoop NO-ERROR.
            IF NOT AVAILABLE bsysPara THEN
            DO:
                CREATE bSysPara.
                ASSIGN  
                    bSysPara.SysHId      = 5 
                    bSysPara.SysGr       = 3 
                    bSysPara.ParaNr      = piLoop
                    .
            END.
            ASSIGN
                bSysPara.Parameter1  = ENTRY(piLoop,pcKode)
                bSysPara.Beskrivelse = ENTRY(piLoop,pcTekst)
                .
            RELEASE bSysPara.
        END. /* LOOP */
    END. /* bSysPara TRANSACTION*/

    ASSIGN
        pcTekst = "Ny ordre,Redigeres,Sendt leverandør"
        pcKode  = "NY,REDIGER,SENDT"
        .

    DO FOR bSysPara TRANSACTION:
        LOOP:
        DO piLoop = 1 TO NUM-ENTRIES(pcKode):
            FIND FIRST bSysPara EXCLUSIVE-LOCK WHERE
                bSysPara.SysHId = 5 AND
                bSysPara.SysGr  = 7 AND
                bSysPara.ParaNr = (IF piLoop > 1 THEN piLoop * 10 ELSE piLoop) NO-ERROR.
            IF NOT AVAILABLE bsysPara THEN
            DO:
                CREATE bSysPara.
                ASSIGN  
                    bSysPara.SysHId      = 5 
                    bSysPara.SysGr       = 7 
                    bSysPara.ParaNr      = (IF piLoop > 1 THEN piLoop * 10 ELSE piLoop)
                    .
            END.
            ASSIGN
                bSysPara.Parameter1  = ENTRY(piLoop,pcKode)
                bSysPara.Beskrivelse = ENTRY(piLoop,pcTekst)
                .
            RELEASE bSysPara.
        END. /* LOOP */
    END. /* bSysPara TRANSACTION*/

   IF NOT CAN-FIND(SysGruppe WHERE       
       SysGruppe.SysHId = 5 AND          
       SysGruppe.SysGr  = 7) THEN        
   DO FOR bSysGruppe TRANSACTION:        
       CREATE bSysGruppe.                
       ASSIGN                            
           bSysGruppe.SysHId = 5         
           bSysGruppe.SysGr  = 7         
           Beskrivelse      = "Ordrestatus suppleringsordre" 
           .                             
       RELEASE bSysGruppe.               
   END. /* bSysGruppe TRANSACTION */     

  FOR EACH PlListeHode EXCLUSIVE-LOCK WHERE
    PlListeHode.plListeStatus = 2:
    plListeHode.plListeStatus = 30.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOverforingsparametreArtikkelkort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOverforingsparametreArtikkelkort Procedure 
PROCEDURE setOverforingsparametreArtikkelkort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 11 AND
    SysGruppe.SysGr  = 4) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 11
        bSysGruppe.SysGr  = 4
        Beskrivelse      = "Parametre overføring artikkelkort"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 11 AND
    SysGruppe.SysGr  = 6) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 11
        bSysGruppe.SysGr  = 6        
        Beskrivelse       = "Posteringsregler overføinger"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 11 AND
    bSysPara.SysGr  = 6 AND
    bSysPara.ParaNr = 1) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 11 
        bSysPara.SysGr        = 6 
        bSysPara.ParaNr       = 1
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "Bruk TBId = 2 (Varer på vei)"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 11 AND
    bSysPara.SysGr  = 6 AND
    bSysPara.ParaNr = 2) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 11 
        bSysPara.SysGr        = 6 
        bSysPara.ParaNr       = 2
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "SerieScanner overføringer"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 11 AND
    bSysPara.SysGr  = 4 AND
    bSysPara.ParaNr = 1) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 11 
        bSysPara.SysGr        = 4 
        bSysPara.ParaNr       = 1
        bSysPara.Parameter1   = "1"
        bSysPara.Beskrivelse  = "Default opprette ny overf.ordre"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 11 AND
    bSysPara.SysGr  = 4 AND
    bSysPara.ParaNr = 2) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 11 
        bSysPara.SysGr        = 4 
        bSysPara.ParaNr       = 2
        bSysPara.Parameter1   = "1"
        bSysPara.Beskrivelse  = "Default direkte oppdatering"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 11 AND
    bSysPara.SysGr  = 4 AND
    bSysPara.ParaNr = 3) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 11 
        bSysPara.SysGr        = 4 
        bSysPara.ParaNr       = 3
        bSysPara.Parameter1   = "1"
        bSysPara.Beskrivelse  = "Default utskrift"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setOverforingsreglerMellombutikker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setOverforingsreglerMellombutikker Procedure
PROCEDURE setOverforingsreglerMellombutikker:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 11 AND
    SysGruppe.SysGr  = 7) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 11
        bSysGruppe.SysGr  = 7        
        Beskrivelse       = "Overføringsregler mellom butikker"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 1 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 1 
              .
          ASSIGN  
              SysPara.Parameter1   = "0"
              SysPara.Beskrivelse  = "Aktivere overføringsregler?"
              SysPara.Hjelpetekst1 = "0-Nei, 1-Ja Avgjør om aktiveringsregler skal benyttes mellom butikkene."
              .
          RELEASE SysPara.
        END.
        
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 2 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 2 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: Vanlige butikker"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.
        END.
        
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 3 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 3 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: Sentrallager"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.
        END.
        
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 4 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 4 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: Outlet"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.          
        END.
        
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 5 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 5 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: NettbutikkLager"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.
        END.
        
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 6 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 6 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: NettbutikkVenteLager"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.
        END.
        
    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 7 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 7 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: Eksternebutikker"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.
        END.

    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 8 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 8 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Parameter2   = ""
              SysPara.Beskrivelse  = "Grp: PlussMinusbutikker"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen."
              SysPara.Hjelpetekst1 = "Liste over butikker som gruppen IKKE kan overføre til."
              .
          RELEASE SysPara.
        END.

    FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  11 AND
        SysPara.SysGr  =  7 AND
        SysPara.ParaNr = 9 NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  11 
              SysPara.SysGr  =  7 
              SysPara.ParaNr = 9 
              .
          ASSIGN  
              SysPara.Parameter1   = ""
              SysPara.Beskrivelse  = "Grp: KanIkkeOverforesTil"
              SysPara.Hjelpetekst1 = "Liste over butikker i gruppen som ingen butikker kan overøfre til."
              .
          RELEASE SysPara.
        END.
        
END. /* TRANSACTION */



END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setPakningsenhet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPakningsenhet Procedure 
PROCEDURE setPakningsenhet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  2 AND
        SysPara.SysGr  =  4 AND
        SysPara.ParaNr = 10 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  2 
              SysPara.SysGr  =  4 
              SysPara.ParaNr = 10 
              .
          ASSIGN  
              Syspara.Beskrivelse = "Pakningsenheter"
              SysPara.Parameter1  = "Stk,Stk.,Par,Pose,PK,Rull,Eske,Pkn,Boks,Sett,Syk"
              .
          RELEASE SysPara.
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-setPrisregister) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPrisregister Procedure
PROCEDURE setPrisregister:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 2 AND
    SysGruppe.SysGr  = 20) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 2
        bSysGruppe.SysGr  = 20
        Beskrivelse      = "Prisregister parametre"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 2 AND
    bSysPara.SysGr  = 20 AND
    bSysPara.ParaNr = 1) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 2 
        bSysPara.SysGr        = 20 
        bSysPara.ParaNr       = 1
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "Åpne vedlikehold av innpris"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



 
&IF DEFINED(EXCLUDE-setPRSOverforing) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPRSOverforing Procedure
PROCEDURE setPRSOverforing:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE BUFFER bSysPara FOR SysPara.

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 1 AND
    bSysPara.SysGr  = 1 AND
    bSysPara.ParaNr = 62) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 1 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 62
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "App server kommunikasjon til PRS POS"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setPRSPos_Parametre) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPRSPos_Parametre Procedure
PROCEDURE setPRSPos_Parametre:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 200 AND
    SysGruppe.SysGr  = 10) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 200
        bSysGruppe.SysGr  = 10
        Beskrivelse      = "PRS POS parametre"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 200 AND
    bSysPara.SysGr  = 10 AND
    bSysPara.ParaNr = 1) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 200 
        bSysPara.SysGr        = 10 
        bSysPara.ParaNr       = 1
        bSysPara.Parameter1   = "0"
        bSysPara.Beskrivelse  = "Send filer til PRS POS"
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 200 AND
    bSysPara.SysGr  = 10 AND
    bSysPara.ParaNr = 2) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 200 
        bSysPara.SysGr        = 10 
        bSysPara.ParaNr       = 2
        bSysPara.Parameter1   = ""
        bSysPara.Beskrivelse  = "IP adresse på Host"
        bSysPara.Hjelpetekst1 = ""
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

IF NOT CAN-FIND(bSysPara WHERE
    bSysPara.SysHId = 200 AND
    bSysPara.SysGr  = 10 AND
    bSysPara.ParaNr = 3) THEN
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 200 
        bSysPara.SysGr        = 10 
        bSysPara.ParaNr       = 3
        bSysPara.Parameter1   = ""
        bSysPara.Beskrivelse  = "Portnr. på host"
        bSysPara.Hjelpetekst1 = ""
        .
    RELEASE bSysPara.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setPubBruker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPubBruker Procedure 
PROCEDURE setPubBruker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
    /* PUB-bruker */
    IF NOT CAN-FIND(skotex._user WHERE skotex._user._userid = "pub") THEN 
        DO:
          CREATE skotex._user.
          ASSIGN skotex._user._userid = "pub"
                 skotex._user._password = ENCODE("pub").
          RELEASE skotex._user.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setPurretrinn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setPurretrinn Procedure 
PROCEDURE setPurretrinn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cListe AS CHAR NO-UNDO.
DEF VAR cNavn  AS CHAR NO-UNDO.

ASSIGN
    cListe = "1,2,3"
    cNavn  = "Purretrinn 1,Purretrinn 2,Purretrinn 3"
    .

DO piLoop = 1 TO NUM-ENTRIES(cListe):
    IF NOT CAN-FIND(Purretrinn WHERE
                    Purretrinn.PurreTrinn = int(ENTRY(piLoop,cListe))) THEN
    DO TRANSACTION:
        CREATE Purretrinn.
        ASSIGN
            Purretrinn.Purretrinn = int(ENTRY(piLoop,cListe))
            Purretrinn.Purretekst = ENTRY(piLoop,cNavn)
            Purretrinn.DagerForfallt = int(ENTRY(piLoop,cListe)) * 10
            .
        RELEASE Purretrinn.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRabSjekkType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRabSjekkType Procedure 
PROCEDURE setRabSjekkType :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DO TRANSACTION:
  IF NOT CAN-FIND(RabSjekkType WHERE 
                  RabSjekkType.RabSjekkTypeNr = 1) THEN 
  DO:
    CREATE RabSjekkType.
    ASSIGN
        RabsjekkType.RabsjekkTypeNr          = 1
        RabSjekkType.RabSjekkTypeBeskrivelse = 'Rabattsjekk'
        RabSjekkType.ArtikkelNr              = 0
        RabSjekkType.TerskelTildeling        = 2000
        RabSjekkType.VerdiPaSjekk            = 100
        RabSjekkType.Kommentar               = 'Rabattsjekk tildeles hvis kjøpet overstiger verdi på angitt terskel innenfor en tidsperiode.'
        .
  END.
  IF NOT CAN-FIND(RabSjekkType WHERE 
                  RabSjekkType.RabSjekkTypeNr = 2) THEN 
  DO:
    CREATE RabSjekkType.
    ASSIGN
        RabsjekkType.RabsjekkTypeNr          = 2
        RabSjekkType.RabSjekkTypeBeskrivelse = 'Medlemsrabatt sjekk'
        RabSjekkType.ArtikkelNr              = 0
        RabSjekkType.TerskelTildeling        = 0
        RabSjekkType.VerdiPaSjekk            = 200
        RabSjekkType.Kommentar               = 'Medlemsrabatt sjekkene tildeles for aktive medlemmer (Har kjøp innen gitt periode). Tildeles normalt hver 6 mnd.'
        .
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setRapportgenParametre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setRapportgenParametre Procedure 
PROCEDURE setRapportgenParametre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT NO-UNDO.
DEF VAR piLoop2 AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.
DEF BUFFER bSysHode   FOR SysHode.


IF NOT CAN-FIND(SysHode WHERE
                SysHode.SysHId = 220) THEN
DO TRANSACTION:
    CREATE bSysHode.
    ASSIGN
        bSysHode.SysHId      = 220
        bSysHode.Beskrivelse = "Oppsett rapportgenerator"
        .
    RELEASE bSysHode.
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 220 AND
    SysGruppe.SysGr  = 1) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 220
        bSysGruppe.SysGr  = 1
        Beskrivelse       = "Visning av felter default"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

/*  */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 220 AND
      syspara.sysgr  = 1 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId      = 220 
          bSysPara.SysGr       = 1 
          bSysPara.ParaNr      = 1
          bSysPara.Parameter1  = "DataObjekt,Beskrivelse,PerLinTxt,AntSolgt,BruttoSolgt,Solgt%,DbKr,Db%,VerdiRabatt,Rab%,GjenkjopAnt,GjenkjopVerdi"
          bSysPara.Beskrivelse = "Standardfelter for STLinje"
          .
      RELEASE bSysPara.
  END.
END.
/*  */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 220 AND
      syspara.sysgr  = 1 AND
      syspara.paranr = 2) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId      = 220 
          bSysPara.SysGr       = 1 
          bSysPara.ParaNr      = 2
          bSysPara.Parameter1  = "Dataobjekt,Beskrivelse,Charbutik,LagAnt,LagerVerdi"
          bSysPara.Beskrivelse = "Standard for lager"
          .
      RELEASE bSysPara.
  END.
END.
/*  */
DO TRANSACTION:
  FIND bsyspara WHERE
      bsyspara.syshid = 220 AND
      bsyspara.sysgr  = 1 AND
      bsyspara.paranr = 3 NO-ERROR.
  IF NOT AVAILABLE bSysPara THEN
  DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId      = 220 
          bSysPara.SysGr       = 1 
          bSysPara.ParaNr      = 3
          .
  END.
  ASSIGN
      bSysPara.Parameter1  = "Dataobjekt,Beskrivelse,PerlinTxt,Bruttosolgt,Bruttosolgt2,Verdisolgt,Verdisolgt2,SolgtDiff%,Db%,Db%2,Rab%,Rab%2"
      bSysPara.Beskrivelse = "Standars for sammenlign"
      .
  RELEASE bSysPara.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSalgsEnhet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSalgsEnhet Procedure 
PROCEDURE setSalgsEnhet :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF VAR piLoop       AS INT NO-UNDO.
DEF VAR piSalgsEnhId AS INT NO-UNDO.

FIND syspara WHERE syshid = 2 AND sysgr = 4 AND paranr = 10 NO-ERROR.
IF AVAILABLE SysPara THEN
  DO piLoop = 1 TO NUM-ENTRIES(SysPara.Parameter1):
    IF NOT CAN-FIND(FIRST SalgsEnhet WHERE
                          SalgsEnhet.SalgsEnhTekst = trim(ENTRY(piLoop,SysPara.Parameter1))) THEN
      DO:
        FIND LAST SalgsEnhet NO-LOCK NO-ERROR.
        IF AVAILABLE SalgsEnhet 
          THEN piSalgsEnhId = SalgsEnhet.SalgsEnhId + 1.
        ELSE piSalgsEnhId = 1.
        CREATE SalgsEnhet.
        ASSIGN
          SalgsEnhet.SalgsEnhId    = piSalgsEnhId
          SalgsEnhet.SalgsEnhTekst = TRIM(ENTRY(piLoop,SysPara.Parameter1)).
      END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSIETransParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSIETransParam Procedure 
PROCEDURE setSIETransParam :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 20) THEN
  DO FOR bSysGruppe:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 20
        bSysGruppe.Beskrivelse = "SIE Transaksjons akkumulering og eksport"
        .
    RELEASE bSysGruppe.
  END. /* bSysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Lagre finansdata som SIE transaksjoner"
          SysPara.Hjelpetekst1 = "0=Nei, 1=Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = "9999"
          Syspara.Beskrivelse  = "Default konto for ikke koblede poster"
          SysPara.Hjelpetekst1 = "0=Nei, 1=Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = "6993"
          Syspara.Beskrivelse  = "Differansekonto SIETrans"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Summeringsnivå i SIE fil"
          SysPara.Hjelpetekst1 = "0=per varegr, 1=per konto"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 5.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Krav om godkjent kassarapport før eksport"
          SysPara.Hjelpetekst1 = "0=Nei, 1=Ja : Eksport til SIE fil gjøres ikke før kassarapporten er godkjent"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 6.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Sjekk EOD før eksport"
          SysPara.Hjelpetekst1 = "0=Nei, 1=Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 7 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 7.
      ASSIGN  
          SysPara.Parameter1   = "c:\home\lindbak\sendes"
          Syspara.Beskrivelse  = "SIE eksportkatalog"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 8 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 8.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Skal 3020 konto skapes"
          SysPara.Hjelpetekst1 = "0=Nei, 1=Ja."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 9 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 9.
      ASSIGN  
          SysPara.Parameter1   = "3010,6993"
          Syspara.Beskrivelse  = "Konto med kostnadssted"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 10 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 10.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Bruk kortnummer eller innløser"
          SysPara.Hjelpetekst1 = "0=Kortnummer, 1=Innløser : Styrer om korrtypen skal bestemmes av kortnr eller via innløser."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 11 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 11.
      ASSIGN  
          SysPara.Parameter1   = ""
          Syspara.Beskrivelse  = "Butikker som ikke leverer EOD"
          SysPara.Hjelpetekst1 = "Kommaseparert liste med butikknr. For disse butikkene simuleres EOD melding."
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  20 AND
    SysPara.ParaNr = 12 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  20 
          SysPara.ParaNr = 12.
      ASSIGN  
          SysPara.Parameter1   = ""
          Syspara.Beskrivelse  = "Salgskonto for frakt"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  IF AVAILABLE SysPara THEN 
    RELEASE SysPara.
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setStartArtikelSokFelt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setStartArtikelSokFelt Procedure 
PROCEDURE setStartArtikelSokFelt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId =  2 AND
        SysPara.SysGr  =  4 AND
        SysPara.ParaNr = 18 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE SysPara.
          ASSIGN
              SysPara.SysHId =  2 
              SysPara.SysGr  =  4 
              SysPara.ParaNr = 18 
              .
          ASSIGN  
              Syspara.Beskrivelse  = "Startfelt for artikkelsøk"
              SysPara.Parameter1   = "0"
              SysPara.Hjelpetekst1 = "0=EAN,1=VareGr,2=Varetekst,3=Lev.art.nr"
              .
          RELEASE SysPara.
        END.

        FIND SysPara EXCLUSIVE-LOCK WHERE
          SysPara.SysHId =  2 AND
          SysPara.SysGr  =  4 AND
          SysPara.ParaNr = 21 NO-ERROR.
        IF NOT AVAILABLE SysPara THEN
          DO:
            CREATE SysPara.
            ASSIGN
                SysPara.SysHId =  2 
                SysPara.SysGr  =  4 
                SysPara.ParaNr = 21 
                .
            ASSIGN  
                Syspara.Beskrivelse  = "Skjul artikkelsøkefelt"
                SysPara.Parameter1   = "1"
                SysPara.Hjelpetekst1 = "0=Alle synlig,1=LevKod og Bestillingsnr skjult"
                .
            RELEASE SysPara.
          END.

  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysPara2av5Interleave) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysPara2av5Interleave Procedure 
PROCEDURE setSysPara2av5Interleave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

/*IF NOT CAN-FIND(FIRST SysPara where
    SysPara.SysHId = 2 and
    SysPara.SysGr  = 7) THEN */
DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 17 TO 17:
        FIND bSysPara WHERE
            bSysPara.SysHId = 2 AND
            bSysPara.SysGr  = 4 AND
            bSysPara.ParaNr = piLoop EXCLUSIVE-LOCK NO-ERROR.
        IF  NOT AVAILABLE bSysPara THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2 
                bSysPara.SysGr       = 4 
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = "0"
                bSysPara.Beskrivelse = "Gen 2av5Interleave i BestNr"
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaABCartikkelrapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaABCartikkelrapport Procedure 
PROCEDURE setSysparaABCartikkelrapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER bSysHode   FOR SysHode.
DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysHode WHERE
            SysHode.SysHId = 224) THEN
        DO FOR bSysHode:
            CREATE bSysHode.
            ASSIGN
                bSysHode.SysHId = 224
                Beskrivelse     = "ABC rapport standardparametre"
                .
            RELEASE bSysHode.
        END. /* bSysHode */
      
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 224 AND
            SysGruppe.SysGr  = 1) THEN
        DO FOR bSysGruppe:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 224
                bSysGruppe.SysGr  = 1
                Beskrivelse       = "ABC artikkelrapport standardparametre"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe */
      
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 224 AND
            SysGruppe.SysGr  = 10) THEN
        DO FOR bSysGruppe:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 224
                bSysGruppe.SysGr  = 10
                Beskrivelse       = "ABC oversettelse"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe */

        IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 224 AND
        syspara.sysgr = 1 AND
        syspara.paranr = 1) THEN 
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 224 
                bSysPara.SysGr       = 1 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Filkatalog"
                bSysPara.Parameter1  = "FilePath"
                bSysPara.Parameter2  = "c:\appdir\se\utskrift\abc"
                .
            RELEASE bSysPara.
        END.
        
        IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 224 AND
        syspara.sysgr = 1 AND
        syspara.paranr = 2) THEN 
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 224 
                bSysPara.SysGr       = 1 
                bSysPara.ParaNr      = 2
                bSysPara.Beskrivelse = "Fil ekstent"
                bSysPara.Parameter1  = "FileExtent"
                bSysPara.Parameter2  = ".xls"
                .
            RELEASE bSysPara.       
        END.

        IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 224 AND
        syspara.sysgr = 1 AND
        syspara.paranr = 4) THEN 
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 224 
                bSysPara.SysGr       = 1 
                bSysPara.ParaNr      = 4
                bSysPara.Beskrivelse = "Vis skjulte felt"
                bSysPara.Parameter1  = "ShowHiddenGUIfields"
                bSysPara.Parameter2  = "yes"
                .
            RELEASE bSysPara.       
        END.

      IF NOT CAN-FIND(SysHode WHERE
          SysHode.SysHId = 225) THEN
      DO FOR bSysHode:
          CREATE bSysHode.
          ASSIGN
          bSysHode.SysHId = 225
          Beskrivelse     = "ABC artikkel rapportoppsett"
          .
          RELEASE bSysHode.
      END. /* bSysHode */

      IF NOT CAN-FIND(SysGruppe WHERE
          SysGruppe.SysHId = 225 AND
          SysGruppe.SysGr  = 1) THEN
      DO FOR bSysGruppe:
          CREATE bSysGruppe.
          ASSIGN
              bSysGruppe.SysHId = 225
              bSysGruppe.SysGr  = 1
              Beskrivelse       = "ABC artikkelrapport"
              .
          RELEASE bSysGruppe.
      END. /* bSysGruppe */
      
      IF NOT CAN-FIND(SysHode WHERE
          SysHode.SysHId = 226) THEN
      DO FOR bSysHode:
          CREATE bSysHode.
          ASSIGN
          bSysHode.SysHId = 226
          Beskrivelse     = "ABC varegruppe rapportoppsett"
          .
          RELEASE bSysHode.
      END. /* bSysHode */
      
    END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaBatch) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaBatch Procedure 
PROCEDURE setSysparaBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 1 AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 200 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 6
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "Regen./korr. av artlag ute av synk."
            bSysPara.Hjelpetekst1 = "0-Ikke kjør, 1-Kjør"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 1 AND
        syspara.paranr = 7) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 200 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 7
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "Ombygging av StLager tabellen."
            bSysPara.Hjelpetekst1 = "0-Ikke kjør, 1-Kjør"
            .
        RELEASE bSysPara.
    END.
  
  END.

  DO TRANSACTION:
    
    /* Intervall och tidpunkt för läsning från kassan: varannan minut från 0700-2359*/
    /*
    FIND syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 2 AND
        syspara.paranr = 2.
    IF INT(syspara.parameter1) = 0 OR INT(syspara.parameter1) >= 10 THEN
        ASSIGN syspara.parameter1 = "10".
    FIND syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 2 AND
        syspara.paranr = 4.
    ASSIGN syspara.parameter1 = "25200,86360".
    RELEASE syspara.
    */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 2 AND
        syspara.paranr = 100) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 200 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 100
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Overføre finans/bongdata til PRO"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 2 AND
        syspara.paranr = 101) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 200 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 101
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Overføre finans/bongdata til Preem"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 2 AND
        syspara.paranr = 102) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 200 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 102
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Flytte salgsdatafiler til Pro"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 2 AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 200 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 6
            bSysPara.Parameter1  = "Ja"
            bSysPara.Beskrivelse = "Lese inn VPI"
            .
        RELEASE bSysPara.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaBehandlingskode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaBehandlingskode Procedure 
PROCEDURE setSysParaBehandlingskode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 2 AND
        syspara.sysgr  = 4  AND
        syspara.paranr = 19) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 2 
            bSysPara.SysGr        = 4 
            bSysPara.ParaNr       = 19
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Sende behandlingskode eller Høy/Lav mva til kasse"
            bSysPara.Hjelpetekst1 = "0-Behandlingskode, 1-Høy/lav mva"
            .
        RELEASE bSysPara.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaBestillingVedNyArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaBestillingVedNyArtikkel Procedure 
PROCEDURE setSysParaBestillingVedNyArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bSysPara FOR SysPara.

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 5 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 100) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 4 
            bSysPara.ParaNr      = 100
            bSysPara.Parameter1  = "0"
            bSysPara.Parameter2  = ""
            bSysPara.Beskrivelse = "Opprett bestilling ved ny artikkel"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 5 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 31) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 4 
            bSysPara.ParaNr      = 31
            bSysPara.Parameter1  = "icon\johsvart.jpg"
            bSysPara.Parameter2  = ""
            bSysPara.Beskrivelse = "Plassering logofil kundeordreutskrift"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 5 AND
        bSysPara.SysGr  = 4 AND
        bSysPara.ParaNr = 32) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 4 
            bSysPara.ParaNr      = 32
            bSysPara.Parameter1  = "Vis strekkode på kundeordre utskrift"
            bSysPara.Parameter2  = "0"
            bSysPara.Beskrivelse = "0-Ingen, 1-Utskrift"
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaDampbageriet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaDampbageriet Procedure 
PROCEDURE setSysparaDampbageriet :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE BUFFER bSysGruppe FOR SysGruppe.
DEFINE BUFFER bSysPara   FOR SysPara.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 210 AND
    SysGruppe.SysGr  = 101) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 210
        bSysGruppe.SysGr  = 101
        Beskrivelse       = "Butikkbakeren"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

/*  */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 210 AND
      syspara.sysgr  = 101 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId      = 210 
          bSysPara.SysGr       = 101 
          bSysPara.ParaNr      = 1
          bSysPara.Beskrivelse = "Siste eksportdato kontantsalg"
          bSysPara.Parameter1  = ""
          .
      RELEASE bSysPara.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaEANHandtering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaEANHandtering Procedure 
PROCEDURE setSysParaEANHandtering :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes: Disse parameterne benyttes av bibl_chkean.p.                                                                                                                                       
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysHode   FOR SysHode.
  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 1 AND
        SysGruppe.SysGr  = 91) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 1
            bSysGruppe.SysGr       = 91
            bSysGruppe.Beskrivelse = "EAN håndtering vekt og pris i kode"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 91 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 91 
            bSysPara.ParaNr       = 1
            bSysPara.Beskrivelse  = 'Konverter EAN med prefiks 20 til 25'
            bSysPara.Parameter1   = '0'
            bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja. Legger inn 0 i pris og vektfelt og 0 som sjekksiffer.'
            bSysPara.Parameter2   = 'Vekt og pris i kode.'
            bSysPara.Hjelpetekst2 = ''
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 91 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 91 
            bSysPara.ParaNr       = 2
            bSysPara.Beskrivelse  = 'Konverter EAN med prefiks 26 til 28'
            bSysPara.Parameter1   = '0'
            bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja. Legger inn 0 i pris og vektfelt og 0 som sjekksiffer.'
            bSysPara.Parameter2   = 'Varegruppe/plu artikler med pris i kode.'
            bSysPara.Hjelpetekst2 = ''
            .
        RELEASE bSysPara.
    END.    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 91 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 91 
            bSysPara.ParaNr       = 3
            bSysPara.Beskrivelse  = 'Konverter EAN med prefiks 7388 (Tidninger Sverige)'
            bSysPara.Parameter1   = '0'
            bSysPara.Hjelpetekst1 = '0-Nei, 1-Ja. Legger inn 0 i prisfelt og 0 som sjekksiffer.'
            bSysPara.Parameter2   = 'Tidningsartikler med pris i kode (Sverige).'
            bSysPara.Hjelpetekst2 = ''
            .
        RELEASE bSysPara.
    END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaEANNrSerie) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaEANNrSerie Procedure 
PROCEDURE setSysParaEANNrSerie :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysHode   FOR SysHode.
  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 1 AND
        SysGruppe.SysGr  = 90) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 1
            bSysGruppe.SysGr       = 90
            bSysGruppe.Beskrivelse = "EAN nummerserier"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 90 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 90 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "EAN-8|8|EAN-13|13"
            bSysPara.Parameter2   = "13"
            bSysPara.Beskrivelse  = "EAN typer (EAN-8,13)"
            bSysPara.Hjelpetekst1 = "Legg liste (8,13,14) med tilgjengelige EAN typer"
            bSysPara.Hjelpetekst2 = "Parameter 2 inneholder default verdi."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 90 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 90 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "4|4|5|5|7|7"
            bSysPara.Parameter2   = "5"
            bSysPara.Beskrivelse  = "Ant. siffer i EAN lev.nr (EAN 13)"
            bSysPara.Hjelpetekst1 = "Liste med ulike lengder (4|4|5|5|7|7). Separator = |."
            bSysPara.Hjelpetekst2 = "Parameter 2 inneholder default verdi. Normalt 5."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 90 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 90 
            bSysPara.ParaNr       = 3
            bSysPara.Parameter1   = "02 Bedriftsinterne|02|70 Norsk|70|73 Svensk|73"
            bSysPara.Parameter2   = "02"
            bSysPara.Beskrivelse  = "Landkoder"
            bSysPara.Hjelpetekst1 = "Liste med tilatte landkoder nummer og tekst. | som separator."
            bSysPara.Hjelpetekst2 = "Paramter2 ineholder defaultverdi. Normalt 02."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 90 AND
        syspara.paranr = 4) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 90 
            bSysPara.ParaNr       = 4
            bSysPara.Parameter1   = "0|0|4|4"
            bSysPara.Parameter2   = "4"
            bSysPara.Beskrivelse  = "Antall siffer i levnr (EAN 8)"
            bSysPara.Hjelpetekst1 = "Liste med ulike lengder (0|0|4|4). Separator = |."
            bSysPara.Hjelpetekst2 = "Parameter 2 inneholder default verdi. Normalt 4."
            .
        RELEASE bSysPara.
    END.
  END. /* TRANSACTION */

  IF NOT CAN-FIND(FIRST EANNrSerie) THEN
  DO:
      DO TRANSACTION:
          CREATE EANNrSerie.
          ASSIGN
              EANNrSerie.EANSerieId       = 1
              EANNrSerie.EANBeskrivelse   = "Stardard bedriftsintern EANserie"
              EANNrSerie.EANType          = 13
              EANNrSerie.EANLandKode      = 02
              EANNrSerie.AntSifferiLevNr  = 5
              EANNrSerie.EANLevNr         = iCL
              EANNrSerie.FraEANArtikkelNr = 0
              EANNrSerie.TilEANArtikkelNr = 99999
              .
          FIND CURRENT EANNrSerie NO-LOCK.
      END.
      /*IF DYNAMIC-FUNCTION("runproc","eannrserie_generer.p",string(EANNrSerie.EANSerieId),?) THEN.*/ /* Ingenting :) */
      DEF VARIABLE ocReturn    AS CHAR NO-UNDO.
      DEF VARIABLE obOK        AS LOG NO-UNDO.
      DEFINE VARIABLE ihBuffer AS HANDLE NO-UNDO.
      iHBuffer = ?.
      
      RUN eannrserie_generer.p (STRING(EANNrSerie.EANSerieId),iHBuffer,'', OUTPUT ocReturn, OUTPUT obOk).
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaEksportImportKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaEksportImportKasse Procedure 
PROCEDURE setSysparaEksportImportKasse :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 51) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 51
            bSysPara.Beskrivelse = "Eksport katalog"
            bSysPara.Parameter1  = "c:\home\lindbak\sendes"
            bSysPara.Parameter2  = "".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 52) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 52
            bSysPara.Beskrivelse = "Import katalog"
            bSysPara.Parameter1  = "c:\home\lindbak\ankommet"
            bSysPara.Parameter2  = "".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 56) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 56
            bSysPara.Beskrivelse = "Eksport katalog kasse"
            bSysPara.Parameter1  = "c:\home\lindbak\kasse"
            bSysPara.Parameter2  = "".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 58) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 58
            bSysPara.Beskrivelse = "Eksport katalog VPI"
            bSysPara.Parameter1  = "c:\home\lindbak\sendes"
            bSysPara.Parameter2  = "".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 59) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 59
            bSysPara.Beskrivelse = "Katalog for loggfiler"
            bSysPara.Parameter1  = ".\log"
            bSysPara.Parameter2  = "".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 60) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 60
            bSysPara.Beskrivelse = "Katalog for dump av db"
            bSysPara.Parameter1  = "c:\home\lindbak\dump"
            bSysPara.Parameter2  = "".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
          syspara.syshid = 1 AND
          syspara.sysgr =  1 AND
          syspara.paranr = 61) THEN 
      DO:
          CREATE bSysPara.
          ASSIGN  
              bSysPara.SysHId      = 1 
              bSysPara.SysGr       = 1 
              bSysPara.ParaNr      = 61
              bSysPara.Beskrivelse = "Eksport av eksterne data"
              bSysPara.Parameter1  = ""
              bSysPara.Parameter2  = "".
          .
          RELEASE bSysPara.
      END.

      IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr =  1 AND
        syspara.paranr = 64) THEN 
      DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 64
            bSysPara.Beskrivelse = "Eksport av lagerteller til PRS POS"
            bSysPara.Parameter1  = "1"
            bSysPara.Parameter2  = "0-Nei, 1-Ja".
            .
        RELEASE bSysPara.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaEksportPricat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaEksportPricat Procedure
PROCEDURE setSysParaEksportPricat:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF BUFFER bSysHode   FOR SysHode.
    DEF BUFFER bSysPara   FOR SysPara.
    DEF BUFFER bSysGruppe FOR SysGruppe.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 50 AND
            SysGruppe.SysGr  = 29) THEN
        DO FOR bSysGruppe:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 50
                bSysGruppe.SysGr  = 29
                Beskrivelse       = "Oppsett mapping Pricat export"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe */
      
        IF NOT CAN-FIND(syspara WHERE
            syspara.syshid = 50 AND
            syspara.sysgr = 29 AND
            syspara.paranr = 1) THEN 
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 50 
                bSysPara.SysGr        = 29 
                bSysPara.ParaNr       = 1
                bSysPara.Beskrivelse  = ""
                bSysPara.Parameter1   = "EDB-System"
                bSysPara.Parameter2   = ""
                bSysPara.Hjelpetekst1 = "Når EDB system er angitt, aktiveres mapping ved eksport."
                .
            RELEASE bSysPara.
        END.
    END. /* TRANSACTION */


END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setSysParaEtikett82) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaEtikett82 Procedure 
PROCEDURE setSysParaEtikett82 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 20 AND
        syspara.paranr = 82) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 20 
            bSysPara.ParaNr      = 82
            bSysPara.Beskrivelse = "Ark 3*11"
            bSysPara.Parameter1  = "Dummy"
            bSysPara.Parameter2  = "HH_Ark_33".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 21 AND
        syspara.paranr = 82) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 21 
            bSysPara.ParaNr      = 82
            bSysPara.Beskrivelse = "Utskrift HH_Ark"
            bSysPara.Parameter1  = "startetikett"
            bSysPara.Parameter2  = "XAVAIL;XSTART".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 20 AND
        syspara.paranr = 83) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 20 
            bSysPara.ParaNr      = 83
            bSysPara.Beskrivelse = "Ark 3*11_m2"
            bSysPara.Parameter1  = "Dummy"
            bSysPara.Parameter2  = "HH_Ark_332".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 21 AND
        syspara.paranr = 83) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 21 
            bSysPara.ParaNr      = 83
            bSysPara.Beskrivelse = "Utskrift HH_Ark_2"
            bSysPara.Parameter1  = "startetikett"
            bSysPara.Parameter2  = "XAVAIL;XSTART".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 20 AND
        syspara.paranr = 84) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 20 
            bSysPara.ParaNr      = 84
            bSysPara.Beskrivelse = "Etikett EAN SKO 1"
            bSysPara.Parameter1  = "Dummy"
            bSysPara.Parameter2  = "SKO-1".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 21 AND
        syspara.paranr = 84) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 21 
            bSysPara.ParaNr      = 84
            bSysPara.Beskrivelse = "Utskrift EAN SKO-1"
            bSysPara.Parameter1  = "startetikett"
            bSysPara.Parameter2  = "XAVAIL;XSTART".
            .
        RELEASE bSysPara.
    END.
    
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 5 AND
        SysGruppe.SysGr  = 28) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 5
            bSysGruppe.SysGr  = 28
            Beskrivelse      = "Etikettvisning av EAN-kod"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 28 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 28 
            bSysPara.ParaNr      = 1
            bSysPara.Beskrivelse = "Om streckkod i Etikett_TIME_2_11"
            bSysPara.Parameter1  = "yes"
            bSysPara.Hjelpetekst1  = "Default yes. Om no skrivs inte streckkod ut".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 28 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 28 
            bSysPara.ParaNr      = 2
            bSysPara.Beskrivelse = "Om streckkod i Etikett_TIME_3_5"
            bSysPara.Parameter1  = "yes"
            bSysPara.Hjelpetekst1  = "Default yes. Om no skrivs beställningsnummer ut som streckkod enligt Dobson2oF5".
            .
        RELEASE bSysPara.
    END.
    
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 5 AND
        SysGruppe.SysGr  = 29) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 5
            bSysGruppe.SysGr  = 29
            Beskrivelse      = "Butikk oppkoblingsparametre for RFID"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 5 AND
        SysGruppe.SysGr  = 30) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 5
            bSysGruppe.SysGr  = 30
            Beskrivelse      = "Katalogliste RFID etikettfiler"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 30 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 30 
            bSysPara.ParaNr      = 1
            bSysPara.Beskrivelse = "Std. katalog for klient"
            bSysPara.Parameter1  = "filer\"
            bSysPara.Hjelpetekst1  = "Avslutt katalognavn med '\'".
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 30 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 30 
            bSysPara.ParaNr      = 2
            bSysPara.Beskrivelse = "Std. katalog for AppServer"
            bSysPara.Parameter1  = "C:\OpenEdge\WRK\filer\"
            bSysPara.Hjelpetekst1  = "Avslutt katalognavn med '\'".
            .
        RELEASE bSysPara.
    END.
    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaExcelEkstent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaExcelEkstent Procedure 
PROCEDURE setSysparaExcelEkstent :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE BUFFER bSysPara FOR SysPara.
DO TRANSACTION:
    /*
    FIND FIRST bSysPara WHERE
            bSysPara.SysHId = 1 AND
            bSysPara.SysGr  = 4 AND
            bSysPara.ParaNr = 1 EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bSysPara THEN
    DO:
        ASSIGN  
            bSysPara.Parameter1  = 'csv'
            .
        RELEASE bSysPara.
    END.
    FIND FIRST bSysPara WHERE
            bSysPara.SysHId = 1 AND
            bSysPara.SysGr  = 1 AND
            bSysPara.ParaNr = 4 EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE bSysPara THEN
    DO:
        ASSIGN  
            bSysPara.Parameter1  = 'csv'
            .
        RELEASE bSysPara.
    END. /* bSysPara TRANSACTION*/
    */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setSysparaGant) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaGant Procedure
PROCEDURE setSysparaGant:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  
  DEFINE VARIABLE cEDB-System AS CHARACTER INITIAL 'Gant Global' NO-UNDO.
  DEFINE VARIABLE cButLst     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cEkstNr     AS CHARACTER NO-UNDO.
  
  ASSIGN
    cButLst = '10000,70001,10013,7000,14012,95000'
    cEkstNr = '2,2,2,2,2,2'
    cButLst = cButLst + ',' + '10004,70002,7004,14041,95004'
    cEkstNr = cEkstNr + ',' + '3,3,3,3,3'

    cButLst = cButLst + ',' + '70006,7005,14131'
    cEkstNr = cEkstNr + ',' + '4,4,4'

    cButLst = cButLst + ',' + '10020,14023,95021'
    cEkstNr = cEkstNr + ',' + '5,5,5'

    cButLst = cButLst + ',' + '10025,14000,70007,7007,95025'
    cEkstNr = cEkstNr + ',' + '6,6,6,6,6'

    cButLst = cButLst + ',' + '70003,10008,14112,95008'
    cEkstNr = cEkstNr + ',' + '8,8,8,8'

    cButLst = cButLst + ',' + '10018,70005,14114,95018,12179'
    cEkstNr = cEkstNr + ',' + '9,9,9,9,9'

    cButLst = cButLst + ',' + '50000'
    cEkstNr = cEkstNr + ',' + '10'

    cButLst = cButLst + ',' + '10001,70000,14034'
    cEkstNr = cEkstNr + ',' + '11,11,11'

    cButLst = cButLst + ',' + '10003,70004'
    cEkstNr = cEkstNr + ',' + '14,14'

    cButLst = cButLst + ',' + '55000,55001,55002,55003'
    cEkstNr = cEkstNr + ',' + '15,15,15,15'
    .

  DO piLoop = 1 TO NUM-ENTRIES(cButLst):
    RUN opprettMapping (cEDB-System,'Butiker',ENTRY(piLoop,cEkstNr),ENTRY(piLoop,cButLst)).
  END.
  
  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 210 AND
        SysPara.SysGr  = 100 AND
        SysPara.ParaNr = 4 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId = 210 
              bSysPara.SysGr  = 100
              bSysPara.ParaNr = 4 
              .
          ASSIGN  
              bSyspara.Beskrivelse  = "Rabatt% outlet"
              bSysPara.Parameter1   = "40"
              bSysPara.Hjelpetekst1 = "Rabatt% på pakksedler fra overføringer"
              .
          RELEASE bSysPara.
        END.
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 210 AND
        SysPara.SysGr  = 100 AND
        SysPara.ParaNr = 5 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId = 210 
              bSysPara.SysGr  = 100
              bSysPara.ParaNr = 5 
              .
          ASSIGN  
              bSyspara.Beskrivelse  = "Artikler som ikke skal importeres"
              bSysPara.Parameter1   = ""
              bSysPara.Hjelpetekst1 = "Komma separert liste med Lev.artnr"
              .
          RELEASE bSysPara.
        END.
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 210 AND
        SysPara.SysGr  = 100 AND
        SysPara.ParaNr = 8 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId = 210 
              bSysPara.SysGr  = 100
              bSysPara.ParaNr = 8 
              .
          ASSIGN  
              bSyspara.Beskrivelse  = "Gant Aktiv"
              bSysPara.Parameter1   = "0"
              bSysPara.Hjelpetekst1 = "0-Ikke aktiv, 1=Aktiv"
              .
          RELEASE bSysPara.
        END.
  END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setSysParaHTutlegg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaHTutlegg Procedure 
PROCEDURE setSysParaHTutlegg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 200 AND
        SysGruppe.SysGr  = 6) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 200
            bSysGruppe.SysGr  = 6
            Beskrivelse      = "Utlegg HT-fil"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */


    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 200 AND
        syspara.sysgr = 6 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 200 
            bSysPara.SysGr       = 6 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = "0"
            bSysPara.Parameter2  = "75600"
            bSysPara.Beskrivelse = "Automatiskt utlegg HTfil"
            bSysPara.Hjelpetekst1 = "1-Ja."
            bSysPara.Hjelpetekst2 = "Tid som skal passeres for at utlegg skal skje (75600=21:00)"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara EXCLUSIVE-LOCK WHERE
          bsyspara.syshid = 200 AND
          bsyspara.sysgr = 6 AND
          bsyspara.paranr = 1 NO-ERROR.
      IF AVAILABLE bSysPara THEN 
        bSysPara.Parameter1 = '0'.    
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaKreditkortNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaKreditkortNavn Procedure 
PROCEDURE setSysParaKreditkortNavn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Oppsett BBS BankKort tabell med kodetyper. */
  RUN setSysParaKreditkortNavn.p.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaLeverandor) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaLeverandor Procedure 
PROCEDURE setSysParaLeverandor :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 16 AND
        syspara.sysgr  = 2  AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
          ASSIGN  
            bSysPara.SysHId       = 16 
            bSysPara.SysGr        = 2
            bSysPara.ParaNr       = 3
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "Tillatt nyregistrering av leverandører"
            bSysPara.Hjelpetekst1 = "0-Sperret, 1-Tillatt"
            .
        RELEASE bSysPara.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaMedlem Procedure 
PROCEDURE setSysParaMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 5
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Automatisk eller manuelt tildelt kortnr"
            bSysPara.Hjelpetekst1 = "0-Automatisk kortnr, 1-Manuelt kortnr, 2-PersonNr via SPAR"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
        FIND syspara EXCLUSIVE-LOCK WHERE
             syspara.syshid = 14 AND
             syspara.sysgr  = 1  AND
             syspara.paranr = 5.
        ASSIGN
            SysPara.Beskrivelse  = "Automatisk eller manuelt tildelt kortnr"
            SysPara.Hjelpetekst1 = "0-Automatisk kortnr, 1-Manuelt kortnr, 2-PersonNr via SPAR"
        .  
        
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 6
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Atomatisk opprett kundekort"
            bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 8) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 8
            bSysPara.Parameter1   = "c:\appdir\se\SPAR\PS_SPARpersonsokningfraga.xsd"
            bSysPara.Parameter2   = "https://kt-ext-ws.statenspersonadressregister.se/spar-webservice/SPARPersonsokningService/"
            bSysPara.Beskrivelse  = "SPAR integrasjon"
            bSysPara.Hjelpetekst1 = "Para1: <c:\appdir\se\SPAR\PS_SPARpersonsokningfraga.xsd> Para2: <https://kt-ext-ws.statenspersonadressregister.se/spar-webservice/SPARPersonsokningService/> "
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 9) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 9
            bSysPara.Parameter1   = "16"
            bSysPara.Beskrivelse  = "Minstealder medlemsklubb"
            bSysPara.Hjelpetekst1 = "Angis som antall år."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 20) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 20
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Default medlemsklubb"
            bSysPara.Hjelpetekst1 = "0-Ingen, <> fra 0 = Default medlemsklubb"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 25) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 25
            bSysPara.Parameter1   = "4,5,6,10"
            bSysPara.Beskrivelse  = "Lengde på Medlems ID"
            bSysPara.Hjelpetekst1 = "Kommaseperte tall som angir tillatte lengder"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 31) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 31
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb1"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 32) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 32
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb2"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 33) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 33
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb3"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 34) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 34
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb4"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 35) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 35
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb5"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 36) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 36
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb6"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 37) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 37
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb7"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 38) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 38
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb8"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 39) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 39
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikkliste alt. medlemsklubb9"
            bSysPara.Hjelpetekst1 = "Kommaseparert butikknrliste"
            bSysPara.Hjelpetekst2 = "Medlemsklubb nr."
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 2  AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 2 
            bSysPara.ParaNr       = 5
            bSysPara.Parameter1   = "400499"
            bSysPara.Beskrivelse  = "Siste brukte kundekortnr"
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 14 AND
        syspara.sysgr  = 2  AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 14 
            bSysPara.SysGr        = 2 
            bSysPara.ParaNr       = 6
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Eksport av alle eller spesifikke kunder til butikk"
            bSysPara.Hjelpetekst1 = "0-Alle, 1-SPesifikk kunde til butikk"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 31) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 31
            bSysPara.Parameter1   = "-H localhost -AppService asbroker1"
            bSysPara.Parameter2   = "c:\appdir\se\SPAR\PS_SPARpersonsokningfraga.xsd"
            bSysPara.Beskrivelse  = "Oppkoblingsparametre AppServer"
            bSysPara.Hjelpetekst1 = "Standard: -H localhost -AppService asbroker1"
            .
        RELEASE bSysPara.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaMeny) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaMeny Procedure 
PROCEDURE setSysParaMeny :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysHode   FOR SysHode.
  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysHode WHERE
                    SysHode.SysHId = 300) THEN
    DO:
      CREATE bSysHode.
      ASSIGN
          bSysHode.SysHId      = 300
          bSysHode.Beskrivelse = "Oppsett av meny"
          .
      RELEASE bSysHode.
    END.

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 300 AND
        SysGruppe.SysGr  = 1) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 300
            bSysGruppe.SysGr       = 1
            bSysGruppe.Beskrivelse = "Varslingsmeny1"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 300 AND
        SysGruppe.SysGr  = 2) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 300
            bSysGruppe.SysGr       = 2
            bSysGruppe.Beskrivelse = "Varslingsmeny2 (Flere kan legges opp)"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 300 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 300 
            bSysPara.SysGr        = 1
            bSysPara.ParaNr       = 1
            bSysPara.Beskrivelse  = "Ordreforslag"
            bSysPara.Parameter1   = "pllisteordre.w"
            bSysPara.Parameter2   = "alarm_plliste.p"
            bSysPara.Hjelpetekst1 = "Navn på GUI program som skal startes"
            bSysPara.Hjelpetekst2 = "Navn på kontrollprogram"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 300 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 300 
            bSysPara.SysGr        = 1
            bSysPara.ParaNr       = 2
            bSysPara.Beskrivelse  = "Pakkseddel"
            bSysPara.Parameter1   = "pksdlhode.w"
            bSysPara.Parameter2   = "alarm_pksdl.p"
            bSysPara.Hjelpetekst1 = "Navn på GUI program som skal startes"
            bSysPara.Hjelpetekst2 = "Navn på kontrollprogram"
            .
        RELEASE bSysPara.
    END.
  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaMottakskontroll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaMottakskontroll Procedure 
PROCEDURE setSysparaMottakskontroll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF BUFFER bSysPara FOR SysPara.

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 1 AND
        bSysPara.SysGr  = 1 AND
        bSysPara.ParaNr = 200) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 1 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 200
            bSysPara.Parameter1  = "Navn på systemansvarlig"
            bSysPara.Parameter2  = "e-mail@adresse.no"
            bSysPara.Beskrivelse = "Systemansvarlig"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 1 AND
        bSysPara.SysGr  = 1 AND
        bSysPara.ParaNr = 201) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 201
            bSysPara.Parameter1   = "1"
            bSysPara.Hjelpetekst1 = "0=Ingen rapport, 1= Rapport skal sendes, 2=Rapport skal bare sendes når det er feil"
            bSysPara.Beskrivelse  = "Frekvens statusrapport"
            .
        RELEASE bSysPara.
    END.

    /* Statuser filmottak */
    FIND bSysPara WHERE
        bSysPara.SysHId = 1 AND
        bSysPara.SysGr  = 11 AND
        bSysPara.ParaNr = 4 NO-ERROR.
    IF AVAILABLE bSysPAra THEN
    DO:
        IF NUM-ENTRIES(bSysPara.Parameter1,",") < 15 THEN
        ASSIGN  
            bSysPara.Parameter1  = "Melding,0,Feil,1,Alvorlig feil,2,Graverende feil,3,Oppdater,4,Oppdaterer,5,Start logging,9,Slutt logging,8"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 1 AND
        bSysPara.SysGr  = 1 AND
        bSysPara.ParaNr = 210) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 210
            bSysPara.Parameter1   = ""
            bSysPara.Hjelpetekst1 = "<Blank>-Excel eksport benyttes,<Path lagt inn> OpenOffice benyttes."
            bSysPara.Beskrivelse  = "OpenOffice Path to Exec "
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaNets_MainCard) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaNets_MainCard Procedure
PROCEDURE setSysParaNets_MainCard:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF BUFFER bSysHode   FOR SysHode.
    DEF BUFFER bSysPara   FOR SysPara.
    DEF BUFFER bSysGruppe FOR SysGruppe.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 50 AND
            SysGruppe.SysGr  = 28) THEN
        DO FOR bSysGruppe:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 50
                bSysGruppe.SysGr  = 28
                Beskrivelse       = "Oppsett MainCard/Nets integrasjon"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe */
      
        IF NOT CAN-FIND(syspara WHERE
            syspara.syshid = 50 AND
            syspara.sysgr = 28 AND
            syspara.paranr = 1) THEN 
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 50 
                bSysPara.SysGr        = 28 
                bSysPara.ParaNr       = 1
                bSysPara.Beskrivelse  = "Logg til NettsLogg"
                bSysPara.Parameter1   = "0"
                bSysPara.Parameter2   = ""
                bSysPara.Hjelpetekst1 = "0-Ikke aktiv, 1-Aktiv."
                .
            RELEASE bSysPara.
        END.
    END. /* TRANSACTION */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-setSysParaNettbutikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaNettbutikk Procedure 
PROCEDURE setSysParaNettbutikk :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEF BUFFER bSysHode   FOR SysHode.
DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysHode WHERE
      SysHode.SysHId = 150) THEN
    DO FOR bSysHode:
      CREATE bSysHode.
      ASSIGN
        bSysHode.SysHId = 150
        Beskrivelse     = "Nettbutikk parametre"
        .
      RELEASE bSysHode.
    END. /* bSysHode */
      
    IF NOT CAN-FIND(SysGruppe WHERE
      SysGruppe.SysHId = 150 AND
      SysGruppe.SysGr  = 1) THEN
    DO FOR bSysGruppe:
      CREATE bSysGruppe.
      ASSIGN
        bSysGruppe.SysHId = 150
        bSysGruppe.SysGr  = 1
        Beskrivelse       = "Nettbutikk standardparametre"
        .
      RELEASE bSysGruppe.
    END. /* bSysGruppe */
      
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 1) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 1
        bSysPara.Beskrivelse  = "Nettbutikk aktiv"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Ikke aktiv, 1-Aktiv."
        .
      RELEASE bSysPara.
    END.
        
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 2) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 2
        bSysPara.Beskrivelse  = "Butikknr. nettbutikk"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Nettbutikk ikke aktiv, <> 0 for aktiv nettbutikk."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 3) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 3
        bSysPara.Beskrivelse  = "Primærlager Nettbutikk(butikknr)"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Ingen primærlager for nettbutikk, <> 0 Primærlager for nettbutikk."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 4) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 4
        bSysPara.Beskrivelse  = "Plukklagre Nettbutikk(butikknrliste)"
        bSysPara.Parameter1   = ""
        bSysPara.Parameter2   = "0"
        bSysPara.Hjelpetekst1 = "-Blank>-Alle butikker/lagre, <> '' Kommaseparert liste med lagre."
        bSysPara.Hjelpetekst2 = "0-Ikke overfør, 1-Overfør fra plukk butikk."
        .
      RELEASE bSysPara.       
    END.
    
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 5) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 5
        bSysPara.Beskrivelse  = "Send reservasjonsmelding (eMail)"
        bSysPara.Parameter1   = "1"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Ingen melding,1-Send melding."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 6) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 6
        bSysPara.Beskrivelse  = "Default lager"
        bSysPara.Parameter1   = "181"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "Butikknr som skal ha ordren hvis ingen andre lager finnes."
        .
      RELEASE bSysPara.       
    END.
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 7) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 7
        bSysPara.Beskrivelse  = "Publiserflagg via lagerteller"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja. Nettbutikken skal ha -1000 som lagerantall når art. ikke skal publiseres."
        .
      RELEASE bSysPara.       
    END.
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 8) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 8
        bSysPara.Beskrivelse  = "Opprette faktura for nettbutikkordre"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja. 0-Ingen faktura opprettes. 1-Det opprettes faktura på alle nettbutikkordre"
        .
      RELEASE bSysPara.       
    END.
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 9) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 9
        bSysPara.Beskrivelse  = "Legg ut akkumulert lager på nettbutikk"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja. Hvis ja, summeres lager for alle butikker og legges ut som lager i butikk 10."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 10) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 10
        bSysPara.Beskrivelse  = "Artikler fra disse leverandører skal trekkes fra angitt lager"
        bSysPara.Parameter1   = ""
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "Parameter1=Komma separart liste over leverandører. Parameter2=ButikkNr det skal trekkes fra"
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 11) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 11
        bSysPara.Beskrivelse  = "Kopier bilder til nettbutikk"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = "0-Nei,1-Ja"
        bSysPara.Hjelpetekst1 = "Styrer om bilder skal kopieres fra bildekatalog over til nettbutikk via sendes katalogen."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 12) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 12
        bSysPara.Beskrivelse  = "Standard kundegruppe og type"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = "0"
        bSysPara.Hjelpetekst1 = "Legg inn gruppe i parameter 1 og type i parameter 2."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 13) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 13
        bSysPara.Beskrivelse  = "Standard medlemsgruppe og type"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = "0"
        bSysPara.Hjelpetekst1 = "Legg inn gruppe i parameter 1 og type i parameter 2."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 14) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 14
        bSysPara.Beskrivelse  = "Standard medlemsklubb"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "Legg inn medlemsklubbid som skal være standard."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 15) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 15
        bSysPara.Beskrivelse  = "Kundeid på ordre - sjekk også kundekortnr"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Bare sjekk eksternt id, 1-Sjekk også kunde kortnr"
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 16) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 16
        bSysPara.Beskrivelse  = "Svensk format på postnr"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Ingen endring, 1-Svensk format XXX XX."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 17) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 17
        bSysPara.Beskrivelse  = "Ekstra Lagre som sendes til Nettbutikk"
        bSysPara.Parameter1   = ""
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "-Blank>-Alle butikker/lagre, <> '' Kommaseparert liste med lagre."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 18) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 18
        bSysPara.Beskrivelse  = "Aktiver artikkel i nettbutikk ved varemottak"
        bSysPara.Parameter1   = ""
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
        .
      RELEASE bSysPara.       
    END.
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 20) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 20
        bSysPara.Beskrivelse  = "Nettbutikkk type"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Ingen,1-Phx,2-WooCom"
        bSysPara.Hjelpetekst2 = "Butikliste butikker som tilhører typen hvis nødvendig."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 21) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 21
        bSysPara.Beskrivelse  = "Skrive pakkseddel ved utlevering?"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Ingen,1-Ja"
        bSysPara.Hjelpetekst2 = "Om pakkseddel skal skrives ut ved utlevering av ordre til kunde."
        .
      RELEASE bSysPara.       
    END.

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 1 AND
      syspara.paranr = 25) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 25
        bSysPara.Beskrivelse  = "Flagge manko i kundeordre?"
        bSysPara.Parameter1   = "0"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
        bSysPara.Hjelpetekst2 = "Flagger kundeordre som ikke kan leveres pga manko."
        .
      RELEASE bSysPara.       
    END.

    /* Opphav */
    IF NOT CAN-FIND(SysGruppe WHERE
      SysGruppe.SysHId = 150 AND
      SysGruppe.SysGr  = 2) THEN
    DO FOR bSysGruppe:
      CREATE bSysGruppe.
      ASSIGN
        bSysGruppe.SysHId = 150
        bSysGruppe.SysGr  = 2
        Beskrivelse       = "Kundeordre opphav"
        .
      RELEASE bSysGruppe.
    END. /* bSysGruppe */
      
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 2 AND
      syspara.paranr = 1) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 2 
        bSysPara.ParaNr       = 1
        bSysPara.Beskrivelse  = "Manuelt reg."
        bSysPara.Parameter1   = "Manuelt reg."
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = ""
        .
      RELEASE bSysPara.
    END.
    
    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 2 AND
      syspara.paranr = 10) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 2 
        bSysPara.ParaNr       = 10
        bSysPara.Beskrivelse  = "Nettbutikk"
        bSysPara.Parameter1   = "Nettbutikk"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = ""
        .
      RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(SysGruppe WHERE
      SysGruppe.SysHId = 150 AND
      SysGruppe.SysGr  = 10) THEN
    DO FOR bSysGruppe:
      CREATE bSysGruppe.
      ASSIGN
        bSysGruppe.SysHId = 150
        bSysGruppe.SysGr  = 10
        Beskrivelse       = "Nettbutikk fraktartikler"
        .
      RELEASE bSysGruppe.
    END. /* bSysGruppe */

    IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 150 AND
      syspara.sysgr = 10 AND
      syspara.paranr = 1) THEN 
    DO:
      CREATE bSysPara.
      ASSIGN  
        bSysPara.SysHId       = 150 
        bSysPara.SysGr        = 10 
        bSysPara.ParaNr       = 1
        bSysPara.Beskrivelse  = "Posten"
        bSysPara.Parameter1   = "2000553"
        bSysPara.Parameter2   = ""
        bSysPara.Hjelpetekst1 = "Artikkelnr for frakt"
        .
      RELEASE bSysPara.
    END.
    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaAnalyser) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaAnalyser Procedure 
PROCEDURE setSysParaAnalyser :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.             
               
  DO TRANSACTION:
    /* Opphav */
    IF NOT CAN-FIND(SysGruppe WHERE
      SysGruppe.SysHId = 6 AND
      SysGruppe.SysGr  = 11) THEN
    DO FOR bSysGruppe:
      CREATE bSysGruppe.
      ASSIGN
        bSysGruppe.SysHId = 6
        bSysGruppe.SysGr  = 11
        Beskrivelse       = "Analyser"
        .
      RELEASE bSysGruppe.
    END. /* bSysGruppe */
    /* Denne status benyttes på HK mens pakkseddelen importeres fra ERP */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 6 AND
        syspara.sysgr  = 11 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 6 
            bSysPara.SysGr        = 11 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = ""
            bSysPara.Beskrivelse  = "Skjul faner"
            bSysPara.Hjelpetekst1 = "Legg inn kommaseparert liste med de faner (Fanenr) som skal skjules"
            .
        RELEASE bSysPara.
    END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaOn-LineKassarapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaOn-LineKassarapport Procedure 
PROCEDURE setSysParaOn-LineKassarapport :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.             
               
  DO TRANSACTION:
    /* Opphav */
    IF NOT CAN-FIND(SysGruppe WHERE
      SysGruppe.SysHId = 6 AND
      SysGruppe.SysGr  = 10) THEN
    DO FOR bSysGruppe:
      CREATE bSysGruppe.
      ASSIGN
        bSysGruppe.SysHId = 6
        bSysGruppe.SysGr  = 10
        Beskrivelse       = "ON-Line kassarapport"
        .
      RELEASE bSysGruppe.
    END. /* bSysGruppe */
    
    /* Vis ingen eller alle butikker i butikkfane */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 6 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 6 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Vis alle butikker i butikkfane"
            bSysPara.Hjelpetekst1 = "0-Tilg.kontroll, 1-Vis alle"
            .
        RELEASE bSysPara.
    END.

    /* Visning av faner i budsjettvisning i kasse. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 6 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 6 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "0,0,1,0,1,1,1,1,1,1"
            bSysPara.Beskrivelse  = "Visning av faner i budsjettvisning"
            bSysPara.Hjelpetekst1 = "Default verdi er: 0,0,1,0,1,1,1,1,1,1"
            .
        RELEASE bSysPara.
    END.
    
    /* Overstyring av vinsing av butikker i budjsettvisning. */
    /* Er parametrene blanke, brukes oppsett styrt av bruker */
    /* og brukergruppe, samt teamtype = 2.                  */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 6 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 6 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 3
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Tilgjengelige butikker"
            bSysPara.Hjelpetekst1 = "Liste over tilgjegnelige butikker for visning."
            .
        RELEASE bSysPara.
    END.
    /* Overstyring av vinsing av butikker i budjsettvisning. */
    /* Her legges inn butikknr. på akkumuleringsbutikk.      */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 6 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 6 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 5
            bSysPara.Parameter1   = ""
            bSysPara.Parameter2   = ""
            bSysPara.Beskrivelse  = "Butikknr. for akkumulering"
            bSysPara.Hjelpetekst1 = "Bruker parameter 1 og 2. Er parameter blank, vises ikke sumlinje for tilhørende gruppe."
            .
        RELEASE bSysPara.
    END.
  END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaOverforing) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaOverforing Procedure 
PROCEDURE setSysParaOverforing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:


    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 11 AND
        SysGruppe.SysGr  = 5) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 11
            bSysGruppe.SysGr  = 5
            Beskrivelse      = "Plukklistebehandling PDA"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /* Denne status benyttes på HK mens pakkseddelen importeres fra ERP */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 11 AND
        syspara.sysgr  = 5 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 11 
            bSysPara.SysGr        = 5 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Innhold i varetekst"
            bSysPara.Hjelpetekst1 = "0-Vg/Løpenr,1-Varetekst,2-LevNr/Best.nr"
            .
        RELEASE bSysPara.
    END.
    /* Styrer om overføringer skal oppdateres direkte */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 11 AND
        syspara.sysgr  = 5 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 11 
            bSysPara.SysGr        = 5 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Oppdater overføring fra PDa direkte"
            bSysPara.Hjelpetekst1 = "0-Direkte,1-Ikke oppdater"
            .
        RELEASE bSysPara.
    END.

  /* Styrer om overføringsordren skal oppdatares direkte */
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 11 AND
      syspara.sysgr  = 5 AND
      syspara.paranr = 3) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 11 
          bSysPara.SysGr        = 5 
          bSysPara.ParaNr       = 3
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Oppdater overføringsordre direkte"
          bSysPara.Hjelpetekst1 = "0-Direkte,1-Ikke oppdater"
          .
      RELEASE bSysPara.
  END.

  /* Styrer antall dager ferdig oppdaterte\ og overførte plukklister skal ligge før de slettes. */
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 11 AND
      syspara.sysgr  = 5 AND
      syspara.paranr = 4) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 11 
          bSysPara.SysGr        = 5 
          bSysPara.ParaNr       = 4
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Antall dager før gamle lister slettes"
          bSysPara.Hjelpetekst1 = "30"
          .
      RELEASE bSysPara.
  END.

  /* Styrer om det er EAN kode eller bestillingsnummer som skal legges ut. */
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 11 AND
      syspara.sysgr  = 5 AND
      syspara.paranr = 5) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 11 
          bSysPara.SysGr        = 5 
          bSysPara.ParaNr       = 5
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Utlegg av EAN eller bestillingsnr"
          bSysPara.Hjelpetekst1 = "0-EAN kode, 1-Bestillingsnummer"
          .
      RELEASE bSysPara.
  END.

  /* Styrer sortering på utlegg. */
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 11 AND
      syspara.sysgr  = 5 AND
      syspara.paranr = 6) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 11 
          bSysPara.SysGr        = 5 
          bSysPara.ParaNr       = 6
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Sortering på utlegg til PDA"
          bSysPara.Hjelpetekst1 = "0-Varetekst, 1-Lev/Bestnr., 2-Vg/Løpnr"
          .
      RELEASE bSysPara.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaPakkseddel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaPakkseddel Procedure 
PROCEDURE setSysParaPakkseddel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:


    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 5 AND
        SysGruppe.SysGr  = 25) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 5
            bSysGruppe.SysGr  = 25
            Beskrivelse      = "Status for pakklistehode"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 5 AND
        SysGruppe.SysGr  = 27) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 5
            bSysGruppe.SysGr  = 27
            Beskrivelse      = "Flagg for visningkolonneoverstyring"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /* Denne status benyttes på HK mens pakkseddelen importeres fra ERP */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr  = 25 AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 5
            bSysPara.Parameter1  = "Innleses"
            bSysPara.Beskrivelse = "Innleses"
            .
        RELEASE bSysPara.
    END.

    /* Denne status benyttes på HK når pakkseddel er klar til å sende den til butikk */
    /* Når den er sendt fra HK til butikk, får den status 90.                        */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr  = 25 AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 6
            bSysPara.Parameter1  = "Import"
            bSysPara.Beskrivelse = "Import"
            .
        RELEASE bSysPara.
    END.

    /* Status på pakkseddel som er klar til å gjøre varemottak på. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr  = 25 AND
        syspara.paranr = 9) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 9
            bSysPara.Parameter1  = "Import - ikke videresendt"
            bSysPara.Beskrivelse = "Import - ikke videresendt"
            .
        RELEASE bSysPara.
    END.

    /* Status på pakkseddel som er klar til å gjøre varemottak på. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr  = 25 AND
        syspara.paranr = 10) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 10
            bSysPara.Parameter1  = "Ny"
            bSysPara.Beskrivelse = "Ny"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr =  25 AND
        syspara.paranr = 15) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 15
            bSysPara.Parameter1  = "Delvis mottatt"
            bSysPara.Beskrivelse = "Delvis mottatt"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr =  25 AND
        syspara.paranr = 20) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 20
            bSysPara.Parameter1  = "Mottatt"
            bSysPara.Beskrivelse = "Mottatt"
            .
        RELEASE bSysPara.
    END.
    /* Denne status brukes på HK når pakkseddelen er overført til butikk. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr =  25 AND
        syspara.paranr = 90) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 25 
            bSysPara.ParaNr      = 90
            bSysPara.Parameter1  = "Sendt"
            bSysPara.Beskrivelse = "Sendt"
            .
        RELEASE bSysPara.
    END.

    /* Denne parameter styrer styrer visning i knapp på toolbar for kolonneoverstyring. */
    FIND bsyspara WHERE
        bsyspara.syshid = 5 AND
        bsyspara.sysgr =  27 AND
        bsyspara.paranr = 1 EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 27 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = "no"
            bSysPara.Beskrivelse = "Vis knapp for kolonneoppsett (yes/no)"
            .
    END.
    IF AVAILABLE bSysPara THEN
        RELEASE bSysPara.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaPakkseddelmottak) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaPakkseddelmottak Procedure 
PROCEDURE setSysParaPakkseddelmottak :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysHode   FOR SysHode.
  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(bSysHode WHERE
                    bSysHode.SysHId = 22) THEN
    DO:
        CREATE bSysHode.
        ASSIGN
            bSysHode.SysHId      = 22
            bSysHode.Beskrivelse = "Pakkseddelmottak"
            bSysHode.Hjelpetekst = " "
            .
    END.


    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 22 AND
        SysGruppe.SysGr  = 1) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 22
            bSysGruppe.SysGr       = 1
            bSysGruppe.Beskrivelse = "Pakkseddel import butikk"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /* Overstyring av HK pris med lokal pris. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 22 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 22 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Bruk lokal utpris"
            bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara WHERE
        bsyspara.syshid = 22 AND
        bsyspara.sysgr  = 1 AND
        bsyspara.paranr = 1 NO-ERROR.
      IF AVAILABLE bSysPara THEN 
      ASSIGN
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Bruk lokal utpris"
            bSysPara.Hjelpetekst1 = "0-Nei,1-Ja".
    END.

    /* Overstyring av HK innpris med lokal innpris. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 22 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 22 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Bruk lokal innpris"
            bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
            .
        RELEASE bSysPara.
    END.
    ELSE DO:
      FIND bsyspara WHERE
        bsyspara.syshid = 22 AND
        bsyspara.sysgr  = 1 AND
        bsyspara.paranr = 2 NO-ERROR.
      IF AVAILABLE bSysPara THEN 
      ASSIGN
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Bruk lokal Innpris"
            bSysPara.Hjelpetekst1 = "0-Nei,1-Ja".
    END.

    /* Pakkseddel behandling --------------------------------------------*/
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 22 AND
        SysGruppe.SysGr  = 5) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 22
            bSysGruppe.SysGr       = 5
            bSysGruppe.Beskrivelse = "Pakkseddel behandling butikk"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

      IF NOT CAN-FIND(syspara WHERE
          syspara.syshid = 22 AND
          syspara.sysgr  = 5 AND
          syspara.paranr = 1) THEN 
      DO:
          CREATE bSysPara.
          ASSIGN  
              bSysPara.SysHId       = 22 
              bSysPara.SysGr        = 5 
              bSysPara.ParaNr       = 1
              bSysPara.Parameter1   = "20"
              bSysPara.Beskrivelse  = "Butikkliste butikker som ikke skal ha etiketter"
              bSysPara.Hjelpetekst1 = "Gjelder når butikken gjør varemottak på pakkseddel fra kassen."
              .
          RELEASE bSysPara.
      END.

      IF NOT CAN-FIND(syspara WHERE
          syspara.syshid = 22 AND
          syspara.sysgr  = 5 AND
          syspara.paranr = 2) THEN 
      DO:
          CREATE bSysPara.
          ASSIGN  
              bSysPara.SysHId       = 22 
              bSysPara.SysGr        = 5 
              bSysPara.ParaNr       = 2
              bSysPara.Parameter1   = "10,40"
              bSysPara.Beskrivelse  = "Butikkliste Outlet butikker"
              bSysPara.Hjelpetekst1 = "Outlet butikker har i noen tilfeller egen håndtering."
              .
          RELEASE bSysPara.
    END.
    
    /* ---------------------- HK ----------------------------------------*/
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 22 AND
        SysGruppe.SysGr  = 10) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 22
            bSysGruppe.SysGr       = 10
            bSysGruppe.Beskrivelse = "Pakkseddel import hk"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /* Varenummer på ukjent vare fra ERP/logistikk. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 22 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 22 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "7000"
            bSysPara.Beskrivelse  = "Varenr. ukjent vare fra ERP"
            bSysPara.Hjelpetekst1 = "Normalt varenr. 7000"
            .
        RELEASE bSysPara.
    END.
    /* Varegruppe på ukjent vare fra ERP/logistikk. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 22 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 22 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "9400"
            bSysPara.Beskrivelse  = "Varegruppe på ukjent vare fra ERP"
            bSysPara.Hjelpetekst1 = "Normalt vg. 9400"
            .
        RELEASE bSysPara.
    END.

    /* Opprettelse av ukjente varer skal logger i korreksjonslogg. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 22 AND
        syspara.sysgr  = 10 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 22 
            bSysPara.SysGr        = 10 
            bSysPara.ParaNr       = 3
            bSysPara.Parameter1   = "1000900"
            bSysPara.Beskrivelse  = "HK's VPI korreksjonslomme"
            bSysPara.Hjelpetekst1 = "Normalt 1000900"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 22 AND
        SysGruppe.SysGr  = 20) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 22
            bSysGruppe.SysGr       = 20
            bSysGruppe.Beskrivelse = "Pakkseddel ved overføring til overskuddslager"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /* Varenummer på ukjent vare fra ERP/logistikk. */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 22 AND
        syspara.sysgr  = 20 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 22 
            bSysPara.SysGr        = 20 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "0"
            bSysPara.Parameter2   = "0"
            bSysPara.Beskrivelse  = "Opprette pakkseddel"
              bSysPara.Hjelpetekst1 = "Butikknr Param-1=Oversk.lager, Param-2=Mottar pakkseddel."
              .
          RELEASE bSysPara.
      END.

      /* Butikker hvor det skal opprettes pakksedler ved varemottak. */
      IF NOT CAN-FIND(syspara WHERE
          syspara.syshid = 22 AND
          syspara.sysgr  = 20 AND
          syspara.paranr = 2) THEN 
      DO:
          CREATE bSysPara.
          ASSIGN  
              bSysPara.SysHId       = 22 
              bSysPara.SysGr        = 20 
              bSysPara.ParaNr       = 2
              bSysPara.Parameter1   = "0"
              bSysPara.Parameter2   = ""
              bSysPara.Beskrivelse  = "Butikkliste - opprette pksdl"
            bSysPara.Hjelpetekst1 = "Komma separert liste med butikknr."
            .
        RELEASE bSysPara.
    END.

    END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaReklamasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaReklamasjon Procedure 
PROCEDURE setSysparaReklamasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  /* Lageroppdateringsstatus */
  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 15 AND
        SysGruppe.SysGr  = 10) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 15
            bSysGruppe.SysGr  = 10
            Beskrivelse      = "Lageroppdateringstatus"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 10 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 10 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = "Ikke oppdatert lager"
            bSysPara.Beskrivelse = "Ikke oppdatert lager"
            .
        RELEASE bSysPara.
    END.    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 10 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 10 
            bSysPara.ParaNr      = 2
            bSysPara.Parameter1  = "Oppdatert lager"
            bSysPara.Beskrivelse = "Oppdatert lager"
            .
        RELEASE bSysPara.
    END.
  END.

  /* Faktureringssstatus */
  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 15 AND
        SysGruppe.SysGr  = 11) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 15
            bSysGruppe.SysGr  = 11
            Beskrivelse      = "Faktureringsoppdateringstatus"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 11 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 11 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = "Ikke fakturert"
            bSysPara.Beskrivelse = "Ikke fakturert"
            .
        RELEASE bSysPara.
    END.    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 11 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 11 
            bSysPara.ParaNr      = 2
            bSysPara.Parameter1  = "Fakturert"
            bSysPara.Beskrivelse = "Fakturert"
            .
        RELEASE bSysPara.
    END.
  END.

  /* Reklamasjonstype */
  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 15 AND
        SysGruppe.SysGr  = 12) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 15
            bSysGruppe.SysGr  = 12
            Beskrivelse      = "Reklamasjonstyper"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 12 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 12 
            bSysPara.ParaNr      = 3
            bSysPara.Parameter1  = "Kundereklamasjon"
            bSysPara.Beskrivelse = "Kundereklamasjon"
            .
        RELEASE bSysPara.
    END.    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 12 AND
        syspara.paranr = 4) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 12 
            bSysPara.ParaNr      = 4
            bSysPara.Parameter1  = "Lagerreklamasjon"
            bSysPara.Beskrivelse = "Lagerreklamasjon"
            .
        RELEASE bSysPara.
    END.
  END.

  /* Reklamasjonskonfigurasjon */
  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 15 AND
        SysGruppe.SysGr  = 14) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 15
            bSysGruppe.SysGr  = 14
            Beskrivelse      = "Reklamasjonskonfigurasjon"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 15 AND
        syspara.sysgr = 14 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 15 
            bSysPara.SysGr       = 14 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Lagerreklam. legges opp pr. butikk"
            bSysPara.Hjelpetekst1 = "0-Nei,1-Ja".
            .
        RELEASE bSysPara.
    END.    
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaSkomodus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaSkomodus Procedure 
PROCEDURE setSysParaSkomodus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.

  DO TRANSACTION:
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 1  AND
        syspara.paranr = 54) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 54
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Skomodus"
            bSysPara.Hjelpetekst1 = "0-Ikke, 1-Skomodus"
            .
        RELEASE bSysPara.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaSlettArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaSlettArtikkel Procedure 
PROCEDURE setSysParaSlettArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

DO FOR bSysPara TRANSACTION:
    IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 3 AND
        bSysPara.ParaNr = 2) THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 2 
            bSysPara.SysGr       = 3 
            bSysPara.ParaNr      = 2
            bSysPara.Parameter1  = "730"
            bSysPara.Beskrivelse = "Slettekontroll. Antall dager "
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaSmtpmail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaSmtpmail Procedure 
PROCEDURE setSysParaSmtpmail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 50 AND
        SysGruppe.SysGr  = 50) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId      = 50
            bSysGruppe.SysGr       = 50
            bSysGruppe.Beskrivelse = "smtpmail"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /*  */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 1
            bSysPara.Parameter1   = "mail.polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mailhub".
            bSysPara.Hjelpetekst1 = "0-Ja,1-Nei"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 2
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "DoAuth".
            bSysPara.Hjelpetekst1 = "0-FALSE,1-TRUE"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 3
            bSysPara.Parameter1   = "base64"
            bSysPara.Beskrivelse  = "AuthType".
            bSysPara.Hjelpetekst1 = "Type autentisering"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 4) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 4
            bSysPara.Parameter1   = "prssport1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "User".
            bSysPara.Hjelpetekst1 = "smtpbruker - blank om para 2 = 0"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 5
            bSysPara.Parameter1   = "2009Sport1"
            bSysPara.Beskrivelse  = "Password".
            bSysPara.Hjelpetekst1 = "blank om para 4 = blank"
            .
        RELEASE bSysPara.
    END.
    
    /* Renser litt */
    FIND bSyspara WHERE
        bsyspara.syshid = 50 AND
        bsyspara.sysgr  = 50 AND
        bsyspara.paranr = 6 NO-ERROR.
    IF AVAILABLE bSysPara AND
      bSysPara.Beskrivelse = "Send mail via elogg server" THEN 
      DELETE bSysPara.
           
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 6
            bSysPara.Parameter1   = "blat"
            bSysPara.Beskrivelse  = "Mailprogram".
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 7) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 7
            bSysPara.Parameter1   = "polygonsoftware.no"
            bSysPara.Beskrivelse  = "SMS Domain".
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 8) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 8
            bSysPara.Parameter1   = "esms.nu"
            bSysPara.Beskrivelse  = "SMS Provider".
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 9) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 9
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Beskrivelse  = "SMSReplyTo".
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 10) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 10
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Parameter2   = "ken1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mottaker nettbutikkordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av overføringsordre fra nettbutikk"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 11) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 11
            bSysPara.Parameter1   = "prssport1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "SMS User".
            bSysPara.Hjelpetekst1 = "SMS Bruker"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 12) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 12
            bSysPara.Parameter1   = "2009Sport1"
            bSysPara.Beskrivelse  = "SMS Password".
            bSysPara.Hjelpetekst1 = "SMS passord"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 20) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 20
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Parameter2   = "ken1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mottaker suppleringsordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av suppleringsordre"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 29) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 29
            bSysPara.Parameter1   = "prssport1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Default avsender suppleringsordre".
            bSysPara.Hjelpetekst1 = "Er denne blank, benyttes eMAil fra butikkregister"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 30) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 30
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Parameter2   = "ken1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mottaker forslag suppleringsordre".
            bSysPara.Hjelpetekst1 = "eMail til mottaker av forslag suppleringsordre"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 31) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 31
            bSysPara.Parameter1   = "tomn@polygonsoftware.no"
            bSysPara.Parameter2   = "ken1@polygonsoftware.no"
            bSysPara.Beskrivelse  = "Mottaker eksport kontant og kreditsalg".
            bSysPara.Hjelpetekst1 = "eMail til regnskapsfører"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 32) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 32
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Send mail via elogg server".
            bSysPara.Hjelpetekst1 = "0-Send direkte, 1-via eloggserver"
              .
          RELEASE bSysPara.
      END.
    IF NOT CAN-FIND(syspara WHERE
          syspara.syshid = 50 AND
          syspara.sysgr  = 50 AND
          syspara.paranr = 33) THEN 
    DO:
          CREATE bSysPara.
          ASSIGN  
              bSysPara.SysHId      = 50 
              bSysPara.SysGr       = 50 
              bSysPara.ParaNr      = 33
              bSysPara.Parameter1  = ""
              bSysPara.Parameter2  = ""
              bSysPara.Beskrivelse = "Send mail ved varemottak via pakkseddel"
              bSysPara.Hjelpetekst1 = "eMail adresseliste (Semicolonseparert)"
              bSysPara.Hjelpetekst2 = "Butikkliste butikker det skal varsles for (Kommaseparert)"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 50 AND
        syspara.sysgr  = 50 AND
        syspara.paranr = 34) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 50 
            bSysPara.SysGr        = 50 
            bSysPara.ParaNr       = 34
            bSysPara.Parameter1   = "tomn@nsoft.no"
            bSysPara.Parameter2   = "0"
            bSysPara.Beskrivelse  = "Mottaker fakturaEMail"
            bSysPara.Hjelpetekst1 = "eMail med faktura fra overføring ved varemottak."
            bSysPara.Hjelpetekst2 = "Ikke aktiv=0, Aktiv =1."
            .
        RELEASE bSysPara.
    END.


  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaSuppleringsordre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaSuppleringsordre Procedure 
PROCEDURE setSysParaSuppleringsordre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  FOR EACH VareBehType:
    CASE VareBehType.VareBehType:
      WHEN 1 THEN VareBehType.BeskrivelseVareBehType = 'Suppleringsordre'.
      WHEN 2 THEN VareBehType.BeskrivelseVareBehType = 'Forhåndsordre'.
    END CASE.
  END.

  DO TRANSACTION:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 5 AND
        SysGruppe.SysGr  = 24) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 5
            bSysGruppe.SysGr  = 24
            Beskrivelse      = "Parametre suppleringsordre"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */


    FIND bsyspara WHERE
         bsyspara.syshid = 5 AND
         bsyspara.sysgr = 24 AND
         bsyspara.paranr = 1 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN 
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 1
            .
    END.
    ASSIGN
        bSysPara.Parameter1  = "SUPPLERINGSORDRE"
        bSysPara.Parameter2  = "FORHÅNDSORDRE"
        bSysPara.Beskrivelse = "Infotekst i skjermbilde"
        .

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 2
            bSysPara.Parameter1  = "Forslaget viser tilgjengelige suppleringsvarer fra sentrallager."
            bSysPara.Beskrivelse = "Infotekst om sentrallager"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 3) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 3
            bSysPara.Parameter1  = "38"
            bSysPara.Beskrivelse = "Leverandørnr. kjede sentrallager"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 4) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 4
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Skal kjedeleverte varere legges på separate supl.ordre?"
            bSysPara.Hjelpetekst1 = " 0-Nei, 1-Ja. Ved opprettelse av suppl.ordre forslag, vil det bli opprettet en ordre pr. leverandør på kjedeleverte varer."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 5) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 5
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Skal det legges opp suppl.ordreforslag på grunnsortimentsvarer?"
            bSysPara.Hjelpetekst1 = " 0-Nei, 1-Ja. Det vil nå også bli opprettet supl.ordreforslag på grunnsort.varer. En ordre pr. leverandør."
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 6) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 6
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Skal suppl.ordre på kjedeleverte varer dannes som overføringsordre?"
            bSysPara.Hjelpetekst1 = " 0-Nei, 1-Ja. På kjedeleverte varer, vil det nå bli dannet overføringsordre."
            .
        RELEASE bSysPara.
    END.
    
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 7) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 7
            bSysPara.Parameter1  = "1"
            bSysPara.Beskrivelse = "Skal det sjekkes mot kjedevare og grunnsortimetnsflagg?"
            bSysPara.Hjelpetekst1 = " 0-Nei, 1-Ja. NB: Overstyrer parameter 5 når Nei er angitt."
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 8) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 8
            bSysPara.Parameter1  = "0"
            bSysPara.Parameter2  = "0"
            bSysPara.Beskrivelse = "Skal det sjekkes mot translogg eller lager?"
            bSysPara.Hjelpetekst1 = " 0-Translogg, 1-Lager."
            bSysPara.Hjelpetekst2 = " Antall - Mindre eller lik antall (0=2 Default)"
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr = 24 AND
        syspara.paranr = 9) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24 
            bSysPara.ParaNr      = 9
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Ved lagersjekk: Antall salgsdager for ordreforslag"
            bSysPara.Hjelpetekst1 = " 0=20 (Default)."
            .
        RELEASE bSysPara.
    END.

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 21 AND
        syspara.sysgr = 3 AND
        syspara.paranr = 100) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 21 
            bSysPara.SysGr        = 3 
            bSysPara.ParaNr       = 100
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Legg inn nye artikler ved import"
            bSysPara.Hjelpetekst1 = " 0-Nei, 1-Ja. Ved import av pakkseddel, skal varer som ikke ligger i varebok legges inn automatisk."
            .
        RELEASE bSysPara.
    END.

    FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 5 AND
        bsyspara.sysgr = 6 AND
        bsyspara.paranr = 1 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN 
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 5 
            bSysPara.SysGr        = 6 
            bSysPara.ParaNr       = 1.
    END.
    ASSIGN 
            bSysPara.Parameter1   = "1"
            bSysPara.Beskrivelse  = "Forhåndsordre"
            bSysPara.Hjelpetekst1 = "".
    RELEASE bSysPara.

    FIND bsyspara EXCLUSIVE-LOCK WHERE
        bsyspara.syshid = 5 AND
        bsyspara.sysgr = 6 AND
        bsyspara.paranr = 2 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN 
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 5 
            bSysPara.SysGr        = 6 
            bSysPara.ParaNr       = 2.
    END.
    ASSIGN 
            bSysPara.Parameter1   = "2"
            bSysPara.Beskrivelse  = "Suppleringsordre"
            bSysPara.Hjelpetekst1 = "".
    RELEASE bSysPara.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaTilHK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaTilHK Procedure 
PROCEDURE setSysparaTilHK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.

  DO TRANSACTION:
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 3 AND
        SysPara.SysGr  = 4 AND
        SysPara.ParaNr = 3 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId = 3 
              bSysPara.SysGr  = 4 
              bSysPara.ParaNr = 3 
              .
          ASSIGN  
              bSyspara.Beskrivelse  = "Masterrecord send"
              bSysPara.Parameter1   = "1"
              bSysPara.Hjelpetekst1 = "Styring om ny metode för HK-eksport skal skje. Para1 = Blank->gammal metode"
              .
          RELEASE bSysPara.
        END.
      FIND SysPara EXCLUSIVE-LOCK WHERE
        SysPara.SysHId = 3 AND
        SysPara.SysGr  = 4 AND
        SysPara.ParaNr = 4 NO-ERROR.
      IF NOT AVAILABLE SysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId = 3 
              bSysPara.SysGr  = 4 
              bSysPara.ParaNr = 4 
              .
          ASSIGN  
              bSyspara.Beskrivelse  = "Send translogg til HK"
              bSysPara.Parameter1   = "0"
              bSysPara.Hjelpetekst1 = "Styrer om translogg skal eksporteres til HK"
              .
          RELEASE bSysPara.
        END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysParaVaretellingslister) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysParaVaretellingslister Procedure 
PROCEDURE setSysParaVaretellingslister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  DO TRANSACTION:


    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 4 AND
        SysGruppe.SysGr  = 2) THEN
    DO FOR bSysGruppe:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 4
            bSysGruppe.SysGr  = 2
            Beskrivelse      = "Tellelistetyper"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 4 AND
        syspara.sysgr  = 2 AND
        syspara.paranr = 1) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 4 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 1
            bSysPara.Parameter1  = "Telleliste"
            bSysPara.Beskrivelse = "TelleListe"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 4 AND
        syspara.sysgr  = 2 AND
        syspara.paranr = 2) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 4 
            bSysPara.SysGr       = 2 
            bSysPara.ParaNr      = 2
            bSysPara.Parameter1  = "Lokasjonsliste"
            bSysPara.Beskrivelse = "Lokasjonsliste"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 4 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 4) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 4 
            bSysPara.SysGr       = 1 
            bSysPara.ParaNr      = 4
            bSysPara.Parameter1  = "0"
            bSysPara.Beskrivelse = "Direkte oppdatering RFID"
            bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
            .
        RELEASE bSysPara.
    END.
  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSysparaVareType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSysparaVareType Procedure 
PROCEDURE setSysparaVareType :
/*------------------------------------------------------------------------------
          Purpose:                                                                                                                                        
          Notes:                                                                                                                                          
  ------------------------------------------------------------------------------*/
  DEF VAR piLoop AS INT NO-UNDO.

  DEF BUFFER bSysPara   FOR SysPara.
  DEF BUFFER bSysGruppe FOR SysGruppe.

  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 2 AND
    SysGruppe.SysGr  = 11) THEN
  DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
      bSysGruppe.SysHId = 2
      bSysGruppe.SysGr  = 11
      Beskrivelse       = "Varetype"
      .
    RELEASE bSysGruppe.
  END. /* bSysGruppe TRANSACTION */

  /*IF NOT CAN-FIND(FIRST SysPara where
      SysPara.SysHId = 2 and
      SysPara.SysGr  = 11) THEN */
  DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 3:
      IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 11 AND
        bSysPara.ParaNr = piLoop) THEN
      DO:
        CREATE bSysPara.
        ASSIGN  
          bSysPara.SysHId      = 2 
          bSysPara.SysGr       = 11 
          bSysPara.ParaNr      = piLoop
          bSysPara.Parameter1  = STRING(piLoop)
          bSysPara.Beskrivelse = (IF piLoop = 1      THEN "Butikkvare"
                                        ELSE IF piLoop = 2 THEN "Hentevare"
                                        ELSE IF piLoop = 3 THEN "Skaffevare"
                                        ELSE                    "Ukjent")
          .
        RELEASE bSysPara.
      END.
    END. /* LOOP */
  END. /* bSysPara TRANSACTION*/

  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 2 AND
    SysGruppe.SysGr  = 12) THEN
  DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
      bSysGruppe.SysHId = 2
      bSysGruppe.SysGr  = 12
      Beskrivelse       = "Salgsstopp"
      .
    RELEASE bSysGruppe.
  END. /* bSysGruppe TRANSACTION */

  DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 3:
      IF NOT CAN-FIND(FIRST bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 12 AND
        bSysPara.ParaNr = piLoop) THEN
      DO:
        CREATE bSysPara.
        ASSIGN  
          bSysPara.SysHId      = 2 
          bSysPara.SysGr       = 12 
          bSysPara.ParaNr      = piLoop
          bSysPara.Parameter1  = STRING(piLoop - 1)
          bSysPara.Beskrivelse = (IF piLoop = 1      THEN "Åpen"
                                        ELSE IF piLoop = 2 THEN "Mykt stopp"
                                        ELSE IF piLoop = 3 THEN "Hardt stopp"
                                        ELSE                    "Ukjent")
          .
        RELEASE bSysPara.
      END.
    END. /* LOOP */
  END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setSyspareOrdreBestilling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSyspareOrdreBestilling Procedure 
PROCEDURE setSyspareOrdreBestilling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bSysPara FOR SysPara.
               
  DO TRANSACTION:
    /* Denne status benyttes på HK mens pakkseddelen importeres fra ERP */
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 5 AND
        syspara.sysgr  = 4 AND
        syspara.paranr = 17) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 5 
            bSysPara.SysGr        = 4 
            bSysPara.ParaNr       = 17
            bSysPara.Parameter1   = "0"
            bSysPara.Beskrivelse  = "Type innleveranserapport"
            bSysPara.Hjelpetekst1 = "0-Side pr. butikk, 1-Samlet"
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 29) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 29
            bSysPara.Parameter1   = "90000000-99999999"
            bSysPara.Beskrivelse  = "HK nummerserie - bestilling"
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    IF NOT CAN-FIND(syspara WHERE
        syspara.syshid = 1 AND
        syspara.sysgr  = 1 AND
        syspara.paranr = 30) THEN DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId       = 1 
            bSysPara.SysGr        = 1 
            bSysPara.ParaNr       = 30
            bSysPara.Parameter1   = "90000000-99999999"
            bSysPara.Beskrivelse  = "Nummerserie - rabattsjekker"
            bSysPara.Hjelpetekst1 = ""
            .
        RELEASE bSysPara.
    END.
    END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTeamType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTeamType Procedure 
PROCEDURE setTeamType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR piLoop AS INT  NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.

ASSIGN
    cTekst = "Ordre,Rapport,Kampanje"
    .
       
DO piLoop = 1 TO 3 TRANSACTION:
    IF NOT CAN-FIND(TeamType WHERE
                    TeamType.TeamTypeId = piLoop) THEN
    DO:
        CREATE TeamType.
        ASSIGN
            TeamType.TeamTypeId  = piLoop
            TeamType.Beskrivelse = ENTRY(piLoop,cTekst)
            TeamType.Notat       = ""
            .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTimeGripParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTimeGripParam Procedure 
PROCEDURE setTimeGripParam :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.
DEFINE VARIABLE cTekst1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst2 AS CHARACTER NO-UNDO.
DEF VAR piLoop AS INT NO-UNDO.

DO TRANSACTION:
  IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 21) THEN
  DO FOR bSysGruppe:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 21
        bSysGruppe.Beskrivelse = "TimeGrip akkumulering og eksport"
        .
    RELEASE bSysGruppe.
  END. /* bSysGruppe */

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 1 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 1.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Benytte Kasserer eller selger transaksjoner"
          SysPara.Hjelpetekst1 = "0-Selger, 1-Kasserer"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 2 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 2.
      ASSIGN  
          SysPara.Parameter1   = "0"
          Syspara.Beskrivelse  = "Benytte Produsent, varemerke eller ingen av delene"
          SysPara.Hjelpetekst1 = "0=Ingen, 1=Produsent, 2=Varemerke"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 3 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 3.
      ASSIGN  
          SysPara.Parameter1   = "c:\home\lindbak\sendes"
          Syspara.Beskrivelse  = "Eksportkatalog timegrip eksportfiler"
          SysPara.Hjelpetekst1 = ""
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 4 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 4.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Eksport TgMed fil"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 5 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 5.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Eksport TgSales fil"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 6 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 6.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Eksport TgSales_Ext fil"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.
    
  FIND SysPara EXCLUSIVE-LOCK WHERE
    SysPara.SysHId =  50 AND
    SysPara.SysGr  =  21 AND
    SysPara.ParaNr = 7 NO-ERROR.
  IF NOT AVAILABLE SysPara THEN
    DO:
      CREATE SysPara.
      ASSIGN
          SysPara.SysHId =  50 
          SysPara.SysGr  =  21 
          SysPara.ParaNr = 7.
      ASSIGN  
          SysPara.Parameter1   = "1"
          Syspara.Beskrivelse  = "Eksport TgTimeStamp fil"
          SysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE SysPara.
    END.

  /* Legger opp lønnsprofiler */
  DO piLoop = 100 TO 110:
    ASSIGN
      cTekst1 = 'Fastlønn u/tillegg|Fastlønn m/overtid|Timelønn m/overtid|Timelønn UB 12 t/u|Timelønn u/tillegg||||||'
      cTekst2 = '1|2|3|4|5||||||'.
    FIND SysPara EXCLUSIVE-LOCK WHERE
      SysPara.SysHId =  50 AND
      SysPara.SysGr  =  21 AND
      SysPara.ParaNr = piLoop NO-ERROR.
    IF NOT AVAILABLE SysPara THEN
      DO:
        CREATE SysPara.
        ASSIGN
            SysPara.SysHId =  50 
            SysPara.SysGr  =  21 
            SysPara.ParaNr = piLoop.
        ASSIGN  
            SysPara.Parameter1   = ENTRY(piLoop - 99,cTekst1,'|')
            SysPara.Parameter2   = ENTRY(piLoop - 99,cTekst2,'|')
            Syspara.Beskrivelse  = "Lønnsprofiler"
            SysPara.Hjelpetekst1 = "Tekst i parameter 1. Kode i parameter 2. <Blank> for ikke i bruk."
            .
        RELEASE SysPara.
      END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setTransaksjonstyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTransaksjonstyper Procedure 
PROCEDURE setTransaksjonstyper :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEF BUFFER bTransType FOR TransType.

    IF NOT CAN-FIND(TransBeskr WHERE
        TransBeskr.TTId = 6 AND
        TransBeskr.TBId = 2) THEN
    DO TRANSACTION:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID        = 6
            TransBeskr.TBId        = 2
            TransBeskr.Beskrivelse = "Overført fra"
            .
    END.
    ELSE 
    DO TRANSACTION:
        FIND TransBeskr WHERE
            TransBeskr.TTId = 6 AND
            TransBeskr.TBId = 2 NO-ERROR.
        IF AVAILABLE TransBeskr THEN 
            TransBeskr.Beskrivelse = "Overført fra".
    END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 160) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 160
        bTransType.Beskrivelse = "Bonuskort"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Bonuskort"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 27) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 27
        bTransType.Beskrivelse = "Kundeordre"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Kundeordre"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 109) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 109
        bTransType.Beskrivelse = "Kombinasjonskampanje"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId =1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Kombinasjonskampanje"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 113) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 113
        bTransType.Beskrivelse = "Bonginfo"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId =1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Bonginfo"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 125) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 125
        bTransType.Beskrivelse = "Alternativt varenr."
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId =1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Alternativt varenr."
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 136) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 136
        bTransType.Beskrivelse = "Gavekort/Tilgodelapp nr. mottatt"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId =1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Gavekort/Tilgodelapp nummer mottatt"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 149) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 149
        bTransType.Beskrivelse = "Manuell prisendring"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Manuell prisendring"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 204) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 204
        bTransType.Beskrivelse = "Z-Rapport"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 1) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 1
            TransBeskr.Beskrivelse = "Z-Rapport"
            .
    END.
END.

IF NOT CAN-FIND(Transtype WHERE
                TransType.TTId = 205) THEN
DO FOR bTransType TRANSACTION:
    CREATE bTransType.
    ASSIGN
        bTranstype.TTId = 205
        bTransType.Beskrivelse = "Kupong spesifikasjon"
        bTransType.Aktiv = TRUE
        .
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 2) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 2
            TransBeskr.Beskrivelse = "KI B Betalcheck"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 3) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 3
            TransBeskr.Beskrivelse = "KI C Värdecheck"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 4) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 4
            TransBeskr.Beskrivelse = "KI R Rabattkupong"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 5) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 5
            TransBeskr.Beskrivelse = "KI E Elektronisk kupong"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 6) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 6
            TransBeskr.Beskrivelse = "KI M Mobil kupong"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 7) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 7
            TransBeskr.Beskrivelse = "KI P Produktcheck"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 8) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 8
            TransBeskr.Beskrivelse = "KI V Varecheck"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 9) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 9
            TransBeskr.Beskrivelse = "KI C Presentcheck"
            .
    END.
    IF NOT CAN-FIND(TransBeskr WHERE
                    TransBeskr.TTId = bTransType.TTId AND
                    TransBeskr.TBId = 10) THEN
    DO:
        CREATE TransBeskr.
        ASSIGN
            TransBeskr.TTID = bTransType.TTId
            TransBeskr.TBId = 10
            TransBeskr.Beskrivelse = "KI ? Øvrigt"
            .
    END.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-setVagabondEksport) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVagabondEksport Procedure
PROCEDURE setVagabondEksport:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 1 AND
    SysGruppe.SysGr  = 8) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 1
        bSysGruppe.SysGr  = 8
        Beskrivelse       = 'Eksportparametre Vagabond'.
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

VAGABOND:
DO FOR bSysPara TRANSACTION:
  LOOP:
  DO piLoop = 1 TO 7:
      IF NOT CAN-FIND(FIRST bSysPara WHERE
          bSysPara.SysHId = 1 AND
          bSysPara.SysGr  = 8 AND
          bSysPara.ParaNr = piLoop) THEN
      OPPRETT:
      DO:
          CREATE bSysPara.
          ASSIGN  
              bSysPara.SysHId       = 1 
              bSysPara.SysGr        = 8 
              bSysPara.ParaNr       = piLoop
              bSysPara.Beskrivelse  = (IF piLoop      = 1 THEN "Eksportkatalog"
                                       ELSE IF piLoop = 2 THEN "Filprefiks"
                                       ELSE IF piLoop = 3 THEN "Kundenr. hos Vagabond"
                                       ELSE IF piLoop = 4 THEN "Lende løpnr. og løpnr"
                                       ELSE IF piLoop = 5 THEN "Ekstent"
                                       ELSE IF piLoop = 6 THEN "Butikknr. Vagabondeksport"
                                       ELSE IF piLoop = 7 THEN "Levnr. som skal behandles"
                                       ELSE "")
              bSysPara.Parameter1   = (IF piLoop      = 1 THEN "c:\tmp"
                                       ELSE IF piLoop = 2 THEN "P"
                                       ELSE IF piLoop = 3 THEN ""
                                       ELSE IF piLoop = 4 THEN "7"
                                       ELSE IF piLoop = 5 THEN "VB"
                                       ELSE IF piLoop = 6 THEN ""
                                       ELSE IF piLoop = 7 THEN ""
                                       ELSE "")
              bSysPara.Hjelpetekst1 = (IF piLoop      = 1 THEN ""
                                       ELSE IF piLoop = 2 THEN ""
                                       ELSE IF piLoop = 3 THEN ""
                                       ELSE IF piLoop = 4 THEN ""
                                       ELSE IF piLoop = 5 THEN ""
                                       ELSE IF piLoop = 6 THEN "Semikolon separert liste"
                                       ELSE IF piLoop = 7 THEN "Semikolon separert liste"
                                       ELSE "")
              .
          RELEASE bSysPara.
      END. /* OPPRETT */
  END. /* LOOP */
END. /* VAGABOND bSysPara TRANSACTION*/

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-setVareBokParam) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVareBokParam Procedure 
PROCEDURE setVareBokParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT NO-UNDO.
DEF VAR piLoop2 AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.
DEF BUFFER bSysHode   FOR SysHode.

IF NOT CAN-FIND(SysHode WHERE
                SysHode.SysHId = 21) THEN
DO TRANSACTION:
    CREATE bSysHode.
    ASSIGN
        bSysHode.SysHId      = 21
        bSysHode.Beskrivelse = "Oppsett varebøker"
        .
    RELEASE bSysHode.
END.

DO piLoop = 1 TO 4:
    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 21 AND
        SysGruppe.SysGr  = piLoop) THEN
    DO FOR bSysGruppe TRANSACTION:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 21
            bSysGruppe.SysGr  = piLoop
            Beskrivelse      = IF piLoop = 1 THEN "Styreparametre Varebok"
                               ELSE IF piLoop = 2 THEN "Styreparametre Messeordre"
                               ELSE IF piLoop = 3 THEN "Styreparametre Suppleringsordre"
                               ELSE "Direkte varemottak"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */
END.

/*IF NOT CAN-FIND(FIRST SysPara where
    SysPara.SysHId = 2 and
    SysPara.SysGr  = 7) THEN */
VAREBOKLOOP:
DO piLoop2 = 1 TO 4:
  DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 4:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 21 AND
            bSysPara.SysGr  = piLoop2 AND
            bSysPara.ParaNr = piLoop) THEN
        OPPRETT:
        DO:
            IF piLoop2 = 4 AND piLoop = 4 THEN
                LEAVE OPPRETT.
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId       = 21 
                bSysPara.SysGr        = piLoop2 
                bSysPara.ParaNr       = piLoop
                bSysPara.Parameter1   = "1" /* Default er tilgang */
                bSysPara.Beskrivelse  = (IF piLoop = 1      THEN "Tilgang artikkelkort"
                                         ELSE IF piLoop = 2 THEN "Tilgang forenklet varevedlikehold"
                                         ELSE IF piLoop = 3 THEN "Tilgang Utvalg og Alt-S(Artikkelsøk)"
                                         ELSE IF piLoop = 4 THEN "Tilgang sortimentregistrering"
                                         ELSE "")
                bSysPara.Hjelpetekst1 = "0=Ingen,1 = Tilgang"
                .
            RELEASE bSysPara.
        END. /* OPPRETT */
    END. /* LOOP */
  END. /* bSysPara TRANSACTION*/
END. /* VAREBOKLOOP */

/* Logistikkpartner */
IF NOT CAN-FIND(FIRST SysPara WHERE
    SysPara.SysHId = 1 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr = 53) THEN
OPPRETT:
DO FOR bSysPara TRANSACTION:
    CREATE bSysPara.
    ASSIGN  
        bSysPara.SysHId       = 1 
        bSysPara.SysGr        = 1 
        bSysPara.ParaNr       = 53
        bSysPara.Parameter1   = "38" /* Default = Ingen leverandør */
        bSysPara.Beskrivelse  = "Kjedens logistikkpartner"
        bSysPara.Hjelpetekst1 = "Angi leverandørnummer som representerer logistikkpartner."
        .
    RELEASE bSysPara.
END. /* OPPRETT */

OPPRETT:
DO FOR bSysPara TRANSACTION:
    FIND bSysPara WHERE
        bSysPara.SysHId = 5 AND
        bSysPara.SysGr  = 3 AND
        bSysPara.ParaNr = 3 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId = 5 
            bSysPara.SysGr  = 3 
            bSysPara.ParaNr = 3
            .
    END.
    ASSIGN
        bSysPara.Beskrivelse  = "Delvis bekreftet"
        bSysPara.Parameter1   = "DBEKR" 
        .

    FIND bSysPara WHERE
        bSysPara.SysHId = 5 AND
        bSysPara.SysGr  = 3 AND
        bSysPara.ParaNr = 5 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId = 5 
            bSysPara.SysGr  = 3 
            bSysPara.ParaNr = 5
            .
    END.
    ASSIGN
        bSysPara.Beskrivelse  = "Delvis levert"
        bSysPara.Parameter1   = "DLEV" 
        .
    RELEASE bSysPara.
END. /* OPPRETT */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVPIBehandlingsStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIBehandlingsStatus Procedure 
PROCEDURE setVPIBehandlingsStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bSysGruppe FOR SysGruppe.
    DEF BUFFER bSysPara   FOR SysPara.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 21 AND
            SysGruppe.SysGr  = 200) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 21
                bSysGruppe.SysGr  = 200
                bSysGruppe.Beskrivelse      = "VPI behandlingsstatus"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 0) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 0
                bSysPara.Beskrivelse = "Alle ubehandlede"
                bSysPara.Parameter1  = "Alle ubehandlede".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Ubehandlet"
                bSysPara.Parameter1  = "Ubehandlet".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 2) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 2
                bSysPara.Beskrivelse = "Aut.importert"
                bSysPara.Parameter1  = "Aut.importert".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 3) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 3
                bSysPara.Beskrivelse = "Strekkodekontroll"
                bSysPara.Parameter1  = "Strekkodekontroll".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Vent"
                bSysPara.Parameter1  = "Vent".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 20) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 20
                bSysPara.Beskrivelse = "Kontroller"
                bSysPara.Parameter1  = "Kontroller".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 30) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 30
                bSysPara.Beskrivelse = "Feil"
                bSysPara.Parameter1  = "Feil".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 80) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 80
                bSysPara.Beskrivelse = "Avvist"
                bSysPara.Parameter1  = "Avvist".
            RELEASE bSysPara.
        END.
        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 21 AND
                        bSysPara.SysGr  = 200 AND
                        bSysPara.ParaNr = 90) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 200 
                bSysPara.ParaNr      = 90
                bSysPara.Beskrivelse = "Behandlet"
                bSysPara.Parameter1  = "Behandlet".
            RELEASE bSysPara.
        END.
    END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVPIFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIFil Procedure 
PROCEDURE setVPIFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR pcTypeLst  AS CHAR NO-UNDO.
DEF VAR pcKortLst  AS CHAR NO-UNDO.
DEF VAR pcBeskrLst AS CHAR NO-UNDO.
DEF VAR piType     AS INT  NO-UNDO.
DEF VAR pcBeskr    AS CHAR NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.
DEF VAR pcKort     AS CHAR NO-UNDO.

DEF BUFFER bEkstVPIFil FOR EkstVPIFil.

STRONG_SCOOP:
DO FOR bEkstVPIFil TRANSACTION:
    /* Korrekskjonsmelding fra HK til butikk */
    FIND bEkstVPIFil WHERE
         bEkstVPIFil.EkstVPILevNr = 1 AND
         bEkstVPIFil.VPIFilNr     = 5 NO-ERROR.
    IF NOT AVAILABLE bEkstVPIFil THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 1 
            bEkstVPIFil.VPIFilNr              = 5
            bEkstVPIFil.VPIFilType            = 6
            bEkstVPIFil.VPIFilBeskrivelse     = "VPI korr. melding fra HK"
            bEkstVPIFil.VPIFilNavn            = "HKKORRVPI"
            bEkstVPIFil.VPIEkst               = STRING(iCl)
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    ELSE DO:
        ASSIGN
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
    END.

    /* VPI fra HK til butikk */
    FIND bEkstVPIFil WHERE
         bEkstVPIFil.EkstVPILevNr = 1 AND
         bEkstVPIFil.VPIFilNr     = 1 NO-ERROR.
    IF NOT AVAILABLE bEkstVPIFil THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 1 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "Vare og prisinformasjon fra HK"
            bEkstVPIFil.VPIFilNavn            = "HKVPI"
            bEkstVPIFil.VPIEkst               = STRING(iCl)
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xhkvpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xhkvpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    /*
    ELSE DO:
        ASSIGN
            bEkstVPIFil.VPIFilNavn            = "HKVPI"
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    */
    /* POS Salg fra butikk til HK */
    FIND bEkstVPIFil WHERE
         bEkstVPIFil.EkstVPILevNr = 1 AND
         bEkstVPIFil.VPIFilNr     = 2 NO-ERROR.
    IF NOT AVAILABLE bEkstVPIFil THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 1 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 10
            bEkstVPIFil.VPIFilBeskrivelse     = "Salgsdata fra butikkene"
            bEkstVPIFil.VPIFilNavn            = "POS2"
            bEkstVPIFil.VPIEkst               = STRING(iCl)
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xpossalginnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xpossalgutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    ELSE DO:
        ASSIGN
            bEkstVPIFil.VPIFilNavn            = "POS2"
            .
        RELEASE bEkstVPIFil.
    END.

    /* Ordre fra HK */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 1 AND
                    bEkstVPIFil.VPIFilNr     = 3) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 1 
            bEkstVPIFil.VPIFilNr              = 3
            bEkstVPIFil.VPIFilType            = 4
            bEkstVPIFil.VPIFilBeskrivelse     = "Ordrebekreftelse fra HK"
            bEkstVPIFil.VPIFilNavn            = "ORDHK"
            bEkstVPIFil.VPIEkst               = STRING(iCl)
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xhkordinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /*
    ELSE DO:
        FIND bEkstVPIFil WHERE
             bEkstVPIFil.EkstVPILevNr = 1 AND
             bEkstVPIFil.VPIFilNr     = 3 EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bEkstVPIFil THEN
        DO:
            ASSIGN
            bEkstVPIFil.VPIFilAktiv = TRUE.
            RELEASE bEkstVPIFil.
        END.
    END.
    */
    /* Pakkseddel fra HK */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 1 AND
                    bEkstVPIFil.VPIFilNr     = 4) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILevNr          = 1 
            bEkstVPIFil.VPIFilNr              = 4
            bEkstVPIFil.VPIFilType            = 12
            bEkstVPIFil.VPIFilBeskrivelse     = "Pakkseddel fra HK"
            bEkstVPIFil.VPIFilNavn            = "PKSDLHK"
            bEkstVPIFil.VPIEkst               = STRING(iCl)
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xhkordinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    /*
    ELSE DO:
        FIND bEkstVPIFil WHERE
             bEkstVPIFil.EkstVPILevNr = 1 AND
             bEkstVPIFil.VPIFilNr     = 4 EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bEkstVPIFil THEN
        DO:
            ASSIGN
            bEkstVPIFil.VPIFilAktiv = TRUE.
            RELEASE bEkstVPIFil.
        END.
    END.
    */
    
    /* Symbol 8800 Varetelling */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 894) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 894
            EkstVPILev.KortNavn     = "PPT 8800"
            EkstVPILev.Navn         = "Symbol PPT 8800"
            EkstVPILev.AktivLev     = TRUE 
            .
    END.
    /* Håndterminalfil fra PPT8800 */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 894 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 894 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 8
            bEkstVPIFil.VPIFilBeskrivelse     = "Varetelling fra Symb PPT 8800"
            bEkstVPIFil.VPIFilNavn            = "VARETRAN"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xppt880iinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xppt880utpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    
    /* Hjem&Hobby */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 897) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 897
            EkstVPILev.KortNavn     = "Hjem&Hobby"
            EkstVPILev.Navn         = "Hjem & Hobby"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* Hjem&Hobby VPI fil */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 897 AND
        bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 897 
            bEkstVPIFil.VPIFilNr             = 1
            bEkstVPIFil.VPIFilType           = 2
            bEkstVPIFil.VPIFilBeskrivelse    = "Konvertering VPI til pricat"
            bEkstVPIFil.VPIFilNavn           = "HHVPI"
            bEkstVPIFil.VPIEkst              = "csv"
            bEkstVPIFil.VPIKatalog           = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine = "xhhvpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xstdutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* GantNorge */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 918) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 918
            EkstVPILev.KortNavn     = "Gant Norge"
            EkstVPILev.Navn         = "Gant Norge"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* Gant Norge VPI fil */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 918 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 918 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Konvertering VPI til pricat"
            bEkstVPIFil.VPIFilNavn            = "GNVPI"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xgantvpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Gant Norge VPI fil */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
        bEkstVPIFil.EkstVPILevNr = 918 AND
        bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr         = 918 
            bEkstVPIFil.VPIFilNr             = 2
            bEkstVPIFil.VPIFilType           = 1
            bEkstVPIFil.VPIFilBeskrivelse    = "VPI Pricat"
            bEkstVPIFil.VPIFilNavn           = "VPI918"
            bEkstVPIFil.VPIEkst              = "csv"
            bEkstVPIFil.VPIKatalog           = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine  = "xsport1vpiutpakk"
            bEkstVPIFil.VPIOperator          = 2
            bEkstVPIFil.VPIFilAktiv          = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Websystem */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 898) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 898
            EkstVPILev.KortNavn     = "MedWeb"
            EkstVPILev.Navn         = "Medlemsweb"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* MEDLEMSDATA */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 898 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 898 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 14
            bEkstVPIFil.VPIFilBeskrivelse     = "Medlemsdata fra Web"
            bEkstVPIFil.VPIFilNavn            = "MEDLW"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xwebmedinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Kreditkunder */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 898 AND
                    bEkstVPIFil.VPIFilNr     = 5) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 898 
            bEkstVPIFil.VPIFilNr              = 5
            bEkstVPIFil.VPIFilType            = 14
            bEkstVPIFil.VPIFilBeskrivelse     = "Import av kreditkunder"
            bEkstVPIFil.VPIFilNavn            = "KREDK"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xkundeimport"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Websystem */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 899) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 899
            EkstVPILev.KortNavn     = "VISMAG"
            EkstVPILev.Navn         = "VISMA Ordre/ordrebekr."
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* Import av lagerfil */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 899 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 899 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 3
            bEkstVPIFil.VPIFilBeskrivelse     = "Lagerstatus fra leverandør"
            bEkstVPIFil.VPIFilNavn            = "LAG"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xvglaginnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Import av ordrebekreftelse */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 899 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 899 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 4
            bEkstVPIFil.VPIFilBeskrivelse     = "Ordre og ordrebekr. fra leverandør"
            bEkstVPIFil.VPIFilNavn            = "BEKR"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xvgordinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Import av pakkseddel */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 899 AND
                    bEkstVPIFil.VPIFilNr     = 3) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 899 
            bEkstVPIFil.VPIFilNr              = 3
            bEkstVPIFil.VPIFilType            = 13
            bEkstVPIFil.VPIFilBeskrivelse     = "Ordre og ordrebekr. fra leverandør"
            bEkstVPIFil.VPIFilNavn            = "PKSDL"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xvgordinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 899 AND
                    bEkstVPIFil.VPIFilNr     = 4) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 899 
            bEkstVPIFil.VPIFilNr              = 4
            bEkstVPIFil.VPIFilType            = 9
            bEkstVPIFil.VPIFilBeskrivelse     = "Lagerstatus fra webbutikk"
            bEkstVPIFil.VPIFilNavn            = "WEBV"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xvgwebinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = NOT lHK
            .
        RELEASE bEkstVPIFil.
    END.

    /* Import av statusfil fra OC - Sentrallager */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 899 AND
                    bEkstVPIFil.VPIFilNr     = 5) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 899 
            bEkstVPIFil.VPIFilNr              = 5
            bEkstVPIFil.VPIFilType            = 9
            bEkstVPIFil.VPIFilBeskrivelse     = "Lagerstatus fra webbutikk"
            bEkstVPIFil.VPIFilNavn            = "HKWEBV"
            bEkstVPIFil.VPIEkst               = STRING(iCL)
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xvgwebbutinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = NOT lHK
            .
        RELEASE bEkstVPIFil.
    END.


    /* EkstvPILev*/
    IF NOT CAN-FIND(FIRST EkstVPILev WHERE
                    EkstVPILevNr = 901) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 901
            EkstVPILev.KortNavn     = "PLUKKLISTE"
            EkstVPILev.Navn         = "Plukkliste fra PDA"
            EkstVPILev.Aktiv        = FALSE
            .
        RELEASE EkstVPILev.
    END.
    /* Import av plukkliste fra PDA */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 901 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 901 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 7
            bEkstVPIFil.VPIFilBeskrivelse     = "Plukkliste fra PDA"
            bEkstVPIFil.VPIFilNavn            = "PLTRANS"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xplukklisteinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* EkstvPILev*/
    IF NOT CAN-FIND(FIRST EkstVPILev WHERE
                    EkstVPILevNr = 902) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 902
            EkstVPILev.KortNavn     = "Kassereroppgjør"
            EkstVPILev.Navn         = "Kassereroppgjør InfoPOS 8.0 kasse"
            EkstVPILev.Aktiv        = FALSE
            .
        RELEASE EkstVPILev.
    END.
    /* Import av plukkliste fra PDA */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 902 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 902 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 16
            bEkstVPIFil.VPIFilBeskrivelse     = "Kassereroppgjør InfoPOS 8.0"
            bEkstVPIFil.VPIFilNavn            = "kassdag"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xkassdaginnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
END. /* STRONG_SCOOP */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVPIFil2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIFil2 Procedure 
PROCEDURE setVPIFil2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR pcTypeLst  AS CHAR NO-UNDO.
DEF VAR pcKortLst  AS CHAR NO-UNDO.
DEF VAR pcBeskrLst AS CHAR NO-UNDO.
DEF VAR piType     AS INT  NO-UNDO.
DEF VAR pcBeskr    AS CHAR NO-UNDO.
DEF VAR piLoop     AS INT  NO-UNDO.
DEF VAR pcKort     AS CHAR NO-UNDO.

DEF BUFFER bEkstVPIFil FOR EkstVPIFil.

STRONG_SCOOP:
DO FOR bEkstVPIFil TRANSACTION:
    /* Innlesning av kvitteringsfil fra WinEDI (Postpakkeetikett) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 885) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 885
            EkstVPILev.KortNavn     = "WinEDI"
            EkstVPILev.Navn         = "WinEDI Postpakkeetikettt"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* VPI */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 885 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 885 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 15
            bEkstVPIFil.VPIFilBeskrivelse     = "WinEDI Postpakkeetikettt"
            bEkstVPIFil.VPIFilNavn            = "POST"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xwinEDIinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Innlesning av pakkefil (Sport1 leverandør) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 887) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 887
            EkstVPILev.KortNavn     = "Pakke vare"
            EkstVPILev.Navn         = "Pakke vare"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* VPI */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 887 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 887 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Pakke vare struktur import"
            bEkstVPIFil.VPIFilNavn            = "VPIPK"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1pakkeinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* RIGAL vareinnlesning (Preem leverandør) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 889) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 889
            EkstVPILev.KortNavn     = "Menigo RIGAL"
            EkstVPILev.Navn         = "Menigo RIGAL"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    ELSE DO:
      FIND EkstVPILev WHERE
           EkstVPILEv.EkstVPILevNr = 889 NO-ERROR.
        ASSIGN
            EkstVPILev.KortNavn     = "Menigo RIGAL"
            EkstVPILev.Navn         = "Menigo RIGAL"
            .    
    END.    
    
    /* VPI - konvertering av RIGAL */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 889 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 889 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "RIGAL1 VPI til PRICAT"
            bEkstVPIFil.VPIFilNavn            = "V"
            bEkstVPIFil.VPIEkst               = "001"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xri1viinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* VPI - innlesning av konvertert vpi fil (Utvidet pricat format) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 889 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 889 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "VPI Pricat konvertert fra Rigal"
            bEkstVPIFil.VPIFilNavn            = "GVPI889"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* IPS RIGAL vareinnlesning */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 891) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 891
            EkstVPILev.KortNavn     = "IPS RIGAL"
            EkstVPILev.Navn         = "IPS RIGAL"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* IPS - konvertering av IPS-RIGAL */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 891 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 891 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS RIGAL VPI til PRICAT"
            bEkstVPIFil.VPIFilNavn            = "V"
            bEkstVPIFil.VPIEkst               = "001"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xri1viinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* VPI - innlesning av konvertert vpi fil (Utvidet pricat format) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 891 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 891 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS VPI Pricat konvertert fra Rigal"
            bEkstVPIFil.VPIFilNavn            = "GVPI891"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Innlesning av WBART fil fra nettbutikk */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 890) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 890
            EkstVPILev.KortNavn     = "Nettb.art.info"
            EkstVPILev.Navn         = "Nettbutikk art.info"
            EkstVPILev.AktivLev     = FALSE 
            .
    END.
    /* Artikkefil fra nettbutikk - settint av webbutikk */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 890 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 890 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "Artikkelinformasjon fra nettbutikk"
            bEkstVPIFil.VPIFilNavn            = "WBART"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xwbartinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Artikkefil fra nettbutikk - utlegg av lager */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 890 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 890 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Utlegg lager"
            bEkstVPIFil.VPIFilNavn            = "WBREQ"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xwreqinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.


    /* Casio Varetelling */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 893) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 893
            EkstVPILev.KortNavn     = "CASIO DT900"
            EkstVPILev.Navn         = "CASIO DT900 M51E"
            EkstVPILev.AktivLev     = TRUE 
            .
    END.
    /* Håndterminalfil fra PPT8800 */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 893 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 893 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 8
            bEkstVPIFil.VPIFilBeskrivelse     = "Varetelling fra CASIO DT900 M51E"
            bEkstVPIFil.VPIFilNavn            = "TELLING"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xcasiodt900iinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xppt880utpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* BxMobile Varetelling */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 892) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 892
            EkstVPILev.KortNavn     = "BxMobile import"
            EkstVPILev.Navn         = "BxMobile import"
            EkstVPILev.AktivLev     = TRUE 
            .
    END.
    /* Håndterminalfil fra PPT8800 */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 892 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 892 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 8
            bEkstVPIFil.VPIFilBeskrivelse     = "Varetelling fra BxMobile"
            bEkstVPIFil.VPIFilNavn            = "INV"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            /*bEkstVPIFil.VPIInnlesningsrutine  = "xbxmobileinvinnles"*/
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxwuinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    ELSE DO:
        FIND bEkstVPIFil WHERE
             bEkstVPIFil.EkstVPILevNr = 892 AND
             bEkstVPIFil.VPIFilNr     = 1 NO-ERROR.
        IF AVAILABLE bEkstVPIFil THEN 
        DO:
            ASSIGN 
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxwuinnles"
            bEkstVPIFil.VPIFilAktiv           = TRUE
            bEkstVPIFil.VPIEkst               = "*".          
            RELEASE bEkstVPIFil.
        END.      
    END.

    /* BxMobile Varetelling */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 917) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 917
            EkstVPILev.KortNavn     = "BxCentral (Gml)"
            EkstVPILev.Navn         = "BxCentral (Gml)"
            EkstVPILev.AktivLev     = TRUE 
            .
    END.
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 917 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 917 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 8
            bEkstVPIFil.VPIFilBeskrivelse     = "Varetelling fra BxCentral (Gml)"
            bEkstVPIFil.VPIFilNavn            = "INV"
            bEkstVPIFil.VPIEkst               = "DAT"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            /*bEkstVPIFil.VPIInnlesningsrutine  = "xbxmobileinvinnles"*/
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxCentralinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    ELSE DO:
        FIND bEkstVPIFil WHERE
             bEkstVPIFil.EkstVPILevNr = 917 AND
             bEkstVPIFil.VPIFilNr     = 1 NO-ERROR.
        IF AVAILABLE bEkstVPIFil THEN 
        DO:
            ASSIGN 
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxCentralinnles"
            /*bEkstVPIFil.VPIFilAktiv           = FALSE*/
            bEkstVPIFil.VPIEkst               = "DAT".          
            RELEASE bEkstVPIFil.
        END.      
    END.
    
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 917 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 917 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 9
            bEkstVPIFil.VPIFilBeskrivelse     = "Pakkseddel fra BxCentral (Gml)"
            bEkstVPIFil.VPIFilNavn            = "PINV"
            bEkstVPIFil.VPIEkst               = "DAT"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxCPksdlInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Håndterminalfil fra PPT8800 */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 892 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 892 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 19
            bEkstVPIFil.VPIFilBeskrivelse     = "Lagerjustering BxMobile"
            bEkstVPIFil.VPIFilNavn            = "LJUST"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            /*bEkstVPIFil.VPIInnlesningsrutine  = "xbxmobileinvinnles"*/
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxljustinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Håndterminalfil fra PPT8800 */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 892 AND
                    bEkstVPIFil.VPIFilNr     = 3) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 892 
            bEkstVPIFil.VPIFilNr              = 3
            bEkstVPIFil.VPIFilType            = 20
            bEkstVPIFil.VPIFilBeskrivelse     = "Priskontroll BxMobile"
            bEkstVPIFil.VPIFilNavn            = "PKONT"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            /*bEkstVPIFil.VPIInnlesningsrutine  = "xbxmobileinvinnles"*/
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxpriskinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Håndterminalfil fra PPT8800 */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 892 AND
                    bEkstVPIFil.VPIFilNr     = 4) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 892 
            bEkstVPIFil.VPIFilNr              = 4
            bEkstVPIFil.VPIFilType            = 20
            bEkstVPIFil.VPIFilBeskrivelse     = "Overføring BxMobile"
            bEkstVPIFil.VPIFilNavn            = "OVERF"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            /*bEkstVPIFil.VPIInnlesningsrutine  = "xbxmobileinvinnles"*/
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxoverforinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.

    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 892 AND
                    bEkstVPIFil.VPIFilNr     = 5) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 892 
            bEkstVPIFil.VPIFilNr              = 5
            bEkstVPIFil.VPIFilType            = 12
            bEkstVPIFil.VPIFilBeskrivelse     = "Varemottak BxMobile"
            bEkstVPIFil.VPIFilNavn            = "VMOT"
            bEkstVPIFil.VPIEkst               = "*"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            /*bEkstVPIFil.VPIInnlesningsrutine  = "xbxmobileinvinnles"*/
            bEkstVPIFil.VPIInnlesningsrutine  = "xbxvmotinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.

    /* BITS ePages (Nettbutikk) Kundeimport */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 888) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 888
            EkstVPILev.KortNavn     = "BITS Kundefil"
            EkstVPILev.Navn         = "BITS ePages Kunde"
            EkstVPILev.AktivLev     = TRUE 
            .
    END.
    /* Kundefil fra ePAges BITS */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 888 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 888 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 14 /* MED Medlemsinformasjon */
            bEkstVPIFil.VPIFilBeskrivelse     = "Nettkunde fra ePages BITS"
            bEkstVPIFil.VPIFilNavn            = "WBKUN"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xinnBITSCustomer"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Kundefil fra ePAges BITS */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 888 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 888 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 15 /* KORD Kundeordreinformasjon */
            bEkstVPIFil.VPIFilBeskrivelse     = "Kundeordre fra ePages BITS"
            bEkstVPIFil.VPIFilNavn            = "WBORD"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xinnBITSOrder"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.


    /* Automatisk import av VPI fra ERP - Sport1HK */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 903) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 903
            EkstVPILev.KortNavn     = "Pricat fra ERP"
            EkstVPILev.Navn         = "Pricat fra ERP"
            EkstVPILev.AktivLev     = FALSE
            .
    END.

    /* VPI Sesongbok */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 903 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 903 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Sesongbok Pricat fra ERP"
            bEkstVPIFil.VPIFilNavn            = "SBA"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnauto"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiauto"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* VPI Sesongbok */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 903 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 903 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Endringer i sesong Pricat fra ERP"
            bEkstVPIFil.VPIFilNavn            = "AE"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnauto"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiauto"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Automatisk innlesning av pakkefil (Sport1 ERP) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 904) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 904
            EkstVPILev.KortNavn     = "Pricat pakke fra ERP"
            EkstVPILev.Navn         = "Pricat pakke fra ERP"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* VPI */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 904 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 904 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Sesongbok Pakke vare struktur"
            bEkstVPIFil.VPIFilNavn            = "SPK"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1pakkeinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 904 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 904 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Endringer i sesong Pakke vare struktur"
            bEkstVPIFil.VPIFilNavn            = "APK"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1pakkeinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* Automatisk innlesning av pakkefil (Sport1 ERP) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 905) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 905
            EkstVPILev.KortNavn     = "Budsjettunderlag"
            EkstVPILev.Navn         = "Budsjettunderlag"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* VPI */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 905 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 905 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 17
            bEkstVPIFil.VPIFilBeskrivelse     = "Budsjettunderlag"
            bEkstVPIFil.VPIFilNavn            = "BUDGET"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xbudgetinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    /* IPS Excel til RIGAL vareinnlesning */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 906) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 906
            EkstVPILev.KortNavn     = "IPS EXCEL"
            EkstVPILev.Navn         = "IPS EXCEL"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* IPS - konvertering av IPS-RIGAL */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 906 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 906 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS Excel VPI til RIGAL"
            bEkstVPIFil.VPIFilNavn            = "RVPI"
            bEkstVPIFil.VPIEkst               = "xls"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xexcelvpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* IPS VPI EDI fil til Pricat Axfood/Preem*/
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 907) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 907
            EkstVPILev.KortNavn     = "VPI EDI"
            EkstVPILev.Navn         = "VPI EDI Preem/Axfood"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* IPS VPI EDI fil til Pricat Axfood/Preem */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 907 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 907 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS EDI til PRICAT (Axfood)"
            bEkstVPIFil.VPIFilNavn            = "ARTIKEL"
            bEkstVPIFil.VPIEkst               = "EDI"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xedivpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* VPI - innlesning av konvertert vpi fil (Utvidet pricat format) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 907 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 907 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "IPS EDI til PRICAT (Axfood)"
            bEkstVPIFil.VPIFilNavn            = "GVPI907"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* PRICAT VPI fil fra T.Fjellan (MxSport) */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 908) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 908
            EkstVPILev.KortNavn     = "VPI Pricat"
            EkstVPILev.Navn         = "VPI Pricat MxSport"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* PRICAT VPI fil fra T.Fjellan (MxSport) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 908 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 908 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Pricat Mx til PRICAT PRS"
            bEkstVPIFil.VPIFilNavn            = "MXVPI"
            bEkstVPIFil.VPIEkst               = "txt"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xpricatmxinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Størrelsesindeks fra ERP */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 910 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        IF NOT CAN-FIND(EkstVPILev WHERE
                        EkstVPILEv.EkstVPILevNr = 910) THEN
        DO:
            CREATE EkstVPILev.
            ASSIGN
                EkstVPILev.EkstVPILevNr = 910
                EkstVPILev.KortNavn     = "STR indeks"
                EkstVPILev.Navn         = "Størrelsesindeks fra ERP"
                EkstVPILev.AktivLev     = TRUE 
                .
        END.
        IF NOT CAN-FIND(bVPIFilType WHERE 
                        bVPIFiltype.VPIFilTypeNr = 18) THEN 
                        DO:
                            CREATE bVPIFiltype.
                            ASSIGN
                                bVPIFilType.VPIFiltypeNr          = 18
                                bVPIFilType.VPIFiltypeKNavn       = 'STR indeks'
                                bVPIFiltype.VPIFiltypeBeskrivelse = 'Størrelses indeks'. 
                        END.
    
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 910 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 18
            bEkstVPIFil.VPIFilBeskrivelse     = "Størrelsesindeks fra ERP"
            bEkstVPIFil.VPIFilNavn            = "STR"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpistr"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = TRUE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Magento nettbutikk */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 911) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 911
            EkstVPILev.KortNavn     = "Magento"
            EkstVPILev.Navn         = "Magento Nettbutikk"
            EkstVPILev.AktivLev     = FALSE
            .
    END.
    /* Artikkefil fra nettbutikk - settint av webbutikk */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 911 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 911 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "Artikkelinformasjon fra Magento nettbutikk"
            bEkstVPIFil.VPIFilNavn            = "PublishEComProductStatus"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xmagartinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Kundefil fra Magento */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 911 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 911 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 14 /* MED Medlemsinformasjon */
            bEkstVPIFil.VPIFilBeskrivelse     = "Nettkunde fra Magento"
            bEkstVPIFil.VPIFilNavn            = "PublishEComCustomer"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xinnMAGCustomer"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Ordrefil fra Magento */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 911 AND
                    bEkstVPIFil.VPIFilNr     = 3) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 911 
            bEkstVPIFil.VPIFilNr              = 3
            bEkstVPIFil.VPIFilType            = 15 /* KORD Kundeordreinformasjon */
            bEkstVPIFil.VPIFilBeskrivelse     = "Kundeordre fra Magento"
            bEkstVPIFil.VPIFilNavn            = "ProcessEComSalesOrder"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xinnMAGOrder"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* Returforespørsel fra Magento */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 911 AND
                    bEkstVPIFil.VPIFilNr     = 4) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 911 
            bEkstVPIFil.VPIFilNr              = 4
            bEkstVPIFil.VPIFilType            = 15 /* Returforespørsel */
            bEkstVPIFil.VPIFilBeskrivelse     = "Retur fra Magento"
            bEkstVPIFil.VPIFilNavn            = "ProcessEComReturnRequest"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xinnMAGRetur"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* PRICAT VPI fil fra Boomerang */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 912) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 912
            EkstVPILev.KortNavn     = "VPI Pricat"
            EkstVPILev.Navn         = "VPI Pricat Boomerang"
            EkstVPILev.AktivLev     = FALSE
            EkstVPILev.LevNr        = 100
            .
    END.
    /* PRICAT VPI fil fra T.Fjellan (MxSport) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 912 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 912 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "Pricat Boomerang til PRICAT PRS"
            bEkstVPIFil.VPIFilNavn            = "BOART"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xboomvpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

    /* PRICAT import */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 914) THEN
    DO:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 914
            EkstVPILev.KortNavn     = "PRICAT VPI"
            EkstVPILev.Navn         = "PRICAT VPI import"
            EkstVPILev.AktivLev     = FALSE
            .
    END.

    /* VPI - innlesning av konvertert vpi fil (Utvidet pricat format) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 914 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 914 
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "PRICAT VPI import"
            bEkstVPIFil.VPIFilNavn            = "VPI914"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = "c:\home\lindbak\ankommet"
            bEkstVPIFil.VPIInnlesningsrutine  = "xsport1vpiinnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xsport1vpiutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.

END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


&IF DEFINED(EXCLUDE-setVPIImport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIImport Procedure 
PROCEDURE setVPIImport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT NO-UNDO.
DEF VAR piLoop2 AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.
DEF BUFFER bSysHode   FOR SysHode.

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 3) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 3
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Tillat <Blank> EAN kode"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 4) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 4
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Generer EAN hvis <Blank> EAN kode"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 5) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 5
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Opprett mapping hvis den ikke finnes"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 6) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 6
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Tillatt ugyldige tegn i EAN"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 7) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 7
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Opprett ukjente salgsenheter"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 8) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 8
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Vis butikker som VPI leverandører"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 9) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 9
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Opprett ukjente butikker (og VPI leverandør)"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 10) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 10
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Opprett ukjente størrelser"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 11) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 11
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Behold lokal artikkelinformasjon"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 12) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 12
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Sjekk levnr + bestillingsnr"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja. NB: Bare relevant for servicehandel"
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 14) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 14
          bSysPara.Parameter1   = "c:\home\lindbak\ankommet"
          bSysPara.Beskrivelse  = "Importkatalog Exce2Rigal"
          bSysPara.Hjelpetekst1 = "Importkatalog som programmet Excel to Rigal bruker"
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 15) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 15
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Bruk EAN som art.nr på nye artikler"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 16) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 16
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Importer/overfør bedriftsinterne EAN koder"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 17) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 17
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Opprette nye importerte varer som lagerstyrte"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 18) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 18
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Distribuer IPS filer til butikk"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja"
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 19) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 19
          bSysPara.Parameter1   = "c:\home\lindbak\sendes"
          bSysPara.Beskrivelse  = "Eksportkatalog for IPS filer"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 20) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 20
          bSysPara.Parameter1   = '0'
          bSysPara.Beskrivelse  = "Setting av leverandørnr ved import"
          bSysPara.Hjelpetekst1 = "0-Bruk importert levnr, 1-Bruk EkstVPILev.LevNr" 
          bSysPara.Hjelpetekst2 = "Styrer om leverandørnr skal hentes fra EkstVPILev.LevNr"
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 21) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 21
          bSysPara.Parameter1   = '0'
          bSysPara.Beskrivelse  = "Konvertere pant LinkVareNr fra EAN til artikkelnr"
          bSysPara.Hjelpetekst1 = "0-Bruk importert linknr, 1-Konverter linknr til artikkelnr" 
          bSysPara.Hjelpetekst2 = "Styrer om importert linkvarenr skal konverteres til et artikkelnr"
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 22) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 22
          bSysPara.Parameter1   = '0'
          bSysPara.Beskrivelse  = "Tildel levnr+ERPnr som artikkelnr "
          bSysPara.Hjelpetekst1 = "0-Bruk standard, 1-Overstyr artikkelnr" 
          bSysPara.Hjelpetekst2 = "Er bruk av EAN som artikkelnr også satt, har bruk av EAN priorritet."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 23) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 23
          bSysPara.Parameter1   = '0'
          bSysPara.Beskrivelse  = "Opprett nye artikler automatisk"
          bSysPara.Hjelpetekst1 = "0-Ikke opprett, 1-Opprett" 
          bSysPara.Hjelpetekst2 = "Styrer om nye artikkelnr. som importeres skal opprettes automatisk i artikkelregisteret."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 24) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 24
          bSysPara.Parameter1   = '0'
          bSysPara.Beskrivelse  = "Kopier nye artikkler til VPI lomme"
          bSysPara.Hjelpetekst1 = "Angi EkstVPILevn på den VPI lomme som skal ha de nye artikklene." 
          bSysPara.Hjelpetekst2 = "Kopiering skal bare skje når det leses inn artikler i butikkenes VPI lomme."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 25) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 25
          bSysPara.Parameter1   = ''
          bSysPara.Beskrivelse  = "Default varegruppe ved ukjent varegruppe"
          bSysPara.Hjelpetekst1 = "Hvis artikler som importeres har ukjent varegruppe, skal denne varegruppen benyttes." 
          bSysPara.Hjelpetekst2 = "Er det ikke lagt inn default varegruppe, avvises slike artikler i importen."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 26) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 26
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = '900000,900199'
          bSysPara.Beskrivelse  = "Frigjør ean koder fra slaskartikler"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = "Hvis slaskartikler har vært benyttet til å postere ukjene ean, skal disse frigjøres ved VPI import."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 27) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 27
          bSysPara.Parameter1   = '1'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Opprett ukjente varemerker"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = "Oppretter ukjente varemerker ved import av VPI."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 50) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 50
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Opprett ukjente farger"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = "Oppretter ukjente farger ved import av VPI."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 51) THEN DO:
      CREATE SysPara.
      ASSIGN  
          SysPara.SysHId       = 50 
          SysPara.SysGr        = 15 
          SysPara.ParaNr       = 51
          SysPara.Parameter1   = '900,999'
          SysPara.Parameter2   = ''
          SysPara.Beskrivelse  = "Standard strtype range"
          SysPara.Hjelpetekst1 = "Normalt 900,999" 
          SysPara.Hjelpetekst2 = "Fra/Til strtypeid på standard strtyper."
          .
      RELEASE SysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 28) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 28
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Opprett ukjente produsenter"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = "Oppretter ukjente produsenter ved import av VPI."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 29) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 29
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Oppdater priskø automatisk for IPS"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = "Klargjøring av priskøposter blir kjørt automatisk."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 30) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 30
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Oppdater også utpris i priskø (IPS)?"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = "Overstyrer også butikkens lokale utpris i priskøen."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 31) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 31
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = 'tomn@polygonsoftware.no'
          bSysPara.Beskrivelse  = "Send err.logg via eMail til systemansvarlig"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 32) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 32
          bSysPara.Parameter1   = ''
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Default leverandør ved ukjent leverandør"
          bSysPara.Hjelpetekst1 = "Leverandørn som benyttes hvis det er angitt et ukjent leverandørnr" 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 33) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 33
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Oppdater artikkel hvis ant. i lev.forpakn. er mindre eller lik"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja. Det er den artikkel som har laves ant. i lev.forpakn som skal brukes." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 34) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 34
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Opprett ELogg for alle poster i IPS fil"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 35) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 35
          bSysPara.Parameter1   = '15'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Tillatt db avvik i %"
          bSysPara.Hjelpetekst1 = "Er avviket større enn angitt, vil varelinjen bli logget og rapportert." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 36) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 36
          bSysPara.Parameter1   = ''
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Sperreliste VPILev og varegruppe"
          bSysPara.Hjelpetekst1 = "Kommaseparert liste med VPILev i paraNr1, og komma separert liste md Vg i paranr 2." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 37) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 37
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Opprett varegruppe hvis den ikke finnes."
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja" 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 38) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 38
          bSysPara.Parameter1   = ''
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Default vareområde nye artikler."
          bSysPara.Hjelpetekst1 = "La feltet stå blankt hvis det ikke er noe default vareområde." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 39) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 39
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Undertrykk utskrift av feillogger."
          bSysPara.Hjelpetekst1 = "0-Utskrift,1-Ingen utskrift (Notepadfiler)." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 40) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 40
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Utskrift av meldingslogg mottatte varer."
          bSysPara.Hjelpetekst1 = "0-Utskrift,1-Ingen utskrift (Notepadfiler)." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 41) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 41
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Opprett sesongkoder"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 42) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 42
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "La priskø post likke ubehandlet for nye artikler i IPS fil"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.
    
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 43) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 43
          bSysPara.Parameter1   = '1'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Sjekk om artikkel finnes med LevKod, Beskr og LevFargKod"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.
    
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 44) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 44
          bSysPara.Parameter1   = '1'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Tildel løpenr på nye artikler"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 45) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 45
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Legg til størrelser i størrelsestypen"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 46) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 46
          bSysPara.Parameter1   = 'NOK'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Default valuta"
          bSysPara.Hjelpetekst1 = "Legg inn 3 bokstavs valutakode" 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  

  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 47) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 47
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Slett behandlede poster etter import"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja Poster som blir automatisk behandlet, slettes hvis de har fått status 90." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.
    
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 48) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 48
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Logg prisendringer"
          bSysPara.Hjelpetekst1 = "0-Nei, 1-Ja." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.
  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 15 AND
      syspara.paranr = 49) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 15 
          bSysPara.ParaNr       = 49
          bSysPara.Parameter1   = '0'
          bSysPara.Parameter2   = ''
          bSysPara.Beskrivelse  = "Etikett"
          bSysPara.Hjelpetekst1 = "0-Ingen, 1-Pr.Str, 2-Hylle." 
          bSysPara.Hjelpetekst2 = " "
          .
      RELEASE bSysPara.
  END.  
    
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 17) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 17
        bSysGruppe.Beskrivelse = "VPI oppsett Excel import"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 17 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 17 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "889"
          bSysPara.Beskrivelse  = "Komma separert liste med VPI leverandører"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 17 AND
      syspara.paranr = 2) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 17 
          bSysPara.ParaNr       = 2
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Dupliser IPS VPI record til HK"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
END. /* TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 18) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 18
        bSysGruppe.Beskrivelse = "VPI oppsett EDI import"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 18 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 18 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "907"
          bSysPara.Beskrivelse  = "VPI leverandørnr HK's VPI lomme"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
END. /* TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 19) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 19
        bSysGruppe.Beskrivelse = "VPI oppsett ERP import"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 19 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 19 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "903"
          bSysPara.Beskrivelse  = "VPI leverandørnr HK's VPI lomme"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
END. /* TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 22) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 22
        bSysGruppe.Beskrivelse = "VPI oppsett T.Fjeland (MxSport)"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 22 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 22 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "101"
          bSysPara.Beskrivelse  = "VPI leverandørnr. som tildeles konvertert fil"
          bSysPara.Hjelpetekst1 = "VPI filen fra T.Fjelland konverteres til vanlig pricat fil på angitt EkstVPILevNr."
          .
      RELEASE bSysPara.
  END.  
END. /* TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 23) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 23
        bSysGruppe.Beskrivelse = "VPI oppsett NæraDej RVPI (Excel) import"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 23 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 23 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "Ja"
          bSysPara.Beskrivelse  = "Legg ut RIGAL filer til butikkene"
          bSysPara.Hjelpetekst1 = "Ved innlesning av RVPI fil. legges det ut RIGAL fil til butikkene."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 23 AND
      syspara.paranr = 2) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 23 
          bSysPara.ParaNr       = 2
          bSysPara.Parameter1   = "Ja"
          bSysPara.Beskrivelse  = "Importer RVPI filene i VPI databasen"
          bSysPara.Hjelpetekst1 = "Ved innlesning av RVPI fil. Importeres også artiklene i VPI databasen."
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 23 AND
      syspara.paranr = 3) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 23 
          bSysPara.ParaNr       = 3
          bSysPara.Parameter1   = "c:\home\lindbak\sendes"
          bSysPara.Beskrivelse  = "Eksport katalog for RIGAL filer"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 23 AND
      syspara.paranr = 4) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 23 
          bSysPara.ParaNr       = 4
          bSysPara.Parameter1   = "c:\home\lindbak\ankommet"
          bSysPara.Beskrivelse  = "Import katalog for RIGAL filer (Til VPI registeret)"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 23 AND
      syspara.paranr = 5) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 23 
          bSysPara.ParaNr       = 5
          bSysPara.Parameter1   = "c:\home\lindbak\ankommet"
          bSysPara.Beskrivelse  = "Katalog hvor RIGAL sekvensfil legges."
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
END. /* TRANSACTION */

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 24) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 24
        bSysGruppe.Beskrivelse = "VPI oppsett Boomerang VPIimport"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 24 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 24 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "c:\home\lindbak\ankommet"
          bSysPara.Beskrivelse  = "Eksport katalog VPI filer"
          bSysPara.Hjelpetekst1 = "Det leses inn to filer som slås sammen til en pricat fil."
          .
      RELEASE bSysPara.
  END.  
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 25) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 25
        bSysGruppe.Beskrivelse = "Oppsett VPI Mottakskontroll"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Vis filter"
          bSysPara.Hjelpetekst1 = "0-Ikke vis, 1-Vis."
          .
      RELEASE bSysPara.
  END.  
END.
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 2) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 2
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Vis varebok"
          bSysPara.Hjelpetekst1 = "0-Ikke vis, 1-Vis."
          .
      RELEASE bSysPara.
  END.  
END.
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 3) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 3
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Vis enkel toolbar"
          bSysPara.Hjelpetekst1 = "0-Ikke vis, 1-Vis."
          .
      RELEASE bSysPara.
  END.  
END.
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 4) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 4
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Vis bare VPI datasett som har VPI"
          bSysPara.Hjelpetekst1 = "0-Ikke vis, 1-Vis."
          .
      RELEASE bSysPara.
  END.  
END.
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 5) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 5
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Initialverdi på behandlingsstatus"
          bSysPara.Hjelpetekst1 = "<Blank>-Alle,0-Alle under 90,Eller annen aktuell status"
          .
      RELEASE bSysPara.
  END.  
END.
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 6) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 6
          bSysPara.Parameter1   = "0"
          bSysPara.Beskrivelse  = "Undertrykk filter på artikkelinfo ved overføring til priskø"
          bSysPara.Hjelpetekst1 = "0-Vis filter,1-Undertrykk filter"
          .
      RELEASE bSysPara.
  END.  
END.
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 25 AND
      syspara.paranr = 7) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 25 
          bSysPara.ParaNr       = 7
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Avgrens på vareområde"
          bSysPara.Hjelpetekst1 = "0-Nei,1-Ja"
          .
      RELEASE bSysPara.
  END.  
END.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 26) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId      = 50
        bSysGruppe.SysGr       = 26
        bSysGruppe.Beskrivelse = "VPI oppsett Pakkseddel import"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 26 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 26 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Vis eventuell feilmelding ved overføring av artikkel til varebok"
          bSysPara.Hjelpetekst1 = ""
          .
      RELEASE bSysPara.
  END.  
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVPIMottakFraHK) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIMottakFraHK Procedure 
PROCEDURE setVPIMottakFraHK :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop  AS INT NO-UNDO.
DEF VAR piLoop2 AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.
DEF BUFFER bSysHode   FOR SysHode.


IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 50 AND
    SysGruppe.SysGr  = 16) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 50
        bSysGruppe.SysGr  = 16
        Beskrivelse       = "VPI mottak fra HK"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

/* Skal innprisendringer oppdateres direkte? */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 16 AND
      syspara.paranr = 1) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 16 
          bSysPara.ParaNr       = 1
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Oppdater innprisendring fra hk direkte"
          bSysPara.Hjelpetekst1 = "0-Ingen,1-Direkte"
          .
      RELEASE bSysPara.
  END.
END.
/* Skal artikkelinformasjon oppdateres direkte? */
DO TRANSACTION:
  IF NOT CAN-FIND(syspara WHERE
      syspara.syshid = 50 AND
      syspara.sysgr  = 16 AND
      syspara.paranr = 2) THEN DO:
      CREATE bSysPara.
      ASSIGN  
          bSysPara.SysHId       = 50 
          bSysPara.SysGr        = 16 
          bSysPara.ParaNr       = 2
          bSysPara.Parameter1   = "1"
          bSysPara.Beskrivelse  = "Oppdater art.info fra hk direkte"
          bSysPara.Hjelpetekst1 = "0-Ingen,1-Direkte"
          .
      RELEASE bSysPara.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVPIMottakStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIMottakStatus Procedure 
PROCEDURE setVPIMottakStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bSysGruppe FOR SysGruppe.
    DEF BUFFER bSysPara   FOR SysPara.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 2 AND
            SysGruppe.SysGr  = 10) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 2
                bSysGruppe.SysGr  = 10
                bSysGruppe.Beskrivelse      = "VPI mottak status"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 10 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 10 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "Ubehandlet"
                bSysPara.Parameter1  = "Ubehandlet".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 10 AND
                        bSysPara.ParaNr = 10) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 10 
                bSysPara.ParaNr      = 10
                bSysPara.Beskrivelse = "Kontrolleres"
                bSysPara.Parameter1  = "Kontrolleres".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 10 AND
                        bSysPara.ParaNr = 20) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 10 
                bSysPara.ParaNr      = 20
                bSysPara.Beskrivelse = "Endret"
                bSysPara.Parameter1  = "Endret".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 10 AND
                        bSysPara.ParaNr = 80) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 10 
                bSysPara.ParaNr      = 80
                bSysPara.Beskrivelse = "Avvist"
                bSysPara.Parameter1  = "Avvist".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 10 AND
                        bSysPara.ParaNr = 90) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 10 
                bSysPara.ParaNr      = 90
                bSysPara.Beskrivelse = "Behandlet"
                bSysPara.Parameter1  = "Behandlet".
            RELEASE bSysPara.
        END.
    END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-setVPIMottakTyper) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setVPIMottakTyper Procedure 
PROCEDURE setVPIMottakTyper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bSysGruppe FOR SysGruppe.
    DEF BUFFER bSysPara   FOR SysPara.

    DO TRANSACTION:
        IF NOT CAN-FIND(SysGruppe WHERE
            SysGruppe.SysHId = 2 AND
            SysGruppe.SysGr  = 9) THEN
        DO:
            CREATE bSysGruppe.
            ASSIGN
                bSysGruppe.SysHId = 2
                bSysGruppe.SysGr  = 9
                bSysGruppe.Beskrivelse      = "VPI mottak typer"
                .
            RELEASE bSysGruppe.
        END. /* bSysGruppe TRANSACTION */

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 9 AND
                        bSysPara.ParaNr = 1) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 9 
                bSysPara.ParaNr      = 1
                bSysPara.Beskrivelse = "VPI"
                bSysPara.Parameter1  = "VPI".
            RELEASE bSysPara.
        END.


        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 9 AND
                        bSysPara.ParaNr = 2) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 9 
                bSysPara.ParaNr      = 2
                bSysPara.Beskrivelse = "Korr. VPI"
                bSysPara.Parameter1  = "Korr. VPI".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 9 AND
                        bSysPara.ParaNr = 3) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 9 
                bSysPara.ParaNr      = 3
                bSysPara.Beskrivelse = "Ordre VPI"
                bSysPara.Parameter1  = "Ordre VPI".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 9 AND
                        bSysPara.ParaNr = 4) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 9 
                bSysPara.ParaNr      = 4
                bSysPara.Beskrivelse = "Vare VPI"
                bSysPara.Parameter1  = "Vare VPI".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 9 AND
                        bSysPara.ParaNr = 5) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 9 
                bSysPara.ParaNr      = 5
                bSysPara.Beskrivelse = "Kampanje VPI"
                bSysPara.Parameter1  = "Kampanje VPI".
            RELEASE bSysPara.
        END.

        IF NOT CAN-FIND(bSysPara WHERE
                        bSysPara.SysHId = 2 AND
                        bSysPara.SysGr  = 9 AND
                        bSysPara.ParaNr = 6) THEN DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 2
                bSysPara.SysGr       = 9 
                bSysPara.ParaNr      = 6
                bSysPara.Beskrivelse = "PDA VPI"
                bSysPara.Parameter1  = "PDA VPI".
            RELEASE bSysPara.
        END.
    END. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
 
&IF DEFINED(EXCLUDE-setXmlVPIImport) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setXmlVPIImport Procedure
PROCEDURE setXmlVPIImport:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	DEFINE VARIABLE cImportKatalog AS CHARACTER NO-UNDO.
	  
    {syspara.i 1 1 52 cImportKatalog}
    IF cImportKatalog = '' THEN 
      cImportKatalog = '.\kom\in'.
	
    /* Opprettelse av VPI leveranødør */
    IF NOT CAN-FIND(EkstVPILev WHERE
                    EkstVPILEv.EkstVPILevNr = 916) THEN
    DO TRANSACTION:
        CREATE EkstVPILev.
        ASSIGN
            EkstVPILev.EkstVPILevNr = 916
            EkstVPILev.KortNavn     = "XML Vpi Imp. Ax"
            EkstVPILev.Navn         = "XML Vpi Import Axfood"
            EkstVPILev.AktivLev     = FALSE
            .
        RELEASE EkstVPILev.
    END. /* TRANSACTION */

    /* Opprettelse av import for PRS Pricat */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 916 AND
                    bEkstVPIFil.VPIFilNr     = 1) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 916 
            bEkstVPIFil.VPIFilNr              = 1
            bEkstVPIFil.VPIFilType            = 2
            bEkstVPIFil.VPIFilBeskrivelse     = "XML Vpi Import Axfood"
            bEkstVPIFil.VPIFilNavn            = "TradeItemPrice"
            bEkstVPIFil.VPIEkst               = "xml"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xinnTradeItemPrice"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xstdutpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END. /* TRANSACTION */

    /* VPI - innlesning av konvertert vpi fil (Utvidet pricat format) */
    IF NOT CAN-FIND(bEkstVPIFil WHERE
                    bEkstVPIFil.EkstVPILevNr = 916 AND
                    bEkstVPIFil.VPIFilNr     = 2) THEN
    DO TRANSACTION:
        CREATE bEkstVPIFil.
        ASSIGN
            bEkstVPIFil.EkstVPILEvNr          = 916
            bEkstVPIFil.VPIFilNr              = 2
            bEkstVPIFil.VPIFilType            = 1
            bEkstVPIFil.VPIFilBeskrivelse     = "Pricat fra Axfood konvertert fra XML"
            bEkstVPIFil.VPIFilNavn            = "GVPI916"
            bEkstVPIFil.VPIEkst               = "csv"
            bEkstVPIFil.VPIKatalog            = cImportKatalog
            bEkstVPIFil.VPIInnlesningsrutine  = "xPRSPricatInnles"
            bEkstVPIFil.VPIUtpakkingsrutine   = "xPRSPricatUtpakk"
            bEkstVPIFil.VPIOperator           = 2
            bEkstVPIFil.VPIFilAktiv           = FALSE
            .
        RELEASE bEkstVPIFil.
    END.
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-sjekk_og_fiks_strtypeid_0) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekk_og_fiks_strtypeid_0 Procedure
PROCEDURE sjekk_og_fiks_strtypeid_0:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
  ArtBas.StrTypeId = 0:
  RUN bibl_opprettStrtypeForModell.p (ArtBas.ArtikkelNr, OUTPUT ArtBas.StrTypeID).
END.
FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
  NOT CAN-FIND(StrType OF ArtBAs):
  RUN bibl_opprettStrtypeForModell.p (ArtBas.ArtikkelNr, OUTPUT ArtBas.StrTypeID).
END.
FIND FIRST ArtBas NO-LOCK NO-ERROR.

FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE 
  VPIArtBas.StrTypeId = 0:
  RUN bibl_opprettVPIStrtypeForModell.p (VPIArtBas.ArtikkelNr, VPIArtBas.EkstVPILevNr,OUTPUT VPIArtBas.StrTypeID).
END.
FOR EACH VPIArtBas EXCLUSIVE-LOCK WHERE 
  NOT CAN-FIND(StrType OF VPIArtBas):
  RUN bibl_opprettVPIStrtypeForModell.p (VPIArtBas.ArtikkelNr, VPIArtBas.EkstVPILevNr,OUTPUT VPIArtBas.StrTypeID).
END.
FIND FIRST VPIArtBas NO-LOCK NO-ERROR.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-SjekkBatchStatus) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkBatchStatus Procedure 
PROCEDURE SjekkBatchStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufBatchLogg FOR BatchLogg.

  IF CAN-FIND(FIRST BatchLogg WHERE
              BatchLogg.OppdStatus = 0) THEN
  DO:
      FOR EACH BatchLogg NO-LOCK WHERE
          BatchLogg.OppdStatus = 0:

          /* Ikke dagens */
          IF BatchLogg.RegistrertDato < TODAY THEN
          DO TRANSACTION:
              FIND bufBatchLogg EXCLUSIVE-LOCK WHERE
                  RECID(bufBatchLogg) = RECID(BatchLogg).

              IF NOT CAN-FIND(FIRST Translogg OF bufBatchLogg) THEN
                  bufBatchLogg.OppdStatus = 4.
              ELSE
                  bufBatchLogg.OppdStatus = 3.
              RELEASE bufBatchLogg.
          END.

      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkFaktura) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkFaktura Procedure 
PROCEDURE sjekkFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Bug Hider. Pga. feil i ver. 4.2p6.      
------------------------------------------------------------------------------*/
      FOR EACH FakturaHode WHERE 
        FakturaHode.Bilagstype = 1 AND
        FakturaHode.BTTekst    = "Kreditnota":
        ASSIGN
            FakturaHode.Bilagstype = 2
            .
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-slettGamleSyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slettGamleSyspara Procedure 
PROCEDURE slettGamleSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR cGrpLst AS CHAR NO-UNDO.
DEF VAR piLoop  AS INT  NO-UNDO.

ASSIGN
    cGrpLst = '250,301,401,501,601,701,801,901,1001'
    .
DO piLoop = 1 TO NUM-ENTRIES(cGrpLst):
    FOR EACH SysGruppe EXCLUSIVE-LOCK WHERE
        SysGruppe.SysHId = 6 AND
        SysGruppe.SysGr = INT(ENTRY(piLoop,cGrpLst)):

        FOR EACH sysPara EXCLUSIVE-LOCK WHERE
            SysPara.SysHId = 6 AND
            SysPara.SysGr  = SysGruppe.SysGr:
            DELETE SysPara.
        END.
        DELETE SysGruppe.
    END.
    FOR EACH SysGruppe EXCLUSIVE-LOCK WHERE
        SysGruppe.SysHId = 9 AND
        SysGruppe.SysGr = INT(ENTRY(piLoop,cGrpLst)):

        FOR EACH sysPara EXCLUSIVE-LOCK WHERE
            SysPara.SysHId = 9 AND
            SysPara.SysGr  = SysGruppe.SysGr:
            DELETE SysPara.
        END.
        DELETE SysGruppe.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-slettJBoxUserSetting) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slettJBoxUserSetting Procedure 
PROCEDURE slettJBoxUserSetting :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Tar bort brukerinstillinger for pakkseddel. */
FOR EACH JBoxUserSetting EXCLUSIVE-LOCK
         WHERE JBoxUserSetting.cSourceFile BEGINS "pksdl":
  DELETE JBoxUserSetting.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-slettStrKonv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slettStrKonv Procedure 
PROCEDURE slettStrKonv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH StrKonv WHERE
    StrKonv.Merknad BEGINS "Opprettet":

    DELETE StrKonv.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysParaBudsjett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysParaBudsjett Procedure
PROCEDURE SysParaBudsjett:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR piLoop AS INT NO-UNDO.

    DEFINE BUFFER bSysPara   FOR SysPara.
    DEFINE BUFFER bSysGruppe FOR SysGruppe.
    DEFINE BUFFER bSysHode   FOR SysHode.
    
    IF NOT CAN-FIND(SysHode WHERE
        SysHode.SysHId = 23) THEN
    DO TRANSACTION:
        CREATE bSysHode.
        ASSIGN
            bSysHode.SysHId = 23
            Beskrivelse     = "Budsjett"
            .
        RELEASE bSysHode.
    END. /* bSysGruppe TRANSACTION */
    

    IF NOT CAN-FIND(SysGruppe WHERE
        SysGruppe.SysHId = 23 AND
        SysGruppe.SysGr  = 1) THEN
    DO FOR bSysGruppe TRANSACTION:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 23
            bSysGruppe.SysGr  = 1
            Beskrivelse       = "Kalender"
            .
        RELEASE bSysGruppe.
    END. /* bSysGruppe TRANSACTION */

    /*IF NOT CAN-FIND(FIRST SysPara where
        SysPara.SysHId = 2 and
        SysPara.SysGr  = 7) THEN */
    DO FOR bSysPara TRANSACTION:
        LOOP:
        DO piLoop = 1 TO 2:
            FIND bSysPara WHERE
                bSysPara.SysHId = 23 AND
                bSysPara.SysGr  = 1 AND
                bSysPara.ParaNr = piLoop EXCLUSIVE-LOCK NO-ERROR.
            IF  NOT AVAILABLE bSysPara THEN
            DO:
                CREATE bSysPara.
                ASSIGN  
                    bSysPara.SysHId      = 23 
                    bSysPara.SysGr       = 1 
                    bSysPara.ParaNr      = piLoop
                    bSysPara.Parameter1  = IF piLoop = 1 
                                              THEN "JAN,FEB,MAR,APR,MAI,JUN,JUL,AUG,SEP,OKT,NOV,DES"
                                           ELSE "SØN,MAN,TIR,ONS,TOR,FRE,LØR"
                    bSysPara.Beskrivelse = "Måneder"
                    .
                RELEASE bSysPara.
            END.
        END. /* LOOP */
    END. /* bSysPara TRANSACTION*/


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-SysParaOverforingsbilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysParaOverforingsbilag Procedure 
PROCEDURE SysParaOverforingsbilag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 6 AND
    SysGruppe.SysGr  = 1101) THEN
DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
        bSysGruppe.SysHId = 6
        bSysGruppe.SysGr  = 1101
        Beskrivelse      = "Overføringsbilag"
        .
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

/*IF NOT CAN-FIND(FIRST SysPara where
    SysPara.SysHId = 2 and
    SysPara.SysGr  = 7) THEN */
DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 1:
        FIND bSysPara WHERE
            bSysPara.SysHId = 6 AND
            bSysPara.SysGr  = 1101 AND
            bSysPara.ParaNr = piLoop EXCLUSIVE-LOCK NO-ERROR.
        IF  NOT AVAILABLE bSysPara THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 6 
                bSysPara.SysGr       = 1101 
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = "PLUKKLISTE,MOTTAKSLISTE,FØLGESEDDEL,STØRRELSESENDRING,AVDELINGSREGNSKAP"
                bSysPara.Beskrivelse = "Siderubrikker"
                .
            RELEASE bSysPara.
        END.
        ELSE DO:
            ASSIGN  
                bSysPara.Parameter1  = "PLUKKLISTE,MOTTAKSLISTE,FØLGESEDDEL,STØRRELSESENDRING,AVDELINGSREGNSKAP"
                bSysPara.Beskrivelse = "Siderubrikker"
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysParaSuppleringsOrdre) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysParaSuppleringsOrdre Procedure 
PROCEDURE SysParaSuppleringsOrdre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

DO FOR bSysGruppe TRANSACTION:
    FIND bSysGruppe EXCLUSIVE-LOCK WHERE
         bSysGruppe.SysHId = 21 AND
         bSysGruppe.SysGr  = 100 NO-ERROR.
    IF NOT AVAILABLE bSysGruppe THEN
    DO:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 21
            bSysGruppe.SysGr  = 100
            Beskrivelse       = "Bestillingsstatus suppleringsordre"
            .
    END.
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysGruppe TRANSACTION:
    FIND bSysGruppe EXCLUSIVE-LOCK WHERE
         bSysGruppe.SysHId = 21 AND
         bSysGruppe.SysGr  = 101 NO-ERROR.
    IF NOT AVAILABLE bSysGruppe THEN
    DO:
        CREATE bSysGruppe.
        ASSIGN
            bSysGruppe.SysHId = 21
            bSysGruppe.SysGr  = 101
            Beskrivelse       = "Søk best.status suppl.ordre"
            .
    END.
    RELEASE bSysGruppe.
END. /* bSysGruppe TRANSACTION */

DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 3:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 21 AND
            bSysPara.SysGr  = 100 AND
            bSysPara.ParaNr = int(ENTRY(piLoop,"3,4,44"))) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 100
                bSysPara.ParaNr      = int(ENTRY(piLoop,"3,4,44"))
                bSysPara.Beskrivelse = ENTRY(piLoop,"På ordre,Ordre sendt,Bekreftet")
                bSysPara.Parameter1  = ENTRY(piLoop,"3,4,44")
                bSysPara.Parameter2  = ENTRY(piLoop,"Ikke sendte ordre,Sendte ordre,Bekreftede ordre")
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */

    /* Oppdaterer posten hvis den skulle ligge med feil verdier. */
    FIND bSysPara WHERE
        bSysPara.SysHId = 21 AND
        bSysPara.SysGr  = 100 AND
        bSysPara.ParaNr = 44 NO-ERROR.
    IF AVAILABLE bSysPara THEN
    DO:
        ASSIGN  
            bSysPara.SysHId      = 21 
            bSysPara.SysGr       = 100
            bSysPara.ParaNr      = 44
            bSysPara.Beskrivelse = "Bekreftet"
            bSysPara.Parameter1  = "44"
            bSysPara.Parameter2  = "Bekreftede ordre"
            .
        RELEASE bSysPara.
    END.

    /* Oppdaterer posten hvis den skulle ligge med feil verdier. */
    FIND bSysPara WHERE
        bSysPara.SysHId = 5 AND
        bSysPara.SysGr  = 24 AND
        bSysPara.ParaNr = 2 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN
    DO:
        CREATE bSysPara.
        ASSIGN  
            bSysPara.SysHId      = 5 
            bSysPara.SysGr       = 24
            bSysPara.ParaNr      = 2
            bSysPara.Beskrivelse = "Infotekst"
            bSysPara.Parameter1  = "Forslaget viser tilgjengelige suppleringsvarer fra S1D"
            bSysPara.Parameter2  = ""
            .
        RELEASE bSysPara.
    END.
END. /* bSysPara TRANSACTION*/

DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 6:
        IF NOT CAN-FIND(FIRST bSysPara WHERE
            bSysPara.SysHId = 21 AND
            bSysPara.SysGr  = 101 AND
            bSysPara.ParaNr = int(ENTRY(piLoop,"3,4,5,6,9,44"))) THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 21 
                bSysPara.SysGr       = 101
                bSysPara.ParaNr      = int(ENTRY(piLoop,"3,4,5,6,9,44"))
                bSysPara.Beskrivelse = ENTRY(piLoop,"På ordre,Ordre sendt,Delhvis levert,Levert,Avskrevet,Bekreftet")
                bSysPara.Parameter1  = ENTRY(piLoop,"03,04,05,06,09,44")
                bSysPara.Parameter2  = ENTRY(piLoop,"Ikke sendte ordre,Sendte ordre,Delhvis levert ordre,Leverte ordre,Avskrevet,Bekreftede ordre")
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysParaTilfeldigVare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysParaTilfeldigVare Procedure 
PROCEDURE SysParaTilfeldigVare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

/*IF NOT CAN-FIND(FIRST SysPara where
    SysPara.SysHId = 2 and
    SysPara.SysGr  = 7) THEN */
DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 2 TO 2:
        FIND bSysPara WHERE
            bSysPara.SysHId = 19 AND
            bSysPara.SysGr  = 100 AND
            bSysPara.ParaNr = piLoop EXCLUSIVE-LOCK NO-ERROR.
        IF  NOT AVAILABLE bSysPara THEN
        DO:
            CREATE bSysPara.
            ASSIGN  
                bSysPara.SysHId      = 19 
                bSysPara.SysGr       = 100 
                bSysPara.ParaNr      = piLoop
                bSysPara.Parameter1  = "9012"
                bSysPara.Beskrivelse = "EAN kode tilfeldig vare"
                .
            RELEASE bSysPara.
        END.
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SysParaVareSlag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SysParaVareSlag Procedure 
PROCEDURE SysParaVareSlag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR piLoop AS INT NO-UNDO.

DEF BUFFER bSysPara   FOR SysPara.
DEF BUFFER bSysGruppe FOR SysGruppe.

IF NOT CAN-FIND(SysGruppe WHERE
    SysGruppe.SysHId = 2 AND
    SysGruppe.SysGr  = 7) THEN
  DO FOR bSysGruppe TRANSACTION:
    CREATE bSysGruppe.
    ASSIGN
      bSysGruppe.SysHId = 2
      bSysGruppe.SysGr  = 7
      Beskrivelse       = "Vareslag"
      .
    RELEASE bSysGruppe.
  END. /* bSysGruppe TRANSACTION */

  /*IF NOT CAN-FIND(FIRST SysPara where
      SysPara.SysHId = 2 and
      SysPara.SysGr  = 7) THEN */
  DO FOR bSysPara TRANSACTION:
    LOOP:
    DO piLoop = 1 TO 11:
      FIND bSysPara WHERE
        bSysPara.SysHId = 2 AND
        bSysPara.SysGr  = 7 AND
        bSysPara.ParaNr = piLoop EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bSysPara THEN DELETE bSysPara.
      IF NOT AVAILABLE bSyspara THEN 
      DO:
        CREATE bSysPara.
        ASSIGN  
          bSysPara.SysHId = 2 
          bSysPara.SysGr  = 7 
          bSysPara.ParaNr = piLoop
          .
      END.
      ASSIGN
      bSysPara.Parameter1  = STRING(piLoop - 1)
                /*
                bSysPara.Beskrivelse = (IF piLoop = 1      THEN "Lager m/str"
                                        ELSE IF piLoop = 2 THEN "Lager u/str"
                                        ELSE IF piLoop = 3 THEN "U/lager m/str"
                                        ELSE IF piLoop = 4 THEN "U/lager u/str"
                                        ELSE IF piLoop = 5 THEN "PLU"
                                        ELSE IF piLoop = 6 THEN "Pakkevare"
                                        ELSE IF piLoop = 7 THEN "Pant m/lager"
                                        ELSE IF piLoop = 8 THEN "Pant m/lager"
                                        ELSE                    "Pant m/lager")
                */
      bSysPara.Beskrivelse = (IF piLoop      =  1 THEN "Stykkvare (stk)"
                              ELSE IF piLoop =  2 THEN "Vektvare (kg)"
                              ELSE IF piLoop =  3 THEN "Vektvare (hg)"
                              ELSE IF piLoop =  4 THEN "Metervare (m)"
                              ELSE IF piLoop =  5 THEN "Kvadratmeter (m2)"
                              ELSE IF piLoop =  6 THEN "Volumvare (l)"
                              ELSE IF piLoop =  7 THEN "Åpen pris"
                              ELSE IF piLoop =  8 THEN "Pakkevare"
                              ELSE IF piLoop =  9 THEN "Pant" 
                              ELSE IF piLoop = 10 THEN "Non-Sale" 
                              ELSE IF piLoop = 11 THEN "Non-Sale neg.vare" 
                              ELSE 'Ukjent vareslag'
                              )
      .
    END. /* LOOP */
END. /* bSysPara TRANSACTION*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

