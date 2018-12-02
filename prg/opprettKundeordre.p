&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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
DEF INPUT PARAMETER dB_Id AS DEC NO-UNDO.
DEF OUTPUT PARAMETER bOk AS LOG NO-UNDO.

DEFINE VARIABLE dKOrdreId LIKE KOrdreHode.KOrdre_Id NO-UNDO.
DEFINE VARIABLE hKOrdreLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE iVgLopNr AS INTEGER NO-UNDO. 

DEFINE BUFFER clButiker FOR Butiker.
DEFINE VARIABLE ocValue AS CHARACTER NO-UNDO.

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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 19 9 2 iVgLopNr INT} 
   
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR. 

FIND BongHode NO-LOCK WHERE
    BongHode.B_Id = dB_Id NO-ERROR.
IF NOT AVAILABLE BongHode THEN
DO:
    bOk = FALSE.
    RETURN 'Ukjent bongreferanse ' + STRING(dB_Id) + '.'.
END.

/* Ordren må legges på Kundeordre kassen */
FIND LAST Kasse NO-LOCK WHERE 
  Kasse.ButikkNr = BongHode.ButikkNR NO-ERROR.

FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
IF NOT AVAILABLE Kunde THEN
DO :
    bOk = FALSE.
    RETURN 'Ukjent kundendenr ' + STRING(BongHode.B_Id) + '.'.
END.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = BongHode.ButikkNR NO-ERROR.
IF NOT AVAILABLE Butiker THEN
DO :
    bOk = FALSE.
    RETURN 'Ukjent butikknr ' + STRING(BongHode.ButikkNr) + '.'.
END.

RUN OpprettHentKOrdreHode.

RUN OpprettKOrdreLinjer.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

 
 
&IF DEFINED(EXCLUDE-KordreLinjeBetaling) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KordreLinjeBetaling Procedure
PROCEDURE KordreLinjeBetaling:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
    
    FIND LAST KOrdrelinje NO-LOCK WHERE
              KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
    IF AVAILABLE KOrdreLinje THEN 
        iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
    ELSE
        iLinjeNr = 1.
    DO TRANSACTION:
        CREATE KOrdreLinje.
        ASSIGN
           KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
           KOrdreLinje.KOrdreLinjeNr = iLinjeNr 
           /* Betaling */
           KOrdreLinje.MomsKod       = 0
           KOrdreLinje.Antall        = BongLinje.Antall
           KOrdreLinje.NettoPris     = BongLinje.LinjeSum
           KOrdreLinje.MvaKr         = 0
           KORdreLinje.Mva%          = 0
           KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
           KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
           KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
           KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
           .          
        CASE BongLinje.TTId:
           WHEN 50 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'KONTANT (50)'
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1.
           WHEN 51 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'BETALINGSKORT (51)'
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1.
           WHEN 52 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'KREDITKORT (52)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 53 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'GAVEKORT (53)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 54 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'SJEKK (54)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 56 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'KUPONG1 (56)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 57 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'PANT (57)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 58 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'BANK (58)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 59 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'DROPP (59)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 61 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'INNBETALT (61)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 62 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'UTBETALT (62)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 65 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'KREDIT (65)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 66 THEN 
                            ASSIGN KORdreLinje.Varetekst = 'TILGODELAPP (66)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           OTHERWISE 
               ASSIGN KORdreLinje.Varetekst = 'Ukjent betalingsmåte'
                      KORdreLinje.VareNr    = 'BETALT'.
         END CASE.         

        RELEASE KOrdreLinje.
    END. /* TRANSACTION */
       
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-KordreLinjeVareSalg) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KordreLinjeVareSalg Procedure
PROCEDURE KordreLinjeVareSalg:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEFINE VARIABLE iLinjeNr AS INTEGER NO-UNDO.
  
    DO TRANSACTION:
    
        FIND ArtBas NO-LOCK WHERE
             ArtBAs.ArtikkelNr = DEC(BongLinje.ArtikkelNr) NO-ERROR.
               
        IF AVAILABLE ArtBas THEN 
          FIND ArtPris OF ArtBas NO-LOCK WHERE
               ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN 
          FIND ArtPris OF ArtBas NO-LOCK WHERE
               ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
        FIND FIRST Moms NO-LOCK WHERE
            Moms.MomsKod = BongLinje.MvaGr NO-ERROR.
        FIND Strekkode NO-LOCK WHERE 
            Strekkode.Kode = BongLinje.Strekkode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            FIND StrKonv NO-LOCK WHERE
                 StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            FIND Lager NO-LOCK WHERE
                 Lager.ArtikkelNr = ArtBas.ArtikkelNR AND 
                 Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
           
        FIND LAST KOrdrelinje NO-LOCK WHERE
                  KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
        IF AVAILABLE KOrdreLinje THEN 
            iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
        ELSE
            iLinjeNr = 1.
           
        CREATE KOrdreLinje.
        ASSIGN
            KOrdreLinje.KOrdre_ID         = KOrdreHode.KOrdre_Id
            KOrdreLinje.KOrdreLinjeNr     = iLinjeNr 
            KOrdreLinje.VareNr            = BongLinje.ArtikkelNr
            KOrdreLinje.Varetekst         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '** Ukjent ' + KOrdreLinje.VareNr)
            KOrdreLinje.Antall            = BongLinje.Antall
            KOrdreLinje.NettoPris         = ROUND(BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall),2)
            KOrdreLinje.NettoLinjesum     = KOrdreLinje.NettoPris * KOrdreLinje.Antall
            KOrdreLinje.LinjeRabattKr     = (BongLinje.LinjeRab + BongLinje.SubtotalRab) / ABSOLUTE(BongLinje.Antall)
            KOrdreLinje.LinjeRabattKr     = IF KOrdreLinje.LinjeRabattKr = ? THEN 0 ELSE KOrdreLinje.LinjeRabattKr
            KOrdreLinje.LinjeRab%         = (KOrdreLinje.LinjeRabattKr / (KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr)) * 100  
            KOrdreLinje.LinjeRab%         = IF KOrdreLinje.LinjeRab% = ? THEN 0 ELSE KOrdreLinje.LinjeRab%  
            KOrdreLinje.MomsKod           = BongLinje.MvaGr
            KOrdreLinje.MvaKr             = DECIMAL(BongLinje.MvaKr / ABSOLUTE(BongLinje.Antall))
            KOrdreLinje.Storl             = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
            KOrdreLinje.Mva%              = IF AVAILABLE Moms THEN Moms.MomsProc ELSE 0
            KOrdreLinje.StrKode           = IF AVAILABLE StrKonv THEN StrKonv.StrKode ELSE 0
            KOrdreLinje.VareKost          = IF AVAILABLE Lager THEN Lager.VVareKost ELSE 0
            KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost
            KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = 0 AND AVAILABLE ArtPris) THEN  ArtPris.VareKost[1] ELSE KOrdreLinje.VareKost
            
            KOrdreLinje.BruttoPris        = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall)  
            KOrdreLinje.Pris              = KOrdreLinje.NettoPris + (KOrdreLinje.LinjeRabattKr / KOrdreLinje.Antall) 
            KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr 
            KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
            KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
            KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%) 
            KOrdreLinje.RefNr             = 0 /*BongLinje.BongNr*/
            KOrdreLinje.RefTekst          = 'Kasse/BongNr: ' + STRING(BongLinje.KasseNr) + '/' + STRING(BongLinje.BongNr) 
            KOrdreLinje.Bestillingsnummer = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
            KOrdreLinje.LevFargKod        = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
            KOrdreLinje.ValKod            = KOrdreHode.ValKod           
           NO-ERROR.
           
           IF AVAILABLE ArtBas THEN 
           DO:
             CASE iVgLopNr:
               WHEN 1 THEN 
                      ASSIGN KOrdreLinje.VareTekst = IF ArtBas.LopNr <> ? 
                                                        THEN STRING(ArtBas.VG) + '-' + STRING(ArtBas.LopNr) 
                                                        ELSE KOrdreLinje.VareTekst.
               WHEN 2 THEN  
                      ASSIGN KOrdreLinje.VareTekst = ArtBas.Beskr.                 
             END CASE. 
           END.
           /* Oppdaterer sumfelt i KOrdreHode. */
           hKOrdreLinje = BUFFER KOrdreLinje:HANDLE.
           RUN kordrelinje_post_update.p (hKOrdreLinje,'','',OUTPUT ocValue).

           RELEASE KOrdreLinje.

    END. /* TRANSACTION */
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-OpprettHentKOrdreHode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettHentKOrdreHode Procedure 
PROCEDURE OpprettHentKOrdreHode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    /*
    FIND FIRST KORdreHode EXCLUSIVE-LOCK WHERE 
      KOrdreHode.ButikkNr       = BongHode.ButikkNr AND 
      KOrdreHode.KundeNr        = BongHode.KundeNr AND 
      KOrdreHode.RegistrertDato >= DATE (MONTH(BongHode.Dato),
                                         1,
                                         YEAR(BongHode.Dato)) AND 
      KOrdreHode.Opphav = 5 NO-ERROR.
    IF AVAILABLE KOrdreHode THEN
    OPPDAT_KORDRE: 
    DO:
        ASSIGN 
            KOrdreHode.VerkstedMerknad = KOrdreHode.VerkstedMerknad + 
                                         (IF TRIM(KOrdreHode.VerkstedMerknad) <> '' THEN CHR(13) ELSE '') +  
                                         'Innlest bong: ' + STRING(BongHode.BongNr) + ' ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS'). 
        
    END. /* OPPDAT_KORDRE */
    ELSE IF NOT AVAILABLE KORdreHode THEN
    */
    OPPRETT_KORDRE: 
    DO:
        FIND Kunde NO-LOCK WHERE 
          Kunde.KundeNr = BongHode.KundeNr NO-ERROR.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = BongHode.ButikkNr NO-ERROR.
        CREATE KOrdreHode.
        ASSIGN
            KOrdreHode.VerkstedMerknad   = 'Innlest bong: ' + STRING(BongHode.BongNr) + ' ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
            KOrdreHode.Opphav            = 5 /* Kasse */
            KOrdreHode.EkstOrdreNr       = ""
            KOrdreHode.internMerknad     = "" 
            KOrdreHode.KundeMerknad      = "" 
            KOrdreHode.RegistrertDato    = BongHode.Dato
            KOrdreHode.RegistrertTid     = BongHode.Tid
            KOrdreHode.RegistrertAv      = " "  /* /Shops/DemoShop/Users/magbyr*/
            KOrdreHode.LevFNr            = 4 /* Utlevering i butikk */ 
            KOrdreHode.KundeMerknad      = ''
            NO-ERROR.
        ASSIGN                   
            KOrdreHode.KundeNr           = (IF AVAILABLE Kunde THEN Kunde.KundeNr ELSE DEC(BongHode.KundeNr))
            KOrdreHode.ButikkNr          = BongHode.ButikkNr
            KOrdreHode.KasseNr           = IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE 0
            KOrdreHode.ForsNr            = BongHode.KassererNr
            KOrdreHode.SelgerNr          = BongHode.SelgerNr
            NO-ERROR.
        ASSIGN     
            KOrdreHode.Navn              = IF AVAILABLE Kunde THEN Kunde.Navn ELSE ''
            KOrdreHode.KontNavn          = IF AVAILABLE Kunde THEN Kunde.KontNavn ELSE ''
            NO-ERROR.
        IF AVAILABLE Kunde THEN 
          FIND Post NO-LOCK WHERE 
            Post.PostNr = Kunde.PostNr NO-ERROR.
        ASSIGN 
            KOrdreHode.Adresse1          = IF AVAILABLE Kunde THEN Kunde.Adresse1 ELSE ''
            KOrdreHode.Adresse2          = IF AVAILABLE Kunde THEN Kunde.Adresse2 ELSE ''
            KOrdreHode.PostNr            = IF AVAILABLE Kunde THEN Kunde.PostNr ELSE ''
            KOrdreHode.Poststed          = IF AVAILABLE Post THEN Post.Beskrivelse ELSE ''
            KOrdreHode.ePostAdresse      = IF AVAILABLE Kunde THEN Kunde.ePostAdresse ELSE ''               
            KOrdreHode.Telefon           = IF AVAILABLE Kunde THEN Kunde.Telefon ELSE ''
            KOrdreHode.Telefaks          = IF AVAILABLE Kunde THEN Kunde.Telefaks ELSE ''
            NO-ERROR.
        ASSIGN      
            KOrdreHode.DeresRef          = IF AVAILABLE Kunde THEN Kunde.DeresRef ELSE " "
            KOrdreHode.VaarRef           = IF AVAILABLE Butiker THEN Butiker.VaarRef ELSE ''
            NO-ERROR.
        ASSIGN
            KOrdreHode.Leveringsdato     = BongHode.Dato
            KOrdreHode.BetaltDato        = BongHode.Dato
            KOrdreHode.TotalRabatt%      = 0.0
            KOrdreHode.BetBet            = 0
            NO-ERROR.
        IF AVAILABLE Kunde THEN 
          FIND Post NO-LOCK WHERE 
            Post.PostNr = Kunde.FaktPostNr NO-ERROR.
        ASSIGN 
            KOrdreHode.LevStatus         = "10" /* Bestilt */
            KOrdreHode.FaktAdresse1      = IF AVAILABLE Kunde THEN Kunde.FaktAdresse1 ELSE ''
            KOrdreHode.FaktAdresse2      = IF AVAILABLE Kunde THEN Kunde.FaktAdresse2 ELSE ''
            KOrdreHode.FaktPostNr        = IF AVAILABLE Kunde THEN Kunde.FaktPostNr ELSE ''
            KOrdreHode.FaktPoststed      = IF AVAILABLE Post THEN Post.Beskrivelse ELSE ''
            KOrdreHode.FirmaNavn         = IF AVAILABLE Kunde THEN Kunde.Navn ELSE ''
            KOrdreHode.FirmaAdresse1     = IF AVAILABLE Kunde THEN Kunde.Adresse1 ELSE ''
            KOrdreHode.FirmaAdresse2     = IF AVAILABLE Kunde THEN Kunde.Adresse2 ELSE ''
            KOrdreHode.FaktLand          = IF AVAILABLE Kunde THEN Kunde.Land ELSE ''
            .
        IF AVAILABLE Kunde THEN 
          FIND Post NO-LOCK WHERE 
            Post.PostNr = Kunde.LevPostNr NO-ERROR.
        ASSIGN 
            KOrdreHode.LevAdresse1       = IF AVAILABLE Kunde THEN Kunde.LevAdresse1 ELSE ''
            KOrdreHode.LevAdresse2       = IF AVAILABLE Kunde THEN Kunde.LevAdresse2 ELSE ''
            KOrdreHode.LevPostNr         = IF AVAILABLE Kunde THEN Kunde.LevPostNR ELSE ''
            KOrdreHode.LevPoststed       = IF AVAILABLE Post THEN Post.Beskrivelse ELSE ''
            KOrdreHode.LevLand           = IF AVAILABLE Kunde THEN Kunde.LevLand ELSE ''
            KOrdreHode.ValKod            = ''
            NO-ERROR.
    END. /* OPPRETT_KORDRE */
     
    IF AVAILABLE KOrdreHode THEN 
      FIND CURRENT KORdreHode NO-LOCK.

END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

 
&IF DEFINED(EXCLUDE-OpprettKOrdreLinjer) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKOrdreLinjer Procedure
PROCEDURE OpprettKOrdreLinjer:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
BONGLINJER:
FOR EACH BongLinje NO-LOCK WHERE
  BongLinje.B_Id = BongHode.b_id AND 
  BongLinje.Makulert = FALSE:

  IF CAN-DO('1,10',STRING(BongLinje.TTID)) THEN 
    RUN KordreLinjeVareSalg.
  
  IF CAN-DO('50,51,52,53,54,55,56,57,58,59,60,61,,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79',STRING(BongLinje.TTID)) THEN 
    RUN KordreLinjeBetaling.

END. /* BONGLINJER */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


