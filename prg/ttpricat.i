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

DEF  {&NEW} {&SHARED} TEMP-TABLE ttPriKat NO-UNDO
/*  1 */ FIELD R1 AS CHAR 
/*  2 */ FIELD LevNr AS CHAR 
/*  3 */ FIELD LevModellNr AS CHAR
/*  4 */ FIELD EANnr AS CHAR 
/*  5 */ FIELD VareTekst AS CHAR
/*  6 */ FIELD FargeKode AS CHAR 
/*  7 */ FIELD FargeTekst AS CHAR 
/*  8 */ FIELD Str        AS CHAR
/*  9 */ FIELD StrTab AS CHAR 
/* 10 */ FIELD Varemerke AS CHAR
/* 11 */ FIELD Enh AS CHAR
/* 12 */ FIELD AntIEnh AS CHAR
/* 13 */ FIELD LevPrisEngros AS CHAR
/* 14 */ FIELD ValKod AS CHAR
/* 15 */ FIELD forhRab% AS CHAR
/* 16 */ FIELD suppRab% AS CHAR
/* 17 */ FIELD VeilPris AS CHAR
/* 18 */ FIELD PAKstru AS CHAR
/* 19 */ FIELD LevUke1 AS CHAR
/* 20 */ FIELD LevUke2 AS CHAR
/* 21 */ FIELD LevUke3 AS CHAR
/* 22 */ FIELD LevUke4 AS CHAR
/* 23 */ FIELD VareGruppe AS CHAR
/* 24 */ FIELD LevNavn AS CHAR
/* 25 */ FIELD LevKod AS CHAR
/* 26 */ FIELD nettoForh AS CHAR
/* 27 */ FIELD kalkForh AS CHAR
/* 28 */ FIELD BFforh AS CHAR
/* 29 */ FIELD nettoSupp AS CHAR
/* 30 */ FIELD kalkSupp AS CHAR
/* 31 */ FIELD BFsupp AS CHAR 
/* 32 */ FIELD MarkedsPris AS CHAR
/* 33 */ FIELD Sortiment AS CHAR
/* 34 */ FIELD Sesong AS CHAR
/* 35 */ FIELD VPIBildeKode AS CHAR
/* 36 */ FIELD Merknad AS CHAR
/* 37 */ FIELD KjedeValutaPris AS CHARACTER 
/* 38 */ FIELD KjedeProdusent AS CHARACTER 
/* 39 */ FIELD ERPNr AS CHARACTER
/* 40 */ FIELD SalgsEnhetsType AS CHAR
/* 41 */ FIELD AktivFraDato AS CHAR
/* 42 */ FIELD AktivTilDato AS CHAR
/* 43 */ FIELD Bongtekst AS CHAR
/* 44 */ FIELD Etikettekst1 AS CHAR
/* 45 */ FIELD Funksjonskode AS CHARACTER
/* 46 */ FIELD Mva_Proc AS CHARACTER
/* 47 */ FIELD LinkVare AS CHARACTER
/* 48 */ FIELD PantBelop AS CHARACTER
/* 49 */ FIELD Filial AS CHARACTER
/* 50 */ FIELD Produsent AS CHARACTER
/* 51 */ FIELD Mengde AS CHARACTER
/* 52 */ FIELD JamforEnhet AS CHARACTER
/* 53 */ FIELD Kontrolleres AS LOG /* Dette feltet settes hvis posten skal legges med behandlingstatus 'på vent'. */
/* 54 */ FIELD ArtikkelNr  LIKE ArtBas.ArtikkelNr
/* 55 */ FIELD OpprettArtikkel AS LOG 
/* 56 */ FIELD PosterPrisending AS LOG 
/* 57 */ FIELD KjedeRab% AS DEC 
/* 58 */ FIELD KjedeSupRab% AS DEC 
/* 59 - 71 importeres p.t. ikke */
/* 62 */ FIELD Karakteristikk AS CHARACTER
/* 65 */ FIELD Alder AS CHARACTER 
/* 72 */ FIELD EkstStrTypeNavn AS CHAR 
/* 73 - 79 importeres p.t. ikke */
/* 76 */ FIELD AlfaKode2 AS CHARACTER  
/* 80 */ FIELD KjedeInnkPris AS DEC 
/* 81 */ FIELD KjedeSupInnkPris AS DEC 
/* 82 - 88 importeres p.t. ikke */
/* 89 */ FIELD Etikett AS INTEGER 
/* 90 */ FIELD Lager AS LOG 
/* 91 */ FIELD Sortimentkoder AS CHARACTER 
/* 92 */ FIELD Lagerkoder AS CHARACTER 
/* 93 */ FIELD Gjennomfaktureres AS LOG 
/* 94 */ FIELD KjedeVare AS LOG 
/* 95 */ FIELD Kampanjeuker AS CHARACTER 
/* 96 */ FIELD Kampanjestotte AS CHARACTER
/* 97 */ FIELD BehStatus AS INTEGER   
/* 98 */ FIELD Grunnsortiment AS CHARACTER 
/* 99 */ FIELD Opphav AS CHARACTER /* 0-Pricat, 1-Excel GUI import, 2-RIGAL, 3-RIGAL IPS, 4-EDI ARTIKEL, 5-EDI IPS ARTIKEL, 6-XML VPI import*/
/*100 */ FIELD RAvdNr AS CHARACTER /* Vareområde */  
/*101 */ FIELD OrgFilNavn AS CHARACTER /* Vareområde */  
/*102 */ FIELD LoggFilNavn AS CHARACTER /* Navn på loggfil */  
/*103 */ FIELD Etikettekst2 AS CHARACTER
/*104 */ FIELD ArtSlag AS CHARACTER 
/*105 */ FIELD OPris AS CHARACTER 
/*106 */ FIELD NON_Sale AS CHARACTER 
/*107 */ FIELD NegVare AS CHARACTER 
/*108 */ FIELD Pant AS CHARACTER 
/*109 */ FIELD Telefonkort AS CHARACTER 
/*110 */ FIELD WebButikkArtikkel AS CHARACTER 
/*111 */ FIELD PubliserINettbutikk AS CHARACTER 
/*112 */ FIELD HoyLav AS CHARACTER 
/*113 */ FIELD WebLeveringstid AS CHARACTER 
/*114 */ FIELD WebMinLager AS CHARACTER 
/*115 */ FIELD KampanjeKode AS CHARACTER 
/*116 */ FIELD LopNr AS CHARACTER 
/*117 */ FIELD GarantiKl AS CHARACTER
/*118 */ FIELD PostBredde AS CHARACTER
/*119 */ FIELD PostHoyde AS CHARACTER
/*120 */ FIELD PostLengde AS CHARACTER
/*121 */ FIELD PostVekt AS CHARACTER
/*122 */ FIELD AntLinkVare AS CHARACTER

    /* Hjelpefelter */
    FIELD ErrFlag AS LOG
    FIELD LinjeNr AS INT
    FIELD ModellNr LIKE ArtBas.Modell
    FIELD ModellFarge LIKE ArtBas.ModellFarge
    FIELD HovedModellFarge LIKE ArtBas.HovedModellFarge
    FIELD EkstVPILevNr LIKE VPIArtBas.EkstVPILevNr
    FIELD Vg AS INT
    FIELD Hg AS INT
    FIELD Farg AS INT
    FIELD StrTypeId LIKE StrType.StrTypeId
    FIELD iLevNr LIKE LevBas.LevNr
    FIELD iProdNr LIKE Produsent.ProdNr
    FIELD MomsKod AS INT
    FIELD VmId LIKE Varemerke.VmId
    FIELD Sasong AS INT
    FIELD SeqNrStr AS INTEGER
    FIELD ButikkNr AS INTEGER
    FIELD ProfilNr AS INTEGER  
    FIELD Produktid AS CHARACTER
    FIELD StrKode AS INTEGER
    FIELD PrisRab% AS CHAR
    FIELD BehStatTekst AS CHARACTER 
    INDEX ArtikkelNr
        LevModellNr
        VareTekst
        FargeTekst
        ArtikkelNr
    INDEX SettArtikkelNr
        ArtikkelNr
        LevModellNr
        VareTekst
        FargeTekst
        AntIEnh
        Markedspris
    INDEX SettModell
        EkstVPILevNr
        EANNr
    INDEX SettModell2
        EkstVPILevNr
        ArtikkelNr
    INDEX VPIUtpakk
        ArtikkelNr
        FargeTekst
    INDEX LinjeNr
        LinjeNr
    INDEX ERPNr
        ERPNr
    INDEX LevKod
        LevKod
    INDEX VG_Sjekk
        EkstVPILevNr
        Vg
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


