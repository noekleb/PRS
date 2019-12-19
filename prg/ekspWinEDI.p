&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : ekspWinEDI.p 
    Purpose     : Eksport av kundeordre til WinEDI for utskrift av postetikett.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    DEFINE VAR cParaString AS CHARACTER INIT "1090000001|WinEDI|Skriver" NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
&ENDIF


DEF VAR iAntLinjer   AS INT        NO-UNDO.
DEF VAR iAlle        AS INT        NO-UNDO.
DEF VAR bStream      AS LOG        NO-UNDO.

DEFINE STREAM Ut.

/* Filhåndtering */
DEF VAR cFilNavn   AS CHAR FORMAT "x(40)"    NO-UNDO.
DEFINE VARIABLE cBkuFil AS CHARACTER NO-UNDO.
DEF VAR cKatalog   AS CHAR                   NO-UNDO.
DEF VAR cPrefix    AS CHAR                   NO-UNDO.
DEF VAR cEkstent   AS CHAR                   NO-UNDO.
DEF VAR iSekvens   AS INT  FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR cEDBSystem AS CHAR INITIAL "WinEDI" NO-UNDO.
DEFINE VARIABLE dKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE cSkriver   AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt_WinEDI
  /*  1 */ FIELD OrdreNummer AS CHARACTER 
  /*  2 */ FIELD Adresse1 AS CHARACTER 
  /*  3 */ FIELD Adresse2 AS CHARACTER 
  /*  4 */ FIELD PostNr AS CHARACTER 
  /*  5 */ FIELD PostSted AS CHARACTER 
  /*  6 */ FIELD PostBoks  AS CHARACTER 
  /*  7 */ FIELD PostNr2 AS CHARACTER 
  /*  8 */ FIELD PostSted2 AS CHARACTER         /* Her skal KOrdreHode.FaktPoststed ut. */
  /*  9 */ FIELD KundeNr AS CHARACTER 
  /* 10 */ FIELD Kontaktperson AS CHARACTER 
  /* 11 */ FIELD KundensReferanse AS CHARACTER 
  /* 12 */ FIELD eMail AS CHARACTER 
  /* 13 */ FIELD OppkravsBelop AS CHARACTER 
  /* 14 */ FIELD Betalingsmate AS CHARACTER 
  /* 15 */ FIELD LandKode AS CHARACTER           /* Her skal KOrdreHode.Telefaks (Mobilnr fra ePages) ut. */
  /* 16 */ FIELD Sendingsmate AS CHARACTER 
  /* 17 */ FIELD MeldingTilMottaker AS CHARACTER /* Her skal KOrdreHode.LevFNr ut. */
  /* 18 */ FIELD PostEtikettSkriver AS CHARACTER
  /* 19 */ FIELD BrukerId AS CHARACTER 
  /* 20 */ FIELD Skrivervalg AS CHARACTER /* Overstyr skrivervalg */
  /* 21 */ FIELD MobilTlf AS CHARACTER 
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

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* IF lDirekte AND NOT CAN-DO(SESSION:GET-PRINTERS(),cPrinter) THEN */
/*     RETURN.                                                      */

  ASSIGN
    dKOrdre_Id = DECIMAL(ENTRY(1,cParaString,'|'))
    cSkriver   = ENTRY(3,cParaString,'|').

  RUN PopulateTT.

/* Når det kommer flere formater, håndter dem på    */
/* denne måten. Da kjøres utlegget for de formatene */
/* som er satt aktive.                              */
/* EDB systemene skal være opprettet via sysinit.p  */
/* Bruk Case for å teste hvilken eksportrutine som  */
/* skal kjøres.                                     */
/* For each EkstEDBSystem where                     */
/*   EkstEDBSystem.EDBSystem begins "EkspFAK" and   */
/*   EkstEDBSystem.Aktiv = true:                    */

  RUN EksporterWinEDI. /* (EkstEDBSystem.EDBSystem)  */
/* end. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-EDBSystem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EDBSystem Procedure 
PROCEDURE EDBSystem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO TRANSACTION:
    FIND EkstEDBSystem WHERE
        EkstEDBSystem.EDBSystem = cEDBSystem EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE EkstEDBSystem THEN DO:
        LEAVE.
    END.
    ELSE DO:
        ASSIGN
            cKatalog           = TRIM(EkstEDBSystem.Filkatalog,"\")
            cPrefix            = EkstEDBSystem.FilPrefix
            cEkstent           = TRIM(EkstEDBSystem.FilEkstent,".")
            iSekvens           = IF (EkstEDBSystem.SeqNr + 1) > EkstEDBSystem.MaksSeq
                                    THEN 1
                                    ELSE EkstEDBSystem.SeqNr + 1
            EkstEDBSystem.SeqNr = iSekvens
            cFilNavn           = cKatalog + "\" +
                                 cPrefix  + 
                                 STRING(iSekvens,"99999999") + "." + 
                                 cEkstent
                                 
            cBkuFil            = cKatalog + "\bku\" +
                                 cPrefix  + 
                                 STRING(iSekvens,"99999999") + "." + 
                                 cEkstent
                                 
            .
        OS-COMMAND SILENT VALUE('mkdir ' + cKatalog + '\bku'). 
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterWinEDI) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterWinEDI Procedure 
PROCEDURE EksporterWinEDI :
/*------------------------------------------------------------------------------
  Purpose:     Utlegg av fakturadata til Visma Global. Opprinnelig for Dampbageriet.
               Et enkelt utlegg som er basert på at priser og rabatter hentes og 
               beregnes fra Visma Global.
               Legg merke til at heller ikke størrelse er lagt ut, da denne forutsettes
               alltid være 1.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   FIND FIRST tt_WinEDI NO-ERROR.
   IF NOT AVAILABLE tt_WinEDI THEN
     RETURN.
   
   RUN EDBSystem.        /* (EkstEDBSystem.EDBSystem)  */
   IF cFilnavn = "" THEN
     RETURN.
     
   EKSPORT:
   DO:
     OUTPUT STREAM Ut TO VALUE(cFilnavn) NO-ECHO.     
     PUT STREAM Ut UNFORMATTED 
       /*  1 */ tt_WinEDI.OrdreNummer ';'        
       /*  2 */ QUOTER(tt_WinEDI.Adresse1) ';'           
       /*  3 */ QUOTER(tt_WinEDI.Adresse2) ';'           
       /*  4 */ tt_WinEDI.PostNr ';'             
       /*  5 */ QUOTER(tt_WinEDI.PostSted) ';'           
       /*  6 */ QUOTER(tt_WinEDI.PostBoks) ';'           
       /*  7 */ QUOTER(tt_WinEDI.PostNr2) ';'            
       /*  8 */ QUOTER(tt_WinEDI.PostSted2) ';'          
       /*  9 */ tt_WinEDI.KundeNr ';'            
       /* 10 */ QUOTER(tt_WinEDI.Kontaktperson) ';'      
       /* 11 */ QUOTER(tt_WinEDI.KundensReferanse) ';'   
       /* 12 */ QUOTER(tt_WinEDI.eMail) ';'              
       /* 13 */ tt_WinEDI.OppkravsBelop ';'      
       /* 14 */ QUOTER(tt_WinEDI.Betalingsmate) ';'      
       /* 15 */ tt_WinEDI.LandKode ';'           
       /* 16 */ QUOTER(tt_WinEDI.Sendingsmate) ';'       
       /* 17 */ QUOTER(tt_WinEDI.MeldingTilMottaker) ';'
       /* 18 */ QUOTER(tt_WinEDI.PostEtikettSkriver) ';'
       /* 19 */ QUOTER(tt_WinEDI.BrukerId) ';'
       /* 20 */ QUOTER(tt_WinEDI.Skrivervalg)  ';'
       /* 21 */ QUOTER(tt_WinEDI.MobilTlf)
       SKIP.
     
     OUTPUT STREAM Ut CLOSE.

     /* TEST */
     OUTPUT STREAM Ut TO VALUE(cBkuFil) NO-ECHO.     
     PUT STREAM Ut UNFORMATTED 
       /*  1 */ tt_WinEDI.OrdreNummer ';'        
       /*  2 */ QUOTER(tt_WinEDI.Adresse1) ';'           
       /*  3 */ QUOTER(tt_WinEDI.Adresse2) ';'           
       /*  4 */ tt_WinEDI.PostNr ';'             
       /*  5 */ QUOTER(tt_WinEDI.PostSted) ';'           
       /*  6 */ QUOTER(tt_WinEDI.PostBoks) ';'           
       /*  7 */ QUOTER(tt_WinEDI.PostNr2) ';'            
       /*  8 */ QUOTER(tt_WinEDI.PostSted2) ';'          
       /*  9 */ tt_WinEDI.KundeNr ';'            
       /* 10 */ QUOTER(tt_WinEDI.Kontaktperson) ';'      
       /* 11 */ QUOTER(tt_WinEDI.KundensReferanse) ';'   
       /* 12 */ QUOTER(tt_WinEDI.eMail) ';'              
       /* 13 */ tt_WinEDI.OppkravsBelop ';'      
       /* 14 */ QUOTER(tt_WinEDI.Betalingsmate) ';'      
       /* 15 */ tt_WinEDI.LandKode ';'           
       /* 16 */ QUOTER(tt_WinEDI.Sendingsmate) ';'       
       /* 17 */ QUOTER(tt_WinEDI.MeldingTilMottaker) ';'
       /* 18 */ QUOTER(tt_WinEDI.PostEtikettSkriver) ';'
       /* 19 */ QUOTER(tt_WinEDI.BrukerId) ';'
       /* 20 */ QUOTER(tt_WinEDI.Skrivervalg)  ';'
       /* 21 */ QUOTER(tt_WinEDI.MobilTlf)
       SKIP.
     
     OUTPUT STREAM Ut CLOSE.
   END. /* EKSPORT */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateTT Procedure 
PROCEDURE PopulateTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/

  FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.KOrdre_Id = dKOrdre_Id NO-ERROR.
  IF AVAILABLE KOrdreHode THEN
  DO:
    CREATE tt_WinEDI.
    ASSIGN
      tt_WinEDI.OrdreNummer        = IF KOrdreHode.EkstOrdreNr <> "" THEN KOrdreHode.EkstOrdreNr ELSE STRING(KOrdreHode.KOrdre_Id)
      tt_WinEDI.Adresse1           = IF KOrdreHode.LevAdresse1 <> '' THEN KORdreHode.LevAdresse1 ELSE KOrdreHode.Adresse1
      tt_WinEDI.Adresse2           = IF KOrdreHode.LevAdresse1 <> '' THEN KORdreHode.LevAdresse2 ELSE KOrdreHode.Adresse2
      tt_WinEDI.PostNr             = ''
      tt_WinEDI.PostSted           = ''
      tt_WinEDI.PostBoks           = ''
      tt_WinEDI.PostNr2            = IF KOrdreHode.LevAdresse1 <> '' THEN KORdreHode.LevPostNr ELSE KOrdreHode.PostNr
      tt_WinEDI.PostSted2          = IF KOrdreHode.FaktPoststed <> '' THEN KOrdreHode.FaktPoststed ELSE KOrdreHode.PostSted
      tt_WinEDI.KundeNr            = STRING(KOrdreHode.KundeNr)
      tt_WinEDI.Kontaktperson      = KOrdreHode.Navn
      tt_WinEDI.KundensReferanse   = KOrdreHode.Referanse
      tt_WinEDI.eMail              = KOrdreHode.ePostAdresse
      tt_WinEDI.OppkravsBelop      = '0'
      tt_WinEDI.Betalingsmate      = ''
      tt_WinEDI.LandKode           = KOrdreHode.Telefaks
      tt_WinEDI.Sendingsmate       = STRING(KOrdreHode.LevFNr)
      tt_WinEDI.MeldingTilMottaker = '5' /*STRING(KOrdreHode.LevFNr)*/
      tt_WinEDI.PostEtikettSkriver = cSkriver
      tt_WinEDI.BrukerId           = USERID('SkoTex')
      tt_WinEDI.Skrivervalg        = '14'
      tt_WinEDI.MobilTlf           = (IF TRIM(KordreHode.MobilTlf) <> '' 
                                        THEN KOrdreHode.MobilTlf
                                        ELSE KOrdreHode.Telefon)
      tt_WinEDI.MobilTlf           = TRIM(tt_WinEDI.MobilTlf,' ')
      tt_WinEDI.MobilTlf           = REPLACE(tt_WinEDI.MobilTlf,' ','')
      NO-ERROR.
    DO TRANSACTION:
        FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
        ASSIGN 
            KOrdreHode.AntPPEti = KOrdreHode.AntPPEti + 1
            .        
        FIND CURRENT KOrdreHode EXCLUSIVE-LOCK.
    END.  
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

