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

DEFINE INPUT  PARAMETER lcDataSet AS LONGCHAR   NO-UNDO.
DEFINE OUTPUT PARAMETER iStatus AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cMsg    AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lJFPlock AS LOGICAL     NO-UNDO.
DEFINE VARIABLE bSvenskFormat AS LOG NO-UNDO.
DEFINE VARIABLE bAutoOverfor AS LOG NO-UNDO.

DEFINE VARIABLE iCL AS INTEGER NO-UNDO.
DEFINE VARIABLE iNettbutikk AS INTEGER NO-UNDO.
DEFINE VARIABLE iVgLopNr AS INT NO-UNDO.

DEFINE VARIABLE bSJekkKundeKort AS LOG NO-UNDO.
DEFINE VARIABLE cKOrdre_Id_Lst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStrkode AS INTEGER     NO-UNDO.

DEFINE VARIABLE iKndGruppeId  AS INTEGER NO-UNDO.
DEFINE VARIABLE iKndTypeId    AS INTEGER NO-UNDO.
DEFINE VARIABLE iMedGruppeId  AS INTEGER NO-UNDO.
DEFINE VARIABLE iMedTypeId    AS INTEGER NO-UNDO.
DEFINE VARIABLE iStdMKlubbId  AS INTEGER NO-UNDO.
DEFINE VARIABLE cMKlubbId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lReservationsweb AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iResBetBet AS INTEGER     NO-UNDO.
DEFINE VARIABLE iResLevsatt AS INTEGER     NO-UNDO.
DEFINE BUFFER bNettbutikk FOR Butiker.
DEFINE BUFFER bSentrallager FOR Butiker.

{readWooComOrder.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-betDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD betDato Procedure 
FUNCTION betDato RETURNS DATE
    ( INPUT cYYYYMMDD AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTid Procedure 
FUNCTION getTid RETURNS INTEGER
    ( INPUT cHHMMSS AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentDato Procedure 
FUNCTION hentDato RETURNS DATE
( INPUT cDatoTid AS CHAR ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentSisteEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentSisteEntry Procedure 
FUNCTION hentSisteEntry RETURNS CHARACTER
( INPUT cTekst AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD hentTid Procedure 
FUNCTION hentTid RETURNS INTEGER
        ( INPUT cDatoTid AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

DEFINE VARIABLE cRet_val AS CHARACTER NO-UNDO.

/* Returnerer hvis det ikke er en xml fil som skal leses. */

{syspara.i 150 1 15 cTekst} 
IF CAN-DO('1,J,Ja,Y,YES,true',cTekst) THEN 
  bSJekkKundeKort = TRUE.
    
{syspara.i 150 1 9 cTekst} 
IF cTekst = "3" THEN
    lJFPlock = TRUE.

{syspara.i 150 1 16 cTekst}
IF CAN-DO("1,J,Ja,Yes,True",cTekst) THEN 
    bSvenskFormat = TRUE.
          
{syspara.i 150 1 19 cTekst}
IF CAN-DO("1,J,Ja,Yes,True",cTekst) THEN 
    bAutoOverfor = TRUE.          
          
{syspara.i 5 1 1 iCL int}
{syspara.i 150 1 2 iNettbutikk int}
{syspara.i 19 9 2 iVgLopNr INT}

{syspara.i 150 1 12 iKndGruppeId int}   
{syspar2.i 150 1 12 iKndTypeId int}
    
{syspara.i 150 1 13 iMedGruppeId int}   
{syspar2.i 150 1 13 iMedTypeId int}

{syspara.i 150 1 14 iStdMKlubbId int}
{syspara.i 150 17 1 cTmp}  /* tillsvidare 1 = reservationswebshop med bestämd butik */
lReservationsweb = cTmp = "1".
IF lReservationsweb THEN DO:
    {syspara.i 150 17 2 iResBetBet INT}  
    {syspara.i 150 17 3 iResLevsatt INT}  
END.

IF NOT CAN-FIND(Butiker WHERE
                Butiker.Butik = iNettbutikk) THEN DO:
    RETURN.
END.
FIND bNettButikk NO-LOCK WHERE
    bNettbutikk.Butik = iNettbutikk NO-ERROR.
IF NOT AVAIL bNettButikk THEN
    RETURN.
FIND bSentrallager NO-LOCK WHERE
    bSentrallager.Butik = iCL.

RUN SkapaDS.

/* IF NOT CAN-FIND(FIRST tt_order) THEN */
/*     RETURN.                          */
FIND FIRST tt_Order NO-ERROR.
IF NOT AVAIL tt_order THEN DO:
    iStatus = 300.
    cMsg    = "Ingen order".
    RETURN.
END.
IF NOT CAN-FIND(FIRST tt_line_items) THEN DO:
    iStatus = 399.
    cMsg    = "Inga orderlinjer".
    RETURN.
END.
IF NUM-ENTRIES(tt_Order.cwebbutnr,";") = 2 AND ENTRY(1,tt_Order.cwebbutnr,";") = "But" THEN DO:
 FIND bNettButikk NO-LOCK WHERE
     bNettbutikk.Butik = INT(ENTRY(2,tt_Order.cwebbutnr,";")) NO-ERROR.
END.
FIND KOrdreHode NO-LOCK WHERE
    KOrdreHode.butikknr    = bNettbutikk.butik AND
    KOrdreHode.EkstOrdreNr = STRING(tt_Order.number) NO-ERROR.
IF AVAIL Kordrehode THEN DO:
    iStatus = 350.
    cMsg    = "Order finns sedan tidigare".
    RETURN.
END.

RUN SkapaKund.
RUN posterOrdreData.           

IF cKOrdre_Id_Lst <> '' THEN /* egentligen bara 1 kordre */
  DO:
    iStatus = 200.
    /* Setter plukkbutikk på kundeordrelinjene. */
/*     IF lJFPlock = TRUE THEN */
/*     IF lReservationsweb AND KOrdreHode.Butik <> 0 THEN DO: */
    IF KOrdreHode.Butik <> 0 THEN DO: /* Sätts genom att det är en reservationsweb eller c&c local_pickup */
        /*  */
        FOR EACH KOrdrelinje WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id:
            ASSIGN KOrdreLinje.PlukkButikk   = IF KordreLinje.plockstatus = -1 THEN 0 ELSE KOrdreHode.butik
                   KOrdreLinje.UtleverButikk = IF KordreLinje.plockstatus = -1 THEN 0 ELSE KOrdreLinje.PlukkButikk
                   KordreLinje.plockstatus   = IF KordreLinje.plockstatus = -1 THEN 0 ELSE 1.
        END.
        /* Om vi är har för att C&C är satt vid icke reservationsweb så sätter vi om iResBetBet */
        IF NOT lReservationsweb THEN DO:
            iResBetBet = KOrdreHode.BetBet. /* för enkelhetens skull */
            {syspara.i 150 17 3 iResLevsatt INT}  
        END.
        ASSIGN KOrdreHode.BetBet = iResBetBet
               KOrdreHode.LevFNr = iResLevsatt.
    END.
    ELSE
        RUN sett_plukkbutikk_kundeordreJF.p (cKOrdre_Id_Lst,"",0). /* tredje parameter är rowid, används vid byte av plockbutik från KOrdrelinje.w till ny butik */
/*     ELSE                                                    */
/*         RUN sett_plukkbutikk_kundeordre.p (cKOrdre_Id_Lst). */
    /* Opprettelse av overføringsordre og sending av email. En ordre pr. fra/til butikkrelasjon. */
    IF bAutoOverfor THEN DO:
/*         IF lJFPlock THEN */
            RUN opprett_OverforingsordreJF.p (cKOrdre_Id_Lst,TRUE,""). /* tredje parameter är rowid, används efter ändring av plockbutik från KOrdrelinje.w */
/*         ELSE                                                       */
/*             RUN opprett_Overforingsordre.p (cKOrdre_Id_Lst,FALSE). */
    END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-posterOrdreData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE posterOrdreData Procedure 
PROCEDURE posterOrdreData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cFraktMetode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE hKordreLinje AS HANDLE NO-UNDO.
DEFINE VARIABLE cKundeId     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iVg          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLopnr       AS INTEGER     NO-UNDO.
DEFINE VARIABLE cVgLop AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLinjeNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE ocValue AS CHARACTER   NO-UNDO.
DO TRANSACTION:
     /* Disse temp tabellene finnes det bare en post i */
/*      FIND FIRST tt_Order NO-ERROR. ----  I MAIN 
     IF NUM-ENTRIES(tt_Order.cwebbutnr,";") = 2 AND ENTRY(1,tt_Order.cwebbutnr,";") = "But" THEN DO:
         FIND bNettButikk NO-LOCK WHERE
             bNettbutikk.Butik = INT(ENTRY(2,tt_Order.cwebbutnr,";")) NO-ERROR.
     END.
     */
     /* Sjekker om ordren finnes fra før. <creationDate>24.04.2009 22:30:15</creationDate> */
     IF tt_Order.date_created <> '' OR tt_Order.number <> 0 THEN 
         FIND KOrdreHode NO-LOCK WHERE
              KOrdreHode.RegistrertDato = hentDato(tt_Order.date_created) AND
              KOrdreHode.EkstOrdreNr    = STRING(tt_Order.number) NO-ERROR.
     ELSE DO:
       RETURN '** Feil - Ingen opprettelsesdato/ordrereferanse på ordre fra nettbutikk.'.
     END.
     /* Henter kunde. */
/*      ASSIGN cKundeId = hentSisteEntry(tt_Order.customer) NO-ERROR. */
     ASSIGN cKundeId = STRING(tt_Order.number) NO-ERROR.   /* ordernumret är externt kundnr */
     FIND Kunde NO-LOCK WHERE Kunde.EksterntKundeNr = cKundeId AND Kunde.butikknr = bNettbutikk.Butik NO-ERROR.
     IF NOT AVAILABLE Kunde THEN
     DO:
/*        RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: ** Feil - Ukjent kunde angitt på kundeordre. ' + 'Dato: ' + STRING(tt_Order.date_created) + ' OrdreNr: ' + STRING(tt_Order.cAlias) + ' Kunde: ' + STRING(tt_Order.customer)). */
       RETURN '** Feil - Ukjent kunde angitt på kundeordre ' + STRING(tt_Order.number) + ' fra nettbutikk.'.
     END.
     IF AVAILABLE KOrdreHode THEN
     OPPDATER_ORDRE:
     DO:
       /* Legg inn oppdatering av aktuelle feilt. Ref. mail med spørsmål som er sendt til Ronny Jordalen. */
       RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: Ordre EKSISTERER FRA FØR. ' + 'Kordre_Id: ' + STRING(KOrdreHode.KOrdre_ID) + ' og EksterntId: ' + STRING(KOrdreHode.EkstOrdreNr) + ' Kunde: ' + STRING(KOrdreHode.KundeNr)).
     END. /* OPPDATER_ORDRE */
     
     ELSE 
     OPPRETTELSE_ORDRE:
     DO:           
         /* Ordrenummer settes ved create i db trigger. */
         CREATE KOrdreHode.
         RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'xmlreadBITSOrder.p: Ordre opprettet. ' + 'Kordre_Id: ' + STRING(KOrdreHode.KOrdre_ID) + ' og EksterntId: ' + STRING(KOrdreHode.EkstOrdreNr) + ' Kunde: ' + STRING(KOrdreHode.KundeNr)).
         ASSIGN
             KOrdreHode.VerkstedMerknad   = 'Innlest ' + STRING(TODAY) + ' ' + STRING(TIME,'HH:MM:SS') 
             KOrdreHode.Opphav            = 10 /* Nettbutikk */
             KOrdreHode.EkstOrdreNr       = STRING(tt_Order.number)
/*              KOrdreHode.internMerknad     = tt_Order.internalComment  */
             KOrdreHode.KundeMerknad      = tt_Order.customer_note.
         KOrdreHode.RegistrertDato  = hentDato(tt_Order.date_created).
         KOrdreHode.RegistrertTid   = hentTid(tt_Order.date_created).
         KOrdreHode.RegistrertAv    = "Webbutik". /* hentSisteEntry(tt_Order.cuser). */
         IF KORdreHode.RegistrertDato = ? THEN 
             ASSIGN 
             KOrdreHode.RegistrertDato = TODAY
             KOrdreHode.RegistrertTid  = TIME.
         /* Bygger en liste over de kundeordre som det skal sendes eMail og opprettes overføringsordre på. */
         IF NOT CAN-DO(cKOrdre_Id_Lst,STRING(KOrdreHode.KORdre_Id)) THEN
           ASSIGN cKOrdre_Id_Lst = cKOrdre_Id_Lst + (IF cKOrdre_Id_Lst = '' THEN '' ELSE ',') + STRING(KOrdreHode.KOrdre_Id).
         /* Henter kasse. Kasse 99 legges alltid opp for kundeordre. Brukes også for nettbutikk. */
         FIND LAST Kasse NO-LOCK WHERE Kasse.ButikkNr = bNettbutikk.Butik NO-ERROR.
         /* Henter kasserer. Det skal ligge en kasserer med netbutikk.brukerid = forsalg.brukerid2. */
/*          FIND FIRST Forsalj NO-LOCK WHERE Forsal.Brukerid2 = hentSisteEntry(tt_Order.cuser) NO-ERROR. */
         IF NOT AVAILABLE Kunde THEN
         DO:
           RETURN '** Feil - Ukjent kunde angitt på kundeordre ' + STRING(tt_Order.number) + ' fra nettbutikk.'.
         END.
         ELSE DO:
            /* Kundenr, butikk, kasserer m.m. */               
            ASSIGN                   
            KOrdreHode.KundeNr              = Kunde.KundeNr
            KOrdreHode.ButikkNr             = bNettbutikk.Butik
            KOrdreHode.KasseNr              = (IF AVAILABLE Kasse THEN Kasse.KasseNr ELSE 0)
            KOrdreHode.ForsNr               = (IF AVAILABLE Forsalj THEN Forsalj.ForsNr ELSE 0)
            KOrdreHode.SelgerNr             = 0 NO-ERROR.
            /* Kontoradresse m.m. */
            ASSIGN     
            KOrdreHode.Navn                 = tt_Order.bi_first_name + ' ' + tt_order.bi_last_name
            KOrdreHode.KontNavn             = Kunde.KontNavn NO-ERROR.
            /* Kontoradresse skal hentes fra fakturadresse på ordre. Er ikke vedlikeholdt i kunderegister. */
            ASSIGN KOrdreHode.Adresse1      = tt_Order.bi_address_1
                   KOrdreHode.Adresse2      = tt_Order.bi_address_2
                   KOrdreHode.PostNr        = tt_Order.bi_postcode
                   KOrdreHode.Poststed      = tt_Order.bi_city 
                   KOrdreHode.ePostAdresse  = tt_Order.bi_email
                   KOrdreHode.Telefon       = REPLACE(tt_Order.bi_phone," ","") NO-ERROR.
            IF KOrdreHode.MobilTlf = '' THEN ASSIGN KOrdreHode.MobilTlf = KOrdreHode.Telefon NO-ERROR.
            IF bSvenskFormat AND LENGTH(KOrdreHode.PostNr) = 5 THEN 
              IF LENGTH(KOrdreHode.PostNr) = 5 THEN KOrdreHode.PostNr = SUBSTRING(KOrdreHode.PostNr,1,3) + ' ' + SUBSTRING(KOrdreHode.PostNr,4,2).
            
            ASSIGN KOrdreHode.DeresRef = Kunde.DeresRef
                   KOrdreHode.VaarRef  = bNettbutikk.VaarRef no-error.
            ASSIGN
            KOrdreHode.Leveringsdato        = KOrdreHode.RegistrertDato + 7
            KOrdreHode.BetaltDato           = KOrdreHode.RegistrertDato
            KOrdreHode.TotalRabatt%         = 0.0
            KOrdreHode.LevStatus            = '30' /* Bekreftet */
            KOrdreHode.BetBet               = Kunde.BetBet no-error.
            /* Overstyrer elementer i fakturaadresse. */
            ASSIGN KOrdreHode.DeresRef      = tt_Order.bi_first_name + ' ' + tt_order.bi_last_name
                   KOrdreHode.FaktAdresse1  = tt_Order.bi_address_1
                   KOrdreHode.FaktAdresse2  = tt_Order.bi_address_2
                   KOrdreHode.FaktPostNr    = tt_Order.bi_postcode
                   KOrdreHode.FaktPoststed  = tt_Order.bi_city
                   KOrdreHode.ePostAdresse  = tt_Order.bi_email
                   KOrdreHode.Telefon       = REPLACE(tt_Order.bi_phone," ","")
/*             ASSIGN KOrdreHode.Telefaks      = tt_billingAddress.phoneCell NO-ERROR. */
                   KOrdreHode.FirmaNavn     = tt_Order.bi_company
                   KOrdreHode.FirmaAdresse1 = tt_Order.bi_first_name
                   KOrdreHode.FirmaAdresse2 = tt_order.bi_last_name NO-ERROR.
            IF bSvenskFormat AND LENGTH(KOrdreHode.FaktPostNr) = 5 THEN 
              IF LENGTH(KOrdreHode.FaktPostNr) = 5 THEN KOrdreHode.FaktPostNr = SUBSTRING(KOrdreHode.FaktPostNr,1,3) + ' ' + SUBSTRING(KOrdreHode.FaktPostNr,4,2).
            IF TRIM(tt_Order.bi_country) <> '' THEN
            DO: 
              IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
              FIND NumLandKode NO-LOCK WHERE
                   NumLandKode.NumLandKode = INTEGER(tt_Order.bi_country) NO-ERROR.
              IF NOT AVAILABLE NumLandKode THEN  
                FIND AlfaLandKode NO-LOCK WHERE 
                     AlfaLandKode.AlfaKode3 = TRIM(tt_Order.bi_country) NO-ERROR.
              IF NOT AVAILABLE NumLandKode AND NOT AVAILABLE AlfaLandKode THEN  
                FIND AlfaLandKode NO-LOCK WHERE 
                     AlfaLandKode.AlfaKode2 = TRIM(tt_Order.bi_country) NO-ERROR.
              IF NOT AVAILABLE NumLandKode AND AVAILABLE AlfaLandKode THEN 
                FIND NumLandKode NO-LOCK WHERE
                     NumLandKode.NumLandKode = AlfaLandKode.NumLandKode NO-ERROR.                     
              /* Fakturaadresse */
              ASSIGN
              KOrdreHode.FaktLand          = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE '')
              NO-ERROR.
            END.
            IF KOrdreHode.FaktAdresse1 = '' AND KOrdreHode.FaktAdresse2 = '' THEN 
              ASSIGN
              KOrdreHode.FaktAdresse1         = KOrdreHode.Adresse1
              KOrdreHode.FaktAdresse2         = KOrdreHode.Adresse2
              KOrdreHode.FaktPostNr           = KOrdreHode.PostNr
              KOrdreHode.FaktLand             = Kunde.Land NO-ERROR.
            /* Leveringsadresse */
            ASSIGN KOrdreHode.LevAdresse1  = tt_Order.sh_address_1 NO-ERROR. 
            ASSIGN KOrdreHode.LevAdresse2  = tt_Order.sh_address_2 NO-ERROR.
            ASSIGN KOrdreHode.LevPostNr    = tt_Order.sh_postcode NO-ERROR.
            ASSIGN KOrdreHode.LevPoststed  = tt_Order.sh_city NO-ERROR.
            /* Land */
            IF bSvenskFormat AND LENGTH(KOrdreHode.LevPostNr) = 5 THEN 
              IF LENGTH(KOrdreHode.LevPostNr) = 5 THEN KOrdreHode.LevPostNr = SUBSTRING(KOrdreHode.LevPostNr,1,3) + ' ' + SUBSTRING(KOrdreHode.LevPostNr,4,2).
            IF TRIM(tt_Order.sh_country) <> '' THEN
            DO: 
              IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
              FIND NumLandKode NO-LOCK WHERE
                   NumLandKode.NumLandKode = INTEGER(tt_Order.sh_country) NO-ERROR.
              IF NOT AVAILABLE NumLandKode THEN  
                FIND AlfaLandKode NO-LOCK WHERE 
                     AlfaLandKode.AlfaKode3 = TRIM(tt_Order.sh_country) NO-ERROR.
              IF NOT AVAILABLE NumLandKode AND NOT AVAILABLE AlfaLandKode THEN  
                FIND AlfaLandKode NO-LOCK WHERE 
                     AlfaLandKode.AlfaKode2 = TRIM(tt_Order.sh_country) NO-ERROR.
              IF NOT AVAILABLE NumLandKode AND AVAILABLE AlfaLandKode THEN 
                FIND NumLandKode NO-LOCK WHERE
                     NumLandKode.NumLandKode = AlfaLandKode.NumLandKode NO-ERROR.                     
              /* Fakturaadresse */
              ASSIGN
              KOrdreHode.LevLand             = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE '')
              NO-ERROR.
            END.
            IF KOrdreHode.LevAdresse1 = '' AND KOrdreHode.LevAdresse2 = '' THEN 
              ASSIGN 
              KOrdreHode.LevAdresse1          = KOrdreHode.FaktAdresse1
              KOrdreHode.LevAdresse2          = KOrdreHode.FaktAdresse2
              KOrdreHode.LevPostNr            = KOrdreHode.FaktPostNr
              KOrdreHode.LevPostSted          = KOrdreHode.FaktPostSted
              KOrdreHode.LevLand              = KOrdreHode.FaktLand NO-ERROR.
         END.
       ASSIGN KOrdreHode.ValKod = tt_Order.currency. /* Valutakode */

       IF AVAILABLE Kunde THEN DO:
         FIND CURRENT Kunde EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF AVAILABLE Kunde THEN DO:
           ASSIGN
             Kunde.Navn          = KOrdreHode.DeresRef
             Kunde.DeresRef      = KOrdreHode.DeresRef
             Kunde.Adresse1      = KOrdreHode.Adresse1
             Kunde.Adresse2      = KOrdreHode.Adresse2
             Kunde.PostNr        = KOrdreHode.PostNr
             Kunde.ePostAdresse  = KOrdreHode.ePostAdresse
             Kunde.Telefon       = KOrdreHode.Telefon
             Kunde.Telefaks      = KOrdreHode.MobilTlf
             Kunde.MobilTlf      = KOrdreHode.MobilTlf
             Kunde.FaktAdresse1  = KOrdreHode.Adresse1
             Kunde.FaktAdresse2  = KOrdreHode.Adresse2
             Kunde.FaktPostNr    = KOrdreHode.PostNr 
             Kunde.LevAdresse1   = KOrdreHode.Adresse1
             Kunde.LevAdresse2   = KOrdreHode.Adresse2
             Kunde.LevPostNr     = KOrdreHode.PostNr
             Kunde.Butik         = KOrdreHode.ButikkNr.
             IF bSvenskFormat AND LENGTH(Kunde.PostNr) = 5 THEN 
               IF LENGTH(Kunde.PostNr) = 5 THEN Kunde.PostNr = SUBSTRING(Kunde.PostNr,1,3) + ' ' + SUBSTRING(Kunde.PostNr,4,2).
           FIND CURRENT Kunde NO-LOCK.
         END.
/*          ELSE                                                                    */
/*           FIND Kunde NO-LOCK WHERE                                               */
/*              Kunde.EksterntKundeNr = hentSisteEntry(tt_Order.customer) NO-ERROR. */
       END.
       IF NEW KOrdreHode THEN 
       VARELINJER:
       FOR EACH tt_line_items NO-LOCK: /* BREAK BY tt_ProductLineItem.SeqNr: */
         cVgLop = ENTRY(1,tt_line_items.sku,"-").
         iVg = INT(SUBSTR(cVgLop,1,LENGTH(cVgLop) - 4)).
         iLopnr = INT(SUBSTR(cVgLop,LENGTH(cVgLop) - 3)).
         iStrKode = INT(ENTRY(2,tt_line_items.sku,"-")).
         FIND ArtBas NO-LOCK WHERE ArtBAs.vg = iVg AND Artbas.lopnr = iLopnr NO-ERROR.
         IF AVAILABLE ArtBas THEN 
           FIND ArtPris OF ArtBas NO-LOCK WHERE ArtPris.ProfilNr = bNettButikk.ProfilNr NO-ERROR.
         IF NOT AVAILABLE ArtPris AND AVAILABLE ArtBas THEN 
           FIND ArtPris OF ArtBas NO-LOCK WHERE ArtPris.ProfilNr = bSentrallager.ProfilNr NO-ERROR.
         
/*          FIND FIRST Moms NO-LOCK WHERE Moms.MomsProc = DECIMAL(REPLACE(tt_ProductLineItem.taxRate,',','.')) NO-ERROR. */
         FIND StrKonv NO-LOCK WHERE StrKonv.StrKode = iStrKode NO-ERROR.
         FIND FIRST Strekkode OF ArtBas NO-LOCK WHERE Strekkode.STrKode = iStrKode NO-ERROR.
         IF AVAILABLE ArtBas THEN 
           FIND Lager NO-LOCK WHERE
             Lager.ArtikkelNr = Artbas.ArtikkelNr AND 
             Lager.Butik      = bNettButikk.Butik NO-ERROR.
         FIND LAST KOrdrelinje NO-LOCK WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
         IF AVAILABLE KOrdreLinje THEN 
           iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
         ELSE
           iLinjeNr = 1.
         RUN SkapaKOLinje ("Produkt",iLinjeNr).
         
         IF AVAILABLE ArtBas THEN DO:
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
           
       END. /* VARELINJER */
       /* Fraktmetode */
       IF NEW KOrdreHode THEN 
       FOR EACH tt_shipping_lines /* NO-LOCK WHERE DECIMAL(TT_shippingLineItem.lineItemPrice) <> 0 */ :
         ASSIGN cFraktMetode = tt_shipping_lines.method_title.
         /* Här avgör vi om hämtning i butik */
         IF tt_shipping_lines.method_id = "local_pickup" THEN DO:
             KOrdreHode.SendingsNr = cFraktMetode.
             FIND FIRST syspara  WHERE SysPara.sysHId = 150 AND
                                       SysPara.SysGr  = 16  AND
                                       SysPara.Parameter1 = tt_shipping_lines.instance_id NO-LOCK NO-ERROR.
/*              "instance_id": "5", */
             IF AVAIL SysPara THEN DO:
                 KOrdreHode.Butik = SysPara.ParaNr.
                 /* cWebSubType = 1 betyder bara varureservation i bestämd butik */
                 IF lReservationsweb THEN
                     RETURN.
             END.
         END.

              

              /*          FIND FIRST Moms NO-LOCK WHERE                                                     */
/*            Moms.MomsProc = DECIMAL(REPLACE(TT_shippingLineItem.taxRate,',','.')) NO-ERROR. */
         FIND LAST KOrdrelinje NO-LOCK WHERE
           KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
         IF AVAILABLE KOrdreLinje THEN 
           iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
         ELSE
           iLinjeNr = 1.
         RUN SkapaKOLinje ("Frakt",iLinjeNr).

         IF cFraktMetode <> "" THEN DO:
             FIND FIRST Leveringsform NO-LOCK WHERE Leveringsform.LevFormMetode = cFraktMetode NO-ERROR.
             IF AVAILABLE Leveringsform THEN
                 KOrdreHode.LevFNr = Leveringsform.LevFNr.
             ELSE DO:
                 FIND LAST LeveringsForm NO-LOCK NO-ERROR.
                 IF AVAILABLE LeveringsForm THEN
                     KOrdreHode.LevFNr = LeveringsFor.LevFNr + 1.
                 ELSE
                     KOrdreHode.LevFNr = 1.
                 CREATE LeveringsForm.
                 ASSIGN LeveringsForm.LevFNr             = KOrdreHode.LevFNr
                        LeveringsForm.LevFormMetode      = cFraktMetode
                        LeveringsForm.LevFormBeskrivelse = cFraktMetode.
             END. 
         END.
       END. /* Fraktmetode */.
     END. /* OPPRETTELSE_ORDRE */
     RUN bibl_logg.p ('Nettbutikk_Ordreimport', 'readWooComOrder.p: Ordre behandling. ' + ' Ny ordre: ' + STRING(NEW KOrdreHode) + ' Kordre_Id: ' + STRING(KOrdreHode.KOrdre_ID) + ' og EksterntId: ' + STRING(KOrdreHode.EkstOrdreNr) + ' Kunde: ' + STRING(KOrdreHode.KundeNr)).
     IF NEW KOrdreHode THEN 
     IF tt_Order.payment_method <> "" THEN DO:
       ASSIGN cBetType = tt_Order.payment_method NO-ERROR.
       FIND LAST KOrdrelinje NO-LOCK WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id NO-ERROR.
       IF AVAILABLE KOrdreLinje THEN 
         iLinjeNr = KOrdreLinje.KOrdreLinjeNr + 1.
       ELSE
         iLinjeNr = 1.
       RUN SkapaKOLinje ("Payment",iLinjeNr).
       RUN SetBetType(cBetType).
/*        ASSIGN cBetType = tt_paymentLineItem.bbsEpayment.                 */
/*        CASE cBetType:                                                    */
/*          WHEN 'Epayment Pro' THEN                                        */
/*                            ASSIGN KORdreLinje.Varetekst = 'BBS Betaling' */
/*                                   KORdreLinje.Varetekst = 'BETALT'.      */
/*        END CASE.                                                         */
     END. /* Betaling */
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetBetType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBetType Procedure 
PROCEDURE SetBetType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cBetType AS CHARACTER   NO-UNDO.
         CASE cBetType:
           WHEN 'bbsEpayment' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betalt bbsEPayment'
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1.
           WHEN 'bbsNetaxept' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betalt bbsNetaxept'
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1.
           WHEN 'Klarna' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (Klarna)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'kco' THEN 
                         ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (kco)' 
                                KORdreLinje.VareNr    = 'BETALT'
                                KOrdreLinje.Antall    = 1
                                 .
           WHEN 'klarna_checkout' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (KlarnaCheckOut)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'ecster' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Ecster Pay' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'paypal' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betalt via PayPal' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'zaarpay' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Faktureras via Klarna (zaarPay)' 
                                   KORdreLinje.VareNr    = 'BETALT'
                                   KOrdreLinje.Antall    = 1
                                   .
           WHEN 'Invoice' THEN 
                            ASSIGN KORdreLinje.Varetekst     = 'Betales via faktura kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr        = 'KREDIT'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           WHEN 'Faktura' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Betales via faktura kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr    = 'KREDIT'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           WHEN 'postoppkrav' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Postoppkrav kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr    = 'POSTOPPKRAV'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           WHEN 'oppkrav' THEN 
                            ASSIGN KORdreLinje.Varetekst = 'Postoppkrav kr: ' + STRING(ABS(KOrdreLinje.Linjesum))
                                   KORdreLinje.VareNr    = 'POSTOPPKRAV'
                                   KOrdreLinje.MomsKod       = 0
                                   KOrdreLinje.NettoPris     = 0
                                   KOrdreLinje.MvaKr         = 0 
                                   KORdreLinje.Mva%          = 0 
                                   KOrdreLinje.NettoLinjesum = 0
                                   KOrdreLinje.BruttoPris    = 0  
                                   KOrdreLinje.Pris          = 0 
                                   KOrdreLinje.Linjesum      = 0
                                   KOrdreLinje.Antall        = 1
                                   .
           OTHERWISE 
               ASSIGN KORdreLinje.Varetekst = 'Ukjent betalingsmåte'
                      KORdreLinje.VareNr    = 'BETALT'.
         END CASE.         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaDS) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaDS Procedure 
PROCEDURE SkapaDS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DATASET dsttWooComOrder:READ-JSON("longchar",lcDataSet,"EMPTY").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaKOLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaKOLinje Procedure 
PROCEDURE SkapaKOLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cTyp AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iLinjeNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER   NO-UNDO.
    IF cTyp = "Produkt" THEN DO:
        IF AVAIL Artbas AND AVAIL StrKonv THEN DO:
            FIND FIRST Strekkode OF artbas WHERE StrekKode.StrKode = strkonv.strkode NO-LOCK NO-ERROR.
            IF AVAIL StrekKode THEN
                cKode = strekkode.kode.
        END.
        CREATE KOrdreLinje.
        ASSIGN
        KOrdreLinje.KOrdre_ID         = KOrdreHode.KOrdre_Id
        KOrdreLinje.KOrdreLinjeNr     = iLinjeNr 
        KOrdreLinje.VareNr            = IF AVAIL artbas THEN STRING(Artbas.ArtikkelNr) ELSE "999999999"
        KOrdreLinje.Kode              = cKode
        KOrdreLinje.Varetekst         = (IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '** Ukjent ' + KOrdreLinje.VareNr)
        KOrdreLinje.Antall            = tt_line_items.quantity
        KOrdreLinje.NettoPris         = tt_line_items.pris - tt_line_items.rabatt
        KOrdreLinje.NettoLinjesum     = KOrdreLinje.NettoPris
        KOrdreLinje.LinjeRabattKr     = tt_line_items.rabatt
        KOrdreLinje.LinjeRab%         = (KOrdreLinje.LinjeRabattKr / (KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr)) * 100  
        KOrdreLinje.LinjeRab%         = IF KOrdreLinje.LinjeRab% = ? THEN 0 ELSE KOrdreLinje.LinjeRab%  
    
        KOrdreLinje.MomsKod           = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
        /*KOrdreLinje.MvaKr             = DECIMAL(tt_ProductLineItem.taxAmount)*/
        KOrdreLinje.MvaKr             = tt_line_items.mvakr
        KOrdreLinje.Storl             = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
        KOrdreLinje.Mva%              = IF AVAIL artpris THEN artpris.mva%[1] ELSE 25
        KOrdreLinje.StrKode           = iStrKode
        KOrdreLinje.VareKost          = (IF AVAILABLE Lager THEN Lager.VVareKost ELSE 0)

        KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = ? OR KOrdreLinje.VareKost <= 0) THEN 0 ELSE KOrdreLinje.VareKost
        KOrdreLinje.VareKost          = IF (KOrdreLinje.VareKost = 0 AND AVAILABLE ArtPris) THEN  ArtPris.VareKost[1] ELSE KOrdreLinje.VareKost

        KOrdreLinje.Pris              = tt_line_items.pris
        KOrdreLinje.BruttoPris        = KOrdreLinje.pris
        KOrdreLinje.Linjesum          = KOrdreLinje.NettoLinjesum + KOrdreLinje.LinjeRabattKr
        KOrdreLinje.DbKr              = KOrdreLinje.NettoLinjesum - (KOrdreLinje.VareKost * KOrdreLinje.Antall)
        KOrdreLinje.Db%               = (KOrdreLinje.DbKr / KOrdreLinje.NettoLinjesum) * 100
        KOrdreLinje.Db%               = (IF KOrdreLinje.Db% = ? THEN 0 ELSE KOrdreLinje.Db%)
        KOrdreLinje.RefNr             = 0
        KOrdreLinje.RefTekst          = ''
        KOrdreLinje.Bestillingsnummer = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE ''
        KOrdreLinje.LevFargKod        = IF AVAILABLE ArtBas THEN ArtBas.LevFargKod ELSE ''
        KOrdreLinje.ValKod            = KOrdreHode.ValKod
        NO-ERROR.
    END.
    ELSE IF cTyp = "Frakt" THEN DO:
        FIND FIRST SysPara NO-LOCK WHERE
          SysPara.SysHId       = 150 AND  
          SysPara.SysGr        = 10 AND  
          SysPara.Beskrivelse  = tt_shipping_lines.method_title NO-ERROR.
        IF NOT AVAIL SysPara THEN
            FIND FIRST SysPara NO-LOCK WHERE
              SysPara.SysHId       = 150 AND  
              SysPara.SysGr        = 10 NO-ERROR.
/*             AND                                         */
/*               SysPara.Beskrivelse  = "Posten" NO-ERROR. */

        CREATE KOrdreLinje.
        ASSIGN             
          KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
          KOrdreLinje.KOrdreLinjeNr = iLinjeNr
          KOrdreLinje.Varetekst     = 'FRAKT - ' + tt_shipping_lines.method_title 
          /* ArtikkelNr */
          KOrdreLinje.VareNr        = (IF AVAILABLE Syspara THEN Syspara.Parameter1 ELSE '')
          /* Betaling */
          KOrdreLinje.MomsKod       = (IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0)
          KOrdreLinje.MvaKr         = TT_shipping_lines.total_tax
          KOrdreLinje.Mva%          = IF TT_shipping_lines.total_tax > 0 AND TT_shipping_lines.dTotal > 0 THEN
                                             ROUND(TT_shipping_lines.total_tax / TT_shipping_lines.dTotal,2) * 100 ELSE 0
          KOrdreLinje.Antall        = 1
          KOrdreLinje.NettoPris     = TT_shipping_lines.dTotal + TT_shipping_lines.total_tax
          KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
          KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
          KOrdreLinje.Pris          = KOrdreLinje.NettoPris /*  + KOrdreLinje.MvaKr */
          KOrdreLinje.Storl         = IF KOrdreLinje.NettoPris > 0 THEN " 1" ELSE ""
          KOrdreLinje.Linjesum      = KOrdreLinje.NettoLinjesum  /* + KOrdreLinje.MvaKr */
          KOrdreLinje.plockstatus   = IF KOrdreHode.Butik > 0 THEN -1 ELSE 0 /* Special för c&c - Frakt*/
          .   

        IF KOrdreLinje.Mva% = 0 THEN 
          DO:
            ASSIGN 
            KOrdreLinje.Mva%          = 25
            KOrdreLinje.MvaKr         = KOrdreLinje.NettoLinjesum - (KOrdreLinje.NettoLinjesum / (1 + (KOrdreLinje.Mva% / 100)))
            .             
          END.         
    END.
    IF cTyp = "Payment" THEN DO:
/*         FIND FIRST Moms NO-LOCK WHERE                                                    */
/*           Moms.MomsProc = DECIMAL(REPLACE(tt_paymentLineItem.taxRate,',','.')) NO-ERROR. */
      
      CREATE KOrdreLinje.
      ASSIGN
        KOrdreLinje.KOrdre_ID     = KOrdreHode.KOrdre_Id
        KOrdreLinje.KOrdreLinjeNr = iLinjeNr 
        /* Betaling */
        KOrdreLinje.MomsKod       = IF AVAILABLE Moms THEN Moms.MomsKod ELSE 0
        KOrdreLinje.Antall        = 1
        KOrdreLinje.NettoPris     = DECIMAL(REPLACE(tt_Order.TOTAL,".",",")) * -1
/*         KOrdreLinje.MvaKr         = (DECIMAL(tt_paymentLineItem.basePrice) -                                           */
/*                                     (                                                                                  */
/*                                      DECIMAL(tt_paymentLineItem.basePrice) / (1 + DECIMAL(tt_paymentLineItem.taxRate)) */
/*                                     )) * -1                                                                            */
/*         KORdreLinje.Mva%          = DECIMAL(tt_paymentLineItem.taxRate) * 100                                          */
        KOrdreLinje.NettoLinjesum = KOrdreLinje.NettoPris
        KOrdreLinje.BruttoPris    = KOrdreLinje.NettoPris  
        KOrdreLinje.Pris          = KOrdreLinje.NettoPris 
        KOrdreLinje.Linjesum      = KOrdreLinje.NettoPris
        KOrdreLinje.plockstatus   = IF KOrdreHode.Butik > 0 THEN -1 ELSE 0 /* Special för c&c - Betal */ .
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaKund) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaKund Procedure 
PROCEDURE SkapaKund :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE plKundeNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plKortNr AS INTEGER NO-UNDO.
  DEFINE VARIABLE plMedlemsNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE bOk AS LOG NO-UNDO.
  DEFINE VARIABLE cMsgs AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPersonNr AS CHARACTER NO-UNDO. 
  DEFINE VARIABLE cPostNr AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPostSted AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iKundeButik AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cMKortNr      AS CHAR      FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cBongTekst    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cNavn         AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cStatus       AS CHAR      FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE cMelding      AS CHARACTER FORMAT "x(40)" NO-UNDO.

/*
  OUTPUT cMKortNr,
  OUTPUT cBongTekst,
  OUTPUT cNavn,                                
  OUTPUT cStatus,
  OUTPUT bOk,
  OUTPUT cMelding
  */
  
  /* Kobler medlem til medlemsklubb ut fra hvilken butikk det kommer inn data fra. */
    iKundeButik = bNettbutikk.Butik.

    FIND FIRST SysPara NO-LOCK WHERE
        SysPara.SysHId = 14 AND
        SysPara.SysGr  = 1 AND
        SysPara.ParaNr >= 31 AND 
        SysPara.ParaNr <= 39 AND
        CAN-DO(SysPara.Parameter1,STRING(iKundeButik)) NO-ERROR.
/*         CAN-DO(SysPara.Parameter1,STRING(iCl)) NO-ERROR. */
    IF AVAILABLE SysPara THEN
        ASSIGN 
            cMKlubbId      = SysPara.Parameter2
            .  
    IF NOT AVAILABLE SysPara OR 
        cMKlubbId = ''
        THEN 
    DO:
      {syspara.i 14 1 7 cMKlubbId}
    END.
   
   DO TRANSACTION:
       /* Henter eksisterende eller oppretter ny kunde. */
       FIND FIRST Kunde EXCLUSIVE-LOCK WHERE
           Kunde.EksterntKundeNr = STRING(tt_order.number) AND Kunde.ButikkNr = iKundeButik NO-ERROR.
       /* NB: Kundenummer og kundekort opprettes automatisk av db trigger c_kunde.p */
       /* Her settes de felt som skal initieres ved ny kunde fra nettbutikk.        */
       IF NOT AVAILABLE Kunde THEN
       DO:

           FIND FIRST KundeType NO-LOCK NO-ERROR.
           FIND FIRST KundeGruppe NO-LOCK NO-ERROR.
           CREATE Kunde.
           ASSIGN
               Kunde.EksterntKundeNr     = STRING(tt_order.number)
               Kunde.Kilde               = 'Nettbutikk'
               Kunde.TilgKilde           = TRIM(tt_Order.bi_first_name + " " + tt_Order.bi_last_name)
               Kunde.WebKunde            = TRUE 
               Kunde.Aktiv               = TRUE
               Kunde.TypeId              = IF AVAILABLE KundeType THEN KundeType.TypeId ELSE 0
               Kunde.GruppeId            = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 0
               Kunde.WebKunde             = TRUE
               Kunde.GruppeId            = iKndGruppeId
               Kunde.TypeId              = iKndTypeId               
               .
       END.
       IF AVAILABLE NumLandKode THEN RELEASE NumLandKode.
       DO:
         FIND NumLandKode NO-LOCK WHERE
              NumLandKode.NumLandKode = INTEGER(tt_Order.bi_country) NO-ERROR.
         IF NOT AVAILABLE NumLandKode THEN  
           FIND AlfaLandKode NO-LOCK WHERE 
                AlfaLandKode.AlfaKode3 = TRIM(tt_Order.bi_country) NO-ERROR.
         IF NOT AVAILABLE NumLandKode AND NOT AVAILABLE AlfaLandKode THEN  
           FIND AlfaLandKode NO-LOCK WHERE 
                AlfaLandKode.AlfaKode2 = TRIM(tt_Order.bi_country) NO-ERROR.
         IF NOT AVAILABLE NumLandKode AND AVAILABLE AlfaLandKode THEN 
           FIND NumLandKode NO-LOCK WHERE
                NumLandKode.NumLandKode = AlfaLandKode.NumLandKode NO-ERROR.                     
        END.       
    
       /* Oppdaterer kunderecord. */
       /* Her settes de felt som skal oppdateres når kunden har oppdatert sin profil på nettet. */
/*        IF tt_Billingaddress.Oppdatert THEN */
       ASSIGN   
           Kunde.Navn         = TRIM(tt_Order.bi_first_name + " " + tt_Order.bi_last_name)
           Kunde.KontNavn     = Kunde.Navn
           Kunde.Navn         = IF Kunde.Navn = '' THEN Kunde.KontNavn ELSE Kunde.Navn                                                     
           Kunde.Adresse1     = tt_Order.bi_address_1
           Kunde.Adresse2     = tt_Order.bi_address_2           
           Kunde.PostNr       =  tt_Order.bi_postcode
           Kunde.Land         = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
/*            Kunde.Region       = tt_billingAddress.state */
           Kunde.ButikkNr     = IF iKundeButik > 0 THEN iKundeButik ELSE iCL                       
           Kunde.Telefon      = REPLACE(tt_Order.bi_phone," ","")
           Kunde.TelefonFirma = REPLACE(tt_Order.bi_phone," ","")
           Kunde.Telefaks     = REPLACE(tt_Order.bi_phone," ","")
           Kunde.MobilTlf     = REPLACE(tt_Order.bi_phone," ","")
           Kunde.ePostAdresse = tt_Order.bi_email
           Kunde.eMailFirma   = tt_Order.bi_email
           Kunde.KontE-Post   = tt_Order.bi_email
           Kunde.KontTelefon  = REPLACE(tt_Order.bi_phone," ","")
/*            Kunde.Stilling     = tt_billingAddress.jobTitle */
/*            Kunde.Kjon         = IF INTEGER(tt_billingAddress.Gender) <= 2 THEN INTEGER(tt_billingAddress.Gender) ELSE 0 */
/*            Kunde.BankKonto    = tt_billingAddress.bankAccountNo */
           Kunde.Postgiro     = ''
           Kunde.OrgNr        = tt_Order.personnummer
           Kunde.OrgNr        = REPLACE(Kunde.OrgNr,'-','')
           Kunde.OrgNr        = IF LENGTH(Kunde.OrgNr) = 12 THEN SUBSTR(Kunde.OrgNr,3) ELSE Kunde.OrgNr
/*            Kunde.MottaeMailUtsendelser = (IF tt_billingAddress.MottaeMailUtsendelser = 'yes' THEN TRUE ELSE FALSE) */
           Kunde.Kommentar    = tt_Order.customer_note
/*            Kunde.WebKanSendeEMail = IF tt_Customer.isHtmlEMailAllowed = 'true' THEN TRUE ELSE FALSE */
/*            Kunde.WebKanSetteOrdre = IF tt_Customer.isDoOrderAllowed = 'true' THEN TRUE ELSE FALSE   */
           Kunde.ByNavn       = tt_Order.bi_city
/*            Kunde.Avdeling     = tt_billingAddress.departmen  */
/*            Kunde.Tittel       = tt_billingAddress.ctitle     */
/*            Kunde.UrlFirma     = tt_billingAddress.cURL       */
/*            Kunde.Hilsen       = tt_billingAddress.salutation */
/*            Kunde.BankNavn     = tt_billingAddress.bankName   */
/*            Kunde.BankKode     = tt_billingAddress.bankCode   */
           /* Fakturaadressen settes til det samme som firmaadressen */
           Kunde.FaktAdresse1 = tt_Order.sh_address_1
           Kunde.FaktAdresse2 = tt_Order.sh_address_1
           Kunde.FaktPostNr   = REPLACE(tt_Order.sh_postcode,' ','')
           Kunde.FaktLand     = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
       .
       IF TRIM(Kunde.MobilTlf) = '' THEN Kunde.MobilTlf = REPLACE(tt_Order.bi_phone," ","").
       
       IF bSvenskFormat THEN 
       DO:
         IF LENGTH(Kunde.PostNr) = 5 THEN Kunde.PostNr = SUBSTRING(Kunde.PostNr,1,3) + ' ' + SUBSTRING(Kunde.PostNr,4,2).
         IF LENGTH(Kunde.FaktPostNr) = 5 THEN Kunde.FaktPostNr = SUBSTRING(Kunde.FaktPostNr,1,3) + ' ' + SUBSTRING(Kunde.FaktPostNr,4,2).
         IF LENGTH(Kunde.LevPostNr) = 5 THEN Kunde.LevPostNr = SUBSTRING(Kunde.LevPostNr,1,3) + ' ' + SUBSTRING(Kunde.LevPostNr,4,2).
       END.
       
       /* Egen assign for å kunne håndtere feil i datokonvertering. */
/*        ASSIGN */
/*        Kunde.FodtDato = DATE(REPLACE(ENTRY(1,tt_billingAddress.birthday,' '),'.','/')) */
/*            Kunde.Alder    = YEAR(Kunde.FodtDato) - YEAR(TODAY) + 1                     */
/*            NO-ERROR.                                                                   */
/*        IF TT_ShippingAdress.Oppdatert THEN                                                        */
/*        ASSIGN                                                                                     */
/*            Kunde.LevAdresse1   = TT_ShippingAdress.street                                         */
/*            Kunde.LevAdresse2   = ''                                                               */
/*            Kunde.LevPostNr     = TT_ShippingAdress.zipcode                                        */
/*            Kunde.LevLand       = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land) */
/*            .                                                                                      */
/*        /* Er det lagt inn en ekstra leveringadresse, skal denne benyttes. */                      */
/*        IF TT_UserShippingAdress.Oppdatert THEN                                                    */
/*        ASSIGN                                                                                     */
/*            Kunde.LevAdresse1   = TT_UserShippingAdress.street                                     */
/*            Kunde.LevAdresse2   = ''                                                               */
/*            Kunde.LevPostNr     = TT_UserShippingAdress.zipcode                                    */
/*            Kunde.LevLand       = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land) */
/*            .                                                                                      */
       FIND FIRST KundeKort OF Kunde NO-LOCK NO-ERROR.
       IF AVAILABLE KundeKort AND NOT CAN-FIND(FIRST Medlem WHERE Medlem.KundeNr = Kunde.KundeNr) THEN
       KNDKORT: 
       DO:       
            IF AVAILABLE Medlem THEN RELEASE Medlem.
            
            ASSIGN
            plKortNr  = INT(KundeKort.KortNr)
            plKundeNr = Kunde.KundeNr 
            cPersonNr = tt_Order.personnummer /* (IF tt_billingAddress.VATID <> '' THEN tt_billingAddress.VATID ELSE '') */
            cPersonNr = REPLACE(cPersonNr,'-','')
            cPersonNr = IF LENGTH(cPersonNr) = 12 THEN SUBSTR(cPersonNr,3) ELSE cPersonNr
            NO-ERROR.
            IF ERROR-STATUS:ERROR THEN LEAVE KNDKORT.
            /* Oppretter medlem via SPAR. */
            IF LENGTH(cPersonNr) = 10 THEN 
            DO:
                RUN asMedlem.p (IF iKundeButik > 0 THEN iKundeButik ELSE iCl,
                                99,
                                cPersonNr, 
                                USERID('SkoTex'),
                                OUTPUT plMedlemsNr,
                                OUTPUT cMKortNr,
                                OUTPUT cBongTekst,
                                OUTPUT cNavn,                                
                                OUTPUT cStatus,
                                OUTPUT bOk,
                                OUTPUT cMelding
                               ).
                FIND Medlem EXCLUSIVE-LOCK WHERE
                    Medlem.MedlemsNr = DEC(cPersonNr + cMKlubbId) NO-ERROR.
                IF AVAILABLE Medlem THEN 
                DO:
                    ASSIGN 
                    Medlem.KundeNr = Kunde.KundeNr
                    plMedlemsNr    = Medlem.MedlemsNr
                    .
                    FIND FIRST MedlemsKort OF Medlem EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE MedlemsKort THEN 
                      MedlemsKort.InterntKKortId = KundeKort.InterntKKortId.
                END. 
            END. 
            
            IF plMedlemsNr = 0 THEN 
            DO:
                /* Bare medlem skal opprettes. */
                RUN genkundeMedlem.p (IF iKundeButik > 0 THEN iKundeButik ELSE iCL,
                                      iKndGruppeId,
                                      INPUT-OUTPUT plKundeNr,
                                      OUTPUT plMedlemsNr,
                                      OUTPUT bOk,
                                      OUTPUT cMsgs).
                /* Kundekortene legges opp på samme kunde, men medlemskortene */
                /* legges på separate medlemmer.                              */
                RUN genkundekort_og_medlem.p (IF iKundeButik > 0 THEN iKundeButik ELSE iCL,
                                              plKundeNr,
                                              plMedlemsNr,
                                              plKortNr,
                                              plKortNr,
                                              999,
                                              OUTPUT bOk,
                                              OUTPUT cMsgs).
            END.
            
            IF plMedlemsNr > 0 THEN 
            FIND Medlem EXCLUSIVE-LOCK WHERE 
              Medlem.MedlemsNr = plMedlemsNr NO-ERROR.
            IF AVAILABLE Medlem THEN
            DO: 
/*               ASSIGN Medlem.MKlubbId = INT(tt_billingAddress.MedlemsKlubb) NO-ERROR. */
              IF Medlem.MKlubbId = 0 THEN 
                Medlem.MKlubbId = iStdMKlubbId.
                
              /* Oppdaterer annen medlemsinfo fra kunde. */
              ASSIGN
              Medlem.ForNavn   = tt_Order.bi_first_name
              Medlem.Etternavn = tt_Order.bi_last_name
              NO-ERROR.
              
/*               ASSIGN                                                                    */
/*               Medlem.Fodselsdato = DATE(tt_billingAddress.birthday) NO-ERROR.           */
/*               IF Medlem.Fodselsdato <> ? THEN Medlem.FodtAr = YEAR(Medlem.Fodselsdato). */
              
              ASSIGN 
              Medlem.Personnr          = (IF cPersonNr <> '' THEN cPersonNr ELSE Kunde.OrgNr)
              Medlem.Adresse1          = tt_Order.bi_address_1
              Medlem.Adresse2          = tt_Order.bi_address_2
              Medlem.PostNr            = REPLACE(tt_Order.bi_postcode,' ','')
              Medlem.Land              = (IF AVAILABLE NumLandKode THEN NumLandKode.Land ELSE Kunde.Land)
              Medlem.ePostAdresse      = tt_Order.bi_email
              Medlem.Telefon           = REPLACE(tt_Order.bi_phone," ","")
              Medlem.MobilTlf          = REPLACE(tt_Order.bi_phone," ","")
              Medlem.Telefaks          = REPLACE(tt_Order.bi_phone," ","")
              Medlem.AktivertFraWeb    = TODAY 
              Medlem.ButikkNr          = iCL
              Medlem.Kilde             = 'Nettbutikk'
/*               Medlem.EksterntMedlemsNr = tt_Customer.KundeNr */
              NO-ERROR.
              
              IF LENGTH(cPersonNr) = 10 THEN 
                  Medlem.Kjonn = Medlem.Kjonn.
/*               ELSE Medlem.Kjonn             = IF INTEGER(tt_billingAddress.Gender) = 1 THEN TRUE  */
/*                                          ELSE IF INTEGER(tt_billingAddress.Gender) = 2 THEN FALSE */
/*                                          ELSE ?.                                                  */
             
             IF bSvenskFormat AND LENGTH(Medlem.PostNr) = 5 THEN 
               IF LENGTH(Medlem.PostNr) = 5 THEN Medlem.PostNr = SUBSTRING(Medlem.PostNr,1,3) + ' ' + SUBSTRING(Medlem.PostNr,4,2).
              
              FIND FIRST Medlemskort OF Medlem EXCLUSIVE-LOCK NO-ERROR.
              IF AVAILABLE MedlemsKort THEN 
                Medlemskort.Innehaver = Kunde.Navn.
            END.
            IF NOT CAN-FIND(Post WHERE 
                Post.PostNr = Medlem.PostNr) THEN 
                DO:
                    CREATE Post.
                    ASSIGN 
                        Post.PostNr      = Medlem.PostNr
                        Post.Beskrivelse = REPLACE(tt_Order.bi_city,' ','').
                    RELEASE Post.
                END.
       END. /* KNDKORT */ 
       ELSE IF AVAILABLE KundeKort AND CAN-FIND(FIRST Medlem WHERE Medlem.KundeNr = Kunde.KundeNr) THEN
       DO:
           FIND FIRST Medlem EXCLUSIVE-LOCK WHERE 
             Medlem.KundeNr = KundeKort.KundeNr NO-ERROR.
           IF AVAILABLE Medlem THEN 
           DO:
               cPersonNr = REPLACE((IF tt_Order.personnummer <> '' THEN tt_Order.personnummer ELSE ''),'-','').
               IF Medlem.PersonNr = '' 
                 THEN ASSIGN 
                        Medlem.PersonNr = cPersonNr
                        plMedlemsNr     = Medlem.MedlemsNr.
/*                ASSIGN Medlem.MKlubbId = INT(tt_billingAddress.MedlemsKlubb). */
           END.
       END.       
   END. /* TRANSACTION */
   
    IF plMedlemsNr > 0 THEN 
      RUN konverter_medlemsnr_personnr.p (INPUT-OUTPUT plMedlemsNr).
   
   IF AVAILABLE MedlemsKort THEN RELEASE MedlemsKort.
   IF AVAILABLE Medlem      THEN RELEASE Medlem.
   IF AVAILABLE Kunde       THEN RELEASE Kunde.
   IF AVAILABLE KundeKort   THEN RELEASE KundeKort.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-betDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION betDato Procedure 
FUNCTION betDato RETURNS DATE
    ( INPUT cYYYYMMDD AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes: cYYYYMMDD är separerad med "-" 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dDato AS DATE NO-UNDO.
    dDato = DATE(INT(ENTRY(2,cYYYYMMDD,"-")),INT(ENTRY(3,cYYYYMMDD,"-")),INT(ENTRY(1,cYYYYMMDD,"-"))) NO-ERROR.   /* Function return value. */
    RETURN dDato.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTid Procedure 
FUNCTION getTid RETURNS INTEGER
    ( INPUT cHHMMSS AS CHARACTER ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes: cHHMMSS är separerad med ":" (kolon) 
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTid AS INTEGER INIT ? NO-UNDO.
    iTid = INT(ENTRY(1,cHHMMSS,":")) * 3600 + INT(ENTRY(2,cHHMMSS,":")) * 60 + INT(ENTRY(3,cHHMMSS,":")) NO-ERROR.   /* Function return value. */
    RETURN iTid.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentDato) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentDato Procedure 
FUNCTION hentDato RETURNS DATE
( INPUT cDatoTid AS CHAR ):

/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

DEFINE VARIABLE dDato AS DATE NO-UNDO.

IF cDatoTid = '' THEN
    dDato = ?.
ELSE dDato = DATE(REPLACE(ENTRY(1,cDatoTid),'.','/')) NO-ERROR.
    RETURN dDato.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentSisteEntry) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentSisteEntry Procedure 
FUNCTION hentSisteEntry RETURNS CHARACTER
( INPUT cTekst AS CHARACTER ):

/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes:                                                                                                                                            
------------------------------------------------------------------------------*/

DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

IF cTekst = '' AND NUM-ENTRIES(cTekst,'/') = 0 THEN
    cResult = ''.
ELSE cResult = ENTRY(NUM-ENTRIES(cTekst,'/'),cTekst,'/').    
    RETURN cResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-hentTid) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION hentTid Procedure 
FUNCTION hentTid RETURNS INTEGER
        ( INPUT cDatoTid AS CHARACTER ):

/*------------------------------------------------------------------------------
                Purpose:                                                                                                                                          
                Notes: <creationDate>24.04.2009 22:30:15</creationDate>                                                                                                                                           
------------------------------------------------------------------------------*/

DEFINE VARIABLE iTid AS INTEGER NO-UNDO.
DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.

IF cDatoTid = '' THEN
    iTid = 0.
ELSE ASSIGN
    cTid = ENTRY(2,cDatoTid)
    iTid = INT(ENTRY(1,cTid,":")) * 3600 + INT(ENTRY(2,cTid,":")) * 60 + INT(ENTRY(3,cTid,":")) NO-ERROR. 
RETURN iTid.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

