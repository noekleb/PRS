&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : asMedlem 
    Purpose     : Oppslag / opprettelse av medlemmer.

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER iButikkNr     AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER iKasseNr      AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER cPersonNr     AS CHAR        NO-UNDO.
DEFINE INPUT  PARAMETER cBrukerId     AS CHAR        NO-UNDO.
DEFINE INPUT  PARAMETER iMKlubb       AS INTEGER     NO-UNDO.

DEFINE OUTPUT PARAMETER lMedlemsNr    AS DEC       FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE OUTPUT PARAMETER cMKortNr      AS CHAR      FORMAT "x(30)" NO-UNDO.
DEFINE OUTPUT PARAMETER cBongTekst    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE OUTPUT PARAMETER cNavn         AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE OUTPUT PARAMETER cMobil        AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE OUTPUT PARAMETER cEmail        AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE OUTPUT PARAMETER cStatus       AS CHAR      FORMAT "x(10)" NO-UNDO.
DEFINE OUTPUT PARAMETER lDoUpdate     AS LOGICAL     NO-UNDO.
  /* Status = 0, ukjent medlem.                     */
  /* Status = 1, Medelm funnet i SPAR og opprettet. */
  /* Status = 2, Medlemmet funnet lokalt.           */
DEFINE OUTPUT PARAMETER bOK           AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER cMelding      AS CHARACTER FORMAT "x(40)" NO-UNDO.

DEFINE VARIABLE cSprak       AS CHAR NO-UNDO.
DEFINE VARIABLE cMsgInnlogg  AS CHAR NO-UNDO.
DEFINE VARIABLE cMsgNyMedlem AS CHAR NO-UNDO.
DEFINE VARIABLE cMsgDefault  AS CHAR NO-UNDO.
DEFINE VARIABLE cOrgPersonNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMKlubbId    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE iAlder       AS INTEGER. /* Minstealder. */ 
DEFINE VARIABLE cButKlubbListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLengdeLst  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iIdx        AS INTEGER     NO-UNDO.
{syspara.i 14 1 25 cLengdeLst}
IF cLengdeLst = '' THEN 
    cLengdeLst = '10'.

/* Kobler medlem til medlemsklubb ut fra hvilken butikk det kommer inn data fra. */
FIND FIRST SysPara NO-LOCK WHERE
    SysPara.SysHId = 14 AND
    SysPara.SysGr  = 1 AND
    SysPara.ParaNr >= 31 AND 
    SysPara.ParaNr <= 39 AND
    CAN-DO(SysPara.Parameter1,STRING(iButikkNr)) NO-ERROR.
IF AVAILABLE SysPara THEN
  ASSIGN 
      cButKlubbListe = SysPara.Parameter1
      cMKlubbId      = SysPara.Parameter2
      .  
/* om vi inte hittat så ser vi efter avvik butik/kassa */
IF cMKlubbId = '' THEN DO:
    FIND FIRST SysPara NO-LOCK WHERE
        SysPara.SysHId = 14 AND
        SysPara.SysGr  = 10 AND
        SysPara.ParaNr = iButikkNr NO-ERROR.
    IF AVAIL Syspara THEN DO:
        iIdx = LOOKUP(STRING(iKassenr),SysPara.Parameter1).
        IF iIdx > 0 THEN DO:
            IF NUM-ENTRIES(SysPara.Parameter2) >= iIdx THEN
                cMKlubbId = ENTRY(iIdx,SysPara.Parameter2) NO-ERROR.
            IF INT(cMKlubbId) = 0 OR INT(cMKlubbId) = ? THEN
                cMKlubbId = "".
        END.
    END.
END.

IF NOT AVAILABLE SysPara OR 
  cMKlubbId = ''
THEN DO:
    {syspara.i 14 1 7 cMKlubbId}
END.

IF iMKlubb > 0 THEN
    cMKlubbId = STRING(iMKlubb).


ASSIGN
cOrgPersonNr = cPersonNr.

/* Legger på årstall på svensk personnumer før oppslag i SPAR. */
IF LENGTH(cPersonNr) = 10 THEN 
ASSIGN
    cPersonNr    = IF YEAR(TODAY) - INT('19' + SUBSTRING(cPersonNr,1,2)) > 100
                     THEN "20" + cPersonNr
                   ELSE "19" + cPersonNr.

/* Jedviks */
/* IF CAN-DO('5,10,12',STRING(iButikkNr)) THEN */
IF cMKlubbId = "4" THEN 
  ASSIGN
  cMsgNyMedlem = 
     'DH;*** NY MEDLEM ***'
     + '|;'
     + '|;MEDLEMSREGISTRERING'
     + '|;JEDVIKS GULDKLUBB'
     + '|;'
     + '|;Förnamn..: &FORNAVN'
     + '|;'
     + '|;Efternamn: &ETTERNAVN'
     + '|;'
     + '|;PersonNr.: &PERSONNR'
     + '|;'
     + '|;Adress...: &ADRESSE'
     + '|;'
     + '|;VIKTIGT! Skriv i dina kontaktuppgifter'
     + '|;'
     + '|;'
     + '|;Email   *................................'
     + '|;'
     + '|;MobilNr *................................'
     + '|;* Obligatoriska uppgifter för anslutning ' 
     + '|;* till Jedviks Guldklubb                 ' 
     + '|;'
     + '|;    Jag har tagit del av villkoren i     '
     + '|;    informationsfoldern och ger mitt     '
     + '|;     medgivande till att Jedviks         '
     + '|;    lagrar mina personuppgifter samt     '
     + '|; registrerar information om alla min köp.'
     + '|; Jag godkänner samtidigt att uppgifterna '
     + '|;får behandlas för marknadsföringsändamål.'
     + '|;'
     + '|;Ort/Datum: ..............................'
    + '|;'
     + '|;Underskrift:............ ................'
  .
ELSE 
  ASSIGN 
  cMsgNyMedlem = 
     'DH;*** NY MEDLEM ***'
     + '|;'
     + '|;MEDLEMSREGISTRERING'
     + '|;CLUB JOHANSSONS'
     + '|;'
     + '|;Förnamn..: &FORNAVN'
     + '|;'
     + '|;Efternamn: &ETTERNAVN'
     + '|;'
     + '|;PersonNr.: &PERSONNR'
     + '|;'
     + '|;Adress...: &ADRESSE'
     + '|;'
     + '|;VIKTIGT! Skriv i dina kontaktuppgifter'
     + '|;så att du inte missar din bonus!'
     + '|;'
     + '|;'
     + '|;Email   *................................'
     + '|;'
     + '|;MobilNr *................................'
     + '|;* Obligatoriska uppgifter för anslutning ' 
     + '|;* till Club Johanssons.                  ' 
     + '|;'
     + '|;    Jag har tagit del av villkoren i     '
     + '|;    informationsfoldern och ger mitt     '
     + '|;     medgivande till att Johanssons      '
     + '|;    lagrar mina personuppgifter samt     '
     + '|; registrerar information om alla min köp.'
     + '|; Jag godkänner samtidigt att uppgifterna '
     + '|;får behandlas för marknadsföringsändamål.'
     + '|;'
     + '|;Ort/Datum: ..............................'
     + '|;'
     + '|;Underskrift:............ ................'
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

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 14 1 9 iAlder INT}
IF iAlder = 0 THEN iAlder = 16.

FIND Bruker WHERE Bruker.Brukerid = cBrukerId NO-LOCK NO-ERROR.
IF AVAILABLE Bruker THEN
DO:
    IF Bruker.lng = "SE" OR Bruker.lng = "SVE" THEN
        cSprak = "".
END.
ELSE cSprak = ''.

ASSIGN 
    bOK      = FALSE
    cStatus  = '0'
    cMelding = '** kontroll av medlems id.'. 

FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
FIND FIRST MedlemsType   NO-LOCK NO-ERROR.

/* Sjekker om legnden på medlemsid er en av de tillatte lengder. */
IF NOT CAN-DO(cLengdeLst,STRING(LENGTH(cOrgPersonNr))) THEN 
    DO:
        ASSIGN
            bOk      = FALSE
            cMelding = '** Fel längd på medlemsid. Tillåtna längder är ' + cLengdeLst + '.'
            cStatus  = '0'
            .
        RETURN.    
    END.

IF LENGTH(cOrgPersonNr) < 10 THEN 
    RUN sjekkLokalt.
IF bOK = TRUE THEN /* Vi hittade */
    RETURN.

IF bOk = FALSE THEN 
    RUN sjekkOmPersNrFinnesLokalt.

/* IF bOK = TRUE THEN */
    RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-settMedlemsData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settMedlemsData Procedure 
PROCEDURE settMedlemsData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT AVAILABLE Medlem AND AVAILABLE Medlemskort THEN
      RETURN.
  ELSE 
  SETT_MEDLEMSDATA:
  DO:
    ASSIGN
        lMedlemsNr = Medlem.MedlemsNr
        cMKortNr   = MedlemsKort.KortNr
        cNavn      = Medlem.ForNavn + ' ' + Medlem.Etternavn
        cMobil     = Medlem.MobilTlf 
        cEmail     = Medlem.ePostAdresse
        cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&FORNAVN',Medlem.ForNavn) 
        cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&ETTERNAVN',Medlem.EtterNavn) 
        cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&PERSONNR',cOrgPersonNr) 
        cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&ADRESSE',Medlem.Adresse1)
        cBongTekst   = cMsgNyMedlem
        .

  END. /* SETT_MEDLEMSDATA */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-settNyttMedlemsNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE settNyttMedlemsNr Procedure 
PROCEDURE settNyttMedlemsNr :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER plMedlemsNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plLoop AS DECIMAL NO-UNDO.
  
  DEFINE BUFFER bufMedlem FOR Medlem.
  
  LOOPEN:
  DO plLoop = 1 TO 9999999999:
      IF NOT CAN-FIND(bufMedlem WHERE 
                      bufMedlem.MedlemsNr = plLoop) THEN 
      DO: 
          plMedlemsNr = plLoop.
          LEAVE LOOPEN.       
      END.    
  END. /* LOOPEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkAlder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkAlder Procedure 
PROCEDURE sjekkAlder :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dFDato AS DATE. /* Fødselsdato */
  DEFINE VARIABLE dGDato AS DATE. /* Grensedato  */
        
  ASSIGN
/*     iAlder    = 16 */
    dFDato    = DATE(INT(SUBSTRING(cPersonNr,5,2)),
                     INT(SUBSTRING(cPersonNr,7,2)),
                     INT(SUBSTRING(cPersonNr,1,4))
                     )
    dGDato    = DATE(MONTH(TODAY),
                     DAY(TODAY),
                     YEAR(TODAY) - iAlder
                     )
    NO-ERROR. 
  IF ERROR-STATUS:ERROR THEN 
  DO:
      ASSIGN 
      bOk      = FALSE
      cStatus  = "0"
      cMelding = "Fel datum i personnummer " + cPersonNr + ".". 
      RETURN.
  END.
  
  ELSE IF (dFDato > dGDato) OR dFDato = ? THEN 
  DO:
      ASSIGN bOK      = FALSE
             cStatus  = "0"
             cMelding = "Nya medlemmar måste vara " + STRING(iAlder) + " år eller mer.". 
  END.
  ELSE bOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkLokalt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkLokalt Procedure 
PROCEDURE sjekkLokalt :
/*------------------------------------------------------------------------------
     Purpose:
     Notes: Her kommer vi bare inn når det er medlemsid som ikke er personnr 
            som er angitt. Oppslag her gjøres uten hensyn til klubbid.
    ------------------------------------------------------------------------------*/

    /* Slår opp etter medlemskort. Slike medlemmer skal ha medlemskortnr = medlemsnr (Normalt ansatte).*/
    FIND FIRST Medlem NO-LOCK WHERE 
        Medlem.MedlemsNr = DEC(cOrgPersonNr) 
/*         AND Medlem.MKlubbId = INT(iMKlubb) */
        NO-ERROR.
    IF AVAILABLE Medlem THEN
    HENT_VIA_MEDLEMSKORT:
    DO:
        FIND FIRST MedlemsKort OF Medlem NO-LOCK NO-ERROR.
        IF AVAILABLE Medlem AND AVAILABLE Medlemskort THEN 
        DO:
            RUN settMedlemsData.
            RUN bibl_loggDbFri.p ('asMedlemLogg', 'asmedlem.p: sjekkLokalt: Funnet medlem via medlemskort (Ansatt): ' 
                + STRING(Medlem.MedlemsNr) + ' ' 
                + Medlem.Fornavn + ' ' 
                + Medlem.Etternavn + ' ' 
                + 'KortNr: '  + (IF AVAILABLE MedlemsKort THEN MedlemsKort.KortNr ELSE '') + ' '
                + 'PersNr: '  + cOrgPersonNr + ' '
                + 'KlubbId: ' + STRING(Medlem.MKLubbId) + ' '  
                + 'ButikkNr: ' + STRING(iButikkNr)  
                ).              
        END.
    END. /* HENT_VIA_MEDLEMSKORT */

    IF (AVAILABLE Medlem AND AVAILABLE MedlemsKort) THEN
        ASSIGN
            bOk      = TRUE
            cStatus  = '2'
            cMelding = ''.
    ELSE 
        /* test */
        ASSIGN
            bOk      = FALSE
            cStatus  = '0'
            cMelding = 'Medlemsid inte registrerat lokalt.'
            .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkOmFinnesSPAR) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkOmFinnesSPAR Procedure 
PROCEDURE sjekkOmFinnesSPAR :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE Persondetaljer_Fornamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Efternamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Fodelsetid AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Kon AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Utdelningsadress2 AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_PostNr AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Postort AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordLanKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordKommunKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordForsamlingKod AS CHAR NO-UNDO. 
DEFINE VARIABLE cErrorMessage AS CHAR NO-UNDO.
DEFINE VARIABLE lPersonFunnet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMedlemsNr AS DECIMAL NO-UNDO.
RUN bibl_loggDbFri.p ('asMedlemLogg', 'asmedlem_spar3.p: Stepp 7_0: ' 
    + 'MedemsId: '  + cOrgPersonNr + ' '
    + 'bOk: ' + STRING(bOk) + ' ' 
    + 'Status ' + cStatus + ' ' 
    + 'Melding ' + "Är vi här"
    ).

RUN SPAR_Personsokninfraga.p (
        cPersonNr, /* personnr */
        OUTPUT Persondetaljer_Fornamn,
        OUTPUT Persondetaljer_Efternamn,
        OUTPUT Persondetaljer_Fodelsetid,
        OUTPUT Persondetaljer_Kon,
        OUTPUT Folkbokforingsadress_Utdelningsadress2,
        OUTPUT Folkbokforingsadress_PostNr,
        OUTPUT Folkbokforingsadress_Postort,
        OUTPUT Folkbokforingsadress_FolkbokfordLanKod,
        OUTPUT Folkbokforingsadress_FolkbokfordKommunKod,
        OUTPUT Folkbokforingsadress_FolkbokfordForsamlingKod,
        OUTPUT cErrorMessage,
        OUTPUT lPersonFunnet,
        OUTPUT lSuccess) NO-ERROR. 

IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN bOk      = FALSE
           cStatus  = "0"
           cMelding = "Fel i kontakt med SPAR". 
END.
  
IF lSuccess = TRUE THEN 
DO:
    IF lPersonFunnet THEN 
    DO TRANSACTION:
        FIND FIRST Post WHERE 
          Post.PostNr = Folkbokforingsadress_PostNr NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Post THEN 
        DO:
            CREATE Post.
            ASSIGN
                Post.PostNr      = REPLACE(Folkbokforingsadress_PostNr,' ','')
                Post.Beskrivelse = Folkbokforingsadress_Postort
                .
        END.
        FIND Medlem EXCLUSIVE-LOCK WHERE 
          Medlem.MedlemsNr = DEC(cOrgPersonNr + cMKlubbId) NO-ERROR.
        IF NOT AVAILABLE Medlem THEN 
        DO:
            CREATE Medlem.
            ASSIGN 
                Medlem.MedlemsNr = DEC(cOrgPersonNr + cMKlubbId).
        END.
        ASSIGN 
            Medlem.PersonNr  = cOrgPersonNr
            Medlem.ForNavn   = Persondetaljer_Fornamn
            Medlem.EtterNavn = Persondetaljer_Efternamn
            Medlem.PostNr    = REPLACE(Folkbokforingsadress_PostNr,' ','')
            Medlem.Adresse1  = Folkbokforingsadress_Utdelningsadress2
            Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0
            Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType ELSE 0
            Medlem.MKlubbId  = INT(cMKlubbId)
            Medlem.ButikkNr  = iButikkNr
            Medlem.Kjonn     = CAN-DO('1,3,5,7,9',SUBSTRING(cOrgPersonNr,9,1))
            lMedlemsNr = Medlem.MedlemsNr
            cBongTekst = cMsgNyMedlem
            cNavn      = Medlem.ForNavn + ' ' + Medlem.Etternavn
            . 
            
        FIND MedlemsKort EXCLUSIVE-LOCK WHERE 
          MedlemsKort.KortNr = cOrgPersonNr + cMKlubbId NO-ERROR.
        IF NOT AVAILABLE MedlemsKort THEN 
        DO:    
            CREATE MedlemsKort.
            ASSIGN
                MedlemsKort.MedlemsNr = Medlem.MedlemsNr
                MedlemsKort.KortNr    = cOrgPersonNr + cMKlubbId.
        END.
        ASSIGN    
            MedlemsKort.AktivertDato = TODAY 
            MedlemsKort.UtgarDato    = TODAY + 999
            MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
            MedlemsKort.KortType     = 1
        .
        RUN settMedlemsData.
        
        RUN bibl_loggDbFri.p ('asMedlemLogg', 'asmedlem.p: sjekkOmFinnesSPAR: Funnet medlem via SPAR: ' 
                              + STRING(Medlem.MedlemsNr) + ' ' 
                              + Medlem.Fornavn + ' ' 
                              + Medlem.Etternavn + ' ' 
                              + 'KortNr: '  + MedlemsKort.KortNr + ' '
                              + 'PersNr: '  + cOrgPersonNr + ' '
                              + 'KlubbId: ' + cMKLubbId + ' '  
                              + 'ButikkNr: ' + STRING(iButikkNr)  
                              ).
        
        ASSIGN bOK      = TRUE
               cStatus  = "1"
               cMelding = ""
               . 
    END.
    ELSE DO:
        ASSIGN bOK      = FALSE
               cStatus  = "0"
               cMelding = "Okänd person i SPAR. " + CHR(10) + cErrormessage + CHR(10) + 
        'SPARasLogg asMedlem.p - ' 
        + (IF lSuccess THEN 'SUCCESS' ELSE 'ERROR') + ';' 
        + cPersonNr + ';'  
        + Persondetaljer_Fornamn + ';' 
        + Persondetaljer_Efternamn + ';' 
        + STRING(Persondetaljer_Fodelsetid) + ';' 
        + STRING(Persondetaljer_Kon) + ';' 
        + Folkbokforingsadress_Utdelningsadress2 + ';' 
        + Folkbokforingsadress_PostNr + ';' 
        + STRING(Folkbokforingsadress_Postort) + ';' 
        + STRING(Folkbokforingsadress_FolkbokfordLanKod) + ';' 
        + STRING(Folkbokforingsadress_FolkbokfordKommunKod) + ';' 
        + STRING(Folkbokforingsadress_FolkbokfordForsamlingKod) + ';' 
        + cErrorMessage + ';' 
        + STRING(lPersonFunnet) + ';' 
        + STRING(lSuccess) + CHR(10)
        .  
    END.        
END.
ELSE DO:
    DO TRANSACTION:
        /*
        IF CAN-FIND(Medlem WHERE 
                    Medlem.MedlemsNr = DEC(cOrgPersonNr + cMKlubbId)) THEN 
          RUN settNyttMedlemsNr(OUTPUT lMedlemsNr).
        ELSE 
          lMedlemsNr = DEC(cOrgPersonNr + cMKlubbId).
        
        CREATE Medlem.
        ASSIGN 
            Medlem.MedlemsNr = lMedlemsNr
            Medlem.PersonNr  = cOrgPersonNr
            Medlem.ForNavn   = 'Ny kund'
            Medlem.EtterNavn = ''
            Medlem.PostNr    = ''
            Medlem.Adresse1  = ''
            Medlem.MedGruppe = IF AVAILABLE MedlemsGruppe THEN MedlemsGruppe.MedGruppe ELSE 0
            Medlem.MedType   = IF AVAILABLE MedlemsType   THEN MedlemsType.MedType ELSE 0
            Medlem.MKlubbId  = INT(cMKlubbId)
            Medlem.ButikkNr  = iButikkNr
            lMedlemsNr       = Medlem.MedlemsNr
            cBongTekst       = cMsgNyMedlem
            cNavn            = ''
            . 
        CREATE MedlemsKort.
        ASSIGN
            MedlemsKort.MedlemsNr    = Medlem.MedlemsNr
            MedlemsKort.KortNr       = STRING(lMedlemsNr)
            MedlemsKort.AktivertDato = TODAY 
            MedlemsKort.UtgarDato    = TODAY + 999
            MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
            MedlemsKort.KortType     = 1
        .
        */
    END.

    RUN bibl_loggDbFri.p ('asMedlemLogg', 'asmedlem.p: sjekkOmFinnesSPAR: Ikke kontakt med SPAR: ' 
                          + 'PersNr: '  + cOrgPersonNr + ' '
                          + 'ButikkNr: ' + STRING(iButikkNr)  
                          ).

    ASSIGN bOK          = FALSE
           cStatus      = "0"
           cMelding     = "Ingen kontakt med SPAR. " + cOrgPersonNr + "."
           /*
           cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&FORNAVN','..............................') 
           cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&ETTERNAVN','..............................') 
           cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&PERSONNR',cOrgPersonNr) 
           cMsgNyMedlem = REPLACE(cMsgNyMedlem,'&ADRESSE','..............................')
           cBongTekst   = cMsgNyMedlem.
           cMelding     = cErrormessage
           */
           .
END.

/*   IF (AVAILABLE Medlem AND AVAILABLE MedlemsKort) THEN */
/*       ASSIGN                                           */
/*       bOk        = TRUE                                */
/*       cStatus    = '2'                                 */
/*       cMelding   = ''.                                 */
/*   ELSE                                                 */
/*       /* test */                                       */
/*       ASSIGN                                           */
/*           bOk        = FALSE                           */
/*           cStatus    = '0'                             */
/*           cMelding = '** Ukjent medlem'.               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkOmFinnesSPARTest) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkOmFinnesSPARTest Procedure 
PROCEDURE sjekkOmFinnesSPARTest :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
DEFINE VARIABLE cErrorMessage AS CHAR NO-UNDO.
DEFINE VARIABLE lPersonFunnet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.

DO:
    DO TRANSACTION:
        CREATE Medlem.
        ASSIGN 
            Medlem.MedlemsNr = DEC(cPersonNr  + cMKlubbId)
            Medlem.PersonNr  = cPersonNr
            Medlem.ForNavn   = 'Fornavn: ' + cPersonNr
            Medlem.EtterNavn = 'Etteravn: ' + cPersonNr
            Medlem.PostNr    = '1170'
            Medlem.Adresse1  = 'Gurrestien'
            lMedlemsNr       = Medlem.MedlemsNr
            cBongTekst       = cMsgNyMedlem
            cNavn            = Medlem.ForNAvn + ' ' + Medlem.Etternavn
            . 
        CREATE MedlemsKort.
        ASSIGN
            MedlemsKort.MedlemsNr    = Medlem.MedlemsNr
            MedlemsKort.KortNr       = cPersonNr  + cMKlubbId
            MedlemsKort.AktivertDato = TODAY 
            MedlemsKort.UtgarDato    = TODAY + 999
            MedlemsKort.Innehaver    = Medlem.Fornavn + Medlem.EtterNavn
            MedlemsKort.KortType     = 1
        .
        RUN settMedlemsData.
        ASSIGN bOK      = TRUE
               cStatus  = "1"
               cMelding = ""
               . 
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-sjekkOmPersNrFinnesLokalt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sjekkOmPersNrFinnesLokalt Procedure 
PROCEDURE sjekkOmPersNrFinnesLokalt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: Her sjekkes kun personnummer.   
------------------------------------------------------------------------------*/

  /* Slår opp etter medlem uten klubbid som suffix i personnr. */
  
  FIND FIRST Medlemskort NO-LOCK WHERE 
      Medlemskort.KortNr = cOrgPersonNr AND  
      CAN-FIND(FIRST Medlem OF Medlemskort NO-LOCK WHERE 
               Medlem.MKlubbId = INT(cMKLubbId)) NO-ERROR.
  /* Slår opp etter medlem med klubbid som suffix i personnr. */
  IF NOT AVAILABLE MedlemsKort THEN 
    FIND FIRST Medlemskort NO-LOCK WHERE 
        MedlemsKort.KortNr = cOrgPersonNr + cMKlubbId AND 
      CAN-FIND(FIRST Medlem OF Medlemskort NO-LOCK WHERE 
               Medlem.MKlubbId = INT(cMKLubbId)) NO-ERROR.
  IF AVAILABLE Medlemskort THEN
  HENT_VIA_MEDLEMSKORT:
  DO:
      FIND Medlem OF Medlemskort NO-LOCK NO-ERROR.
      IF AVAILABLE Medlem AND AVAILABLE Medlemskort THEN 
      DO:
          RUN settMedlemsData.
          RUN bibl_loggDbFri.p ('asMedlemLogg', 'asmedlem.p: sjekkOmFinnesLokalt: Funnet medlem via medlemskort: ' 
                                + STRING(Medlem.MedlemsNr) + ' ' 
                                + Medlem.Fornavn + ' ' 
                                + Medlem.Etternavn + ' ' 
                                + 'KortNr: '  + MedlemsKort.KortNr + ' '
                                + 'PersNr: '  + cOrgPersonNr + ' '
                                + 'KlubbId: ' + cMKLubbId + ' '  
                                + 'ButikkNr: ' + STRING(iButikkNr)  
                                ).              
      END.
  END. /* HENT_VIA_MEDLEMSKORT */

  IF NOT AVAILABLE MedlemsKort THEN  
  HENT_VIA_PERSONNR:
  DO:
      FIND FIRST Medlem NO-LOCK WHERE
                 Medlem.PersonNr = cOrgPersonNr AND 
                 Medlem.MKlubbId = INT(cMKLubbId) NO-ERROR.
      IF AVAILABLE Medlem THEN
      DO:
          FIND FIRST medlemskort OF Medlem NO-LOCK NO-ERROR.
          IF AVAILABLE Medlem AND AVAILABLE Medlemskort THEN
          DO: 
              RUN settMedlemsData.
              RUN bibl_loggDbFri.p ('asMedlemLogg', 'asmedlem.p: sjekkOmFinnesLokalt: Funnet medlem via personnr: ' 
                                    + STRING(Medlem.MedlemsNr) + ' ' 
                                    + Medlem.Fornavn + ' ' 
                                    + Medlem.Etternavn + ' ' 
                                    + 'KortNr: '  + MedlemsKort.KortNr + ' '
                                    + 'PersNr: '  + cOrgPersonNr + ' '
                                    + 'KlubbId: ' + cMKLubbId + ' ' 
                                    + 'ButikkNr: ' + STRING(iButikkNr)  
                                    ).              
          END.
      END.
  END. /* HENT_VIA_PERSONNR */

  IF (AVAILABLE Medlem AND AVAILABLE MedlemsKort) THEN DO:
      ASSIGN
      bOk        = TRUE
      cStatus    = '2'
      cMelding   = ''.
      IF TRIM(medlem.ePostAdresse) = "" OR TRIM(medlem.MobilTlf) = "" THEN DO:
          lDoUpdate = TRUE.
          IF TRIM(medlem.ePostAdresse) = "" THEN
              cMelding = "Epost".
          IF TRIM(medlem.MobilTlf) = "" THEN
              cMelding = cMelding + (IF cMelding <> "" THEN "," ELSE "") + "Mobilnr".
          cMelding = "SAKNAS: " + cMelding.
      END.
  END.
  ELSE 
      /* test */
      ASSIGN
          bOk        = FALSE
          cStatus    = '0'
          .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

