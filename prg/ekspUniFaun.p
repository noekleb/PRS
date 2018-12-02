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
    DEFINE VAR cParaString AS CHARACTER INIT "1090000001|UniFaun|Skriver" NO-UNDO.
&ELSE
    DEFINE INPUT PARAMETER cParaString AS CHARACTER NO-UNDO.
&ENDIF


DEF VAR iAntLinjer   AS INT        NO-UNDO.
DEF VAR iAlle        AS INT        NO-UNDO.
DEF VAR bStream      AS LOG        NO-UNDO.

DEFINE STREAM Ut.

/* Filhåndtering */
DEF VAR cFilNavn       AS CHAR FORMAT "x(40)"    NO-UNDO.
DEF VAR cKatalog       AS CHAR                   NO-UNDO.
DEF VAR cPrefix        AS CHAR                   NO-UNDO.
DEF VAR cEkstent       AS CHAR                   NO-UNDO.
DEF VAR iSekvens       AS INT  FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR cEDBSystem     AS CHAR INITIAL "UniFaun" NO-UNDO.
DEF VAR cFraktArtikkel AS CHAR                   NO-UNDO.
DEF VAR dKOrdre_Id     AS DECIMAL                NO-UNDO.
DEF VAR cSkriver       AS CHARACTER              NO-UNDO.
DEFINE VARIABLE cTjanster    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLevTjanst   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReturTjanst AS CHARACTER   NO-UNDO.

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

  cLevTjanst   = "P25".
  cReturTjanst = "P24".
  /* Ser om vi har en överstyrning av Unifaun-tjänster */

  {syspar2.i 19 9 1 cTjanster}
  IF NUM-ENTRIES(cTjanster) = 2 AND TRIM(ENTRY(1,cTjanster)) <> "" AND TRIM(ENTRY(2,cTjanster)) <> "" THEN DO:
      cLevTjanst   = TRIM(ENTRY(1,cTjanster)).
      cReturTjanst = TRIM(ENTRY(2,cTjanster)).
  END.
  RUN EksporterUniFaun (cLevTjanst). 
  RUN EksporterUniFaun (cReturTjanst). 
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
/*                     */
/* cKatalog = "c:\tmp" */
/*                     */
            cPrefix            = EkstEDBSystem.FilPrefix
            cEkstent           = trim(EkstEDBSystem.FilEkstent,".")
            iSekvens           = IF (EkstEDBSystem.SeqNr + 1) > EkstEDBSystem.MaksSeq
                                    THEN 1
                                    ELSE EkstEDBSystem.SeqNr + 1
            EkstEDBSystem.SeqNr = iSekvens
            cFilNavn           = cKatalog + "\" +
                                 cPrefix  + 
                                 STRING(iSekvens,"99999999") + "." + 
                                 cEkstent
            .
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-EksporterUniFaun) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EksporterUniFaun Procedure 
PROCEDURE EksporterUniFaun :
/*------------------------------------------------------------------------------
  Purpose:     Utlegg av fakturadata til Visma Global. Opprinnelig for Dampbageriet.
               Et enkelt utlegg som er basert på at priser og rabatter hentes og 
               beregnes fra Visma Global.
               Legg merke til at heller ikke størrelse er lagt ut, da denne forutsettes
               alltid være 1.
  Parameters:  <none>
  Notes:       
    
       
<?xml version="1.0" encoding="ISO-8859-1"?>
<data>
        <receiver rcvid="001">
                <val n="name">Unifaun AB</val>
                <val n="address1">Kaserntorget 5</val>
                <val n="zipcode">97469</val>
                <val n="city">BERLIN</val>
                <val n="country">DE</val>
                <val n="email">support@unifaun.se</val>
                <val n="phone">031-725 35 00</val>
                <val n="sms">0700-000000</val>
                <val n="contact">Support</val>
        </receiver>
        <shipment orderno="12345">
                <val n="from">1</val>
                <val n="to">001</val>
                <val n="reference">Testreferens</val>
                <val n="freetext1">Valfri text</val>
                <val n="termcode">003</val>
                <service srvid="APL">
                </service>
                <ufonline>
                <option optid="enot">
                <val n="message">Valfritt meddelande</val>
                <val n="cc">info@unifaun.se</val>                               <!-- Skicka en kopia på föraviseringsmeddelandet -->
                <val n="bcc">hemlig@unifaun.se</val>                    <!-- Skicka en hemlig kopia på föraviseringsmeddelandet -->
                </option>
                </ufonline>
                <container type="parcel">
                        <val n="weight">15</val>
                        <val n="copies">1</val>
                        <val n="contents">Prylar</val>
                </container>
        </shipment>
</data>    
    
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cServiceSrvId AS CHARACTER NO-UNDO.

  DEF VAR lVekt AS DEC FORMAT ">>>9.999" NO-UNDO.

   RUN EDBSystem.        /* (EkstEDBSystem.EDBSystem)  */
   IF cFilnavn = "" THEN
     RETURN.

   FIND KOrdreHode NO-LOCK WHERE
     KOrdreHode.KOrdre_Id = dKOrdre_Id NO-ERROR.
   IF NOT AVAILABLE KOrdreHode THEN
       RETURN.
   FIND Kunde NO-LOCK WHERE 
       Kunde.KundeNr = KOrdreHode.KundeNr NO-ERROR.
   IF NOT AVAILABLE Kunde THEN
       RETURN.
   FIND Butiker NO-LOCK WHERE
       Butiker.Butik = KOrdreHode.ButikkNr NO-ERROR.
   IF NOT AVAILABLE Butiker THEN
       RETURN.
          
   EKSPORT:
   DO:
     OUTPUT STREAM Ut TO VALUE(cFilnavn) NO-ECHO.     
     /* Header */
     PUT STREAM Ut UNFORMATTED 
       '<?xml version="1.0" encoding="ISO-8859-1"?>' SKIP
       '<data>' SKIP.

     /* Sender - Data henter fra butikkregisteret. */
     IF cLevTjanst = "P25" THEN
         PUT STREAM Ut UNFORMATTED 
           '    <sender sndid="1">' SKIP
           '        <val n="name">' + Butiker.ButNamn + '</val>' SKIP
           '        <val n="address1">' + Butiker.BuAdr + '</val>' SKIP
           '        <val n="zipcode">' + Butiker.BuPoNr + '</val>' SKIP
           '        <val n="city">' + Butiker.BuPAdr + '</val>' SKIP
           '        <val n="country">' + Butiker.ButLand + '</val>' SKIP
           '        <val n="email">' + Butiker.ePostAdresse + '</val>' SKIP
           '        <val n="phone">' + Butiker.BuTel + '</val>' SKIP
           '        <val n="contact">' + Butiker.LevKontakt + '</val>' SKIP
           '        <partner parid="PLAB">' SKIP /* PLAB = Posten Logistik */
           '        <val n="custno">' + Butiker.LevMerknad + '</val>' SKIP
           '        </partner>' SKIP
           '    </sender>' SKIP.

     ELSE
         PUT STREAM Ut UNFORMATTED 
           '    <sender sndid="1">' SKIP
           '        <val n="name">' + Butiker.ButNamn + '</val>' SKIP
           '        <val n="address1">' + Butiker.BuAdr + '</val>' SKIP
           '        <val n="zipcode">' + Butiker.BuPoNr + '</val>' SKIP
           '        <val n="city">' + Butiker.BuPAdr + '</val>' SKIP
           '        <val n="country">' + Butiker.ButLand + '</val>' SKIP
           '        <val n="email">' + Butiker.ePostAdresse + '</val>' SKIP
           '        <val n="phone">' + Butiker.BuTel + '</val>' SKIP
           '        <val n="contact">' + Butiker.LevKontakt + '</val>' SKIP
           '        <partner parid="PBREV">' SKIP /* PLAB = Posten Logistik */
           '        <val n="custno">' + Butiker.LevMerknad + '</val>' SKIP
           '        </partner>' SKIP
           '    </sender>' SKIP.
     /* Receiver */
     PUT STREAM Ut UNFORMATTED 
       '    <receiver rcvid="001">' SKIP
       '        <val n="name">' + Kunde.Navn + '</val>' SKIP
       '        <val n="address1">' + (IF KOrdreHode.LevAdresse1 <> '' THEN KORdreHode.LevAdresse1 ELSE KOrdreHode.Adresse1) + '</val>' SKIP
       '        <val n="address2">' + (IF KOrdreHode.LevAdresse1 <> '' THEN KORdreHode.LevAdresse2 ELSE KOrdreHode.Adresse2) + '</val>' SKIP
       '        <val n="zipcode">' + (IF KOrdreHode.LevAdresse1 <> '' THEN KORdreHode.LevPostNr ELSE KOrdreHode.PostNr) + '</val>' SKIP
       '        <val n="city">' + (IF KOrdreHode.FaktPoststed <> '' THEN KOrdreHode.FaktPoststed ELSE KOrdreHode.PostSted) + '</val>' SKIP
/*        '        <val n="country">' + "SE" + '</val>' SKIP */
       '        <val n="contact">' + KOrdreHode.Referanse + '</val>' SKIP
       '        <val n="phone">' + (IF TRIM(KordreHode.MobilTlf) <> '' 
                                           THEN KOrdreHode.MobilTlf
                                           ELSE KOrdreHode.Telefon) + '</val>' SKIP
/*        '        <val n="fax">' + "031-000 00 00" + '</val>' SKIP */
       '        <val n="email">' + KordreHode.ePostAdresse + '</val>' SKIP
       '        <val n="sms">' + (IF TRIM(KordreHode.MobilTlf) <> '' 
                                           THEN KOrdreHode.MobilTlf
                                           ELSE KOrdreHode.Telefon) + '</val>' SKIP
/*        '        <val n="sms">' + "0700-00 00 00" + '</val>' SKIP */
       '    </receiver>' SKIP.

     /* Shipment header */
     PUT STREAM Ut UNFORMATTED 
       '    <shipment orderno="' + STRING(KOrdreHode.KOrdre_Id) + '">' SKIP
       '        <val n="from">1</val>' SKIP
       '        <val n="to">001</val>' SKIP
       '        <val n="reference">' + "Webbutik ordernr: " + KOrdreHode.EkstOrdreNr + '</val>' SKIP
/*        '        <val n="freetext1">' + "Valfri text" + '</val>' SKIP */
       '        <val n="termcode">' + "003" + '</val>' SKIP
       '        <service srvid="' + cServiceSrvId + '">' SKIP  /* STRING(KOrdreHode.LevFNr) ??? */
       '        <addon adnid="notsms">' SKIP 
       '        </addon>' SKIP 
       '        </service>' SKIP
       '        <ufonline>' SKIP
       '            <option optid="enot">' SKIP
       '                <val n="message">' + "Paket skickat" + '</val>' SKIP
/*       '                <val n="cc">' + "info@unifaun.se" + '</val>' SKIP*/
/*       '                <val n="bcc">' + "tomn@polygon.se;Johanna@johanssons.se" + '</val>' SKIP*/
       '            </option>' SKIP
       '        </ufonline>' SKIP
       .

     FIND FIRST SysPara NO-LOCK WHERE
       SysPara.SysHId       = 150 AND  
       SysPara.SysGr        = 10 AND  
       SysPara.Beskrivelse  = "Posten" NO-ERROR.

     lVekt = 0.6.
     FOR EACH KOrdreLinje OF KOrdreHode NO-LOCK:

         /* Frakt skal ikke fraktes :) */
         IF KOrdreLinje.VareNr =  (IF AVAILABLE Syspara THEN Syspara.Parameter1 ELSE '') THEN
             NEXT.
         /* Betaling skal ikke fraktes */
         IF CAN-DO('BETALT,KREDIT',KOrdreLinje.VareNr) THEN
             NEXT.
         /* Bare vekt på eksisterende artikler kan legges ut. */
         FIND ArtBas NO-LOCK WHERE
             ArtBas.ArtikkelNr = DEC(KOrdreLinje.VareNr) NO-ERROR.
         ASSIGN lVekt = lVekt + 1.
     END.
     lVekt = 1.9.
     /* Shipment Row */
     PUT STREAM Ut UNFORMATTED 
       '        <container type="parcel">' SKIP
       '            <val n="weight">' + TRIM(STRING(lVekt,">>>9.999")) + '</val>' SKIP
       '            <val n="copies">' + "1" + '</val>' SKIP
       '            <val n="contents">' + 'Varer' + ' ' +
                                          '</val>' SKIP
       '        </container>' SKIP.


     /* Shipment footer */
     PUT STREAM Ut UNFORMATTED        
       '    </shipment>' SKIP.
       
     /* Footer */
     PUT STREAM Ut UNFORMATTED 
     '</data>' SKIP.

     OUTPUT STREAM Ut CLOSE.
   END. /* EKSPORT */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

