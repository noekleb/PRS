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
DEFINE VARIABLE cOversett AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lOversett AS LOGICAL     NO-UNDO.
DEFINE TEMP-TABLE tt_lbl NO-UNDO
    FIELD lbl AS CHAR
    FIELD se  AS CHAR
    INDEX lbl IS PRIMARY UNIQUE lbl.

cOversett = "Adresse1;Adress1," +
            "Adresse2;Adress2," +
            "Amex;Amex," +
            "AnonseArtikkel;AnnonsArtikel," +
            "Ant;Ant," +
            "Antall;Antal," +
            "Antdiff%;Antdiff%," +
            "AntPar;AntPar," +
            "AntSUM;AntSUM," +
            "ArtBeskr;Artnamn," +
            "Artikkel;Artikel," +
            "Artikkelnr;Artikelnr," +
            "Avdbeskr;Avdnamn," +
            "Avdel;Avd," +
            "Avdeling;Avdelning," +
            "AvdelingNr;Avdelning," +
            "Bankkort;Bankkort," +
            "BankPose;Bankkuvert," +
            "BatchNr;BatchNr," +
            "Beløp;Belopp," +
            "Beskr;Beskr," +
            "Beskrivelse;Beskrivning," +
            "Best.dato;Best.datum," +
            "BestNr;BestNr," +
            "BestStat;BestStat," +
            "BestType;BestTyp," +
            "Betalingskort;Betalningskort," +
            "BokfNr;BokfNr," +
            "BongId;KvittoId," +
            "BongNr;KvittoNr," +
            "BongStatus;KvittoStatus," +
            "Bongtekst;Kvittotext," +
            "Brekkasje;Kassation," +
            "Brekkasje kr;Kassation kr," +
            "BrukerID;LoginID," +
            "Butikk;Butik," +
            "Butikknr;Butiknr," +
            "Dag;Dag," +
            "DataSettId;DataSettId," +
            "Dato;Datum," +
            "Dato-YMD;Datum-YMD," +
            "DB kr;TB kr," +
            "Db%;TB%," +
            "DB% tilb;TB% kamp," +
            "Db%1;TB%1," +
            "Db%2;TB%2," +
            "DbAndel%;TBAndel%," +
            "DbKr;TBKr," +
            "DbKr Diff%;TBKr Diff%," +
            "DBKr tilb;TBKr tilb," +
            "DbKr1;TBKr1," +
            "DbKr2;TBKr2," +
            "DiffKasse;DiffKassa," +
            "Diners;Diners," +
            "DirekteLev;DirektLev," +
            "DiverseKort;DiverseKort," +
            "EDato;EDato," +
            "EkstId;ExtId," +
            "Email;Email," +
            "Etternavn;Efternavn," +
            "Eurocard;Eurocard," +
            "FarBeskr;FärgBeskr," +
            "Farve;Färg," +
            "Fornavn;Förnavn," +
            "FOTTØY;FOTTØY," +
            "Fra butikk;Fra butikk," +
            "Gavekort;Gåvokort," +
            "GavekortBruktAndre;GåvokortAnvAndre," +
            "GavekortBruktEgne;GåvokortAnvEgne," +
            "GavekortUt;GåvekortUt," +
            "Gjenkjøp;Återköp," +
            "Gjenkjøp kr;Återköp kr," +
            "Hg;Hg," +
            "HgBeskr;HgNamn," +
            "Hovedgr;Huvudgr," +
            "InnbetaltKunde;InbetaltKund," +
            "Innkjopt;Inköpt," +
            "Innkjopt kr;Inköpt kr," +
            "Innkjverdi;Inkvärde," +
            "InnLev;InLev," +
            "Internt forbruk;Internt förbruk," +
            "Internt forbruk kr;Internt förbruk kr," +
            "jmfenhet;Jmfenhet," +
            "Justert;Justerat," +
            "Justert kr;Justerat kr," +
            "Kampanje;Kampanj," +
            "Kamptilbud;Kamptilbud," +
            "Kasse;Kassa," +
            "Kasserer;Kassör," +
            "KassererNavn;KassörNamn," +
            "KassererNr;KassörNr," +
            "KategoriBeskr;KategoriNamn," +
            "Kilde;Källa," +
            "Kjopandel%;Köpandel%," +
            "Kjøpt;Köpt," +
            "Kjøpt kr;Köpt kr," +
            "Kontant;Kontant," +
            "KontKjopKasse;KontKöpKassa," +
            "Konvertert;Konverterat," +
            "Kortnr;Kortnr," +
            "KortType;KortTyp," +
            "Kreditkort;Kreditkort," +
            "Kreditsalg;Kreditfsg," +
            "Kunde;Kund," +
            "KundeKort;KundKort," +
            "KundeNavn;KundNamn," +
            "KundeNr;KundNr," +
            "Kunderekl;Kundrekl," +
            "Kunderekl kr;Kundrekl kr," +
            "Kupong1;Kupong1," +
            "Lagervare;Lagervara," +
            "Lagerverdi;Lagervärde," +
            "LapTop;LapTop," +
            "LevDato;LevDatum," +
            "Leverandør;Leverantör," +
            "Levfarge;Levfärg," +
            "LevFargKod;LevFärgKod," +
            "LevKod;LevKod," +
            "Levnavn;Levnamn," +
            "LevNr;LevNr," +
            "Levrekl;Levrekl," +
            "Levrekl kr;Levrekl kr," +
            "LinjeNr;LinjeNr," +
            "Linjerab;Linjerab," +
            "Lopnr;Löpnr," +
            "LpNr;LpNr," +
            "Makulert;Makulerat," +
            "MatBeskr;MatNamn," +
            "MatKod;MatKod," +
            "MedGruppe;MedGrupp," +
            "Medlem;Medlem," +
            "MedlemNavn;MedlemNamn," +
            "MedlemsKort;MedlemsKort," +
            "Medlemsnavn;Medlemsnamn," +
            "MedlemsNr;MedlemsNr," +
            "MedType;MedTyp," +
            "Merknad;Anm," +
            "Mva;Moms," +
            "Mva verdi;Moms kr," +
            "Mva%;Moms%," +
            "Navn;Namn," +
            "Neg.lager;Neg.lager," +
            "Neg.lagerverdi;Neg.lagervärde," +
            "NegLager;NegLager," +
            "OmsetningEksKred;OmsättningExKred," +
            "OpdKvit;UppdKvit," +
            "OpdUtskKopi;UpddUtskKopia," +
            "OrdreNr;OrderNr," +
            "Ov.butikk;Öv.butikk," +
            "OverforingsNr;ÖverföringsNr," +
            "Overført;Överfört," +
            "Overført kr;Överfört kr," +
            "Overførte kr;Överförda kr," +
            "Overlev;Pluslev," +
            "Periode;Period," +
            "Personalrab;Personalrab," +
            "Plukket;Plockat," +
            "Pos.lager;Pos.lager," +
            "Postadr;Postadr," +
            "Postert;Posterad," +
            "Postert tid;Posterad tid," +
            "PostertDato;PosteradDatum," +
            "Postnr;Postnr," +
            "Pris;Pris," +
            "Pris inkl. rab;Pris inkl. rab," +
            "Pris tilb;Pris kamp," +
            "Prodbeskr;Prodnamn," +
            "Prodnr;Prodnr," +
            "På tilbud;På kampanj," +
            "Rab%;Rab%," +
            "Rab%1;Rab%1," +
            "Rab%2;Rab%2," +
            "Rabandel%;Rabandel%," +
            "Rabatt;Rabatt," +
            "Rabatt kr;Rabatt kr," +
            "Rabatter;Rabatter," +
            "Rabattverdi;Rabattvärde," +
            "RegistrertAv;RegistreratAv," +
            "RegistrertDato;RegistreratDatum," +
            "Rekvisisjon;Rekvisition," +
            "Rest;Rest," +
            "Retur lev;Retur lev," +
            "Returer;Returer," +
            "Returer kr;Returer kr," +
            "Returer kunde;Returer kund," +
            "Salgssum brutto;Fsgvärde brutto," +
            "Salgssum netto;Fsgvärde netto," +
            "Salgsverdi;Fsgvärde," +
            "Selger;Säljare," +
            "SelgerNavn;SäljarNamn," +
            "SelgerNr;SäljarNr," +
            "SenterGavekort;CenterPresentkort," +
            "SesBeskr;SäsNamn," +
            "Sesong;Säsong," +
            "Sjekk;Check," +
            "Solgt;Sålt," +
            "Solgt brutto;Sålt brutto," +
            "Solgt brutto1;Sålt brutto1," +
            "Solgt brutto2;Sålt brutto2," +
            "Solgt diff%;Sålt diff%," +
            "Solgt netto;Sålt netto," +
            "Solgt netto1;Sålt netto1," +
            "Solgt netto2;Sålt netto2," +
            "Solgt%;Sålt%," +
            "Solgt1;Såt1," +
            "Solgt2;Sålt2," +
            "Strekkode;Streckkod," +
            "StrTypeID;StrTypID," +
            "Størrelse;Storlek," +
            "Subtotalrab;Subtotalrab," +
            "SUM;SUM," +
            "Sum DB kr;Sum TB kr," +
            "Sum netto;Sum netto," +
            "Sum Vektet Vk;Sum Viktad Vk," +
            "SumInnbutikk;SumInbutik," +
            "Svinn;Svinn," +
            "Svinn kr;Svinn kr," +
            "Systemkort;Systemkort," +
            "TeamNr;TeamNr," +
            "Telefon;Telefon," +
            "Til butikk;Till butikk," +
            "Til strl;Till strl," +
            "Tilbud;Kampanj," +
            "Tilbud kr;Kampanj kr," +
            "TilgKilde;TilgKälla," +
            "TilgodeBruktAndre;TillgodoAnvAndra," +
            "TilgodeBruktEgne;TillgodoAnvEgna," +
            "TilgodeUt;TillgodoUt," +
            "Time;Timme," +
            "Tot;Tot," +
            "Transdato-YMD;Transdatum-YMD," +
            "TransNr;TransNr," +
            "Transtid;Transtid," +
            "TTId;TTId," +
            "Ukjent;Okänd," +
            "Utsolgt%;Utsålt%," +
            "Varegr;Varugr," +
            "Varekost;Varukost," +
            "Varekost tilb;Varukost kamp," +
            "VareMerke;VaruMärke," +
            "Veil. pris;Riktpris," +
            "Vektet varekost;Viktad varukost," +
            "Verdier;Värden," +
            "Verdirabatt1;Rabatt1 kr," +
            "Verdirabatt2;Rabatt2 kr," +
            "Vg;Vg," +
            "Vg/Løpenr;Vg/Löpnr," +
            "VgBeskr;VgNamn," +
            "VgLopNr;VgLöpNr," +
            "Vgnavn;Vgnamn," +
            "VgNr;VgNr," +
            "Visa;Visa," +
            "VMBeskr;VMNamn," +
            "VVarekost;Vvarukost".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-Oversett2SE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Oversett2SE Procedure 
FUNCTION Oversett2SE RETURNS CHARACTER
  ( INPUT cLabels AS CHARACTER )  FORWARD.

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
FIND bruker NO-LOCK WHERE bruker.brukerid = USERID("skotex") NO-ERROR.
IF AVAIL bruker AND CAN-DO("SE,SVE",bruker.lng) THEN
    lOversett = TRUE.
RUN ByggTT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggTT Procedure 
PROCEDURE ByggTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
/*     DEFINE TEMP-TABLE tt_lbl NO-UNDO     */
/*         FIELD lbl AS CHAR                */
/*         FIELD se  AS CHAR                */
/*         INDEX lbl IS PRIMARY UNIQUE lbl. */

    DO ii = 1 TO NUM-ENTRIES(cOversett):
        CREATE tt_lbl.
        ASSIGN tt_lbl.lbl = ENTRY(1,ENTRY(ii,cOversett),";")
               tt_lbl.se  = ENTRY(2,ENTRY(ii,cOversett),";").
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Oversett2SE) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Oversett2SE Procedure 
FUNCTION Oversett2SE RETURNS CHARACTER
  ( INPUT cLabels AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VAR cSE    AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

    IF lOversett = TRUE THEN DO:
        cSe = FILL(",",NUM-ENTRIES(cLabels) - 1).
        DO ii = 1 TO NUM-ENTRIES(cLabels):
            IF TRIM(ENTRY(ii,cLabels)) <> "" THEN DO:
                FIND tt_lbl WHERE tt_lbl.lbl = TRIM(ENTRY(ii,cLabels)) NO-ERROR.
                ENTRY(ii,cSE) = IF AVAIL tt_lbl THEN tt_lbl.se ELSE TRIM(ENTRY(ii,cLabels)).
            END.
        END.
    END.
  RETURN IF lOversett THEN cSe ELSE cLabels.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

