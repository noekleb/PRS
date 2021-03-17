/* lesexcelvpifil.i */
DEFINE {&NEW} {&SHARED} TEMP-TABLE TT_Moms NO-UNDO LIKE Moms.

DEFINE {&NEW} {&SHARED} VARIABLE ExcludeImportFields AS CHAR EXTENT 2 NO-UNDO.

DEFINE {&NEW} {&SHARED} TEMP-TABLE TT_Error NO-UNDO
    FIELD RadNr  AS INTE
    FIELD cError AS CHAR FORMAT "x(40)" LABEL "Felinfo"
    INDEX radnr IS PRIMARY UNIQUE radnr.

DEFINE {&NEW} {&SHARED} TEMP-TABLE TT_VareOrgInfo NO-UNDO
    FIELD RadNr  AS INTE
    FIELD cTTfelt AS CHAR
    FIELD cFelt  AS CHAR FORMAT "x(40)" LABEL "Fält"
    FIELD cVerdi AS CHAR FORMAT "x(40)" LABEL "Värde"
    INDEX radnr IS PRIMARY radnr.

DEFINE {&NEW} {&SHARED} TEMP-TABLE TT_Prisfil NO-UNDO
    FIELD Radnr           AS INTE FORMAT ">>>>9"         LABEL "Radnr"
    FIELD lNy             AS LOG  FORMAT "J/"            LABEL "Ny"
    FIELD cTandem         AS CHAR FORMAT "x(15)"         LABEL "Tandem"
    FIELD cStatus         AS CHAR FORMAT "x(4)"          LABEL "Anm."
    FIELD artbas_levnr    AS INTE FORMAT ">>>>>>>>>>>9"  LABEL "Levnr"
    FIELD artbas_artnr    AS CHAR FORMAT "x(14)"         LABEL "Varunr"
    FIELD strekkode_kode  AS CHAR FORMAT "x(15)"         LABEL "EAN"
    FIELD pris_bestnr     AS CHAR FORMAT "x(14)   "      LABEL "Bestnr"
    FIELD vare_varetekst  AS CHAR FORMAT "x(40)"         LABEL "Varutext"
    FIELD vare_mengde     AS DECI FORMAT ">>,>>9.999"    LABEL "Jf.mängd"
    FIELD vare_enhet      AS INTE FORMAT ">9"            LABEL "Enhet"
    FIELD vare_Cenhet     AS CHAR FORMAT "x(4)"          LABEL  "Jf.enh.txt" /* = 1 THEN "st" ELSE IF vare.enhet = 2 THEN "kg" ELSE IF vare.enhet = 3 THEN "l" ELSE "m") */
    FIELD vare_hgr        AS INTE FORMAT ">>>9"          LABEL "Varugr"
    FIELD pris_engrosn    AS DECI FORMAT ">>,>>9.99"     LABEL "Inpris"
    FIELD rabatt          AS DECI FORMAT "9"             LABEL "Rab"
    FIELD pris_utprisn    AS DECI FORMAT ">>,>>9.99"     LABEL "Utpris"
    FIELD dbpris_utprisn  AS DECI FORMAT ">>,>>9.99" 
    FIELD pris_veilpris   AS DECI FORMAT ">>,>>9.99"     LABEL "Riktpris"
    FIELD vare_mva        AS DECI FORMAT ">9.99"         LABEL "Moms%"
    FIELD vare_mvagr      AS INTE FORMAT "9"             LABEL "Momsgr" /* konv vid inläsning */
    FIELD vare_antpkn     AS DECI FORMAT ">,>>9.999"     LABEL "Förp"
    FIELD vare_link       AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Link"
    FIELD Vare_Produsent  AS CHAR FORMAT "x(40)"         LABEL "Producent"
    FIELD ProdNr          AS INT  FORMAT ">>>>>>9"       LABEL "ProdNr"
    FIELD Vare_Varemerke  AS CHAR FORMAT "x(40)"         LABEL "Varumärke"
    FIELD VmId            AS INT  FORMAT ">>>>>9"        LABEL "VmId"
    FIELD Butikk_ButNr    AS INT  FORMAT ">>>>>9"        LABEL "KundNr"
    FIELD ButNamn         AS CHAR FORMAT "x(30)"         LABEL "ButNamn"
    FIELD Butik           AS INT  FORMAT ">>>>>9"        LABEL "Butikk"
    FIELD fil_tbproc      AS DECI FORMAT "->>9.99"       LABEL "TB%"
    FIELD hgrprof_brutto% AS DECI FORMAT "->>9.99"       LABEL "VG TB%"
    FIELD orgdata         AS CHAR 
    FIELD cDummy          AS CHAR FORMAT "x(2)"          LABEL ""
    FIELD Salgsenhet      AS CHARACTER FORMAT "x(10)"    LABEL "Salgsenhet"
    FIELD ArtikkelNr      AS DECIMAL FORMAT ">>>>>>>>>>>>>9" LABEL 'ArtikkelNr'
    INDEX Radnr IS PRIMARY UNIQUE Radnr
    INDEX Butik Butik vare_varetekst pris_bestnr.

DEFINE {&NEW} {&SHARED} TEMP-TABLE TT_RigalVare NO-UNDO
/* Feltnr     Betegnelse                       Feltinnhold                     Format  Kommentar                                                      */
/* 1   "VAR"     */ FIELD Kode         AS CHAR /* "VAR" */
/* 2   "E"       */ FIELD AID_kode     AS CHAR /* "E" eller "P" */
/* 3   130200004 */ FIELD Nummer       AS DECI /* EAN/PLU-nummer  I(13) */
/* 4   13020000  */ FIELD Artnr        AS DECI /* Internt artikkelnr */
/* 5   "N"       */ FIELD Flag         AS CHAR /* Funksjonskode "N" = "Normal",  vare/prisendring"K" = Kampanje"M" =  Medlemstilbud "U" = Utmelding (tolkes som sletting) "A" =  Slett ikke påbegynt kampanje, avslutt påbegynt kamåpanje     */
/* 6   1         */ FIELD Grossist     AS INTE /* Leverandørnummer I(8) */
/* 7   0         */ FIELD Bestnr       AS INTE /* Lev artnumr I(8)  Bestillingsnummer */
/* 8   2002061825*/ FIELD Fradato      AS DECI /* Dato for prisendring,evt startdato tilbudDato  */
/* 9             */ FIELD Tildato      AS DECI /* Sluttdato tilbudDato */
/* 10  "ORIGINAL */ FIELD Varetek      AS CHAR /* Varetekst C(30) */
/* 11  "ORIGINAL */ FIELD Bongtek      AS CHAR /* Bongtekst C(20) */
/* 12  "ORIGINAL */ FIELD Etitekst1    AS CHAR /* Etikettekst 1 C(30)   Må i fylles ut med kunderettede tekster                      */
/* 13  ""        */ FIELD Etitekst2    AS CHAR /* Etikettekst 2 C(40)   Må i fylles ut med utfyllende kunderettede tekster (se over) */
/* 14  1         */ FIELD Pakn         AS INTE /* Salgsenhet  I 1=stk, 2=kg, 3=liter, 4=meter osv.*/
/* 15  1         */ FIELD Antpkn       AS INTE /* Antall i pakning */
/* 16  0         */ FIELD Storrelse    AS INTE /* Pakningsstørrelse*/
/* 17  6         */ FIELD Avdeling     AS INTE /* Avdeling      */
/* 18  6330      */ FIELD Hgr          AS INTE /* HovedgruppeI  */
/* 19  1         */ FIELD Ugr          AS INTE /* Undergruppe   */
/* 20  47        */ FIELD Engros       AS DECI /* EngrosprisD   */
/* 21  47        */ FIELD Netto        AS DECI /* Nettopris     */
/* 22  102       */ FIELD Utpris       AS DECI /* UtsalgsprisD  */
/* 23  0         */ FIELD Veil         AS DECI /* Veiledende pris */
/* 24  0         */ FIELD Avt_sperre   AS INTE /* Avtalesperre */
/* 25  "0E"      */ FIELD Rabtyp1      AS CHAR /* Rabattype    */
/* 26  "0E"      */ FIELD Rabtyp2      AS CHAR
/* 27  "0E"      */ FIELD Rabtyp3      AS CHAR
/* 28  "0N"      */ FIELD Rabtyp4      AS CHAR
/* 29  "0N"      */ FIELD Rabtyp5      AS CHAR
/* 30  "0N"      */ FIELD Rabtyp6      AS CHAR
/* 31  "0E"      */ FIELD Rabtyp7      AS CHAR
/* 32  "0E"      */ FIELD Rabtyp8      AS CHAR
/* 33  "0E"      */ FIELD Rabtyp9      AS CHAR
/* 34  "0E"      */ FIELD Rabtyp10     AS CHAR
/* 35  0         */ FIELD Rab1         AS INTE
/* 36  0         */ FIELD Rab2         AS INTE
/* 37  0         */ FIELD Rab3         AS INTE
/* 38  0         */ FIELD Rab4         AS INTE
/* 39  0         */ FIELD Rab5         AS INTE
/* 40  0         */ FIELD Rab6         AS INTE
/* 41  0         */ FIELD Rab7         AS INTE
/* 42  0         */ FIELD Rab8         AS INTE
/* 43  0         */ FIELD Rab9         AS INTE
/* 44  0         */ FIELD Rab10        AS INTE
/* 45  24        */ FIELD Mva%         AS DECI
/* 46  1         */ FIELD Bonusg       AS INTE
/* 47  "G"       */ FIELD Sorti        AS CHAR
/* 48  "N"       */ FIELD Vtype        AS CHAR
/* 49  0         */ FIELD Mm-nr        AS INTE
/* 50  1         */ FIELD Etikett      AS INTE
/* 51  0         */ FIELD Lag          AS INTE
/* 52  0         */ FIELD Pall         AS INTE
/* 53  628       */ FIELD Leverandor   AS INTE
/* 54  228       */ FIELD Filial       AS INTE
/* 55  0         */ FIELD Pant         AS DECI
/* 56  0         */ FIELD DUN_Velosity AS INTE
/* 57  ""        */ FIELD AID_kode2    AS CHAR
/* 58  0         */ FIELD Linknr       AS DECI
/* 59  ""        */ FIELD Statflag     AS CHAR
/* 60  1         */ FIELD Produsent    AS INTE
/* 61  ""        */ FIELD AID_kode3    AS CHAR
/* 62  0         */ FIELD Salgskode3   AS DECI
/* 63  ""        */ FIELD AID_kode4    AS CHAR
/* 64  0         */ FIELD Salgskode4   AS DECI
/* 65  ""        */ FIELD Bnrbesk      AS CHAR
/* 66  0         */ FIELD Kampid       AS INTE
/* 67  ""        */ FIELD Kampnavn     AS CHAR
/* 68  0         */ FIELD ENVAnr       AS INTE
/* 69  ""        */ FIELD Selektering1 AS CHAR
/* 70  ""        */ FIELD Selektering2 AS CHAR
/* 71  ""        */ FIELD Selektering3 AS CHAR
/* 72  N         */ FIELD Vardek       AS CHAR
/* 73  J         */ FIELD Laggros      AS CHAR
/* 74  N         */ FIELD Miljokode    AS CHAR
/* 75  N         */ FIELD Genmod       AS CHAR
/* 76  N         */ FIELD Okologisk    AS CHAR
/* 77  0         */ FIELD Erstvar      AS DECI
/* 78  0         */ FIELD Fabrikat     AS INTE
/* 79  0         */ FIELD Konvfak      AS DECI
/* 80  0         */ FIELD Konvpri      AS DECI
/* 81  ""        */ FIELD Lok_i_butikk AS CHAR
/* 82  ""        */ FIELD Planogram    AS CHAR
/* 83  0         */ FIELD Bredde       AS DECI
/* 84  0         */ FIELD Dybde        AS DECI
/* 85  0         */ FIELD Hoyde        AS DECI
/* 86  0         */ FIELD Antbredde    AS INTE
/* 87  0         */ FIELD Antdybde     AS INTE
/* 88  0         */ FIELD Anthoyde     AS INTE
/* 89  N         */ FIELD Anbrekk      AS CHAR
/* 90            */ FIELD Bestilles_fra_datoDato AS DATE
/* 91            */ FIELD Bestilles_til_datoDato AS DATE
/* 92  0         */ FIELD Konvenh      AS  INTE  /* Konverteringsenhet Angir enhet som brukes ifm. enhetspris. Samme koder som i  felt 7.1.14                                                  */
/* 93  0         */ FIELD Garantiklasse AS INTE /* Garantiklassenummer Jfr. "Salg av vare til kunde", pkt. 4                        */
/* 94  0         */ FIELD Idkrav        AS INTE /* I (1)   Angir at det er aldersgrense på salg av varen, jfr. "Varer i butikk", pkt. 5.0=ingen, 1=alkohol, 2=tobakk)                */
/* 95  ""        */ FIELD Bilde        AS CHAR /* Navn på billedfil C(20)   Jfr. "VPI", pkt. 8 */
/* 96  ""        */ FIELD Mersalg      AS CHAR /* EAN/PLUnummer Tilleggsvarer, forslag til mersalg for kasserer adskilt  med; mellom hvert varenr                                     */
/* 97  J         */ FIELD Lokalpris    AS CHAR /* LOGICAL J/N Tillatt å endre pris lokalt i InfoPOS i butikk (frukt&grønt) */
/* 98  0         */ FIELD Holdbarhet   AS INTE           /*  I (2)   Antall dager holdbarhet                                      */
/* 99  0         */ FIELD Taranetto    AS DECI           /* Tara eller netto D       Antall gram tara for vektvarer eller nettovekt for stkvarer  */
/* 100 0         */ FIELD Trykkbilde   AS INTE           /* I (1)   Valg av trykkbilde på ferskvarevekt                          */
/* 101 ""        */ FIELD Etikettenhet AS CHAR           /* C (10)  Tekst for salgsenehet som skal skrives på etikett/plakat     */
/* 102 13020     */ FIELD Modellnr     AS INTE           /* I (8)   Modellnr*/
/* 103 0         */ FIELD Variantnr    AS INTE           /* I (3)   Sortering innen en modell                                    */
/* 104 ""        */ FIELD Farge        AS CHAR           /* Fargetekst                      C(10)   Fargetekst                                                   */
/* 105 ""        */ FIELD Storrelsetekst AS CHAR           /* Størrelsestekst                 C(10)   Størrelsestekst                                              */
/* 106 N         */ FIELD Kunlot       AS CHAR /* LOGICAL Bestilles kun i lot             J/N     Bestilling kun i LOT                                         */
/* 107 ""        */ FIELD FabrikatNavn AS CHAR           /* Fabrikatnavn                    C(30)   Navn på fabrikat - erstatter fabrikatnr (7.1.78)             */
/* 108 ""        */ FIELD Levvnr       AS CHAR           /* Underleverandørs varenr         C(20)   Underleverandørs varenr (dennes bestillingsnr)               */
/* 109 ""        */ FIELD Individtype  AS CHAR           /* Type individhåndtering          C(1)    E=elektro, V=våpen osv.                                      */
/* 110 ""        */ FIELD Artnr2       AS CHAR           /* Alfanumerisk internt artikkelnr C(20)                                                                */
/* 111 0         */ FIELD Matrialkode  AS INTE           /* I2 */
/* 112 0         */ FIELD Sesongkode   AS INTE             /* I3 */
INDEX nummer IS PRIMARY nummer
.
