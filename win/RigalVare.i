DEFINE TEMP-TABLE TT_RigalVare NO-UNDO
/* Feltnr     Betegnelse                       Feltinnhold                     Format  Kommentar                                                      */
/* 7.1.1   "VAR"           */ FIELD Kode         AS CHARACTER /* "VAR"                                                                                                */
/* 7.1.2   "E"             */ FIELD AID_kode     AS CHARACTER /* "E" eller "P"                           Angir om neste felt er EAN eller PLU                         */
/* 7.1.3   130200004       */ FIELD Nummer       AS DECIMAL   /* EAN/PLU-nummer  I(13)                                                                */
/* 7.1.4   13020000        */ FIELD Artnr        AS DECIMAL   /* DATATYPE ????? Internt artikkelnr                                                                                   */
/* 7.1.5   "N"             */ FIELD Flag         AS CHARACTER /* Funksjonskode                           "N" = "Normal", dvs.  vare/prisendring"K" = Kampanje"M" =    Medlemstilbud "U" = Utmelding (tolkes som sletting)"A" =  Slett ikke påbegynt kampanje, avslutt påbegynt kamåpanje     */
/* 7.1.6   1               */ FIELD Grossist     AS INTEGER   /* Leverandørnummer                I(8)                                                                 */
/* 7.1.7   0               */ FIELD Bestnr       AS INTEGER   /* Leverandørens artikkelnummer    I(8)    Bestillingsnummer                                            */
/* 7.1.8   2002061825      */ FIELD Fradato      AS DECI      /* Dato for prisendring,evt startdato tilbudDato                                                        */
/* 7.1.9                   */ FIELD Tildato      AS DECI      /* Sluttdato tilbudDato                                                                                 */
/* 7.1.10  "ORIGINAL F 5CM"*/ FIELD Varetek      AS CHARACTER /* Varetekst                       C(30)                                                                */
/* 7.1.11  "ORIGINAL F 5CM"*/ FIELD Bongtek      AS CHARACTER /* Bongtekst                       C(20)                                                                */
/* 7.1.12  "ORIGINAL F 5CM"*/ FIELD Etitekst1    AS CHARACTER /* Etikettekst 1                   C(30)   Må i fylles ut med kunderettede tekster                      */
/* 7.1.13  ""              */ FIELD Etitekst2    AS CHARACTER /* Etikettekst 2                   C(40)   Må i fylles ut med utfyllende kunderettede tekster (se over) */
/* 7.1.14  1               */ FIELD Pakn         AS INTEGER   /* Salgsenhet                      I       1=stk, 2=kg, 3=liter, 4=meter osv.                           */
/* 7.1.15  1               */ FIELD Antpkn       AS INTEGER   /* Antall i pakning                I                                                                    */
/* 7.1.16  0               */ FIELD Storrelse    AS INTEGER   /* DATATYPE ?????  Pakningsstørrelse                                                                                    */
/* 7.1.17  6               */ FIELD Avdeling     AS INTEGER   /* Avdeling                                                                                             */
/* 7.1.18  6330            */ FIELD Hgr          AS INTEGER   /* HovedgruppeI                                                                                         */
/* 7.1.19  1               */ FIELD Ugr          AS INTEGER   /* Undergruppe                                                                                          */
/* 7.1.20  47              */ FIELD Engros       AS DECIMAL   /* EngrosprisD                                                                                          */
/* 7.1.21  47              */ FIELD Netto        AS DECIMAL   /* Nettopris                                                                                            */
/* 7.1.22  102             */ FIELD Utpris       AS DECIMAL   /* UtsalgsprisD                                                                                         */
/* 7.1.23  0               */ FIELD Veil         AS DECIMAL  /* Veiledende pris                                                                                      */
/* 7.1.24  0               */ FIELD Avt_sperre   AS INTEGER  /* ????? Avtalesperre                                                                                         */
/* 7.1.25  "0E"            */ FIELD Rabtyp1      AS CHARACTER  /* ????? Rabattype                                                                                            */
/* 7.1.26  "0E"            */ FIELD Rabtyp2      AS CHARACTER /* ?????                                                                                                      */
/* 7.1.27  "0E"            */ FIELD Rabtyp3      AS CHARACTER /* ?????                                                                                                      */
/* 7.1.28  "0N"            */ FIELD Rabtyp4      AS CHARACTER /* ?????                                                                                                      */
/* 7.1.29  "0N"            */ FIELD Rabtyp5      AS CHARACTER /* ?????                                                                                                      */
/* 7.1.30  "0N"            */ FIELD Rabtyp6      AS CHARACTER /* ?????                                                                                                      */
/* 7.1.31  "0E"            */ FIELD Rabtyp7      AS CHARACTER /* ?????                                                                                                      */
/* 7.1.32  "0E"            */ FIELD Rabtyp8      AS CHARACTER /* ?????                                                                                                     */
/* 7.1.33  "0E"            */ FIELD Rabtyp9      AS CHARACTER /* ?????                                                                                                     */
/* 7.1.34  "0E"            */ FIELD Rabtyp10     AS CHARACTER /* ?????                                                                                                      */
/* 7.1.35  0               */ FIELD Rab1         AS INTEGER  /* ????? Rabatt i prosent eller kroner                                                                        */
/* 7.1.36  0               */ FIELD Rab2         AS INTEGER /* ?????                                                                                                      */
/* 7.1.37  0               */ FIELD Rab3         AS INTEGER /* ?????                                                                                                      */
/* 7.1.38  0               */ FIELD Rab4         AS INTEGER /* ?????                                                                                                      */
/* 7.1.39  0               */ FIELD Rab5         AS INTEGER /* ?????                                                                                                      */
/* 7.1.40  0               */ FIELD Rab6         AS INTEGER /* ?????                                                                                                      */
/* 7.1.41  0               */ FIELD Rab7         AS INTEGER /* ?????                                                                                                      */
/* 7.1.42  0               */ FIELD Rab8         AS INTEGER /* ?????                                                                                                      */
/* 7.1.43  0               */ FIELD Rab9         AS INTEGER /* ?????                                                                                                      */
/* 7.1.44  0               */ FIELD Rab10        AS INTEGER /* ?????                                                                                                      */
/* 7.1.45  24              */ FIELD Mva%         AS DECIMAL /* Mva-prosentD                                                                                         */
/* 7.1.46  1               */ FIELD Bonusg       AS INTEGER /* Bonusgruppe                     I(1)    Flagg som angir om en vare skal gi kjøpeutbytte eller ikke , men angitt med verdien 0=ikke kjøpeutbytte, 1=kjøpeutbytte   */
/* 7.1.47  "G"             */ FIELD Sorti        AS CHARACTER /* Sortiment                       C                                                                    */
/* 7.1.48  "N"             */ FIELD Vtype        AS CHARACTER /* Varetype  C       "N" = vanlig vare"K" = vektvare (veies i kassen)"O" = åpen   pris "I" = Ikke pris i kassen                                                        */
/* 7.1.49  0               */ FIELD Mm-nr        AS INTEGER   /* Mix-matchnummer                 I                                                                    */
/* 7.1.50  1               */ FIELD Etikett      AS INTEGER   /* Etikettnummer                   I       Jfr. "Varer i butikk", pkt. 5                                */
/* 7.1.51  0               */ FIELD Lag          AS INTEGER   /* Antall pr. pallelag                                                                                  */
/* 7.1.52  0               */ FIELD Pall         AS INTEGER   /* Antall pr. palle                I       Antall av 7.1.15 i pall (antall bestillingsenheter)          */
/* 7.1.53  628             */ FIELD Leverandor   AS INTEGER   /* Leverandørnummer                I(8)    Evt. underleverandør                                         */
/* 7.1.54  228             */ FIELD Filial       AS INTEGER   /* Filialnummer                                                                                         */
/* 7.1.55  0               */ FIELD Pant         AS DECIMAL   /* Pantebeløp i kroner             D                                                                    */
/* 7.1.56  0               */ FIELD DUN_Velosity AS INTEGER   /* ????? Bestnr. For pakn.                                                                                    */
/* 7.1.57  ""              */ FIELD AID_kode2    AS CHARACTER /* "E" eller "P"                           Angir om neste felt er EAN eller PLU                         */
/* 7.1.58  0               */ FIELD Linknr       AS DECIMAL   /* EAN/PLU linkvare                I(13)   Varenummer for koblet vare, f.eks. pant, miljøavgift e.l.    */
/* 7.1.59  ""              */ FIELD Statflag     AS CHARACTER   /* ????? Statistikkflagg                                                                                      */
/* 7.1.60  1               */ FIELD Produsent    AS INTEGER   /* ????? Produsentnummer                                                                                      */
/* 7.1.61  ""              */ FIELD AID_kode3    AS CHARACTER /* "E" eller "P"                           Angir om neste felt er EAN eller PLU                         */
/* 7.1.62  0               */ FIELD Salgskode3   AS DECIMAL   /* EAN/PLU-nummer                  I(13)   Alternativt varenummer (tandem)                              */
/* 7.1.63  ""              */ FIELD AID_kode4    AS CHARACTER /* "E" eller "P"                           Angir om neste felt er EAN eller PLU                         */
/* 7.1.64  0               */ FIELD Salgskode4   AS DECIMAL   /* EAN/PLU-nummer                  I(13)   Alternativt varenummer (tandem)                              */
/* 7.1.65  ""              */ FIELD Bnrbesk      AS CHARACTER /* Beskrivelse av bestillingsnummerKartong, pall, ½ pall o.l.                                           */
/* 7.1.66  0               */ FIELD Kampid       AS INTEGER   /* ????? Kampanjenummer                                                                                       */
/* 7.1.67  ""              */ FIELD Kampnavn     AS CHARACTER /* Kampanjenavn                                                                                         */
/* 7.1.68  0               */ FIELD ENVAnr       AS INTEGER   /* ENVAnummer                      I(4)    EAN Norges varegruppestandard                                */
/* 7.1.69  ""              */ FIELD Selektering1 AS CHARACTER /* Fri tekst                       C       Brukes til alternativ varegruppe (Elektronikkforbundet, NOBB o.l.)Tenkt bygd opp av 1 bokstav som angisbransje + etterfølgende siffer som angir varegruppenummer              */
/* 7.1.70  ""              */ FIELD Selektering2 AS CHARACTER /* Fri tekst                       I(8)    LeverandørID elektrisk/bygg (fabrikat)                       */
/* 7.1.71  ""              */ FIELD Selektering3 AS CHARACTER /* Fri tekst                                                                                            */
/* 7.1.72  N               */ FIELD Vardek       AS CHARACTER /* LOGICAL                                  J/N     Angir om det finnes deklarasjon på varen                     */
/* 7.1.73  J               */ FIELD Laggros      AS CHARACTER /* LOGICAL                         J/N     Angir om varen lagerføres hos grossist                       */
/* 7.1.74  N               */ FIELD Miljokode    AS CHARACTER /* LOGICAL                                 J/N     Angir om varen er miljøkodet                                 */
/* 7.1.75  N               */ FIELD Genmod       AS CHARACTER /* LOGICAL                                 J/N     Angir om varen er genmodifisert                              */
/* 7.1.76  N               */ FIELD Okologisk    AS CHARACTER /* LOGICAL                                J/N     Angir om varen er økologisk                                  */
/* 7.1.77  0               */ FIELD Erstvar      AS DECIMAL   /* Erstatningsvare                          EAN/PLUnummer for erstatningsvare                           */
/* 7.1.78  0               */ FIELD Fabrikat     AS INTEGER   /* ????? Fabrikatnummer                           Ifølge El.forbundets nummerserie                            */
/* 7.1.79  0               */ FIELD Konvfak      AS DECIMAL   /* Konverteringsfaktor             D       Brukes ved beregning av enhetspris                           */
/* 7.1.80  0               */ FIELD Konvpri      AS DECIMAL   /* Enhetspris                               Utregnet pris for sammenlignbar enhet                       */
/* 7.1.81  ""              */ FIELD Lok_i_butikk AS CHARACTER /* Fri tekst                                Angir varens lokalisering i butikk                          */
/* 7.1.82  ""              */ FIELD Planogram    AS CHARACTER /* ????? Planogram-id                                                                                         */
/* 7.1.83  0               */ FIELD Bredde       AS DECIMAL   /* Desimaltall                              Angir mål på forbrukerpakning                               */
/* 7.1.84  0               */ FIELD Dybde        AS DECIMAL   /* Desimaltall                              Angir mål på forbrukerpakning                               */
/* 7.1.85  0               */ FIELD Hoyde        AS DECIMAL   /* Desimaltall                              Angir mål på forbrukerpakning                               */
/* 7.1.86  0               */ FIELD Antbredde    AS INTEGER   /* Antall i bredde                          Eksponering mot kunde (facing)                              */
/* 7.1.87  0               */ FIELD Antdybde     AS INTEGER   /* Antall i dybde                           Eksponering mot kunde (facing)                              */
/* 7.1.88  0               */ FIELD Anthoyde     AS INTEGER   /* Antall i høyde                           Eksponering mot kunde (facing)                              */
/* 7.1.89  N               */ FIELD Anbrekk      AS CHARACTER /* LOGICAL                                J/N     Angir om vare kan bestilles i anbrekk                        */
/* 7.1.90                  */ FIELD Bestilles_fra_datoDato AS DATE /*                                         Angir når varen første gang kan bestilles                    */
/* 7.1.91                  */ FIELD Bestilles_til_datoDato AS DATE /*                                         Angi siste bestillingsdato                                   */
/* 7.1.92  0               */ FIELD Konvenh      AS INTEGER  /* Konverteringsenhet              I       Angir enhet som brukes ifm. enhetspris. Samme koder som i  felt 7.1.14                                                  */
/* 7.1.93  0               */ FIELD Garantiklasse AS INTEGER /* Garantiklassenummer             I       Jfr. "Salg av vare til kunde", pkt. 4                        */
/* 7.1.94  0               */ FIELD Idkrav        AS INTEGER /*                                 I (1)   Angir at det er aldersgrense på salg av varen, jfr. "Varer i butikk", pkt. 5.0=ingen, 1=alkohol, 2=tobakk)                */
/* 7.1.95  ""              */ FIELD Bilde        AS CHARACTER /* Navn på billedfil               C(20)   Jfr. "VPI", pkt. 8                                           */
/* 7.1.96  ""              */ FIELD Mersalg      AS CHARACTER /* EAN/PLUnummer                   C       Tilleggsvarer, forslag til mersalg for kasserer adskilt  med; mellom hvert varenr                                     */
/* 7.1.97  J               */ FIELD Lokalpris    AS CHARACTER /* LOGICAL J/N                             J/N     Tillatt å endre pris lokalt i InfoPOS i butikk (frukt&grønt) */
/* 7.1.98  0               */ FIELD Holdbarhet   AS INTEGER             /*                                 I (2)   Antall dager holdbarhet                                      */
/* 7.1.99  0               */ FIELD Taranetto    AS DECIMAL             /* Tara eller netto                D       Antall gram tara for vektvarer eller nettovekt for stkvarer  */
/* 7.1.100 0               */ FIELD Trykkbilde   AS INTEGER             /*                                 I (1)   Valg av trykkbilde på ferskvarevekt                          */
/* 7.1.101 ""              */ FIELD Etikettenhet AS CHARACTER           /*                                 C (10)  Tekst for salgsenehet som skal skrives på etikett/plakat     */
/* 7.1.102 13020           */ FIELD Modellnr     AS INTEGER             /*  I (8)   Modellnr                                                     */
/* 7.1.103 0               */ FIELD Variantnr    AS INTEGER             /*                                 I (3)   Sortering innen en modell                                    */
/* 7.1.104 ""              */ FIELD Farge        AS CHARACTER           /* Fargetekst                      C(10)   Fargetekst                                                   */
/* 7.1.105 ""              */ FIELD Storrelsetekst AS CHARACTER           /* Størrelsestekst                 C(10)   Størrelsestekst                                              */
/* 7.1.106 N               */ FIELD Kunlot       AS CHARACTER /* LOGICAL Bestilles kun i lot             J/N     Bestilling kun i LOT                                         */
/* 7.1.107 ""              */ FIELD FabrikatNavn AS CHARACTER           /* Fabrikatnavn                    C(30)   Navn på fabrikat - erstatter fabrikatnr (7.1.78)             */
/* 7.1.108 ""              */ FIELD Levvnr       AS CHARACTER           /* Underleverandørs varenr         C(20)   Underleverandørs varenr (dennes bestillingsnr)               */
/* 7.1.109 ""              */ FIELD Individtype  AS CHARACTER           /* Type individhåndtering          C(1)    E=elektro, V=våpen osv.                                      */
/* 7.1.110 ""              */ FIELD Artnr2       AS CHARACTER           /* Alfanumerisk internt artikkelnr C(20)                                                                */
/* 7.1.111 0               */ FIELD Matrialkode  AS INTEGER             /* I2 */
/* 7.1.112 0               */ FIELD Sesongkode	 AS INTEGER             /* I3 */
INDEX nummer IS PRIMARY nummer
.
