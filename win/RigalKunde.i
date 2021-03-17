DEFINE TEMP-TABLE TT_RigalVare NO-UNDO
/* Feltnr     Betegnelse                       Feltinnhold                     Format  Kommentar              */
/* 23.1   "KUN"            */ FIELD Kode          AS CHARACTER /* "KUN"                                       */
/* 23.2   517              */ FIELD Nummer        AS INTEGER   /* Kundenummer  I(6)                           */
/* 23.3   "E"              */ FIELD Flag          AS CHARACTER /* "N"-Ny "E"-Endring "S"-Sletting             */
/* 23.4   160166566        */ FIELD Medlemsnummer AS DECIMAL   /*  Medlemsnummer S-lagsnummer + medlemsnummer */
/* 23.5   "Jan Berthelsen" */ FIELD Navn          AS CHARACTER /* Navn x(50)                                  */
/* 23.6   "Nökkesv. 1"     */ FIELD Adresse       AS CHARACTER /* FDakturaadresse                             */
/* 23.7   12345            */ FIELD Postnr        AS INTEGER   /* Postnummer för fakturaadresse               */
/* 23.8   "Nökkesv. 1"     */ FIELD LevAdresse    AS CHARACTER /* Leveringsadresse                            */
/* 23.9   12345            */ FIELD Levpostnr     AS INTEGER   /* Postnummer för leveringsadresse             */
/* 23.10  "22273311"       */ FIELD Telefon       AS CHARACTER /* Telefon x(15)                               */
/* 23.11  "22273312"       */ FIELD Fax           AS CHARACTER /* Fax x(15)                                   */
/* 23.12   N               */ FIELD Sperret       AS LOGICAL   /* J/N                                         */
/* 23.13  10000            */ FIELD limit         AS INTEGER   /* Max kredit I(7)                             */
/* 23.14  222              */ FIELD Saldo         AS DECIMAL   /* Saldo                                       */
/* 23.15  20060101         */ FIELD Dato          AS INTEGER   /* 20060101 ÅÅÅÅMMDD                           */
/* 23.16  5                */ FIELD Kundegr       AS INTEGER   /* Kundegruppe                                 */
/* 23.17  6                */ FIELD TeamNr        AS INTEGER   /* ???                                         */
INDEX nummer IS PRIMARY nummer
.
