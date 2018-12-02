DEF VAR cTekst     AS CHAR NO-UNDO.

CURRENT-WINDOW:WIDTH = 250.
/*
ASSIGN
cTekst = "001,003,004,008,010,203,091,062,061,094,096,057,059,050,079,053,054,058,065,002,011,010,003,005,007,009" +
         ",006,090,093,132,092,086,072,150,089,088,144,149,095,066,145,146,135,136,147,134,056,136,113"
    .
*/

cTekst = "001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,022,023,050,079,051,052,053," + 
             "054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069," + 
             "070,071,072,073,080,081,082,086,087,088,089,090,091,092,093,094,095,096,097,098,099," +
             "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143,144,145,146,149," +
             "200,201,202,203,147,150,132,135,134,136,056,136,203,114,204".

/* 
Overgangstabell mellom InfoPOS 8.0 koder og PRS POS koder. 
----------------------------------------------------------
InfoPOS 8.0              PRS POS
----------------------   ---------------------------------
Varesalg          ,003 - 001,003,010
Åpning av kasse   ,012 - 203
Bong slettet      ,019 - 091
Utbetaling        ,022 - 062
Innbetaling       ,023 - 061
Medlem            ,024 - 094
Signon/off        ,030 - 096
Pant              ,035 - 057
Dropp             ,052 - 059
Kontant           ,055 - 050
Reserveløsning    ,056 - 079
Gavekort          ,057 - 053 
Sjekk             ,058 - 054
Bank              ,059 - 058
Kreditt           ,060 - 065
Varetransaksjoner ,061 - 002,003,005,006,007,009,010,011,024,025 
                       - Brekkasje           1 - 002
                       - Innternt forbruk    2 - 011
                       - Gjenkjøp            3 - 010
                       - Reklamasjon         4 - 003
                       - Varemottak          5 - 005
                       - Lagerjustering      6 - 007
                       - Varetelling         7 - 009
                       - Internt salg        9 - 006
                       - Trening            10 - Transaksjonene på bongen ignoreres
                       - Etikett (Artikkel) 20 - 024
                       - Etkett (Batch)     19 - 025
Kunde             ,062 - 090
Grandtotal        ,068 - 093
Pakkevare         ,073 - 132
EOD post          ,074 - 092
Kundeordre        ,075 - 086
Depositum         ,077 - 072
Kassereroppgjør   ,078 - 150
Innbetaling kunde ,080 - 089
Kundeinfo         ,082 - 088
Korr. finanspost  ,083 - IKKE IMPLEMENTERT
Valutainformasjon ,084 - IKKE IMPLEMENTERT
Kundeinformasjon  ,087 - IKKE IMPLEMENTERT
Tekst bankterm.   ,088 - 144
Man. prisendring  ,091 - 149
Fri tekst/bankterm,092 - 095
Tilgode           ,094 - 066
Feil/tiltakskode  ,095 - 145
Selgernr          ,102 - 146
Utbetalingstype   ,104 - 135
Gave/tilgode motta,105 - 136 - Se også 112
Størrelseskode    ,106 - 147
Gavekort          ,108 - 134
Kundeinformasjon  ,110 - IKKE IMPLEMENTERT
Finansiering      ,111 - 056
Gave/tilgode motta,112 - 136
Oppd. tildigere   ,113 - 113
Individ/serienr   ,119 - IKKE IMPLEMENTERT
Spør på pris      ,120 - IKKE IMPLEMENTERT
Varenummer        ,125 - IKKE IMPLEMENTERT
PremiumCheck      ,126 - IKKE IMPLEMENTERT
SaleId            ,131 - IKKE IMPLEMENTERT
Pakkevarelinje    ,8195 - IKKE IMPLEMENTERT
Z-Rapport         ,204 - 204
*/

DO TRANSACTION:
  IF NOT CAN-FIND(Transtype WHERE
                  TransType.TTId = 114) THEN
  DO:
      CREATE TransType.
      ASSIGN
          TransType.TTID         = 114
          TransType.Beskrivelse  = "Avslutt varetransaksjon"
          TransType.Aktiv        = TRUE
          .
      CREATE TransBeskr.
      ASSIGN
          TransBeskr.TTId        = 114
          TransBeskr.TBId        = 1
          TransBeskr.Beskrivelse = "Avslutt varetransaksjon"
          .
  END.
END. /* TRANSACTION */

FOR EACH TransType EXCLUSIVE-LOCK:
    /*
    DISPLAY
        TransType.TTID
        TransType.Beskrivelse
        TransType.Aktiv
        CAN-DO(cTekst,STRING(TransType.TTID,"999"))
        .
    */
    ASSIGN
        TransType.Aktiv = CAN-DO(cTekst,STRING(TransType.TTID,"999")).
END.
