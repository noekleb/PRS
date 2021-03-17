DEFINE TEMP-TABLE TT_akt_rapp NO-UNDO  SERIALIZE-NAME "Timesalg" 
    FIELD butik          AS INTE  LABEL "Butikk"  
    FIELD tid_txt        AS CHAR  LABEL "Tekst"  
    FIELD bruttosalg     AS DECI      LABEL "Salg brutto"
/*     FIELD oms_verd       AS DECIMAL LABEL "Oms. Verdi" */
/*     FIELD svk            AS deci  LABEL "Varekost" */
/*     FIELD mva_kr         AS deci  LABEL "Mva" */
/*     FIELD DBKr       AS DECI      LABEL "DB Kr" */
    FIELD DBproc        AS DECI   LABEL "DB%"
    FIELD ant_kunder     AS INTE  LABEL "Kunder"
    FIELD ant_kvitto     AS INTE FORMAT ">>9" LABEL "Kvitteringer"
    FIELD ant_ret        AS INTE FORMAT ">>9" LABEL "Returer"
    FIELD verd_ret       AS DECI FORMAT ">>>,>>9" LABEL "Returer kr"
    INDEX tid_txt tid_txt.
/*     FIELD dato           AS date  LABEL "Dag"       */
/*     FIELD uke_dag        AS inte  LABEL "Uke Dag"   */
/*     FIELD uke_nr         AS inte  LABEL "Uke Nr."   */
/*     FIELD mnd            AS inte  LABEL "Måned"     */
/*     FIELD kasse          AS inte  LABEL "Kasse"     */
/*     FIELD tid            AS inte  LABEL "Tid"       */
/*     FIELD oms_ant        AS deci  LABEL "Oms. Ant." */

DEFINE TEMP-TABLE TT_Butiker NO-UNDO   SERIALIZE-NAME "Butikker"
    FIELD Butik         AS  INTE LABEL "Butikk"
    FIELD butnamn       AS  CHAR FORMAT "x(30)" LABEL "Beskrivelse"
    FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
    FIELD salgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD salgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Budsjett Kr"
/*     FIELD VerdiSolgt    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg netto" */
    FIELD DBproc           AS  DECI LABEL "DB%"
    FIELD VerdiRabatt   AS  DECI FORMAT ">,>>>,>>9" LABEL "Rabatter netto"
/*     FIELD DBkr          AS  DECI FORMAT ">,>>>,>>9" LABEL "DB kr" */
/*     FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*     FIELD vvarekost     AS  DECI FORMAT ">,>>>,>>9"LABEL "Varekost" */
    FIELD GjenkjopAnt   AS  DECI FORMAT ">>9" LABEL "Returer"
    FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9"LABEL "Returer kr"
    FIELD Antkunder     AS  INTE FORMAT ">>9" LABEL  "Kunder"
    FIELD Antkvitt      AS  INTE FORMAT ">>9" LABEL  "Kvitteringer"
    INDEX butik IS UNIQUE PRIMARY butik.

DEFINE TEMP-TABLE TT_SBudDW NO-UNDO   SERIALIZE-NAME "Budsjett"
    FIELD Butik            AS  INTE LABEL "Butikk"
    FIELD butnamn          AS  CHAR FORMAT "x(30)" LABEL "Beskrivelse"
    FIELD DAGsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD DAGsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD DAGSalgAvvikKr   AS  DECI LABEL "Avvik kr"
    FIELD DAGSalgAvvikproc AS  DECI LABEL "Avvik %" 
    FIELD Skille           AS CHARACTER LABEL '' FORMAT "x(1)" INITIAL ' '
    FIELD UKEsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD UKEsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD UKESalgAvvikKr   AS  DECI FORMAT ">,>>>,>>9" LABEL "Avvik kr"
    FIELD UKESalgAvvikproc AS  DECI LABEL "Avvik %" 
    INDEX butik IS UNIQUE PRIMARY butik.

DEFINE TEMP-TABLE TT_SBudMY NO-UNDO   SERIALIZE-NAME "Budsjett"
    FIELD Butik            AS  INTE LABEL "Butikk"
    FIELD butnamn          AS  CHAR FORMAT "x(30)" LABEL "Beskrivelse"
    FIELD MNDsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD MNDsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD MNDSalgAvvikKr   AS  DECI LABEL "Avvik kr"
    FIELD MNDSalgAvvikproc AS  DECI LABEL "Avvik %" 
    FIELD Skille           AS CHARACTER LABEL '' FORMAT "x(1)" INITIAL ' '
    FIELD AARsalgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD AARsalgBudsjett  AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg budsjett"
    FIELD AARSalgAvvikKr   AS  DECI FORMAT ">,>>>,>>9" LABEL "Avvik kr"
    FIELD AARSalgAvvikproc AS  DECI LABEL "Avvik %" 
    INDEX butik IS UNIQUE PRIMARY butik.

DEFINE TEMP-TABLE TT_Avdeling NO-UNDO SERIALIZE-NAME "Avdeling"
    FIELD avdelingnr    AS  INTE LABEL "Avdeling"
    FIELD AvdelingNavn  AS  CHAR LABEL "Beskrivelse"
    FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
    FIELD salgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
    FIELD DBproc        AS  DECI LABEL "DB%"
/*     FIELD VerdiSolgt    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg netto" */
    FIELD VerdiRabatt   AS  DECI FORMAT ">,>>>,>>9" LABEL "Rabatter netto"
/*     FIELD DBkr          AS  DECI FORMAT ">>>,>>>,>>9" LABEL "DB kr" */
/*     FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*     FIELD vvarekost     AS  DECI LABEL "Varekost" */
    FIELD GjenkjopAnt   AS  DECI FORMAT ">>9"     LABEL "Returer"
    FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9" LABEL "Returer kr"
    INDEX avdelingnr IS UNIQUE PRIMARY avdelingnr.

DEFINE TEMP-TABLE TT_Lev NO-UNDO SERIALIZE-NAME "Leverandør"
  FIELD levnr         AS  INTE LABEL "Levnr"
  FIELD LevNavn       AS  CHAR LABEL "Beskrivelse"
  FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
  FIELD salgbrutto    AS  DECI LABEL "Salg brutto"
  FIELD DBproc           AS  DECI LABEL "DB%"
/*   FIELD VerdiSolgt    AS  DECI LABEL "Salg netto" */
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto"
/*   FIELD DBkr          AS  DECI LABEL "DB kr" */
/*   FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*   FIELD vvarekost     AS  DECI LABEL "Varekost" */
  FIELD GjenkjopAnt   AS  DECI FORMAT ">>9" LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9" LABEL "Returer kr"
    INDEX levnr IS UNIQUE PRIMARY levnr.

DEFINE TEMP-TABLE TT_Selger NO-UNDO  SERIALIZE-NAME "Selgere"
  FIELD selgernr      AS  INTE LABEL "Selger"
  FIELD Navn      AS  CHAR LABEL "Beskrivelse"
  FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
  FIELD salgbrutto    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg brutto"
  FIELD DBproc           AS  DECI LABEL "DB%"
/*   FIELD VerdiSolgt    AS  DECI FORMAT ">,>>>,>>9" LABEL "Salg netto" */
  FIELD VerdiRabatt   AS  DECI FORMAT ">,>>>,>>9" LABEL "Rabatter netto"
/*   FIELD DBkr          AS  DECI FORMAT ">,>>>,>>9" LABEL "DB kr" */
/*   FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*   FIELD vvarekost     AS  DECI LABEL "Varekost" */
  FIELD GjenkjopAnt   AS  DECI FORMAT ">>9" LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9" LABEL "Returer kr"
  FIELD AntKunder     AS  INTE FORMAT ">>9" LABEL "Kunder"
  FIELD Hitrate       AS DECI  FORMAT ">>9.9" LABEL "Hitrate %"
  FIELD Merfsg        AS  DECI FORMAT ">>9.9" LABEL "Mer fsg %"
  FIELD AntUtens      AS DECI
  FIELD SumUtensBrutto AS DECI
    INDEX selgernr IS UNIQUE PRIMARY Selgernr.


DEFINE TEMP-TABLE TT_HuvGr NO-UNDO  SERIALIZE-NAME "Hovedgrupper"
  FIELD hg            AS  INTE LABEL "Hg"
  FIELD HgBeskr       AS  CHAR LABEL "Beskrivelse"
  FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
  FIELD salgbrutto    AS  DECI LABEL "Salg brutto"
  FIELD DBproc           AS  DECI LABEL "DB%"
/*   FIELD VerdiSolgt    AS  DECI LABEL "Salg netto" */
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto"
/*   FIELD DBkr          AS  DECI LABEL "DB kr" */
/*   FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*   FIELD vvarekost     AS  DECI LABEL "Varekost" */
  FIELD GjenkjopAnt   AS  DECI FORMAT ">>9" LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9" LABEL "Returer kr"
    INDEX hg IS UNIQUE PRIMARY hg.

DEFINE TEMP-TABLE TT_Vg NO-UNDO SERIALIZE-NAME "Varegrupper"
  FIELD vg            AS  INTE LABEL "Vg"
  FIELD VgBeskr       AS  CHAR LABEL "Beskrivelse"
  FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
  FIELD salgbrutto    AS  DECI LABEL "Salg brutto"
  FIELD DBproc           AS  DECI LABEL "DB%"
/*   FIELD VerdiSolgt    AS  DECI LABEL "Salg netto" */
  FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto"
/*   FIELD DBkr          AS  DECI LABEL "DB kr" */
/*   FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*   FIELD vvarekost     AS  DECI LABEL "Varekost" */
  FIELD GjenkjopAnt   AS  DECI FORMAT ">>9" LABEL "Returer"
  FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9" LABEL "Returer kr"
    INDEX vg IS UNIQUE PRIMARY vg.

DEFINE TEMP-TABLE TT_VareSalg NO-UNDO  SERIALIZE-NAME "Varesalg"
    FIELD Artikkelnr    AS  DECI FORMAT ">>>>>>>>>>>>>9" LABEL "Artikkelnr"
    FIELD Beskr         AS  CHAR LABEL "Beskrivelse"
    FIELD Farg          AS  CHAR LABEL "Farge"
    FIELD AntSolgt      AS  DECI LABEL "Solgt" FORMAT ">>>>9"
    FIELD salgbrutto    AS  DECI LABEL "Salg brutto"
    FIELD DBproc        AS  DECI LABEL "DB%"
/*     FIELD VerdiSolgt    AS  DECI LABEL "Salg netto" */
    FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto"
/*     FIELD DBkr          AS  DECI LABEL "DB kr" */
/*     FIELD mvaverdi      AS  DECI LABEL "Mva" */
/*     FIELD vvarekost     AS  DECI LABEL "Varekost" */
    FIELD GjenkjopAnt   AS  DECI FORMAT ">>9" LABEL "Returer"
    FIELD GjenkjopVerdi AS  DECI FORMAT ">>>,>>9" LABEL "Returer kr"
    FIELD LevKod        AS  CHAR LABEL "Levkod"
/*     FIELD Varemerke     AS  CHAR LABEL "Varemerke" */
      INDEX Artikkelnr  IS UNIQUE PRIMARY Artikkelnr.

