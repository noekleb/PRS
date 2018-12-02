&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

DEF INPUT PARAMETER piBatchNr AS INT  NO-UNDO.

DEF VAR plOms      AS DEC  NO-UNDO.
DEF VAR plDbKr     AS DEC  NO-UNDO.
DEF VAR plRab      AS DEC  NO-UNDO.
DEF VAR piHg       AS INT  NO-UNDO.
DEF VAR pcTTId     AS CHAR NO-UNDO.
DEF VAR piAntall   AS INT  NO-UNDO.
DEF VAR lVVAreKost AS DEC  NO-UNDO.

ASSIGN
    lVVAreKost = 0
    .
/* Leser transaksjonene og posterer p} dagsrapport/selger.     */
/* Kode 16 er ikke med her, da den ligger innbakt i 02 linjen. */
TRANSRAD:
FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
    Translogg.BatchNr = piBatchNr:

    IF TransLogg.Postert THEN
        NEXT TRANSRAD.

    IF TransLogg.TTId <> 1 THEN 
        NEXT TransRad.

    ASSIGN
        pcTTId = STRING(TransLogg.TTId,"999")
        .

    /* Henter ArtBas */
    FIND ArtBas NO-LOCK where
      ArtBas.Vg    = TransLogg.Vg and
      ArtBas.LopNr = TransLogg.LopNr NO-ERROR.

    /* Henter butikken */
    FIND Butiker NO-LOCK where
      Butiker.Butik = TransLogg.Butik NO-ERROR.

    /* Henter kalkyle som inneholder kalkulert "Solgte varers kostpris". */
    if AVAILABLE ArtBas and available Butiker then
      DO:
        /* Alltid fra Sentrallager. */
        FIND FIRST ArtPris NO-LOCK where
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
          ArtPris.Profil     = clButiker.Profil NO-ERROR.
      END.

    /* Henter VVArekost fra Lager filen. */
    FIND Lager NO-LOCK where
      Lager.ArtikkelNR = ArtBas.ArtikkelNR and
      Lager.Butik      = Butiker.Butik NO-ERROR.
    if NOT available Lager then
      FIND Lager NO-LOCK where
        Lager.ArtikkelNR = ArtBas.ArtikkelNR and
        Lager.Butik      = clButiker.Butik NO-ERROR.

    /* VVareKost pr. stk. */
    if AVAILABLE Lager then
      lVVAreKost = Lager.VVareKost.
    ELSE if AVAILABLE ArtPris then
      lVVAreKost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1].

    /* Beregner netto verdi hvis det er ønsket på dagsrapporten. */
    if iDags_Moms = 0 /* search("dags_moms") = ? */ then
    do:
        /*
        {p311f.i}.
        */
        ASSIGN
            /* Total rabatt */
            plRab = (IF TransLogg.Antall >= 0
                       THEN (TransLogg.RabKr + TransLogg.SubtotalRab)
                       ELSE (TransLogg.RabKr + TransLogg.SubtotalRab) * -1)
            /* Omsetning eks mva. */
            plOms = (IF TransLogg.Antall >= 0
                       THEN (TransLogg.Pris - plRab - TransLogg.Mva)
                       ELSE (TransLogg.Pris + plRab - TransLogg.Mva))
            plOms = plOms * ABS(Translogg.Antall)
            /* Trekker ut Mva fra rabatten. */
            plRab = (100 * plRab   ) / (100 + ArtPris.Mva%[IF ArtPris.Tilbud THEN 2 ELSE 1])
            /* Setter antall */
            piAntall = TransLogg.Antall
            .
        /* Beregner varekost hvis denne er ukjent. */
        IF lVVAreKost = 0 then
          DO:
            IF AVAILABLE ArtBas then
              FIND VarGr of ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE VarGr then
              lVVAreKost = (plOms * VarGr.Kost_Proc) / 100.
          END.
        IF lVVareKost = ? 
            THEN lVVareKost = 0.

        /* Beregner DB hvis Artpris er ukjent. */
        if not available ArtPris then
          do:
            plDbKr = plOms * plDbFaktorNetto. /* o.30. -  30% om vare ej finnes */
          end.
        /* Beregner DB */
        else
          plDbKr = plOms - ABS(lVVAreKost * piAntall).
   
    end.
    /* Brutto verdi */
    else do:
      IF lVVAreKost = 0 then
        DO:
          IF AVAILABLE ArtBas then
            FIND VarGr of ArtBas NO-LOCK NO-ERROR.
          IF AVAILABLE VarGr then
            lVVAreKost = (plOms * VarGr.Kost_Proc) / 100.
        END.
      IF lVVareKost = ? THEN
          lVVareKost = 0.

      /* Omsetning - inkl. Mva og ekskl. rabatter. */
      ASSIGN
          plOms = (TransLogg.Pris - TransLogg.RabKr - TransLogg.SubtotalRab)
          plOms = plOms * ABS(TransLogg.antall)
          plOms = plOms * (IF TransLogg.Antall >= 0
                             THEN 1
                             ELSE -1)
          .
      /* Beregner DB hvis ikke artpris finnes. */
      if not available ArtPris then
        do:
          plDbKr = plOms * plDbFaktorBrutto. /* 0.24.  - 30% om vare ej finnes */
        end.
      else
        /*plDbKr = (plOms * 0.80) - (lVVAreKost * BongLinje.Antall). */
          ASSIGN
              plDbKr = (plOms * (1 / (1 + (plMva% / 100)))) - (lVVareKost * TransLogg.Antall)
              plDbKr = IF plDbKr = ? THEN 0 ELSE plDbKr
              .
      ASSIGN
          plRab = TransLogg.RabKr + TransLogg.SubtotalRab
          plRab = plRab * (IF Translogg.Antall >= 0
                             THEN 1
                             ELSE -1)
          .
    end.

    /* Henter dagsrapport filen */
    find dags_rap where dags_rap.butik = TransLogg.Butik and
                        dags_rap.dato  = TransLogg.dato
                        exclusive-lock no-error.

    /* Legger opp posten hvis den ikke finnes. */
    if not available dags_rap then
    do:
        create dags_rap.
        assign dags_rap.butikk = TransLogg.Butik
               dags_rap.dato   = TransLogg.dato
               dags_rap.mnd    = month(TransLogg.dato)
               dags_rap.aar    = year(TransLogg.dato).
    end.

    /* Bestemmer avdelingen.                                   */
    /* Finnes ikke avdelingen, legges salget p} avd6           */
    /* Hovedgruppe "0" skal ogs} p} diverse.                   */
    find vargr where 
        vargr.vg = TransLogg.Vg no-lock no-error.
    FIND HuvGr NO-LOCK WHERE
        HuvGr.Hg = VarGr.Hg NO-ERROR.
    if available HuvGr then 
        piHg = HuvGr.AvdelingNr.
    else 
        piHg = 6.

    if piHg = 0 then 
        piHg = 6.
    IF piHg > 6 THEN
        piHg = 6.

    /* Gj|r posteringene */
    if piHg = 1 then
    do:
        dags_rap.tb1 = dags_rap.tb1 + plDbKr.
        dags_rap.hg1_oms = dags_rap.hg1_oms + plOms.
    end.
    else if piHg = 2 then
    do:
        dags_rap.tb2 = dags_rap.tb2 + plDbKr.
        dags_rap.hg2_oms = dags_rap.hg2_oms + plOms.
    end.
    else if piHg = 3 then
    do:
        dags_rap.tb3 = dags_rap.tb3 + plDbKr.
        dags_rap.hg3_oms = dags_rap.hg3_oms + plOms.
    end.
    else if piHg = 4 then
    do:
        dags_rap.tb4 = dags_rap.tb4 + plDbKr.
        dags_rap.hg4_oms = dags_rap.hg4_oms + plOms.
    end.
    else if piHg = 5 then
    do:
        dags_rap.tb5 = dags_rap.tb5 + plDbKr.
        dags_rap.hg5_oms = dags_rap.hg5_oms + plOms.
    end.
    else if piHg = 6 then
    do:
        dags_rap.tb6 = dags_rap.tb6 + plDbKr.
        dags_rap.hg6_oms = dags_rap.hg6_oms + plOms.
    end.

end. /* TRANSRAD. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


