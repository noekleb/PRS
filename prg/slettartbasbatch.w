&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
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

DEF INPUT PARAMETER lArtikkelNr AS DEC FORMAT ">>>>>>>>>>>>>9" NO-UNDO.


DEF VAR cReturn-Value AS CHAR INITIAL "AVBRYT" NO-UNDO.
DEF VAR cSletteLogg   AS CHAR INITIAL "slettelogg.txt" NO-UNDO.
DEFINE VARIABLE cEanLst AS CHARACTER NO-UNDO.

DEF STREAM SletteLogg.

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

MESSAGE 'test-1'
VIEW-AS ALERT-BOX.
/*{sww.i} /* Session wait staite. */*/
RUN SlettArtikkel.
/*{swn.i} /* Session wait staite. */*/

RETURN cReturn-value.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SlettArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettArtikkel Procedure 
PROCEDURE SlettArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pbStat AS LOG INITIAL FALSE NO-UNDO.
  DEF VAR wBestHodeRecid AS RECID NO-UNDO.
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR iAntDager AS INT NO-UNDO.
  DEFINE VARIABLE bSlett AS LOG NO-UNDO.
  
  /* KonvReg */
  pcTekst = "".
  {syspara.i 1 2 2 pcTekst} /* RESTPAR */
  {syspara.i 2 3 2 iAntDager INT} /* Slettekontroll. Antall dager siden siste bevegelse. */
  IF iAntDager = 0 THEN
      iAntDager = 730.
  
  OUTPUT STREAM SletteLogg TO VALUE (cSletteLogg) APPEND.

MESSAGE 'test-2'
VIEW-AS ALERT-BOX.
  /* Henter artikkelen */
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
      cReturn-value = "Ukjent artikkelnr. " + STRING(lArtikkelNr) + ".".
      RETURN cReturn-value.
  END.
  /* Finnes det ikke oppdaterte transer for sanert artikkel, skal ikke artikkelen kunne slettes. */
  IF Artbas.sanertdato <> ? AND CAN-FIND(FIRST TransLogg WHERE Translogg.ArtikkelNr = ArtBas.ArtikkelNr AND
                                        TransLogg.postert = FALSE) THEN DO:
      cReturn-value = "Sanert artikkel med ikke oppdaterte transakjoner."  + STRING(lArtikkelNr) + ".".
      RETURN cReturn-value.
  END.
  
  MESSAGE 'test-3'
  VIEW-AS ALERT-BOX.
  /* Artikler med registrete lagerbevegelser slettes ikke. */
  /* Sanerte artikler slettes.                             */
  IF Artbas.sanertdato = ? THEN
  SJEKK_TRANSLOGG:
  DO:
    /* Varesalg */
    IF CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 1) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 2) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 3) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 4) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 5) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 6) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 7) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 8) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 10) OR  
       CAN-FIND(FIRST Translogg WHERE
              TransLogg.ArtikkelNr = ArtBas.ArtikkelNr AND 
              Translogg.Dato      >= TODAY - iAntDager AND
              Translogg.TTId       = 11)   
      THEN  
      DO:
        cReturn-value = "Det ligger varetransaksjoner på artikkelen "  + STRING(lArtikkelNr) + ".".
        RETURN cReturn-value.
      END. 
  END. /* SJEKK_TRANSLOGG */

MESSAGE 'test-4'
VIEW-AS ALERT-BOX.
  /* Artikler med ikke mottatte pakkseddler slettes ikke. */
  cReturn-value = ''.
  PkSdlSJEKK:
  FOR EACH PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10,
    EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE 
        PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr:
        cReturn-value = "Det ikke innleverte pakkseddler på artikkelen "  + STRING(lArtikkelNr) + ".".
        LEAVE PkSdlSJEKK.            
  END. /* PkSdlSJEKK */
  IF cReturn-Value <> '' THEN 
    RETURN cReturn-value.

  /* Artikler som ligger på ikke utleverte kundeordre, slettes ikke. */
  cReturn-value = ''.
  KOrdreSJEKK:
  FOR EACH KOrdreHode NO-LOCK WHERE 
    KOrdreHode.LevStatus < '50',
    EACH KOrdreLinje OF KOrdreHode NO-LOCK WHERE 
        KOrdreLinje.VareNr = STRING(ArtBas.ArtikkelNr):
        cReturn-value = "Det ikke utleverte kundeordre på artikkelen "  + STRING(lArtikkelNr) + ".".
        LEAVE KOrdreSJEKK.            
  END. /* KOrdreSJEKK */
  IF cReturn-Value <> '' THEN 
    RETURN cReturn-value.
  
  SLETTING:
  DO ON ERROR UNDO, RETRY: /* TRANSACTION - Locking ble for stor */
    /* Ligger det bestillinger som har leveringsdato fremmoveri tid, med status under 5, */
    /* skal artikkelen ikke slettes. Den er da lagt inn som forhåndsordre.               */
    bSlett = TRUE.
/*    BESTILLING:                                     */
/*    FOR EACH BestHode OF ArtBas NO-LOCK:            */
/*      /* Disse er innlevert eller delhvis levert. */*/
/*      IF BestHode.BestStat >= 5 THEN                */
/*        NEXT BESTILLING.                            */
/*      /* Ikke ferdigbehandlede bestillinger. */     */
/*      IF BestHode.LevDato = ? THEN                  */
/*        NEXT BESTILLING.                            */
/*      /* Gamle bestillinger. */                     */
/*      IF BestHode.LevDato < (TODAY - 60) THEN       */
/*        NEXT BESTILLING.                            */
/*      /* Da skal den slettes. */                    */
/*      ELSE DO:                                      */
/*        bSlett = FALSE.                             */
/*        LEAVE BESTILLING.                           */
/*      END.                                          */
/*    END.  /* BESTILLING */                          */
    /* Den har aktuelle forhåndsordre og skal ikke slettes. */
    IF bSlett = FALSE THEN LEAVE SLETTING.
      
    DO TRANSACTION:
        FIND FIRST KonvReg EXCLUSIVE-LOCK WHERE
             KonvReg.EDB-System = pcTekst AND
             KonvReg.Tabell     = "" AND
             KonvReg.EkstId     = string(ArtBas.ArtikkelNr) NO-ERROR.
        IF AVAILABLE KonvReg THEN
            DELETE KonvReg.
    END.

    /* Kontroll mot kampanjeregister */
    FOR EACH KampanjeLinje EXCLUSIVE-LOCK WHERE
        KampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr:
        DELETE KampanjeLinje.
    END.

    /* Sletter koblede underkategorier */
    FOR EACH ArtBasUnderKategori OF ArtBas EXCLUSIVE-LOCK:
        DELETE ArtBasUnderkategori.
    END.
    /* Sletter bestillingene */
    FOR EACH BestHode OF ArtBas NO-LOCK:
        ASSIGN
            wBestHodeRecid = RECID(BestHode).
        RUN w-gridord.w (INPUT RECID(ArtBas), 
                         INPUT-OUTPUT wBestHodeRecid, "SLETT").
    END.

    /* Sletter transaksjoner på artikkelen. */
    FOR EACH TransLogg EXCLUSIVE-LOCK WHERE
      TransLogg.ArtikkelNr = ArtBas.ArtikkelNr:
      DELETE TransLogg.
    END.

    /* Frikobler kundetransaksjoner */
    FOR EACH KundeTrans EXCLUSIVE-LOCK WHERE KundeTrans.ArtikkelNr = ArtBas.ArtikkelNr:
        ASSIGN KundeTrans.ArtikkelNr = 0.
    END.

    /* Frikobler medlemstransaksjoner */
    FOR EACH MedTrans EXCLUSIVE-LOCK WHERE 
        MedTrans.ArtikkelNr = ArtBas.ArtikkelNr:
        ASSIGN MedTrans.ArtikkelNr = 0.
    END.
    
    /* Sletter artikkelstatistikken. */
    FOR EACH StLinje EXCLUSIVE-LOCK WHERE
      StLinje.DataObjekt = string(ArtBas.ArtikkelNr,"9999999999999") AND
      StLinje.StTypeId     = "ARTIKKEL":
      DELETE StLinje.
      ASSIGN
          pbStat = TRUE
          .
    END.
    /* Sletter artikkelstatistikken. */
    FOR EACH StLinje EXCLUSIVE-LOCK WHERE
      StLinje.DataObjekt = string(ArtBas.ArtikkelNr,"9999999999999") AND
      StLinje.StTypeId     = "KASS-ART":
      DELETE StLinje.
      ASSIGN
          pbStat = TRUE
          .
    END.
    /* Sletter artikkelstatistikken. */
    FOR EACH StLinje EXCLUSIVE-LOCK WHERE
      StLinje.DataObjekt = string(ArtBas.ArtikkelNr,"9999999999999") AND
      StLinje.StTypeId     = "SELGER-ART":
      DELETE StLinje.
      ASSIGN
          pbStat = TRUE
          .
    END.

    /* Nullstiller koblinger til overføringstransaksjoner */
    FOR EACH OvBuffer EXCLUSIVE-LOCK WHERE
        OvBuffer.ArtikkelNr = ArtBas.ArtikkelNr:
        DELETE OvBuffer.
    END.

    FOR EACH VareBokLinje EXCLUSIVE-LOCK WHERE 
      VareBokLinje.ArtikkelNr = ArtBas.ArtikkelNr:
      DELETE VarebokLinje.
    END.
    FOR EACH ArtSort EXCLUSIVE-LOCK WHERE
      ArtSort.ArtikkelNr = ArtBas.ArtikkelNr:
      DELETE ArtSort.
    END.
    FOR EACH VareBehLinje EXCLUSIVE-LOCK WHERE
      VareBehLinje.ArtikkelNr = ArtBas.ArtikkelNr:
      DELETE VareBehLinje.  
    END.
    FOR EACH VareBehLinjeTrans EXCLUSIVE-LOCK WHERE 
      VareBehLinjeTrans.ArtikkelNr = ArtBas.ArtikkelNr:
      DELETE VareBehLinjeTrans.  
    END.
    FOR EACH VareBehBestHode EXCLUSIVE-LOCK WHERE
      VareBehBestHode.ArtikkelNr = ArtBas.ArtikkelNr:
      FOR EACH VareBehBestLinje OF VareBehBestHode EXCLUSIVE-LOCK:
        DELETE VareBehBestLinje.
      END.
      DELETE VareBehBestHode.
    END.
MESSAGE 'test-før slett artikkel'
VIEW-AS ALERT-BOX.    
    /* Sletter SELVE artikkelen */
    DO TRANSACTION:
        /* Sletter lageret */
        FOR EACH Lager OF ArtBas EXCLUSIVE-LOCK:
          DELETE Lager.
        END.

        /* Skal stLager tabellen trekkes ned ? */    
        FOR EACH ArtLag EXCLUSIVE-LOCK WHERE
          ArtLag.ArtikkelNr = ArtBas.ArtikkelNr:
          DELETE ArtLag.
        END.

        /* Sletter prisinformasjonen */
        FOR EACH ArtPris OF ArtBas EXCLUSIVE-LOCK:
          DELETE ArtPris.
        END.

        /* Sletter priskøposter */
        FOR EACH PrisKo OF ArtBas EXCLUSIVE-LOCK:
            DELETE PrisKo.
        END.

        /* Sletter prishistorikk */
        FOR EACH HPrisko OF ArtBAs EXCLUSIVE-LOCK:
            DELETE HPrisKo.
        END.

        /* Sletter Pakkelinjer om Pakke */
        IF ArtBas.Pakke THEN DO:
            FOR EACH PakkeLinje OF ArtBas:
                DELETE PakkeLinje.
            END.
        END.

        /* Sletter StrekKoder */
        cEanLst = ''.
        FOR EACH StrekKode OF ArtBas:
            ASSIGN 
                cEanLst = cEanLst + 
                          (IF cEanLst = '' THEN '' ELSE ',') + 
                          StrekKode.Kode.
            DELETE StrekKode.
        END.

        /* Solgte individer skal bli liggende med artikkelnummer satt til 0. */
        /* Usolgte indivder slettes sammen med artikkelen.                   */
        FOR EACH Individ OF ArtBas EXCLUSIVE-LOCK:
            IF Individ.SalgDato = ? THEN
                DELETE Individ.
            ELSE 
                individ.ArtikkelNr = 0.
        END.

        /* Logger slettet artikkel */
        PUT STREAM SletteLogg UNFORMATTED
            TODAY ";" 
            STRING(TIME,"HH:MM:SS") ";"
            USERID("SkoTex") ";"
            ArtBas.ArtikkelNr ";"
            ArtBas.LevKod ";"
            ArtBas.Beskr ";"
            ArtBas.LevFargKod ";"
            ArtBas.Vg ";"
            ArtBas.LopNr ";"
            ArtBas.LevNr ";"
            (IF pbStat
               THEN "STATISTIKK"
               ELSE "INGEN STAT") ";"
            cEanLst
            SKIP
            .


        /* Artikkelposten */
        FIND CURRENT ArtBas EXCLUSIVE-LOCK.
        DELETE ArtBas.
    END. /* TRANSACTION */

  END. /* SLETTING */

  OUTPUT STREAM SletteLogg CLOSE.

  /* Flagger at sletting har gått OK */
  ASSIGN
      cReturn-value = ""
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

