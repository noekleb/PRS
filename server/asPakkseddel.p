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
    Notes       : RUN asPakkseddel.p (BongLinje.ButikkNr, BongLinje.Antall, bEtikettKasse).
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE INPUT  PARAMETER ipiButikkNr     AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE INPUT  PARAMETER iplAntall       AS DECIMAL FORMAT "->>>>>>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT  PARAMETER ipbEtikettKasse AS LOG NO-UNDO.
DEFINE INPUT  PARAMETER bSkrivEtikett   AS LOG NO-UNDO. 
DEFINE OUTPUT PARAMETER bbOk             AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn         AS CHARACTER NO-UNDO.

DEFINE VARIABLE ibuntNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iSeq        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cInfoRad1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoRad2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoRad3   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInfoRad4   AS CHARACTER NO-UNDO.
DEFINE VARIABLE icParam     AS CHARACTER      NO-UNDO.
DEFINE VARIABLE ihBuffer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE ihBufPkSdl  AS HANDLE    NO-UNDO.
DEFINE VARIABLE icSessionId AS CHARACTER      NO-UNDO.
DEFINE VARIABLE ocReturn    AS CHARACTER      NO-UNDO.
DEFINE VARIABLE obOK        AS LOG       NO-UNDO.
DEFINE VARIABLE cButikkNr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIkkeEttikettBatch AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutletLst  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop       AS INTEGER NO-UNDO.
DEFINE VARIABLE ix AS INTEGER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iNettButLager AS INTEGER NO-UNDO.
DEFINE VARIABLE iFraBut     AS INTEGER NO-UNDO.
DEFINE VARIABLE iDummy      AS INTEGER NO-UNDO.
DEFINE VARIABLE piLinjeNr   AS INTEGER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER NO-UNDO. 
DEFINE VARIABLE ceComLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFaktura_Id LIKE FakturaHode.Faktura_Id NO-UNDO.
DEFINE VARIABLE bGyldig AS LOG NO-UNDO.
DEFINE BUFFER bufArtBas  FOR ArtBas.
DEFINE BUFFER bufArtPris FOR ArtPris.
DEFINE BUFFER bufButiker FOR Butiker.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFakturafilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE bSkipJBoxInit AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttPkSdlHode
  FIELD PkSdlId   AS DECIMAL FORMAT ">>>>>>>>>>>>9"
  FIELD SendtDato AS DATE 
  FIELD PkSdlNr   AS CHARACTER 
  FIELD EkstId    AS CHARACTER 
  INDEX Pakkseddel PkSdlNr SendtDato
  . 
DEFINE TEMP-TABLE tmpPkSdlHode LIKE PkSdlHode.
   
DEFINE TEMP-TABLE ttpkSdlLinje LIKE PkSdlLinje.

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

{etikettlogg.i &NEW=NEW}
{overforing.i &NEW=NEW &SHARED="Shared"}

/* sjekker om init av Jukebox skal kjøres. Er programmet starter fra kassen, skal det initieres, ellers ikke. */
PUBLISH 'skipInitJukeBox' (OUTPUT bSkipJBoxInit).
IF bSkipJBoxInit = FALSE THEN
    {initjukebox.i}
ASSIGN
    bbOk = bOk
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

/* Liste over butikker som ikke skal ha etiketter når de gjør varemottak via kassen. */
/* Dette gjelder varemottak som gjøres ved å bestille etiketter fra pakkseddel.      */
/* Ref. løsning som er gjort for Gant.                                               */
{syspara.i 22 5 1 cIkkeEttikettBatch}

{syspara.i 22 5 2 cOutletLst}

{syspara.i 150 1 3 iNettButLager INT}

{syspara.i 5 1 1 iFraBut INT}

{syspara.i 210 100 8 iGantAktiv INT}

{syspara.i 150 1 3 ceComLst}
 
PUBLISH 'getPkSdlLogfileName' (OUTPUT cLogg).

ASSIGN 
    bTest = TRUE 
    cLogg = IF cLogg = '' THEN 'PakkseddelInnlevFraKasse' + REPLACE(STRING(TODAY),'/','') ELSE cLogg
    .

SUBSCRIBE TO "getPkSdlId" ANYWHERE.
SUBSCRIBE TO "getPkSdlNr" ANYWHERE.
SUBSCRIBE TO "putFakturaId" ANYWHERE.
SUBSCRIBE TO 'fakturaFilNavn' ANYWHERE.
/* Tømmer tmp-tabell */
EMPTY TEMP-TABLE ttPkSdlHode.
EMPTY TEMP-TABLE ttpkSdlLinje.

FIND Butiker NO-LOCK WHERE
    Butiker.Butik = ipiButikkNr NO-ERROR.

RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Forespørsel om innleveranse av pakkseddel.' 
    + ' Butikk: '     + STRING(ipiButikkNr)
    + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
    ).

IF NOT AVAILABLE Butiker THEN
DO: 
  ASSIGN bbOk     = FALSE
         cReturn = 'Ugyldig butikknr. angitt for pakkseddel.  Butikk:' + TRIM(STRING(ipiButikkNr,"->>>>>>>>>>>>>>>>9")).
         RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Ugyldig butikknr. angitt for pakkseddel.' 
                               + ' Butikk: '     + STRING(ipiButikkNr)
                               + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                               + ' Ok: '         + STRING(obOk)    
                               ).
  RETURN.
END.

/* Legger opp liste */
FIND LAST PkSdlHode NO-LOCK WHERE
    LEFT-TRIM(PkSdlHode.PkSdlNr,'0') = TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))  AND 
    PkSdlHode.PkSdlStatus = 10 AND 
    CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE PkSdlLinje.ButikkNr = ipiButikkNr)
    USE-INDEX SendtDato NO-ERROR.

IF ipbEtikettKasse = FALSE THEN
DO: /* Oppretter SLUTT etikett (Den skrives ut først). */
    ASSIGN cInfoRad1 = "Varemottak fra kasse"
           cInfoRad3 = IF AVAILABLE Butiker THEN Butiker.butnamn ELSE ""
           cInfoRad4 = "SLUTT"
           iSeq      = iSeq + 1.
    CREATE EtikettLogg.
    ASSIGN
    cInfoRad2             = TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9")) /* PakkseddelNr */
    EtikettLogg.Butik     = ipiButikkNr /* Det skal skrives ut i seqnr ordning. */
    EtikettLogg.Vg        = 0   
    EtikettLogg.LopNr     = 0
    EtikettLogg.Ant       = 0
    EtikettLogg.Storl     = "INFO"
    EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4
    EtikettLogg.Pris      = 0
    EtikettLogg.Pris2     = 0
    EtikettLogg.SeqNr     = iSeq.
END.

/* Er det en Outlet som gjør varemottak og pakkseddelen ligger på en annen outlet butikk, skal pakkseddelen flyttes. */
/* Og varemottak skal deretter gå som normalt.                                                                       */
IF NOT AVAILABLE PkSdlHode THEN
OUTLET_SJEKK: 
DO:
    RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Starter sjekk av Outlet.' 
        + ' Butikk: '     + STRING(ipiButikkNr)
        + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
        + ' Outlet liste: ' + cOutletLst
        ).

    IF CAN-DO(cOutletLst,STRING(ipiButikkNr)) OR 
       iNettButLager = ipiButikkNr THEN 
    DO iLoop = 1 TO NUM-ENTRIES(cOutletLst):
        IF ipiButikkNr = INT(ENTRY(iLoop,cOutletLst)) THEN 
            NEXT.
          
        FIND LAST PkSdlHode NO-LOCK WHERE
            PkSdlHode.PkSdlNr = TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))  AND 
            PkSdlHode.PkSdlStatus = 10 AND 
            CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE PkSdlLinje.ButikkNr = INT(ENTRY(iLoop,cOutletLst)))
            USE-INDEX SendtDato NO-ERROR.
            
        RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: sjekker Outlet ' 
            + ' Butikk: '     + ENTRY(iLoop,cOutletLst)
            + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
            + ' Resultat: ' + STRING(AVAILABLE PkSdlHode)
            ).
            
        /* Finner vi pakkseddelen, skal den flyttes til butikken som ber om innleveranse. */
        IF AVAILABLE PkSdlHode THEN 
        DO:
            RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: starter bytting av butikknr ' 
                + ' Butikk: '     + STRING(ipiButikkNr)
                + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                + ' Resultat: ' + STRING(AVAILABLE PkSdlHode)
                ).

            /* Tømmer tmpTable for sikker hets skyld. */
            FOR EACH tmpPkSdlHode:
                DELETE tmpPkSdlHode.
            END. 
            CREATE tmpPkSdlHode.
            BUFFER-COPY PkSdlHode TO tmpPkSdlHode.
            
            /* Tar vare på handle til buffer som sendes til programmet. */
            ihBufPkSdl = BUFFER tmpPkSdlHode:HANDLE.
            /* Flytter varelinjene */
            RUN pksdl_ByttButNr.p ( 
                STRING(ipiButikkNr),
                ihBufPkSdl, 
                '?',
                OUTPUT ocReturn,
                OUTPUT obOk
                ) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Feil ved bytte av butikknr: ' 
                    + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                    ).
            END.

            RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Endret butikknr på pakkseddel. ' 
                + ' Fra butikk: '     + ENTRY(iLoop,cOutletLst)
                + ' Til butikk: '     + STRING(ipiButikkNr)
                + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                + ' Ok: '         + STRING(obOk)    
                ).

            /* Så forlater vi blokken, og lar varemottak gå sin gang.          */
            /* Pakkseddelen er nå tilgjengelig, slik at varemottak kan kjøres. */
            LEAVE OUTLET_SJEKK.
        END.
    END.
END. /* OUTLET_SJEKK */

/* Sjekker om pakkseddel allerede er innlevert for butikk. */  
IF NOT AVAILABLE pkSdlHode THEN 
DO:
  FIND LAST PkSdlHode NO-LOCK WHERE
  PkSdlHode.PkSdlNr = TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))  AND 
  CAN-FIND(FIRST PkSdlLinje OF PkSdlHode WHERE PkSdlLinje.ButikkNr = ipiButikkNr)
  USE-INDEX SendtDato NO-ERROR.
  IF AVAILABLE PkSdlHode THEN 
  DO:
    ASSIGN
         cReturn = 'Pakkseddel allerede innlevert. ' + IF bSkrivEtikett THEN '' ELSE 'Ingen etikettutskrift er valgt.'.
         RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Pakkseddel allerede innlevert. ' 
                               + IF bSkrivEtikett THEN '' ELSE 'Ingen etikettutskrift er valgt.'
                               + ' Butikk: '     + STRING(ipiButikkNr)
                               + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                               + ' Ok: '         + STRING(obOk)    
                               ).
    IF bSkrivEtikett = FALSE 
      THEN RETURN.
  END.
END.

/* Sjekker om det er ukjent  */
IF NOT AVAILABLE PkSdlHode THEN
DOBBELSJEKK: 
DO:   
  FIND LAST PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlNr = TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
    USE-INDEX SendtDato NO-ERROR.
  IF AVAILABLE PkSdlHode THEN 
  DO:
    ASSIGN bbOk     = FALSE
         cReturn = 'Pakkseddel finnes, men ligger på en annen butikk eller mangler varelinjer. ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9")).
         RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Pakkseddel finnes, men ligger på en annen butikk eller mangler varelinjer. ' 
                               + ' Butikk: '     + STRING(ipiButikkNr)
                               + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                               + ' Ok: '         + STRING(obOk)    
                               ).
    RETURN.
  END.
  ELSE DO:
    ASSIGN bbOk     = FALSE
         cReturn = 'Ugyldig pakkseddelnr. ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9")).
         RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Ugyldig pakkseddelnr.' 
                               + ' Butikk: '     + STRING(ipiButikkNr)
                               + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                               + ' Ok: '         + STRING(obOk)    
                               ).         
    RETURN.
  END.
END. /* DOBBELSJEKK */

IF AVAILABLE PkSdlHode THEN 
DO:
  /* Lagt inn sjekk på 'Rydd'. */
  SJEKK_BLANK_STREKKODE:
  DO:
      bGyldig = TRUE.
      BLOKK1:
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
          IF PkSdlLinje.Kode = '' THEN 
          DO:
             FIND FIRST Strekkode NO-LOCK 
                  WHERE Strekkode.ArtikkelNr = PkSdlLinje.ArtikkelNr
                    AND Strekkode.StrKode    = PkSdlLinje.StrKode
                    AND NOT Strekkode.Kode   BEGINS "02" 
                  NO-ERROR.
              bGyldig = AVAILABLE StrekKode.
          END.
          ELSE DO:
             FIND FIRST Strekkode NO-LOCK 
                  WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
             IF NOT AVAILABLE(Strekkode) THEN 
                bGyldig = AVAILABLE StrekKode.
          END.
          IF bGyldig = FALSE THEN 
            LEAVE BLOKK1.
      END. /* BLOKK1 */
      IF bGyldig = FALSE THEN 
      DO:
          ASSIGN bbOk     = FALSE
            cReturn = 'Pakkseddelen har varelinje(r) uten strekkoder. Kan ikke innleveres.'.
            RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Pakkseddelen har varelinjer uten strekkoder. Kan ikke innleveres.' 
                                  + ' Butikk: '     + STRING(ipiButikkNr)
                                  + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                                  + ' Ok: '         + STRING(obOk)    
                                  ).         
          RETURN.
      END.
  END. /* SJEKK_BLANK_STREKKODE */

  /* Lagt inn sjekk på feilkoblede strekkoder. */
  SJEKK_FEILKOBLEDE_STREKKODER:
  DO:
      bGyldig = TRUE.
      BLOKK2:
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
          FIND FIRST Strekkode NO-LOCK 
               WHERE Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
               
          IF TRIM(PkSdlLinje.Kode) = '' OR NOT AVAILABLE Strekkode THEN 
              bGyldig = FALSE.
          ELSE bGyldig = IF (Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr OR 
                             Strekkode.StrKode    <> PkSdlLinje.StrKode) THEN FALSE ELSE TRUE.
          IF bGyldig = FALSE THEN 
            LEAVE BLOKK2.
      END. /* BLOKK2 */
      IF bGyldig = FALSE THEN 
      DO:
          ASSIGN bbOk     = FALSE
            cReturn = 'Pakkseddelen har varelinje(r) med feilkoblede strekkoder. Koblet mot feil artikkel eller feil størrelse. Kan ikke innleveres.'.
            RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Pakkseddelen har varelinjer med feilkoblede strekkoder. Kan ikke innleveres.' 
                                  + ' Butikk: '     + STRING(ipiButikkNr)
                                  + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                                  + ' Ok: '         + STRING(obOk)    
                                  ).         
          RETURN.
      END.
  END. /* SJEKK_FEILKOBLEDE_STREKKODER */

  /* TN 21/12-17 Utfør priskontroll på utpris. Gjøres ikke for Outlet butikkene. */
  IF NOT CAN-DO(cOutletLst,STRING(ipiButikkNr)) THEN 
      RUN PkSdlUtPrisKontroll.p (PkSdlHode.PkSdlId).
    
  FIND FIRST ttPkSdlHode WHERE 
    ttPkSdlHode.PkSdlId = PkSdlHode.PkSdlId NO-ERROR.
  IF NOT AVAILABLE ttPkSdlHode THEN 
  DO:
    CREATE ttPkSdlHode.
    ASSIGN
      ttPkSdlHode.PkSdlId    = PkSdlHode.PkSdlId
      ttPkSdlHode.SendtDato  = PkSdlHode.SendtDato
      ttPkSdlHode.PkSdlNr    = PkSdlHode.PkSdlNr
      ttPkSdlHode.EkstId     = PkSdlHode.EkstId
      .
  END.
  ELSE 
    ASSIGN
      ttPkSdlHode.SendtDato  = PkSdlHode.SendtDato
      ttPkSdlHode.PkSdlNr    = PkSdlHode.PkSdlNr
      ttPkSdlHode.EkstId     = PkSdlHode.EkstId.
  
  ASSIGN  
      bbOk = TRUE.
END.
ELSE DO: 
  ASSIGN bbOk     = FALSE
         cReturn = 'Ugyldig pakkseddelnr. ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9")).
         RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Ugyldig pakkseddelnr.' 
                               + ' Butikk: '     + STRING(ipiButikkNr)
                               + ' Pakkseddel: ' + TRIM(STRING(iplAntall,"->>>>>>>>>>>>>>>>9"))
                               + ' Ok: '         + STRING(obOk)    
                               ).         
  RETURN.
END.

IF AVAILABLE ttPkSdlHode THEN
INNLEVER_OG_ETIKETTER: 
DO:
  FIND PkSdlHode NO-LOCK WHERE
    PkSdlHode.PkSdlId = ttPkSdlHode.PkSdlId NO-ERROR.
  IF NOT AVAILABLE PkSdlHode THEN
  DO: 
    LEAVE INNLEVER_OG_ETIKETTER.
  END.
  ELSE DO:
    /* Finner fra butikken hvis det er en overføring. Hvis ikke kommer varene fra lager 20. */
    IF NUM-ENTRIES(PkSdlHode.Merknad,CHR(13)) > 1 AND  ENTRY(2,PkSdlHode.Merknad,CHR(13)) BEGINS 'Overført fra butikk ' AND PkSdlHode.PkSdlOpphav = 4 THEN 
    DO:
        cTekst  = ENTRY(2,PkSdlHode.Merknad,CHR(13)).
        cTekst  = ENTRY(1,cTekst,'.').
        iFraBut = INT(ENTRY(4,cTekst,' ')).
    END.

    FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE  
      PkSdlLinje.AntLevert > 0:
        
      FIND bufArtBas NO-LOCK WHERE
        bufArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
      IF AVAILABLE bufArtBas THEN 
      DO:
        FIND bufButiker NO-LOCK WHERE
          bufButiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.
        FIND bufArtPris NO-LOCK WHERE
          bufArtPris.ArtikkelNr = bufArtBas.ArtikkelNr AND
          bufArtPris.ProfilNr   = bufButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE bufArtPris THEN 
          FIND FIRST bufArtPris OF bufArtBas NO-ERROR.
        FIND PkSdlPris NO-LOCK WHERE
          PkSdlPris.PkSdlId = PkSdlLinje.PkSdlId AND 
          PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.

        /* Butikker som skal ha etiketter når de gjør varemottak via kassen. */
        IF NOT CAN-DO(cIkkeEttikettBatch,STRING(PkSdlLinje.ButikkNr)) THEN
        DO: 
          ASSIGN iSeq = iSeq + 1.
          CREATE EtikettLogg.
          ASSIGN EtikettLogg.Butik     = PkSdlLinje.ButikkNr
                 EtikettLogg.Vg        = bufArtBas.Vg
                 EtikettLogg.LopNr     = (IF bufArtBas.LopNr = ? THEN 0 ELSE BufArtBas.LopNr)
                 EtikettLogg.Ant       = PkSdlLinje.AntLevert
                 EtikettLogg.Storl     = PkSdlLinje.Kode
                 EtikettLogg.Bongtekst = PkSdlLinje.Beskr
                 EtikettLogg.Pris      = PksdlPris.NyPris
                 EtikettLogg.Pris2     = bufArtBas.AnbefaltPris
                 EtikettLogg.SeqNr     = iSeq.         
        END.
      END.
    END.  

    /* Er pakkseddelen ikke innlevert fra før, skal den innleveres. */
    IF PkSdlHode.PkSdlStatus = 10 THEN 
    OPPRETT_TMP:
    DO:
        
      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAILABLE PkSdlLinje AND (CAN-DO(cOutletLst,STRING(PkSdlLinje.ButikkNr)) OR 
                                   CAN-DO(ceComLst,STRING(PkSdlLinje.ButikkNr))) 
                                   AND CAN-DO('4,5,7',STRING(PkSdlHode.PkSdlOpphav)) THEN
      OUTLET_MIKS: 
      DO:

          piLinjeNr = 1.
          FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
              FIND bufArtBas NO-LOCK WHERE
                bufArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
              
              FIND StrKonv NO-LOCK WHERE 
                  StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
              FIND PkSdlPris OF PkSdlHode NO-LOCK WHERE 
                   PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
              /* Logger overføringstransaksjonen */
              CREATE TT_OvBuffer.
              ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
                     TT_OvBuffer.LinjeNr     = piLinjeNr
                     TT_OvBuffer.ArtikkelNr  = PkSdlLinje.ArtikkelNr
                     TT_OvBuffer.Vg          = bufArtBas.Vg   
                     TT_OvBuffer.LopNr       = (IF bufArtBas.LopNr = ? THEN 0 ELSE BufArtBas.LopNr)
                     TT_OvBuffer.Antall      = PkSdlLinje.AntLevert
                     TT_OvBuffer.Merknad     = "Varemottak pakkseddel"
                     TT_OvBuffer.Storl       = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                     TT_OvBuffer.TilStorl    = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
                     TT_OvBuffer.Varekost    = PkSdlPris.NyVarekost
                     piLinjeNr               = piLinjeNr + 1
                     /* Setter datoinfo i registrert dato og tid. */
                     TT_OvBuffer.RegistrertDato = TODAY 
                     TT_OvBuffer.RegistrertTid  = TIME
                     TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
                     TT_OvBuffer.ButikkNrFra = iFrabut
                     TT_OvBuffer.ButikkNrTil = PkSdlLinje.ButikkNr        
                     .
          END.
          
          ASSIGN iBuntNr = -2. /* -2 = En overføringsordre pr. bong. Og de markeres som oppdatert. */
         RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                               0,
                               "N" + CHR(1) + "Varemottak outlet " + STRING(TODAY) + STRING(TIME,"HH:MM") + CHR(1) + "N",
                               '',
                               '',
                               7).
      END. /* OUTLET_MIKS */
      ELSE iBuntNr = 0.
      
      /* Legger pakkseddel linjene opp i en temp-tabell. */
      OPPRETT_TMP:
      FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
        CREATE ttpkSdlLinje.              
        BUFFER-COPY pkSdlLinje TO ttpkSdlLinje.
        cButikkNr = STRING(PkSdlLinje.ButikkNr).
        
        FIND StrKonv NO-LOCK WHERE 
            StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
          
        /* For å kunne opprette faktura. */    
        IF (CAN-DO(cOutletLst,cButikkNr) OR CAN-DO(ceComLst,STRING(PkSdlLinje.ButikkNr))) AND CAN-DO('4,5,7',STRING(PkSdlHode.PkSdlOpphav)) THEN 
        DO:
            CREATE tmpOverfor.
            ASSIGN
              tmpOverfor.ArtikkelNr = DEC(PkSdlLinje.ArtikkelNr)
              tmpOverfor.Vg         = bufArtBas.Vg
              tmpOverfor.LopNr      = bufArtBas.LopNr
              tmpOverfor.FraBut     = iFrabut
              tmpOverfor.TilBut     = PkSdlLinje.ButikkNr
              tmpOverfor.FraStorl   = (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '')
              tmpOverfor.TilStorl   = tmpOverfor.FraStorl 
              tmpOverfor.Antall     = PkSdlLinje.AntLevert
              tmpOverfor.BuntNr     = iBuntNr
              tmpOverfor.OrdreNr    = ''
              tmpOverFor.Rab%       = 0
              tmpOverfor.Kode       = PkSdlLinje.Kode 
               .
         END.        
      END. /* OPPRETT_TMP */

      IF bTest THEN  
            /* TEST */ TEMP-TABLE tmpOverfor:WRITE-JSON("file", "log\tmpOverfor" + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + ".json", TRUE).
      
      /* Henter butikk */
      IF AVAILABLE bufbutiker THEN RELEASE bufButiker. 
      FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
      IF AVAILABLE PkSdlLinje THEN 
          FIND bufButiker NO-LOCK WHERE
              bufButiker.Butik = PkSdlLinje.ButikkNr NO-ERROR.
      
      ihBuffer = BUFFER ttpkSdlLinje:HANDLE.              
      RUN pksdl_opprett_ordre.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).
      
      /* Prisendring skal slå gjensidig på profilene 1 og 16. Prisendringer skal også slå på hele modellen (alle farger) */
      /* Sjekken gjøres FØR varemottake for å kunne sjekke pris før varemottaket endrer denne.                           */
      /* Prisoppdateringene gjøres her. Selv om den gjrøes en gang til ved varemottak under.                             */
      /* Er det bare en vare i modellen, gjøres ingenting her. Da er det varemottaket som oppdaterer pris.               */
      IF iGantAktiv = 1 THEN 
        RUN pksdl_oppd_pris_profiler.p (cLogg, PkSdlHode.PkSdlId).
      
      RUN pksdl_innlever.p (USERID('SkoTex'), ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).

      /* Er pakkseddelen kommet fra sentrallageret, skal lageret trekkes ned på sentrallageret når det gjøres varemottak i outlet. */  
      /* Det utstedes da samtidig faktura for varemottaket.                                                                        */
      IF CAN-DO(cOutletLst,cButikkNr) AND CAN-DO('5',STRING(PkSdlHode.PkSdlOpphav)) THEN 
          RUN pksdl_internsalg.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).

      /* Er pakkseddelen kommet fra nettbutikkens ventelager, skal lageret trekkes ned på sentrallageret når det gjøres varemottak i Nettbutikk. */  
      /* Det skal ikke utstedes faktura utstedes da samtidig faktura for varemottaket.                                                           */
      /* Opphav = 7. Pakksedler flyttet fra outlet til annen butikk før det gjøres varemottak.                                                   */
      ELSE IF CAN-DO('6,7',STRING(PkSdlHode.PkSdlOpphav)) THEN 
          RUN pksdl_internsalg.p ('', ihBuffer,'' ,OUTPUT ocReturn, OUTPUT obOk).
          
      /* Er det overført fra en annen butikk til outlet, skal det bare utstedes faktura. 'Fra butikkens' lager skal da ikke røres her. Det er gjort tidligere. */
      IF (CAN-DO(cOutletLst,cButikkNr) OR CAN-DO(ceComLst,STRING(PkSdlLinje.ButikkNr))) AND CAN-DO('4,5,7',STRING(PkSdlHode.PkSdlOpphav)) THEN 
      DO:
          lFaktura_Id = 0.
          RUN opprettfakturaoverfor.p (OUTPUT iDummy, OUTPUT cTekst).

          RUN bibl_loggDbFri.p (cLogg, '            : Outlett - Fakturegenerering'
                              + ' FakturaNr: ' + STRING(lFaktura_Id)
                              + ' PkSdlButikk: ' + STRING(PkSdlLinje.ButikkNr)
                              + ' BufButikk: ' + STRING(bufButiker.Butik)
                              ).

          /* Skriver ut faktura i butikk som mottar varene. Fikk faktura_id via putFakturaId subscribe. */
          IF lFaktura_Id > 0 AND
            CAN-FIND(FakturaHode NO-LOCK WHERE
                     FakturaHode.Faktura_Id = lFaktura_Id) THEN
          DO:

                  RUN faktura_fakturaskriver.p (STRING(PkSdlLinje.ButikkNr) + "|1|",
                                  ?,
                                  "",
                                  OUTPUT cTekst,
                                  OUTPUT obOk).

                  RUN bibl_loggDbFri.p (cLogg, '            : Outlett - Fakturegenerering'
                                      + ' Skriver: ' + cTekst
                                      ).



                  IF NUM-ENTRIES(cTekst,'|') >= 5 THEN
                  DO:
                      RUN skrivfaktura.p (STRING(lFaktura_Id) + "|",ENTRY(1,cTekst,"|"),ENTRY(2,cTekst,"|"),ENTRY(3,cTekst,"|"),ENTRY(4,cTekst,"|"),ENTRY(5,cTekst,"|")).

                      RUN SendEMail (cFakturafilNavn, PkSdlHode.PkSdlNr, PkSdlLinje.ButikkNr).

                      RUN bibl_loggDbFri.p (cLogg, '            : Outlett - Fakturegenerering'
                                          + ' Faktura skrevet. '
                                          ).
                  END.
          END.
      END.
      /* Skriver ut pakkseddelen i butikken */
/*      ELSE RUN skrivpakkseddel.p (STRING(PkSdlHode.PkSdlId) + "|", TRUE,bufButiker.RapPrinter,'1',"",1).*/

      /* Gjelder Gant.                                                                 */
      /* På varer som innleveres, og som står på kampanje, skal varen tas av kampanje. */
      /* Den skal også slettes fra alle kampanjer i kampanjeregisteret.                */
      IF iGantAktiv = 1 THEN
      AVSLUTT_KAMPANJE:
      DO:
          RUN pksdlAvsluttKampanje.p (PkSdlHode.PkSdlId).          
      END. /* AVSLUTT_KAMPANJE */       
      
      RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Innlevert pakkseddel' 
                          + ' Butikk: '     + STRING(ipiButikkNr)
                          + ' Pakkseddel: ' + STRING(PkSdlHode.PkSdlNr)
                          + ' Ok: '         + STRING(obOk)    
                          ).
      cReturn = 'Varemottak gjennomført for:|' + 
                'Pksdlnr: ' + STRING(PkSdlHode.PkSdlNr).
    END.
    
    IF VALID-HANDLE(ihBuffer) THEN DELETE OBJECT ihBuffer.  
  END.  
END. /* INNLEVER_OG_ETIKETTER */

IF CAN-FIND(FIRST EtikettLogg) AND bSkrivEtikett THEN
DO:
    RUN asEtikett.p (ipiButikkNr,2,INPUT TABLE EtikettLogg,OUTPUT obOK).
    RUN bibl_loggDbFri.p (cLogg, 'asPakkseddel.p: Etikettutskrift pakkseddel' 
                          + ' Butikk: '     + STRING(ipiButikkNr)
                          + ' Pakkseddel: ' + STRING(iplAntall)
                          + ' Ok: '         + STRING(obOk)    
                          ).
    ASSIGN
      cReturn = cReturn + (IF cReturn <> '' THEN '|' ELSE '') + 
                'Etiketter utskrevet for:|' + 
                'Pksdlnr: ' + STRING(PkSdlHode.PkSdlNr).
END.

/* Rydder opp før avsluttning av programmet. */
EMPTY TEMP-TABLE Etikettlogg.     
EMPTY TEMP-TABLE ttPkSdlHode.
EMPTY TEMP-TABLE tmpPkSdlHode.
EMPTY TEMP-TABLE ttpkSdlLinje.
EMPTY TEMP-TABLE TT_OvBuffer.
EMPTY TEMP-TABLE tmpOverfor.

UNSUBSCRIBE TO "getPksdlId".
UNSUBSCRIBE TO "getPksdlNr".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

 
&IF DEFINED(EXCLUDE-FakturafilNavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FakturafilNavn Procedure
PROCEDURE FakturafilNavn:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pcFakturafilNavn AS CHARACTER NO-UNDO.

    ASSIGN 
        cFakturafilNavn = pcFakturafilNavn
        .

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getPkSdlId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPkSdlId Procedure
PROCEDURE getPkSdlId:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER lPkSdlId AS DECIMAL NO-UNDO.

    IF AVAILABLE PkSdlHode THEN 
        lPkSdlId = PkSdlHode.PkSdlId.
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-getPkSdlNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPkSdlNr Procedure
PROCEDURE getPkSdlNr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cPkSdlNr AS CHARACTER NO-UNDO.

    IF AVAILABLE PkSdlHode THEN 
        cPkSdlNr = PkSdlHode.PkSdlNr.
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-Ken1Test) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ken1Test Procedure
PROCEDURE Ken1Test:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

DEFINE VARIABLE dePris     AS DECIMAL                        NO-UNDO.
  DEFINE VARIABLE iKr        AS INTEGER FORMAT ">>>>>9"        NO-UNDO.
  DEFINE VARIABLE iOren      AS INTEGER FORMAT "99"            NO-UNDO.
  DEFINE VARIABLE iRowEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iRow       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cRow       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCol       AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE cCol       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iColEntry  AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iEtikettnr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iCount     AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE iCount2    AS INTEGER                        NO-UNDO.
  DEFINE VARIABLE dPris2     AS DECIMAL  DECIMALS 2  NO-UNDO.
  DEFINE VARIABLE wOK        AS LOGICAL    NO-UNDO.
  DEFINE VARIABLE cFormat AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cColKronor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cColOren   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cInfoEtiTxt AS CHARACTER EXTENT 4 NO-UNDO.
  DEFINE VARIABLE cPrisKr   AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE dPris1 AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE cPrisOren AS CHARACTER  NO-UNDO.
  ASSIGN cColKronor = "200,200,200,185,160"
         cColOren   = "240,275,310,330,340".

DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
/* SESSION:PRINTER-NAME = "DatamaxIP". */
OUTPUT TO PRINTER VALUE("Meto-Mn-4") CONVERT TARGET "IBM850".
iKr = 100.
iOren = 50.
FIND FIRST artbas WHERE opris = FALSE.
FIND farg OF artbas NO-LOCK NO-ERROR.
FOR EACH strekkode OF artbas NO-LOCK.
    FIND strkonv OF strekkode NO-LOCK.
      DO:
              ASSIGN cFormat = CHR(2) + "m" + CHR(2) + "KcLW400" + CHR(2) + "M0800" + CHR(2) + "O0000". /* m=metric */
              PUT CONTROL cFormat.
              cFormat = CHR(2) + "LW400" + CHR(13). /* CR.*/. 
              PUT CONTROL cFormat.
              ASSIGN cPrisKr   = SUBSTITUTE("191100600200&1",ENTRY(LENGTH(STRING(iKr)),cColKronor)) + TRIM(STRING(iKr,">>>>9")) + CHR(13)
                     cPrisOren = SUBSTITUTE("191100400400&1",ENTRY(LENGTH(STRING(iKr)),cColOren)) + TRIM(STRING(iOren,"99")) + CHR(13).
         PUT UNFORMATTED CHR(2) "L" CHR(13) "C0000" CHR(13) "D11" CHR(13)
             "191100202100020" ArtBas.Bongtekst CHR(13)
             "191100201800020" (IF ArtBas.StrTypeId > 2 AND AVAILABLE StrKonv THEN TRIM(StrKonv.Storl) + " " ELSE "") 
                         (IF ArtBas.LevFargKod <> '' THEN ArtBas.LevFargKod ELSE IF AVAILABLE Farg THEN Farg.FarBeskr ELSE '' ) CHR(13)
             "1F2206500900065" TRIM(StrekKode.Kode) CHR(13)
          (IF dPris2 > 0 THEN 
               "191100300300020" + "(" + TRIM(STRING(dPris2,">>>>9.99")) + ")" + CHR(13) ELSE "")
                 cPrisKr  
                 cPrisOren
             (IF TRIM(ArtBas.LevKod) <> "" THEN
                    "121100200000020   L-NR:" + TRIM(ArtBas.LevKod) + CHR(13) ELSE "")
             SUBSTITUTE("Q&1",STRING(2,"9999")) CHR(13) "E" CHR(13).
      END.
      LEAVE.
  END.

  PUT UNFORMATTED CHR(2) "F".
  OUTPUT CLOSE.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-putFakturaId) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putFakturaId Procedure
PROCEDURE putFakturaId:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER plFaktura_Id LIKE FakturaHode.Faktura_Id NO-UNDO.
    
    ASSIGN 
        lFaktura_Id = plFaktura_Id
        .

END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

&IF DEFINED(EXCLUDE-sendEMail) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sendEMail Procedure
PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icFil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pcPkSdlNr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER piButNr AS INTEGER NO-UNDO.

DEFINE BUFFER pbufButiker FOR Butiker.
FIND pbufButiker NO-LOCK WHERE 
    pbufbutiker.butik = piButNr NO-ERROR.

FILE-INFO:FILE-NAME = icFil.

rSendEMail:parToADDRESS = (IF (AVAILABLE pbufButiker AND NUM-ENTRIES(pbufbutiker.ePostAdresse,'@') > 1) 
                             THEN pbufButiker.ePostAdresse 
                             ELSE ''
                           ).
rSendEMail:parMailType = 'PKSDLFaktura'.
rSendEMail:parSUBJECT  = 'Faktura til butikk ' + STRING(piButNr) + ' ' + pbufButiker.butNamn + ' for pakkseddel ' + pcPkSdlNr + ' (Dato/Tid: ' + STRING(NOW,"99/99/9999 HH:MM:SS") + ').'.
rSendEMail:parMESSAGE  = "Fakturafil: " + icFil + '.'.
rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  

RUN bibl_loggDbFri.p (cLogg,'    eMail info:').
RUN bibl_loggDbFri.p (cLogg,'        ' + rSendEMail:parMailType).
RUN bibl_loggDbFri.p (cLogg,'        ' + rSendEMail:parSUBJECT).
RUN bibl_loggDbFri.p (cLogg,'        ' + rSendEMail:parMESSAGE).
RUN bibl_loggDbFri.p (cLogg,'        ' + rSendEMail:parFILE).

obOk = rSendEMail:send( ).

RUN bibl_loggDbFri.p (cLogg,'    eMail sende resultat: ' + STRING(obOk)).
                    
IF ERROR-STATUS:ERROR THEN 
    DO:
        RUN bibl_loggDbFri.p (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
            RUN bibl_loggDbFri.p (cLogg, '          ' 
                + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                ).
        END.            
    END.


END PROCEDURE.
    
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

