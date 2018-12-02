/*------------------------------------------------------------------------
    File        : xhkvpiinnles.p
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
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS DECIMAL FORMAT ">>,>>>,>>>,>>9"  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.

DEF VAR cType         AS CHAR NO-UNDO.
DEF VAR iRecType      AS INT  NO-UNDO. 
DEF VAR cTabellNavn   AS CHAR NO-UNDO. 
DEF VAR cRecVersion   AS CHAR NO-UNDO.
DEF VAR iAntRecord    AS INT  NO-UNDO.
DEF VAR cTblLst       AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR cKontrolltabell AS CHARACTER  NO-UNDO. /* Kontroll av vilken data vi skall testa mot */
DEFINE VARIABLE lHK AS LOG NO-UNDO.

DEF STREAM InnFil.

DEFINE TEMP-TABLE TT_Translogg        NO-UNDO LIKE TransLogg 
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_ArtLag           NO-UNDO LIKE ArtLag 
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Lager            NO-UNDO LIKE Lager
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Avdeling         NO-UNDO LIKE Avdeling
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Akt_Rapp         NO-UNDO LIKE Akt_Rapp
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_hgrdag           NO-UNDO LIKE hgrdag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Timedag          NO-UNDO LIKE timedag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Varedag          NO-UNDO LIKE VareDag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_StLinje          NO-UNDO LIKE StLinje
  FIELD RecType AS INT
    INDEX GURRE StTypeId PerId DataObjekt Diverse Butik Aar PerLinNr.

DEFINE TEMP-TABLE TT_Gruppe           NO-UNDO LIKE Gruppe
  FIELD RecType AS INT.    
DEFINE TEMP-TABLE TT_Kasse            NO-UNDO LIKE Kasse
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Forsalj          NO-UNDO LIKE Forsalj
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Selger           NO-UNDO LIKE Selger
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Dags_Rap         NO-UNDO LIKE Dags_Rap
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Kas_Rap          NO-UNDO LIKE Kas_Rap
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Konto            NO-UNDO LIKE Konto
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Kort_Spes        NO-UNDO LIKE Kort_Spes
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KassererDag      NO-UNDO LIKE KassererDag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KassererBilag    NO-UNDO LIKE KassererBilag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KassererKontanter NO-UNDO LIKE KassererKontanter
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KassererOppgj    NO-UNDO LIKE KassererOppgj
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KassererValuta   NO-UNDO LIKE KassererValuta
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_BokforingsBilag  NO-UNDO LIKE Bokforingsbilag
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .

DEFINE TEMP-TABLE tmpTT_Translogg        NO-UNDO LIKE Translogg
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_ArtLag           NO-UNDO LIKE ArtLag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Lager            NO-UNDO LIKE Lager
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Avdeling         NO-UNDO LIKE Avdeling
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Akt_Rapp         NO-UNDO LIKE Akt_Rapp
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_hgrdag           NO-UNDO LIKE hgrdag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Timedag          NO-UNDO LIKE timedag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Varedag          NO-UNDO LIKE VareDag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_StLinje          NO-UNDO LIKE StLinje
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Gruppe           NO-UNDO LIKE Gruppe
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Kasse            NO-UNDO LIKE Kasse
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Forsalj          NO-UNDO LIKE Forsalj
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Selger           NO-UNDO LIKE Selger
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Dags_Rap         NO-UNDO LIKE Dags_Rap
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Kas_Rap          NO-UNDO LIKE Kas_Rap
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Konto            NO-UNDO LIKE Konto
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Kort_Spes        NO-UNDO LIKE Kort_Spes
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KassererDag      NO-UNDO LIKE KassererDag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KassererBilag    NO-UNDO LIKE KassererBilag
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KassererKontanter NO-UNDO LIKE KassererKontanter
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KassererOppgj    NO-UNDO LIKE KassererOppgj
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KassererValuta   NO-UNDO LIKE KassererValuta
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_BokforingsBilag  NO-UNDO LIKE Bokforingsbilag
  FIELD RecType AS INT.

DEFINE TEMP-TABLE TT_StLager  NO-UNDO LIKE StLager
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_StLager  NO-UNDO LIKE StLager
  FIELD RecType AS INT.

{windows.i}

/* ***************************  Main Block  *************************** */

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
{syspara.i 1 1 25 cKontrolltabell}
IF NOT CAN-DO("1,2",cKontrolltabell) THEN
    ASSIGN cKontrolltabell = "1".

{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) THEN 
  lHk = TRUE.
ELSE
  lHK = FALSE.

RUN LesInnFil.

RETURN.

PROCEDURE "lesTranslogg1.0":

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Translogg
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil      
    tmpTT_{&Tabell}.BatchNr
    tmpTT_{&Tabell}.Butik
    tmpTT_{&Tabell}.TransNr
    tmpTT_{&Tabell}.ForsNr
    tmpTT_{&Tabell}.TTId
    tmpTT_{&Tabell}.TBId
    tmpTT_{&Tabell}.ArtikkelNr
    tmpTT_{&Tabell}.LevNr
    tmpTT_{&Tabell}.RegistrertDato
    tmpTT_{&Tabell}.RegistrertTid
    tmpTT_{&Tabell}.RegistrertAv
    tmpTT_{&Tabell}.BongId
    tmpTT_{&Tabell}.BongLinjeNr
    tmpTT_{&Tabell}.KassaNr
    tmpTT_{&Tabell}.Vg
    tmpTT_{&Tabell}.LopNr
    tmpTT_{&Tabell}.Storl
    tmpTT_{&Tabell}.Antall
    tmpTT_{&Tabell}.Pris
    tmpTT_{&Tabell}.RabKr
    tmpTT_{&Tabell}.Mva
    tmpTT_{&Tabell}.Plukket
    tmpTT_{&Tabell}.Dato
    tmpTT_{&Tabell}.Tid
    tmpTT_{&Tabell}.Postert
    tmpTT_{&Tabell}.PostertDato
    tmpTT_{&Tabell}.PostertTid
    tmpTT_{&Tabell}.BestNr
    tmpTT_{&Tabell}.OvButik
    tmpTT_{&Tabell}.OvTransNr
    tmpTT_{&Tabell}.SeqNr
    tmpTT_{&Tabell}.FeilKode
    tmpTT_{&Tabell}.TilStorl
    tmpTT_{&Tabell}.VVarekost
    tmpTT_{&Tabell}.SattVVareKost
    tmpTT_{&Tabell}.MedlemsNr
    tmpTT_{&Tabell}.KortNr
    tmpTT_{&Tabell}.KortType
    tmpTT_{&Tabell}.KundNr
    tmpTT_{&Tabell}.KalkylePris
    tmpTT_{&Tabell}.ProfilNr
    tmpTT_{&Tabell}.SelgerNr
    tmpTT_{&Tabell}.SubtotalRab
    tmpTT_{&Tabell}.RefTekst
    tmpTT_{&Tabell}.Kode
    tmpTT_{&Tabell}.RefNr
    tmpTT_{&Tabell}.Ordreforslag
    tmpTT_{&Tabell}.LinjeRab
    tmpTT_{&Tabell}.PersonalRab
    tmpTT_{&Tabell}.BongTekst
    tmpTT_{&Tabell}.NegLager
    tmpTT_{&Tabell}.individnr
    tmpTT_{&Tabell}.Mva%
    tmpTT_{&Tabell}.Varekost
    tmpTT_{&Tabell}.KampId
    tmpTT_{&Tabell}.KampEierId
    tmpTT_{&Tabell}.KampTilbId                       
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Butik    = tmpTT_{&Tabell}.Butik AND
        TT_{&Tabell}.TransNr  = tmpTT_{&Tabell}.TransNr AND
        TT_{&Tabell}.SeqNr    = tmpTT_{&Tabell}.SeqNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKonto.
  END. /* LOOPEN */
END PROCEDURE.
 
PROCEDURE oppdTranslogg:

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
               STRING(iAntLinjer) + 
               " av " + 
               STRING(iTotAntLinjer) + 
               ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Butik    = TT_{&Tabell}.Butik  AND
      {&Tabell}.TransNr  = TT_{&Tabell}.TransNr  AND
      {&Tabell}.SeqNr    = TT_{&Tabell}.SeqNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Butik    = TT_{&Tabell}.Butik  AND
      {&Tabell}.TransNr  = TT_{&Tabell}.TransNr  AND
      {&Tabell}.SeqNr    = TT_{&Tabell}.SeqNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
     DEFINE INPUT PARAMETER HWND AS LONG.
     DEFINE INPUT PARAMETER lpOperation AS CHAR.
     DEFINE INPUT PARAMETER lpFile AS CHAR.
     DEFINE INPUT PARAMETER lpParameters AS CHAR.
     DEFINE INPUT PARAMETER lpDirectory AS CHAR.
     DEFINE INPUT PARAMETER nShowCmd AS LONG.
     DEFINE RETURN PARAMETER hInstance AS LONG.
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

IF NOT CAN-FIND(FIRST tt_error) 
  THEN RETURN.

IF AVAILABLE VPIFilHode 
  THEN pcTekst = "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
ELSE pcTekst = "Ukjent/slettet VPI fil i program xpossalginnles.p.".

  IF CAN-FIND(FIRST tt_Error) THEN 
  DO:
      OUTPUT TO VALUE("PosSalgError.Txt").
      PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
          pcTekst SKIP       
          .
        FOR EACH tt_Error:
          PUT UNFORMATTED tt_Error.Tekst SKIP.
        END.
      OUTPUT CLOSE.
      IF SEARCH("PosSalgError.Txt") <> ? THEN
      DO:
        DEF VAR hInstance AS INT.
    
        RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("PosSalgError.Txt"),
                                  "",
                                      1,
                                      OUTPUT hInstance).
    
      END.
  END.
END PROCEDURE.

PROCEDURE lesAkt_rapp1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   akt_rapp
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
    IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_{&Tabell}.dato       
      tmpTT_{&Tabell}.uke_dag    
      tmpTT_{&Tabell}.uke_nr     
      tmpTT_{&Tabell}.mnd        
      tmpTT_{&Tabell}.butik      
      tmpTT_{&Tabell}.kasse      
      tmpTT_{&Tabell}.tid        
      tmpTT_{&Tabell}.tid_txt    
      tmpTT_{&Tabell}.oms_ant    
      tmpTT_{&Tabell}.oms_verd   
      tmpTT_{&Tabell}.ant_kunder 
      tmpTT_{&Tabell}.svk        
      tmpTT_{&Tabell}.ant_ret    
      tmpTT_{&Tabell}.verd_ret   
      tmpTT_{&Tabell}.ant_kvitto
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.

    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Dato   = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.Butik  = tmpTT_{&Tabell}.Butik AND
        TT_{&Tabell}.Kasse  = tmpTT_{&Tabell}.Kasse AND
        TT_{&Tabell}.Tid    = tmpTT_{&Tabell}.Tid
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdAkt_Rapp.
  END. /* LOOPEN */
END PROCEDURE.

&IF DEFINED(EXCLUDE-lesArtlag1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesArtlag1.0 Procedure 
PROCEDURE lesArtlag1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Artlag
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
    IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.vg         
        tmpTT_{&Tabell}.lopnr      
        tmpTT_{&Tabell}.storl      
        tmpTT_{&Tabell}.butik      
        tmpTT_{&Tabell}.lagant     
        tmpTT_{&Tabell}.retant     
        tmpTT_{&Tabell}.Lager      
        tmpTT_{&Tabell}.ArtikkelNr 
        tmpTT_{&Tabell}.AntSolgt   
        tmpTT_{&Tabell}.BrekkAnt   
        tmpTT_{&Tabell}.IntAnt     
        tmpTT_{&Tabell}.ReklAnt    
        tmpTT_{&Tabell}.ReklLAnt   
        tmpTT_{&Tabell}.GjenkjopAnt
        tmpTT_{&Tabell}.RetLAnt    
        tmpTT_{&Tabell}.KjopAnt    
        tmpTT_{&Tabell}.OvAnt      
        tmpTT_{&Tabell}.JustAnt    
        tmpTT_{&Tabell}.JustVerdi  
        tmpTT_{&Tabell}.SvinnAnt   
        tmpTT_{&Tabell}.SvinnVerdi 
        tmpTT_{&Tabell}.NedAnt     
        tmpTT_{&Tabell}.NedVerdi   
        tmpTT_{&Tabell}.AntRab     
        tmpTT_{&Tabell}.StrKode
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Butik      = tmpTT_{&Tabell}.Butik      AND
        TT_{&Tabell}.Artikkelnr = tmpTT_{&Tabell}.Artikkelnr AND
        TT_{&Tabell}.Storl      = tmpTT_{&Tabell}.Storl
        NO-ERROR.
    
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell} NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        DO: 
          IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
          IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
          UNDO LOOPEN. 
        END.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdArtLag.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesBokforingsbilag1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesBokforingsbilag1.0 Procedure 
PROCEDURE lesBokforingsbilag1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Bokforingsbilag
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr      
        tmpTT_{&Tabell}.BokforingsNr  
        tmpTT_{&Tabell}.EDato         
        tmpTT_{&Tabell}.ETid          
        tmpTT_{&Tabell}.BrukerID      
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid 
        tmpTT_{&Tabell}.RegistrertAv  
        tmpTT_{&Tabell}.SendtDato     
        tmpTT_{&Tabell}.SendAv        
        tmpTT_{&Tabell}.SendtTid      
        tmpTT_{&Tabell}.OmsetningsDato
        tmpTT_{&Tabell}.SendtRegnskap 
        tmpTT_{&Tabell}.Aar           
        tmpTT_{&Tabell}.GodkjentDato  
        tmpTT_{&Tabell}.GodkjentTid   
        tmpTT_{&Tabell}.GodkjentAv    
        tmpTT_{&Tabell}.GodkjentFlagg
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr     = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.Aar          = tmpTT_{&Tabell}.Aar AND
        TT_{&Tabell}.BokforingsNr = tmpTT_{&Tabell}.BokforingsNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdBokforingsbilag.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesDags_Rap1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesDags_Rap1.0 Procedure 
PROCEDURE lesDags_Rap1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Dags_Rap
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.butikk    
        tmpTT_{&Tabell}.dato      
        tmpTT_{&Tabell}.mnd       
        tmpTT_{&Tabell}.hg1_oms   
        tmpTT_{&Tabell}.hg2_oms   
        tmpTT_{&Tabell}.hg3_oms   
        tmpTT_{&Tabell}.hg4_oms   
        tmpTT_{&Tabell}.hg5_oms   
        tmpTT_{&Tabell}.hg6_oms   
        tmpTT_{&Tabell}.retur_korr
        tmpTT_{&Tabell}.tb1       
        tmpTT_{&Tabell}.tb2       
        tmpTT_{&Tabell}.tb3       
        tmpTT_{&Tabell}.tb4       
        tmpTT_{&Tabell}.tb5       
        tmpTT_{&Tabell}.tb6       
        tmpTT_{&Tabell}.aar       
        tmpTT_{&Tabell}.MvaVerdi  
        tmpTT_{&Tabell}.hg7_oms   
        tmpTT_{&Tabell}.hg8_oms   
        tmpTT_{&Tabell}.hg9_oms   
        tmpTT_{&Tabell}.hg10_oms  
        tmpTT_{&Tabell}.tb7       
        tmpTT_{&Tabell}.tb8       
        tmpTT_{&Tabell}.tb9       
        tmpTT_{&Tabell}.tb10
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Butik  = tmpTT_{&Tabell}.Butik AND
        TT_{&Tabell}.Dato   = tmpTT_{&Tabell}.Dato
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdDags_Rap.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesDummy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesDummy Procedure 
PROCEDURE lesDummy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop     AS INT NO-UNDO.
  DEF VAR piAntError AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil
      ^
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        piAntError = piAntError + 1.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesForsalj1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesForsalj1.0 Procedure 
PROCEDURE lesForsalj1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Forsalj
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ForsNr         
        tmpTT_{&Tabell}.FoAnstNr       
        tmpTT_{&Tabell}.FoNamn         
        tmpTT_{&Tabell}.FoAdr          
        tmpTT_{&Tabell}.FoPoNr         
        tmpTT_{&Tabell}.FoPadr         
        tmpTT_{&Tabell}.FoTel          
        tmpTT_{&Tabell}.FoPersNr       
        tmpTT_{&Tabell}.LevNr          
        tmpTT_{&Tabell}.EDato          
        tmpTT_{&Tabell}.ETid           
        tmpTT_{&Tabell}.BrukerID       
        tmpTT_{&Tabell}.RegistrertDato 
        tmpTT_{&Tabell}.RegistrertTid  
        tmpTT_{&Tabell}.RegistrertAv   
        tmpTT_{&Tabell}.AnsattNr       
        tmpTT_{&Tabell}.Rabatt         
        tmpTT_{&Tabell}.Prisendring    
        tmpTT_{&Tabell}.Retur          
        tmpTT_{&Tabell}.slettTidligere
        tmpTT_{&Tabell}.SlettBong      
        tmpTT_{&Tabell}.SletteForste   
        tmpTT_{&Tabell}.FodtDato       
        tmpTT_{&Tabell}.navnikasse     
        tmpTT_{&Tabell}.passord        
        tmpTT_{&Tabell}.ForsaljAktiv
        tmpTT_{&Tabell}.ButikkNr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ForsNr = tmpTT_{&Tabell}.ForsNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdForsalj.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesGruppe1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesGruppe1.0 Procedure 
PROCEDURE lesGruppe1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Gruppe
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr      
        tmpTT_{&Tabell}.GruppeNr      
        tmpTT_{&Tabell}.Navn          
        tmpTT_{&Tabell}.EDato         
        tmpTT_{&Tabell}.ETid          
        tmpTT_{&Tabell}.BrukerId      
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid 
        tmpTT_{&Tabell}.RegistrertAv
        NO-ERROR.        
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr  = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.GruppeNr  = tmpTT_{&Tabell}.GruppeNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdGruppe.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesHgrDag1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesHgrDag1.0 Procedure 
PROCEDURE lesHgrDag1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   HgrDag
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.butnr   
        tmpTT_{&Tabell}.hgr     
        tmpTT_{&Tabell}.dato    
        tmpTT_{&Tabell}.kostpris
        tmpTT_{&Tabell}.mvakr   
        tmpTT_{&Tabell}.salgssum
        tmpTT_{&Tabell}.kostkamp
        tmpTT_{&Tabell}.mvakamp 
        tmpTT_{&Tabell}.salgkamp
        tmpTT_{&Tabell}.kostmix 
        tmpTT_{&Tabell}.mvamix  
        tmpTT_{&Tabell}.salgmix 
        tmpTT_{&Tabell}.kostmed 
        tmpTT_{&Tabell}.mvamed  
        tmpTT_{&Tabell}.salgmed 
        tmpTT_{&Tabell}.medrabkr
        tmpTT_{&Tabell}.kunrabkr
        tmpTT_{&Tabell}.perrabkr
        tmpTT_{&Tabell}.genrabkr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButNr  = tmpTT_{&Tabell}.ButNr AND
        TT_{&Tabell}.Hgr    = tmpTT_{&Tabell}.Hgr AND
        TT_{&Tabell}.Dato   = tmpTT_{&Tabell}.Dato
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdHgrDag.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS DECIMAL FORMAT ">>,>>>,>>>,>>9"  NO-UNDO.
  DEF VAR pcLinje   AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEF VAR pbOk      AS LOG  NO-UNDO.
  DEF VAR piAntFeil AS DECIMAL FORMAT ">>,>>>,>>>,>>9"  NO-UNDO.
  DEF VAR pcTekst   AS CHAR NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.

  RUN TellOppLinjer.

  ASSIGN
    cTekst = "Antall linjer i filen " + STRING(iTotAntLinjer) + "." NO-ERROR.
  PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

  FIND LAST VPIFilLinje OF VPIFilHode NO-LOCK NO-ERROR.
  IF AVAILABLE VPIFilLinje THEN
      piLinjeNr = VPIFilLinje.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  ASSIGN
      pbOk        = TRUE
      cType       = ""
      iAntLinjer  = 0
      piAntFeil   = 0
      iRecType    = 0  
      cTabellNavn = ""   
      cRecVersion = ""  
      iAntRecord  = 0  
      cTblLst     = ""
      piLoop      = 0
      .

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    /* Vi er ferdige */
    IF iAntLinjer > iTotAntLinjer THEN
        LEAVE LESERLINJER.

    IF iAntLinjer MODULO 250 = 0 THEN
    DO:
        ASSIGN
          cTekst = "Leser fillinje for tabell (" + cTabellNavn + "). Linje " + STRING(iAntLinjer) + " " + STRING(TRANSACTION).
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
    END.

    /* Hensikten med ERROR-LOOP er at når det kommer en ny header record, skal */
    /* Case setningen starte om og behandle samme linje pånytt.                */
    ERROR-LOOP:
    DO WHILE TRUE:
        /* Leser linje fra filen */
        IMPORT STREAM InnFil
          cType
          cTabellNavn
          iRecType   
          cRecVersion
          iAntRecord 
          NO-ERROR.
        /* Feil på linje */
        IF ERROR-STATUS:ERROR = TRUE OR cType = "" THEN
        DO:
          /*
          ASSIGN
            piAntFeil = piAntFeil + 1
            .
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + "."
            .
          */
          LEAVE LESERLINJER.
        END.

        /* Sjekker at filen er ok */
        IF cType <> "H" THEN
        DO:
          ASSIGN
            pbOk = FALSE
            cTekst = "** Ødelagt fil - eller data for ukjent tabell (" + cTabellNavn + ") i filen. Innlesning avbrytes. ".
          PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
          LEAVE LESERLINJER.
        END.
        /* Rapporterer mottatte fildata */
        ASSIGN
          cTekst = "Linje : " + string(iAntLinjer) + " Tabell: " + cTabellNavn + (IF iRecType = 3
                                               THEN " Slettet"
                                               ELSE " Ny/Endret") + " - antall " + STRING(iAntRecord) + ".".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

        /* Logger hvilke tabeller som er kommet. */
        IF NOT CAN-DO(cTblLst,cTabellNavn) THEN
          cTblLst = cTblLst + (IF cTblLst = "" THEN "" ELSE ",") + cTabellNavn.

        FOR EACH tmpTT_{&Tabell}: DELETE tmpTT_{&Tabell}. END.
        
        /* Bygger temp-Tabeller */
        CASE (cTabellNavn + cRecVersion):
          WHEN "Akt_Rapp1.0"          THEN DO:
              RUN value("lesAkt_Rapp"          + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdAkt_Rapp. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "HgrDag1.0"            THEN  DO:
              RUN value("lesHgrDag"            + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdHgrDag. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Bokforingsbilag1.0"   THEN DO:
              RUN value("lesBokforingsbilag"   + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdBokforingsbilag. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Dags_Rap1.0"          THEN DO:
              RUN value("lesDags_Rap"          + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdDags_Rap. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Forsalj1.0"           THEN DO:
              RUN value("lesForsalj"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdForsalj. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Gruppe1.0"            THEN DO:
              RUN value("lesGruppe"            + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdGruppe. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Kasse1.0"             THEN DO:
              RUN value("lesKasse"             + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKasse. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "KassererBilag1.0"     THEN DO:
              RUN value("lesKassererbilag"     + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKassererbilag. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "KassererKontanter1.0" THEN DO:
              RUN value("lesKassererKontanter" + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKassererKontanter. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "KassererOppgj1.0"     THEN DO:
              RUN value("lesKassererOppgj"     + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKassererOppgj. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "KassererValuta1.0"    THEN DO:
              RUN value("lesKassererValuta"    + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKassererValuta. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Kas_Rap1.0"           THEN DO:
              RUN value("lesKas_Rap"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKas_Rap. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Konto1.0"             THEN DO:
              RUN value("lesKonto"             + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKonto. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Kort_Spes1.0"         THEN DO:
              RUN value("lesKort_Spes"         + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdKort_Spes. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Selger1.0"            THEN DO:
              RUN value("lesSelger"            + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdSelger. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "StLinje1.0"           THEN DO:
              RUN value("lesStLinje"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdStLinje. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Timedag1.0"           THEN DO:
              RUN value("lesTimedag"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdTimedag. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "VareDag1.0"           THEN DO:
              RUN value("lesVareDag"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdVareDag. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "StLager1.0"           THEN DO:
              RUN value("lesStLager"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdStLager. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Lager1.0"             THEN DO:
              RUN value("lesLager"             + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdLager. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "ArtLag1.0"            THEN DO:
              RUN value("lesArtLag"            + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdArtLag. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          WHEN "Translogg1.0"           THEN DO:
              RUN value("lesTranslogg"           + cRecVersion) (iRecType,iAntRecord). 
              RUN oppdTranslogg. 
              IF ERROR-STATUS:ERROR THEN NEXT ERROR-LOOP.
          END.
          OTHERWISE DO:
            ASSIGN
              pbOk   = FALSE
              cTekst = "** Feil versjon tabell " + cTabellnavn + ". Versjon : " + cRecVersion + 
                       " (Antall linjer " + STRING(iAntRecord) + ").".
            PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
            RUN lesDummy (iRecType,iAntRecord).
          END.
        END CASE.

        /* Her skal vi ut av den evige loopen. */
        LEAVE ERROR-LOOP.
    END. /* ERROR-LOOP */
    STATUS DEFAULT "Innlesning ferdig. Lest ant linjer: " + 
                   STRING(iTotAntLinjer) + 
                   ".".

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som innlest og oppdatert. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
      IF pbOk = FALSE THEN
      DO:
        ASSIGN
          cTekst = "** Feil ved oppdatering av fil." 
          .
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
      END.
      ASSIGN
        cTekst = "Fil oppdatert (Antall linjer: " + STRING(iAntLinjer) + ").". 
        .
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
      RELEASE VPIFilHode.
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN DO:
      STATUS DEFAULT "Behandler feilloggen...".
      RUN ErrorLogg.
      STATUS DEFAULT "Behandling av feillogg ferdig...".
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKasse1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKasse1.0 Procedure 
PROCEDURE lesKasse1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Kasse
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr
        tmpTT_{&Tabell}.GruppeNr
        tmpTT_{&Tabell}.EDato
        tmpTT_{&Tabell}.ETid
        tmpTT_{&Tabell}.BrukerId
        tmpTT_{&Tabell}.Navn
        tmpTT_{&Tabell}.KasseNr
        tmpTT_{&Tabell}.LayoutNr
        tmpTT_{&Tabell}.TekstHode[ 1]
        tmpTT_{&Tabell}.TekstHode[ 2]
        tmpTT_{&Tabell}.TekstHode[ 3]
        tmpTT_{&Tabell}.TekstHode[ 4]
        tmpTT_{&Tabell}.TekstHode[ 5]
        tmpTT_{&Tabell}.TekstHode[ 6]
        tmpTT_{&Tabell}.TekstHode[ 7]
        tmpTT_{&Tabell}.TekstHode[ 8]
        tmpTT_{&Tabell}.TekstHode[ 9]
        tmpTT_{&Tabell}.TekstHode[10]
        tmpTT_{&Tabell}.TekstSlutt[ 1]
        tmpTT_{&Tabell}.TekstSlutt[ 2]
        tmpTT_{&Tabell}.TekstSlutt[ 3]
        tmpTT_{&Tabell}.TekstSlutt[ 4]
        tmpTT_{&Tabell}.TekstSlutt[ 5]
        tmpTT_{&Tabell}.TekstSlutt[ 6]
        tmpTT_{&Tabell}.TekstSlutt[ 7]
        tmpTT_{&Tabell}.TekstSlutt[ 8]
        tmpTT_{&Tabell}.TekstSlutt[ 9]
        tmpTT_{&Tabell}.TekstSlutt[10]
        tmpTT_{&Tabell}.TekstHStil[ 1]
        tmpTT_{&Tabell}.TekstHStil[ 2]
        tmpTT_{&Tabell}.TekstHStil[ 3]
        tmpTT_{&Tabell}.TekstHStil[ 4]
        tmpTT_{&Tabell}.TekstHStil[ 5]
        tmpTT_{&Tabell}.TekstHStil[ 6]
        tmpTT_{&Tabell}.TekstHStil[ 7]
        tmpTT_{&Tabell}.TekstHStil[ 8]
        tmpTT_{&Tabell}.TekstHStil[ 9]
        tmpTT_{&Tabell}.TekstHStil[10]
        tmpTT_{&Tabell}.TekstSStil[ 1]
        tmpTT_{&Tabell}.TekstSStil[ 2]
        tmpTT_{&Tabell}.TekstSStil[ 3]
        tmpTT_{&Tabell}.TekstSStil[ 4]
        tmpTT_{&Tabell}.TekstSStil[ 5]
        tmpTT_{&Tabell}.TekstSStil[ 6]
        tmpTT_{&Tabell}.TekstSStil[ 7]
        tmpTT_{&Tabell}.TekstSStil[ 8]
        tmpTT_{&Tabell}.TekstSStil[ 9]
        tmpTT_{&Tabell}.TekstSStil[10]
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid
        tmpTT_{&Tabell}.RegistrertAv
        tmpTT_{&Tabell}.Aktiv
        tmpTT_{&Tabell}.ElJournal[1]
        tmpTT_{&Tabell}.ElJournal[2]
        tmpTT_{&Tabell}.Kvittering[1]
        tmpTT_{&Tabell}.Kvittering[2]
        tmpTT_{&Tabell}.Utskriftskopi[1]
        tmpTT_{&Tabell}.Utskriftskopi[2]
        tmpTT_{&Tabell}.KassererOpgj[1]
        tmpTT_{&Tabell}.KassererOpgj[2]
        tmpTT_{&Tabell}.DagsOpgj[1]
        tmpTT_{&Tabell}.DagsOpgj[2]
        tmpTT_{&Tabell}.ElJournalId
        tmpTT_{&Tabell}.KvitteringId
        tmpTT_{&Tabell}.UtskriftsKopiId
        tmpTT_{&Tabell}.KassererOppgjId
        tmpTT_{&Tabell}.DagsOppgj
        tmpTT_{&Tabell}.ElJournalAktiv
        tmpTT_{&Tabell}.KvitteringAktiv
        tmpTT_{&Tabell}.UtskriftskopiAktiv
        tmpTT_{&Tabell}.KassererOppgjAktiv
        tmpTT_{&Tabell}.DagsOppgjAktiv
        tmpTT_{&Tabell}.ElJournalKatalog
        tmpTT_{&Tabell}.KvitteringKatalog
        tmpTT_{&Tabell}.UtskriftskopiKatalog
        tmpTT_{&Tabell}.KassererOppgjKatalog
        tmpTT_{&Tabell}.DagsOppgjKatalog
        tmpTT_{&Tabell}.ElJournalKonv
        tmpTT_{&Tabell}.KvitteringKonv
        tmpTT_{&Tabell}.UTskriftskopiKonv
        tmpTT_{&Tabell}.KassererOppgjKonv
        tmpTT_{&Tabell}.DagsOppgjKonv
        tmpTT_{&Tabell}.DagsOppgjId
        tmpTT_{&Tabell}.ElJournalOperand
        tmpTT_{&Tabell}.KvitteringOperand
        tmpTT_{&Tabell}.UtskriftsKopiOperand
        tmpTT_{&Tabell}.KassererOppgjOperand
        tmpTT_{&Tabell}.DagsOppgjOperand
        tmpTT_{&Tabell}.ElJournalInnles
        tmpTT_{&Tabell}.KvitteringInnles
        tmpTT_{&Tabell}.UtskriftskopiInnles
        tmpTT_{&Tabell}.KassererOppgjInnles
        tmpTT_{&Tabell}.DagsOppgjInnles
        tmpTT_{&Tabell}.ElJournalBehandle
        tmpTT_{&Tabell}.KvitteringBehandle
        tmpTT_{&Tabell}.UtskriftskopiBehandle
        tmpTT_{&Tabell}.KassererOppgjBehandle
        tmpTT_{&Tabell}.DagsOppgjBehandle
        tmpTT_{&Tabell}.ModellNr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.GruppeNr = tmpTT_{&Tabell}.GruppeNr AND
        TT_{&Tabell}.KasseNr  = tmpTT_{&Tabell}.KasseNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKasse.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKassererBilag1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKassererBilag1.0 Procedure 
PROCEDURE lesKassererBilag1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   KassererBilag
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr
        tmpTT_{&Tabell}.Dato
        tmpTT_{&Tabell}.KassererNr
        tmpTT_{&Tabell}.z_nummer
        tmpTT_{&Tabell}.EDato
        tmpTT_{&Tabell}.ETid
        tmpTT_{&Tabell}.BrukerID
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid
        tmpTT_{&Tabell}.RegistrertAv
        tmpTT_{&Tabell}.BilagsNr
        tmpTT_{&Tabell}.Meknad
        tmpTT_{&Tabell}.Belop
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr   = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.Dato       = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.KassererNr = tmpTT_{&Tabell}.KassererNr AND
        TT_{&Tabell}.z_Nummer   = tmpTT_{&Tabell}.z_Nummer AND
        TT_{&Tabell}.BilagsNr   = tmpTT_{&Tabell}.BilagsNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKassererBilag.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKassererKontanter1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKassererKontanter1.0 Procedure 
PROCEDURE lesKassererKontanter1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   KassererKontanter
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr       
        tmpTT_{&Tabell}.Dato           
        tmpTT_{&Tabell}.KassererNr     
        tmpTT_{&Tabell}.z_nummer       
        tmpTT_{&Tabell}.Belop[ 1]      
        tmpTT_{&Tabell}.Belop[ 2]      
        tmpTT_{&Tabell}.Belop[ 3]      
        tmpTT_{&Tabell}.Belop[ 4]      
        tmpTT_{&Tabell}.Belop[ 5]      
        tmpTT_{&Tabell}.Belop[ 6]      
        tmpTT_{&Tabell}.Belop[ 7]      
        tmpTT_{&Tabell}.Belop[ 8]      
        tmpTT_{&Tabell}.Belop[ 9]      
        tmpTT_{&Tabell}.Belop[10]      
        tmpTT_{&Tabell}.Belop[11]      
        tmpTT_{&Tabell}.Belop[12]      
        tmpTT_{&Tabell}.Belop[13]      
        tmpTT_{&Tabell}.Belop[14]      
        tmpTT_{&Tabell}.Belop[15]      
        tmpTT_{&Tabell}.EDato          
        tmpTT_{&Tabell}.ETid           
        tmpTT_{&Tabell}.BrukerID       
        tmpTT_{&Tabell}.RegistrertDato 
        tmpTT_{&Tabell}.RegistrertTid  
        tmpTT_{&Tabell}.RegistrertAv   
        tmpTT_{&Tabell}.AntallValor[ 1]
        tmpTT_{&Tabell}.AntallValor[ 2]
        tmpTT_{&Tabell}.AntallValor[ 3]
        tmpTT_{&Tabell}.AntallValor[ 4]
        tmpTT_{&Tabell}.AntallValor[ 5]
        tmpTT_{&Tabell}.AntallValor[ 6]
        tmpTT_{&Tabell}.AntallValor[ 7]
        tmpTT_{&Tabell}.AntallValor[ 8]
        tmpTT_{&Tabell}.AntallValor[ 9]
        tmpTT_{&Tabell}.AntallValor[10]
        tmpTT_{&Tabell}.AntallValor[11]
        tmpTT_{&Tabell}.AntallValor[12]
        tmpTT_{&Tabell}.AntallValor[13]
        tmpTT_{&Tabell}.AntallValor[14]
        tmpTT_{&Tabell}.AntallValor[15]
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr   = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.Dato       = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.KassererNr = tmpTT_{&Tabell}.KassererNr AND
        TT_{&Tabell}.z_Nummer   = tmpTT_{&Tabell}.z_Nummer
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKassererKontanter.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKassererOppgj1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKassererOppgj1.0 Procedure 
PROCEDURE lesKassererOppgj1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   KassererOppgj
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr
        tmpTT_{&Tabell}.Dato
        tmpTT_{&Tabell}.KassererNr
        tmpTT_{&Tabell}.z_nummer
        tmpTT_{&Tabell}.OpptaltVeksel
        tmpTT_{&Tabell}.OpptaltKontanter
        tmpTT_{&Tabell}.OpptaltSjekk
        tmpTT_{&Tabell}.OpptaltReserve
        tmpTT_{&Tabell}.OpptaltGavekort
        tmpTT_{&Tabell}.OpptaltTilgode
        tmpTT_{&Tabell}.EDato
        tmpTT_{&Tabell}.ETid
        tmpTT_{&Tabell}.BrukerID
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid
        tmpTT_{&Tabell}.RegistrertAv
        tmpTT_{&Tabell}.OpptaltGavekortAndre
        tmpTT_{&Tabell}.OpptaltGavekortUtlevert
        tmpTT_{&Tabell}.OpptaltTilgodeAndre
        tmpTT_{&Tabell}.OpptaltTilgodeUtlevert
        tmpTT_{&Tabell}.OpptaltInnVeksel
        tmpTT_{&Tabell}.OpptaltValuta
        tmpTT_{&Tabell}.OpptaltLevertBank
        tmpTT_{&Tabell}.OpptaltBilag
        tmpTT_{&Tabell}.PoseNr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr   = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.Dato       = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.KassererNr = tmpTT_{&Tabell}.KassererNr AND
        TT_{&Tabell}.z_Nummer   = tmpTT_{&Tabell}.z_Nummer
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKassererOppgj.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKassererValuta1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKassererValuta1.0 Procedure 
PROCEDURE lesKassererValuta1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   KassererValuta
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.ButikkNr
        tmpTT_{&Tabell}.KasseNr
        tmpTT_{&Tabell}.Dato
        tmpTT_{&Tabell}.KassererNr
        tmpTT_{&Tabell}.z_nummer
        tmpTT_{&Tabell}.ValKod
        tmpTT_{&Tabell}.KasseValKurs
        tmpTT_{&Tabell}.Valuta
        tmpTT_{&Tabell}.Belop
        tmpTT_{&Tabell}.EDato
        tmpTT_{&Tabell}.ETid
        tmpTT_{&Tabell}.BrukerID
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid
        tmpTT_{&Tabell}.RegistrertAv
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButikkNr   = tmpTT_{&Tabell}.ButikkNr AND
        TT_{&Tabell}.KasseNr    = tmpTT_{&Tabell}.KasseNr AND
        TT_{&Tabell}.Dato       = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.KassererNr = tmpTT_{&Tabell}.KassererNr AND
        TT_{&Tabell}.z_Nummer   = tmpTT_{&Tabell}.z_Nummer AND
        TT_{&Tabell}.Valkod     = tmpTT_{&Tabell}.Valkod
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKassererValuta.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKas_rap1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKas_rap1.0 Procedure 
PROCEDURE lesKas_rap1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Kas_Rap
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.dato             
        tmpTT_{&Tabell}.butikk           
        tmpTT_{&Tabell}.kasse            
        tmpTT_{&Tabell}.z_nummer         
        tmpTT_{&Tabell}.kontant          
        tmpTT_{&Tabell}.sjekk            
        tmpTT_{&Tabell}.kort             
        tmpTT_{&Tabell}.kredit           
        tmpTT_{&Tabell}.kupong1          
        tmpTT_{&Tabell}.kupong2          
        tmpTT_{&Tabell}.tilgode          
        tmpTT_{&Tabell}.layaway_inn      
        tmpTT_{&Tabell}.layaway_ut       
        tmpTT_{&Tabell}.kont_inn         
        tmpTT_{&Tabell}.kont_ut          
        tmpTT_{&Tabell}.Gavekort         
        tmpTT_{&Tabell}.Rekvisisasjon    
        tmpTT_{&Tabell}.Pant             
        tmpTT_{&Tabell}.Bank             
        tmpTT_{&Tabell}.Dropp            
        tmpTT_{&Tabell}.Overfort         
        tmpTT_{&Tabell}.CashBack         
        tmpTT_{&Tabell}.Veksel           
        tmpTT_{&Tabell}.Avrunding        
        tmpTT_{&Tabell}.Reklamasjon      
        tmpTT_{&Tabell}.Retur            
        tmpTT_{&Tabell}.InnbetaltKunde   
        tmpTT_{&Tabell}.Medlemssalg      
        tmpTT_{&Tabell}.AntCashBack      
        tmpTT_{&Tabell}.AntMedlemssalg   
        tmpTT_{&Tabell}.AntInnbetaltKunde
        tmpTT_{&Tabell}.AntRetur         
        tmpTT_{&Tabell}.AntKontant       
        tmpTT_{&Tabell}.AntSjekk         
        tmpTT_{&Tabell}.AntKort          
        tmpTT_{&Tabell}.AntKredit        
        tmpTT_{&Tabell}.AntKupong1       
        tmpTT_{&Tabell}.AntKupong2       
        tmpTT_{&Tabell}.AntTilgode       
        tmpTT_{&Tabell}.AntBank          
        tmpTT_{&Tabell}.AntGavekort      
        tmpTT_{&Tabell}.AntRekvisisjon   
        tmpTT_{&Tabell}.AntVeksel        
        tmpTT_{&Tabell}.AntAvrunding     
        tmpTT_{&Tabell}.AntDropp         
        tmpTT_{&Tabell}.AntOverfort      
        tmpTT_{&Tabell}.AntKont_Inn      
        tmpTT_{&Tabell}.AntKont_Ut       
        tmpTT_{&Tabell}.AntLayAway_Inn   
        tmpTT_{&Tabell}.AntLayAway_Ut    
        tmpTT_{&Tabell}.AntReturer       
        tmpTT_{&Tabell}.TilgodeInn       
        tmpTT_{&Tabell}.TilgodeUt        
        tmpTT_{&Tabell}.AntTilgodeInn    
        tmpTT_{&Tabell}.AntTilgodeUt     
        tmpTT_{&Tabell}.GavekortUt       
        tmpTT_{&Tabell}.GavekortInn      
        tmpTT_{&Tabell}.AntGavekortUt    
        tmpTT_{&Tabell}.AntGavekortInn   
        tmpTT_{&Tabell}.Medlemsrabatt
        tmpTT_{&Tabell}.GavekortAndreInn    
        tmpTT_{&Tabell}.AntGavekortAndreInn 
        tmpTT_{&Tabell}.GavekortRabatt      
        tmpTT_{&Tabell}.AntGavekortRabUt    
        tmpTT_{&Tabell}.Kunderabatt  /* Lagt til felt 7/2-05 TN */       
        tmpTT_{&Tabell}.Personalrabatt      
        tmpTT_{&Tabell}.GenerellRabatt      
        tmpTT_{&Tabell}.AntPersonalrabatt   
        tmpTT_{&Tabell}.AntMedlemsrabatt    
        tmpTT_{&Tabell}.AntKunderabatt      
        tmpTT_{&Tabell}.AntGenerellRabatt   
        tmpTT_{&Tabell}.OverfortInn         
        tmpTT_{&Tabell}.OverfortUt          
        tmpTT_{&Tabell}.AntOverfortInn      
        tmpTT_{&Tabell}.AntOverfortUt       
        tmpTT_{&Tabell}.MvaGrp[ 1]          
        tmpTT_{&Tabell}.MvaGrp[ 2]          
        tmpTT_{&Tabell}.MvaGrp[ 3]          
        tmpTT_{&Tabell}.MvaGrp[ 4]          
        tmpTT_{&Tabell}.MvaGrp[ 5]          
        tmpTT_{&Tabell}.MvaGrp[ 6]          
        tmpTT_{&Tabell}.MvaGrp[ 7]          
        tmpTT_{&Tabell}.MvaGrp[ 8]          
        tmpTT_{&Tabell}.MvaGrp[ 9]          
        tmpTT_{&Tabell}.MvaGrp[10]          
        tmpTT_{&Tabell}.MvaGrunnlag[ 1]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 2]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 3]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 4]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 5]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 6]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 7]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 8]     
        tmpTT_{&Tabell}.MvaGrunnlag[ 9]     
        tmpTT_{&Tabell}.MvaGrunnlag[10]     
        tmpTT_{&Tabell}.MvaBelop[ 1]        
        tmpTT_{&Tabell}.MvaBelop[ 2]        
        tmpTT_{&Tabell}.MvaBelop[ 3]        
        tmpTT_{&Tabell}.MvaBelop[ 4]        
        tmpTT_{&Tabell}.MvaBelop[ 5]        
        tmpTT_{&Tabell}.MvaBelop[ 6]        
        tmpTT_{&Tabell}.MvaBelop[ 7]        
        tmpTT_{&Tabell}.MvaBelop[ 8]        
        tmpTT_{&Tabell}.MvaBelop[ 9]        
        tmpTT_{&Tabell}.MvaBelop[10]        
        tmpTT_{&Tabell}.AntReklamasjoner    
        tmpTT_{&Tabell}.Vekselbeholdning    
        tmpTT_{&Tabell}.Kontantbeholdning   
        tmpTT_{&Tabell}.Sjekkbeholdning     
        tmpTT_{&Tabell}.Lagerjustering      
        tmpTT_{&Tabell}.Varemottak          
        tmpTT_{&Tabell}.AntLagerjustering   
        tmpTT_{&Tabell}.AntVaremottak       
        tmpTT_{&Tabell}.Brekkasje           
        tmpTT_{&Tabell}.InterntForbruk      
        tmpTT_{&Tabell}.AntBrekkasje        
        tmpTT_{&Tabell}.AntInterntForbruk   
        tmpTT_{&Tabell}.Reservelosning      
        tmpTT_{&Tabell}.AntReservelosning   
        tmpTT_{&Tabell}.AntPakkerabatt      
        tmpTT_{&Tabell}.Pakkerabatt         
        tmpTT_{&Tabell}.TilgodeAndre        
        tmpTT_{&Tabell}.GavekortAndreInn    
        tmpTT_{&Tabell}.AntGavekortAndreInn 
        tmpTT_{&Tabell}.GavekortRabatt      
        tmpTT_{&Tabell}.AntGavekortRabUt    
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Dato       = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.Butikk     = tmpTT_{&Tabell}.Butikk AND
        TT_{&Tabell}.Kasse      = tmpTT_{&Tabell}.Kasse AND
        TT_{&Tabell}.z_Nummer   = tmpTT_{&Tabell}.z_Nummer
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKas_Rap.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKonto1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKonto1.0 Procedure 
PROCEDURE lesKonto1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Konto
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.butikk     
        tmpTT_{&Tabell}.dato       
        tmpTT_{&Tabell}.kontonummer
        tmpTT_{&Tabell}.vg         
        tmpTT_{&Tabell}.lopnr      
        tmpTT_{&Tabell}.storl      
        tmpTT_{&Tabell}.pris       
        tmpTT_{&Tabell}.antall     
        tmpTT_{&Tabell}.kvitto     
        tmpTT_{&Tabell}.kasse      
        tmpTT_{&Tabell}.forsnr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Dato        = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.Butikk      = tmpTT_{&Tabell}.Butikk AND
        TT_{&Tabell}.KontoNummer = tmpTT_{&Tabell}.KontoNummer
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKonto.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKort_Spes1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKort_Spes1.0 Procedure 
PROCEDURE lesKort_Spes1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Kort_Spes
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.dato           
        tmpTT_{&Tabell}.butikk         
        tmpTT_{&Tabell}.kasse          
        tmpTT_{&Tabell}.z_nummer       
        tmpTT_{&Tabell}.AntKort        
        tmpTT_{&Tabell}.KortType       
        tmpTT_{&Tabell}.Belop          
        tmpTT_{&Tabell}.EDato          
        tmpTT_{&Tabell}.ETid           
        tmpTT_{&Tabell}.BrukerID       
        tmpTT_{&Tabell}.RegistrertDato 
        tmpTT_{&Tabell}.RegistrertTid  
        tmpTT_{&Tabell}.RegistrertAv
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Dato       = tmpTT_{&Tabell}.Dato AND
        TT_{&Tabell}.Butikk     = tmpTT_{&Tabell}.Butikk AND
        TT_{&Tabell}.Kasse      = tmpTT_{&Tabell}.Kasse AND
        TT_{&Tabell}.Z_Nummer   = tmpTT_{&Tabell}.Z_Nummer AND
        TT_{&Tabell}.KortType   = tmpTT_{&Tabell}.KortType
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdKort_Spes.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesLager1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesLager1.0 Procedure 
PROCEDURE lesLager1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Lager
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.EDato         
        tmpTT_{&Tabell}.ETid          
        tmpTT_{&Tabell}.BrukerID      
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid 
        tmpTT_{&Tabell}.RegistrertAv  
        tmpTT_{&Tabell}.ArtikkelNr    
        tmpTT_{&Tabell}.VVarekost     
        tmpTT_{&Tabell}.LagAnt        
        tmpTT_{&Tabell}.SistInnlevert 
        tmpTT_{&Tabell}.Butik         
        tmpTT_{&Tabell}.AntSolgt      
        tmpTT_{&Tabell}.BrekkAnt      
        tmpTT_{&Tabell}.IntAnt        
        tmpTT_{&Tabell}.ReklAnt       
        tmpTT_{&Tabell}.ReklLAnt      
        tmpTT_{&Tabell}.GjenkjopAnt   
        tmpTT_{&Tabell}.RetLAnt       
        tmpTT_{&Tabell}.KjopAnt       
        tmpTT_{&Tabell}.OvAnt         
        tmpTT_{&Tabell}.JustAnt       
        tmpTT_{&Tabell}.JustVerdi     
        tmpTT_{&Tabell}.SvinnAnt      
        tmpTT_{&Tabell}.SvinnVerdi    
        tmpTT_{&Tabell}.NedAnt        
        tmpTT_{&Tabell}.NedVerdi      
        tmpTT_{&Tabell}.VerdiSolgt    
        tmpTT_{&Tabell}.KjopVerdi     
        tmpTT_{&Tabell}.BrekkVerdi    
        tmpTT_{&Tabell}.IntVerdi      
        tmpTT_{&Tabell}.ReklVerdi     
        tmpTT_{&Tabell}.ReklLVerdi    
        tmpTT_{&Tabell}.GjenkjopVerdi 
        tmpTT_{&Tabell}.OvVerdi       
        tmpTT_{&Tabell}.VerdiRabatt   
        tmpTT_{&Tabell}.AntRab
        tmpTT_{&Tabell}.Svk
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
        DO: 
          UNDO LOOPEN. 
        END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.Butik      = tmpTT_{&Tabell}.Butik AND
        TT_{&Tabell}.ArtikkelNr = tmpTT_{&Tabell}.ArtikkelNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdLager.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesSelger1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesSelger1.0 Procedure 
PROCEDURE lesSelger1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Selger
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.SelgerNr      
        tmpTT_{&Tabell}.Navn           
        tmpTT_{&Tabell}.EDato          
        tmpTT_{&Tabell}.ETid           
        tmpTT_{&Tabell}.BrukerID       
        tmpTT_{&Tabell}.RegistrertDato 
        tmpTT_{&Tabell}.RegistrertTid  
        tmpTT_{&Tabell}.RegistrertAv   
        tmpTT_{&Tabell}.AnsattNr       
        tmpTT_{&Tabell}.Adresse1       
        tmpTT_{&Tabell}.Telefon        
        tmpTT_{&Tabell}.PersonNr       
        tmpTT_{&Tabell}.Mobiltelefon   
        tmpTT_{&Tabell}.PostNr         
        tmpTT_{&Tabell}.Adresse2       
        tmpTT_{&Tabell}.NavnIKasse
        tmpTT_{&Tabell}.ButikkNr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.SelgerNr = tmpTT_{&Tabell}.SelgerNr
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdSelger.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesStLager1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesStLager1.0 Procedure 
PROCEDURE lesStLager1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   StLager
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.EDato         
        tmpTT_{&Tabell}.ETid          
        tmpTT_{&Tabell}.BrukerID      
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid 
        tmpTT_{&Tabell}.RegistrertAv  
        tmpTT_{&Tabell}.VVarekost      
        tmpTT_{&Tabell}.LagAnt         
        tmpTT_{&Tabell}.SistInnlevert  
        tmpTT_{&Tabell}.Butik          
        tmpTT_{&Tabell}.AntSolgt       
        tmpTT_{&Tabell}.BrekkAnt       
        tmpTT_{&Tabell}.IntAnt         
        tmpTT_{&Tabell}.ReklAnt        
        tmpTT_{&Tabell}.ReklLAnt       
        tmpTT_{&Tabell}.GjenkjopAnt    
        tmpTT_{&Tabell}.RetLAnt        
        tmpTT_{&Tabell}.KjopAnt        
        tmpTT_{&Tabell}.OvAnt          
        tmpTT_{&Tabell}.JustAnt        
        tmpTT_{&Tabell}.JustVerdi      
        tmpTT_{&Tabell}.SvinnAnt       
        tmpTT_{&Tabell}.SvinnVerdi     
        tmpTT_{&Tabell}.NedAnt         
        tmpTT_{&Tabell}.NedVerdi       
        tmpTT_{&Tabell}.VerdiSolgt     
        tmpTT_{&Tabell}.KjopVerdi      
        tmpTT_{&Tabell}.BrekkVerdi     
        tmpTT_{&Tabell}.IntVerdi       
        tmpTT_{&Tabell}.ReklVerdi      
        tmpTT_{&Tabell}.ReklLVerdi     
        tmpTT_{&Tabell}.GjenkjopVerdi  
        tmpTT_{&Tabell}.OvVerdi        
        tmpTT_{&Tabell}.VerdiRabatt    
        tmpTT_{&Tabell}.AntRab         
        tmpTT_{&Tabell}.StTypeId       
        tmpTT_{&Tabell}.DataObjekt     
        tmpTT_{&Tabell}.vSnittKostPris 
        tmpTT_{&Tabell}.Svk
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.StTypeId   = tmpTT_{&Tabell}.StTypeId AND
        TT_{&Tabell}.Butik      = tmpTT_{&Tabell}.Butik AND
        TT_{&Tabell}.DataObjekt = tmpTT_{&Tabell}.DataObjekt
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdStLager.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesStLinje1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesStLinje1.0 Procedure 
PROCEDURE lesStLinje1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   StLinje
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.EDato         
        tmpTT_{&Tabell}.ETid          
        tmpTT_{&Tabell}.BrukerID      
        tmpTT_{&Tabell}.RegistrertDato
        tmpTT_{&Tabell}.RegistrertTid 
        tmpTT_{&Tabell}.RegistrertAv  
        tmpTT_{&Tabell}.VVarekost     
        tmpTT_{&Tabell}.Butik         
        tmpTT_{&Tabell}.AntSolgt      
        tmpTT_{&Tabell}.BrekkAnt      
        tmpTT_{&Tabell}.IntAnt        
        tmpTT_{&Tabell}.ReklAnt       
        tmpTT_{&Tabell}.ReklLAnt      
        tmpTT_{&Tabell}.GjenkjopAnt   
        tmpTT_{&Tabell}.KjopAnt       
        tmpTT_{&Tabell}.OvAnt         
        tmpTT_{&Tabell}.JustAnt       
        tmpTT_{&Tabell}.JustVerdi     
        tmpTT_{&Tabell}.SvinnAnt      
        tmpTT_{&Tabell}.SvinnVerdi
        tmpTT_{&Tabell}.NedAnt        
        tmpTT_{&Tabell}.NedVerdi      
        tmpTT_{&Tabell}.VerdiSolgt    
        tmpTT_{&Tabell}.KjopVerdi     
        tmpTT_{&Tabell}.BrekkVerdi    
        tmpTT_{&Tabell}.IntVerdi      
        tmpTT_{&Tabell}.ReklVerdi     
        tmpTT_{&Tabell}.ReklLVerdi    
        tmpTT_{&Tabell}.GjenkjopVerdi 
        tmpTT_{&Tabell}.OvVerdi       
        tmpTT_{&Tabell}.DataObjekt    
        tmpTT_{&Tabell}.StTypeId      
        tmpTT_{&Tabell}.Beskrivelse   
        tmpTT_{&Tabell}.PerId         
        tmpTT_{&Tabell}.Aar           
        tmpTT_{&Tabell}.PerLinNr      
        tmpTT_{&Tabell}.MvaVerdi      
        tmpTT_{&Tabell}.Diverse       
        tmpTT_{&Tabell}.AntTilbSolgt  
        tmpTT_{&Tabell}.VerdiTilbSolgt
        tmpTT_{&Tabell}.TilbVVarekost 
        tmpTT_{&Tabell}.TilbMvaVerdi  
        tmpTT_{&Tabell}.AntRabatt     
        tmpTT_{&Tabell}.VerdiRabatt   
        tmpTT_{&Tabell}.LagerAnt      
        tmpTT_{&Tabell}.PrimoAnt      
        tmpTT_{&Tabell}.OmlHast       
        tmpTT_{&Tabell}.Hg            
        tmpTT_{&Tabell}.VisBut        
        tmpTT_{&Tabell}.PerLinTxt     
        tmpTT_{&Tabell}.DbKr          
        tmpTT_{&Tabell}.Db%           
        tmpTT_{&Tabell}.Utsolgt%      
        tmpTT_{&Tabell}.LagerVerdi    
        tmpTT_{&Tabell}.Primoverdi    
        tmpTT_{&Tabell}.DiverseAnt    
        tmpTT_{&Tabell}.Diverseverdi  
        tmpTT_{&Tabell}.TotalPost
        tmpTT_{&Tabell}.artVg
        tmpTT_{&Tabell}.artSasong
        tmpTT_{&Tabell}.artFarg
        tmpTT_{&Tabell}.artMatKod
        tmpTT_{&Tabell}.art_Beskr
        tmpTT_{&Tabell}.artLevNr
        tmpTT_{&Tabell}.artLevKod
        tmpTT_{&Tabell}.artVmId
        tmpTT_{&Tabell}.artLevFargKod
        tmpTT_{&Tabell}.artProdNr
        NO-ERROR.        
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    /*
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.StTypeId   = tmpTT_{&Tabell}.StTypeId AND
        TT_{&Tabell}.PerId      = tmpTT_{&Tabell}.PerId AND
        TT_{&Tabell}.DataObjekt = tmpTT_{&Tabell}.DataObjekt AND
        TT_{&Tabell}.Diverse    = tmpTT_{&Tabell}.Diverse AND
        TT_{&Tabell}.Butik      = tmpTT_{&Tabell}.Butik AND
        TT_{&Tabell}.Aar        = tmpTT_{&Tabell}.Aar AND
        TT_{&Tabell}.PerLinNr   = tmpTT_{&Tabell}.PerLinNr
        NO-ERROR.
    IF NOT AVAILABLE TT_{&Tabell} THEN
    */
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.

    IF iAntLinjer MODULO 100 = 0 THEN
      RUN oppdStLinje.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesTimedag1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesTimedag1.0 Procedure 
PROCEDURE lesTimedag1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   Timedag
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButNr   = tmpTT_{&Tabell}.ButNr AND
        TT_{&Tabell}.Dato    = tmpTT_{&Tabell}.Dato
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdTimedag.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVareDag1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVareDag1.0 Procedure 
PROCEDURE lesVareDag1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  &SCOPED-DEFINE Tabell   VareDag
  
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
      IF AVAILABLE tmpTT_{&Tabell} THEN DELETE tmpTT_{&Tabell}.
    CREATE tmpTT_{&Tabell}.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_{&Tabell}.RecType = piRecType
      .
    IMPORT STREAM InnFil 
        tmpTT_{&Tabell}.butnr   
        tmpTT_{&Tabell}.ean     
        tmpTT_{&Tabell}.dato    
        tmpTT_{&Tabell}.antall  
        tmpTT_{&Tabell}.kostpris
        tmpTT_{&Tabell}.mvakr   
        tmpTT_{&Tabell}.salgssum
        tmpTT_{&Tabell}.antkamp 
        tmpTT_{&Tabell}.kostkamp
        tmpTT_{&Tabell}.mvakamp 
        tmpTT_{&Tabell}.salgkamp
        tmpTT_{&Tabell}.antmix  
        tmpTT_{&Tabell}.kostmix 
        tmpTT_{&Tabell}.mvamix  
        tmpTT_{&Tabell}.salgmix 
        tmpTT_{&Tabell}.antmed  
        tmpTT_{&Tabell}.kostmed 
        tmpTT_{&Tabell}.mvamed  
        tmpTT_{&Tabell}.salgmed 
        tmpTT_{&Tabell}.medrabkr
        tmpTT_{&Tabell}.kunrabkr
        tmpTT_{&Tabell}.perrabkr
        tmpTT_{&Tabell}.genrabkr
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
          DO: 
            UNDO LOOPEN. 
          END.
    FIND TT_{&Tabell} WHERE
        TT_{&Tabell}.ButNr   = tmpTT_{&Tabell}.ButNr AND
        TT_{&Tabell}.Ean     = tmpTT_{&Tabell}.Ean AND
        TT_{&Tabell}.Dato    = tmpTT_{&Tabell}.Dato
        NO-ERROR.
    IF AVAILABLE TT_{&Tabell} THEN RELEASE TT_{&Tabell}.
    BUFFER-COPY tmpTT_{&Tabell} TO TT_{&Tabell}.

    RELEASE TT_{&Tabell}.
    DELETE tmpTT_{&Tabell}.


    IF iAntLinjer MODULO 100 = 0 THEN RUN oppdVareDag.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdAkt_rapp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdAkt_rapp Procedure 
PROCEDURE oppdAkt_rapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Akt_Rapp

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Dato  = TT_{&Tabell}.Dato  AND
      {&Tabell}.Butik = TT_{&Tabell}.Butik AND
      {&Tabell}.Kasse = TT_{&Tabell}.Kasse AND
      {&Tabell}.Tid   = TT_{&Tabell}.Tid  
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Dato  = TT_{&Tabell}.Dato  AND
      {&Tabell}.Butik = TT_{&Tabell}.Butik AND
      {&Tabell}.Kasse = TT_{&Tabell}.Kasse AND
      {&Tabell}.Tid   = TT_{&Tabell}.Tid  
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdArtLag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdArtLag Procedure 
PROCEDURE oppdArtLag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   ArtLag

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ArtikkelNr = TT_{&Tabell}.artikkelNr AND
      {&Tabell}.Storl      = TT_{&Tabell}.Storl  AND
      {&Tabell}.Butik      = TT_{&Tabell}.Butik
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    IF TT_{&Tabell}.Butik > 0 AND 
       TT_{&Tabell}.Vg > 0 THEN 
    DO:
      FIND {&Tabell} EXCLUSIVE-LOCK WHERE
          {&Tabell}.ArtikkelNr = TT_{&Tabell}.artikkelNr AND
          {&Tabell}.Storl      = TT_{&Tabell}.Storl  AND
          {&Tabell}.Butik      = TT_{&Tabell}.Butik
        NO-ERROR.
      /* Dobbelsjekk pga to unike indekser. */
      IF NOT AVAILABLE {&Tabell} THEN
          FIND {&Tabell} EXCLUSIVE-LOCK WHERE
              {&Tabell}.Butik      = TT_{&Tabell}.Butik AND
              {&Tabell}.Vg         = TT_{&Tabell}.Vg AND
              {&Tabell}.LopNr      = TT_{&Tabell}.LopNr AND
              {&Tabell}.Storl      = TT_{&Tabell}.Storl
              NO-ERROR.
      /* Oppdateres bare hvis artikkel finnes */
      IF CAN-FIND(ArtBas WHERE
                  ArtBas.ArtikkelNr = TT_{&Tabell}.ArtikkelNr) OR 
         CAN-FIND(ArtBas WHERE
                  ArtBas.Vg    = TT_{&Tabell}.Vg AND
                  ArtBas.LopNr = TT_{&Tabell}.LopNr) THEN
      DO:
          IF NOT AVAILABLE {&Tabell} THEN
            CREATE {&Tabell}.
          BUFFER-COPY TT_{&Tabell} TO {&Tabell} NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
            DO:
              IF AVAILABLE {&Tabell} THEN DELETE {&Tabell}.
            END. 
          ELSE DO:
            IF AVAILABLE {&Tabell} THEN RELEASE {&Tabell}.
          END.
      END.
    END.       
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdBokforingsbilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdBokforingsbilag Procedure 
PROCEDURE oppdBokforingsbilag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR d31DecFgAr AS DATE NO-UNDO.
  &SCOPED-DEFINE Tabell   Bokforingsbilag

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".


  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr     = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.Aar          = TT_{&Tabell}.Aar       AND
      {&Tabell}.BokforingsNr = TT_{&Tabell}.BokforingsNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr     = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.Aar          = TT_{&Tabell}.Aar       AND
        {&Tabell}.BokforingsNr = TT_{&Tabell}.BokforingsNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.

/* Åpningsskjemahantering */
   IF cKontrolltabell = "2" THEN DO:
       FIND ApnSkjema WHERE ApnSkjema.ButikkNr = {&Tabell}.ButikkNr AND
                            ApnSkjema.Ar       = {&Tabell}.Aar NO-ERROR.  
       IF AVAIL ApnSkjema THEN DO:
           ASSIGN d31DecFgAr = DATE(12,31,{&Tabell}.Aar - 1)
                  ENTRY({&Tabell}.OmsetningsDato - d31DecFgAr,ApnSkjema.OpenClosed) = "4".
       END.
   END.
/* Åpnings.... SLUTT      */
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdDags_Rap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdDags_Rap Procedure 
PROCEDURE oppdDags_Rap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Dags_Rap

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
               STRING(iAntLinjer) + 
               " av " + 
               STRING(iTotAntLinjer) + 
               ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Butikk       = TT_{&Tabell}.Butikk    AND
      {&Tabell}.Dato         = TT_{&Tabell}.Dato
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.Butikk       = TT_{&Tabell}.Butikk    AND
        {&Tabell}.Dato         = TT_{&Tabell}.Dato
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdForsalj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdForsalj Procedure 
PROCEDURE oppdForsalj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Forsalj

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ForsNr        = TT_{&Tabell}.ForsNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ForsNr        = TT_{&Tabell}.ForsNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdGruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdGruppe Procedure 
PROCEDURE oppdGruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Gruppe

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
               STRING(iAntLinjer) + 
               " av " + 
               STRING(iTotAntLinjer) + 
               ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.GruppeNr       = TT_{&Tabell}.GruppeNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.GruppeNr       = TT_{&Tabell}.GruppeNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdHgrDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdHgrDag Procedure 
PROCEDURE oppdHgrDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   HgrDag

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
               STRING(iAntLinjer) + 
               " av " + 
               STRING(iTotAntLinjer) + 
               ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButNr = TT_{&Tabell}.ButNr  AND
      {&Tabell}.Hgr   = TT_{&Tabell}.Hgr    AND
      {&Tabell}.Dato  = TT_{&Tabell}.Dato
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButNr = TT_{&Tabell}.ButNr  AND
      {&Tabell}.Hgr   = TT_{&Tabell}.Hgr    AND
      {&Tabell}.Dato  = TT_{&Tabell}.Dato
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKasse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKasse Procedure 
PROCEDURE oppdKasse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Kasse

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
               STRING(iAntLinjer) + 
               " av " + 
               STRING(iTotAntLinjer) + 
               ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.GruppeNr       = TT_{&Tabell}.GruppeNr  AND
      {&Tabell}.KasseNr        = TT_{&Tabell}.KasseNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.GruppeNr       = TT_{&Tabell}.GruppeNr  AND
        {&Tabell}.KasseNr        = TT_{&Tabell}.KasseNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    IF lHK = FALSE THEN 
        BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    ELSE 
        ASSIGN 
        {&Tabell}.ButikkNr = TT_{&Tabell}.ButikkNr
        {&Tabell}.GruppeNr = TT_{&Tabell}.GruppeNr
        {&Tabell}.KasseNr  = TT_{&Tabell}.KasseNr
        .
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKassererbilag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKassererbilag Procedure 
PROCEDURE oppdKassererbilag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   KassererBilag

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
      {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer  AND
      {&Tabell}.BilagsNr       = TT_{&Tabell}.BilagsNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
        {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer  AND
        {&Tabell}.BilagsNr       = TT_{&Tabell}.BilagsNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKassererKontanter) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKassererKontanter Procedure 
PROCEDURE oppdKassererKontanter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   KassererKontanter

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
      {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer 
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
        {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer 
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKassererOppgj) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKassererOppgj Procedure 
PROCEDURE oppdKassererOppgj :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   KassererOppgj

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
      {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
        {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKassererValuta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKassererValuta Procedure 
PROCEDURE oppdKassererValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   KassererValuta

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
      {&Tabell}.KasseNr        = TT_{&Tabell}.KasseNr AND
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
      {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer  AND
      {&Tabell}.ValKod         = TT_{&Tabell}.ValKod
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButikkNr       = TT_{&Tabell}.ButikkNr  AND
        {&Tabell}.KasseNr        = TT_{&Tabell}.KasseNr AND
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.KassererNr     = TT_{&Tabell}.KassererNr  AND
        {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer  AND
        {&Tabell}.ValKod         = TT_{&Tabell}.ValKod
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKas_Rap) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKas_Rap Procedure 
PROCEDURE oppdKas_Rap :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Kas_Rap

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".


  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.Butikk         = TT_{&Tabell}.Butikk  AND
      {&Tabell}.Kasse          = TT_{&Tabell}.Kasse  AND
      {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.Butikk         = TT_{&Tabell}.Butikk  AND
        {&Tabell}.Kasse          = TT_{&Tabell}.Kasse  AND
        {&Tabell}.Z_Nummer       = TT_{&Tabell}.Z_Nummer
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKonto) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKonto Procedure 
PROCEDURE oppdKonto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Konto

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.Butikk         = TT_{&Tabell}.Butikk  AND
      {&Tabell}.Kontonummer    = TT_{&Tabell}.Kontonummer
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.Butikk         = TT_{&Tabell}.Butikk  AND
        {&Tabell}.Kontonummer    = TT_{&Tabell}.Kontonummer
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKort_Spes) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKort_Spes Procedure 
PROCEDURE oppdKort_Spes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Kort_Spes

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
      {&Tabell}.Butikk         = TT_{&Tabell}.Butikk  AND
      {&Tabell}.Kasse          = TT_{&Tabell}.Kasse  AND
      {&Tabell}.z_nummer       = TT_{&Tabell}.z_Nummer  AND
      {&Tabell}.KortType       = TT_{&Tabell}.KortType
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.Dato           = TT_{&Tabell}.Dato AND
        {&Tabell}.Butikk         = TT_{&Tabell}.Butikk  AND
        {&Tabell}.Kasse          = TT_{&Tabell}.Kasse  AND
        {&Tabell}.z_nummer       = TT_{&Tabell}.z_Nummer  AND
        {&Tabell}.KortType       = TT_{&Tabell}.KortType
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdLager Procedure 
PROCEDURE oppdLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Lager

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ArtikkelNr = TT_{&Tabell}.ArtikkelNr AND
      {&Tabell}.Butik      = TT_{&Tabell}.Butik
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    IF TT_{&Tabell}.Butik > 0 THEN 
    DO:
      FIND {&Tabell} EXCLUSIVE-LOCK WHERE
          {&Tabell}.ArtikkelNr = TT_{&Tabell}.ArtikkelNr AND
          {&Tabell}.Butik      = TT_{&Tabell}.Butik
        NO-ERROR.
      IF CAN-FIND(ArtBas WHERE
                  ArtBas.ArtikkelNr = TT_{&Tabell}.ArtikkelNr) THEN
      DO:
          IF NOT AVAILABLE {&Tabell} THEN
            CREATE {&Tabell}.
          BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
          RELEASE {&Tabell}.
      END.
    END.    
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdSelger) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdSelger Procedure 
PROCEDURE oppdSelger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Selger

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.SelgerNr       = TT_{&Tabell}.SelgerNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.SelgerNr       = TT_{&Tabell}.SelgerNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdStLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdStLager Procedure 
PROCEDURE oppdStLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   StLager

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.StTypeId       = TT_{&Tabell}.StTypeId AND
      {&Tabell}.Butik          = TT_{&Tabell}.Butik    AND
      {&Tabell}.DataObjekt     = TT_{&Tabell}.DataObjekt 
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.StTypeId       = TT_{&Tabell}.StTypeId AND
        {&Tabell}.Butik          = TT_{&Tabell}.Butik  AND
        {&Tabell}.DataObjekt     = TT_{&Tabell}.DataObjekt
      NO-ERROR.
    DO:
        IF NOT AVAILABLE {&Tabell} THEN
          CREATE {&Tabell}.
        BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    END.
    IF AVAILABLE {&Tabell} THEN
        RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdStLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdStLinje Procedure 
PROCEDURE oppdStLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   StLinje

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
               STRING(iAntLinjer) + 
               " av " + 
               STRING(iTotAntLinjer) + 
               ".".


  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.StTypeId       = TT_{&Tabell}.StTypeId AND
      {&Tabell}.PerId          = TT_{&Tabell}.PerId  AND
      {&Tabell}.DataObjekt     = TT_{&Tabell}.DataObjekt  AND
      {&Tabell}.Diverse        = TT_{&Tabell}.Diverse  AND
      {&Tabell}.Butik          = TT_{&Tabell}.Butik  AND
      {&Tabell}.Aar            = TT_{&Tabell}.Aar  AND
      {&Tabell}.PerLinNr       = TT_{&Tabell}.PerLinNr
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.StTypeId       = TT_{&Tabell}.StTypeId AND
        {&Tabell}.PerId          = TT_{&Tabell}.PerId  AND
        {&Tabell}.DataObjekt     = TT_{&Tabell}.DataObjekt  AND
        {&Tabell}.Diverse        = TT_{&Tabell}.Diverse  AND
        {&Tabell}.Butik          = TT_{&Tabell}.Butik  AND
        {&Tabell}.Aar            = TT_{&Tabell}.Aar  AND
        {&Tabell}.PerLinNr       = TT_{&Tabell}.PerLinNr
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    ASSIGN
        {&Tabell}.AarPerLinNr = int(STRING({&Tabell}.Aar,"9999") + STRING({&Tabell}.PerLinNr,"999"))
        .

    /* Oppretter eventuelle lokale leverandører. */
    IF {&Tabell}.StType = "LEVERAN" THEN
    DO:
        IF NOT CAN-FIND(FIRST LevBas WHERE
                        LevBas.LevNr = INT({&Tabell}.DataObjekt)) THEN
        DO:
            CREATE LevBas.
            ASSIGN
                LevBas.LevNr   = INT({&Tabell}.DataObjekt)
                LevBas.LevNamn = "Lokal lev(" + string(INT({&Tabell}.Butik)) + ").".
                .
            RELEASE LevBas.
        END.
    END.

    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdTimeDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdTimeDag Procedure 
PROCEDURE oppdTimeDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   Timedag

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButNr          = TT_{&Tabell}.ButNr AND
      {&Tabell}.Dato           = TT_{&Tabell}.Dato
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButNr          = TT_{&Tabell}.ButNr AND
        {&Tabell}.Dato           = TT_{&Tabell}.Dato
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVareDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVareDag Procedure 
PROCEDURE oppdVareDag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE Tabell   VareDag

  IF NOT CAN-FIND(FIRST TT_{&Tabell}) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON WRITE OF {&Tabell} OVERRIDE 
  DO:  
  END.
  ON DELETE OF {&Tabell} OVERRIDE 
  DO:  
  END.

  STATUS DEFAULT "Lese linje " + 
                 STRING(iAntLinjer) + 
                 " av " + 
                 STRING(iTotAntLinjer) + 
                 ".".

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 3 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
      {&Tabell}.ButNr          = TT_{&Tabell}.ButNr AND
      {&Tabell}.Ean            = TT_{&Tabell}.Ean AND
      {&Tabell}.Dato           = TT_{&Tabell}.Dato
      NO-ERROR.
    IF AVAILABLE {&Tabell} THEN
    DO:
      /* Sett inn betingelse her.. */
      DO:
        DELETE {&Tabell} NO-ERROR.
        DELETE TT_{&Tabell}.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_{&Tabell} WHERE
    TT_{&Tabell}.RecType = 1 TRANSACTION:
    FIND {&Tabell} EXCLUSIVE-LOCK WHERE
        {&Tabell}.ButNr          = TT_{&Tabell}.ButNr AND
        {&Tabell}.Ean            = TT_{&Tabell}.Ean AND
        {&Tabell}.Dato           = TT_{&Tabell}.Dato
      NO-ERROR.
    IF NOT AVAILABLE {&Tabell} THEN
      CREATE {&Tabell}.
    BUFFER-COPY TT_{&Tabell} TO {&Tabell}.
    RELEASE {&Tabell}.
    DELETE TT_{&Tabell}.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR piError AS INT  NO-UNDO.

  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT ON ERROR UNDO, RETRY:
    /* Ved overføring av bilder blir det kalabaise. Derfor No-ERROR. */
    IMPORT STREAM InnFil 
        ^
        NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        piError = piError + 1.

    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .

    IF iTotAntLinjer MODULO 100 = 0 THEN
        STATUS DEFAULT "Teller opp linjer: " + 
                       STRING(iTotAntLinjer)  +
                       ".".

/*     IF ERROR-STATUS:ERROR = FALSE AND trim(entry(1,pcTekst," "),'"') = "H" THEN          */
/*     DO:                                                                                  */
/*       ASSIGN                                                                             */
/*           iTotAntLinjer = iTotAntLinjer + 1                                              */
/*           iTotAntLinjer = iTotAntLinjer + INT(ENTRY(NUM-ENTRIES(cLinje," "),cLinje," ")) */
/*           .                                                                              */
/*     END.                                                                                 */
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

