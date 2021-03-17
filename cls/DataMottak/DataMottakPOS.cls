 
 /*------------------------------------------------------------------------
    File        : DataMottakPOS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Thu Feb 20 13:56:15 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.DataMottak.DataMottakPOS FINAL: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLogKat  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest    AS LOG       NO-UNDO.
  DEFINE VARIABLE cSearchPattern AS CHARACTER NO-UNDO.
  DEFINE VARIABLE piFilType AS INTEGER NO-UNDO.
  DEFINE VARIABLE h_Parent AS HANDLE NO-UNDO.
  DEFINE VARIABLE cBkuFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bAppServer AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
/*  DEFINE VARIABLE rFile AS cls.StdFunk.File NO-UNDO.*/

  DEFINE TEMP-TABLE ttDataSett LIKE DataSett.

  DEFINE STREAM Inn.
  DEFINE STREAM Ut.
  
  {cls\Stdfunk\File.i}  
  { cls\StdFunk\filliste.i }
    
  CONSTRUCTOR PUBLIC DataMottakPOS ( INPUT pcLogg AS CHARACTER ):
    SUPER ().
    
    ASSIGN       
      bTest    = TRUE
      cLogg    = (IF pcLogg = '' THEN 
                   'DataMottakPOS' + REPLACE(STRING(TODAY),'/','')
                 ELSE 
                   pcLogg)
      cSearchPattern = 'PRSJournal*.*'     
      piFilType = 1
      .
    
    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).
/*    rFile = NEW cls.StdFunk.File( ).*/
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Initiert DataMottakPOS.cls.' 
        ).    

    {syspara.i 200 1 9 cKatalog}
    ASSIGN
      cLogKat  = 'log\' 
      cKatalog = RIGHT-TRIM(cKatalog,'\') 
      .
    /* For utviklers test. */  
    IF SEARCH('tnc.txt') <> ? THEN 
      cKatalog = 'C:\NSoft\Polygon\PRS\kom\in'.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Leser fra katalog: ' + cKatalog 
        ).    
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose: Bygger opp en liste over filene som ligger klar for innlesning.
   Notes:   Filene skal stemme medSearchPattern.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL byggFilListe( INPUT-OUTPUT DATASET dsFile ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cPrefixFilter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExtentFilter AS CHARACTER NO-UNDO.
    
/*    rFile:emptyTempTable( ).                                   */
/*    rFile:readDirectory(INPUT cKatalog, INPUT cSearchPattern ).*/
/*    rFile:getDataSetFile(OUTPUT DATASET dsFile).               */

    ASSIGN 
      cPrefixFilter = 'PRSJournal'
      cExtentFilter = '' /* Skal være blankt her. */
      .

    EMPTY TEMP-TABLE ttFile.
    EMPTY TEMP-TABLE tmpFiler.
    rStandardFunksjoner:LagFillisteForKatalog( INPUT  cKatalog,
                                               INPUT  cPrefixFilter, 
                                               INPUT  cExtentFilter, 
                                               OUTPUT TABLE tmpFiler ).
    IF CAN-FIND(FIRST tmpFiler) THEN
    DO:
      FOR EACH tmpFiler:
        CREATE ttFile.
        ASSIGN 
          ttFile.iFileId        = tmpFiler.FilId
          ttFile.FileName       = tmpFiler.File-Name 
          ttFile.FullName       = tmpFiler.Full-Path-Name 
          ttFile.FileDirectory  = REPLACE(tmpFiler.PathName,tmpFiler.File-Name,'')
          ttFile.FileSize       = tmpFiler.File-Size
          ttFile.Creationtime   = DATETIME(tmpFiler.File-Create-Date,File-Create-Time)
          ttFile.LastAccessTime = ttFile.Creationtime
          ttFile.LastWriteTime  = ttFile.Creationtime
          ttFile.Extension      = ENTRY(2,tmpFiler.File-Name,'.') 
          ttFile.DirectoryName  = tmpFiler.PathName 
          ttFile.IsReadOnly     = FALSE
          .
      END.
      EMPTY TEMP-TABLE tmpFiler.     
      bResult = TRUE.
    END.
    ELSE 
      bResult = FALSE.

    IF bTest AND SEARCH('tnc.txt') <> ? THEN 
      DATASET dsFile:WRITE-JSON ("file", cLogKat + 'PRSJournalLstTEST1.' + REPLACE(STRING(TODAY),'/','') + 'JSon',TRUE).
      
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Renser listen for poster som ikke fyller kravene.
   Notes:   Ekstent skal være et gyldig butikknr, og kassenummeret skal være en 
            kasse som er lagt opp på butikken.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL rensFilListe( INPUT-OUTPUT DATASET dsFile  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE piButNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE piGruppeNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE piKasseNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcFilNavn AS CHARACTER NO-UNDO.
    
    ASSIGN 
      piGruppeNr = 1
      .
    /* Sjekker butikk, og kasse */
    FILSJEKK:
    FOR EACH ttFile:
      ASSIGN 
        piButNr   = INT(ENTRY(2,ttFile.FileName,'.'))
        piKasseNr = INT(ENTRY(2,ttFile.FileName,'_'))
        pcFilNavn = ttFile.FullName 
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO: 
        DELETE ttFile.
        NEXT.
      END.

      FIND Butiker NO-LOCK WHERE 
        Butiker.butik  = piButNr NO-ERROR. 
      IF NOT AVAILABLE Butiker THEN
      DO: 
        DELETE ttFile.
        NEXT.
      END.
      
      FIND Kasse NO-LOCK WHERE 
        Kasse.butikkNr  = piButNr AND 
        Kasse.GruppeNr  = piGruppeNr AND 
        Kasse.KasseNr   = piKasseNr NO-ERROR.
      IF NOT AVAILABLE Kasse THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    **Ukjent butikk og kasse - innlesning av fil avbrutt (' + STRING(piButNr) + '/' + STRING(piKasseNr) + ' ' + ttFile.FileName + ' ).'
            ).    
        DELETE ttFile.
        NEXT.
      END.
    END. /* FILSJEKK */

    IF CAN-FIND(FIRST ttFile) THEN 
      bResult = TRUE.
    ELSE 
      bResult = FALSE.
      
    IF bTest AND SEARCH('tnc.txt') <> ? THEN 
      DATASET dsFile:WRITE-JSON ("file", cLogKat + 'PRSJournalLstTEST2.' + REPLACE(STRING(TODAY),'/','') + 'JSon',TRUE).
      
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Leser listen med filer som skal importeres og behandles.
            For hver gyldig fil i listen, startes behandling av fil.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL lesInnFiler( INPUT-OUTPUT DATASET dsFile ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    
    LESLISTE:
    FOR EACH ttFile:
      bResult = behandleFiler( ttFile.FileName, ttFile.FileDirectory, ttFile.FileName ).  
    END. /* LESLISTE */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL behandleFiler( INPUT pcFilNavn AS CHARACTER,
                                       INPUT pcKatalog AS CHARACTER, 
                                       INPUT pcFulltNavn AS CHARACTER ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE plFilId LIKE Filer.FilId NO-UNDO.
    DEFINE VARIABLE piAntLinjer AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcFullPath AS CHARACTER NO-UNDO.

    ASSIGN 
      pcFullPath = RIGHT-TRIM(pcKatalog,'\') + '\' + pcFilNavn
      .

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Leser inn fil(' + pcFullPath + ' ).'
        ).

    pcFullPath = SEARCH(pcFullPath). 

    TRANSBLOKK:
    DO /*TRANSACTION*/:
      IF SEARCH(pcFullPath) = ? THEN
      DO: 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    **Filen er slettet - innlesning av fil avbrutt (' + pcFullPath + ' ).'
            ).    
        LEAVE TRANSBLOKK.
      END.

      /* Henter filinformasjonen */
      FILE-INFO:FILE-NAME = pcFullPath.

      /* Sjekker om filen er innlest fra før. */
      IF CAN-FIND(Filer WHERE 
                  Filer.FilNavn   = pcFilNavn AND 
                  Filer.Dato      = FILE-INFO:FILE-MOD-DATE AND 
                  Filer.Kl        = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS") AND 
                  Filer.Storrelse = FILE-INFO:FILE-SIZE AND 
                  Filer.Katalog   = pcKatalog
                  ) THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    **Filen er innlest tidligere (' + pcFullPath + ' ).'
            ).
        LEAVE TRANSBLOKK.
      END.

      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Leser inn fil: ' + pcFullPath 
          ).    
      
      /* Oppretter Fil record. */
      FILRECORD:
      DO:
        /* Finner FilId */
        FIND LAST Filer NO-LOCK NO-ERROR.
        IF AVAILABLE Filer THEN
          plFilId = Filer.FilId + 1.
        ELSE
          plFilId = 1.
          
        CREATE Filer.
        ASSIGN
          Filer.FilId     = plFilId
          Filer.FilNavn   = pcFilNavn
          Filer.Katalog   = pcKatalog
          Filer.Dato      = FILE-INFO:FILE-MOD-DATE
          Filer.Kl        = STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS")
          Filer.Storrelse = FILE-INFO:FILE-SIZE
          Filer.AntLinjer = 0
          Filer.FilType   = 1
          .
        FIND CURRENT Filer NO-LOCK.    
      END. /* FILRECORD */
      
      /* Importerer og behandler filen */
      IF AVAILABLE Filer THEN 
      IMPORTER:
      DO ON ERROR UNDO, LEAVE:
        piAntLinjer = 0.
        RUN xinnprseljournal.p 
            (INPUT  Filer.FilId,
             INPUT  h_Parent,
             INPUT  h_Parent,
             OUTPUT piAntLinjer
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  ** Error ved innlensing av fil (Ant. linjer: ' + STRING(piAntLinjer) + ').' 
              ).  
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  ** Filen er ikke lagt til backup katalog (Ant. linjer: ' + STRING(piAntLinjer) + ').' 
              ).  
        END.
        ELSE DO:
          IF RETURN-VALUE <> '' THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  RETURN-VALUE (xinnprseljournal.p): ' + RETURN-VALUE + ').' 
                ).  
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Lest inn fil (Ant. linjer: ' + STRING(piAntLinjer) + ').' 
              ).
          /* Flagger filen klar for overføring */
          klarForOverforing (Filer.FilId).
          /* Kopierer filens innhold til backup fil, og sletter filen. */
          backupAvFil (pcFullPath).
          
          bResult = TRUE.
        END.  
      END. /* IMPORTER */
    END. /* TRANSBLOKK TRANSACTION */

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID setParent( INPUT ph_Parent AS HANDLE ):
    
    ASSIGN 
      h_Parent = ph_Parent
      .
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes: kom\in\PRSJournal_1_20160914.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL backupAvFil( INPUT pcFil AS CHARACTER ):
    
    DEFINE VARIABLE pBesult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcBkuFil AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecord AS CHARACTER NO-UNDO.
    
    ASSIGN 
      pcBkuFil = REPLACE(pcFil,'.~\','')
      pcBkuFil = REPLACE(pcBkuFil,'PRSJournal','PRSPOSJournal')
      .
    ENTRY(NUM-ENTRIES(pcBkuFil,'~\'),pcBkuFil,'~\') = 'bku~\' + ENTRY(NUM-ENTRIES(pcBkuFil,'~\'),pcBkuFil,'~\'). 
    
    DO ON ERROR UNDO, LEAVE:
      INPUT STREAM Inn FROM VALUE(pcFil).
      OUTPUT STREAM Ut TO VALUE(pcBkuFil) APPEND.
      REPEAT:
        IMPORT STREAM Inn UNFORMATTED  
          cRecord.
        PUT STREAM Ut UNFORMATTED 
          cRecord 
          SKIP. 
      END.
      OUTPUT STREAM Ut CLOSE.
      INPUT STREAM Inn CLOSE.
      OS-DELETE VALUE(pcFil).
    END.
    
    RETURN pBesult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID klarForOverforing( INPUT plFilId AS DECIMAL ):

    DEFINE BUFFER bufFiler FOR Filer.
  
    FIND Filer NO-LOCK WHERE 
      Filer.FilId = plfilId  NO-ERROR.
    IF AVAILABLE Filer THEN 
    DO FOR bufFiler TRANSACTION:
      FIND bufFiler EXCLUSIVE-LOCK WHERE 
        ROWID(bufFiler) = ROWID(Filer) NO-ERROR.
      IF AVAILABLE bufFiler THEN 
      DO:
        ASSIGN 
          bufFiler.Innlest       = TRUE
          bufFiler.InnlestDato   = TODAY
          bufFiler.InnlestKl     = TIME
          bufFiler.InnlestAv     = USERID('Skotex')
          bufFiler.Oppdatert     = TRUE
          bufFiler.OppdatertDato = TODAY
          bufFiler.OppdatertKl   = TIME
          bufFiler.OppdatertAv   = USERID('Skotex')
          bufFiler.Overfort      = FALSE
          bufFiler.OppdatertDato = ?
          buffiler.OppdatertKl   = 0
          bufFiler.OppdatertAv   = ''
          .
        RELEASE bufFiler.
      END.
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Stoppe rutinen hvis det er grunn for det.
   Notes:   Sjekker om databasene er oppkoblet, om stoppfil finnes eller om det 
            er lagt inn stopp i systemparameter.
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC CHARACTER sjekkStopp( pcStoppFil AS CHARACTER ):
    
    DEFINE VARIABLE cResult AS CHAR NO-UNDO.

    DEFINE BUFFER bufSyspara FOR sysPara.

    ASSIGN 
      cResult = 'OK'
      .
    SJEKKBLOKK:
    DO:
      IF SEARCH(pcStoppFil) <> ? THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Stoppfil funnet. Rutine stoppet (' + SEARCH(pcStoppFil) + ').' 
            ).  
        cResult = 'FALSE'.
        LEAVE SJEKKBLOKK.
      END.
      IF NOT CONNECTED('SkoTex') THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Database Skotex er ikke oppkoblet. Rutine stoppet.' 
            ).  
        cResult = 'FALSE'.
        LEAVE SJEKKBLOKK.
      END.
      IF NOT CONNECTED('Data') THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Database Data er ikke oppkoblet. Rutine stoppet.' 
            ).  
        cResult = 'FALSE'.
        LEAVE SJEKKBLOKK.
      END.
      IF NOT CONNECTED('Vpi') THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Database Vpi er ikke oppkoblet. Rutine stoppet.' 
            ).  
        cResult = 'FALSE'.
        LEAVE SJEKKBLOKK.
      END.
/*      IF NOT CONNECTED('Wr') THEN                             */
/*      DO:                                                     */
/*        rStandardFunksjoner:SkrivTilLogg(cLogg,               */
/*            '  Database Wr er ikke oppkoblet. Rutine stoppet.'*/
/*            ).                                                */
/*        cResult = 'FALSE'.                                    */
/*        LEAVE SJEKKBLOKK.                                     */
/*      END.                                                    */
    END. /* SJEKKBLOKK */
    
    IF cResult = 'OK' THEN     
    DO FOR bufSysPara TRANSACTION:
      FIND bufSysPara NO-LOCK WHERE
        bufSysPara.SysHId = 200 AND
        bufSysPara.SysGr  = 1 AND
        bufSysPara.ParaNr = 3 NO-ERROR.
      IF AVAILABLE bufSysPara THEN
      DO:
        IF bufSysPara.Parameter2 MATCHES '*STOPP*' THEN 
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Stoppkode satt i systemparameter 200 1 3. Rutine stoppet (' + bufSysPara.Parameter2 + ').' 
              ).
                
        cResult = bufSysPara.Parameter2.
      END.
    END.
    
    RETURN cResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC VOID byggListeDatasett( INPUT-OUTPUT TABLE ttDataSett ):
    
    EMPTY TEMP-TABLE ttDataSett.
    
    FOR EACH DataSett NO-LOCK WHERE
      DataSett.FilId >= 0 AND
      DataSett.SettStatus >= 2 AND 
      DataSett.SettStatus <= 8 AND
      DataSett.Behandlet  >= 3 AND
      DataSett.Behandlet  <= 4:
        
      CREATE ttDataSett.
      BUFFER-COPY DataSett TO ttDataSett.
    END.
    
    RETURN.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
     
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL overforDataSett(  INPUT-OUTPUT TABLE ttDataSett ):
    
    DEFINE VARIABLE bReturn AS LOG NO-UNDO.    
    DEFINE VARIABLE piAntLinjer AS INTEGER NO-UNDO.
    DEFINE VARIABLE iX AS INTEGER NO-UNDO.
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  overforDatasett: '
        ).
    
    DATASETT_LOOP:
    FOR EACH ttDataSett:
      OVERFOR:
      DO ON ERROR UNDO, LEAVE:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Datasett FØR: FilId: ' + STRING(ttDataSEtt.FilId) +  
              ' ButikkNr: ' + STRING(ttDataSett.ButikkNr) + 
              ' Dato: ' + STRING(ttDataSett.Dato) + 
              ' SettNr: ' + STRING(ttDataSett.SettNr) + 
              ' SettStatus: ' + STRING(ttDataSett.SettStatus) + 
              ' Behandlet: ' + STRING(ttDataSett.Behandlet)
            ).

        /* Starter overføring av bonger. */
        RUN xoverforbong.p (ttDataSett.DataSettId, 
                            h_Parent, 
                            h_Parent, 
                            OUTPUT piAntLinjer).
        IF ERROR-STATUS:ERROR OR RETURN-VALUE <> "" THEN
        DO:
          IF RETURN-VALUE <> "" THEN 
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  **Feil (RETURN-VALUE) ved overføring av datasett: ' + RETURN-VALUE
                ).
          ELSE DO:
            DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:  
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '  **Feil ved overføring av datasett: ' + ERROR-STATUS:GET-MESSAGE(ix)
                  ).
            END.  
             
          END.
        END.
        ELSE DO:
          IF RETURN-VALUE <> '' THEN
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '  RETURN (fra xbehipeljournalpos.p): ' + RETURN-VALUE + '.'
                ).
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '  Overført datasett (Ant. linjer: ' + STRING(piAntLinjer) + ').'
              ).
          bReturn = TRUE.
        END.
        /* Flagger filen som overført. */
        IF bReturn THEN 
          SetFilOverfort( ttDataSett.FilId).
      END. /* OVERFOR */
    END. /* DATASETT_LOOP */

    RETURN bReturn.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL SetFilOverfort( INPUT pLFilId AS DECIMAL ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DO TRANSACTION:
        FIND Filer EXCLUSIVE-LOCK WHERE
            Filer.FilId = pLFilId NO-ERROR.
        IF AVAILABLE Filer THEN
            ASSIGN
            Filer.Overfort     = TRUE
            Filer.OverfortDato = TODAY
            Filer.OverfortTid  = TIME
            Filer.OverfortAv   = USERID("SkoTex")
            .
        IF AVAILABLE Filer THEN
          RELEASE Filer.
    END.

    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL sjekkEODForDato( INPUT piButNr AS INTEGER, INPUT pdDato AS DATE  ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bufKasse FOR Kasse.
    DEFINE BUFFER bufEODKasse FOR EODKasse.
    
    ASSIGN 
      bResult = TRUE
      .
    
    DO FOR bufKasse:
      FOR EACH bufKasse NO-LOCK WHERE 
        bufKasse.ButikkNr = piButNr AND 
        bufKasse.Aktiv    = TRUE AND 
        bufKasse.KasseNr  <= 90:
        IF NOT CAN-FIND(bufEODKasse WHERE
          bufEODKasse.ButikkNr = bufKasse.ButikkNr AND
          bufEODKasse.GruppeNr = bufKasse.GruppeNr AND 
          bufEODKasse.KasseNr  = bufKasse.KasseNr AND
          bufEODKasse.EODDato  = pdDato) 
          THEN bResult = FALSE.
      END.
    END.

    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC DataMottakPOS ( ):

    EMPTY TEMP-TABLE ttFile.
    EMPTY TEMP-TABLE ttDataSett.
    EMPTY TEMP-TABLE tmpFiler.    
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Avsluttet DataMottakPOS.cls.' 
        ).    

  END DESTRUCTOR.

END CLASS.