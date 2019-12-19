 
 /*------------------------------------------------------------------------
    File        : PostPakkeEtikett
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Sat Oct 26 09:23:58 CEST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Consignor.PostPakkeEtikett: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
  DEFINE VARIABLE pcKatalog AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcPrefixFilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcExtentFilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pcBku AS CHARACTER NO-UNDO.
  DEFINE VARIABLE pix AS INTEGER NO-UNDO.
  
  { cls\StdFunk\filliste.i }

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
    
  CONSTRUCTOR PUBLIC PostPakkeEtikett ( INPUT pcLogg AS CHARACTER ):
    SUPER ().
    
    ASSIGN
      cLogg = pcLogg 
      bTest = (IF SEARCH('test.txt') <> ? THEN TRUE ELSE FALSE)
      bTest = TRUE
      .
    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).

    ASSIGN
      pcKatalog      = '\\gantapp02\Retur_Etikett' 
      pcBku          = pcKatalog + '\bku\'  
      pcPrefixFilter = ''
      pcExtentFilter = 'pdf' 
      .
    rStandardFunksjoner:opprettKatalog( INPUT pcBku ).
  
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL hentFilListe( OUTPUT TABLE tmpFiler ):
    
    rStandardFunksjoner:LagFillisteForKatalog( INPUT  pcKatalog,
                                               INPUT  pcPrefixFilter, 
                                               INPUT  pcExtentFilter, 
                                               OUTPUT TABLE tmpFiler).

    IF bTest THEN
      TEMP-TABLE tmpFiler:WRITE-JSON('file', 'konv\postPakkeFilListe.json', TRUE).
                                               
    IF CAN-FIND(FIRST tmpFiler) 
      THEN bResult = TRUE.
    ELSE bResult = FALSE. 
       
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL importerPdfFiler( INPUT TABLE tmpFiler ):
    DEFINE VARIABLE bResult AS LOG NO-UNDO.
    DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piSeqNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE pitype AS INTEGER NO-UNDO.
    
    IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Start importerPdfFiler. ' 
            ).    
    
    FOR EACH tmpFiler:
      ASSIGN 
        pcTekst = ENTRY(1,tmpFiler.File-Name,'.')
        piType  = IF NUM-ENTRIES(pcTekst,'_') = 2 THEN 2 ELSE 1
        .
      IF NUM-ENTRIES(pcTekst,'_') = 2 THEN
        piSeqNr = INT(ENTRY(2,pcTekst,'_')).
      ELSE 
        piSeqNr = 0.

      FIND LAST KOrdreHode NO-LOCK WHERE 
        KOrdreHode.EkstOrdreNr = ENTRY(1,pcTekst,'_') NO-ERROR.
        
      IF bTest THEN 
      DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Fil: ' + tmpFiler.File-Name 
              ).    
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Availalble KOrdreHode: ' + STRING(AVAILABLE KORdreHode)
              ).    
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    piType: ' + STRING(piType)
              ).    
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    piSeqNr: ' + STRING(piSeqNr)
              ).    
      END.
        
      IF AVAILABLE KOrdreHode THEN 
      DO:
        IF AVAILABLE KOrdrePostPakke THEN 
          RELEASE KOrdrePostPakke.

        FIND KOrdrePostPakke NO-LOCK WHERE 
          KOrdrePostPakke.KOrdre_Id = KOrdreHode.KOrdre_Id AND
          KOrdrePostPakke.Type      = piType AND 
          KOrdrePostPakke.SeqNr     = piSeqNr NO-ERROR. 

        IF NOT AVAILABLE KOrdrePostPakke THEN  
        DO TRANSACTION:

          CREATE KOrdrePostPakke.
          ASSIGN 
            KOrdrePostPakke.KOrdre_Id = KOrdreHode.KOrdre_Id
            KOrdrePostPakke.Type      = piType 
            KOrdrePostPakke.SeqNr     = piSeqNr
            KOrdrePostPakke.DatoTid   = NOW
            KOrdrePostPakke.BrukerId  = USERID('SkoTex') 
            .

          COPY-LOB FROM FILE(tmpFiler.Full-Path-Name) TO KOrdrePostPakke.PDFFil NO-ERROR.
          IF NOT ERROR-STATUS:ERROR THEN 
          DO:
            /* --- Kopierer til backup katalog. ---*/
            IF SEARCH(tmpFiler.Full-Path-Name) <> ? THEN
                OS-COPY VALUE(tmpFiler.Full-Path-Name) VALUE(pcBku + tmpFiler.File-Name).
            /* --- Sletter sendt fil. ---*/
            IF SEARCH(pcBku + tmpFiler.File-Name) <> ? THEN
                OS-DELETE VALUE(tmpFiler.Full-Path-Name).            
          END.
          ELSE DO:
            DO pix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                pcTekst = STRING(ERROR-STATUS:GET-NUMBER(pix)) + ' '+ 
                         ERROR-STATUS:GET-MESSAGE(pix). 
                rStandardFunksjoner:SkrivTilLogg(cLogg, 
                    '   ** Feil: ' + pcTekst 
                ). 
            END.          
          END.
          
          RELEASE KOrdrePostPakke.
          
        END. /*  TRANSACTION */
      END.
    END.

    IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Slutt importerPdfFiler. ' 
            ).
                
    bResult = TRUE.
        
    RETURN bResult.

  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL rensFilListe( INPUT-OUTPUT TABLE tmpFiler ):
    DEFINE VARIABLE plDec AS DECIMAL NO-UNDO.
    DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    FOR EACH tmpFiler:

      ASSIGN 
        pcTekst = ENTRY(1,tmpFiler.File-Name,'.')
        .

      /* Retur etikett har filnanv '500000018 RETUR.pdf' */
      IF NUM-ENTRIES(pcTekst,' ') = 2 AND 
        TRIM(ENTRY(2,pcTekst,' ')) = 'RETUR' THEN
      DO:
        ASSIGN 
          plDEC   = DEC(ENTRY(1,pcTekst,' '))
          NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DELETE tmpFiler.
      END.
      ELSE DO:  
        ASSIGN 
          plDEC   = DEC(ENTRY(1,pcTekst,'_'))
          NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DELETE tmpFiler.
      END.  

      /* Skrives det ut mer enn en etikett på en ordre, får de '_1, _2 osv. i filnavnet. */
      IF NUM-ENTRIES(pcTekst,'_') = 2 THEN
      DO:         
        ASSIGN 
          plDEC = DEC(ENTRY(2,pcTekst,'_'))
          NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
          DELETE tmpFiler.
      END.
    END.

    IF bTest THEN
      TEMP-TABLE tmpFiler:WRITE-JSON('file', 'konv\postPakkeFilListeRenset.json', TRUE).

    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC PostPakkeEtikett ( ):

  END DESTRUCTOR.

END CLASS.