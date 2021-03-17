 
 /*------------------------------------------------------------------------
    File        : pksdlEDIWeb.cls
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tomn
    Created     : Thu Feb 20 13:56:15 CET 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Pakkseddel.pksdlEDIWeb FINAL: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cKatalog AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cLogKat  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest    AS LOG       NO-UNDO.
  DEFINE VARIABLE cBkuFil AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bAppServer AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrefixFilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cExtentFilter AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMailLogg AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
/*  DEFINE VARIABLE rFile AS cls.StdFunk.File NO-UNDO.*/

  DEFINE STREAM Inn.
  DEFINE STREAM Ut.
  
  { cls\StdFunk\filliste.i }
    
  CONSTRUCTOR PUBLIC pksdlEDIWeb ( INPUT pcLogg AS CHARACTER ):
    SUPER ().
    
    ASSIGN       
      bTest    = TRUE
      cLogg    = (IF pcLogg = '' THEN 
                   'pksdlEdiweb' + REPLACE(STRING(TODAY),'/','')
                 ELSE 
                   pcLogg)
      cMailLogg     = 'pksdlEDIWeb' + REPLACE(STRING(TODAY),'/','') + REPLACE(STRING(TIME,"HH:MM:SS"),':','')
      cPrefixFilter = 'ediweb'
      cExtentFilter = 'csv'     
      .
    
    rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ).
/*    rFile = NEW cls.StdFunk.File( ).*/
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Initiert pksdlEDIWeb.cls.' 
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

  METHOD PUBLIC LOGICAL byggFilListe( INPUT-OUTPUT TABLE tmpFiler ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE tmpFiler.
    rStandardFunksjoner:LagFillisteForKatalog( INPUT  cKatalog,
                                               INPUT  cPrefixFilter, 
                                               INPUT  cExtentFilter, 
                                               OUTPUT TABLE tmpFiler ).
    IF CAN-FIND(FIRST tmpFiler) THEN
      bResult = TRUE.
    ELSE 
      bResult = FALSE.

    IF bTest AND SEARCH('tnc.txt') <> ? THEN 
      TEMP-TABLE tmpFiler:WRITE-JSON ("file", cLogKat + 'pksdlEDIWeb' + REPLACE(STRING(TODAY),'/','') + '.JSon',TRUE).
      
    RETURN bResult.

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

  METHOD PUBLIC LOGICAL importerFil( INPUT pcImportFil AS CHARACTER, OUTPUT pcMailLogg AS CHARACTER ):
    
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pcRecord AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcPkSdlNr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE plDummy AS DECIMAL NO-UNDO.
    DEFINE VARIABLE piLinjeNr AS INTEGER NO-UNDO.
    DEFINE VARIABLE pcButikk AS CHARACTER NO-UNDO.
    
    ASSIGN 
      pcMailLogg = ''
      .
    IF SEARCH(pcImportFil) = ? THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    **Ukjent fil. Import av fil avbrutt (' + pcImportFil + ').' 
            ).
      END.

    INPUT STREAM Inn FROM VALUE(pcImportfil).
    IMPORTER:
    REPEAT:
      piLinjeNr = piLinjeNr + 1.
      IMPORT STREAM Inn UNFORMATTED 
        pcRecord. 
      ASSIGN 
        pcPkSdlNr = SUBSTRING(ENTRY(1,pcRecord,';'),3)
        pcButikk  = ENTRY(5,pcRecord,';') + ' ' + ENTRY(6,pcRecord,';')
        pldummy   = DEC(pcPkSdlNr) 
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN 
        NEXT.
      ASSIGN 
        pcPkSdlNr = LEFT-TRIM(pcPkSdlNr,'0')
        .
      FIND LAST PkSdlHode NO-LOCK WHERE 
        PkSdlHode.PkSdlNr = pcPkSdlNr AND 
        PkSdlHode.PkSdlStatus = 10 NO-ERROR. 
      IF NOT AVAILABLE PkSdlHode THEN    
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    Ukjent PkSdlNr (Linje ' + STRING(piLinjeNr) +  '): ' + pcPkSdlNr + ' Butikk: ' + pcButikk + '.' 
            ).
      ELSE DO:
        IF PkSdlHode.SendtOutlet >= 1 THEN 
        DO:
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Allerede flagget PkSdlNr (Linje ' + STRING(piLinjeNr) +  '): ' + pcPkSdlNr + ' Butikk: ' + pcButikk + '.' 
              ).
          rStandardFunksjoner:SkrivTilLogg(cMailLogg,
              '    Allerede flagget PkSdlNr (Linje ' + STRING(piLinjeNr) +  '): ' + pcPkSdlNr + ' Butikk: ' + pcButikk + '.' 
              ).
          ASSIGN
            bResult    = TRUE
            .
        END.
        ELSE DO:
          DO TRANSACTION:
            FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
            ASSIGN 
              PkSdlHode.SendtOutlet            = 1
              PkSdlHode.SendtFraLagerTilOutlet = NOW
              PkSdlHode.Varetype               = IF NOT PkSdlHode.Varetype BEGINS 'EDIWeb' THEN 'EDIWeb ' + PkSdlHode.Varetype ELSE PkSdlHode.Varetype
              .
            FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
          END.
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    Flagget PkSdlNr (Linje ' + STRING(piLinjeNr) +  '): ' + pcPkSdlNr + ' Butikk: ' + pcButikk + '.' 
              ).
          rStandardFunksjoner:SkrivTilLogg(cMailLogg,
              '    Flagget PkSdlNr (Linje ' + STRING(piLinjeNr) +  '): ' + pcPkSdlNr + ' Butikk: ' + pcButikk + '.' 
              ).
          ASSIGN
            bResult    = TRUE
            .
        END.
      END.
      
    END. /* IMPORTER */
    INPUT STREAM Inn CLOSE.

    IF bResult THEN 
      pcMailLogg = SEARCH('log\' + cMailLogg + '.log'). 

    RETURN bResult.

  END METHOD.

  DESTRUCTOR PUBLIC pksdlEDIWeb ( ):

    EMPTY TEMP-TABLE tmpFiler.    
    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Avsluttet pksdlEDIWeb.cls.' 
        ).    

  END DESTRUCTOR.

END CLASS.