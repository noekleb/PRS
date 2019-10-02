 
 /*------------------------------------------------------------------------
    File        : EksportNavision
    Purpose     : 
    Syntax      : 
    Description : Eksporterer ulike datatyper til Navision
    Author(s)   : tomn
    Created     : Mon Sep 30 14:59:39 CEST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS cls.Navision.EksportNavision: 
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.
  DEFINE VARIABLE cBku AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.  

  DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.

  DEFINE PUBLIC PROPERTY iAktiv AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDagsoppgjFil AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cFakturaFil AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY iEksportType AS INTEGER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cDagsKatalog AS CHARACTER NO-UNDO GET. SET.
  DEFINE PUBLIC PROPERTY cFakturaKatalog AS CHARACTER NO-UNDO GET. SET.

  CONSTRUCTOR PUBLIC EksportNavision ( pcLogg AS CHARACTER   ):
    SUPER ().

    rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner().
    
    ASSIGN 
      cLogg = pcLogg
      cBku    = 'bku'
      .
      bTest = IF SEARCH('test.txt') <> ? THEN TRUE ELSE FALSE
      .
    IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          'EksportNavision() NEW:' 
          ).
    
    
  END CONSTRUCTOR.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL eksportDagsrapp(  ):
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    bResult = TRUE.
    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Leser dagsrapp postene som skal eksporteres, og legger dem opp i temp-file.
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL lesDagsrapp(  ):
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    bResult = TRUE.
    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose: Ser til at filkatalogene er opprettet. 
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL prepKatalog(  ):   
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.

    ASSIGN 
      bResult = FALSE 
      .
    /* Oppretter kataloger hvis de mangler. */
    IF rStandardFunksjoner:prepKatalog(cDagsKatalog,cBku) THEN bResult = TRUE.
    IF rStandardFunksjoner:prepKatalog(cFakturaKatalog,cBku) THEN bResult = TRUE.

    RETURN bResult.
  END METHOD.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC LOGICAL settParametre(  ):
    DEFINE VARIABLE pbOk AS LOG NO-UNDO.
    {syspara.i 50 55 1 iAktiv INT}
    {syspara.i 50 55 2 cDagsoppgjFil}
    {syspar2.i 50 55 2 cDagsKatalog}
    {syspara.i 50 55 3 cFakturaFil}
    {syspar2.i 50 55 3 cFakturaKatalog}
    {syspara.i 50 55 4 iEksportType INT}

    ASSIGN 
      cDagsoppgjFil = REPLACE(cDagsoppgjFil,"DDMMYYYY",REPLACE(STRING(TODAY,"99/99/9999"),'/',''))
      cDagsoppgjFil = REPLACE(cDagsoppgjFil,"HHMMSS",REPLACE(STRING(TIME,"HH:MM:SS"),':',''))
      cFakturaFil   = REPLACE(cFakturaFil,"DDMMYYYY",REPLACE(STRING(TODAY,"99/99/9999"),'/',''))
      cFakturaFil   = REPLACE(cFakturaFil,"HHMMSS",REPLACE(STRING(TIME,"HH:MM:SS"),':',''))
      pbOk = TRUE 
      NO-ERROR.
      
    pbOk = FALSE.
    ASSIGN 
      cDagsKatalog = TRIM(cDagsKatalog,'\')
      cDagsKatalog = TRIM(cDagsKatalog,'/')
      cDagsKatalog = cDagsKatalog + '\'
      cFakturaKatalog = TRIM(cFakturaKatalog,'\')
      cFakturaKatalog = TRIM(cFakturaKatalog,'/')
      cFakturaKatalog = cFakturaKatalog + '\'
      pbOk = TRUE 
      NO-ERROR.

    IF bTest THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Parametre:' 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    iAktiv.........:' + STRING(iAktiv) 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cDagsoppgjFil..:' + cDagsoppgjFil 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cDagsKatalog...:' + cDagsKatalog 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cFakturaFil....:' + cFakturaFil 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    cFakturaKatalog:' + cFakturaKatalog 
          ).
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    iEksportType...:' + STRING(iEksportType) 
          ).
    END.

    IF iAktiv = 1 AND pbOk THEN 
      pbOk = TRUE.
    ELSE DO: 
      pbOk = FALSE.
      IF bTest THEN 
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  **Ikke aktiv eller feil ved lasting av parametre.' 
            ).
    END.

    RETURN pbOk.

  END METHOD.

  DESTRUCTOR PUBLIC EksportNavision ( ):

  END DESTRUCTOR.

END CLASS.