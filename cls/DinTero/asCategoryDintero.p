
/*------------------------------------------------------------------------
    File        : asCategoryDintero.p
    Purpose     : App server rutine som lar oppdaterer en kategori inn mot 
                  Dintero.

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 16 10:54:47 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
  {cls\dintero\ttCategory.i}
  {cls\dintero\dsCategory.i}

  DEFINE INPUT  PARAMETER iHovedKatNr AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER bReturn AS LOG.
  DEFINE OUTPUT PARAMETER cReturn AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
  DEFINE VARIABLE bTest AS LOG NO-UNDO.  

  DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
  DEFINE VARIABLE rCustomerDintero    AS cls.Dintero.CustomerDintero NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.
rCustomerDintero = NEW cls.Dintero.CustomerDintero( ) NO-ERROR.

ASSIGN 
  bTest = TRUE
  cLogg = 'asCategoryDintero' + REPLACE(STRING(TODAY,'99/99/9999'),'/','')
  .

RUN behandleCategory.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE behandleCategory:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: 
------------------------------------------------------------------------------*/
  /* Sjekker at det finnes data å behandle. */
  FIND HovedKategori NO-LOCK WHERE 
      Hovedkategori.HovedKatNr = iHovedKatNr NO-ERROR.
  IF NOT AVAILABLE HovedKategori THEN 
    DO:
      cReturn = 'Ukjent hovedkategori: ' + STRING(iHovedKatNr) + '.'.
      bReturn = FALSE.
      RETURN.
    END. 

  CREATE ttCategory.
  ASSIGN 
      ttCategory.Category_id = STRING(Hovedkategori.HovedKatNr)
      ttCategory.category_name = Hovedkategori.HovedKatTekst
      .
  FIND FIRST ttCategory NO-ERROR.

  IF AVAILABLE ttCategory THEN 
  CATEGORYBLOKK:
  DO:
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Opprett/oppdater Category:' +  
      ' category_id: ' + ttCategory.category_id + 
      ' category_name: ' + ttCategory.category_name + '.'    
      ).    
  
    IF NOT rCustomerDintero:postNewCategory( INPUT-OUTPUT DATASET dsCategory, OUTPUT cReturn ) THEN
      DO:
        FIND FIRST ttCategory NO-ERROR.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  Feil ved opprettelse/oppdatering av category ' + ttCategory.category_id + '. Årsak: ' + cReturn
          ).
        cReturn = '  Feil ved opprettelse/oppdatering av category ' + ttCategory.category_id + '. Årsak: ' + cReturn.
        MESSAGE 'test-2' creturn
        VIEW-AS ALERT-BOX.
      END.
    ELSE DO:
      FIND FIRST ttCategory NO-ERROR.
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  Opprettet/oppdaterer Category: '  + ttCategory.category_id + '.'
        ).
      cReturn = '  OK opprettet/oppdaterer Category ' + ttCategory.category_id + '.'.
      bReturn = TRUE.
    END.
  END. /* CATEGORYBLOKK */

  IF bTest AND AVAILABLE ttCategory THEN 
    DATASET dsCategory:WRITE-JSON('file', 'konv\dsCategory' + STRING(ttCategory.category_id) + '.json', TRUE).
  
  EMPTY TEMP-TABLE ttCategory.
  
END PROCEDURE.


