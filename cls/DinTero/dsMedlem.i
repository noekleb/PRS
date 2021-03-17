
/*------------------------------------------------------------------------
    File        : dsMedlem.i
    Purpose     : 

    Syntax      :

    Description : Datasett definisjon for medlems datasettet.

    Author(s)   : Tom Nøkleby
    Created     : Mon Nov 02 15:48:17 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*{cls\dintero\ttMedlem.i}*/
DEFINE DATASET dsMedlem SERIALIZE-HIDDEN
  FOR ttMedlem, ttMedlemskort, ttMedlemsaldo, ttMedlemsgruppe, ttMedlemsklubb, ttMedlemstype   
  DATA-RELATION drMedlemskort FOR ttMedlem, ttMedlemskort RELATION-FIELDS (MedlemsNr, MedlemsNr) NESTED
  DATA-RELATION drMedlemsaldo FOR ttMedlem, ttMedlemsaldo RELATION-FIELDS (MedlemsNr, MedlemsNr) NESTED
  DATA-RELATION drMedlemsgruppe FOR ttMedlem, ttMedlemsgruppe RELATION-FIELDS (MedGruppe, MedGruppe) NESTED
  DATA-RELATION drMedlemsklubb FOR ttMedlem, ttMedlemsklubb RELATION-FIELDS (MKlubbId, MKlubbId) NESTED
  DATA-RELATION drMedlemstype FOR ttMedlem, ttMedlemstype RELATION-FIELDS (MedType, Medtype) NESTED
  .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
