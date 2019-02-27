
/*------------------------------------------------------------------------
    File        : ttVpiArtBas.i
    Purpose     : Laster over data som skal viderebehandles her, for å kunne gjøre samme behandling for ArtBas og VPIArtBas.

    Syntax      :

    Description : Definerer en temp tabell lik som VpiArtBas.

    Author(s)   : tny
    Created     : Thu Oct 11 16:31:15 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttVpiArtBas LIKE VPIArtBas.
DEFINE TEMP-TABLE ttVpiStrekkode LIKE VPIStrekkode.
DEFINE TEMP-TABLE ttVpiArtPris LIKE VPIArtPris.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
