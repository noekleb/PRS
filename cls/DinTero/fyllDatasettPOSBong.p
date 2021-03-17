
/*------------------------------------------------------------------------
    File        : fyllDatasettPOSBong.p
    Purpose     : Felles rutine for ulike programmer som bruker datasettet.

    Syntax      :

    Description : Bygger opp datasettet dsPOSBong.  

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 05 14:30:48 CET 2020
    Notes       :
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\dintero\ttPOSBong.i}
{cls\dintero\dsPOSBong.i}

DEFINE INPUT PARAMETER plB_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER bAdder AS LOG NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPOSBong.

DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.

DEFINE QUERY qPOSBong FOR POS.BongHode.

DEFINE DATA-SOURCE srcPOSBong FOR QUERY qPOSBong.
DEFINE DATA-SOURCE srcPOSBongLinje FOR POS.BongLinje.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

QUERY qPOSBong:QUERY-PREPARE ( "FOR EACH POS.BongHode WHERE POS.BongHode.B_Id = '" + STRING(plB_Id) + "'" ).
  
BUFFER ttPOSBongHode:ATTACH-DATA-SOURCE ( DATA-SOURCE srcPOSBong:HANDLE ).
IF NOT bAdder THEN 
  BUFFER ttPOSBongHode:FILL-MODE = 'EMPTY'.
BUFFER ttPOSBongLinje:ATTACH-DATA-SOURCE ( DATA-SOURCE srcPOSBongLinje:HANDLE ).

DATASET dsPOSBong:FILL ().

RETURN.

/* **********************  Internal Procedures  *********************** */

FINALLY:
   BUFFER ttPOSBongHode:DETACH-DATA-SOURCE ( ).        
   BUFFER ttPOSBongLinje:DETACH-DATA-SOURCE ( ).        
END FINALLY.


  