
/*------------------------------------------------------------------------
    File        : fyllDatasettMedlem.p
    Purpose     : Felles rutine for ulike programmer som bruker datasettet.

    Syntax      :

    Description : Bygger opp datasettet dsMedlem.  

    Author(s)   : Tom Nøkleby
    Created     : Thu Nov 05 14:30:48 CET 2020
    Notes       :
      
    RUN fyllDatasettMedlem.p (Medlem.MedlemsNr, INPUT-OUTPUT DATASET dsMedlem BY-REFERENCE, FALSE).
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\dintero\ttBong.i}
{cls\dintero\dsBong.i}

DEFINE INPUT PARAMETER plB_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER bAdder AS LOG NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBong.

DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.

DEFINE QUERY qBong FOR BongHode.

DEFINE DATA-SOURCE srcBong FOR QUERY qBong.
DEFINE DATA-SOURCE srcBongLinje FOR BongLinje.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

QUERY qBong:QUERY-PREPARE ( "FOR EACH BongHode WHERE BongHode.B_Id = '" + STRING(plB_Id) + "'" ).
  
BUFFER ttBongHode:ATTACH-DATA-SOURCE ( DATA-SOURCE srcBong:HANDLE ).
IF NOT bAdder THEN 
  BUFFER ttBongHode:FILL-MODE = 'EMPTY'.
BUFFER ttBongLinje:ATTACH-DATA-SOURCE ( DATA-SOURCE srcBongLinje:HANDLE ).

DATASET dsBong:FILL ().

RETURN.

/* **********************  Internal Procedures  *********************** */

FINALLY:
   BUFFER ttBongHode:DETACH-DATA-SOURCE ( ).        
   BUFFER ttBongLinje:DETACH-DATA-SOURCE ( ).        
END FINALLY.


  