
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
{cls\dintero\ttMedlem.i} 
{cls\dintero\dsMedlem.i}

DEFINE INPUT PARAMETER plMedlemsNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE INPUT PARAMETER bAdder AS LOG NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsMedlem.

DEFINE VARIABLE cWhere AS CHARACTER NO-UNDO.

DEFINE QUERY qMedlem FOR Medlem.

DEFINE DATA-SOURCE srcMedlem FOR QUERY qMedlem.
DEFINE DATA-SOURCE srcMedlemskort FOR Medlemskort.
DEFINE DATA-SOURCE srcMedlemsaldo FOR Medlemsaldo.
DEFINE DATA-SOURCE srcMedlemsgruppe FOR Medlemsgruppe.
DEFINE DATA-SOURCE srcMedlemsklubb FOR Medlemsklubb.
DEFINE DATA-SOURCE srcMedlemstype FOR Medlemstype.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

QUERY qMedlem:QUERY-PREPARE ( "FOR EACH Medlem WHERE Medlem.MedlemsNr = '" + STRING(plMedlemsNr) + "'" ).
  
BUFFER ttMedlem:ATTACH-DATA-SOURCE ( DATA-SOURCE srcMedlem:HANDLE ).
IF NOT bAdder THEN 
  BUFFER ttMedlem:FILL-MODE = 'EMPTY'.
BUFFER ttMedlemskort:ATTACH-DATA-SOURCE ( DATA-SOURCE srcMedlemskort:HANDLE ).
BUFFER ttMedlemsaldo:ATTACH-DATA-SOURCE ( DATA-SOURCE srcMedlemsaldo:HANDLE ).
BUFFER ttMedlemsgruppe:ATTACH-DATA-SOURCE ( DATA-SOURCE srcMedlemsgruppe:HANDLE ).
BUFFER ttMedlemsklubb:ATTACH-DATA-SOURCE ( DATA-SOURCE srcMedlemsklubb:HANDLE ).
BUFFER ttMedlemstype:ATTACH-DATA-SOURCE ( DATA-SOURCE srcMedlemstype:HANDLE ).

DATASET dsMedlem:FILL ().

RETURN.

/* **********************  Internal Procedures  *********************** */

FINALLY:
   BUFFER ttMedlem:DETACH-DATA-SOURCE ( ).        
END FINALLY.


  