
/*------------------------------------------------------------------------
    File        : fix-slett_pakkseddler.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Thu Dec 19 10:52:19 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cPkSdlListe AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cPkSdlNr AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
CURRENT-WINDOW:WIDTH = 350.

FORM 
WITH FRAME A DOWN.

/*ASSIGN                     */
/*  cPkSdlListe = '214201,' +*/
/*                '214203,' +*/
/*                '214204,' +*/
/*                '214212,' +*/
/*                '214213,' +*/
/*                '214217,' +*/
/*                '214214,' +*/
/*                '214215,' +*/
/*                '214210,' +*/
/*                '214211,' +*/
/*                '214194,' +*/
/*                '214198,' +*/
/*                '214199,' +*/
/*                '214195,' +*/
/*                '214196,' +*/
/*                '214197,' +*/
/*                '214422,' +*/
/*                '200739,' +*/
/*                '200741,' +*/
/*                '200738,' +*/
/*                '200734,' +*/
/*                '200736,' +*/
/*                '200733,' +*/
/*                '200752,' +*/
/*                '200700,' +*/
/*                '200735,' +*/
/*                '200737,' +*/
/*                '191403,' +*/
/*                '214423'   */
/*                .          */


/* Ref. email fra 19/12-19fra Are. */ 
ASSIGN 
  cPkSdlListe = '190892,' +
                '203396,' +
                '206106,' +
                '212738,' +
                '155669,' +
                '160034,' +
                '160406,' +
                '160442,' +
                '160444,' +
                '161952,' +
                '163725,' +
                '176684,' +
                '178176,' +
                '179611,' +
                '187787,' +
                '190892,' +
                '203396,' +
                '204441,' +
                '206106,' +
                '212738,' +
                '214812,' +
                '215683,' +
                '217378,' +
                '160034,' +
                '160406,' +
                '178176,' +
                '206106,' +
                '212738,' +
                '204265,' +
                '202299,' +
                '213942,' +
                '213942,' +
                '204265'
                . 
 
DO iLoop = 1 TO NUM-ENTRIES(cPkSdlListe):
  cPkSdlNr = ENTRY(iLoop,cPkSdlListe).
  FOR EACH PkSdlHode EXCLUSIVE-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10 AND 
    PkSdlHode.PkSdlNr = cPkSdlNr:
    DISPLAY
      cPkSdlNr 
      PkSdlHode.PkSdlNr
      PkSdlHode.PkSdlStatus
      CAN-FIND(FIRST PkSdlLinje OF PkSdlHode)
    WITH WIDTH 350 FRAME A.
    DOWN 1 WITH FRAME A.

    /* Døden - db trigger tar resten. */
    DELETE PkSdlHode.

  END.
END.


