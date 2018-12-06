
/*------------------------------------------------------------------------
    File        : abc_excelrapport_run.p 
    Purpose     : 

    Syntax      :    Toø Goran

    Description : Starter abc rapport 1

    Author(s)   : tomn
    Created     : Mon Apr 06 17:32:28 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE obOk       AS LOG       NO-UNDO.
DEFINE VARIABLE ocMelding  AS CHAR      NO-UNDO.
DEFINE VARIABLE cRappNr    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.
DEFINE VARIABLE dDato      AS DATE      NO-UNDO. 
DEFINE VARIABLE iOffset    AS INT       NO-UNDO.

/* Initiering av JukeBox */
RUN initjukebox.p.
DYNAMIC-FUNCTION('setEnableColor',NO).
DYNAMIC-FUNCTION("setAttribute",SESSION,"SE_PRINTER",SESSION:PRINTER-NAME).

ASSIGN
  dDato   = TODAY
  iOffset = 1.
   
/* Henter oppkoblingsinfo fra oppstartsicon. */
IF SESSION:PARAMETER <> "" THEN 
DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "RAPPNR" AND 
        NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN 
    DO:
        ASSIGN 
            cRappNr = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "OFFSET" AND 
        NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN 
    DO:
        ASSIGN 
            iOffset = INT(ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=")).
    END.
END.
/*DEBUGGER:INITIATE().
DEBUGGER:SET-BREAK().
*/   
IF cRappNr = '' THEN RETURN.
ELSE RUN excelrapport.p (INPUT '225|' + cRappNr + '|' + string(iOffset) ,INPUT ?,INPUT ?, OUTPUT ocMelding,OUTPUT obOk) NO-ERROR.

QUIT.



