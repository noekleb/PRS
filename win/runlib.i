/************************************************************
    Program:  runlib.i
    Created:  TN   11 Jun 98
Description:  Include for håndtering av procedurebibloteket.
              - Definerer handle som peker på bibloteket.
              - Sjekker om prosedurebibloteket er startet fra
                før.
              - Hvis bibloteket ikke er startet, startes
                dette.
              - Bibloteksprosedyren blir merket med
                "BIBLOTEK" i PrivatData, slik at den blir
                lett å kjenne igjen.

              Denne includefilen skal legges inn i alle
              programmer før Main-Blokk.

              {runlib.i} /* Starter prosedurebibloteket. */

Last change:  TN    9 Nov 99    9:40 am
              SJ   27.02.00 Forward function Tx().
************************************************************/

DEF VAR wLibHandle AS HANDLE NO-UNDO.
DEF VAR wWindows   AS HANDLE NO-UNDO.
DEF VAR wWinfunc   AS HANDLE NO-UNDO.
DEF VAR wProExtra  AS HANDLE NO-UNDO.

RUN RunProcLib ("p-biblo.p", "BIBLIOTEK", output wLibHandle).

&IF "{&API}":U="API":U &THEN
  RUN RunProcLib ("windows.p",  "BIB-WIN-API",  output wWindows).
  RUN RunProcLib ("winfunc.p",  "BIB-WIN-FUNC", output wWinfunc).
  RUN RunProcLib ("proextra.p", "BIB-WIN-PROX", output wProExtra).
&ELSE

&ENDIF

/* Kjorer en procedure persistent */
PROCEDURE RunProcLib:
  DEF INPUT  PARAMETER wProcName AS CHAR   NO-UNDO.
  DEF INPUT  PARAMETER wProdID   AS CHAR   NO-UNDO.
  DEF OUTPUT parameter wHandle   AS HANDLE NO-UNDO.

  ASSIGN wHandle = SESSION:FIRST-PROCEDURE.

  DO WHILE VALID-HANDLE(wHandle):
     IF wHandle:FILE-NAME EQ wProcName AND
          wHandle:PRIVATE-DATA EQ wProdID THEN LEAVE.

     ASSIGN wHandle = wHandle:NEXT-SIBLING.
  END.

  IF NOT VALID-HANDLE(wHandle) THEN
  DO:
    RUN VALUE(wProcName) PERSISTENT SET wHandle.
    ASSIGN wHandle:PRIVATE-DATA = wProdID.
  END.

END PROCEDURE.

PROCEDURE AvsluttProgrammet:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END PROCEDURE.

/* Prototyp for Tx */
FUNCTION Tx RETURNS CHARACTER 
    (INPUT wTxt AS CHARACTER, INPUT wTxNr AS INTEGER) FORWARD.


