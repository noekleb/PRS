/************************************************************
    Program:  gmlinterface.i
    Created:  TN   12 Nov 98
Description:  Funksjoner for interface av gamle SkoTex
              programmer.

Last change:  TN   29 Jan 1999   11:07 am
************************************************************/

DEF VAR wDefGlobVarOk     as LOG          init FALSE.

/* Definering av globale variabler for kjøring av gamle SkoTex programmer. */
procedure DefGlobVar:
  if wDefGlobVarOk = FALSE then
    DO:
      assign
        wDefGlobVarOK = TRUE.
      run P000-4.p.
      run P000-5.p.
      run P000-6.p.
      run P000-7.p.
      run P000-8.p.
    END.
END PROCEDURE.


