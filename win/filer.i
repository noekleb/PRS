/************************************************************
    Program:  filer.i
    Created:  TN   13 Nov 99
Description:  Funksjoner for å håndtere filer.

Last change:  TN   17 Nov 99   11:17 pm
************************************************************/

procedure OpenDocument :
  define input parameter wDocument  as char.
  DEFINE INPUT PARAMETER wParameter as CHAR NO-UNDO. /* Semikolonseparert liste med ekstra parametre. */

  def var executable as char.
  def var hInstance as integer.
  
  /* find the associated executable in registry */  
  Executable = fill("x", 255). /* =allocate memory */
  run FindExecutable{&A} in hpApi (wDocument,
                                   "",
                                   input-output Executable,
                                   output hInstance).

  /* if not found, show the OpenAs dialog from the Explorer */
  if hInstance>=0 and hInstance<=32 then  
     run ShellExecute{&A} in hpApi(0,
                                  "open",
                                  "rundll32.exe",
                                  "shell32.dll,OpenAs_RunDLL " + wDocument,
                                  "",
                                  1,
                                  output hInstance).

  /* now open the document. If the user canceled the OpenAs dialog,
     this ShellExecute call will silently fail */
  run ShellExecute{&A} in hpApi  (0,
                                  "open",
                                  wDocument,
                                  "",
                                  "",
                                  1,
                                  output hInstance).
end procedure.

PROCEDURE OpenExcelDocument:
  DEF INPUT PARAMETER ipFilnavn as CHAR NO-UNDO.
  DEF INPUT PARAMETER ipKatalog as CHAR NO-UNDO.

  def var hInstance   as int no-undo.
  
   DEFINE VARIABLE cOpenOfficeExec AS CHAR NO-UNDO. 
   DEFINE VARIABLE lOpenOffice AS LOGICAL NO-UNDO. 
   
   {syspara.i  1 1 210 cOpenOfficeExec}

   lOpenOffice = IF cOpenOfficeExec NE "" THEN TRUE ELSE FALSE. 

   IF lOpenOffice THEN 
   DO:
   
       RUN OpenFileInOpenOffice.p ((IF ipKatalog ne '' then ipKatalog + '\' else '') 
                                   + ipFilnavn).
   END. 

  else
  run ShellExecute{&A} in hpApi(0,
                                "open",
                                "Excel.exe",
                                ipFilnavn,
                                ipKatalog,
                                1,
                                output hInstance).
END PROCEDURE.

PROCEDURE GetTempFileName:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipPrefix      as CHAR NO-UNDO.
  DEF INPUT  PARAMETER ipExtent      as CHAR NO-UNDO.
  def output parameter iptmpFileName as char no-undo.

  /* Dette gir et windows tempfile name. Men da virker ikke macroen i Excel??
  RUN gettmpfile.p (INPUT ipPrefix).
  assign
    iptmpFileName = RETURN-VALUE.
  OVERLAY(iptmpFileName, R-INDEX(iptmpFileName,".") + 1, 3) = ipExtent.
  */

  def var iptmpDirName as char no-undo.
  
  assign
    iptmpDirName = session:TEMP-DIRECTORY.
  
  LET_LOOP:
  do while true:
    assign
      iptmpFileName = iptmpDirName +
                      ipPrefix + 
                      /*string(random(1,9999),"9999") +*/
                      REPLACE(STRING(TODAY),'/','') + 
                      REPLACE(STRING(TIME,"HH:MM:SS"),':','') +
                      "." + ipExtent.
    if search(iptmpFileName) = ? then
      leave LET_LOOP.
  end. /* LET_LOOP */

END PROCEDURE.

PROCEDURE WinTempFileName:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT  PARAMETER ipPrefix      as CHAR NO-UNDO.
  def output parameter iptmpFileName as char no-undo.

  RUN gettmpfile.p (INPUT ipPrefix).
  assign
    iptmpFileName = RETURN-VALUE.

END PROCEDURE.

PROCEDURE OpenWeb:
/*------------------------------------------------------------------------------
  Purpose:     Åpner webside
  Parameters:  se under
  Notes:       
------------------------------------------------------------------------------*/

  define input parameter document as char.

  def var executable as char.
  def var hInstance as integer.
  
  /* find the associated executable in registry */  
  Executable = fill("x", 255). /* =allocate memory */
  run FindExecutable{&A} in hpApi (document,
                                   "",
                                   input-output Executable,
                                   output hInstance).

  /* if not found, show the OpenAs dialog from the Explorer */
  if hInstance>=0 and hInstance<=32 and hInstance <> 2 then 
     run ShellExecute{&A} in hpApi(0,
                                  "open",
                                  "rundll32.exe",
                                  "shell32.dll,OpenAs_RunDLL " + document,
                                  "",
                                  1,
                                  output hInstance).

  /* now open the document. If the user canceled the OpenAs dialog,
     this ShellExecute call will silently fail */
  run ShellExecute{&A} in hpApi  (0,
                                  "open",
                                  document,
                                  "",
                                  "",
                                  1,
                                  output hInstance).
END PROCEDURE.


