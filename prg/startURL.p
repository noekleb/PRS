DEF INPUT PARAMETER cURL AS CHAR NO-UNDO.

DEFINE VARIABLE hInstance AS INTEGER NO-UNDO.

RUN ShellExecuteA ( INPUT 0, 
                  INPUT "open", 
                  INPUT "C:\NSoft\Polygon\PRSRestCl\NetHelp\index.html", 
                  INPUT "", 
                  INPUT "", 
                  INPUT 0, 
                  OUTPUT hInstance ).

PROCEDURE ShellExecuteA EXTERNAL "shell32.dll":
  DEFINE INPUT PARAMETER hwnd AS LONG. /* Handle to parent window */
  DEFINE INPUT PARAMETER lpOperation AS CHAR. /* Operation to perform: open, print */
  DEFINE INPUT PARAMETER lpFile AS CHAR. /* Document or executable name */
  DEFINE INPUT PARAMETER lpParameters AS CHAR. /* Command line parameters to executable in lpFile */
  DEFINE INPUT PARAMETER lpDirectory AS CHAR. /* Default directory */
  DEFINE INPUT PARAMETER nShowCmd AS LONG. /* Whether shown when opened:
                                              0 hidden, 
                                              1 normal, 
                                              minimized 2, 
                                              maximized 3,
                                              0 if lpFile is a document */
  DEFINE RETURN PARAMETER hInstance AS LONG. /* Less than or equal to 32 */
END.
