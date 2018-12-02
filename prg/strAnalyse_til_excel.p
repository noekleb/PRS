/* strAnalyse_til_excel.p                  */
/* Eksport av størrelsesanalyse til Excel. */
DEFINE INPUT PARAMETER cFilNavn AS CHAR NO-UNDO.

/* For Excel eksport */
{runlib.i}
{windows.i}
DEF STREAM Eksport.
DEF STREAM sExportFile.
{methodexcel.i}
/* Excel eksport slutt. */

IF VALID-HANDLE(wLibHandle) THEN
    RUN OpenExcelDocument IN wLibHandle (cFilNavn, " ").
