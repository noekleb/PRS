DEFINE VARIABLE chExcel AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chWorkbook AS COM-HANDLE NO-UNDO.
def var cMyNewFile as char no-undo init "C:\polygon\PRS\konv\NY_OUTLET(TELLING)BEHOLDNING23102020.xlsx".

CREATE "excel.application" chExcel CONNECT.
if error-status:num-messages > 0 then 
CREATE "excel.application" chExcel.

/* Open an Excel document */
chWorkbook=chExcel:Workbooks:OPEN(cMyNewFile).
chExcel:visible = yes.

/*SaveAs
Refer http://msdn.microsoft.com/en-us/library/office/bb241279(v=office.12).aspx
for FileFormat enumerations. For example, 51 saves the file as .xlsx.
*/
chWorkbook:SaveAs("c:\appdir\se\konv\test5",51,"myPass",,,,,,,,,).

/* Quit Excel */
chExcel:quit().

/* Release Com-handle */

RELEASE OBJECT chWorkbook.
RELEASE OBJECT chExcel.
