&GLOBAL-DEFINE This_proc ASSIGN pdf_test.i = "Test".
 
{ pdfglobal.i "NEW SHARED"}
 
&IF "{1}" = "THIS-PROCEDURE" &THEN
    h_PDFinc = THIS-PROCEDURE. 
    DO WHILE VALID-HANDLE(h_PDFinc)
    AND h_PDFinc:PRIVATE-DATA <> 'Persistent PDFinc':
       h_PDFinc = h_PDFinc:NEXT-SIBLING.
    END.
&ENDIF
 
message {&This_proc}.
