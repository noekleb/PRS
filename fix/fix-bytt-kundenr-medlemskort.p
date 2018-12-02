def var dNyttknr as deci no-undo.
dNyttKnr = 12600976.
find kunde where kunde.kundenr = dNyttKnr no-lock.
disp kunde.navn.
for each kundekort where kundekort.kortnr >= "19251" and 
                         kundekort.kortnr <= "20000" and length(kundekort.kortnr) = 5:
                         disp kundekort.innehaver kundekort.kundenr.
    for each medlemskort where medlemskort.interntkkortid = 
         kundekort.interntkkortid.
         find medlem of medlemskort.
/*          assign medlem.kundenr = dNyttKnr */
/*                 medlemskort.innehaver = "Ukjent " + kunde.navn. */
       
     disp kundekort.kortnr medlemskort.medlemsnr medlem.kundenr medlemskort.innehaver.
     end.
/*      assign kundekort.kundenr = dnyttknr */
/*             kundekort.innehaver = "Ukjent " + kunde.navn. */
end.
