FOR EACH ELogg:
    DELETE ELogg.
END.
FOR EACH ArtLag WHERE ArtLag.LagAnt > 0:
    ArtLag.EndretDatoTid = NOW.
END.
/*RUN runPrepGoogleMerchant.p.*/

/* NB: Quit i programmet over gjør at denne ikke kjøres her. kjør den separat.
RUN cls\GoogleMerchant\runGoogleFtpSendfile.p
*/
