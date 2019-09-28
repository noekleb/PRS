FOR EACH ELogg:
    DELETE ELogg.
END.
FOR EACH ArtLag WHERE ArtLag.LagAnt > 0:
    ArtLag.EndretDatoTid = NOW.
END.
RUN runPrepGoogleMerchant.p.
RUN cls\GoogleMerchant\runGoogleFtpSendfile.p
