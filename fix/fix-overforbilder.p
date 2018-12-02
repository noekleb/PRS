FOR EACH jfSkoTex.Bilderegister /*WHERE 
  NOT CAN-FIND(SkoTex.BildeRegister WHERE
               SkoTex.BildeRegister.BildNr = jfSkoTex.BildeRegister.BildNr)*/:
  FOR EACH jfSkoTex.BildeData WHERE 
      jfSkoTex.BildeData.BildNr = jfSkoTex.BildeRegister.BildNr:
    DO:
        IF AVAILABLE SkoTex.BildeData THEN
            RELEASE SkoTex.BildeData.
        IF NOT CAN-FIND(SkoTex.BildeData WHERE
                        SkoTex.BildeData.BildNr = jfSkoTex.BildeData.BildNr AND
                        SkoTex.BildeData.Teller = jfSkoTex.BildeData.Teller) THEN
        
        BUFFER-COPY jfSkoTex.BildeData TO SkoTex.BildeData.
    END.
  END.
END.
