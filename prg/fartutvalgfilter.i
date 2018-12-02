
        CASE ENTRY(iCount,pcFeltliste):
            WHEN "ArtikkelNr" THEN DO:
                IF FI-FraArtikkelNr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ArtikkelNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-FraArtikkelNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilArtikkelNr:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ArtikkelNr"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-TilArtikkelNr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "Levkod" THEN DO:
                IF FI-Levkod:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Levkod:SCREEN-VALUE = TRIM(FI-Levkod:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Levkod"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                    IF NUM-ENTRIES(FI-Levkod:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Levkod:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Levkod:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-Levkod:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "LevFargKod" THEN DO:
                IF FI-LevFargKod:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-LevFargKod:SCREEN-VALUE = TRIM(FI-LevFargKod:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "LevFargKod"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                    IF NUM-ENTRIES(FI-LevFargKod:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-LevFargKod:SCREEN-VALUE,"*") + "*" ELSE
                           FI-LevFargKod:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-LevFargKod:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "AnbefaltPris" THEN DO:
                IF int(FI-AnbefaltPris1:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnbefaltPris"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-AnbefaltPris1:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF int(FI-AnbefaltPris2:SCREEN-VALUE) <> 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnbefaltPris"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-AnbefaltPris2:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "Kjokkenskriver" THEN DO:
                IF int(FI-Kjokkenskriver:SCREEN-VALUE) > 0 THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Kjokkenskriver"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-Kjokkenskriver:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Pakke" THEN DO:
                IF CB-Pakke:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Pakke"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-Pakke:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Beskr" THEN DO:
                IF FI-Beskr:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Beskr:SCREEN-VALUE = TRIM(FI-Beskr:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Utvidetsok"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + FI-Beskr:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "CONTAINS".
            END.
            WHEN "Bongtekst" THEN DO:
                IF FI-Bongtekst:SCREEN-VALUE <> "" THEN
                ASSIGN
                FI-Bongtekst:SCREEN-VALUE = TRIM(FI-Bongtekst:SCREEN-VALUE)
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BongTekst"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                    IF NUM-ENTRIES(FI-Bongtekst:SCREEN-VALUE,"*") > 1 THEN
                        "*" + TRIM(FI-Bongtekst:SCREEN-VALUE,"*") + "*" ELSE
                           FI-Bongtekst:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                               IF NUM-ENTRIES(FI-BongTekst:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                   ELSE "BEGINS".
            END.
            WHEN "RegistrertDato" THEN DO:
                IF FI-FraOpprettet:SCREEN-VALUE <> "" AND FI-FraOpprettet:SCREEN-VALUE <> ? THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "RegistrertDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-FraOpprettet:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilOpprettet:SCREEN-VALUE <> "" AND FI-TilOpprettet:SCREEN-VALUE <> ? THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "RegistrertDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-TilOpprettet:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "EDato" THEN DO:
                IF FI-FraEndret:SCREEN-VALUE <> "" AND FI-FraEndret:SCREEN-VALUE <> ? THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-FraEndret:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilEndret:SCREEN-VALUE <> "" AND FI-TilEndret:SCREEN-VALUE <> ? THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "EDato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-TilEndret:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "VPIdato" THEN DO:
                IF FI-FraVPIdato:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "VPIdato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-FraVPIdato:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
                IF FI-TilVPIdato:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "VPIdato"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           FI-TilVPIdato:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
            END.
            WHEN "OPris" THEN DO:
                IF CB-OPris:SCREEN-VALUE <> "" THEN
                ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "OPris"
                       pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + CB-OPris:SCREEN-VALUE
                       pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Grunnsortiment" THEN DO:
                IF CB-Grunnsortiment:SCREEN-VALUE <> "" THEN
                ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Grunnsortiment"
                       pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + CB-Grunnsortiment:SCREEN-VALUE
                       pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Telefonkort" THEN DO:
                IF CB-Telefonkort:SCREEN-VALUE <> "" THEN
                ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Telefonkort"
                       pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + CB-Telefonkort:SCREEN-VALUE
                       pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Bonus_Givende" THEN DO:
                IF CB-Bonus_Givende:SCREEN-VALUE <> "" THEN
                ASSIGN pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Bonus_Givende"
                       pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + CB-Bonus_Givende:SCREEN-VALUE
                       pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "KjedeVare" THEN DO:
               IF CB-KjedeVare:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "KjedeVare"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-KjedeVare:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "NON_Sale" THEN DO:
               IF CB-NON-Sale:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "NON_Sale"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-NON-Sale:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Pant" THEN DO:
               IF CB-Pant:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Pant"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-Pant:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "NegVare" THEN DO:
               IF CB-NegVare:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "NegVare"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-NegVare:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Gjennomfaktureres" THEN DO:
               IF CB-GjFakturert:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Gjennomfaktureres"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-GjFakturert:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.    
            WHEN "Medlemsutbytte" THEN DO:
               IF CB-Medlemsutbytte:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Medlemsutbytte"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-Medlemsutbytte:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.    
            WHEN "HoyLavMva" THEN DO:
               IF CB-HoyLavMva:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "HoyLavMva"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-HoyLavMva:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.    
            WHEN "WebButikkArtikkel" THEN DO:
               IF CB-Web:SCREEN-VALUE <> "" THEN
               ASSIGN
               pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "WebButikkArtikkel"
               pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                          CB-Web:SCREEN-VALUE
               pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.    
            WHEN "BildeIKasse" THEN DO:
                IF CB-BildeIKasse:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BildeIKasse"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-BildeIKasse:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "HkStyrt" THEN DO:
                IF CB-HkStyrt:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "HkStyrt"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-HkStyrt:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "ManueltOpprettet" THEN DO:
                IF CB-ManueltOpprettet:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ManueltOpprettet"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-ManueltOpprettet:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "IKasse" THEN DO:
                IF CB-IKasse:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "IKasse"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-IKasse:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Kunderabatt" THEN DO:
                IF CB-Kunderabatt:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Kunderabatt"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-Kunderabatt:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "ManRabIKas" THEN DO:
                IF CB-ManRabIKas:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ManRabIKas"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-ManRabIKas:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Storrelser" THEN DO:
                IF CB-Storrelser:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Storrelser"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-Storrelser:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "Utgatt" THEN DO:
                IF CB-Utgatt:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Utgatt"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-Utgatt:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
            WHEN "AnonseArtikkel" THEN DO:
                IF CB-Annonse:SCREEN-VALUE <> "" THEN
                ASSIGN
                pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "AnonseArtikkel"
                pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                           CB-Annonse:SCREEN-VALUE
                pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
            END.
          WHEN "HovedModellFarge" THEN DO:
              IF CB-HovedModellFarge:SCREEN-VALUE = "yes" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "HovedModellFarge"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) +
                         CB-HovedModellFarge:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
              IF CB-HovedModellFarge:SCREEN-VALUE = "no" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "ModellFarge"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + '0'
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "lager" THEN DO:
              IF CB-lager:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "lager"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         CB-lager:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "VareType" THEN DO:
              IF CB-Varetype:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Varetype"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         CB-VareType:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "SalgsStopp" THEN DO:
              IF CB-SalgsStopp:SCREEN-VALUE <> "0" AND CB-SalgsStopp:SCREEN-VALUE <> "" AND CB-SalgsStopp:SCREEN-VALUE <> ? THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "SalgsStopp"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         CB-SalgsStopp:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "BestForslag" THEN DO:
              IF CB-BestForslag:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "BestForslag"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         CB-BestForslag:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "=".
          END.
          WHEN "Lokasjon" THEN DO:
              IF FI-Lokasjon:SCREEN-VALUE <> "" THEN
              ASSIGN
              FI-Lokasjon:SCREEN-VALUE = TRIM(FI-Lokasjon:SCREEN-VALUE)
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Lokasjon"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                  IF NUM-ENTRIES(FI-Lokasjon:SCREEN-VALUE,"*") > 1 THEN
                      "*" + TRIM(FI-Lokasjon:SCREEN-VALUE,"*") + "*" ELSE
                         FI-Lokasjon:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                             IF NUM-ENTRIES(FI-Lokasjon:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                 ELSE "BEGINS".
          END.  
          WHEN "SanertDato" THEN DO:
              IF FI-FraSanertDato:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "SanertDato"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         FI-FraSanertDato:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
              IF FI-TilSanertDato:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "SanertDato"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         FI-TilSanertDato:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
          END.
          WHEN "UtgattDato" THEN DO:
              IF FI-FraUtgattDato:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "UtgattDato"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         FI-FraUtgattDato:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + ">=".
              IF FI-TilUtgattDato:SCREEN-VALUE <> "" THEN
              ASSIGN
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "UtgattDato"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                         FI-TilUtgattDato:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + "<=".
          END.
          WHEN "Kampanjekode" THEN DO:
              IF FI-Kampanjekode:SCREEN-VALUE <> "" THEN
              ASSIGN
              FI-Kampanjekode:SCREEN-VALUE = TRIM(FI-Kampanjekode:SCREEN-VALUE)
              pcFields = pcFields + (IF pcFields = "" THEN "" ELSE ",") + "Kampanjekode"
              pcValues = pcValues + (IF pcValues = "" THEN "" ELSE CHR(1)) + 
                  IF NUM-ENTRIES(FI-Kampanjekode:SCREEN-VALUE,"*") > 1 THEN
                      "*" + TRIM(FI-Kampanjekode:SCREEN-VALUE,"*") + "*" ELSE
                         FI-Kampanjekode:SCREEN-VALUE
              pcOperator = pcOperator + (IF pcOperator = "" THEN "" ELSE ",") + 
                             IF NUM-ENTRIES(FI-Kampanjekode:SCREEN-VALUE,"*") > 1 THEN "MATCHES"
                                 ELSE "BEGINS".
          END.  
        END CASE.
