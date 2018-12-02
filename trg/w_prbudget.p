TRIGGER PROCEDURE FOR WRITE OF prBudget.

prBudget.datotid = deci(STRING(YEAR(TODAY),"9999") + 
                        STRING(MONTH(TODAY),"99")  +
                        STRING(DAY(TODAY),"99")    +
                        REPLACE(STRING(TIME,"HH:MM:SS"),":","")).

