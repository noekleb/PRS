  /* To manipulate the lookup query object add the callback procedure hook "myLookupObject" 
     (probably use "WHERE false" as the initial query in this case) */
  
  JBoxServerAPI:Instance:Lookup("<table>"
                    + ";<field1>"
                    + ";<field2>"
                   ,"WHERE true"
                   ,"<field1>,<field2>"   /* <- return values for these fields */
                    ).

  IF JBoxServerAPI:Instance:LookupOk THEN DO:
    ASSIGN <field1>:SCREEN-VALUE = JBoxServerAPI:Instance:LookupValue("<table>.<field1>")
           .

    APPLY "any-printable" TO <field1>.
  END.
