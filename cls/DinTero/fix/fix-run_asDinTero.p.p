
DEFINE VAR icParam     AS CHARACTER NO-UNDO.
DEFINE VAR ihBuffer    AS HANDLE NO-UNDO.
DEFINE VAR icSessionId AS CHARACTER NO-UNDO.
DEFINE VAR ocReturn    AS CHARACTER NO-UNDO.
DEFINE VAR obOK        AS LOG NO-UNDO.

DEF VAR cApiNode AS CHAR NO-UNDO.

/* getToken */
/* listSessions */
/* listProfiles */
/* createSession */

ASSIGN 
    cApiNode = 'createSession'
    .

CASE cApiNode:
    WHEN 'getToken' THEN
    DO:
        RUN cls\DinTero\asDinTero.p('getToken',
                                    ?,
                                    '',
                                    OUTPUT ocReturn,
                                    OUTPUT obOk
                                    ).
    END.
    WHEN 'listSessions' THEN
    DO:
        RUN cls\DinTero\asDinTero.p('listSessions|' + 
                                    'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJ3cml0ZTpjaGVja291dCByZWFkOmNoZWNrb3V0IHdyaXRlOnJlY2VpcHRzIHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6ZGlzY291bnRzOi9hdmFpbGFibGVfZm9yX3JlY2VpcHQiLCJ2ZXJzaW9uIjoyLCJpYXQiOjE2MDI0MTEzMzIsImV4cCI6MTYwMjQyNTczMn0.JzxvQk3EZYOEi6k7Q7DldUEbMrLAftXrqqceNvv9jipFUY2AEuWeNFL36wrp2aaS4tINTxnMGYprq4NOXtOmfcOfoYV3vcydGNnO4iECcg6UdS-9uoNh0y0wasNwPSjrXqhb9efivezR2UwvssuAt_uXNBq0w_AbLHCfxiI6psrCfT6o9VS0MNIcmjk70Paiegg9mr5JZ5UaytUV4lzN-eoGHD7vN944nIFhhpKZrGJ-w7A_mkhQ8LYSc-dsFBl0KByOUdl9XPbUdyCF7ckCFF3KZ915j3JWy2BIj1_eVzTYcyMAhNyo1xY_HUToeraMR4ZFv0TuZOVdEREzdJ8kLw|' + 
                                    'Bearer',
                                    ?,
                                    '',
                                    OUTPUT ocReturn,
                                    OUTPUT obOk
                                    ).
    END.
    WHEN 'listProfiles' THEN
    DO:
        RUN cls\DinTero\asDinTero.p('listProfiles|' + 
                                    'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJ3cml0ZTpjaGVja291dCByZWFkOmNoZWNrb3V0IHdyaXRlOnJlY2VpcHRzIHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6ZGlzY291bnRzOi9hdmFpbGFibGVfZm9yX3JlY2VpcHQiLCJ2ZXJzaW9uIjoyLCJpYXQiOjE2MDI0MTEzMzIsImV4cCI6MTYwMjQyNTczMn0.JzxvQk3EZYOEi6k7Q7DldUEbMrLAftXrqqceNvv9jipFUY2AEuWeNFL36wrp2aaS4tINTxnMGYprq4NOXtOmfcOfoYV3vcydGNnO4iECcg6UdS-9uoNh0y0wasNwPSjrXqhb9efivezR2UwvssuAt_uXNBq0w_AbLHCfxiI6psrCfT6o9VS0MNIcmjk70Paiegg9mr5JZ5UaytUV4lzN-eoGHD7vN944nIFhhpKZrGJ-w7A_mkhQ8LYSc-dsFBl0KByOUdl9XPbUdyCF7ckCFF3KZ915j3JWy2BIj1_eVzTYcyMAhNyo1xY_HUToeraMR4ZFv0TuZOVdEREzdJ8kLw|' + 
                                    'Bearer',
                                    ?,
                                    '',
                                    OUTPUT ocReturn,
                                    OUTPUT obOk
                                    ).
    END.
    WHEN 'createSession' THEN
    DO:
        RUN cls\DinTero\asDinTero.p('createSession|' + 
                                    'eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6ImFwaS5kaW50ZXJvLmNvbS8wMWUyY2I2ZWZiMTVlNTAyNmQ1OTU3Njc3MjkwZDVkMDY0ZDc1N2ZmIn0.eyJpc3MiOiJodHRwczovL2FwaS5kaW50ZXJvLmNvbSIsImF1ZCI6Imh0dHBzOi8vVDExMTEyNTQyQGFwaS5kaW50ZXJvLmNvbS92MS9hY2NvdW50cy9UMTExMTI1NDIiLCJzdWIiOiJjMGZhYjEwZi1lY2M1LTRjOGQtYTE1Mi05M2ZkMGZmMzE0YWIiLCJzY29wZXMiOiJ3cml0ZTpjaGVja291dCByZWFkOmNoZWNrb3V0IHdyaXRlOnJlY2VpcHRzIHdyaXRlOm5vdGlmaWNhdGlvbnMgd3JpdGU6ZGlzY291bnRzOi9hdmFpbGFibGVfZm9yX3JlY2VpcHQiLCJ2ZXJzaW9uIjoyLCJpYXQiOjE2MDI0MTEzMzIsImV4cCI6MTYwMjQyNTczMn0.JzxvQk3EZYOEi6k7Q7DldUEbMrLAftXrqqceNvv9jipFUY2AEuWeNFL36wrp2aaS4tINTxnMGYprq4NOXtOmfcOfoYV3vcydGNnO4iECcg6UdS-9uoNh0y0wasNwPSjrXqhb9efivezR2UwvssuAt_uXNBq0w_AbLHCfxiI6psrCfT6o9VS0MNIcmjk70Paiegg9mr5JZ5UaytUV4lzN-eoGHD7vN944nIFhhpKZrGJ-w7A_mkhQ8LYSc-dsFBl0KByOUdl9XPbUdyCF7ckCFF3KZ915j3JWy2BIj1_eVzTYcyMAhNyo1xY_HUToeraMR4ZFv0TuZOVdEREzdJ8kLw|' + 
                                    'Bearer',
                                    ?,
                                    '',
                                    OUTPUT ocReturn,
                                    OUTPUT obOk
                                    ).
    END.
END CASE.

MESSAGE obOk SKIP(1)
    ocReturn
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
