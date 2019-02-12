/*seqJBoxUserSettingId*/

FIND LAST JBoxUserSetting.
    DISPLAY
        JBoxUserSetting
        .
CURRENT-VALUE(seqJBoxUserSettingId) = JBoxUserSetting.iJBoxUserSettingId + 1.


