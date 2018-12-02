FIND LAST JboxCompany NO-LOCK NO-ERROR.
IF AVAIL JboxCompany THEN CURRENT-VALUE(seqJboxCompanyId) = JBoxCompany.iJboxCompanyId.

FIND LAST JboxFunctionAccess NO-LOCK NO-ERROR.
IF AVAIL JboxFunctionAccess THEN CURRENT-VALUE(seqJboxFunctionAccessId)
= JBoxFunctionAccess.iJboxFunctionAccessId.

FIND LAST JboxFunction NO-LOCK NO-ERROR.
IF AVAIL JboxFunction THEN CURRENT-VALUE(seqJboxFunctionId) = JBoxFunction.iJboxFunctionId.

FIND LAST JboxGenCode NO-LOCK NO-ERROR.
IF AVAIL JboxGenCode THEN CURRENT-VALUE(seqJboxGenCodeId) = JBoxGenCode.iJboxGenCodeId.

FIND LAST JboxGenCodeType NO-LOCK NO-ERROR.
IF AVAIL JboxGenCodeType THEN CURRENT-VALUE(seqJboxGenCodeTypeId) = JBoxGenCodeType.iJboxGenCodeTypeId.

FIND LAST JboxMenu NO-LOCK NO-ERROR.
IF AVAIL JboxMenu THEN CURRENT-VALUE(seqJboxMenuId) = JBoxMenu.iJboxMenuId.

FIND LAST JboxTranslation NO-LOCK NO-ERROR.
IF AVAIL JboxTranslation THEN CURRENT-VALUE(seqJboxTranslationId) = JBoxTranslation.iJboxTranslationId.

FIND LAST JboxUserGroup NO-LOCK NO-ERROR.
IF AVAIL JboxUserGroup THEN CURRENT-VALUE(seqJboxUserGroupId) = JBoxUserGroup.iJboxUserGroupId.

FIND LAST JboxUserGroupMembers NO-LOCK NO-ERROR.
IF AVAIL JboxUserGroupMembers THEN
CURRENT-VALUE(seqJboxUserGroupMembersId) = JBoxUserGroupMembers.iJboxUserGroupMembersId.

FIND LAST JboxUserMenu NO-LOCK NO-ERROR.
IF AVAIL JboxUserMenu THEN CURRENT-VALUE(seqJboxUserMenuId) = JBoxUserMenu.iJboxUserMenuId.

FIND LAST JboxUserSetting NO-LOCK NO-ERROR.
IF AVAIL JboxUserSetting THEN CURRENT-VALUE(seqJboxUserSettingId) = JBoxUserSetting.iJboxUserSettingId.
