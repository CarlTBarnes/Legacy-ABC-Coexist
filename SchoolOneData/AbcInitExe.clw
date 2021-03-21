   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

   MAP
     MODULE('ABCINITEXE001.CLW')
Main                   PROCEDURE   !To generate Source needed to Init ABC
     END
    ! Declare init functions defined in a different dll
     MODULE('DATAABC.DLL')
DataAbc:Init           PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>)
DataAbc:Kill           PROCEDURE
     END
     MODULE('PROCABC.DLL')
ProcABC:Init           PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>)
ProcABC:Kill           PROCEDURE
     END
   END

SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
!endregion


FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
GlobalRequest        BYTE,EXTERNAL,THREAD                  ! Exported from a library, set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE,EXTERNAL,THREAD                  ! Exported from a library, set to the response from the form
VCRRequest           LONG,EXTERNAL,THREAD                  ! Exported from a library, set to the request from the VCR buttons

  CODE
  GlobalErrors.Init(GlobalErrorStatus)
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  INIMgr.Init('.\AbcInitExe.INI', NVD_INI)                 ! Configure INIManager to use INI file
  DataAbc:Init(GlobalErrors, INIMgr)                       ! Initialise dll (ABC)
  ProcABC:Init(GlobalErrors, INIMgr)                       ! Initialise dll (ABC)
  Main
  INIMgr.Update
  DataAbc:Kill()                                           ! Kill dll (ABC)
  ProcABC:Kill()                                           ! Kill dll (ABC)
  INIMgr.Kill                                              ! Destroy INI manager
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher

