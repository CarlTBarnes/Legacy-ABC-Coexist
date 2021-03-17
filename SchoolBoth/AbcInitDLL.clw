
  PROGRAM
!_ABCDllMode_=>1;_ABCLinkMode_=>0

!---From Init EXE ----
   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE

  MAP 
Abc_Init_Kill PROCEDURE(BYTE KillMe=0)  !,EXPORT !<-- Does not work with #ImpLib

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

FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
GlobalErrorStatus    ErrorStatusClass,THREAD
GlobalErrors         ErrorClass                            ! Global error manager
INIMgr               INIClass                              ! Global non-volatile storage manager
!GlobalRequest        BYTE,EXTERNAL,THREAD                  ! Exported from a library, set when a browse calls a form, to let it know action to perform
!GlobalResponse       BYTE,EXTERNAL,THREAD                  ! Exported from a library, set to the response from the form
!VCRRequest           LONG,EXTERNAL,THREAD                  ! Exported from a library, set to the request from the VCR buttons

AbcHasBeenInited  BYTE 

  CODE
Abc_Init_Kill PROCEDURE(BYTE KillMe=0)
    CODE
    Message('Abc_Init_Kill v2 KillMe=' & KillMe)
    IF ~KillMe AND ~AbcHasBeenInited THEN 

      GlobalErrors.Init(GlobalErrorStatus)
      FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
      FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
      FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
      INIMgr.Init('.\AbcInitExe.INI', NVD_INI)                 ! Configure INIManager to use INI file
      DataAbc:Init(GlobalErrors, INIMgr)                       ! Initialise dll (ABC)
      ProcABC:Init(GlobalErrors, INIMgr)                       ! Initialise dll (ABC)
      AbcHasBeenInited=1 
      
    ELSIF KillMe AND AbcHasBeenInited
      
      AbcHasBeenInited=0
      INIMgr.Update
      DataAbc:Kill()                                           ! Kill dll (ABC)
      ProcABC:Kill()                                           ! Kill dll (ABC)
      INIMgr.Kill                                              ! Destroy INI manager
      FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher

    END
    RETURN 

    