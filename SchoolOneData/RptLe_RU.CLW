                     MEMBER('RptLegacy.clw')

HoldPosition         STRING(512),THREAD
Sav                  GROUP,THREAD
CLA:ClassNumber        LIKE(CLA:ClassNumber)
COU:Number             LIKE(COU:Number)
STU:Number             LIKE(STU:Number)
TEA:Number             LIKE(TEA:Number)
                     END


!--------------------------------------------------
RISnap:Classes      PROCEDURE
  CODE
  Sav.CLA:ClassNumber = CLA:ClassNumber

!--------------------------------------------------
RIUpdate:Classes     FUNCTION(BYTE FromForm)
  CODE
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  LOGOUT(2,Classes,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Update','Classes')
    DO RICloseFiles
    RETURN(1)
  END
  HoldPosition = POSITION(CLA:KeyClassNumber)
  PUT(Classes)
  IF ERRORCODE()
    RISaveError
    IF SaveErrorCode = RecordChangedErr THEN
      IF FromForm THEN
        StandardWarning(Warn:RIFormUpdateError)
      ELSE
        StandardWarning(Warn:RIUpdateError,'Record Changed by Another Station')
      END
      WATCH(Classes)
      REGET(CLA:KeyClassNumber,HoldPosition)
      DO RICloseFiles
      RETURN(2)
    ELSE
      StandardWarning(Warn:RIUpdateError,'Classes')
      DO RICloseFiles
      RETURN(1)
    END
  END
  IF Sav.CLA:ClassNumber <> CLA:ClassNumber
    IF RIUpdate:Classes:Enrollment()
      ROLLBACK
      ENR:ClassNumber = CLA:ClassNumber
      DO RICloseFiles
      RETURN(1)
    END
  END
  COMMIT
  DO RICloseFiles
  RETURN(0)
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Enrollment::Used -= 1
  IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
  EXIT

!--------------------------------------------------
RISnap:Courses      PROCEDURE
  CODE
  Sav.COU:Number = COU:Number

!--------------------------------------------------
RIUpdate:Courses     FUNCTION(BYTE FromForm)
  CODE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  LOGOUT(2,Courses,Classes,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Update','Courses')
    DO RICloseFiles
    RETURN(1)
  END
  HoldPosition = POSITION(COU:KeyNumber)
  PUT(Courses)
  IF ERRORCODE()
    RISaveError
    IF SaveErrorCode = RecordChangedErr THEN
      IF FromForm THEN
        StandardWarning(Warn:RIFormUpdateError)
      ELSE
        StandardWarning(Warn:RIUpdateError,'Record Changed by Another Station')
      END
      WATCH(Courses)
      REGET(COU:KeyNumber,HoldPosition)
      DO RICloseFiles
      RETURN(2)
    ELSE
      StandardWarning(Warn:RIUpdateError,'Courses')
      DO RICloseFiles
      RETURN(1)
    END
  END
  IF Sav.COU:Number <> COU:Number
    IF RIUpdate:Courses:Classes()
      ROLLBACK
      CLA:CourseNumber = COU:Number
      DO RICloseFiles
      RETURN(1)
    END
  END
  COMMIT
  DO RICloseFiles
  RETURN(0)
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Classes::Used -= 1
  IF Classes::Used = 0 THEN CLOSE(Classes).
  Enrollment::Used -= 1
  IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
  EXIT

!--------------------------------------------------
RISnap:Enrollment   PROCEDURE
  CODE

!--------------------------------------------------
RIUpdate:Enrollment  FUNCTION(BYTE FromForm)
  CODE
  LOGOUT(2,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Update','Enrollment')
    DO RICloseFiles
    RETURN(1)
  END
  HoldPosition = POSITION(Enrollment)
  PUT(Enrollment)
  IF ERRORCODE()
    RISaveError
    IF SaveErrorCode = RecordChangedErr THEN
      IF FromForm THEN
        StandardWarning(Warn:RIFormUpdateError)
      ELSE
        StandardWarning(Warn:RIUpdateError,'Record Changed by Another Station')
      END
      WATCH(Enrollment)
      REGET(Enrollment,HoldPosition)
      DO RICloseFiles
      RETURN(2)
    ELSE
      StandardWarning(Warn:RIUpdateError,'Enrollment')
      DO RICloseFiles
      RETURN(1)
    END
  END
  COMMIT
  DO RICloseFiles
  RETURN(0)
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  EXIT

!--------------------------------------------------
RISnap:Students     PROCEDURE
  CODE
  Sav.STU:Number = STU:Number

!--------------------------------------------------
RIUpdate:Students    FUNCTION(BYTE FromForm)
  CODE
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  LOGOUT(2,Students,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Update','Students')
    DO RICloseFiles
    RETURN(1)
  END
  HoldPosition = POSITION(STU:KeyStudentNumber)
  PUT(Students)
  IF ERRORCODE()
    RISaveError
    IF SaveErrorCode = RecordChangedErr THEN
      IF FromForm THEN
        StandardWarning(Warn:RIFormUpdateError)
      ELSE
        StandardWarning(Warn:RIUpdateError,'Record Changed by Another Station')
      END
      WATCH(Students)
      REGET(STU:KeyStudentNumber,HoldPosition)
      DO RICloseFiles
      RETURN(2)
    ELSE
      StandardWarning(Warn:RIUpdateError,'Students')
      DO RICloseFiles
      RETURN(1)
    END
  END
  IF Sav.STU:Number <> STU:Number
    IF RIUpdate:Students:Enrollment()
      ROLLBACK
      ENR:StudentNumber = STU:Number
      DO RICloseFiles
      RETURN(1)
    END
  END
  COMMIT
  DO RICloseFiles
  RETURN(0)
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Enrollment::Used -= 1
  IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
  EXIT

!--------------------------------------------------
RISnap:Teachers     PROCEDURE
  CODE
  Sav.TEA:Number = TEA:Number

!--------------------------------------------------
RIUpdate:Teachers    FUNCTION(BYTE FromForm)
  CODE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  LOGOUT(2,Teachers,Classes,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Update','Teachers')
    DO RICloseFiles
    RETURN(1)
  END
  HoldPosition = POSITION(TEA:KeyTeacherNumber)
  PUT(Teachers)
  IF ERRORCODE()
    RISaveError
    IF SaveErrorCode = RecordChangedErr THEN
      IF FromForm THEN
        StandardWarning(Warn:RIFormUpdateError)
      ELSE
        StandardWarning(Warn:RIUpdateError,'Record Changed by Another Station')
      END
      WATCH(Teachers)
      REGET(TEA:KeyTeacherNumber,HoldPosition)
      DO RICloseFiles
      RETURN(2)
    ELSE
      StandardWarning(Warn:RIUpdateError,'Teachers')
      DO RICloseFiles
      RETURN(1)
    END
  END
  IF Sav.TEA:Number <> TEA:Number
    IF RIUpdate:Teachers:Classes()
      ROLLBACK
      CLA:TeacherNumber = TEA:Number
      DO RICloseFiles
      RETURN(1)
    END
  END
  COMMIT
  DO RICloseFiles
  RETURN(0)
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Classes::Used -= 1
  IF Classes::Used = 0 THEN CLOSE(Classes).
  Enrollment::Used -= 1
  IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
  EXIT
!--------------------------------------------------
RIUpdate:Classes:Enrollment FUNCTION

    CODE
    CLEAR(ENR:Record,0)
    ENR:ClassNumber = Sav.CLA:ClassNumber
    CLEAR(ENR:StudentNumber,-1)
    SET(ENR:SeqStu,ENR:SeqStu)
    LOOP
      NEXT(Enrollment)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN(0)
        ELSE
          RISaveError
          StandardWarning(Warn:RecordFetchError,'Enrollment')
          RETURN(1)
        END
      END
      IF ENR:ClassNumber <> Sav.CLA:ClassNumber
        RETURN(0)
      END
      RISnap:Enrollment
      ENR:ClassNumber = CLA:ClassNumber
      PUT(Enrollment)
      IF ERRORCODE()
        RISaveError
        StandardWarning(Warn:RIUpdateError,'Enrollment')
        RETURN(1)
      END
    END
!--------------------------------------------------
RIUpdate:Courses:Classes FUNCTION

    CODE
    CLEAR(CLA:Record,0)
    CLA:CourseNumber = Sav.COU:Number
    CLEAR(CLA:ClassNumber,-1)
    SET(CLA:KeyCourseNumber,CLA:KeyCourseNumber)
    LOOP
      NEXT(Classes)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN(0)
        ELSE
          RISaveError
          StandardWarning(Warn:RecordFetchError,'Classes')
          RETURN(1)
        END
      END
      IF CLA:CourseNumber <> Sav.COU:Number
        RETURN(0)
      END
      RISnap:Classes
      CLA:CourseNumber = COU:Number
      PUT(Classes)
      IF ERRORCODE()
        RISaveError
        StandardWarning(Warn:RIUpdateError,'Classes')
        RETURN(1)
      END
    END
!--------------------------------------------------
RIUpdate:Students:Enrollment FUNCTION

    CODE
    CLEAR(ENR:Record,0)
    ENR:StudentNumber = Sav.STU:Number
    CLEAR(ENR:ClassNumber,-1)
    SET(ENR:StuSeq,ENR:StuSeq)
    LOOP
      NEXT(Enrollment)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN(0)
        ELSE
          RISaveError
          StandardWarning(Warn:RecordFetchError,'Enrollment')
          RETURN(1)
        END
      END
      IF ENR:StudentNumber <> Sav.STU:Number
        RETURN(0)
      END
      RISnap:Enrollment
      ENR:StudentNumber = STU:Number
      PUT(Enrollment)
      IF ERRORCODE()
        RISaveError
        StandardWarning(Warn:RIUpdateError,'Enrollment')
        RETURN(1)
      END
    END
!--------------------------------------------------
RIUpdate:Teachers:Classes FUNCTION

    CODE
    CLEAR(CLA:Record,0)
    CLA:TeacherNumber = Sav.TEA:Number
    SET(CLA:KeyTeacherNumber,CLA:KeyTeacherNumber)
    LOOP
      NEXT(Classes)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN(0)
        ELSE
          RISaveError
          StandardWarning(Warn:RecordFetchError,'Classes')
          RETURN(1)
        END
      END
      IF CLA:TeacherNumber <> Sav.TEA:Number
        RETURN(0)
      END
      RISnap:Classes
      CLA:TeacherNumber = TEA:Number
      PUT(Classes)
      IF ERRORCODE()
        RISaveError
        StandardWarning(Warn:RIUpdateError,'Classes')
        RETURN(1)
      END
    END
