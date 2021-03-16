                     MEMBER('ProcLegacy.clw')


!--------------------------------------------------
RIDelete:Classes     FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(CLA:KeyClassNumber)
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  LOGOUT(2,Classes,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Delete','Classes')
    DO RICloseFiles
    RETURN(1)
  END
  REGET(CLA:KeyClassNumber,Current:Position)
  IF RIDelete:Classes:Enrollment()
    ROLLBACK
    DO RICloseFiles
    RETURN(1)
  END
  DELETE(Classes)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'Classes')
    DO RICloseFiles
    RETURN(1)
  ELSE
    COMMIT
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Enrollment::Used -= 1
  IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
  EXIT

!--------------------------------------------------
RIDelete:Courses     FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(COU:KeyNumber)
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  LOGOUT(2,Courses,Classes)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Delete','Courses')
    DO RICloseFiles
    RETURN(1)
  END
  REGET(COU:KeyNumber,Current:Position)
  IF RIDelete:Courses:Classes()
    ROLLBACK
    DO RICloseFiles
    RETURN(1)
  END
  DELETE(Courses)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'Courses')
    DO RICloseFiles
    RETURN(1)
  ELSE
    COMMIT
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Classes::Used -= 1
  IF Classes::Used = 0 THEN CLOSE(Classes).
  EXIT

!--------------------------------------------------
RIDelete:Enrollment  FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(ENR:StuSeq)
  LOGOUT(2,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Delete','Enrollment')
    DO RICloseFiles
    RETURN(1)
  END
  REGET(ENR:StuSeq,Current:Position)
  DELETE(Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'Enrollment')
    DO RICloseFiles
    RETURN(1)
  ELSE
    COMMIT
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  EXIT

!--------------------------------------------------
RIDelete:Majors      FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(MAJ:KeyNumber)
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  LOGOUT(2,Majors,Students,Teachers)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Delete','Majors')
    DO RICloseFiles
    RETURN(1)
  END
  REGET(MAJ:KeyNumber,Current:Position)
  IF RIDelete:Majors:Students()
    ROLLBACK
    DO RICloseFiles
    RETURN(1)
  END
  IF RIDelete:Majors:Teachers()
    ROLLBACK
    DO RICloseFiles
    RETURN(1)
  END
  DELETE(Majors)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'Majors')
    DO RICloseFiles
    RETURN(1)
  ELSE
    COMMIT
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Students::Used -= 1
  IF Students::Used = 0 THEN CLOSE(Students).
  Teachers::Used -= 1
  IF Teachers::Used = 0 THEN CLOSE(Teachers).
  EXIT

!--------------------------------------------------
RIDelete:Students    FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(STU:KeyStudentNumber)
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  LOGOUT(2,Students,Enrollment)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Delete','Students')
    DO RICloseFiles
    RETURN(1)
  END
  REGET(STU:KeyStudentNumber,Current:Position)
  IF RIDelete:Students:Enrollment()
    ROLLBACK
    DO RICloseFiles
    RETURN(1)
  END
  DELETE(Students)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'Students')
    DO RICloseFiles
    RETURN(1)
  ELSE
    COMMIT
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Enrollment::Used -= 1
  IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
  EXIT

!--------------------------------------------------
RIDelete:Teachers    FUNCTION
Current:Position     STRING(512)
  CODE
  Current:Position = POSITION(TEA:KeyTeacherNumber)
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  LOGOUT(2,Teachers,Classes)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:LogoutError,'Delete','Teachers')
    DO RICloseFiles
    RETURN(1)
  END
  REGET(TEA:KeyTeacherNumber,Current:Position)
  IF RIDelete:Teachers:Classes()
    ROLLBACK
    DO RICloseFiles
    RETURN(1)
  END
  DELETE(Teachers)
  IF ERRORCODE()
    RISaveError
    StandardWarning(Warn:RIDeleteError,'Teachers')
    DO RICloseFiles
    RETURN(1)
  ELSE
    COMMIT
    DO RICloseFiles
    RETURN(0)
  END
!----------------------------------------------------------------------
RICloseFiles ROUTINE
!|
!| This routine is called to close any files opened durint RI processing
!|
  Classes::Used -= 1
  IF Classes::Used = 0 THEN CLOSE(Classes).
  EXIT

!--------------------------------------------------
RIDelete:Classes:Enrollment FUNCTION
    CODE
    CLEAR(ENR:Record,0)
    ENR:ClassNumber = CLA:ClassNumber
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
      IF CLA:ClassNumber <> ENR:ClassNumber
        RETURN(0)
      END
      RISaveError
      StandardWarning(Warn:RestrictDelete,'Enrollment')
      RETURN(1)
    END

!--------------------------------------------------
RIDelete:Courses:Classes FUNCTION
    CODE
    CLEAR(CLA:Record,0)
    CLA:CourseNumber = COU:Number
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
      IF COU:Number <> CLA:CourseNumber
        RETURN(0)
      END
      RISaveError
      StandardWarning(Warn:RestrictDelete,'Classes')
      RETURN(1)
    END

!--------------------------------------------------
RIDelete:Majors:Students FUNCTION
    CODE
    CLEAR(STU:Record,0)
    STU:Major = MAJ:Number
    CLEAR(STU:LastName,-1)
    CLEAR(STU:FirstName,-1)
    SET(STU:MajorKey,STU:MajorKey)
    LOOP
      NEXT(Students)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN(0)
        ELSE
          RISaveError
          StandardWarning(Warn:RecordFetchError,'Students')
          RETURN(1)
        END
      END
      IF MAJ:Number <> STU:Major
        RETURN(0)
      END
      RISaveError
      StandardWarning(Warn:RestrictDelete,'Students')
      RETURN(1)
    END

!--------------------------------------------------
RIDelete:Majors:Teachers FUNCTION
    CODE
    CLEAR(TEA:Record,0)
    TEA:Department = MAJ:Number
    SET(TEA:KeyDepartment,TEA:KeyDepartment)
    LOOP
      NEXT(Teachers)
      IF ERRORCODE()
        IF ERRORCODE() = BadRecErr
          RETURN(0)
        ELSE
          RISaveError
          StandardWarning(Warn:RecordFetchError,'Teachers')
          RETURN(1)
        END
      END
      IF MAJ:Number <> TEA:Department
        RETURN(0)
      END
      RISaveError
      StandardWarning(Warn:RestrictDelete,'Teachers')
      RETURN(1)
    END

!--------------------------------------------------
RIDelete:Students:Enrollment FUNCTION
    CODE
    CLEAR(ENR:Record,0)
    ENR:StudentNumber = STU:Number
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
      IF STU:Number <> ENR:StudentNumber
        RETURN(0)
      END
      RISaveError
      StandardWarning(Warn:RestrictDelete,'Enrollment')
      RETURN(1)
    END

!--------------------------------------------------
RIDelete:Teachers:Classes FUNCTION
    CODE
    CLEAR(CLA:Record,0)
    CLA:TeacherNumber = TEA:Number
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
      IF TEA:Number <> CLA:TeacherNumber
        RETURN(0)
      END
      RISaveError
      StandardWarning(Warn:RestrictDelete,'Classes')
      RETURN(1)
    END
