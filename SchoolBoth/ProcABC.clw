   PROGRAM



   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE
  INCLUDE('prnprop.clw'),ONCE

   MAP
     MODULE('PROCABC001.CLW')
AbcClassTree           PROCEDURE   !
AbcBrowseClasses       PROCEDURE   !Browse the Classes File
AbcBrowseCourses       PROCEDURE   !Browse the Courses File
AbcBrowseEnrollment    PROCEDURE   !Browse the Enrollment File
AbcBrowseMajors        PROCEDURE   !Browse the Majors File
AbcBrowseStudents      PROCEDURE   !Browse the Students File
AbcBrowseTeachers      PROCEDURE   !Browse the Teachers File
AbcStudentTree         PROCEDURE   !
AbcUpdateGrades        PROCEDURE   !
     END
     MODULE('PROCABC002.CLW')
AbcAttendanceSheets    PROCEDURE   !
AbcClassSchedules1     PROCEDURE   !Student Schedules
AbcClassSchedules2     PROCEDURE   !Teacher Schedules
AbcCourseEnrollment    PROCEDURE   !
AbcEnrollSummary       PROCEDURE   !
AbcFinalGrades         PROCEDURE   !
AbcStudentIDs          PROCEDURE   !
     END
     MODULE('PROCABC003.CLW')
SplashIt               PROCEDURE   !
SelectClasses          PROCEDURE   !Select a Classes Record
SelectCourses          PROCEDURE   !Select a Courses Record
SelectMajors           PROCEDURE   !Select a Majors Record
SelectStudents         PROCEDURE   !Select a Students Record
SelectTeachers         PROCEDURE   !Select a Teachers Record
UpdateClasses          PROCEDURE   !Update the Classes File
UpdateCourses          PROCEDURE   !Update the Courses File
UpdateTeachers         PROCEDURE   !Update the Teachers File
UpdateStudents         PROCEDURE   !Update the Students File
UpdateMajors           PROCEDURE   !Update the Majors File
UpdateEnrollment       PROCEDURE   !Update the Enrollment File
MainABC                PROCEDURE   !Clarion for Windows Wizard Application
     END
    ! Declare functions defined in this DLL
ProcABC:Init           PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>)
ProcABC:Kill           PROCEDURE
    ! Declare init functions defined in a different dll
     MODULE('DATAABC.DLL')
DataABC:Init           PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>),DLL
DataABC:Kill           PROCEDURE,DLL
     END
   END

GLO:FileName         STRING(64)
AppFrameRef          &WINDOW
GLO:DropThread       LONG
GLO:DropControl      LONG
GLO:ThreadRef        &LONG
GLO:CameFrom         STRING(20)
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!region File Declaration
Students             FILE,DRIVER('TOPSPEED'),PRE(STU),CREATE,BINDABLE,THREAD,EXTERNAL(''),DLL(dll_mode) !                     
KeyStudentNumber         KEY(STU:Number),NOCASE,OPT,PRIMARY !                     
MajorKey                 KEY(STU:Major,STU:LastName,STU:FirstName),DUP,NOCASE,OPT !                     
KeyLastName              KEY(STU:LastName),DUP,NOCASE      !                     
KeyGradYear              KEY(-STU:GradYear,STU:LastName,STU:FirstName),DUP,NOCASE,OPT !                     
DynoKey                  INDEX,NOCASE                      !                     
Photograph                  BLOB                           !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
FirstName                   STRING(20)                     !                     
LastName                    STRING(20)                     !                     
Address                     STRING(20)                     !                     
Address2                    STRING(20)                     !                     
City                        STRING(20)                     !                     
State                       STRING(2)                      !                     
Zip                         LONG                           !                     
Telephone                   STRING(12)                     !                     
Major                       LONG                           !                     
GradYear                    LONG                           !                     
                         END
                     END                       

Teachers             FILE,DRIVER('TOPSPEED'),PRE(TEA),CREATE,BINDABLE,THREAD,EXTERNAL(''),DLL(dll_mode) !                     
KeyTeacherNumber         KEY(TEA:Number),NOCASE,OPT,PRIMARY !                     
KeyLastName              KEY(TEA:LastName),DUP,NOCASE      !                     
KeyDepartment            KEY(TEA:Department),DUP,NOCASE,OPT !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
FirstName                   STRING(20)                     !                     
LastName                    STRING(20)                     !                     
Address                     STRING(20)                     !                     
City                        STRING(20)                     !                     
State                       STRING(2)                      !                     
Zip                         LONG                           !                     
Telephone                   STRING(12)                     !                     
Department                  LONG                           !                     
                         END
                     END                       

Classes              FILE,DRIVER('TOPSPEED'),PRE(CLA),CREATE,BINDABLE,THREAD,EXTERNAL(''),DLL(dll_mode) !                     
KeyClassNumber           KEY(CLA:ClassNumber),NOCASE,OPT,PRIMARY !                     
KeyCourseNumber          KEY(CLA:CourseNumber,CLA:ClassNumber),DUP,NOCASE !                     
KeyTeacherNumber         KEY(CLA:TeacherNumber),DUP,NOCASE !                     
Record                   RECORD,PRE()
ClassNumber                 LONG                           !                     
CourseNumber                LONG                           !                     
TeacherNumber               LONG                           !                     
RoomNumber                  LONG                           !                     
ScheduledTime               STRING(20)                     !                     
                         END
                     END                       

Enrollment           FILE,DRIVER('TOPSPEED'),PRE(ENR),CREATE,BINDABLE,THREAD,EXTERNAL(''),DLL(dll_mode) !                     
StuSeq                   KEY(ENR:StudentNumber,ENR:ClassNumber),NOCASE,OPT !                     
SeqStu                   KEY(ENR:ClassNumber,ENR:StudentNumber),NOCASE,OPT !                     
Record                   RECORD,PRE()
StudentNumber               LONG                           !                     
ClassNumber                 LONG                           !                     
MidtermExam                 SHORT                          !                     
FinalExam                   SHORT                          !                     
TermPaper                   SHORT                          !                     
                         END
                     END                       

Courses              FILE,DRIVER('TOPSPEED'),PRE(COU),CREATE,BINDABLE,THREAD,EXTERNAL(''),DLL(dll_mode) !                     
KeyNumber                KEY(COU:Number),NOCASE,OPT,PRIMARY !                     
KeyDescription           KEY(COU:Description),DUP,NOCASE   !                     
CompleteDescription         MEMO(1000)                     !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(40)                     !                     
                         END
                     END                       

Majors               FILE,DRIVER('TOPSPEED'),PRE(MAJ),CREATE,BINDABLE,THREAD,EXTERNAL(''),DLL(dll_mode) !                     
KeyNumber                KEY(MAJ:Number),NOCASE,OPT,PRIMARY !                     
KeyDescription           KEY(MAJ:Description),NOCASE,OPT   !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(20)                     !                     
                         END
                     END                       

!endregion

!GlobalErrorStatus    ErrorStatusClass,THREAD
!GlobalErrors         ErrorClass                            ! Global error manager
Access:Students      &FileManager,THREAD,EXTERNAL,DLL      ! FileManager for Students
Relate:Students      &RelationManager,THREAD,EXTERNAL,DLL  ! RelationManager for Students
Access:Teachers      &FileManager,THREAD,EXTERNAL,DLL      ! FileManager for Teachers
Relate:Teachers      &RelationManager,THREAD,EXTERNAL,DLL  ! RelationManager for Teachers
Access:Classes       &FileManager,THREAD,EXTERNAL,DLL      ! FileManager for Classes
Relate:Classes       &RelationManager,THREAD,EXTERNAL,DLL  ! RelationManager for Classes
Access:Enrollment    &FileManager,THREAD,EXTERNAL,DLL      ! FileManager for Enrollment
Relate:Enrollment    &RelationManager,THREAD,EXTERNAL,DLL  ! RelationManager for Enrollment
Access:Courses       &FileManager,THREAD,EXTERNAL,DLL      ! FileManager for Courses
Relate:Courses       &RelationManager,THREAD,EXTERNAL,DLL  ! RelationManager for Courses
Access:Majors        &FileManager,THREAD,EXTERNAL,DLL      ! FileManager for Majors
Relate:Majors        &RelationManager,THREAD,EXTERNAL,DLL  ! RelationManager for Majors

GlobalRequest        BYTE,EXTERNAL,DLL,THREAD              ! Exported from a dll, set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE,EXTERNAL,DLL,THREAD              ! Exported from a dll, set to the response from the form
VCRRequest           LONG,EXTERNAL,DLL,THREAD              ! Exported from a dll, set to the request from the VCR buttons
FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
LocalErrorStatus     ErrorStatusClass,THREAD
LocalErrors          ErrorClass
LocalINIMgr          INIClass
GlobalErrors         &ErrorClass
INIMgr               &INIClass
DLLInitializer       CLASS                                 ! An object of this type is used to initialize the dll, it is created in the generated bc module
Construct              PROCEDURE
Destruct               PROCEDURE
                     END

  CODE
DLLInitializer.Construct PROCEDURE


  CODE
  LocalErrors.Init(LocalErrorStatus)
  LocalINIMgr.Init('.\ProcABC.INI', NVD_INI)               ! Initialize the local INI manager to use windows INI file
  INIMgr &= LocalINIMgr
  IF GlobalErrors &= NULL
    GlobalErrors &= LocalErrors                            ! Assign local managers to global managers
  END
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  
!These procedures are used to initialize the DLL. It must be called by the main executable when it starts up
ProcABC:Init PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>)
ProcABC:Init_Called    BYTE,STATIC

  CODE
  IF ProcABC:Init_Called
     RETURN
  ELSE
     ProcABC:Init_Called = True
  END
  IF ~curGlobalErrors &= NULL
    GlobalErrors &= curGlobalErrors
  END
  IF ~curINIMgr &= NULL
    INIMgr &= curINIMgr
  END
  Access:Students.SetErrors(GlobalErrors)
  Access:Teachers.SetErrors(GlobalErrors)
  Access:Classes.SetErrors(GlobalErrors)
  Access:Enrollment.SetErrors(GlobalErrors)
  Access:Courses.SetErrors(GlobalErrors)
  Access:Majors.SetErrors(GlobalErrors)
  DataABC:Init(curGlobalErrors, curINIMgr)                 ! Initialise dll - (ABC) -

!This procedure is used to shutdown the DLL. It must be called by the main executable before it closes down

ProcABC:Kill PROCEDURE
ProcABC:Kill_Called    BYTE,STATIC

  CODE
  IF ProcABC:Kill_Called
     RETURN
  ELSE
     ProcABC:Kill_Called = True
  END
  DataABC:Kill()                                           ! Kill dll - (ABC) -
  

DLLInitializer.Destruct PROCEDURE

  CODE
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher
  LocalINIMgr.Kill                                         ! Kill local managers and assign NULL to global refernces
  INIMgr &= NULL                                           ! It is an error to reference these object after this point
  GlobalErrors &= NULL


