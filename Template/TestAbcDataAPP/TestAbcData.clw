   PROGRAM



   INCLUDE('ABASCII.INC'),ONCE
   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABDOCK.INC'),ONCE
   INCLUDE('ABDROPS.INC'),ONCE
   INCLUDE('ABEIP.INC'),ONCE
   INCLUDE('ABERROR.INC'),ONCE
   INCLUDE('ABFILE.INC'),ONCE
   INCLUDE('ABPOPUP.INC'),ONCE
   INCLUDE('ABPRHTML.INC'),ONCE
   INCLUDE('ABPRPDF.INC'),ONCE
   INCLUDE('ABQUERY.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE
   INCLUDE('ABRESIZE.INC'),ONCE
   INCLUDE('ABTOOLBA.INC'),ONCE
   INCLUDE('ABTBLSYN.INC'),ONCE
   INCLUDE('ABUTIL.INC'),ONCE
   INCLUDE('ABUSERCONTROL.INC'),ONCE
   INCLUDE('ABWINDOW.INC'),ONCE
   INCLUDE('ABWMFPAR.INC'),ONCE
   INCLUDE('CSIDLFOLDER.INC'),ONCE
   INCLUDE('CWSYNCWT.INC'),ONCE
   INCLUDE('CLAAWSS3.INC'),ONCE
   INCLUDE('CLAMAIL.INC'),ONCE
   INCLUDE('CLARUNEXT.INC'),ONCE
   INCLUDE('ERRORS.CLW'),ONCE
   INCLUDE('JSON.INC'),ONCE
   INCLUDE('KEYCODES.CLW'),ONCE
   INCLUDE('QUICKWINTRANSLATOR.INC'),ONCE
   INCLUDE('SPECIALFOLDER.INC'),ONCE
   INCLUDE('SYSTEMSTRING.INC'),ONCE
   INCLUDE('ABBREAK.INC'),ONCE
   INCLUDE('ABCPTHD.INC'),ONCE
   INCLUDE('ABFUZZY.INC'),ONCE
   INCLUDE('ABGRID.INC'),ONCE
   INCLUDE('ABPRNAME.INC'),ONCE
   INCLUDE('ABPRTARG.INC'),ONCE
   INCLUDE('ABPRTARY.INC'),ONCE
   INCLUDE('ABPRTEXT.INC'),ONCE
   INCLUDE('ABPRXML.INC'),ONCE
   INCLUDE('ABQEIP.INC'),ONCE
   INCLUDE('ABRPATMG.INC'),ONCE
   INCLUDE('ABRPPSEL.INC'),ONCE
   INCLUDE('ABRULE.INC'),ONCE
   INCLUDE('ABSQL.INC'),ONCE
   INCLUDE('ABVCRFRM.INC'),ONCE
   INCLUDE('CFILTBASE.INC'),ONCE
   INCLUDE('CFILTERLIST.INC'),ONCE
   INCLUDE('CWSYNCHC.INC'),ONCE
   INCLUDE('MDISYNC.INC'),ONCE
   INCLUDE('QPROCESS.INC'),ONCE
   INCLUDE('RTFCTL.INC'),ONCE
   INCLUDE('TRIGGER.INC'),ONCE
   INCLUDE('WINEXT.INC'),ONCE

   MAP
     MODULE('TESTABCDATA_BC.CLW')
DctInit     PROCEDURE                                      ! Initializes the dictionary definition module
DctKill     PROCEDURE                                      ! Kills the dictionary definition module
     END
     INCLUDE('CWUtil.INC')
    ! Declare functions defined in this DLL
TestAbcData:Init       PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>)
TestAbcData:Kill       PROCEDURE
   END

Glo1::MyQueue        QUEUE,PRE(MyQQ)
QField                 STRING(40)
                     END
SilentRunning        BYTE(0)                               ! Set true when application is running in 'silent mode'

!Region CBAbc4Legacy File::Used Counters
Students::Used LONG
Teachers::Used LONG
Classes::Used LONG
Enrollment::Used LONG
Courses::Used LONG
Majors::Used LONG
!endregion  
!region File Declaration
Students             FILE,DRIVER('TOPSPEED'),PRE(STU),CREATE,BINDABLE,THREAD !                     
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

Teachers             FILE,DRIVER('TOPSPEED'),PRE(TEA),CREATE,BINDABLE,THREAD !                     
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

Classes              FILE,DRIVER('TOPSPEED'),PRE(CLA),CREATE,BINDABLE,THREAD !                     
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

Enrollment           FILE,DRIVER('TOPSPEED'),PRE(ENR),CREATE,BINDABLE,THREAD !                     
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

Courses              FILE,DRIVER('TOPSPEED'),PRE(COU),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(COU:Number),NOCASE,OPT,PRIMARY !                     
KeyDescription           KEY(COU:Description),DUP,NOCASE   !                     
CompleteDescription         MEMO(1000)                     !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(40)                     !                     
                         END
                     END                       

Majors               FILE,DRIVER('TOPSPEED'),NAME('majors.tps'),PRE(MAJ),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(MAJ:Number),NOCASE,OPT,PRIMARY !                     
KeyDescription           KEY(MAJ:Description),NOCASE,OPT   !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(20)                     !                     
                         END
                     END                       

!endregion

Access:Students      &FileManager,THREAD                   ! FileManager for Students
Relate:Students      &RelationManager,THREAD               ! RelationManager for Students
Access:Teachers      &FileManager,THREAD                   ! FileManager for Teachers
Relate:Teachers      &RelationManager,THREAD               ! RelationManager for Teachers
Access:Classes       &FileManager,THREAD                   ! FileManager for Classes
Relate:Classes       &RelationManager,THREAD               ! RelationManager for Classes
Access:Enrollment    &FileManager,THREAD                   ! FileManager for Enrollment
Relate:Enrollment    &RelationManager,THREAD               ! RelationManager for Enrollment
Access:Courses       &FileManager,THREAD                   ! FileManager for Courses
Relate:Courses       &RelationManager,THREAD               ! RelationManager for Courses
Access:Majors        &FileManager,THREAD                   ! FileManager for Majors
Relate:Majors        &RelationManager,THREAD               ! RelationManager for Majors

GlobalRequest        BYTE(0),THREAD                        ! Set when a browse calls a form, to let it know action to perform
GlobalResponse       BYTE(0),THREAD                        ! Set to the response from the form
VCRRequest           LONG(0),THREAD                        ! Set to the request from the VCR buttons
FuzzyMatcher         FuzzyClass                            ! Global fuzzy matcher
LocalErrorStatus     ErrorStatusClass,THREAD
LocalErrors          ErrorClass
LocalINIMgr          INIClass
GlobalErrors         &ErrorClass
INIMgr               &INIClass
DLLInitializer       CLASS,TYPE                            ! An object of this type is used to initialize the dll, it is created in the generated bc module
Construct              PROCEDURE
Destruct               PROCEDURE
                     END

Dictionary           CLASS,THREAD
Construct              PROCEDURE
Destruct               PROCEDURE
                     END


  CODE
DLLInitializer.Construct PROCEDURE


  CODE
  LocalErrors.Init(LocalErrorStatus)
  LocalINIMgr.Init('.\TestAbcData.INI', NVD_INI)           ! Initialize the local INI manager to use windows INI file
  INIMgr &= LocalINIMgr
  IF GlobalErrors &= NULL
    GlobalErrors &= LocalErrors                            ! Assign local managers to global managers
  END
  DctInit()
  FuzzyMatcher.Init                                        ! Initilaize the browse 'fuzzy matcher'
  FuzzyMatcher.SetOption(MatchOption:NoCase, 1)            ! Configure case matching
  FuzzyMatcher.SetOption(MatchOption:WordOnly, 0)          ! Configure 'word only' matching
  
!These procedures are used to initialize the DLL. It must be called by the main executable when it starts up
TestAbcData:Init PROCEDURE(<ErrorClass curGlobalErrors>, <INIClass curINIMgr>)
TestAbcData:Init_Called    BYTE,STATIC

  CODE
  IF TestAbcData:Init_Called
     RETURN
  ELSE
     TestAbcData:Init_Called = True
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

!This procedure is used to shutdown the DLL. It must be called by the main executable before it closes down

TestAbcData:Kill PROCEDURE
TestAbcData:Kill_Called    BYTE,STATIC

  CODE
  IF TestAbcData:Kill_Called
     RETURN
  ELSE
     TestAbcData:Kill_Called = True
  END
  

DLLInitializer.Destruct PROCEDURE

  CODE
  FuzzyMatcher.Kill                                        ! Destroy fuzzy matcher
  LocalINIMgr.Kill                                         ! Kill local managers and assign NULL to global refernces
  INIMgr &= NULL                                           ! It is an error to reference these object after this point
  GlobalErrors &= NULL



Dictionary.Construct PROCEDURE

  CODE
  IF THREAD()<>1
     DctInit()
  END


Dictionary.Destruct PROCEDURE

  CODE
  DctKill()

