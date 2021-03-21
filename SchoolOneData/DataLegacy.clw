   PROGRAM

   INCLUDE('Equates.CLW')
   INCLUDE('TplEqu.CLW')
   INCLUDE('Keycodes.CLW')
   INCLUDE('Errors.CLW')
   MAP
     MODULE('DataLegacy001.clw')
       AppRoot
     END
     MODULE('DataL_SF.CLW')
       CheckOpen(FILE File,<BYTE OverrideCreate>,<BYTE OverrideOpenMode>)
       ReportPreview(QUEUE PrintPreviewQueue)
       Preview:JumpToPage(LONG Input:CurrentPage, LONG Input:TotalPages),LONG
       Preview:SelectDisplay(*LONG Input:PagesAcross, *LONG Input:PagesDown)
       StandardWarning(LONG WarningID),LONG,PROC
       StandardWarning(LONG WarningID,STRING WarningText1),LONG,PROC
       StandardWarning(LONG WarningID,STRING WarningText1,STRING WarningText2),LONG,PROC
       SetupStringStops(STRING ProcessLowLimit,STRING ProcessHighLimit,LONG InputStringSize,<LONG ListType>)
       NextStringStop,STRING
       SetupRealStops(REAL InputLowLimit,REAL InputHighLimit)
       NextRealStop,REAL
       INIRestoreWindow(STRING ProcedureName,STRING INIFileName)
       INISaveWindow(STRING ProcedureName,STRING INIFileName)
       RISaveError
     END
     MODULE('DataL_RU.CLW')
       RIUpdate:Classes(BYTE=0),LONG
       RISnap:Classes
       RIUpdate:Courses(BYTE=0),LONG
       RISnap:Courses
       RIUpdate:Enrollment(BYTE=0),LONG
       RISnap:Enrollment
       RIUpdate:Majors(BYTE=0),LONG
       RISnap:Majors
       RIUpdate:Students(BYTE=0),LONG
       RISnap:Students
       RIUpdate:Teachers(BYTE=0),LONG
       RISnap:Teachers
       RIUpdate:Classes:Enrollment,LONG
       RIUpdate:Courses:Classes,LONG
       RIUpdate:Majors:Students,LONG
       RIUpdate:Majors:Teachers,LONG
       RIUpdate:Students:Enrollment,LONG
       RIUpdate:Teachers:Classes,LONG
     END
     MODULE('DataL_RD.CLW')
       RIDelete:Classes,LONG
       RIDelete:Courses,LONG
       RIDelete:Enrollment,LONG
       RIDelete:Majors,LONG
       RIDelete:Students,LONG
       RIDelete:Teachers,LONG
       RIDelete:Classes:Enrollment,LONG,PRIVATE
       RIDelete:Courses:Classes,LONG,PRIVATE
       RIDelete:Majors:Students,LONG,PRIVATE
       RIDelete:Majors:Teachers,LONG,PRIVATE
       RIDelete:Students:Enrollment,LONG,PRIVATE
       RIDelete:Teachers:Classes,LONG,PRIVATE
     END
   END


SaveErrorCode        LONG
SaveError            CSTRING(255)
SaveFileErrorCode    CSTRING(255)
SaveFileError        CSTRING(255)
GlobalRequest        LONG(0),THREAD
GlobalResponse       LONG(0),THREAD
VCRRequest           LONG(0),THREAD
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
Students::Used       LONG,THREAD

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
Teachers::Used       LONG,THREAD

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
Classes::Used        LONG,THREAD

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
Enrollment::Used     LONG,THREAD

Courses              FILE,DRIVER('TOPSPEED'),PRE(COU),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(COU:Number),NOCASE,OPT,PRIMARY !                     
KeyDescription           KEY(COU:Description),DUP,NOCASE   !                     
CompleteDescription         MEMO(1000)                     !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(40)                     !                     
                         END
                     END                       
Courses::Used        LONG,THREAD

Majors               FILE,DRIVER('TOPSPEED'),NAME('majors.tps'),PRE(MAJ),CREATE,BINDABLE,THREAD !                     
KeyNumber                KEY(MAJ:Number),NOCASE,OPT,PRIMARY !                     
KeyDescription           KEY(MAJ:Description),NOCASE,OPT   !                     
Record                   RECORD,PRE()
Number                      LONG                           !                     
Description                 STRING(20)                     !                     
                         END
                     END                       
Majors::Used         LONG,THREAD

!endregion

Sort:Name            STRING(ScrollSort:Name)
Sort:Name:Array      STRING(3),DIM(100),OVER(Sort:Name)
Sort:Alpha           STRING(ScrollSort:Alpha)
Sort:Alpha:Array     STRING(2),DIM(100),OVER(Sort:Alpha)


  CODE
  AppRoot
!---------------------------------------------------------------------------
