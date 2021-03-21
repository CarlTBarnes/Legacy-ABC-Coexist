

   MEMBER('ProcABC.clw')                                   ! This is a MEMBER module


   INCLUDE('ABBROWSE.INC'),ONCE
   INCLUDE('ABREPORT.INC'),ONCE

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AbcAttendanceSheets PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                       JOIN(CLA:KeyCourseNumber,COU:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:ClassNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                           PROJECT(TEA:FirstName)
                           PROJECT(TEA:LastName)
                         END
                         JOIN(ENR:SeqStu,CLA:ClassNumber)
                           PROJECT(ENR:StudentNumber)
                           JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                             PROJECT(STU:FirstName)
                             PROJECT(STU:LastName)
                           END
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1010,1417,6500,7438),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(990,1000,6500,406)
                         STRING('Attendance'),AT(2448,52),USE(?String17),FONT(,18,COLOR:Black,FONT:italic)
                       END
DescriptionBREAK       BREAK(COU:Description)
ClassBREAK               BREAK(CLA:ClassNumber)
                           HEADER,AT(0,0,,1594),FONT('Arial',10,COLOR:Black,FONT:bold)
                             STRING(@s40),AT(10,10),USE(COU:Description),FONT(,18,COLOR:Black)
                             GROUP('Class'),AT(1010,333,1594,760),BOXED
                               STRING(@n4),AT(1844,563),USE(CLA:RoomNumber)
                               STRING('Room'),AT(1156,563),USE(?String25)
                               STRING(@s20),AT(1156,802,1094),USE(CLA:ScheduledTime)
                             END
                             GROUP('Teacher'),AT(4458,333,1594,760),BOXED
                               STRING(@s20),AT(4521,802),USE(TEA:LastName)
                               STRING(@s20),AT(4521,563),USE(TEA:FirstName)
                             END
                             LINE,AT(1021,1479,5500,0)
                             STRING('Student Number'),AT(3531,1198)
                             STRING('Last Name'),AT(1063,1208),USE(?String15)
                             STRING('First Name'),AT(2188,1208),USE(?String16)
                           END
Detail1                    DETAIL,AT(,,,250),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                             STRING(@P###-##-####P),AT(3656,21),USE(ENR:StudentNumber)
                             STRING(@S20),AT(2094,10),USE(STU:FirstName)
                             STRING(@s20),AT(1042,10,948),USE(STU:LastName)
                           END
                           FOOTER,AT(0,0,,240),PAGEAFTER(-1),WITHPRIOR(1)
                           END
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcAttendanceSheets')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open                                      ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Courses, ?Progress:PctText, Progress:Thermometer, 3000)
  ThisReport.AddSortOrder(COU:KeyDescription)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Courses.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Student Schedules
!!! </summary>
AbcClassSchedules1 PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Enrollment)
                       PROJECT(ENR:ClassNumber)
                       PROJECT(ENR:StudentNumber)
                       JOIN(CLA:KeyClassNumber,ENR:ClassNumber)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:CourseNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                         END
                         JOIN(COU:KeyNumber,CLA:CourseNumber)
                           PROJECT(COU:Description)
                         END
                       END
                       JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                         PROJECT(STU:FirstName)
                         PROJECT(STU:LastName)
                         PROJECT(STU:Number)
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

report               REPORT,AT(1000,1552,6000,7448),PRE(RPT),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6000,552)
                         STRING('Student Schedule'),AT(0,281,6000,220),FONT(,,COLOR:Black,FONT:bold),CENTER
                       END
ENR:StudentNumberBreak BREAK(ENR:StudentNumber)
                         HEADER,AT(0,0,5990,677)
                           STRING(@P###-##-####P),AT(3406,63),USE(STU:Number),FONT(,,COLOR:Black,FONT:bold),RIGHT(1)
                           STRING('Room'),AT(2875,292),USE(?String9),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Time'),AT(3688,292),USE(?String10),FONT(,,COLOR:Black,FONT:bold)
                           LINE,AT(50,560,5900,0),USE(?Line2),COLOR(COLOR:Black)
                           STRING('Class'),AT(479,292),USE(?String11),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@S20),AT(83,63),USE(STU:FirstName),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@S20),AT(1760,63),USE(STU:LastName),FONT(,,COLOR:Black,FONT:bold)
                         END
detail                   DETAIL,AT(-10,10,6000,302),USE(?detail)
                           STRING(@S30),AT(250,42),USE(COU:Description)
                           STRING(@n4),AT(2875,42),USE(CLA:RoomNumber)
                           STRING(@s20),AT(3688,42),USE(CLA:ScheduledTime)
                         END
                         FOOTER,AT(0,0,,323),PAGEAFTER(-1)
                         END
                       END
                       FOOTER,AT(1000,9000,6000,219)
                         STRING(@pPage <<<#p),AT(5250,30,700,135),USE(?PageCount),FONT('Arial',8,COLOR:Black,FONT:regular), |
  PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepLongClass                         ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcClassSchedules1')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Enrollment.SetOpenRelated()
  Relate:Enrollment.Open                                   ! File Enrollment used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowNumeric,)
  ThisReport.Init(Process:View, Relate:Enrollment, ?Progress:PctText, Progress:Thermometer, ProgressMgr, ENR:StudentNumber)
  ThisReport.AddSortOrder(ENR:StuSeq)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Enrollment.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Enrollment.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! Teacher Schedules
!!! </summary>
AbcClassSchedules2 PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Teachers)
                       PROJECT(TEA:FirstName)
                       PROJECT(TEA:LastName)
                       PROJECT(TEA:Number)
                       JOIN(CLA:KeyTeacherNumber,TEA:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:CourseNumber)
                         JOIN(COU:KeyNumber,CLA:CourseNumber)
                           PROJECT(COU:Description)
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

report               REPORT,AT(1000,1552,6000,7448),PRE(RPT),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6000,552)
                         STRING('Teacher''s Schedule'),AT(0,281,6000,220),FONT(,,COLOR:Black,FONT:bold),CENTER
                       END
Tea:NumberBreak        BREAK(TEA:Number)
                         HEADER,AT(0,0,,823)
                           STRING(@S20),AT(125,115),USE(TEA:FirstName)
                           STRING(@S20),AT(1802,135),USE(TEA:LastName)
                           STRING(@P###-##-####P),AT(3583,156),USE(TEA:Number),RIGHT(1)
                           STRING('Class'),AT(385,490),USE(?String9),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Time'),AT(3708,490),USE(?String11),FONT(,,COLOR:Black,FONT:bold)
                           LINE,AT(167,708,5563,0),COLOR(COLOR:Black)
                           STRING('Room'),AT(2802,490),USE(?String10),FONT(,,COLOR:Black,FONT:bold)
                         END
detail                   DETAIL,AT(-10,10,6000,302),USE(?detail)
                           STRING(@S30),AT(250,42),USE(COU:Description)
                           STRING(@n4),AT(2875,42),USE(CLA:RoomNumber)
                           STRING(@s20),AT(3688,42),USE(CLA:ScheduledTime)
                         END
                         FOOTER,AT(0,0,,208),PAGEAFTER(-1)
                         END
                       END
                       FOOTER,AT(1000,9000,6000,219)
                         STRING(@pPage <<<#p),AT(5250,30,700,135),USE(?PageCount),FONT('Arial',8,COLOR:Black,FONT:regular), |
  PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcClassSchedules2')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Teachers.SetOpenRelated()
  Relate:Teachers.Open                                     ! File Teachers used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Teachers, ?Progress:PctText, Progress:Thermometer, ProgressMgr, TEA:LastName)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(TEA:KeyLastName)
  ThisReport.SetFilter('CLA:TeacherNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Teachers.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Teachers.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:detail)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AbcCourseEnrollment PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
FinalLetterGrade     STRING(1)                             ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses)
                       PROJECT(COU:CompleteDescription)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                       JOIN(CLA:KeyCourseNumber,COU:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:ClassNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                           PROJECT(TEA:FirstName)
                           PROJECT(TEA:LastName)
                         END
                         JOIN(ENR:SeqStu,CLA:ClassNumber)
                           PROJECT(ENR:FinalExam)
                           PROJECT(ENR:MidtermExam)
                           PROJECT(ENR:StudentNumber)
                           PROJECT(ENR:TermPaper)
                           JOIN(STU:KeyStudentNumber,ENR:StudentNumber)
                             PROJECT(STU:LastName)
                           END
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,1000,6500,7833),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
REPORTBrk              BREAK(LocalRequest)
                         HEADER,AT(0,0),ALONE
                           STRING('ABC Enrollment Report'),AT(1948,2552,2958,406),FONT('Times New Roman',20,COLOR:Black, |
  FONT:bold+FONT:italic),CENTER
                         END
DescriptionBREAK         BREAK(COU:Description)
                           HEADER,AT(0,0,,833),FONT('Arial',10,COLOR:Black,FONT:bold),PAGEBEFORE(-1)
                             STRING(@s40),AT(0,0),USE(COU:Description),FONT(,18,COLOR:Black)
                             TEXT,AT(781,229,5635,573),USE(COU:CompleteDescription),BOXED,COLOR(COLOR:Silver),RESIZE
                           END
ClassBREAK                 BREAK(CLA:ClassNumber)
                             HEADER,AT(0,0,,1313),FONT('Arial',10,COLOR:Black,FONT:bold)
                               GROUP('Class'),AT(1010,94,1594,760),BOXED
                                 STRING(@n4),AT(1844,323),USE(CLA:RoomNumber)
                                 STRING('Room'),AT(1156,323),USE(?String25)
                                 STRING(@s20),AT(1156,563,1094),USE(CLA:ScheduledTime)
                               END
                               GROUP('Teacher'),AT(4458,94,1594,760),BOXED
                                 STRING(@s20),AT(4521,563),USE(TEA:LastName)
                                 STRING(@s20),AT(4521,323),USE(TEA:FirstName)
                               END
                               LINE,AT(1021,1219,5500,0)
                               STRING('Midterm Exam'),AT(2323,958)
                               STRING('Final Exam'),AT(3479,958)
                               STRING('Term Paper'),AT(4427,958)
                               STRING('Final Grade'),AT(5417,958)
                               STRING('Student Number'),AT(1063,958)
                             END
Detail1                      DETAIL,AT(,,,250),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                               STRING(@n-13),AT(1052,0,1073),USE(ENR:StudentNumber)
                               STRING(@n-6),AT(2323,0,958),USE(ENR:MidtermExam)
                               STRING(@n-6),AT(3479,0,750),USE(ENR:FinalExam)
                               STRING(@n-6),AT(4427,0,781),USE(ENR:TermPaper)
                               STRING(@s20),AT(31,0,948),USE(STU:LastName)
                               STRING(@n5.2),AT(5417,0,740),USE(FinalGrade)
                               STRING(@s1),AT(5938,10),USE(FinalLetterGrade)
                               LINE,AT(10,208,6490,0)
                             END
                             FOOTER,AT(0,0,,458),WITHPRIOR(1)
                               STRING(@n5),AT(156,125),USE(Cnt_StudentNumber_2),CNT,RESET(ClassBREAK)
                               STRING('Students Enrolled in this Class'),AT(708,125)
                             END
                           END
                           FOOTER,AT(0,0,,448),WITHPRIOR(1)
                             STRING(@n5),AT(156,104),USE(Cnt_StudentNumber),CNT,RESET(DescriptionBREAK)
                             STRING('Students Enrolled in this Course'),AT(708,104)
                           END
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcCourseEnrollment')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open                                      ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Courses, ?Progress:PctText, Progress:Thermometer, ProgressMgr, COU:Description)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(COU:KeyDescription)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Courses.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  FinalGrade = (ENR:MidtermExam + (ENR:FinalExam * 2) + ENR:TermPaper) / 4
  CASE (INT(FinalGrade / 10))
  OF 9
    FinalLetterGrade = 'A'
  OF 8
    FinalLetterGrade = 'B'
  OF 7
    FinalLetterGrade = 'C'
  OF 6
    FinalLetterGrade = 'D'
  ELSE
    FinalLetterGrade = 'F'
  END
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AbcEnrollSummary PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Courses)
                       PROJECT(COU:Description)
                       PROJECT(COU:Number)
                       JOIN(CLA:KeyCourseNumber,COU:Number)
                         PROJECT(CLA:RoomNumber)
                         PROJECT(CLA:ScheduledTime)
                         PROJECT(CLA:TeacherNumber)
                         PROJECT(CLA:ClassNumber)
                         JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                           PROJECT(TEA:FirstName)
                           PROJECT(TEA:LastName)
                         END
                         JOIN(ENR:SeqStu,CLA:ClassNumber)
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,1104,6500,7729),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,490,6500,615)
                         STRING('Enrollment Summary Report'),AT(1760,250),USE(?String11),FONT(,18,COLOR:Black,FONT:italic)
                       END
DescriptionBREAK       BREAK(COU:Description)
                         HEADER,AT(0,0,,354),FONT('Arial',10,COLOR:Black,FONT:bold)
                           STRING(@s40),AT(0,0),USE(COU:Description),FONT(,18,COLOR:Black)
                         END
ClassBREAK               BREAK(CLA:ClassNumber)
                           HEADER,AT(0,0,,792),FONT('Arial',10,COLOR:Black,FONT:bold)
                             GROUP('Class'),AT(115,94,2146,552),BOXED
                               STRING(@n4),AT(604,313),USE(CLA:RoomNumber)
                               STRING('Room'),AT(156,313),USE(?String25)
                               STRING(@s20),AT(1031,313,1094),USE(CLA:ScheduledTime)
                             END
                             GROUP('Teacher'),AT(2510,125,3240,510),BOXED
                               STRING(@s20),AT(4125,313),USE(TEA:LastName)
                               STRING(@s20),AT(2573,313),USE(TEA:FirstName)
                             END
                           END
Detail1                    DETAIL,AT(,,,0),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                           END
                           FOOTER,AT(0,0,,458),WITHPRIOR(1)
                             STRING(@n5),AT(156,125),USE(Cnt_StudentNumber_2),CNT,RESET(ClassBREAK)
                             STRING('Students Enrolled in this Class'),AT(708,125)
                           END
                         END
                         FOOTER,AT(0,0,,448),PAGEAFTER(-1),WITHPRIOR(1)
                           LINE,AT(63,63,3156,0),USE(?Line1),COLOR(COLOR:Black)
                           STRING(@n5),AT(156,104),USE(Cnt_StudentNumber),CNT,RESET(DescriptionBREAK)
                           STRING('Students Enrolled in this Course'),AT(708,104)
                         END
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcEnrollSummary')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Courses.SetOpenRelated()
  Relate:Courses.Open                                      ! File Courses used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Courses, ?Progress:PctText, Progress:Thermometer, ProgressMgr, COU:Description)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(COU:KeyDescription)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Courses.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Courses.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AbcFinalGrades PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          BYTE                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
Cnt_StudentNumber    LONG                                  ! 
Cnt_StudentNumber_2  LONG                                  ! 
FinalGrade           DECIMAL(5,2)                          ! 
FinalLetterGrade     STRING(1)                             ! 
FullName             STRING(35)                            ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Students)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Number)
                       JOIN(ENR:StuSeq,STU:Number)
                         PROJECT(ENR:FinalExam)
                         PROJECT(ENR:MidtermExam)
                         PROJECT(ENR:StudentNumber)
                         PROJECT(ENR:TermPaper)
                         PROJECT(ENR:ClassNumber)
                         JOIN(CLA:KeyClassNumber,ENR:ClassNumber)
                           PROJECT(CLA:TeacherNumber)
                           PROJECT(CLA:CourseNumber)
                           JOIN(TEA:KeyTeacherNumber,CLA:TeacherNumber)
                             PROJECT(TEA:LastName)
                           END
                           JOIN(COU:KeyNumber,CLA:CourseNumber)
                             PROJECT(COU:Description)
                           END
                         END
                       END
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(1000,2010,6500,6823),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6500,1000)
                         STRING('TopSpeed University'),AT(2010,156),USE(?String19),FONT(,18,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Final Grade Report'),AT(2344,604),USE(?String20),FONT(,14,COLOR:Black,FONT:bold+FONT:underline)
                       END
STU:NumberBreak        BREAK(STU:Number)
                         HEADER,AT(0,0,,802)
                           STRING(@s35),AT(1146,94),USE(FullName),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Student:'),AT(479,94),USE(?String23),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@P###-##-####P),AT(1146,281,823,208),USE(ENR:StudentNumber),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Term Paper'),AT(4667,583),USE(?String13),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Final Grade'),AT(5625,583),USE(?String16),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Class'),AT(83,583),USE(?String17),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Instructor'),AT(2010,573),USE(?String18),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Mid-Term'),AT(3521,583),USE(?String15),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Final'),AT(4208,583),USE(?String14),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                         END
Detail1                  DETAIL,AT(10,,6490,260),USE(?Detail1),FONT('Arial',8,COLOR:Black)
                           STRING(@n-6),AT(3667,42,365,177),USE(ENR:MidtermExam)
                           STRING(@n-6),AT(4188,42,438,177),USE(ENR:FinalExam)
                           STRING(@n-6),AT(4990,42,365,177),USE(ENR:TermPaper)
                           STRING(@n5.2),AT(5677,42,313,177),USE(FinalGrade),RIGHT
                           STRING(@s1),AT(6115,42),USE(FinalLetterGrade)
                           STRING(@S30),AT(73,42),USE(COU:Description)
                           STRING(@S20),AT(2021,42),USE(TEA:LastName)
                         END
                         FOOTER,AT(0,0,,448),FONT('Arial',8,COLOR:Black),PAGEAFTER(-1)
                           LINE,AT(5510,31,1042,0),USE(?Line1),COLOR(COLOR:Black)
                           STRING(@n5.2),AT(5667,135),USE(FinalGrade,,?FinalGrade:2),RIGHT,AVE,RESET(STU:NumberBreak)
                           STRING('Overall GPA:'),AT(4656,135),USE(?String22),FONT(,,COLOR:Black,FONT:bold)
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcFinalGrades')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Students.SetOpenRelated()
  Relate:Students.Open                                     ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Students, ?Progress:PctText, Progress:Thermometer, ProgressMgr, STU:LastName)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(STU:KeyLastName)
  ThisReport.SetFilter('ENR:StudentNumber <<> 0')
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Students.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Students.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  FinalGrade = (ENR:MidtermExam + (ENR:FinalExam * 2) + ENR:TermPaper) / 4
  CASE (INT(FinalGrade / 10))
  OF 9
    FinalLetterGrade = 'A'
  OF 8
    FinalLetterGrade = 'B'
  OF 7
    FinalLetterGrade = 'C'
  OF 6
    FinalLetterGrade = 'D'
  ELSE
    FinalLetterGrade = 'F'
  END
  FullName = CLIP(STU:FirstName) & ' ' & STU:LastName
  ReturnValue = PARENT.TakeRecord()
  PRINT(Rpt:Detail1)
  RETURN ReturnValue

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AbcStudentIDs PROCEDURE 

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
FilesOpened          BYTE                                  ! 
FullName             STRING(35)                            ! 
CSZ                  STRING(35)                            ! 
Progress:Thermometer BYTE                                  ! 
Process:View         VIEW(Students)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Number)
                       PROJECT(STU:Photograph)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                     END
ProgressWindow       WINDOW('Report Progress...'),AT(,,142,59),DOUBLE,CENTER,GRAY,TIMER(1)
                       PROGRESS,AT(15,15,111,12),USE(Progress:Thermometer),RANGE(0,100)
                       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
                       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
                       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
                     END

Report               REPORT,AT(0,0,8500,11000),PRE(RPT),FONT('Arial',10,COLOR:Black,FONT:regular),THOUS
detail                 DETAIL,AT(,,4250,2198),USE(?detail)
                         BOX,AT(219,229,3729,1719),USE(?Box1),COLOR(COLOR:Black),LINEWIDTH(30),ROUND
                         STRING('TopSpeed University'),AT(448,333),USE(?String1),FONT(,12,COLOR:Black,FONT:bold+FONT:italic)
                         IMAGE,AT(2719,385),USE(?Image1)
                         STRING(@S20),AT(448,1219),USE(STU:Address)
                         STRING(@s20),AT(448,1385),USE(STU:Address2)
                         STRING('Student Identification'),AT(448,531),USE(?String2),FONT(,,COLOR:Black,FONT:bold)
                         STRING(@s35),AT(448,792),USE(FullName),TRN
                         STRING(@P###-##-####P),AT(448,969),USE(STU:Number),LEFT(1)
                         STRING(@s35),AT(448,1563),USE(CSZ),TRN
                       END
                     END
ThisWindow           CLASS(ReportManager)
Init                   PROCEDURE(),BYTE,PROC,DERIVED
Kill                   PROCEDURE(),BYTE,PROC,DERIVED
                     END

ThisReport           CLASS(ProcessClass)                   ! Process Manager
TakeRecord             PROCEDURE(),BYTE,PROC,DERIVED
                     END

ProgressMgr          StepStringClass                       ! Progress Manager
Previewer            PrintPreviewClass                     ! Print Previewer

  CODE
  GlobalResponse = ThisWindow.Run()                        ! Opens the window and starts an Accept Loop

!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It`s called after the window open
!|
!---------------------------------------------------------------------------

ThisWindow.Init PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  GlobalErrors.SetProcedureName('AbcStudentIDs')
  SELF.Request = GlobalRequest                             ! Store the incoming request
  ReturnValue = PARENT.Init()
  IF ReturnValue THEN RETURN ReturnValue.
  SELF.FirstField = ?Progress:Thermometer
  SELF.VCRRequest &= VCRRequest
  SELF.Errors &= GlobalErrors                              ! Set this windows ErrorManager to the global ErrorManager
  CLEAR(GlobalRequest)                                     ! Clear GlobalRequest after storing locally
  CLEAR(GlobalResponse)
  Relate:Students.SetOpenRelated()
  Relate:Students.Open                                     ! File Students used by this procedure, so make sure it's RelationManager is open
  SELF.FilesOpened = True
  SELF.Open(ProgressWindow)                                ! Open window
  Do DefineListboxStyle
  ProgressMgr.Init(ScrollSort:AllowAlpha+ScrollSort:AllowNumeric,ScrollBy:RunTime)
  ThisReport.Init(Process:View, Relate:Students, ?Progress:PctText, Progress:Thermometer, ProgressMgr, STU:LastName)
  ThisReport.CaseSensitiveValue = FALSE
  ThisReport.AddSortOrder(STU:KeyLastName)
  SELF.AddItem(?Progress:Cancel,RequestCancelled)
  SELF.Init(ThisReport,Report,Previewer)
  ?Progress:UserString{PROP:Text} = ''
  Relate:Students.SetQuickScan(1,Propagate:OneMany)
  ProgressWindow{PROP:Timer} = 10                          ! Assign timer interval
  SELF.SkipPreview = False
  Previewer.SetINIManager(INIMgr)
  Previewer.AllowUserZoom = True
  SELF.SetAlerts()
  RETURN ReturnValue


ThisWindow.Kill PROCEDURE

ReturnValue          BYTE,AUTO

  CODE
  ReturnValue = PARENT.Kill()
  IF ReturnValue THEN RETURN ReturnValue.
  IF SELF.FilesOpened
    Relate:Students.Close
  END
  ProgressMgr.Kill()
  GlobalErrors.SetProcedureName
  RETURN ReturnValue


ThisReport.TakeRecord PROCEDURE

ReturnValue          BYTE,AUTO

SkipDetails BYTE
  CODE
  FullName = CLIP(STU:FirstName) & ' ' & STU:LastName
  CSZ = CLIP(STU:City) & ', ' & STU:State & ' ' & STU:Zip
  !Assign BLOB to IMAGE control
  Report$?Image1{PROP:NoWidth} = TRUE
  Report$?Image1{PROP:NoHeight} = TRUE
  Report$?Image1{PROP:ImageBlob} = STU:Photograph{PROP:Handle}
  IF Report$?Image1{PROP:Height} > 1000
    AspectRatio$ = Report$?Image1{PROP:Width}/Report$?Image1{PROP:Height}
    Report$?Image1{PROP:Height} = 1000
    Report$?Image1{PROP:Width} = 1000 * AspectRatio$
  END
  IF Report$?Image1{PROP:Width} > 1000
    AspectRatio$ = Report$?Image1{PROP:Height}/Report$?Image1{PROP:Width}
    Report$?Image1{PROP:Width} = 1000
    Report$?Image1{PROP:Height} = 1000 * AspectRatio$
  END
   
  
  ReturnValue = PARENT.TakeRecord()
  PRINT(RPT:detail)
  RETURN ReturnValue

