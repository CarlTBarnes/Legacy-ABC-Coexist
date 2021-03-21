

                     MEMBER('ProcLegacy.clw')              ! This is a MEMBER module

!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
CourseEnrollment PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
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
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
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
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
Report               REPORT,AT(1000,1000,6500,7833),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
REPORTBrk              BREAK(LocalRequest)
                         HEADER,AT(0,0),ALONE
                           STRING('Enrollment Report'),AT(1948,2552,2958,406),FONT('Times New Roman',20,COLOR:Black,FONT:bold+FONT:italic), |
  CENTER
                         END
DescriptionBREAK         BREAK(COU:Description)
                           HEADER,AT(0,0,,833),FONT('Arial',10,COLOR:Black,FONT:bold),PAGEBEFORE(-1)
                             STRING(@s40),AT(0,0),USE(COU:Description),FONT(,18,COLOR:Black)
                             TEXT,AT(781,229,5635,573),USE(COU:CompleteDescription),COLOR(COLOR:Silver),RESIZE
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
Detail1                      DETAIL,AT(,,,250),FONT('Arial',8,COLOR:Black)
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
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(Report)
       Report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
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
         TEA:Number = CLA:TeacherNumber                    ! Assign linking field value
         GET(Teachers,TEA:KeyTeacherNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(TEA:Record)                               ! Clear record if unsuccessful
         END
         STU:Number = ENR:StudentNumber                    ! Assign linking field value
         GET(Students,STU:KeyStudentNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(STU:Record)                               ! Clear record if unsuccessful
         END
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(Rpt:Detail1)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Courses,'QUICKSCAN=off').
  IF SEND(Teachers,'QUICKSCAN=off').
  IF SEND(Students,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(Report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          Report{PROP:FlushPreview} = True
    END
  END
  CLOSE(Report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Students')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(COU:KeyDescription)
  Process:View{Prop:Filter} = 'ENR:StudentNumber <<> 0'
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = 500
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
    !Setup progress bar
    RecordsToProcess = RECORDS(Enrollment)
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Courses,'QUICKSCAN=on')
  SEND(Teachers,'QUICKSCAN=on')
  SEND(Students,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
  BIND(TEA:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  GET(Teachers,TEA:KeyTeacherNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(TEA:Record)                                      ! Clear record if unsuccessful
  END
  STU:Number = ENR:StudentNumber                           ! Assign linking field value
  GET(Students,STU:KeyStudentNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(STU:Record)                                      ! Clear record if unsuccessful
  END
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
AttendanceSheets PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
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
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
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
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
Report               REPORT,AT(1010,1417,6500,7438),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(990,1000,6500,406)
                         STRING('Attendance'),AT(2448,52),USE(?String17),FONT(,18,,FONT:italic)
                       END
DescriptionBREAK       BREAK(COU:Description)
ClassBREAK               BREAK(CLA:ClassNumber)
                           HEADER,AT(,,,1594),FONT('Arial',10,COLOR:Black,FONT:bold)
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
Detail1                    DETAIL,AT(,,,250),FONT('Arial',8,COLOR:Black)
                             STRING(@P###-##-####P),AT(3656,21),USE(ENR:StudentNumber)
                             STRING(@S20),AT(2094,10),USE(STU:FirstName)
                             STRING(@s20),AT(1042,10,948),USE(STU:LastName)
                           END
                           FOOTER,AT(,,,240),PAGEAFTER(-1),WITHPRIOR(1)
                           END
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(Report)
       Report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
         TEA:Number = CLA:TeacherNumber                    ! Assign linking field value
         GET(Teachers,TEA:KeyTeacherNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(TEA:Record)                               ! Clear record if unsuccessful
         END
         STU:Number = ENR:StudentNumber                    ! Assign linking field value
         GET(Students,STU:KeyStudentNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(STU:Record)                               ! Clear record if unsuccessful
         END
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(Rpt:Detail1)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Courses,'QUICKSCAN=off').
  IF SEND(Teachers,'QUICKSCAN=off').
  IF SEND(Students,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(Report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          Report{PROP:FlushPreview} = True
    END
  END
  CLOSE(Report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Students')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(COU:KeyDescription)
  Process:View{Prop:Filter} = 'ENR:StudentNumber <<> 0'
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = 3000
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
    !Setup progress bar
    RecordsToProcess = RECORDS(Enrollment)
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Courses,'QUICKSCAN=on')
  SEND(Teachers,'QUICKSCAN=on')
  SEND(Students,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
  BIND(TEA:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  GET(Teachers,TEA:KeyTeacherNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(TEA:Record)                                      ! Clear record if unsuccessful
  END
  STU:Number = ENR:StudentNumber                           ! Assign linking field value
  GET(Students,STU:KeyStudentNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(STU:Record)                                      ! Clear record if unsuccessful
  END
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Report
!!! Student Schedules
!!! </summary>
ClassSchedules1 PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
Process:View         VIEW(Enrollment)
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
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
report               REPORT,AT(1000,1552,6000,7448),PRE(RPT),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6000,552)
                         STRING('Student Schedule'),AT(0,281,6000,220),FONT(,,COLOR:Black,FONT:bold),CENTER
                       END
ENR:StudentNumberBreak BREAK(ENR:StudentNumber)
                         HEADER,AT(,,5990,677)
                           STRING(@P###-##-####P),AT(3406,63),USE(STU:Number),FONT(,,COLOR:Black,FONT:bold),RIGHT(1)
                           STRING('Room'),AT(2875,292),USE(?String9),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Time'),AT(3688,292),USE(?String10),FONT(,,COLOR:Black,FONT:bold)
                           LINE,AT(50,560,5900,0),USE(?Line2),COLOR(COLOR:Black)
                           STRING('Class'),AT(479,292),USE(?String11),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@S20),AT(83,63),USE(STU:FirstName),FONT(,,COLOR:Black,FONT:bold)
                           STRING(@S20),AT(1760,63),USE(STU:LastName),FONT(,,COLOR:Black,FONT:bold)
                         END
detail                   DETAIL,AT(-10,10,6000,302)
                           STRING(@S30),AT(250,42),USE(COU:Description)
                           STRING(@n4),AT(2875,42),USE(CLA:RoomNumber)
                           STRING(@s20),AT(3688,42),USE(CLA:ScheduledTime)
                         END
                         FOOTER,AT(,,,323),PAGEAFTER(-1)
                         END
                       END
                       FOOTER,AT(1000,9000,6000,219)
                         STRING(@pPage <<<#p),AT(5250,30,700,135),USE(?PageCount),FONT('Arial',8,COLOR:Black,FONT:regular), |
  PAGENO
                       END
                     END
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(report)
       report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
         CLA:ClassNumber = ENR:ClassNumber                 ! Assign linking field value
         GET(Classes,CLA:KeyClassNumber)                   ! Lookup record
         IF ERRORCODE()
           CLEAR(CLA:Record)                               ! Clear record if unsuccessful
         END
         TEA:Number = CLA:TeacherNumber                    ! Assign linking field value
         GET(Teachers,TEA:KeyTeacherNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(TEA:Record)                               ! Clear record if unsuccessful
         END
         COU:Number = CLA:CourseNumber                     ! Assign linking field value
         GET(Courses,COU:KeyNumber)                        ! Lookup record
         IF ERRORCODE()
           CLEAR(COU:Record)                               ! Clear record if unsuccessful
           CLEAR(COU:CompleteDescription)
         END
         STU:Number = ENR:StudentNumber                    ! Assign linking field value
         GET(Students,STU:KeyStudentNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(STU:Record)                               ! Clear record if unsuccessful
         END
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(RPT:detail)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Enrollment,'QUICKSCAN=off').
  IF SEND(Classes,'QUICKSCAN=off').
  IF SEND(Teachers,'QUICKSCAN=off').
  IF SEND(Courses,'QUICKSCAN=off').
  IF SEND(Students,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          report{PROP:FlushPreview} = True
    END
  END
  CLOSE(report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Students')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(ENR:StuSeq)
  Process:View{Prop:Filter} = ''
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = RECORDS(Enrollment)
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Enrollment,'QUICKSCAN=on')
  SEND(Classes,'QUICKSCAN=on')
  SEND(Teachers,'QUICKSCAN=on')
  SEND(Courses,'QUICKSCAN=on')
  SEND(Students,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
  BIND(TEA:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
  CLA:ClassNumber = ENR:ClassNumber                        ! Assign linking field value
  GET(Classes,CLA:KeyClassNumber)                          ! Lookup record
  IF ERRORCODE()
    CLEAR(CLA:Record)                                      ! Clear record if unsuccessful
  END
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  GET(Teachers,TEA:KeyTeacherNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(TEA:Record)                                      ! Clear record if unsuccessful
  END
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  GET(Courses,COU:KeyNumber)                               ! Lookup record
  IF ERRORCODE()
    CLEAR(COU:Record)                                      ! Clear record if unsuccessful
    CLEAR(COU:CompleteDescription)
  END
  STU:Number = ENR:StudentNumber                           ! Assign linking field value
  GET(Students,STU:KeyStudentNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(STU:Record)                                      ! Clear record if unsuccessful
  END
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Report
!!! Teacher Schedules
!!! </summary>
ClassSchedules2 PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
WindowOpened         LONG                                  ! 
RecordsToProcess     LONG,AUTO                             ! 
RecordsProcessed     LONG,AUTO                             ! 
RecordsPerCycle      LONG,AUTO                             ! 
RecordsThisCycle     LONG,AUTO                             ! 
PercentProgress      BYTE                                  ! 
RecordStatus         BYTE,AUTO                             ! 
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
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
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
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
detail                   DETAIL,AT(-10,10,6000,302)
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
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(report)
       report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
         COU:Number = CLA:CourseNumber                     ! Assign linking field value
         GET(Courses,COU:KeyNumber)                        ! Lookup record
         IF ERRORCODE()
           CLEAR(COU:Record)                               ! Clear record if unsuccessful
           CLEAR(COU:CompleteDescription)
         END
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(RPT:detail)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Teachers,'QUICKSCAN=off').
  IF SEND(Courses,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          report{PROP:FlushPreview} = True
    END
  END
  CLOSE(report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Courses')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(TEA:KeyLastName)
  Process:View{Prop:Filter} = 'CLA:TeacherNumber <<> 0'
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = 500
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Teachers,'QUICKSCAN=on')
  SEND(Courses,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(TEA:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  GET(Courses,COU:KeyNumber)                               ! Lookup record
  IF ERRORCODE()
    CLEAR(COU:Record)                                      ! Clear record if unsuccessful
    CLEAR(COU:CompleteDescription)
  END
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
EnrollSummary PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
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
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
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
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
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
Detail1                    DETAIL,AT(,,,0),FONT('Arial',8,COLOR:Black)
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
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(Report)
       Report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
         TEA:Number = CLA:TeacherNumber                    ! Assign linking field value
         GET(Teachers,TEA:KeyTeacherNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(TEA:Record)                               ! Clear record if unsuccessful
         END
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(Rpt:Detail1)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Courses,'QUICKSCAN=off').
  IF SEND(Teachers,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(Report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          Report{PROP:FlushPreview} = True
    END
  END
  CLOSE(Report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Enrollment')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(COU:KeyDescription)
  Process:View{Prop:Filter} = 'ENR:StudentNumber <<> 0'
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = 500
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
    !Setup progress bar
    RecordsToProcess = RECORDS(Enrollment)
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Courses,'QUICKSCAN=on')
  SEND(Teachers,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(TEA:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  GET(Teachers,TEA:KeyTeacherNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(TEA:Record)                                      ! Clear record if unsuccessful
  END
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
FinalGrades PROCEDURE

RejectRecord         LONG                                  ! 
LocalRequest         LONG                                  ! 
LocalResponse        LONG                                  ! 
FilesOpened          LONG                                  ! 
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
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
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
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
Report               REPORT,AT(1000,2010,6500,6823),PRE(Rpt),FONT('Arial',10,COLOR:Black),THOUS
                       HEADER,AT(1000,1000,6500,1000)
                         STRING('TopSpeed University'),AT(2010,156),USE(?String19),FONT(,18,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Final Grade Report'),AT(2344,604),USE(?String20),FONT(,14,COLOR:Black,FONT:bold+FONT:underline)
                       END
STU:NumberBreak        BREAK(STU:Number)
                         HEADER,AT(,,,802)
                           STRING(@s35),AT(1146,94),USE(FullName),FONT(,,,FONT:bold)
                           STRING('Student:'),AT(479,94),USE(?String23),FONT(,,,FONT:bold)
                           STRING(@P###-##-####P),AT(1146,281,823,208),USE(ENR:StudentNumber),FONT(,,COLOR:Black,FONT:bold)
                           STRING('Term Paper'),AT(4667,583),USE(?String13),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Final Grade'),AT(5625,583),USE(?String16),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Class'),AT(83,583),USE(?String17),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Instructor'),AT(2010,573),USE(?String18),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Mid-Term'),AT(3521,583),USE(?String15),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                           STRING('Final'),AT(4208,583),USE(?String14),FONT(,,COLOR:Black,FONT:bold+FONT:underline)
                         END
Detail1                  DETAIL,AT(10,,6490,260),FONT('Arial',8,COLOR:Black)
                           STRING(@n-6),AT(3667,42,365,177),USE(ENR:MidtermExam)
                           STRING(@n-6),AT(4188,42,438,177),USE(ENR:FinalExam)
                           STRING(@n-6),AT(4990,42,365,177),USE(ENR:TermPaper)
                           STRING(@n5.2),AT(5677,42,313,177),USE(FinalGrade),RIGHT
                           STRING(@s1),AT(6115,42),USE(FinalLetterGrade)
                           STRING(@S30),AT(73,42),USE(COU:Description)
                           STRING(@S20),AT(2021,42),USE(TEA:LastName)
                         END
                         FOOTER,AT(,,,448),FONT('Arial',8,COLOR:Black),PAGEAFTER(-1)
                           LINE,AT(5510,31,1042,0),USE(?Line1),COLOR(COLOR:Black)
                           STRING(@n5.2),AT(5667,135),USE(FinalGrade,,?FinalGrade:2),RIGHT,AVE,RESET(STU:NumberBreak)
                           STRING('Overall GPA:'),AT(4656,135),USE(?String22),FONT(,,,FONT:bold)
                         END
                       END
                       FOOTER,AT(1000,8833,,583),FONT('Times New Roman',9,COLOR:Black,FONT:bold+FONT:italic)
                         STRING('Page '),AT(5833,292)
                         STRING(@n5),AT(6115,292),PAGENO
                       END
                     END
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(Report)
       Report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
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
         CLA:ClassNumber = ENR:ClassNumber                 ! Assign linking field value
         GET(Classes,CLA:KeyClassNumber)                   ! Lookup record
         IF ERRORCODE()
           CLEAR(CLA:Record)                               ! Clear record if unsuccessful
         END
         TEA:Number = CLA:TeacherNumber                    ! Assign linking field value
         GET(Teachers,TEA:KeyTeacherNumber)                ! Lookup record
         IF ERRORCODE()
           CLEAR(TEA:Record)                               ! Clear record if unsuccessful
         END
         COU:Number = CLA:CourseNumber                     ! Assign linking field value
         GET(Courses,COU:KeyNumber)                        ! Lookup record
         IF ERRORCODE()
           CLEAR(COU:Record)                               ! Clear record if unsuccessful
           CLEAR(COU:CompleteDescription)
         END
         Do SetDynamicControlsAttributes
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(Rpt:Detail1)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Students,'QUICKSCAN=off').
  IF SEND(Classes,'QUICKSCAN=off').
  IF SEND(Teachers,'QUICKSCAN=off').
  IF SEND(Courses,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(Report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          Report{PROP:FlushPreview} = True
    END
  END
  CLOSE(Report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Courses')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(STU:KeyLastName)
  Process:View{Prop:Filter} = 'ENR:StudentNumber <<> 0'
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Classes::Used = 0
    CheckOpen(Classes,1)
  END
  Classes::Used += 1
  IF Courses::Used = 0
    CheckOpen(Courses,1)
  END
  Courses::Used += 1
  IF Enrollment::Used = 0
    CheckOpen(Enrollment,1)
  END
  Enrollment::Used += 1
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  IF Teachers::Used = 0
    CheckOpen(Teachers,1)
  END
  Teachers::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = 500
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
    !Setup progress bar
    RecordsToProcess = RECORDS(Enrollment)
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Students,'QUICKSCAN=on')
  SEND(Classes,'QUICKSCAN=on')
  SEND(Teachers,'QUICKSCAN=on')
  SEND(Courses,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(CLA:RECORD)
  BIND(COU:RECORD)
  BIND('COU:CompleteDescription',COU:CompleteDescription)
  BIND(ENR:RECORD)
  BIND(STU:RECORD)
  BIND(TEA:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Classes::Used -= 1
    IF Classes::Used = 0 THEN CLOSE(Classes).
    Courses::Used -= 1
    IF Courses::Used = 0 THEN CLOSE(Courses).
    Enrollment::Used -= 1
    IF Enrollment::Used = 0 THEN CLOSE(Enrollment).
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
    Teachers::Used -= 1
    IF Teachers::Used = 0 THEN CLOSE(Teachers).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
  CLA:ClassNumber = ENR:ClassNumber                        ! Assign linking field value
  GET(Classes,CLA:KeyClassNumber)                          ! Lookup record
  IF ERRORCODE()
    CLEAR(CLA:Record)                                      ! Clear record if unsuccessful
  END
  TEA:Number = CLA:TeacherNumber                           ! Assign linking field value
  GET(Teachers,TEA:KeyTeacherNumber)                       ! Lookup record
  IF ERRORCODE()
    CLEAR(TEA:Record)                                      ! Clear record if unsuccessful
  END
  COU:Number = CLA:CourseNumber                            ! Assign linking field value
  GET(Courses,COU:KeyNumber)                               ! Lookup record
  IF ERRORCODE()
    CLEAR(COU:Record)                                      ! Clear record if unsuccessful
    CLEAR(COU:CompleteDescription)
  END
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
!!! <summary>
!!! Generated from procedure template - Report
!!! </summary>
StudentIDs PROCEDURE

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
FilesOpened          LONG                                  ! 
FullName             STRING(35)                            ! 
CSZ                  STRING(35)                            ! 
OriginalRequest      LONG                                  ! 
WindowInitialized    LONG                                  ! 
ForceRefresh         LONG                                  ! 
Process:View         VIEW(Students)
                       PROJECT(STU:Address)
                       PROJECT(STU:Address2)
                       PROJECT(STU:City)
                       PROJECT(STU:FirstName)
                       PROJECT(STU:LastName)
                       PROJECT(STU:Number)
                       PROJECT(STU:State)
                       PROJECT(STU:Zip)
                     END
PrintSkipDetails     BOOL,AUTO
SkipPreview          BYTE
PreviewQueueIndex    LONG
LocalOutputFileQueue PrintPreviewFileQueue
PrintPreviewQueue    PrintPreviewFileQueue
ShowOutputProgress   BYTE(1)   
Progress:Thermometer BYTE
!This VIEW will generate a "Label duplicated, second used" compiler warning,
! which is exactly what is required to to print the photograph
!Process:View         VIEW(Students)
!                     END           
!                                   
Report               REPORT,AT(0,0,8500,11000),PRE(RPT),FONT('Arial',10,COLOR:Black,FONT:regular),THOUS
detail                 DETAIL,AT(,,4250,2198)
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
ProgressWindow WINDOW('Progress...'),AT(,,142,59),CENTER,TIMER(1),GRAY,DOUBLE
       PROGRESS,USE(Progress:Thermometer),AT(15,15,111,12),RANGE(0,100)
       STRING(''),AT(0,3,141,10),USE(?Progress:UserString),CENTER
       STRING(''),AT(0,30,141,10),USE(?Progress:PctText),CENTER
       BUTTON('Cancel'),AT(45,42,50,15),USE(?Progress:Cancel)
     END
  CODE
  PUSHBIND
  LocalRequest    = GlobalRequest
  OriginalRequest = GlobalRequest
  LocalResponse   = RequestCancelled
  ForceRefresh    = False
  CLEAR(GlobalRequest)
  CLEAR(GlobalResponse)
  SkipPreview = False
  IF KEYCODE() = MouseRight
    SETKEYCODE(0)
  END
  DO PrepareProcedure
  ACCEPT
    CASE EVENT()
    OF EVENT:GainFocus
      ForceRefresh = True
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      ELSE
        DO RefreshWindow
      END
    OF EVENT:OpenWindow
         DO GetFirstRecord
         IF LocalResponse = RequestCancelled
           POST(EVENT:CloseWindow)
           CYCLE
         END
       OPEN(Report)
       Report{Prop:Preview} = PrintPreviewQueue.Filename
       Do SetStaticControlsAttributes
      IF NOT WindowInitialized
        DO InitializeWindow
        WindowInitialized = True
      END
      SELECT(1)
    OF EVENT:Timer
        RecordsThisCycle = 0
       !Set the MRP to RecordsPerCycle
       IF Process:View{PROP:IPRequestCount}=0
          Process:View{PROP:IPRequestCount}=RecordsPerCycle
       END
       LOOP RecordsPerCycle TIMES
         FullName = CLIP(STU:FirstName) & ' ' & STU:LastName
         CSZ = CLIP(STU:City) & ', ' & STU:State & ' ' & STU:Zip
         Do SetDynamicControlsAttributes
         !Assign BLOB to IMAGE control
         Report$?Image1{PROP:ImageBlob} = STU:Photograph{PROP:Handle}
         Report$?Image1{PROP:Width} = 969
         Report$?Image1{PROP:Height} = 1240
         
         PrintSkipDetails = FALSE
         
         IF ~PrintSkipDetails THEN
           PRINT(RPT:detail)
         END
         LOOP
           DO GetNextRecord
           DO ValidateRecord
           CASE RecordStatus
             OF Record:OutOfRange
               LocalResponse = RequestCancelled
               BREAK
             OF Record:OK
               BREAK
           END
         END
         IF LocalResponse = RequestCancelled
            LocalResponse = RequestCompleted
            BREAK
         END
         LocalResponse = RequestCancelled
       END
       IF LocalResponse = RequestCompleted
         POST(Event:CloseWindow)
       END
    OF Event:Rejected
      BEEP
      DISPLAY(?)
      SELECT(?)
    END
    CASE FIELD()
    OF ?Progress:Cancel
      CASE EVENT()
      OF EVENT:Accepted
        DO SyncWindow
         LocalResponse = RequestCancelled
         POST(EVENT:CloseWindow)
      END
    END
  END
  IF SEND(Students,'QUICKSCAN=off').
  IF LocalResponse = RequestCompleted
    ENDPAGE(Report)
    IF NOT SkipPreview THEN
       ReportPreview(PrintPreviewQueue)
    ELSE
       GlobalResponse = RequestCompleted
    END
    IF GlobalResponse = RequestCompleted
          FREE(LocalOutputFileQueue)
          LOOP PreviewQueueIndex=1 TO RECORDS(PrintPreviewQueue)
               GET(PrintPreviewQueue,PreviewQueueIndex)
               IF NOT ERRORCODE() THEN
                  LocalOutputFileQueue.FileName = PrintPreviewQueue.FileName
                  ADD(LocalOutputFileQueue)
               END
          END
          Do ProcessOutputFileQueue
          FREE(LocalOutputFileQueue)
          Report{PROP:FlushPreview} = True
    END
  END
  CLOSE(Report)
  FREE(PrintPreviewQueue)
  DO ProcedureReturn
!---------------------------------------------------------------------------
!-----------------------------------------------------------------------------
ValidateRecord       ROUTINE
!|
!| This routine is used to provide for complex record filtering and range limiting. This
!| routine is only generated if you've included your own code in the EMBED points provided in
!| this routine.
!|
  RecordStatus = Record:OutOfRange
  IF LocalResponse = RequestCancelled THEN EXIT.
  RecordStatus = Record:OK
  EXIT
GetNextRecord ROUTINE
!|
!| This routine is used to retrieve the next record from the VIEW.
!|
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  NEXT(Process:View)
  IF ERRORCODE()
     IF ERRORCODE() <> BadRecErr
       StandardWarning(Warn:RecordFetchError,'Students')
     END
     LocalResponse = RequestCancelled
     EXIT
  ELSE
     LocalResponse = RequestCompleted
  END
  RecordsProcessed += 1
  RecordsThisCycle += 1
  IF PercentProgress < 100
     PercentProgress = (RecordsProcessed / RecordsToProcess)*100
     IF PercentProgress > 100
        PercentProgress = 100
     END
     IF PercentProgress <> Progress:Thermometer THEN
        Progress:Thermometer = PercentProgress
        ?Progress:PctText{Prop:Text} = FORMAT(PercentProgress,@N3) & '% Completed'
        DISPLAY()
     END
  END
GetFirstRecord ROUTINE
!|
!| This routine open the view, set the range and the filter and also
!| is used to retrieve the first record from the VIEW.
!| 
!| After the record has been retrieved, the PROGRESS control on the
!| Progress window is updated.
!|
  SET(STU:KeyLastName)
  Process:View{Prop:Filter} = ''
  OPEN(Process:View)
  IF ERRORCODE()
     StandardWarning(Warn:ViewOpenError)
  END
  IF CLIP(Process:View{PROP:Order}) THEN
     SET(Process:View)
  END
  LOOP
     DO GetNextRecord
     DO ValidateRecord
     CASE RecordStatus
        OF Record:Ok
           BREAK
        OF Record:OutOfRange
           LocalResponse = RequestCancelled
           BREAK
     END
  END
ProcessOutputFileQueue          ROUTINE
SetStaticControlsAttributes     ROUTINE
SetDynamicControlsAttributes    ROUTINE
PrepareProcedure ROUTINE
  IF Students::Used = 0
    CheckOpen(Students,1)
  END
  Students::Used += 1
  FilesOpened = TRUE
  DO BindFields
    RecordsToProcess = RECORDS(Students)
    RecordsPerCycle = 25
    RecordsProcessed = 0
    PercentProgress = 0
  OPEN(ProgressWindow)
  WindowOpened=True
  Do DefineListboxStyle
  ProgressWindow{Prop:Text} = 'Generating Report'
  ?Progress:PctText{Prop:Text} = '0% Completed'
  ?Progress:UserString{Prop:Text}=''
  SEND(Students,'QUICKSCAN=on')

!---------------------------------------------------------------------------
BindFields ROUTINE
  BIND(STU:RECORD)
!---------------------------------------------------------------------------
UnBindFields ROUTINE
!---------------------------------------------------------------------------
ProcedureReturn ROUTINE
!|
!| This routine provides a common procedure exit point for all template
!| generated procedures.
!|
!| First, all of the files opened by this procedure are closed.
!|
!| Next, if it was opened by this procedure, the window is closed.
!|
!| Next, GlobalResponse is assigned a value to signal the calling procedure
!| what happened in this procedure.
!|
!| Next, we replace the BINDings that were in place when the procedure initialized
!| (and saved with PUSHBIND) using POPBIND.
!|
!| Finally, we return to the calling procedure.
!|
  IF FilesOpened
    CLOSE(Process:View)
    Students::Used -= 1
    IF Students::Used = 0 THEN CLOSE(Students).
  END
  IF WindowOpened
    CLOSE(ProgressWindow)
  END
  Do UnBindFields
  IF LocalResponse
    GlobalResponse = LocalResponse
  ELSE
    GlobalResponse = RequestCancelled
  END
  POPBIND
  RETURN
!---------------------------------------------------------------------------
InitializeWindow ROUTINE
!|
!| This routine is used to prepare any control templates for use. It should be called once
!| per procedure.
!|
  DO RefreshWindow
!---------------------------------------------------------------------------
RefreshWindow ROUTINE
!|
!| This routine is used to keep all displays and control templates current.
!|
  IF ProgressWindow{Prop:AcceptAll} THEN EXIT.
  Do LookupRelated
  DISPLAY()
  ForceRefresh = False
!---------------------------------------------------------------------------
SyncWindow ROUTINE
!|
!| This routine is used to insure that any records pointed to in control
!| templates are fetched before any procedures are called via buttons or menu
!| options.
!|
  Do LookupRelated
!---------------------------------------------------------------------------
LookupRelated  ROUTINE
!|
!| This routine fetch all related records.
!| It's called from SyncWindow and RefreshWindow
!|
!---------------------------------------------------------------------------
DefineListboxStyle ROUTINE
!|
!| This routine create all the styles to be shared in this window
!| It's called after the window open
!|
!---------------------------------------------------------------------------
