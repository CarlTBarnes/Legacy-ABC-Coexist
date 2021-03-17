!To allow Legacy and ABC to both see the same VCR/GlobalRequest/Response
!this Global Data DLL could be imported by Legacy and ABC APPs
!That would require every APP to Import. There may be other shared data so useful.
!Don;t forget SYSTEM['User Props'} can be seen by both
!
!The Data DLLs for Legacy and ABC declare and export these which won't compile
!Ideally these would be changed to EXTERNAL and NOT EXPORT.
!That requires some tempate chnages, and can be done by hand.
!
!As long as the DATA DLLs do not have nay procedures using GlobalR/R you
!can get away with simply removing them from the EXP file. That can
!be done with a Template I'll post.

  PROGRAM

GlobalRequest        LONG(0),THREAD !,EXPORT   Does not work to have only
GlobalResponse       LONG(0),THREAD !,EXPORT   these kinds of EXPORT
VCRRequest           LONG(0),THREAD !,EXPORT   Must have something in EXP

  CODE
  