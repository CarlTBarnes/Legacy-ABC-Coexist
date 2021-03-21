  MEMBER('DataABC.clw')

  PRAGMA('define(init_priority=>3)')

  MAP
    MODULE('DataABC_BC0.CLW')
DataABC_BC0:DctInit             PROCEDURE()
DataABC_BC0:DctKill             PROCEDURE()
DataABC_BC0:FilesInit           PROCEDURE()
    END
  END

DLLInit              DllInitializer                          !This object is used to initialize the dll, it is defined in the main program module

DctInit PROCEDURE()
  CODE
  DataABC_BC0:DctInit
  DataABC_BC0:FilesInit


DctKill PROCEDURE()
  CODE
  DataABC_BC0:DctKill

