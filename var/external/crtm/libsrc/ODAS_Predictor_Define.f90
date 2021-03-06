!
! ODAS_Predictor_Define
!
! Module defining the Optical Depth Absorber Space (ODAS) Predictor data
! structure and containing routines to manipulate it.
!
! CREATION HISTORY:
!       This file was initially automatically generated so edit at your own risk.
!       Generated by fdefmod.rb on 22-Dec-2006 at 19:08:23
!       Contact info:  Paul van Delst, CIMSS/SSEC
!                      paul.vandelst@ssec.wisc.edu
!
!       Modifed by:    Yong Han, NESDIS/STAR 25-June-2008
!                      yong.han@noaa.gov
!

MODULE ODAS_Predictor_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, SET
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Predictor data structure definition
  PUBLIC :: Predictor_type
  ! Structure procedures
  PUBLIC :: Associated_Predictor
  PUBLIC :: Destroy_Predictor
  PUBLIC :: Allocate_Predictor
  PUBLIC :: Assign_Predictor
  PUBLIC :: Zero_Predictor
    

  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: ODAS_Predictor_Define.f90 4799 2009-09-01 13:57:30Z yong.han@noaa.gov $'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! -----------------------
  ! Derived type definition
  ! -----------------------
  TYPE :: Predictor_type
    INTEGER :: n_Allocates=0
    ! Dimensions
    INTEGER :: n_Layers    =0  ! K
    INTEGER :: n_Predictors=0  ! I
    INTEGER :: n_Absorbers =0  ! J
    INTEGER :: n_Orders    =0  ! IO
    ! Scalars
    REAL(fp) :: Secant_Sensor_Zenith = ZERO
    ! Arrays
    REAL(fp), DIMENSION(:,:),   POINTER :: A   =>NULL() ! 0:K x J, Integrated absorber
    REAL(fp), DIMENSION(:,:),   POINTER :: dA  =>NULL() ! K x J, Integrated absorber level difference
    REAL(fp), DIMENSION(:,:),   POINTER :: aveA=>NULL() ! K x J, Integrated absorber layer average
    REAL(fp), DIMENSION(:,:,:), POINTER :: Ap  =>NULL() ! K x IO x J, Power of absorber level
    REAL(fp), DIMENSION(:,:),   POINTER :: X   =>NULL() ! K x I, Predictors
    
  END TYPE Predictor_type


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Clear_Predictor
!
! PURPOSE:
!       Subroutine to clear the scalar members of a Predictor structure.
!
! CALLING SEQUENCE:
!       CALL Clear_Predictor( Predictor ) ! Output
!
! OUTPUT ARGUMENTS:
!       Predictor:  Predictor structure for which the scalar
!                   members have been cleared.
!                   UNITS:      N/A
!                   TYPE:       Predictor_type
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  SUBROUTINE Clear_Predictor(Predictor)
    TYPE(Predictor_type), INTENT(IN OUT) :: Predictor
    Predictor%Secant_Sensor_Zenith=ZERO
  END SUBROUTINE Clear_Predictor



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Associated_Predictor
!
! PURPOSE:
!       Function to test the association status of the pointer members of a
!       Predictor structure.
!
! CALLING SEQUENCE:
!       Association_Status = Associated_Predictor( Predictor,        &  ! Input
!                                                  ANY_Test=Any_Test )  ! Optional input
!
! INPUT ARGUMENTS:
!       Predictor:           Predictor structure which is to have its
!                            pointer member's association status tested.
!                            UNITS:      N/A
!                            TYPE:       Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ANY_Test:            Set this argument to test if ANY of the
!                            Predictor structure pointer members are
!                            associated.
!                            The default is to test if ALL the pointer members
!                            are associated.
!                            If ANY_Test = 0, test if ALL the pointer members
!                                             are associated.  (DEFAULT)
!                               ANY_Test = 1, test if ANY of the pointer members
!                                             are associated.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Association_Status:  The return value is a logical value indicating
!                            the association status of the Predictor
!                            pointer members.
!                            .TRUE.  - if ALL the Predictor pointer
!                                      members are associated, or if the
!                                      ANY_Test argument is set and ANY of the
!                                      Predictor pointer members are
!                                      associated.
!                            .FALSE. - some or all of the Predictor
!                                      pointer members are NOT associated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Associated_Predictor( &
             Predictor, &  ! Input
             ANY_Test ) & ! Optional input
           RESULT(Association_Status)
    ! Arguments
    TYPE(Predictor_type), INTENT(IN) :: Predictor
    INTEGER, OPTIONAL   , INTENT(IN) :: ANY_Test
    ! Function result
    LOGICAL :: Association_Status
    ! Local variables
    LOGICAL :: ALL_Test
    
    ! Default is to test ALL the pointer members
    ! for a true association status....
    ALL_Test = .TRUE.
    ! ...unless the ANY_Test argument is set.
    IF ( PRESENT( ANY_Test ) ) THEN
      IF ( ANY_Test == SET ) ALL_Test = .FALSE.
    END IF
    
    ! Test the structure associations    
    Association_Status = .FALSE.
    IF (ALL_Test) THEN
      IF ( ASSOCIATED(Predictor%A   ) .AND. &
           ASSOCIATED(Predictor%dA  ) .AND. &
           ASSOCIATED(Predictor%aveA) .AND. &
           ASSOCIATED(Predictor%AP)   .AND. &
           ASSOCIATED(Predictor%X   ) ) THEN
        Association_Status = .TRUE.
      END IF
    ELSE
      IF ( ASSOCIATED(Predictor%A   ) .OR. &
           ASSOCIATED(Predictor%dA  ) .OR. &
           ASSOCIATED(Predictor%aveA) .OR. &
           ASSOCIATED(Predictor%Ap)   .OR. &
           ASSOCIATED(Predictor%X   ) ) THEN
        Association_Status = .TRUE.
      END IF
    END IF
    
  END FUNCTION Associated_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       Destroy_Predictor
! 
! PURPOSE:
!       Function to re-initialize the scalar and pointer members of
!       a Predictor data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_Predictor( Predictor              , &  ! Output
!                                         RCS_Id     =RCS_Id     , &  ! Revision control
!                                         Message_Log=Message_Log  )  ! Error messaging
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:      Re-initialized Predictor structure.
!                       UNITS:      N/A
!                       TYPE:       Predictor_type
!                       DIMENSION:  Scalar OR Rank-1 array
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the structure re-initialisation was successful
!                          == FAILURE - an error occurred, or
!                                     - the structure internal allocation counter
!                                       is not equal to zero (0) upon exiting this
!                                       function. This value is incremented and
!                                       decremented for every structure allocation
!                                       and deallocation respectively.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Destroy_Predictor( &
             Predictor  , &  ! Output
             No_Clear   , &  ! Optional input
             RCS_Id     , &  ! Revision control
             Message_Log) &  ! Error messaging
           RESULT(Error_Status)
    ! Arguments
    TYPE(Predictor_type)  , INTENT(IN OUT) :: Predictor 
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Clear
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Destroy_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Clear
    INTEGER :: Allocate_Status
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Default is to clear scalar members...
    Clear = .TRUE.
    ! ....unless the No_Clear argument is set
    IF ( PRESENT( No_Clear ) ) THEN
      IF ( No_Clear == 1 ) Clear = .FALSE.
    END IF
    
    ! Initialise the scalar members
    IF ( Clear ) CALL Clear_Predictor(Predictor)
    
    ! If ALL pointer members are NOT associated, do nothing
    IF ( .NOT. Associated_Predictor(Predictor) ) RETURN
    
    ! Deallocate the pointer members
    DEALLOCATE( Predictor%A   , &
                Predictor%dA  , &
                Predictor%aveA, &
                Predictor%Ap  , &
                Predictor%X   , &
                STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error deallocating Predictor. STAT = ", i5 )' ) &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF

    ! Reset the dimension indicators
    Predictor%n_Layers    =0
    Predictor%n_Predictors=0
    Predictor%n_Absorbers =0
    Predictor%n_Orders    =0
    
    ! Decrement and test allocation counter
    Predictor%n_Allocates = Predictor%n_Allocates - 1
    IF ( Predictor%n_Allocates /= 0 ) THEN
      WRITE( Message, '( "Allocation counter /= 0, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Destroy_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       Allocate_Predictor
! 
! PURPOSE:
!       Function to allocate the pointer members of the Predictor
!       data structure.
!
! CALLING SEQUENCE:
!       Error_Status = Allocate_Predictor( n_Layers               , &  ! Input
!                                          n_Predictors           , &  ! Input                
!                                          n_Absorbers            , &  ! Input 
!                                          n_Orders               , &  ! Input               
!                                          Predictor              , &  ! Output               
!                                          RCS_Id     =RCS_Id     , &  ! Revision control     
!                                          Message_Log=Message_Log  )  ! Error messaging      
!
! INPUT ARGUMENTS:
!         n_Layers:          Number of atmospheric layers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_Predictors:      Number of absorption predictors.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_Absorbers:       Number of atmospheric absorbers.
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!         n_orders:          Number of the polynormial function orders for all absorbers
!                            Must be > 0
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor:           Predictor structure with allocated pointer members
!                            UNITS:      N/A
!                            TYPE:       Predictor_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      N/A
!                            TYPE:       CHARACTER(*)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the Message_Handler module.
!                            If == SUCCESS the structure re-initialisation was successful
!                               == FAILURE - an error occurred, or
!                                          - the structure internal allocation counter
!                                            is not equal to one (1) upon exiting this
!                                            function. This value is incremented and
!                                            decremented for every structure allocation
!                                            and deallocation respectively.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Predictor argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Allocate_Predictor( n_Layers     , &  ! Input            
                               n_Predictors , &  ! Input                 
                               n_Absorbers  , &  ! Input 
                               n_Orders     , &  ! Input                
                               Predictor    , &  ! Output                
                               RCS_Id       , &  ! Revision control      
                               Message_Log  ) &  ! Error messaging       
                             RESULT( Error_Status )                      
    ! Arguments
    INTEGER               , INTENT(IN)     :: n_Layers
    INTEGER               , INTENT(IN)     :: n_Predictors
    INTEGER               , INTENT(IN)     :: n_Absorbers
    INTEGER               , INTENT(IN)     :: n_Orders
    TYPE(Predictor_type)  , INTENT(IN OUT) :: Predictor
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Allocate_Predictor'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: Allocate_Status
    
    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    
    ! Check dimensions
    IF ( n_Layers     < 1 .OR. &
         n_Predictors < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Orders     < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             'Input Predictor dimensions must all be > 0.', &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Check if ANY pointers are already associated.
    ! If they are, deallocate them but leave scalars.
    IF ( Associated_Predictor( Predictor, ANY_Test=1 ) ) THEN
      Error_Status = Destroy_Predictor( &
                       Predictor, &
                       No_Clear=1, &
                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( &
               ROUTINE_NAME, &
               'Error deallocating Predictor prior to allocation.', &
               Error_Status, &
               Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    
    ! Perform the pointer allocation
    ALLOCATE( &
      Predictor%A(0:n_Layers,1:n_Absorbers), &
      Predictor%dA(1:n_Layers,1:n_Absorbers), &
      Predictor%aveA(1:n_Layers,1:n_Absorbers), &
      Predictor%Ap(1:n_Layers,1:n_Orders,1:n_Absorbers), &
      Predictor%X(1:n_Layers,1:n_Predictors), &
      STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating Predictor data arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Assign the dimensions
    Predictor%n_Layers     = n_Layers
    Predictor%n_Predictors = n_Predictors
    Predictor%n_Absorbers  = n_Absorbers
    Predictor%n_Orders     = n_Orders

    ! Initialise some of the arrays
    Predictor%A    = ZERO
    Predictor%dA   = ZERO
    Predictor%aveA = ZERO
    Predictor%Ap   = ZERO
    Predictor%X    = ZERO

    ! Increment and test the allocation counter
    Predictor%n_Allocates = Predictor%n_Allocates + 1
    IF ( Predictor%n_Allocates /= 1 ) THEN
      WRITE( Message, '( "Allocation counter /= 1, Value = ", i5 )' ) &
                      Predictor%n_Allocates
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             TRIM(Message), &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
  END FUNCTION Allocate_Predictor


!--------------------------------------------------------------------------------
!
! NAME:
!       Assign_Predictor
!
! PURPOSE:
!       Function to copy valid Predictor structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_Predictor( Predictor_in           , &  ! Input
!                                        Predictor_out          , &  ! Output               
!                                        RCS_Id     =RCS_Id     , &  ! Revision control     
!                                        Message_Log=Message_Log  )  ! Error messaging      
!
! INPUT ARGUMENTS:
!       Predictor_in:      Predictor structure which is to be copied.
!                          UNITS:      N/A
!                          TYPE:       Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:       Character string specifying a filename in which any
!                          messages will be logged. If not specified, or if an
!                          error occurs opening the log file, the default action
!                          is to output messages to standard output.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Predictor_out:     Copy of the input structure, Predictor_in.
!                          UNITS:      N/A
!                          TYPE:       Predictor_type
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:            Character string containing the Revision Control
!                          System Id field for the module.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the structure assignment was successful
!                             == FAILURE an error occurred
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output AtmScatter argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Assign_Predictor( Predictor_in , &  ! Input
                             Predictor_out, &  ! Output               
                             RCS_Id       , &  ! Revision control     
                             Message_Log  ) &  ! Error messaging      
                           RESULT( Error_Status )                     
    ! Arguments
    TYPE(Predictor_type)  , INTENT(IN)     :: Predictor_in      
    TYPE(Predictor_type)  , INTENT(IN OUT) :: Predictor_out     
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_Predictor'

    ! Set up
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! ALL *input* pointers must be associated
    IF ( .NOT. Associated_Predictor(Predictor_In) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             'Some or all INPUT Predictor pointer members are NOT associated.', &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Allocate data arrays
    Error_Status = Allocate_Predictor( Predictor_in%n_Layers    , &
                                       Predictor_in%n_Predictors, &
                                       Predictor_in%n_Absorbers , &
                                       Predictor_in%n_Orders    , &
                                       Predictor_out            , &
                                       Message_Log=Message_Log    )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( &
             ROUTINE_NAME, &
             'Error allocating output structure', &
             Error_Status, &
             Message_Log=Message_Log )
      RETURN
    END IF
    
    ! Assign non-dimension scalar members
    Predictor_out%Secant_Sensor_Zenith = Predictor_in%Secant_Sensor_Zenith

    ! Copy array data
    Predictor_out%A    = Predictor_in%A
    Predictor_out%dA   = Predictor_in%dA
    Predictor_out%aveA = Predictor_in%aveA
    Predictor_out%X    = Predictor_in%X
    Predictor_out%Ap   = Predictor_in%Ap

  END FUNCTION Assign_Predictor



!--------------------------------------------------------------------------------
!
! NAME:
!       Zero_Predictor
! 
! PURPOSE:
!       Subroutine to zero-out all members of a Predictor structure - both
!       scalar and pointer.
!
! CALLING SEQUENCE:
!       CALL Zero_Predictor( Predictor )
!
! OUTPUT ARGUMENTS:
!       Predictor:    Zeroed out Predictor structure.
!                     UNITS:      N/A
!                     TYPE:       Predictor_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       - No checking of the input structure is performed, so there are no
!         tests for pointer member association status. This means the Predictor
!         structure must have allocated pointer members upon entry to this
!         routine.
!
!       - Note the INTENT on the output Predictor argument is IN OUT rather than
!         just OUT. This is necessary because the argument must be defined upon
!         input.
!
!--------------------------------------------------------------------------------

  SUBROUTINE Zero_Predictor( Predictor )  ! Output
    TYPE(Predictor_type),  INTENT(IN OUT) :: Predictor
    ! Reset the scalar components
    Predictor%Secant_Sensor_Zenith = ZERO
    ! Reset the array components
    Predictor%A    = ZERO
    Predictor%dA   = ZERO
    Predictor%aveA = ZERO
    Predictor%Ap   = ZERO
    Predictor%X    = ZERO
  END SUBROUTINE Zero_Predictor
  
END MODULE ODAS_Predictor_Define
