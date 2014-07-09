!------------------------------------------------------------------------------
!   Module of general utilities.
!   Copyright (C) 2014  Bertram Ieong
!------------------------------------------------------------------------------


module utils

implicit none

integer(kind=8) :: dummy_int64

integer, parameter :: dp = kind(0.d0)  ! double precision real
integer, parameter :: intdp = kind(dummy_int64)  ! double precision integer

private :: dummy_int64
public  :: dp, intdp

end module utils