program secante
    implicit none

    integer :: i, N = 100
    double PRECISION :: p0,p,p1,p2,E=0.0000001
    character(len=16)::arg
    call getarg(1,arg)
    read(arg,*)p0
    p = p0
    p1 = p0-1
    do i=0, N
        p2=(p*p1+p0)/(p+p1)
        if (ABS(p2-p1)<E .or. p2**2 == p0) then
            print*, REAL(p2)
            EXIT
        end if
        p = p1
        p1 = p2
    end do
end program secante